(export #t)
(import
  :gerbil/gambit/bytes :gerbil/gambit/exact
  :std/misc/number :std/sugar
  :clan/base :clan/with-id
  :clan/poo/poo (only-in :clan/poo/mop Type)
  ./network-config ./assembly ./ethereum ./types)

;; TODO:
;; * better assembler with segments, etc.?
;; * functions that are automatically inlined if called once, or even if tailed-called once.
;; * temporaries that are automatically un-allocated if only used once, immediately (or after shuffling?)

;; We're going to define a hierarchical ABI for contracts, with
;;
;; * Tiny "inline" functions that are expanded inline, operate on stack (what we use below, mostly).
;;   signature is written: instack1 instack2 instack3 ... --> outstack1 outstack2 outstack3 ...
;;   where the leftmost is top of stack (as in evm.md), and elements indicate stack contents
;;
;; * Small "static" functions that use the stack as both data and return stack, local data stack,
;;   that live within a contract invocation.
;;   The signature is also written: instack1 instack2 instack3 ... --> outstack1 outstack2 outstack3 ...
;;   except that one of the inputs is typically a return address ret@C in current code segment,
;;   typically the rightmost argument. The return values if any are left on work stack.
;;   If there are any, last is swapped with the return address just before the JUMP that returns;
;;   often, to avoid lots of swapping in the callee, extra values are left on stack
;;   that the caller must clean after.
;;   + overhead of calling 'subroutineaddr jump [&jumpdest 'retaddr] [8B, 15G]
;;   + overhead of being called and returning: [&jumpdest 'subroutineaddr] ... JUMP [2B, 9G]
;;   + overhead of shuffling arguments around: SWAPn, SWAPn [2B, 6G]
;;   All in all, a small function costs [12B, 30G] for the call and return,
;;   so definitely inline anything below that,
;;   and beyond, depending on how many times the function appears, starting with 2.
;;
;; * Medium "contract-level" functions that live across contract invocation
;;   overhead of calling + returning: copying frames, etc.
;;   These are quite expensive, cost starts at tens of thousands of gas.
;;   They are also limited to what fits in a single contract.
;;   They follow a calling convention with global runtime registers,
;;   then fixed frame address up to contract-dependent address,
;;   then heap.
;;   TODO:
;;   + convention for more than 2 participants.
;;   + convention for content-addressed heap persistence across frames.
;;
;; * Large "cross-contract" that involve DELEGATECALL and possibly CREATE2 to break the code size barrier.
;;   Notably useful for state channels
;;   Expensive in GAS, plus double-copying of arguments from CALLDATA to memory to CALLDATA.
;;   But OK if only used in case of disputes, where the loser covers the fees.
;;
;; * Huge "virtualized" functions that use a VM on top of the EVM with challenge for execution verification.
;;   OK for huge computations, in the style of TrueBit, Fluence.
;;
;; This file includes some tiny blocks, supports defining small functions,
;; and provides runtime infrastructure to define medium functions.


;; Given the assembled runtime code as a vector for a contract,
;; assemble code to initialize the contract;
;; NB: any storage initialization must happen BEFORE that.
(def (&trivial-contract-init contract-runtime)
  (&begin
   ;; Push args for RETURN; doing it in this order saves one byte and some gas
   (bytes-length contract-runtime) 0 #|memory address for the code: 0|# ;;-- 0 length

   ;; Push args for CODECOPY; the DUP's for length and memory target are where the savings are
   DUP2 #|length|# [&push-label1 'runtime-start] DUP3 #|length|# ;;-- 0 start length 0 length

   ;; Initialize the contract by returning the memory array containing the runtime code
   CODECOPY RETURN

   ;; Inline code for the runtime as a code constant in the init code
   [&label 'runtime-start] #| @ 10 |# [&bytes contract-runtime]))

;; Generic initialization code for stateless contracts
;; : Bytes <- Bytes
(def (stateless-contract-init contract-runtime)
  (assemble/bytes (&trivial-contract-init contract-runtime)))

;; Generic initialization code for stateful contracts of any allowable size (<= 24KiB),
;; where the initial state is a single merklized data point.
;; : Bytes <- Bytes32 Bytes
(def (stateful-contract-init state-digest contract-runtime)
  (assemble/bytes [state-digest 0 SSTORE (&trivial-contract-init contract-runtime)]))

(def (param-length type-or-length)
  (cond ((exact-integer? type-or-length) type-or-length)
        ((element? Type type-or-length) (.@ type-or-length .length-in-bytes))
        (else (invalid 'param-length type-or-length))))

(def (param-type type-or-length)
  (cond ((exact-integer? type-or-length) (UIntN (* 8 type-or-length)))
        ((element? Type type-or-length) type-or-length)
        (else (invalid 'param-type type-or-length))))

;; TODO: support intermediate-speed variables that pad-after?
(defrule (define-consecutive-addresses ctx start end (param type-or-length) ...)
  (begin
    (def end start)
    (with-id ctx
        ((type #'param '-type)
         (length #'param '-length)
         (address #'param '@)
         (getter #'param)
         (setter #'param '-set!))
      (def type (param-type type-or-length))
      (def length (param-length type-or-length))
      (def address (post-increment! end length))
      (def getter (&mloadat address length))
      (def setter (&mstoreat address length))) ...))

;; Local memory layout for solidity:
;; 0x00 - 0x3f (64 bytes): scratch space for pair-hashing methods
;; 0x40 - 0x5f (32 bytes): currently allocated memory size (aka. free memory pointer)
;; 0x60 - 0x7f (32 bytes): zero slot (why does solidity need that at all???)

;; The local memory layout for glow has much more structure.
;; First, some global registers.
;; Their actual sizes (in comment) could be cut shorter, but we probably want cheap access.
(define-consecutive-addresses frame@ 0 frame@
  (brk 32 #|3|#) ;; The free memory pointer.
  (calldatapointer 32 #|3|#) ;; Pointer within CALLDATA to yet unread published information.
  (calldatanew 32 #|3|#) ;; Pointer to new information within CALLDATA (everything before was seen).
  (deposit 32 #|12|#)) ;; Required deposit so far.

;; Second, the frame state as merkleized. These are the fields present in all frames:
(define-consecutive-addresses frame@ frame@ params-start@
  (pc 2) ;; Code segment address from which to continue evaluation
  (last-action-block Block)) ;; Block at which the last action took place
;; Then there will be per-frame parameter fields, to be defined in the proper scope with:
(defrule (define-frame-params ctx params ...)
  (with-id ctx (params-end@)
    (define-consecutive-addresses ctx params-start@ params-end@ params ...)))
;; Then there will be per-frame locals and temporaries, to be defined in the proper scope with:
(defrule (define-frame-locals ctx locals ...)
  (with-id ctx (params-end@ locals-end@)
    (define-consecutive-addresses ctx params-end@ locals-end@ locals ...)
    (register-frame-size locals-end@)))
;; Finally, after the (max ...) of the sizes of all frames including locals,
;; there will be the dynamic brk area (so, we must initialize brk to that early on).
(def brk-start (make-parameter #f))
(def (register-frame-size frame-end@)
  (def b (brk-start))
  (box-set! b (max (unbox b) frame-end@)))


;; TODO: dynamic generate the layout depending on which runtime features necessitate which registers,
;; and which compile-time features define the frame.

;; We log the entire CALLDATA zone in one go. The upside is to save on extra 375 per LOG0 cost
;; and simplify the calling and publish convention, so we don't have to track and log individual messages.
;; The downside is the quadratic cost to memory, 3N+N^2/512 where N is the number of 32-byte words used.
;; Our strategy pays as long as we keep the memory under 438 words or so (14016 bytes).
;; For large contracts with lots of data, it may pay to divide the logging into segments.
;; We'll figure that later (TODO!).
;; One solution would be to run the program first, then at the end,
;; log the CALLDATA starting with the new data only,
;; and decide on block size based on MSIZE.
;;
;; TODO: Don't log old merkleized data (top frame, but also other frames), but check it,
;; and log new data, albeit maybe in many chunks of ~400 words (~14KB) Find the optimal solution in gas.
;;
(def &simple-contract-prelude ;; [39B, ?G]
  (&begin
   ;; Init vs running convention!
   ;; Put some values on stack while they're extra cheap.
   GETPC GETPC GETPC 240 ;; -- 240 2 1 0
   ;; Get state frame size, starting with PC, 16 bit
   DUP4 #|0|# CALLDATALOAD DUP2 #|240|# SHR frame@ ;; -- frame@ sz 240 2 1 0
   ;; copy frame to memory
   DUP2 #|sz|# DUP5 #|2|# DUP3 #|frame@|# CALLDATACOPY ;; -- frame@ sz 240 2 1 0
   ;; store calldatapointer
   DUP2 #|sz|# DUP5 #|2|# ADD calldatapointer-set! ;; -- frame@ sz 240 2 1 0
   ;; save the brk variable
   DUP2 #|sz|# DUP2 #|frame@|# ADD DUP8 #|brk@,==0|# MSTORE ;; -- frame@ sz 240 2 1 0
   ;; compute the digest of the frame just restored
   SHA3 ;; -- digest 240 2 1 0
   ;; compare to saved merkleized state, jump to saved label if it matches
   ;; BEWARE: we assume the variable *before* the frame is not initialized, and still 0.
   DUP5 #|0|# SLOAD EQ (- frame@ 30) MLOAD JUMPI ;; -- stack at destination: -- 240 2 1 0

   ;; Abort. We explicitly PUSH1 0 for the first rather than DUPn,
   ;; because we don't assume stack geometry from the caller when aborting.
   [&jumpdest 'abort-contract-call] 0 DUP1 #|0|# REVERT))

;; This *defines* [25B] a function with label 'unsafe-memcopy [(53*LEN+35)G]
;; that you invoke with the following arguments (top-of-stack onward):
;; -- length-in-words destination source return-address
;; The function copies data in memory, assuming the destination is below the source or there is no overlap,
;; and the length is in multiple of 32-byte-words, and it's ok if this includes padding.
;; We assumes this code will be compiled into the first 256 bytes of code, if present
;; By contrast, a series of mloadat, mstoreat 32 bytes at a time is [8*LEN B,12*LEN G]
(def &define-unsafe-memcopy
  (&begin
   [&jumpdest 'unsafe-memcopy-body]
   1 SUB ;; -- len-1 dest src
   DUP3 32 ADD SWAP4 MLOAD ;; -- data len-1 dest src+32
   DUP3 32 ADD SWAP4 MSTORE ;; -- data dest+32 src+32
   [&jumpdest 'unsafe-memcopy] ;; -- len dest src
   DUP1 'unsafe-memcopy-body JUMPI ;; when all done, exit
   POP POP POP JUMP))

;; If condition is TRUE then abort
;; Pseudocode: (lambda (x) (when (truish? x) (abort!)))
(def &require-not! (&begin 'abort-contract-call JUMPI)) ;; [3B, 13G]

;; If condition is FALSE then abort
;; Pseudocode: (lambda (x) (unless (truish? x) (abort!)))
(def &require! (&begin ISZERO &require-not!)) ;; [4B, 16G]

;; Check the requirement that the amount actually deposited in the call (from CALLVALUE) is sufficient
;; to cover the amount that the contract believes should have been deposited (from deposit@ MLOAD).
(def &check-sufficient-deposit
  (&begin deposit@ MLOAD CALLVALUE LT &require-not!)) ;; [8B, 25G]

;; TODO: *in the future*, have a variant of contracts that allows for posting markets,
;; whereby whoever posts the message to the blockchain might not be the participant,
;; and instead, the participant signs the in-contract message.
;;;;(def check-correct-participant/posting-market [participant@ MLOAD ...])
(def &check-participant!
  ;; Scheme pseudocode: (lambda (participant) (require! (eqv? (CALLER) participant)))
  (&begin CALLER EQ &require!)) ;; [6B, 21G]

;; Safely add two UInt256, checking for overflow
(def &safe-add
  ;; Scheme pseudocode: (lambda (x y) (def s (+ x y)) (require! (< (integer-length s) 256)) s)
  ;; (unless (> 2**256 (+ x y)) (abort))
  ;; (unless (>= (- 2**256 1) (+ x y)) (abort))
  ;; (unless (>= (- 2**256 1 x) y) (abort))
  ;; (when (< (- 2**256 1 x) y) (abort))
  ;; (when (< (extract-bit-field 256 0 (bitwise-not x)) y) (abort))
  (&begin DUP2 #|y|# DUP2 #|x|# NOT LT &require-not! ADD)) ;; [8B, 28G]

;; *Assuming* x y are both non-negative integers of integer-length n-bits or less,
;; abort unless their sum is also of integer-length n-bits, return the sum
(def (&safe-add/n-bits n-bits)
  (assert! (and (exact-integer? n-bits) (<= 0 n-bits 256)))
  (cond
   ((= n-bits 256) &safe-add) ;; [8B, 28G]
   ((zero? n-bits) POP) ;; [1B, 2G]
   (else (&begin ADD DUP1 n-bits SHR &require-not!)))) ;; [8B, 25G]

;; Assuming x y are both of integer-length n-bits or less,
;; abort unless their sum is also of integer-length n-bits, return the sum
(def &safe-sub ;; [7B, 25G]
  (&begin DUP2 DUP2 LT &require-not! SUB))

;; Multiply two UInt256, abort if the product overflows UInt256.
(def (&safe-mul) ;; [23B, 72G]
  (let ((safe-mul-body (generate-label 'safe-mul-body))
        (safe-mul-end (generate-label 'safe-mul-end)))
    (&begin ;; -- x y
     DUP2 safe-mul-body JUMPI POP safe-mul-end JUMP ;; [10B, 29G]
     [&jumpdest safe-mul-body] ;; [1B, 1G]
     DUP2 #|y|# DUP2 #|x|# MUL ;; -- xy x y [3B, 11G]
     SWAP2 #|y x xy|# DUP3 #|xy|# DIV ;; -- xy/y x xy [3B, 11G]
     EQ &require! [&jumpdest safe-mul-end]))) ;; -- xy [6B, 20G]

(def &deposit!
  ;; Scheme pseudocode: (lambda (amount) (increment! deposit amount))
  ;; TODO: can we statically prove it's always within range and make the &safe-add an ADD ???
  (&begin deposit@ MLOAD &safe-add deposit@ MSTORE)) ;; [14B, 40G]

(def &send-ethers!
  (&begin ;; -- address value
   0 DUP1 DUP1 SWAP5 DUP2 SWAP5 GAS ;; -- gas address value 0 0 0 0
   CALL &require!)) ;; -- Transfer!

;; TODO: group the withdrawals at the end, like the deposit checks?
(def &withdraw! &send-ethers!)

(def &mload/signature ;; v r s <-- signature@
  (&begin
   DUP1 MLOAD ;; -- r sig@ ;; load the first word of sig
   DUP2 32 ADD MLOAD ;; -- s r sig@ ;; load the second word of sig
   SWAP3 64 ADD (&mload 1))) ;; v r s ;; load the last byte of sig

;; Validate on-stack signature
(def &validate-sig-data ;; v r s <-- v r s
  (&begin
   DUP1 27 SUB 2 GT ;; check that v is 27 or 28, which prevents malleability (not 29 or 30)
   #x7FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF5D576E7357A4501DDFE92F46681B20A0 DUP5 GT ;; s <= s_max
   OR &require-not!))

;; call precompiled contract #1 to recover the signer and message from a signature
(def &ecrecover0 ;; -- v r s digest --> address success
  (&begin
   &brk ;; -- brk digest v r s
   SWAP1 #|digest|# DUP2 #|brk|# MSTORE ;; -- brk v r s
   SWAP1 #|v|# DUP2 #|brk|# 32 ADD MSTORE ;; -- brk r s
   SWAP1 #|r|# DUP2 #|brk|# 64 ADD MSTORE ;; -- brk s
   SWAP1 #|s|# DUP2 #|brk|# 96 ADD MSTORE ;; -- brk
   32 DUP2 #|brk|# 128 DUP2 #|brk|# 0 1 GAS ;; -- gas address value argstart:brk argwidth:128 retstart:brk retwidth:32 brk
   STATICCALL SWAP1 MLOAD))

(def &isValidSignature ;; -- signature digest signer --> bool
  (&begin ;; -- signature digest signer
   &mload/signature ;; -- v r s digest signer
   DUP4 #|digest|# &ecrecover0 ;; -- address success digest signer
   DUP4 #|signer|# EQ AND SWAP2 POP POP)) ;; -- bool

(def (&unsafe-post-increment-at! addr increment)
  (&begin addr MLOAD DUP1 increment ADD addr MSTORE)) ;; for small address, small size [10B, 21G]

(def &brk (&begin brk@ MLOAD)) ;; [3B, 6G]
(def (&brk-cons n-bytes)
  ;; Note the optimization wherein we can write extra zeros *after* the destination address
  ;; since we're mixing data with yet unwritten zeroes anyway
  (assert! (and (exact-integer? n-bytes) (<= 0 n-bytes 32)))
  (cond
   ((zero? n-bytes) POP)
   ((= n-bytes 1) (&begin (&unsafe-post-increment-at! brk@ n-bytes) MSTORE8))
   ((= n-bytes 32) (&begin (&unsafe-post-increment-at! brk@ n-bytes) MSTORE))
   ;; TODO: for programs that use a lot of memory, optimize the last few of these to not use memory?
   ;; But first, optimize the lot of memory into less memory
   (else (&begin (- 256 (* 8 n-bytes)) SHL (&unsafe-post-increment-at! brk@ n-bytes) MSTORE))))

(def (&read-published-datum (n-bytes 32))
  ;; Scheme pseudocode: (lambda () (extract-top-bytes (calldata-ref (post-increment! calldatapointer n)) n))
  (assert! (and (exact-integer? n-bytes) (<= 0 n-bytes 32)))
  (if (zero? n-bytes)
    0
    (&begin calldatapointer@ MLOAD DUP1 #| calldatapointer@ |# 32 + calldatapointer@ MSTORE CALLDATALOAD
            (when (< n-bytes 32) (&begin (* 8 (- 32 n-bytes)) SHR)))))

(def &read-published-data-to-mem
  (&begin ;; -- memaddr size
   calldatapointer@ MLOAD DUP1 #|calldatapointer@|# DUP4 #|size|# ADD
   ;; DUP1 CALLDATASIZE LT &require-not! ;;---we don't actually need to validate that: ethereum will pad with zeroes on overflow, and the rest of the program will see if it's valid.
   calldatapointer@ MSTORE SWAP1 CALLDATACOPY))

;; NB: must be put just *before* the &define-*-logging, so it will fall through it
;; if the tail call is to be handled by another participant
(def &define-tail-call
  (&begin
   [&jumpdest 'stop-contract-call]
   STOP
   [&jumpdest 'tail-call-body]
   frame@ ADD brk@ MSTORE ;; BEWARE: we can only reset the brk because we don't preserve heap data!
   (&mloadat frame@ 2) JUMP
   [&jumpdest 'tail-call]
   ;; -- frame-length TODO: at standard place in frame, info about who is or isn't timing out
   ;; and/or make it a standard part of the cp0 calling convention to catch such.
   (&read-published-datum 1) 'tail-call-body JUMPI
   frame@ SHA3 0 SSTORE
   'stop-contract-call
   [&jumpi1 'commit-contract-call])) ;; update the state, then commit and finally stop

;; Logging the data, simple version, optimal for messages less than 6000 bytes of data.
(def &define-simple-logging
  (&begin
   [&jumpdest 'commit-contract-call] ;; -- return-address
   &check-sufficient-deposit ;; First, check deposit
   calldatanew@ MLOAD CALLDATASIZE DUP2 SUB ;; -- logsz cdn
   0 DUP2 #|logsz|# DUP4 #|cdn|# DUP3 #|0|# CALLDATACOPY LOG0 JUMP))

;; Logging the data
(def &define-variable-size-logging
  (&begin
   [&jumpdest 'commit-contract-call]
   &check-sufficient-deposit ;; First, check the deposit
   ;; compute available buffer size: max(MSIZE, n*256)
   ;; -- TODO: find out the optimal number to minimize gas, considering the quadratic cost of memory
   ;; versus the affine cost of logging, and the cost of this loop.
   ;; i.e. compute total logging gas depending on buffer size, differentiate, minimize
   ;; The marginal cost C of this loop is ~550 (linear logging costs are not marginal), total C*L/B.
   ;; The marginal memory cost beyond M is 3/32*B+B*B/Q, Q=524288. Minimize for B: C*L/B+B*B/Q+3/32*B
   ;; We cancel the derivative, which is -C*L/B^2+2*B/Q+3/32, or (B^3*2/Q + B^2*3/32 -C*L)/B^2.
   ;; Let's neglect the quadratic term for now.
   ;; The optimal buffer size verifies -C*L/B^2 + 3/32 = 0, or B = sqrt(32*C*L/3) = sqrt(32*C/3)*sqrt(L)
   ;; sqrt(32*C/3) is about 77. Under about 6000B (the usual case?), it's always best to have a single log.
   ;; That's before the quadratic term kicks in.
   ;; Now, for large call data sizes, the quadratic term starts to matter:
   ;; 8M gas limit and about 22 g/byte mean that L < 360000 sqrt(L) < 600.
   ;; The optimal number neglecting the quadratic term goes up to 46200,
   ;; but at that point, the total two memory costs are comparable (about 4000 Gas).
   ;; The formula for optimal L with only the quadratic term is cubrt(Q*C/2)*cubrt(L),
   ;; which also tops at 38000 and grows more slowly.
   ;; But we can use Wolfram Alpha to solve exactly:
   ;; https://www.wolframalpha.com/input/?i=Reduce%5B%283+B%5E2%29%2F32+%2B+B%5E3%2F262144+-+550+L+%3D%3D+0%2C+B%5D
   ;; We find the exact real solution:
   ;; B = 64 ((275 L + 5 sqrt(11) sqrt(L (275 L - 4194304)) - 2097152)^(1/3) + 16384/(275 L + 5 sqrt(11) sqrt(L (275 L - 4194304)) - 2097152)^(1/3) - 128)
   ;; We can plot it:
   ;; https://www.wolframalpha.com/input/?i=plot+%7C+64+%28%28275+L+%2B+5+sqrt%2811%29+sqrt%28L+%28275+L+-+4194304%29%29+-+2097152%29%5E%281%2F3%29+%2B+16384%2F%28275+L+%2B+5+sqrt%2811%29+sqrt%28L+%28275+L+-+4194304%29%29+-+2097152%29%5E%281%2F3%29+-+128%29%2C+L+from+0+to+360000&assumption=%7B%22F%22%2C+%22Plot%22%2C+%22plotvariable%22%7D+-%3E%22L%22&assumption=%22FSelect%22+-%3E+%7B%7B%22Plot%22%7D%7D&assumption=%7B%22F%22%2C+%22Plot%22%2C+%22plotlowerrange%22%7D+-%3E%2232%22&assumption=%7B%22C%22%2C+%22plot%22%7D+-%3E+%7B%22Calculator%22%7D&assumption=%7B%22F%22%2C+%22Plot%22%2C+%22plotfunction%22%7D+-%3E%2264+%28%28275+L+%2B+5+sqrt%2811%29+sqrt%28L+%28275+L+-+4194304%29%29+-+2097152%29%5E%281%2F3%29+%2B+16384%2F%28275+L+%2B+5+sqrt%2811%29+sqrt%28L+%28275+L+-+4194304%29%29+-+2097152%29%5E%281%2F3%29+-+128%29%22&assumption=%7B%22F%22%2C+%22Plot%22%2C+%22plotupperrange%22%7D+-%3E%22360000%22
   ;; It grows slowly from 0 to a bit over 30000 for L=360000.
   ;;
   ;; Instead of having the contract itself minimize a polynomial according to some elaborate formula,
   ;; we can just let the user specify their buffer size as a parameter (within meaningful limits);
   ;; if they provide a bad answer, they are the ones who pay the extra gas (or fail).
   ;; This parameter could be a single byte, to be shifted left 7 bits.
   ;; -- getting it wrong is only 70-odd gas wrong,
   ;; less than it costs to use a second byte for precision.
   MSIZE 16384 DUP2 DUP2 GT [&jumpi1 'maxm1] SWAP1
   [&jumpdest 'maxm1] POP ;; -- bufsz
   calldatanew@ MLOAD CALLDATASIZE DUP2 SUB ;; -- logsz cdn bufsz
   ;; Loop:
   [&jumpdest 'logbuf] ;; -- logsz cdn bufsz
   ;; If there's no more data, stop.
   DUP1 #|logsz|# [&jumpi1 'logbuf1] POP POP POP JUMP [&jumpdest 'logbuf1] ;; -- logsz cdn bufsz
   ;; compute the message size: msgsz = min(cdsz, bufsz)
   DUP3 #|bufsz|# DUP2 #|logsz|# LT [&jumpi1 'minbl] SWAP1
   [&jumpdest 'minbl] POP ;; -- msgsz logsz cdn bufsz
   ;; Log a message
   DUP1 #|msgsz|# 0 DUP2 #|msgsz|# DUP6 #|cdn|# DUP3 #|0|# CALLDATACOPY LOG0 ;; -- msgsz logsz cdn bufsz
   ;; Adjust logsz and cdn
   SWAP3 #|cdn logsz msgsz|# DUP3 #|msgsz|# ADD SWAP3 #|msgsz logsz cdn|# SWAP1 SUB ;; -- logsz cdn bufsz
   ;; loop!
   [&jump1 'logbuf]))

(def (timeout-in-blocks)
  (.@ (current-ethereum-network) timeoutInBlocks))

(def (penny-collector-address)
  (.@ (current-ethereum-network) pennyCollector))

;; Define the end-contract library function, if reachable.
;; TODO: one and only one of end-contract or tail-call shall just precede the commit-contract-call function!
;; Might depend on the contract which--usually the tail-call, except when
;; it's a trivial contract like buy-sig. Maybe have a notion of segments that either precede another one,
;; or can be anywhere with a jump in the end, with an expected use frequency function
;; to prefer one the most used one over the alternatives?
(def (&define-end-contract)
  (&begin
   [&jumpdest 'suicide]
   (penny-collector-address) ;; send any leftover money to this address!
   SELFDESTRUCT
   [&jumpdest 'end-contract]
   0 0 SSTORE 'suicide [&jump1 'commit-contract-call]))

(def &end-contract!
  (&begin [&jump 'end-contract])) ;; [2B; 10G]

(def &save-last-action-block ;; -->
  (&begin NUMBER last-action-block-set!)) ;; [17B, 29G]

;; abort unless saved data indicates a timeout
(def (&check-timeout!) ;; -->
  (&begin (timeout-in-blocks) ;; TODO: negotiate the timeout between users?
          last-action-block NUMBER SUB
          LT &require-not!))

;; For two-participant contracts only
(def (&define-check-participant-or-timeout)
  (&begin ;; obliged-actor@ other-actor@ ret@C --> other-actor@
   [&jumpdest 'check-participant-or-timeout]
   (&mload 20) CALLER EQ SWAP3 #|ret@C|# JUMPI ;; if the caller matches, return to normal program
   ;; TODO: support some amount being in escrow for the obliged-actor and returned to him
   ;; Also support ERC20s, etc.
   (&check-timeout!) (&mload 20) SELFDESTRUCT)) ;; give all the money to the other guy.

(def (&check-participant-or-timeout! must-act: obliged-actor or-end-in-favor-of: other-actor)
  (&begin
   (&call 'check-participant-or-timeout other-actor obliged-actor)
   POP)) ;; pop the other-actor@ left on the stack
