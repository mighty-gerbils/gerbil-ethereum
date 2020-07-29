(export #t)
(import
  :gerbil/gambit/bytes :gerbil/gambit/exact
  :std/sugar
  ./assembly)

;; Local memory can only be accessed 32-byte (or, for writes, also 1 byte) at a time,
;; and masking / merging is rather expensive, so for often-used stuff, it makes sense
;; to waste memory to save some gas. On the other hand, the cost of local memory is ultimately
;; quadratic in the total size, so for regular data (vs often-used global registers),
;; it pays to be compact.
;; Reading is cheap enough:
(def (&mload n-bytes)
  (assert! (and (exact-integer? n-bytes) (<= 0 n-bytes 32)))
  (cond
   ((zero? n-bytes) (&begin POP 0)) ;; [3B, 5G]
   ((= n-bytes 32) MLOAD) ;; [1B, 3G]
   (else (&begin MLOAD (- 256 (* 8 n-bytes)) SHR)))) ;; [4B, 9G]

(def (&mloadat n-bytes addr)
  (assert! (and (exact-integer? n-bytes) (<= 0 n-bytes 32)))
  (cond
   ((zero? n-bytes) 0) ;; [2B, 3G]
   ((= n-bytes 32) (&begin addr MLOAD)) ;; [4B, 6G] or for small addresses [3B, 6G]
   (else (&begin addr MLOAD (- 256 (* 8 n-bytes)) SHR)))) ;; [7B, 12G] or for small addresses [6B, 12G]

(def (&mstore n-bytes)
  (assert! (and (exact-integer? n-bytes) (<= 0 n-bytes 32)))
  (cond
   ((zero? n-bytes) (&begin POP POP)) ;; [2B, 4G]
   ((= n-bytes 1) MSTORE8) ;; [1B, 3G]
   ((= n-bytes 2) (&begin DUP2 8 SHR DUP2 MSTORE8 1 ADD MSTORE8)) ;; [10B, 24G]
   ;;(= n-bytes 3) (&begin DUP2 16 SHR DUP2 MSTORE8 1 ADD &mstore16)) ;; [19B, 45G]
   ((= n-bytes 32) MSTORE) ;; [1B, 3G]
   (else ;; [16B, 38G]
    (let (n-bits (* 8 n-bytes))
      ;;(&begin SWAP1 scratch0@ MSTORE DUP1 n-bytes ADD MLOAD scratch1@ MSTORE (- scratch1@ n-bytes) MLOAD SWAP1 MSTORE) ;; [17B, 39G]
      ;; [16B, 38G] -- note that we could skip the ending POP
      (&begin DUP1 n-bytes ADD MLOAD n-bits SHR DUP3 (- 256 n-bits) SHL OR SWAP1 MSTORE POP)))))

(def (&mstoreat n-bytes addr)
  (assert! (and (exact-integer? n-bytes) (<= 0 n-bytes 32)))
  (cond
   ((= n-bytes 32) (&begin addr MSTORE)) ;; [4B, 6G] or for small addresses [3B, 6G]
   ((zero? n-bytes) (&begin POP)) ;; [1B, 2G]
   ((= n-bytes 1) (&begin addr MSTORE8)) ;; [4B, 6G] or for small addresses [3B, 6G]
   ((= n-bytes 2) (&begin DUP1 8 SHR addr MSTORE8 (1+ addr) MSTORE8)) ;; [12B, 21G]
   ;;((= n-bytes 3) (&begin DUP1 16 SHR addr MSTORE8 (&mstore16at (1+ addr)))) ;; [20B, 36G]
   (else ;; [16B, 38G]
    (let (n-bits (* 8 n-bytes))
      (&begin (- 256 n-bits) SHL (+ addr n-bytes) MLOAD n-bits SHR OR addr MSTORE))))) ;; [15B, 36G]

;; Generic initialization code for stateless contracts of less than 255 bytes.
;; : Bytes <- Bytes
(def (constant-stateless-small-contract-init contract-runtime)
  (assert! (< (bytes-length contract-runtime) 256))
  (assemble
   [;; Push args for RETURN; doing it in this order saves one byte and some gas
    (bytes-length contract-runtime) 0 ;; memory address for the code
    ;; -- 0 length

    ;; Push args for CODECOPY; the DUP's for length and memory target are where the savings are
    DUP2 #| length |# [&push-label1 'runtime-start] DUP3 ;; memory target address: 0
    ;; -- 0 start length 0 length

    ;; Initialize the contract by returning the memory array containing the runtime code
    CODECOPY RETURN

    ;; Inline code for the runtime as a code constant in the init code
    [&label 'runtime-start] #| @ 10 |# [&bytes contract-runtime]]))


;; Generic initialization code for stateful contracts of any allowable size (<= 24KiB),
;; where the initial state is a single merklized data point.
;; : Bytes <- Bytes32 Bytes
(def (simple-contract-init state-digest contract-runtime)
  (assemble
   [;; Save the state
    state-digest 0 SSTORE
    ;; Push args for RETURN; doing it in this order saves one byte and some gas
    (bytes-length contract-runtime) 0 ;; memory address for the code
    ;; -- 0 length

    ;; Push args for CODECOPY; the DUP's for length and memory target are where the savings are
    DUP2 #| length |# [&push-label1 'runtime-start] DUP3 #| 0 as memory target address |#
    ;; -- 0 start length 0 length

    ;; Initialize the contract by returning the memory array containing the runtime code
    CODECOPY RETURN

    ;; Inline code for the runtime as a code constant in the init code
    [&label 'runtime-start] #| @ 10 |# [&bytes contract-runtime]]))

;; local-memory-layout for solidity:
;; 0x00 - 0x3f (64 bytes): scratch space for hashing methods
;; 0x40 - 0x5f (32 bytes): currently allocated memory size (aka. free memory pointer)
;; 0x60 - 0x7f (32 bytes): zero slot (why does solidity need that at all???)
;;
;; local-memory-layout for glow:
(def brk@ 0) ;; (32 bytes): brk, free memory pointer
(def calldatapointer@ 32) ;; (32 bytes): pointer within CALLDATA to yet unread published information
(def calldatanew@ 64) ;; (32 bytes): pointer to new information within CALLDATA (everything before was seen)
(def deposit@ 96) ;; (32 bytes): required deposit so far
(def frame@ 128) ;; or do we want it variable?
;; 128 - N-1: as many temporary variables as needed in the program, N is a program constant.
;; N - N+M: reserved for frame variables
;; M - end: heap

;; call stack has fixed layout:
;; - 0

;; We log the entire CALLDATA zone in one go. The upside is to save on extra 375 per LOG0 cost
;; and simplify the calling and publish convention, so we don't have to track and log individual messages.
;; The downside is the quadratic cost to memory, 3N+N^2/512.
;; Our strategy pays as long as we keep the memory under 438 words or so.
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
   ;; Put some values on stack while they're cheap.
   GETPC GETPC GETPC 240 ;; -- 240 2 1 0
   ;; Get state frame size, starting with PC, 16 bit
   DUP4 #|0|# CALLDATALOAD DUP2 #|240|# SHR frame@ ;; -- frame@ sz 240 2 1 0
   ;; copy frame to memory
   DUP2 #|sz|# DUP5 #|2|# DUP3 #|frame@|# CALLDATACOPY ;; -- frame@ sz 240 2 1 0
   ;; store calldatapointer
   DUP2 #|sz|# DUP5 #|2|# ADD calldatapointer@ MSTORE ;; -- frame@ sz 240 2 1 0
   ;; save the brk variable
   DUP2 #|sz|# DUP2 #|frame@|# ADD DUP8 #|brk@,==0|# MSTORE ;; -- frame@ sz 240 2 1 0
   ;; compute the digest of the frame just restored
   SHA3 ;; -- digest 240 2 1 0
   ;; compare to saved merkleized state, jump to saved label if it matches
   DUP5 #|0|# SLOAD EQ (- frame@ 30) MLOAD JUMPI ;; -- stack at destination: -- 240 2 1 0

   ;; Abort. We explicitly PUSH1 0 for the first rather than DUPn,
   ;; because we don't assume stack geometry from the caller when aborting.
   [&jumpdest 'abort-contract-call] 0 DUP1 #|0|# REVERT))

;; This *defines* [25B] a function with label 'unsafe-memcopy [(53*LEN+35)G]
;; that you invoke with the following arguments (top-of-stack onward):
;; -- length-in-words destination source return-address
;; The function copies data in memory, assuming the destination is below the source or there is no overlap,
;; and the length is in multiple of 32-byte-words, and it's ok if this includes padding.
;; We assumes this code will be compiled into the first 256 bytes of code.
(def &unsafe-memcopy
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

;; safely add two UInt256, checking for overflow
(def &safe-add/256
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
   ((= n-bits 256) &safe-add/256) ;; [8B, 28G]
   ((zero? n-bits) POP) ;; [1B, 2G]
   (else (&begin ADD DUP1 n-bits SHR &require-not!)))) ;; [8B, 25G]

;; Assuming x y are both of integer-length n-bits or less,
;; abort unless their sum is also of integer-length n-bits, return the sum
(def &safe-sub ;; [7B, 25G]
  (&begin DUP2 DUP2 LT &require-not! SUB))

(def &deposit!
  ;; Scheme pseudocode: (lambda (amount) (increment! deposit amount))
  ;; TODO: can we statically prove it's always within range and make the &safe-add an ADD ???
  (&begin deposit@ MLOAD &safe-add/256 deposit@ MSTORE)) ;; [14B, 40G]

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

(def &digest-frame
  ;; (lambda (begin-brk) (digest-memory from: begin-brk (current-brk)))
  (&begin &brk DUP2 SUB SWAP1 SHA3))

(def (&read-published-datum (n-bytes 32))
  ;; Scheme pseudocode: (lambda () (extract-top-bytes (calldata-ref (post-increment! calldatapointer n)) n))
  (assert! (and (exact-integer? n-bytes) (<= 0 n-bytes 32)))
  (if (zero? n-bytes)
    0
    (&begin calldatapointer@ MLOAD DUP1 #| calldatapointer@ |# 32 + calldatapointer@ MSTORE CALLDATALOAD
            (when (< n-bytes 32) (&begin (* 8 (- 32 n-bytes)) SHR)))))

;; Logging the data, simple version, optimal for messages less than 6000 bytes of data.
(def &simple-logging
  (&begin
   [&jumpdest 'commit-contract-call]
   &check-sufficient-deposit ;; First, check deposit
   calldatanew@ MLOAD CALLDATASIZE DUP2 SUB ;; -- logsz cdn
   0 DUP2 #|logsz|# DUP4 #|cdn|# DUP3 #|0|# CALLDATACOPY LOG0 STOP))

;; Logging the data
(def &variable-size-logging
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
   ;; It grows slowly from 0 to about 30000 for L=360000.
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
   DUP1 #|logsz|# [&jumpi1 'logbuf1] STOP [&jumpdest 'logbuf1] ;; -- logsz cdn bufsz
   ;; compute the message size: msgsz = min(cdsz, bufsz)
   DUP3 #|bufsz|# DUP2 #|logsz|# LT [&jumpi1 'minbl] SWAP1
   [&jumpdest 'minbl] POP ;; -- msgsz logsz cdn bufsz
   ;; Log a message
   DUP1 #|msgsz|# 0 DUP2 #|msgsz|# DUP6 #|cdn|# DUP3 #|0|# CALLDATACOPY LOG0 ;; -- msgsz logsz cdn bufsz
   ;; Adjust logsz and cdn
   SWAP3 #|cdn logsz msgsz|# DUP3 #|msgsz|# ADD SWAP3 #|msgsz logsz cdn|# SWAP1 SUB ;; -- logsz cdn bufsz
   ;; loop!
   [&jump1 'logbuf]))
