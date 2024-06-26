(export #t)

(import
  :gerbil/gambit
  :std/error :std/format
  :std/misc/bytes
  :std/misc/hash
  :std/misc/number
  :std/srfi/1 :std/stxutil
  :std/sugar
  :std/text/hex :std/values
  :clan/base
  :clan/poo/object :clan/poo/io
  ./types ./ethereum ./network-config)

;; In the future, segments can be nested, and
;; offset would be relative to a subsegment of a segment,
;; to be fully resolved when the segment size is finalized.
;; (Also, a non-embedded front-end syntax.)
;;
;; For now, everything is fixed size and we don't compute
;; label offsets much less optimize segment layout:
;; instead we merely *check* that labels are used properly
;; and the formulas match.

;; 24576, code size limit set by EIP 170 https://github.com/ethereum/EIPs/blob/master/EIPS/eip-170.md
(def max-code-size #x6000)
;; 49152, initialization code size limit set by EIP 3860 (Shanghai, 2023), double the above
(def max-init-code-size #xC000)

;; TODO: use mutable extensible vectors? pure functional arrays?
;; Doubly linked list of sub-segments? Balanced tree that maintains intervals?
;; A segment is one of:
;; a u8vector, used literally as values
;; a list of a segment descriptor and segments
;; the descriptor contains min-start/max-start, min-length/max-length

;; an relocation expression is one of:
;; an integer, representing an address of offset
;; a symbol, label representing an unresolved address
;; a sum (+ x ...) of relocation expressions
;; a negation or difference (- x ...) of relocation expressions
(defstruct Segment
  (bytes ;; vector of bytes
   fill-pointer) ;; how many of those bytes are filled?
  transparent: #t)

(def (new-segment (size max-code-size))
  (make-Segment (make-u8vector size 0) 0))

(def (segment-full? s)
  (>= (Segment-fill-pointer s) (u8vector-length (Segment-bytes s))))

(def (segment-push! s b)
  (when (segment-full? s) (error "segment full" 'segment-push! s b))
  (u8vector-set! (Segment-bytes s) (Segment-fill-pointer s) b)
  (increment! (Segment-fill-pointer s)))

(def (segment-push-bytes! s b)
  (unless (< (+ (u8vector-length b) (Segment-fill-pointer s))
             (u8vector-length (Segment-bytes s)))
    (error "segment full" 'segment-push-bytes! s b))
  (subu8vector-move! b 0 (u8vector-length b) (Segment-bytes s) (Segment-fill-pointer s))
  (increment! (Segment-fill-pointer s) (u8vector-length b)))

(def (segment-contents s)
  (subu8vector (Segment-bytes s) 0 (Segment-fill-pointer s)))

;; a Fixup is a pair of an expression and a size in bits specifying how much space there is for the fixup,
;; at a given address which is used as key of the fixup table.
;; (deftype Fixup (Pair FixupExpression UInt24))

;; an Assembler has a Segment and/or buffer, a table from Symbol to LabelInformation,
;; and a table from address to fixup
(defstruct Assembler
  (segment ;; : Segment
   labels  ;; : (Table UInt16 <- Symbol)
   fixups) ;; : (Table Fixup <- UInt16)
  transparent: #t)
(def (new-assembler)
  (make-Assembler (new-segment) (make-hash-table) (make-hash-table)))

;; TODO: have chunks of code of constant or unknown but bounded length,
;; labels, fixups, displacements, etc.; compile-time merging of constant stuff(?)

;; (define-type Directive (Fun Unit <- Assembler))

(def fixup-functions
  (hash (+ +) (- -) (* *)))

(def (eval-fixup-expression labels expr)
  (match expr
    ((? number? x) x)
    ((? symbol? s)
     (hash-ref/default labels s
       (cut error (format "eval-fixup-expression: expr symbol ~r not found in labels ~r"
                          s
                          (hash-keys labels)))))
    ([f . l]
     (apply (hash-get fixup-functions f) (map (cut eval-fixup-expression labels <>) l)))))

;; : Bytes (Table Offset <- Symbol) <- Directives
(def (assemble directives)
  (def a (new-assembler))
  (cond
   ((procedure? directives) (directives a))
   ((list? directives) (&directives a directives))
   (else (error "invalid directives")))
  (hash-for-each (lambda (offset fixup) (do-fixup a offset (car fixup) (cdr fixup)))
                 (Assembler-fixups a))
  (values (segment-contents (Assembler-segment a)) (Assembler-labels a)))

(def (assemble/bytes directives) (first-value (assemble directives)))

;; disassemble takes a u8vector and disassembles it, returning a list
;; of instructions, of the form:
;;
;; - A symbol for the instruction mnemonic, for most instructions.
;; - (PUSH* number string) for PUSH* instructions (where * is the size of the argument).
;;   The (first) numeric argument is the actual value that is pushed. The second argument
;;   is the same, but encoded as a hexideicmal scheme literal (e.g. "#x0ab1"); having
;;   both forms readily available is sometimes helpful when debugging.
(def (disassemble bytes)
  (def labels (make-hash-table))
  (let loop ((data (u8vector->list bytes)))
    ;; TODO(perf): use numeric indexing, instead of converting the whole
    ;; program to a list of bytes. Not a big deal right now as there's a
    ;; 24K limit on program size anyway.
    (if (null? data)
      []
      (let*
        ((opcode (car data))
         (name (vector-ref rev-opcodes opcode))
         (push-amt (push-code-amount opcode)))
        (cond
          ((not (symbol? name))
           (cons ['invalid opcode] (loop (cdr data))))
          (push-amt
           (let ()
             (def rest (cdr data))
             (def arg-bytes (take rest push-amt))
             (def arg-hex (hex-encode (list->u8vector arg-bytes)))
             (def arg-decimal (with-input-from-string (string-append "#x" arg-hex) read))
             (cons
               [name arg-decimal (string-append "0x" arg-hex)]
               (loop (drop rest push-amt)))))
          (else
            (cons name (loop (cdr data)))))))))

;; push-code-amount : Byte -> (Maybe UInt)
;;
;; Returns the length of the argument to the PUSH instruction with the provided
;; opcode.
(def (push-code-amount opcode)
  (def n (- opcode #x5f)) ;; PUSH0
  (and (<= 0 n 32) n))

(def (&byte a b)
  (segment-push! (Assembler-segment a) b))
(def (&bytes a b)
  (segment-push-bytes! (Assembler-segment a) b))
(def (&type a type x)
  (&bytes a ((.@ type .bytes<-) x)))
(def (&uint a u (n-bytes (uint-length-in-u8 u)))
  (check-argument-uint256 u)
  (check-argument-datum-length n-bytes)
  (check-argument (<= (integer-length u) (* 8 n-bytes)) "valid length for u" [n-bytes u])
  (segment-push-bytes! (Assembler-segment a) (uint->u8vector u big n-bytes)))
(def (&push a u (n-bytes (uint-length-in-u8 u)))
  (check-argument-datum-length n-bytes)
  (&byte a (+ #x5F n-bytes)) ;; PUSH0
  (&uint a u n-bytes))
(def (&push-bytes a bytes)
  (&push a (u8vector->uint bytes)))

(def (current-offset a)
  (Segment-fill-pointer (Assembler-segment a)))

(def (check-byte a offset value msg)
  (unless (= (u8vector-ref (Segment-bytes (Assembler-segment a)) offset) value)
    (error msg)))

;; TODO: should we mask off all but the n-bits lowest bits of actual?
(def (check-uint a n-bits offset expected err)
  (def actual (u8vector-uint-ref (Segment-bytes (Assembler-segment a))
                                 offset big (n-bits->n-u8 n-bits)))
  (unless (= actual expected)
    (err actual)))

(def (do-fixup a offset expr n-bits)
  (def value (eval-fixup-expression (Assembler-labels a) expr))
  (unless value
    (error "fixup has no computed value" offset expr n-bits value))
  (unless (and (<= 0 value) (<= (integer-length value) n-bits))
    (error "fixup has incorrect computed value" offset expr n-bits value))
  (u8vector-uint-set! (Segment-bytes (Assembler-segment a)) offset value big (n-bits->n-u8 n-bits))
  (hash-remove! (Assembler-fixups a) offset))

;; TODO: somehow check that fixup ranges don't overlap.
;; e.g. 32-bit fixup at address 10 and 8-bit fixup at address 12.
(def (&fixup a n-bits expr)
  (def offset (Segment-fill-pointer (Assembler-segment a)))
  (&uint a 0 (n-bits->n-u8 n-bits))
  (hash-put! (Assembler-fixups a) offset (cons expr n-bits)))

(def (&label a l (offset (current-offset a)))
  (def labels (Assembler-labels a))
  (def label-offset (hash-get labels l))
  (if label-offset
    (error "label already defined" l offset label-offset)
    (hash-put! labels l offset)))

(def rev-opcodes (make-vector 256))
(def opcodes (make-hash-table))

(defrule (define-ethereum-bytecodes (code symbol . _) ...)
  (begin
    (begin (def (symbol a) (&byte a code))
           (hash-put! opcodes 'symbol code)
           (vector-set! rev-opcodes code 'symbol)
           (export symbol)) ...))

;; For precise semantics, see evm.md in https://github.com/kframework/evm-semantics
;; Note: posting data is 16 gas / byte (4 if zero), used to be 68 for non-zero, might be 3 after EIP-4488.
(define-ethereum-bytecodes
  (#x00 STOP 0) ;; Halts execution (success, same as 0 0 RETURN)
  (#x01 ADD 3) ;; Addition operation.
  (#x02 MUL 5) ;; Multiplication operation.
  (#x03 SUB 3) ;; Subtraction operation
  (#x04 DIV 5) ;; Integer division operation
  (#x05 SDIV 5) ;; Signed integer division operation (truncated)
  (#x06 MOD 5) ;; Modulo remainder operation
  (#x07 SMOD 5) ;; Signed modulo remainder operation
  (#x08 ADDMOD 8) ;; Modulo addition operation
  (#x09 MULMOD 8) ;; Modulo multiplication operation
  (#x0a EXP 10 #t) ;; Exponential operation
  (#x0b SIGNEXTEND 5) ;; Extend length of two's complement signed integer
  ;; #x0c - #x0f  Unused
  (#x10 LT 3) ;; Less-than comparison
  (#x11 GT 3) ;; Greater-than comparison
  (#x12 SLT 3) ;; Signed less-than comparison
  (#x13 SGT 3) ;; Signed greater-than comparison
  (#x14 EQ 3) ;; Equality comparison
  (#x15 ISZERO 3) ;; Simple not operator
  (#x16 AND 3) ;; Bitwise AND operation
  (#x17 OR 3) ;; Bitwise OR operation
  (#x18 XOR 3) ;; Bitwise XOR operation
  (#x19 NOT 3) ;; Bitwise NOT operation
  (#x1a BYTE 3) ;; Retrieve single byte from word
  (#x1b SHL 3) ;; logical shift left (EIP-145)
  (#x1c SHR 3) ;; logical shift right (EIP-145)
  (#x1d SAR 3) ;; arithmetic shift right (EIP-145)
  ;; #x1e - #x1f  Unused
  (#x20 SHA3 30 #t) ;; Compute Keccak-256 hash. Cost: 30+6/word
  ;; #x21 - #x2f  Unused
  (#x30 ADDRESS 2) ;; Get address of currently executing account
  (#x31 BALANCE 2600 #t) ;; Get balance of the given account. 2600 since EIP-2929
  (#x32 ORIGIN 2) ;; Get execution origination address
  (#x33 CALLER 2) ;; Get caller address (current message sender, Solidity: msg.sender)
  (#x34 CALLVALUE 2) ;; Get deposited value by the instruction/transaction responsible for this execution
  (#x35 CALLDATALOAD 3) ;; Get input data of current environment
  (#x36 CALLDATASIZE 2 #t) ;; Get size of input data in current environment
  (#x37 CALLDATACOPY 3 #t) ;; Copy input data in current environment to memory 3*#word
  (#x38 CODESIZE 2) ;; Get size of code running in current environment
  (#x39 CODECOPY 3 #t) ;; Copy code running in current environment to memory
  (#x3a GASPRICE 2) ;; Get price of gas in current environment
  (#x3b EXTCODESIZE 2600 #t) ;; Get size of an account's code; 2600 since EIP-2929
  (#x3c EXTCODECOPY 2600 #t) ;; Copy an account's code to memory; 2600 since EIP-2929
  (#x3d RETURNDATASIZE 2) ;; Pushes the size of the return data buffer onto the stack -- EIP 21
  (#x3e RETURNDATACOPY 3) ;; Copies data from the return data buffer to memory -- EIP 21
  (#x3f EXTCODEHASH 2600 #t) ;; 2600 since EIP-2929
  (#x40 BLOCKHASH 20) ;; Get the hash of one of the 256 most recent complete blocks
  (#x41 COINBASE 2) ;; Get the block's beneficiary address
  (#x42 TIMESTAMP 2) ;; Get the block's timestamp
  (#x43 NUMBER 2) ;; Get the block's number
  (#x44 DIFFICULTY 2) ;; Get the block's difficulty
  (#x45 GASLIMIT 2) ;; Get the block's gas limit
  (#x46 CHAINID #t)
  (#x47 SELFBALANCE #t) ;; EIP-1884
  ;; #x48 - #x4f  Unused
  (#x50 POP 2) ;; Remove word from stack
  (#x51 MLOAD 3 #t) ;; Load word from memory
  (#x52 MSTORE 3 #t) ;; Save word to memory
  (#x53 MSTORE8 3) ;; Save byte to memory
  (#x54 SLOAD 2100) ;; Load word from storage 200, then 700 now 2100 in EIP-2929.
  (#x55 SSTORE 20000 #t #t) ;; Save word to storage. 5000 per write, 15000 extra for setting from 0 to non-0, 15000 refund resetting from non-0 to 0.
  (#x56 JUMP 8) ;; Alter the program counter
  (#x57 JUMPI 10) ;; Conditionally alter the program counter
  (#x58 GETPC 2) ;; Get the value of the program counter prior to the increment
  (#x59 MSIZE 2) ;; Get the size of active memory in bytes
  (#x5a GAS 2) ;; the amount of available gas, including the corresponding reduction the amount of available gas
  (#x5b JUMPDEST 1) ;; Mark a valid destination for jumps
  ;; #x5c - #x5e  Unused
  (#x5f PUSH0 2) ;; Place 0 on stack (since Shanghai upgrade, 2023)
  (#x60 PUSH1 3) ;; Place 1-byte item on stack
  (#x61 PUSH2 3) ;; Place 2-byte item on stack
  (#x62 PUSH3 3) ;; Place 3-byte item on stack
  (#x63 PUSH4 3) ;; Place 4-byte item on stack
  (#x64 PUSH5 3) ;; Place 5-byte item on stack
  (#x65 PUSH6 3) ;; Place 6-byte item on stack
  (#x66 PUSH7 3) ;; Place 7-byte item on stack
  (#x67 PUSH8 3) ;; Place 8-byte item on stack
  (#x68 PUSH9 3) ;; Place 9-byte item on stack
  (#x69 PUSH10 3) ;; Place 10-byte item on stack
  (#x6a PUSH11 3) ;; Place 11-byte item on stack
  (#x6b PUSH12 3) ;; Place 12-byte item on stack
  (#x6c PUSH13 3) ;; Place 13-byte item on stack
  (#x6d PUSH14 3) ;; Place 14-byte item on stack
  (#x6e PUSH15 3) ;; Place 15-byte item on stack
  (#x6f PUSH16 3) ;; Place 16-byte item on stack
  (#x70 PUSH17 3) ;; Place 17-byte item on stack
  (#x71 PUSH18 3) ;; Place 18-byte item on stack
  (#x72 PUSH19 3) ;; Place 19-byte item on stack
  (#x73 PUSH20 3) ;; Place 20-byte item on stack
  (#x74 PUSH21 3) ;; Place 21-byte item on stack
  (#x75 PUSH22 3) ;; Place 22-byte item on stack
  (#x76 PUSH23 3) ;; Place 23-byte item on stack
  (#x77 PUSH24 3) ;; Place 24-byte item on stack
  (#x78 PUSH25 3) ;; Place 25-byte item on stack
  (#x79 PUSH26 3) ;; Place 26-byte item on stack
  (#x7a PUSH27 3) ;; Place 27-byte item on stack
  (#x7b PUSH28 3) ;; Place 28-byte item on stack
  (#x7c PUSH29 3) ;; Place 29-byte item on stack
  (#x7d PUSH30 3) ;; Place 30-byte item on stack
  (#x7e PUSH31 3) ;; Place 31-byte item on stack
  (#x7f PUSH32 3) ;; Place 32-byte (full word) item on stack
  (#x80 DUP1 3) ;; Duplicate 1st stack item
  (#x81 DUP2 3) ;; Duplicate 2nd stack item
  (#x82 DUP3 3) ;; Duplicate 3rd stack item
  (#x83 DUP4 3) ;; Duplicate 4th stack item
  (#x84 DUP5 3) ;; Duplicate 5th stack item
  (#x85 DUP6 3) ;; Duplicate 6th stack item
  (#x86 DUP7 3) ;; Duplicate 7th stack item
  (#x87 DUP8 3) ;; Duplicate 8th stack item
  (#x88 DUP9 3) ;; Duplicate 9th stack item
  (#x89 DUP10 3) ;; Duplicate 10th stack item
  (#x8a DUP11 3) ;; Duplicate 11th stack item
  (#x8b DUP12 3) ;; Duplicate 12th stack item
  (#x8c DUP13 3) ;; Duplicate 13th stack item
  (#x8d DUP14 3) ;; Duplicate 14th stack item
  (#x8e DUP15 3) ;; Duplicate 15th stack item
  (#x8f DUP16 3) ;; Duplicate 16th stack item
  (#x90 SWAP1 3) ;; Exchange 1st and 2nd stack items
  (#x91 SWAP2 3) ;; Exchange 1st and 3rd stack items
  (#x92 SWAP3 3) ;; Exchange 1st and 4th stack items
  (#x93 SWAP4 3) ;; Exchange 1st and 5th stack items
  (#x94 SWAP5 3) ;; Exchange 1st and 6th stack items
  (#x95 SWAP6 3) ;; Exchange 1st and 7th stack items
  (#x96 SWAP7 3) ;; Exchange 1st and 8th stack items
  (#x97 SWAP8 3) ;; Exchange 1st and 9th stack items
  (#x98 SWAP9 3) ;; Exchange 1st and 10th stack items
  (#x99 SWAP10 3) ;; Exchange 1st and 11th stack items
  (#x9a SWAP11 3) ;; Exchange 1st and 12th stack items
  (#x9b SWAP12 3) ;; Exchange 1st and 13th stack items
  (#x9c SWAP13 3) ;; Exchange 1st and 14th stack items
  (#x9d SWAP14 3) ;; Exchange 1st and 15th stack items
  (#x9e SWAP15 3) ;; Exchange 1st and 16th stack items
  (#x9f SWAP16 3) ;; Exchange 1st and 17th stack items
  (#xa0 LOG0 375) ;; Append log record with no topics
  (#xa1 LOG1 750) ;; Append log record with one topic
  (#xa2 LOG2 1125) ;; Append log record with two topics
  (#xa3 LOG3 1500) ;; Append log record with three topics
  (#xa4 LOG4 1875) ;; Append log record with four topics
  ;; #xa5 - #xef  Unused
  (#xf0 CREATE 32000) ;; Create a new account with associated code
  (#xf1 CALL 2600 #t) ;; Message-call into an account; base 2600 rather than 700 since EIP-2929. Precompiled contracts still at 700. Only 100 on already accessed contracts.
  (#xf2 CALLCODE 2600 #t) ;; Message-call into this account with alternative account's code. 2600+
  (#xf3 RETURN 0) ;; Halt execution returning output data
  (#xf4 DELEGATECALL 2600 #t) ;; Message-call into this account with an alternative account's code, but persisting into this account with an alternative account's code. Gas 2600+
  (#xf5 CREATE2 Complicated) ;; Create a new account with deterministic address, EIP-1014 (Constantinople)
  ;; #xf6 - #xf9  Unused
  (#xfa STATICCALL 40) ;; Similar to CALL, but does not modify state. Gas 2600+
  ;; #xfb - #xfc  Unused
  (#xfd REVERT #t) ;; Stop execution and revert state changes, without consuming all provided gas and providing a reason
  (#xfe INVALID 0) ;; Designated invalid instruction
  (#xff SELFDESTRUCT 5000 #t)) ;; Halt execution, register account for deletion... phased out EIP-6049

(def (&jumpdest a l)
  (&label a l)
  (JUMPDEST a))
(def (&push-label a l)
  (if-let (x (hash-get (Assembler-fixups a) l))
          (&push a x) ;; labels in the past are optimized if short
          (&push-label2 a l))) ;; otherwise, leave it as a 16-bit fixup
(def (&push-label1 a l)
  (PUSH1 a)
  (&fixup a 8 l))
(def (&push-label2 a l)
  (PUSH2 a)
  (&fixup a 16 l))
(def (&jump1 a l)
  (&push-label1 a l)
  (JUMP a))
(def (&jump2 a l)
  (&push-label2 a l)
  (JUMP a))
(def (&jump a l)
  (&push-label a l)
  (JUMP a))
(def (&jumpi1 a l)
  (&push-label1 a l)
  (JUMPI a))
(def (&jumpi2 a l)
  (&push-label2 a l)
  (JUMPI a))
(def (&z a z use-once?: (use-once? #f))
  (set! z ((.@ UInt256 .normalize) z))
  (cond
   ;; Only an optimization if the number is used once
   ;; (actually, a number of times less than the number of bytes saved, or so)
   ((and use-once? (= 65535 (extract-bit-field 16 240 z)))
    (&z a (bitwise-not z))
    (NOT a))
   (else
    (let ((n-bytes (uint-length-in-u8 z)))
      (check-argument-datum-length n-bytes)
      (&byte a (+ #x5f n-bytes))
      (&bytes a (uint->u8vector z big n-bytes))))))

(def (&directive a directive)
  (cond
   ((exact-integer? directive) (&push a directive))
   ((u8vector? directive) (&push-bytes a directive))
   ((address? directive) (&push-bytes a (bytes<- Address directive)))
   ((procedure? directive) (directive a))
   ((pair? directive) (apply (car directive) a (cdr directive)))
   ((symbol? directive) (&push-label a directive))
   ((boolean? directive) (if directive (&push a 1) (&push a 0)))
   ((member directive '(#f #!void ())) (void))
   (else (error "invalid directive" directive))))

(def (&directives a directives)
  (for-each (cut &directive a <>) directives))
(def (&begin* l) (cut &directives <> l))
(defrule (&begin directive ...) (lambda (a) (&directives a [directive ...])))

(def generate-label-counter 0)
(def (generate-label (g 'g))
  (make-symbol g "_" (post-increment! generate-label-counter)))

;; BEWARE: The args will be evaluated right-to-left
(def (&call routine . args)
  (def ret (generate-label 'ret))
  (&begin ret (&begin* (reverse args)) routine JUMP [&jumpdest ret]))

;; Local memory can only be accessed 32-byte (or, for writes, also 1 byte) at a time,
;; and masking / merging is rather expensive, so for often-used stuff, it makes sense
;; to waste memory to save some gas. On the other hand, the cost of local memory is ultimately
;; quadratic in the total size, so for regular data (vs often-used global registers),
;; it pays to be compact.
;; Reading is cheap enough:
(def (&mload n-bytes)
  (check-argument (and (exact-integer? n-bytes) (<= 0 n-bytes 32)) "length for &mload" n-bytes)
  (cond
   ((zero? n-bytes) (&begin POP 0)) ;; [3B, 5G]
   ((= n-bytes 32) MLOAD) ;; [1B, 3G]
   (else (&begin MLOAD (&shr (- 256 (* 8 n-bytes))))))) ;; [4B, 9G]

(def (&mloadat addr (n-bytes 32))
  (when (object? n-bytes) ;; accept a fixed-size type descriptor
    (set! n-bytes (.@ n-bytes .length-in-bytes)))
  (check-argument (and (exact-integer? n-bytes) (<= 0 n-bytes 32)) "length for &mloadat" n-bytes)
  (cond
   ((zero? n-bytes) 0) ;; [2B, 3G]
   ((= n-bytes 32) (&begin addr MLOAD)) ;; [4B, 6G] or for small addresses [3B, 6G]
   (else (&begin addr MLOAD (&shr (- 256 (* 8 n-bytes))))))) ;; [7B, 12G] or for small addresses [6B, 12G]

(def (&mstore n-bytes)
  (check-argument (and (exact-integer? n-bytes) (<= 0 n-bytes 32)) "length for &mstore" n-bytes)
  (cond
   ((zero? n-bytes) (&begin POP POP)) ;; [2B, 4G]
   ((= n-bytes 1) MSTORE8) ;; [1B, 3G]
   ((= n-bytes 2) (&begin DUP2 (&shr 8) DUP2 MSTORE8 1 ADD MSTORE8)) ;; [10B, 24G]
   ;;(= n-bytes 3) (&begin DUP2 (&shr 16) DUP2 MSTORE8 1 ADD &mstore16)) ;; [19B, 45G]
   ((= n-bytes 32) MSTORE) ;; [1B, 3G]
   (else ;; [16B, 38G]
    (let (n-bits (* 8 n-bytes))
      ;;(&begin SWAP1 scratch0@ MSTORE DUP1 n-bytes ADD MLOAD scratch1@ MSTORE (- scratch1@ n-bytes) MLOAD SWAP1 MSTORE) ;; [17B, 39G]
      ;; [16B, 38G] -- note that we could skip the ending POP
      (&begin DUP1 n-bytes ADD MLOAD (&shr n-bits) DUP3 (&shl (- 256 n-bits)) OR SWAP1 MSTORE POP)))))

(def (&mstoreat addr (n-bytes 32))
  (check-argument (and (exact-integer? n-bytes) (<= 0 n-bytes 32)) "length for &mstoreat" n-bytes)
  (cond
   ((= n-bytes 32) (&begin addr MSTORE)) ;; [4B, 6G] or for small addresses [3B, 6G]
   ((zero? n-bytes) (&begin POP)) ;; [1B, 2G]
   ((= n-bytes 1) (&begin addr MSTORE8)) ;; [4B, 6G] or for small addresses [3B, 6G]
   ((= n-bytes 2) (&begin DUP1 (&shr 8) addr MSTORE8 (1+ addr) MSTORE8)) ;; [12B, 21G]
   ;;((= n-bytes 3) (&begin DUP1 (&shr 16) addr MSTORE8 (&mstore16at (1+ addr)))) ;; [20B, 36G], suboptimal
   (else (let (n-bits (* 8 n-bytes)) ;; [15B, 27G]
           (&begin (&shl (- 256 n-bits)) (+ addr n-bytes) MLOAD (&shr n-bits) OR addr MSTORE)))))

;; Like &mstore, but is allowed (not obliged) to overwrite memory after it with padding bytes
(def (&mstore/overwrite-after n-bytes)
  (check-argument (and (exact-integer? n-bytes) (<= 0 n-bytes 32))
                  "length for &mstore/overwrite-after" n-bytes)
  (cond
   ((= n-bytes 32) MSTORE) ;; [1B, 3G]
   ((= n-bytes 1) MSTORE8) ;; [1B, 3G]
   ((zero? n-bytes) MSTORE8) ;; [1B, 3G]
   (else (&begin SWAP1 (&shl (- 256 (* 8 n-bytes))) SWAP1 MSTORE)))) ;; [6B, 15G]

;; Like &mstoreat, but is allowed (not obliged) to overwrite memory after it with padding bytes
(def (&mstoreat/overwrite-after addr (n-bytes 32))
  (check-argument (and (exact-integer? n-bytes) (<= 0 n-bytes 32))
                  "length for &mstoreat/overwrite-after" n-bytes)
  (cond
   ((= n-bytes 32) (&begin addr MSTORE)) ;; [4B, 6G] or for small addresses [3B, 6G]
   ((= n-bytes 1) (&begin addr MSTORE8)) ;; [4B, 6G] or for small addresses [3B, 6G]
   ((zero? n-bytes) (&begin POP)) ;; [1B, 2G]
   (else (&begin (&shl (- 256 (* 8 n-bytes))) addr MSTORE)))) ;; [7B, 12G]

;; Shifts by constant number of bits
;; TODO: somehow detect whether EIP-145 is activated, and use SHL/SHR/SAR when it is.
(def (&shl n)
  (if (ethereum-eip145)
    (&begin n SHL)
    (&begin (arithmetic-shift 1 n) MUL))) ;; pre-EIP-145 version
(def (&shr n)
  (if (ethereum-eip145)
    (&begin n SHR)
    (&begin (arithmetic-shift 1 n) SWAP1 DIV))) ;; pre-EIP-145 version
(def (&sar n)
  (if (ethereum-eip145)
    (&begin n SAR)
    (&begin (arithmetic-shift 1 n) SWAP1 SDIV))) ;; pre-EIP-145 version

;; TESTING STATUS: only partially tested, as part of batching
(def (&unless &cond &then)
  (def unless-label (generate-label "&endunless"))
  (&begin &cond [&jumpi2 unless-label]
          &then
          [&jumpdest unless-label]))

;; TESTING STATUS: wholly tested.
(def (&if &cond &then &else)
  (def then-label (generate-label "&then"))
  (def endif-label (generate-label "&endif"))
  (&begin &cond [&jumpi2 then-label]
          &else [&jump2 endif-label]
          [&jumpdest then-label] &then
          [&jumpdest endif-label]))

;; TESTING STATUS: wholly tested.
(def (&switch comparison-value cases)
  (def reducer
    (λ (current-case next-case)
      (match current-case
        ([case-value case-code-block]
          (&if (&begin comparison-value case-value EQ)
            (&begin* case-code-block)
            next-case))
        (else
          (error "Invalid case in switch expression: " current-case)))))
  (def nested-ifs (cps-foldl reducer cases))
  (nested-ifs (&begin 0 DUP1 REVERT)))

;; A fold where the accumulator is a continuation.
(def (cps-foldl reducer lst)
  (foldl
    (λ (cur continuation)
      (λ (next) (continuation (reducer cur next))))
    identity
    lst))

;; Calling convention is arguments at top of stack followed by return address.
;; Use &call to invoke functions defined this way.
(def (&define-small-function label body-bytes (n-return-values 1))
  (&begin
    [&jumpdest label]
    body-bytes
    (&swap-n n-return-values) JUMP))

(def (&swap-n n)
  (check-argument (<= 1 n 16) "swap number" n)
  (match n
    (1 SWAP1)
    (2 SWAP2)
    (3 SWAP3)
    (4 SWAP4)
    (5 SWAP5)
    (6 SWAP6)
    (7 SWAP7)
    (8 SWAP8)
    (9 SWAP9)
    (10 SWAP10)
    (11 SWAP11)
    (12 SWAP12)
    (13 SWAP13)
    (14 SWAP14)
    (15 SWAP15)
    (16 SWAP16)))
