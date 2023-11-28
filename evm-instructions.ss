(export #t)
(import
  :gerbil/gambit
  :std/assert
  :std/format
  :std/misc/list :std/misc/number
  :std/srfi/1 (only-in :std/srfi/141 floor/)
  :std/sugar
  :clan/base
  :clan/poo/object (only-in :clan/poo/mop Type)
  ./assembly ./ethereum ./evm-runtime ./types)

;; --------------------------------
;; General purpose EVM instructions
;; --------------------------------
;; Extends instruction set.
;; Examples include:
;; - Storing bytes larger than EVM Word
;; - Storing to memory with reference
;; - Load from memory with reference
;;
;; NOTE: Regarding reference types:
;; The start-offset and length are stored on the stack,
;; and the contents are stored in memory.
;; The start-offset and length can then be used
;; to access contents located in memory.
;;
;; Since bytes are frequently created,
;; contents are stored without padding,
;; since cost of memory is quadratic.

;;
;; -------------
;; Byte encoding
;; -------------
;; Instructions encode bytes as big-endian, left-padding them.
;;
;; For example, given an array of two bytes: #u8(#x11 #xff).
;;
;; After &push/any-size:
;; The topmost word on the EVM Stack will be:
;; #x00000000000000000000000000000000000000000000000000000000000011ff
;;
;; After (&mstore/free/any-size 2):
;; "0x11" will be located at mem[offset],
;; "0xff" located at mem[offset+1].
;;
;; For larger bytes (larger than a EVM word),
;; we partition them into smaller chunks,
;; less than or equal to a EVM word.
;; We use the notation: "part[n]", to denote the partition index.
;; E.g. 65-bytes -> | 32-bytes | 32-bytes | 1-byte  |
;;                  | part0    | part1    | part2 |
(defrule (EVM-WORD-SIZE) 32)

;; -----------------------
;; Instruction definitions
;; -----------------------

;; For contracts, to store values in memory,
;; we first have to marshal them to the stack.
;; This instruction serves that purpose.
;;
;; This allows us to PUSH a non-empty sequence of bytes
;; which are variably sized onto the stack.
;; PUSH[1-32] instructions cannot handle more than 32 bytes,
;; which this instruction supports,
;; by chunking up larger byte sequences into 32 bytes or less.
;;
;; stack input: -
;; stack output: part0 part1 ... partn
;; (Thunk part0 part1 ... partn <-) <- Bytes
(def (&push/any-size bytes)
  (&push-0/any-size bytes 0 (u8vector-length bytes)))

(def (&push-0/any-size bytes start total-bytes)
  (def end (min total-bytes (+ start (EVM-WORD-SIZE))))
  (when (< start end)
    (let ()
      (def bytes<=evm-word-size (subu8vector bytes start end))
      (&begin (&push-0/any-size bytes end total-bytes)
              [&push-bytes bytes<=evm-word-size]))))

;; Helper function - Make list of word-sizes for byte partitions.
;;
;; E.g. If we have 65 bytes, we need to store these in 32, 32, 1 sized chunks,
;; since EVM Word is 32 bytes.
;; this function computes the list of chunk sizes:
;; (sizes/word-size<-size 65 32) -> [32 32 1]
;;
;; (ListOf Size) <- Size WordSize
(def (sizes/word-size<-size size (word-size (EVM-WORD-SIZE)))
  (defvalues (n-words rem) (floor/ size word-size))
  (def words (make-list n-words word-size))
  (if (zero? rem) words [words ... rem]))

;; NOTE: Uses brk@ for offset via &brk-cons, dependent on EVM memory layout.
;; stack input:  part0 part1 ... partn
;; stack output: -
;; mem out:      part0 part1 ... partn
;; (Thunk <- part0 part1 ... partn) <- Size
(def (&mstore/free/any-size size)
  (assert-bytes-at-least! size 1)
  (def sizes/base/evm-word-size (sizes/word-size<-size size))
  (&begin (map &brk-cons sizes/base/evm-word-size) ...))

;; stack in:  offset size part0 part1 ... partn
;; stack out: -
;; mem out:   part0 part1 ... partn
;; TODO: (def (&mstore/any-size))

;; stack input:  offset length part0 part1 ... partn
;; stack output: offset length
;; mem out:      part0 part1 ... partn
;; (def (&mstore/ref/any-size)
;;   ;;TODO:
;;   )

;; stack in:  length part0 part1 ... partn
;; stack out: offset length
;; mem out:   part0 part1 ... partn
;; (def (&mstoreat/ref/any-size offset)
;;   (&begin offset (&mstore/ref/any-size)))

;; Helper function - Make list of relative offsets and sizes for partitions.
;; Used by `&mload/any-size` to obtain memory ranges for storing partitions.
;; E.g. (offsets-and-sizes<-size 65) -> [[0 (EVM-WORD-SIZE)] [32 32] [64 1]]
;; (ListOf (List RelativeOffset Size)) <- UInt
(def (offsets-and-sizes<-size size)
  (def sizes (sizes/word-size<-size size))
  (def relative-offsets (iota (length sizes) 0 (EVM-WORD-SIZE)))
  (zip relative-offsets sizes))

;; Given relative-offset and size,
;; generates EVM code to:
;; - load specified bytes
;; - maintain start-offset for loading next segment
;;
;; stack input: start-offset
;; stack output: start-offset bytes[offset:end]
;;   where offset = start-offset+relative-offset
;;         end    = offset+size
;; (Thunk start-offset bytes[offset:end] <- start-offset) <- (List RelativeOffset Size)
(def &mload-1/any-size
  (match <>
    ([relative-offset size]
     (&begin                                    ; start-offset
      DUP1 #|start-offset|# relative-offset ADD ; offset       start-offset
      (&mload size) SWAP1))))                   ; start-offset bytes[offset:end]

;; stack input:  start-offset
;; stack output: part0 part1 ... partn
;; (Thunk part0 ... partn <- start-offset) <- Size
(def (&mload/any-size size)
  (assert-bytes-at-least! size 0)
  (match (offsets-and-sizes<-size size)
    ;; = 0 bytes
    ([] (&begin POP)
     )
    ;; > 0 bytes
    ([[_ start-size] . rest]
     (&begin
      (map &mload-1/any-size (reverse rest)) ...
      (&mload start-size)))))

;; stack in:  -
;; mem in:    part0 part1 ... partn
;; stack out: part0 part1 ... partn
;; (Thunk part0 ... partn <-) <- Offset Size
(def (&mloadat/any-size offset length-size)
  (&begin offset (&mload/any-size length-size)))

;; stack in:  offset length
;; mem in:    part0 part1 ... partn
;; stack out: part0 part1 ... partn
;;
;; Procedure:
;; 1. Find last partition (partn) length (partn/l).
;;    partn/l = (modulo length 32)
;; 2. Find partn relative offset (partn/ro).
;;    partn/ro = length - partn/l
;; 3. Find partn offset (partn/o)
;;    partn/o = offset + relative-offset
;; 4. Load partn with partn/o, partn/l
;; 5. Initialize: current-part = partn
;; 6. if current-part/ro == 0:
;;        end
;;    else:
;;        next-part/ro = current-part/ro - 32
;;        next-part/o = offset + next-part/ro
;;        LOAD (next-part/o, 32)
;;        JUMP to step 6.
;;
;; (Thunk part0 part1 ... partn <- offset length) <-
(def (&mload/ref/any-size)
  ;; FIXME: gen unique labels
  ;; (def lw-start (generate-label 'lw-start))
  ;; (def end (generate-label 'end))
  (&begin                    ; -- offset length
    (&mload-tail/ref/any-size) ; offset partn/ro partn

    [&jumpdest '&lw-start] ; offset partn/ro partn

    ;; If partx/ro == 0

    ;; Yes -> End
    DUP2 #|partx/ro|# ISZERO ; partx/ro==0
    [&jumpi1 '&end]          ; offset current-part/ro current-part ... partn

    ;; No -> Continue
    ;; Find partx/new/ro
    (&mload-next-part/ref/any-size) ; offset next-part/ro next-part current-part ... partn

    ;; Jump to LW-START
    [&jump '&lw-start]

    [&jumpdest '&end] ; END ; offset partx/ro part0 part1 ... partn
    POP POP                 ; part0 part1 ... partn
    ))

;; (Thunk offset partn/ro partn <- offset length) <-
(def (&mload-tail/ref/any-size)                 ; -- offset length
  (&begin
   (&if (&begin DUP2 ISZERO) []
        (&begin
          ;; Find partn/l
          SWAP1 #|length|# 32 DUP2 #|length|# MOD      ; -- partn/l length offset

          ;; Find partn/ro
          SWAP1 #|length|# DUP2 #|partn/l|# SWAP1 SUB  ; -- partn/ro partn/l offset
          SWAP1                                        ; -- partn/l partn/ro offset
                                                        ; NOTE: partn/ro, offset are state vars
                                                        ; hence we store them at end of stack

          ;; Find partn/o
          DUP3 #|offset|# DUP3 #|partn/ro|# ADD        ; -- partn/o partn/l partn/ro offset

          ;; Load partn
          (&mload/ref)                                 ; partn partn/ro offset
          SWAP2                                        ; offset partn/ro partn
                                                        ; NOTE: partitions will occupy lowest region of the stack
                                                        ; as they are return values.
          ))))

; stack in:  offset current-part/ro
; stack out: offset next-part/ro next-part
(def (&mload-next-part/ref/any-size)
  (&begin                                  ; offset current-part/ro
    SWAP1 #|current-part/ro|# 32 SWAP1 SUB ; next-part/ro offset

    ;; Find next-part/o
    DUP2 #|offset|# DUP2 #|next-part/ro|# ADD ; next-part/o next-part/ro offset

    ;; Load next-part
    MLOAD ; next-part next-part/ro offset
    SWAP2 ; offset next-part/ro next-part
    ))

;; stack in:  length
;; mem in:    part0 part1 ... partn
;; stack out: part0 part1 ... partn
;; (def (&mloadat/ref/any-size offset)
;;   (&begin offset (&mload/ref/any-size)))

;; stack in:
;; mem in:    part0 part1 ... partn
;; stack out: offset length part0 part1 ... partn
;; TODO: (def (&mloadat/ref/known-size offset size))
;; Optimization since we can generate offsets statically,
;; rather than only during evm's runtime.

;; stack in:  offset length
;; stack out: <n-bytes>
(def (&mload/ref)
  ;; FIXME: Runtime assertions: (<= 1 n-bytes 32) - see: &safe-add
  (&begin                         ; offset length
    MLOAD                         ; value/padded length
    SWAP1                         ; length value/padded
    8 MUL #|length-bits|# 256 SUB ; shift/unpad value/padded
    SHR                           ; value ; FIXME: pre-EIP-145 compat - see: &shr
    ))

;; Batch 2
;; TODO calldata/any-size
;; TODO memcpy

;; ----------------
;; Shared Utilities
;; ----------------

;; FIXME: there should be some upper bound for the length of bytes,
;; where it will be unfeasible due to stack/memory/gas constraints to push/store/load bytes.
(def (assert-bytes-at-least! total-bytes lower-bound)
  (assert!
    (<= lower-bound total-bytes)
    (format "total bytes: ~d should be more than ~d" total-bytes lower-bound)))



;; load size bytes from pointer at calldataptr, increment calldataptr by size
(def (&calldata-load-increment (size 32)) ;; calldataptr --> data calldataptr+32
  (check-argument-datum-length size)
  (cond
   ((zero? size) (&begin 0)) ;; 1b 2g
   ((= size 32) (&begin DUP1 32 ADD SWAP1 CALLDATALOAD)) ;; 6b 14g
   (else (&begin DUP1 size ADD SWAP1 CALLDATALOAD (- 256 (* 8 size)) SHR)))) ;; 9b 19g

(def (calldata-load-varint-increment) ;; 22b 49g
  (&begin ; -- calldataptr
   DUP1 CALLDATALOAD ;; -- len calldataptr ;; 2b 6g
   SWAP1 1 ADD ;; calldataptr+1 len ;; 4b 9g
   DUP2 DUP2 ADD ;; calldataptr+1 len ;; 3b 9g
   SWAP1 MLOAD ;; datapad calldataptr+1+len len ;; 2b 6g
   SWAP1 SWAP2 8 MUL 256 SUB SHR)) ;; data calldataptr+1+len ;; 9b 19g
#|
;; Do it with code vector? Nah, it adds up to 54 instead of 49... JUMP has too much overhead;
;; unless it's used as a shared function, in which case it's still 54 but with more expensive rivals
  (&begin 'ret ;; -- ret len calldataptr ;; 2b 2g ;; push the return address
     DUP2 11 MUL 'base ADD JUMP ;; 9b 21g ;;
     (&label 'base)
     [repeat 32 times, with small optimization for n=0]
     JUMPDEST SWAP1 DUP3 ADD SWAP2 CALLDATALOAD (- 256 (* 8 n)) SHR SWAP1 JUMP ;; 11b 32g
     (&jumpdest 'ret)) ;; 1
|#
