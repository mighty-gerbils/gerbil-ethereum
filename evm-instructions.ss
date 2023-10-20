(export #t)
(import
  :gerbil/gambit
  :std/assert :std/format
  :std/misc/list :std/misc/number
  :std/srfi/1 (only-in :std/srfi/141 floor/)
  :std/sugar
  :clan/base
  :clan/poo/object (only-in :clan/poo/mop Type)
  ./assembly ./ethereum ./evm-runtime)

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
;; and the contents are stored compactly in memory.
;; The start-offset and length can then be used
;; to access contents located in memory.
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
(def (&mstore/free/any-size size)
  (assert-bytes-at-least! size 1)
  (def sizes/base/evm-word-size (sizes/word-size<-size size))
  (&begin (map &brk-cons sizes/base/evm-word-size) ...))

;; Helper function - Make list of relative offsets and sizes for partitions.
;; Used by `&mload/any-size` to obtain memory ranges for storing partitions.
;; E.g. (offsets-and-sizes<-size 65) -> [[0 (EVM-WORD-SIZE)] [32 32] [64 1]]
;; (ListOf (List RelativeOffset Size)) <- Nat
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

;; ----------------
;; Shared Utilities
;; ----------------

;; FIXME: there should be some upper bound for the length of bytes,
;; where it will be unfeasible due to stack/memory/gas constraints to push/store/load bytes.
(def (assert-bytes-at-least! total-bytes lower-bound)
  (assert!
    (<= lower-bound total-bytes)
    (format "total bytes: ~d should be more than ~d" total-bytes lower-bound)))
