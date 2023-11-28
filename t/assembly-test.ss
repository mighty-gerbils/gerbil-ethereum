(export #t)

(import :std/sort
        :std/iter
        :std/test
        :std/srfi/1
        :std/misc/list
        :std/misc/number
        :clan/pure/dict/assq
        (only-in :std/misc/bytes big uint->u8vector)
        ../assembly)

;; jumplabel-len : UInt
;; jump2-len : UInt
(def &jumpdest-len 1)
(def &jump2-len 4)

;; test-labels : [Assqof Symbol UInt] -> TestSuite
(def (test-labels names/uints)
  (unless (pair? names/uints) (error 'test-labels "expected at least one label"))
  (def n-labels (length names/uints))
  (let ((names/uints (sort names/uints (lambda (x y) (< (cdr x) (cdr y))))))
   (def names (map car names/uints))
   (def uints (map cdr names/uints))
   (def n-jumps (apply max (map cdr names/uints)))
   (def uints/names (map pair-flip names/uints))
   (def (uint->i-code uint)
     (check-argument-uint uint)
     ; if directly on one of the uints, it's on the JUMPDEST, not after
     (def n-labels-before (count (lambda (n) (< n uint)) uints))
     (+ (* uint &jump2-len) (* n-labels-before &jumpdest-len)))
   (def instrs
     (flatten1
      (for/collect ((i (in-range (1+ n-jumps))))
        (def instr-jump [&jump2 (list-ref names (modulo i n-labels))])
        (def ?name-label (assq i uints/names))
        (cond (?name-label [[&jumpdest (cdr ?name-label)] instr-jump])
              (else        [instr-jump])))))
   (def code (assemble/bytes instrs))
   (test-case "check code length"
     (check-equal? (u8vector-length code) (+ (uint->i-code (last uints)) &jumpdest-len &jump2-len)))
   (test-case "check JUMPDEST on every &jumpdest"
     (for ((uint uints))
       (def i-code (uint->i-code uint))
       (def j-code (+ i-code &jumpdest-len))
       ;; #x5b is JUMPDEST
       (check-equal? (subu8vector code i-code j-code) #u8(#x5b))))
   (test-case "check &jump2 code pointers"
     (for ((i (in-range n-jumps)))
       (def uint-jump (list-ref uints (modulo i n-labels)))
       (def i-code-dest (uint->i-code uint-jump))
       (def i-code-jump (+ (uint->i-code i) (if (member i uints) &jumpdest-len 0)))
       (def j-code-jump (+ i-code-jump &jump2-len))
       (check-equal? (subu8vector code i-code-jump j-code-jump)
                     ;; #x61 is PUSH2, #x56 is JUMP
                     (u8vector-append #u8(#x61) (uint->u8vector i-code-dest big 2) #u8(#x56)))))))

;; pair-flip : (cons a b) -> (cons b a)
(def (pair-flip p) (with ((cons a b) p) (cons b a)))

(def (check-disassemble input output)
  (check-equal?
    (disassemble (assemble/bytes input))
    output))

(def assembly-test
  (test-suite "test suite for ethereum/assembly"
    (test-labels '((a . 13) (b . 8) (c . 21) (d . 0) (e . 34) (f . 3) (g . 5)))
    (test-case "Check that assemble & disassemble agree"
      (for-each
        (cut apply check-disassemble <>)
        [[[ADD MUL 8 SUB]
          '(ADD MUL (PUSH1 8 "0x08") SUB)]
         [[SHR 520 1000000 SWAP1]
          '(SHR (PUSH2 520 "0x0208") (PUSH3 1000000 "0x0f4240") SWAP1)]]))
    (test-case "Check disassemble on large pushes for small values."
      (check-equal?
        (disassemble (list->u8vector [(hash-ref opcodes 'PUSH3) 0 0 42]))
        '((PUSH3 42 "0x00002a"))))))
