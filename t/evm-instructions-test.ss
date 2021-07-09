(export #t)

(import
  :std/test :clan/poo/object
  ../assembly ../evm-instructions ../types)

;; Verify assembled bytecode of instruction(s)
(def (check-inst? i/actual i/expected)
  (check-equal? (assemble/bytes i/actual) (assemble/bytes i/expected)))

;; NOTE: Boxed/Unboxed stack<-mem (load) methods are dependent
;; on EVM network (eip145).
;; Hence they are tested in integration tests.
(def evm-instructions-test
  (test-suite "test suite for evm-instructions"
    (test-case "&push/any-size <= 32"
      (def 1-byte #u8(1))
      (check-inst? (&push/any-size 1-byte) [1-byte])

      (def 5-bytes #u8(104 101 108 108 111))
      (check-inst? (&push/any-size 5-bytes) [5-bytes])

      (def 32-bytes (list->u8vector (make-list 32 65)))
      (check-inst? (&push/any-size 32-bytes) [32-bytes])
      )

    (test-case "&push/any-size > 32"
      (def 65-bytes (list->u8vector (make-list 65 65)))
      (u8vector-set! 65-bytes 0 66)  ; bytes/0-32:  66 65 65 ... 65
      (u8vector-set! 65-bytes 32 67) ; bytes/32-64: 67 65 65 ... 65
      (u8vector-set! 65-bytes 64 68) ; bytes/64-65: 68

      (check-inst? (&push/any-size 65-bytes)
                   [(subu8vector 65-bytes 64 65) ; bytes/64-65: 68
                    (subu8vector 65-bytes 32 64) ; bytes/32-64: 67 65 65 ... 65
                    (subu8vector 65-bytes 0  32) ; bytes/0-32:  66 65 65 ... 65
                    ]))))
