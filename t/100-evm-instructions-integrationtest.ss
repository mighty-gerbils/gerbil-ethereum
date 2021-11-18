(export #t)

(import
  :std/test :clan/number :clan/poo/object
  ../types ../assembly ../evm-runtime
  ../testing ../evm-instructions.ss
  ./10-json-rpc-integrationtest)

;; Initializes free memory pointer
(def &init-brk
  (&begin #x20 (&mstoreat brk@ 32)))

;; Stores free memory pointer at free memory location for returning
(def &store-brk
  (&begin (&mloadat brk@) DUP1 MSTORE))

(def 100-evm-instructions-integrationtest
  (test-suite "integration tests for evm instructions"
    (test-case "EVM-type: &mstore/free/any-size, size = 1"
      (evm-test [] (&begin
                    &init-brk
                    (&push/any-size #u8(1))
                    (&mstore/free/any-size 1)
                    &store-brk
                    )
                [[Bytes1 . #u8(1)]
                 [UInt256 . 33]]
                result-in-memory?: #t
                result-start: #x20))

    (test-case "EVM-type: &mstore/free/any-size, size = 1, left-padded for topmost word on EVM stack"
      (evm-test [] (&begin
                    &init-brk
                    (&push/any-size #u8(1))
                    (&mstore/free/any-size 32)
                    &store-brk
                    )
                [[Bytes32 . #u8(0 0 0 0  0 0 0 0  0 0 0 0  0 0 0 0
                                0 0 0 0  0 0 0 0  0 0 0 0  0 0 0 1
                                )]
                 [UInt256 . 64]]
                result-in-memory?: #t
                result-start: #x20))

    (test-case "EVM-type: &mstore/free/any-size, size = 2"
      (evm-test [] (&begin
                    &init-brk
                    (&push/any-size #u8(#x11 #xff))
                    (&mstore/free/any-size 2)
                    &store-brk
                    )
                [[Bytes2 . #u8(#x11 #xff)]
                 [UInt256 . 34]]
                result-in-memory?: #t
                result-start: #x20))

    (test-case "EVM-type: &mstore/free/any-size, size = 2, left-padded for topmost word on EVM stack"
      (evm-test [] (&begin
                    &init-brk
                    (&push/any-size #u8(#x11 #xff))
                    (&mstore/free/any-size 32)
                    &store-brk
                    )
                [[Bytes32 . #u8(0 0 0 0  0 0 0 0  0 0 0 0  0 0 0 0
                                0 0 0 0  0 0 0 0  0 0 0 0  0 0 #x11 #xff
                                )]
                 [UInt256 . 64]]
                result-in-memory?: #t
                result-start: #x20))

    (test-case "EVM-type: &mstore/free/any-size, size = 65"
      (def 65-bytes (list->u8vector (make-list 65 65)))
      (u8vector-set! 65-bytes 0 66)
      (u8vector-set! 65-bytes 32 67)
      (u8vector-set! 65-bytes 64 68)

      (def 65-bytes/0-32  (subu8vector 65-bytes 0  32))
      (def 65-bytes/32-64 (subu8vector 65-bytes 32 64))
      (def 65-bytes/64-65 (subu8vector 65-bytes 64 65))

      (evm-test [] (&begin
                    &init-brk
                    (&push/any-size 65-bytes)
                    (&mstore/free/any-size 65)
                    &store-brk
                    )
                [[Bytes32 . 65-bytes/0-32]
                 [Bytes32 . 65-bytes/32-64]
                 [Bytes1  . 65-bytes/64-65]
                 [UInt256 . 97] ; 32 (free mem ptr) + 65 (str65) = 97
                 ]
                result-in-memory?: #t
                result-start: #x20))

    (test-case "EVM-type &mload/free/any-size, size = 65"
      (def 65-bytes (list->u8vector (make-list 65 65)))
      (u8vector-set! 65-bytes 0 66)
      (u8vector-set! 65-bytes 32 67)
      (u8vector-set! 65-bytes 64 68)

      (def 65-bytes/0-32  (subu8vector 65-bytes 0  32))
      (def 65-bytes/32-64 (subu8vector 65-bytes 32 64))
      (def 65-bytes/64-65 (subu8vector 65-bytes 64 65))

      (evm-test [] (&begin
                    &init-brk

                    ;; Store str65 in mem
                    (&push/any-size 65-bytes)    ; bytes[0-32] bytes[32-64] bytes[64-65]
                    (&mstore/free/any-size 65)   ; -
                    (&mloadat/any-size #x20 65)  ; bytes[0-32] bytes[32-64] bytes[64-65]
                    (&mstore/free/any-size 65)   ; -

                    &store-brk
                    )

                 ;; Original 65-bytes
                [[Bytes32 . 65-bytes/0-32]
                 [Bytes32 . 65-bytes/32-64]
                 [Bytes1  . 65-bytes/64-65]

                 ;; Duplicate 65-bytes
                 [Bytes32 . 65-bytes/0-32]
                 [Bytes32 . 65-bytes/32-64]
                 [Bytes1  . 65-bytes/64-65]

                 ;; freememptr: 32 + 65 + 65 = 162
                 [UInt256 . 162        ]
                 ]
                result-in-memory?: #t
                result-start: #x20)
      )

    ;; Dependent on network config initialized during integration tests
    (test-case "&mload/any-size assembled, size = 65"
      (def &load65/actual (assemble/bytes (&mload/any-size 65)))
      (def &load65/expected
           (assemble/bytes (&begin            ; -- offset
                            ;; Load last string segment
                            DUP1 64 ADD       ; -- offset+64   offset
                            (&mload 1) SWAP1  ; -- offset      bytes[64-65]

                            ;; Load second string segment
                            DUP1 32 ADD       ; -- offset+32   offset       bytes[64-65]
                            (&mload 32) SWAP1 ; -- offset      bytes[32-64] bytes[64-65]

                            ;; Load first string segment
                            (&mload 32))))    ; -- bytes[0-32] bytes[32-64] bytes[64-65]
      (check-equal? &load65/actual &load65/expected))))
