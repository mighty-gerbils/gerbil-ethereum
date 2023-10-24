(export #t)

(import
  :std/test :std/misc/number :clan/poo/object
  ../types ../assembly ../evm-runtime
  ../testing ../evm-instructions.ss
  ./10-json-rpc-integrationtest)

;; Initializes free memory pointer
(def &init-brk
  (&begin #x20 (&mstoreat brk@ 32)))

;; Stores free memory pointer at free memory location for returning
(def &store-brk
  (&begin (&mloadat brk@) DUP1 MSTORE))

(def 99-evm-instructions-integrationtest
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

    (test-case "EVM-type &mload/ref, size = 1"
      (def offset 0)
      (def len 1)
      (evm-test [] (&begin
                    ;; Store str65 in mem
                    (&push/any-size #u8(1))
                    (&mstoreat offset len)
                    len offset (&mload/ref)
                    1 ADD
                    (&mstoreat offset len)
                    )

                [[Bytes1 . #u8(2)]
                 ]
                result-in-memory?: #t
                result-start: #x00)
      )

    (test-case "EVM-type &mload/ref, size = 2"
      (def offset 0)
      (def len 2)
      (evm-test [] (&begin
                    ;; Store str65 in mem
                    (&push/any-size #u8(1 2))
                    (&mstoreat offset len)
                    len offset (&mload/ref)
                    #u8(1 1) ADD
                    (&mstoreat offset len)
                    )

                [[Bytes2 . #u8(2 3)]
                 ]
                result-in-memory?: #t
                result-start: #x00)
      )

    (test-case "EVM-type &mload/ref/any-size, size = 0, should not push values to stack"
      (evm-test [] (&begin
                    (&push/any-size #u8(1))   ; 1
                    0 0 (&mload/ref/any-size) ; 1 ; no values should be pushed onto stack
                    (&mstoreat 0 1)
                    )

                [[Bytes1 . #u8(1)]
                 ]
                result-in-memory?: #t
                result-start: #x00)
      )

    (test-case "EVM-type &mload/ref/any-size, size = 1"
      (def offset 0)
      (def len 1)
      (evm-test [] (&begin
                    ;; Store str65 in mem
                    (&push/any-size #u8(1))
                    (&mstoreat offset len)
                    ;; 1
                    len offset (&mload/ref/any-size) ; 1
                    ;; len offset (&mload-tail/ref/any-size) POP POP ; 1
                    1 ADD
                    (&mstoreat offset len)
                    )

                [[Bytes1 . #u8(2)]
                 ]
                result-in-memory?: #t
                result-start: #x00)
      )

    (test-case "EVM-type &mload/ref/any-size, size = 2"
      (def offset 0)
      (def len 2)
      (evm-test [] (&begin
                    ;; Store str65 in mem
                    (&push/any-size #u8(1 2))
                    (&mstoreat offset len)
                    ;; 1
                    len offset (&mload/ref/any-size) ; 1
                    ;; len offset (&mload-tail/ref/any-size) POP POP ; 1
                    #u8(1 1) ADD
                    (&mstoreat offset len)
                    )

                [[Bytes2 . #u8(2 3)]
                 ]
                result-in-memory?: #t
                result-start: #x00)
      )

    (test-case "EVM-type &mload/ref/any-size, size = 31"
      (def offset 0)
      (def len 31)
      (evm-test [] (&begin
                    ;; Store str65 in mem
                    (&push/any-size #u8(1 2))
                    (&mstoreat offset len)
                    ;; 1
                    len offset (&mload/ref/any-size) ; 1
                    ;; len offset (&mload-tail/ref/any-size) POP POP ; 1
                    #u8(1 1) ADD
                    (&mstoreat offset len)
                    )

                [[Bytes31 . #u8(0 0 0 0  0 0 0 0  0 0 0 0  0 0 0 0
                                0 0 0 0  0 0 0 0  0 0 0 0  0 2 3)]
                 ]
                result-in-memory?: #t
                result-start: #x00)
      )

    (test-case "EVM-type &mload/ref/any-size, size = 32"
      (evm-test [] (&begin
                    (&push/any-size #u8(0 0 0 0  0 0 0 0  0 0 0 0  0 0 0 0
                                        0 0 0 0  0 0 0 0  0 0 0 0  0 0 1 2))
                    (&mstoreat 0 32)
                    32 0 (&mload/ref/any-size)
                    #u8(1 1) ADD
                    (&mstoreat 0 32)
                    )

                [[Bytes32 . #u8(0 0 0 0  0 0 0 0  0 0 0 0  0 0 0 0
                                0 0 0 0  0 0 0 0  0 0 0 0  0 0 2 3)]]
                result-in-memory?: #t
                result-start: #x00)
      )

    (test-case "EVM-type &mload-next-part/ref/any-size, size = 34"
      (evm-test []
        (&begin
          (&push/any-size #u8(0 0 0 0  0 0 0 0  0 0 0 0  0 0 0 0
                              0 0 0 0  0 0 0 0  0 0 0 0  0 0 1 2))
          (&mstoreat 0 32)
          32 #|current-part/ro|# 0 #|offset|# (&mload-next-part/ref/any-size) ; offset next-part/ro next-part
          (&mstoreat 0 32)  ; next-part/ro next-part
          (&mstoreat 32 32) ; next-part
          (&mstoreat 64 32) ; -
          )

          [[Bytes32 . #u8(0 0 0 0  0 0 0 0  0 0 0 0  0 0 0 0
                          0 0 0 0  0 0 0 0  0 0 0 0  0 0 0 0)]
           [Bytes32 . #u8(0 0 0 0  0 0 0 0  0 0 0 0  0 0 0 0
                          0 0 0 0  0 0 0 0  0 0 0 0  0 0 0 0)]
           [Bytes32 . #u8(0 0 0 0  0 0 0 0  0 0 0 0  0 0 0 0
                          0 0 0 0  0 0 0 0  0 0 0 0  0 0 1 2)]]
          result-in-memory?: #t
          result-start: #x00))

    (test-case "EVM-type &mload/ref/any-size, size = 34"
      (evm-test [] (&begin
                    ;; Store str34 in mem
                    (&push/any-size #u8(0 0 0 0  0 0 0 0  0 0 0 0  0 0 0 0
                                        0 0 0 0  0 0 0 0  0 0 0 0  0 0 0 0  1 2))
                    (&mstoreat 0 32)
                    (&mstoreat 32 2)
                    34 0 (&mload/ref/any-size) ; 1
                    SWAP1 #u8(1 1) ADD SWAP1
                    (&mstoreat 0 32)
                    (&mstoreat 32 2)
                    )

                [[Bytes32 . #u8(0 0 0 0  0 0 0 0  0 0 0 0  0 0 0 0
                                0 0 0 0  0 0 0 0  0 0 0 0  0 0 0 0)]
                 [Bytes2 . #u8(2 3)]
                 ]
                result-in-memory?: #t
                result-start: #x00)
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
