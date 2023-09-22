(export #t)

(import
  :gerbil/gambit
  :std/format :std/iter :std/pregexp :std/misc/bytes :std/misc/number
  :std/srfi/1 :std/sugar :std/test
  :clan/debug
  :clan/crypto/keccak
  :clan/poo/object :clan/poo/io :clan/poo/debug :clan/poo/brace
  :clan/crypto/secp256k1
  :clan/persist/content-addressing
  ../types ../ethereum ../known-addresses ../assembly ../evm-runtime
  ../network-config ../json-rpc ../testing
  ./30-transaction-integrationtest)

(def (digest<-tvps alst)
  (def out (open-output-u8vector))
  (for ((p alst))
    (with (([t . v] p)) (marshal t v out)))
  (digest<-bytes (get-output-u8vector out)))

(.def (Bytes128 @ BytesN.) n: 128 sexp: 'Bytes128)
(define-type String128
  {(:: @ String)
   .length-in-bytes: 128
   .element?: (lambda (s) (and (string? s) (= (bytes-length (string->bytes s)) 128)))
   .Bytes: Bytes128})

(def 80-evm-eval-integrationtest
  (test-suite "unit tests for evm functions"
    (test-case "return"
      (evm-test []
                (&begin 42 (&mstoreat 0 1))
                [[UInt16 (* 42 256)...]]
                result-in-memory?: #t))

    (test-case "if"
      (evm-test [] (&if (&begin 1 2 GT) (&begin 0) (&begin 1)) [[Bool . #f]]))

    (test-case "switch"
      (evm-test [] (&switch 1 [[0 [5]] [1 [23]] [2 [42]]]) [[UInt8 . 23]]))

    (test-case "digest with single value"
      (def digest-value
        [[UInt256 . 7]])
      (evm-test [] (&digest<-tvps digest-value) [[Digest (digest<-tvps digest-value)...]]))

    (test-case "digest with multiple values"
      (def digest-value
        [[UInt256 . 7]
         [UInt256 . 21]])
      (evm-test [] (&digest<-tvps digest-value) [[Digest (digest<-tvps digest-value)...]]))

    (test-case "digest with realistic frame state"
      (def digest-value
        [[UInt16 . 1014]
         [Block . 2185]
         [Address . alice]
         [Address . bob]
         [Digest . #u8(99 29 22 99 149 169 218 44 150 207 223 114 84 92 253 163 49 233 56 120 20 117 144 222 173 81 88 153 240 137 9 49)]
         [UInt256 . 1000000000]
         [UInt256 . 26494137127516106733148689316263385574755820242808037201403224934424794788569]
         [UInt256 . 1000000000]])
      (evm-test [] (&digest<-tvps digest-value) [[Digest (digest<-tvps digest-value)...]]))

    (test-case "simple small function"
      (evm-test []
                (&begin
                 'start JUMP
                 (&define-small-function 'sub SUB)
                 [&jumpdest 'start]
                 (&call 'sub 9 6))
                [[UInt8 . 3]]))

    (test-case "small function calling small function"
      (evm-test []
                (&begin
                 'start JUMP
                 (&define-small-function 'add-then-double
                                         (&begin
                                          ADD
                                          (&mstoreat 0 1)
                                          (&call 'multiply 2 (&mloadat 0 1))))
                 (&define-small-function 'multiply MUL)
                 [&jumpdest 'start]
                 (&call 'add-then-double 3 6))
                [[UInt8 . 18]]))

    (test-case "safe-sub when a operand equal b operand"
      (evm-test [[UInt8 . 8] [UInt8 . 8]] &safe-sub [[UInt8 . 0]]))
    (test-case "safe-sub when b operand less than a operand"
      (evm-test-failure [[UInt16 . 42] [UInt16 . 80]] &safe-sub))
    (test-case "safe-sub when b operand greater than a operand"
      (evm-test [[UInt16 . 80] [UInt16 . 42]] &safe-sub [[UInt16 . 38]]))

    (test-case "safe-add normal case"
      (evm-test [[UInt8 . 8] [UInt8 . 8]] &safe-add [[UInt8 . 16]])
      (check-equal? (safe-add 8 8) 16))
    (test-case "safe-add overflow"
      (def 2^255 (expt 2 255))
      (evm-test-failure [[UInt256 . 2^255] [UInt256 . 2^255]] &safe-add)
      (check-exception (safe-add 2^255 2^255)
                       (lambda (e)
                         (pregexp-match "safe-add: overflow from adding" (error-message e)))))
    (test-case "safe-add/n-bits n-bits equals 256"
      (evm-test [[UInt256 . 4000] [UInt256 . 6000]] (&safe-add/n-bits 200) [[UInt256 . 10000]]))
    (test-case "safe-add/n-bits n-bits equals 0"
      (evm-test [[UInt256 . 0] [UInt256 . 0]] (&safe-add/n-bits 0) [[UInt256 . 0]]))
    (test-case "safe-add/n-bits n-bits under size"
      (evm-test-failure [[UInt256 . 1000] [UInt256 . 1000]] (&safe-add/n-bits 10)))

    (test-case "safe-mul normal case"
      (evm-test [[UInt256 (expt 2 25)...] [UInt256 (expt 2 25)...]]
                (&safe-mul) [[UInt256 (expt 2 50)...]]))
    (test-case "safe-mul overflow case"
      (evm-test-failure [[UInt256 (- (expt 2 250) 1)...] [UInt256 (expt 2 25)...]] (&safe-mul)))

    (test-case "validate-sig-data"
      (evm-test [[UInt256 . 27]
                 [UInt256 . #x9955af11969a2d2a7f860cb00e6a00cfa7c581f5df2dbe8ea16700b33f4b4b9b]
                 [UInt256 . #x69f945012f7ea7d3febf11eb1b78e1adc2d1c14c2cf48b25000938cc1860c83e]]
                &validate-sig-data
                [[UInt16 . 27]
                 [UInt256 . #x9955af11969a2d2a7f860cb00e6a00cfa7c581f5df2dbe8ea16700b33f4b4b9b]
                 [UInt256 . #x69f945012f7ea7d3febf11eb1b78e1adc2d1c14c2cf48b25000938cc1860c83e]]))
    (test-case "validate-sig-data with wrong v value"
      (evm-test-failure
       [[UInt256 . 29]
        [UInt256 . #x9955af11969a2d2a7f860cb00e6a00cfa7c581f5df2dbe8ea16700b33f4b4b9b]
        [UInt256 . #x69f945012f7ea7d3febf11eb1b78e1adc2d1c14c2cf48b25000938cc1860c83e]]
       &validate-sig-data))
    (test-case "validate-sig-data with wrong s value"
      (evm-test-failure
       [[UInt256 . 27]
        [UInt256 . #x9955af11969a2d2a7f860cb00e6a00cfa7c581f5df2dbe8ea16700b33f4b4b9b]
        [UInt256 . -1]]
       &validate-sig-data))

    (test-case "&unsafe-post-increment-at!"
      (evm-test [[UInt256 . 128] [UInt256 . 27]]
                (&begin MSTORE (&unsafe-post-increment-at! 128 20))
                [[UInt256 . 47]]
                result-in-memory?: #t
                result-start: 128))

    (def pony-stanza
      (string-append "You want the end of all wars? "
                     "I want the end of all wars, "
                     "And then a trip to the stars; "
                     "And a pony.\n"))

    (test-case "&memcpy/const-size destination second"
      (evm-test []
                (&begin
                 (string->bytes "en a trip to the stars; And a po") 64 MSTORE
                 (string->bytes "You want the end of all wars? I ") 0 MSTORE
                 (string->bytes "ny.\nTHIS IS BOGUS, NOT INCLUDED.") 96 MSTORE
                 (string->bytes "want the end of all wars, And th") 32 MSTORE
                 200 0 (&memcpy/const-size 100))
                [[String128 (string-append pony-stanza (make-string 28 (integer->char 0)))...]]
                result-in-memory?: #t
                result-start: 200))

    (test-case "&memcpy/const-size destination first"
      (evm-test []
                (&begin
                 (string->bytes "en a trip to the stars; And a po") 64 MSTORE
                 (string->bytes "You want the end of all wars? I ") 0 MSTORE
                 (string->bytes "ny.\nTHIS IS BOGUS, NOT INCLUDED.") 96 MSTORE
                 (string->bytes "want the end of all wars, And th") 32 MSTORE
                 0 200 (&memcpy/const-size 100 dst-first?: #t))
                [[String128 (string-append pony-stanza (make-string 28 (integer->char 0)))...]]
                result-in-memory?: #t
                result-start: 200))

    (test-case "&memcpy/const-size when n is not divisible 32"
      (evm-test []
                (&begin 85 0 MSTORE 95 32 MSTORE 105 64 MSTORE 5 96 (&mstore 2)
                        0 128 (&memcpy/const-size 98 dst-first?: #t))
                [[UInt16 . 5]] result-in-memory?: #t result-start: 224))

    (test-case "&memcpy/const-size when n is not divisible by 32 and with overwrite-after? true"
      (evm-test []
                (&begin 85 0 MSTORE 95 32 MSTORE 105 64 MSTORE 5 96 (&mstore 2)
                        0 128 (&memcpy/const-size 98 overwrite-after?: #t dst-first?: #t))
                [[UInt16 . 5]] result-in-memory?: #t result-start: 224))

    (test-case "&memcpy/const-size/const-src when n is not divisible by 32 and with overwrite-after? true"
      (evm-test []
                (&begin 85 0 MSTORE 95 32 MSTORE 105 64 MSTORE 5 96 (&mstore 2)
                        128 (&memcpy/const-size/const-src 0 98 overwrite-after?: #t))
                [[UInt16 . 5]] result-in-memory?: #t result-start: 96))

    (test-case "&memcpy/const-size/expr-src when &addr is nat"
      (evm-test []
                (&begin 85 0 MSTORE 95 32 MSTORE 105 64 MSTORE 5 96 (&mstore 2)
                        128 (&memcpy/const-size/expr-src 0 98 overwrite-after?: #t))
                [[UInt16 . 5]] result-in-memory?: #t result-start: 224))

    ;; TODO
    ;; (test-case "&check-sufficient-deposit0 when deposit0 is GT"
    ;; (evm-test-failure [] (&begin &check-sufficient-deposit0)))

    (test-case "&add-var!/deposit0"
      (evm-test [[UInt256 . 300]]
                (&begin (&add-var! deposit0-var) deposit0 (&add-var! deposit0-var) deposit0)
                [[UInt256 . 600]]))

    (test-case "&add-var!/withdraw0"
      (evm-test [[UInt256 . 300]]
                (&begin (&add-var! withdraw0-var) withdraw0 (&add-var! withdraw0-var) withdraw0)
                [[UInt256 . 600]]))

    (test-case "&brk-cons when n-bytes is 32"
      (evm-test [[UInt256 . 500]]
                (&brk-cons 32)
                [[UInt256 . 500]] result-in-memory?: #t result-start: brk@))

    (test-case "&brk-cons when n-bytes is 1"
      (evm-test [[UInt8 . 50]]
                (&brk-cons 1)
                [[UInt8 . 50]]
                result-in-memory?: #t result-start: brk@))

    (test-case "&brk-cons when n-bytes is 0"
      (evm-test [[UInt256 . 100] [UInt256 . 5]]
                (&brk-cons 0)
                [[UInt256 . 5]]))

    (test-case "&brk-cons when n-bytes is  not 0 , 1 or 30"
      (evm-test [[UInt16 . 5000]]
                (&brk-cons 2)
                [[UInt16 . 5000]] result-in-memory?: #t result-start: brk@))

    ;; Not sure is this is not hacky and correct
    (test-case "&ecrecover0"
      (def message "hello, world")
      (defvalues (v r s)
        (vrs<-signature (make-signature String (secret-key<-address alice) message)))
      (evm-test [[Digest (keccak256<-bytes (string->bytes message))...]
                 [UInt256 . v]
                 [UInt256 . r]
                 [UInt256 . s]]
                &ecrecover0
                [[Address . alice]
                 [Bool . #t]]))

    ;; TODO: create an on-chain contract that uses &read-published-data-to-mem,
    ;; then send it data in a transaction, and see how it processes it.
    ;;;;(test-case "&read-published-data-to-mem"
    ;;;;  (void))

    ;; TODO: for &SELFDESTRUCT, create a contract first, put money in it,
    ;; and check that the money has been transfered to Bob in the end?
    ;;;;(test-case "&SELFDESTRUCT"
    ;;;;  (evm-test [] (&begin bob (&SELFDESTRUCT)) [[UInt256 . 0]]))

    ;; TODO: for start-timer, etc., do some comparison between timer-start and NUMBER before and after?
    (test-case "&start-timer!"
      (evm-test [] (&begin &start-timer!
                           timer-start NUMBER EQ)
                [[Bool . #t]]))

    (test-case "&stop-timer!"
      (evm-test [] (&begin &stop-timer!
                           timer-start max-block EQ)
                [[Bool . #t]]))

    (test-case "&marshal UInt256"
      (evm-test [] (&begin brk DUP1 DUP1 (&marshal UInt256 7))
                [[UInt256 . 32]]))

    (test-case "&marshal UInt8"
      (evm-test [] (&begin brk DUP1 DUP1 (&marshal UInt8 7))
                [[UInt8 . 1]]))

    (test-case "&marshal UInt8"
      (evm-test [] (&begin brk DUP1 DUP1 (&marshal UInt16 7))
                [[UInt16 . 2]]))

    (test-case "&mstore 1 byte"
      (evm-test [] (&begin 42 0 (&mstore 1))
                [[UInt8 . 42]]
                result-in-memory?: #t))

    (test-case "&mstore 32 bytes"
      (def maxUInt256 (- (expt 2 256) 1))
      (evm-test [] (&begin maxUInt256 0 (&mstore 32))
                [[UInt256 . maxUInt256]]
                result-in-memory?: #t))))
