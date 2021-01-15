(export #t)

(import
  :std/srfi/1 :std/test  :std/format :std/sugar
  :clan/debug :clan/number
  :clan/poo/poo :clan/poo/io :clan/poo/debug
  ../contract-runtime ../assembly ../json-rpc ../types ../testing
  ./30-transaction-integrationtest)

(def 90-contract-runtime-integrationtest
  (test-suite "integration test for ethereum/contract-runtime"
    (test-case "safe-sub when a operand equal b operand"
      (evm-test [[UInt8 . 8] [UInt8 . 8]] &safe-sub [[UInt8 . 0]]))

    (test-case "safe-sub when b operand less than a operand"
      (evm-test-failure [[UInt16 . 80] [UInt16 . 42]] &safe-sub))

    (test-case "safe-sub when b operand greater than a operand"
      (evm-test [[UInt16 . 42] [UInt16 . 80]] &safe-sub [[UInt16 . 38]]))

    (test-case "safe-add normal case"
      (evm-test [[UInt8 . 8] [UInt8 . 8]] &safe-add [[UInt8 . 16]]))

    (test-case "safe-add/n-bits n-bits equals 256"
      (evm-test [[UInt256 . 4000] [UInt256 . 6000]] (&safe-add/n-bits 200) [[UInt256 . 10000]]))

    (test-case "safe-add/n-bits n-bits equals 0"
      (evm-test [[UInt256 . 0] [UInt256 . 0]] (&safe-add/n-bits 0) [[UInt256 . 0]]))

    (test-case "safe-add/n-bits n-bits under size"
      (evm-test-failure [[UInt256 . 1000] [UInt256 . 1000]] (&safe-add/n-bits 10)))

    (test-case "safe-mul normal case"
      (evm-test [[UInt256 (expt 2 25)...] [UInt256 (expt 2 25)...]] (&safe-mul) [[UInt256 . 1125899906842624]]))

    (test-case "safe-mul oveflow case"
      (evm-test-failure [[UInt256 (- (expt 2 250) 1)...] [UInt256 (expt 2 25)...]] (&safe-mul)))

    (test-case "validate-sig-data"
      (evm-test [[UInt256 . #x69f945012f7ea7d3febf11eb1b78e1adc2d1c14c2cf48b25000938cc1860c83e] 
                 [UInt256 . #x9955af11969a2d2a7f860cb00e6a00cfa7c581f5df2dbe8ea16700b33f4b4b9b]
                 [UInt256 . 27]] 
                 &validate-sig-data
                 [[UInt16 . 27]]))

    (test-case "validate-sig-data with wrong v value"
      (evm-test-failure [[UInt256 . #x69f945012f7ea7d3febf11eb1b78e1adc2d1c14c2cf48b25000938cc1860c83e] 
                 [UInt256 . #x9955af11969a2d2a7f860cb00e6a00cfa7c581f5df2dbe8ea16700b33f4b4b9b]
                 [UInt256 . 30]] 
                 &validate-sig-data
                 ))

    (test-case "validate-sig-data with wrong s value"
      (evm-test-failure [[UInt256 . #x8Ff945012f7ea7d3febf11eb1b78e1adc2d1c14c2cf48b25000938cc1860c83e] 
                 [UInt256 . #x9955af11969a2d2a7f860cb00e6a00cfa7c581f5df2dbe8ea16700b33f4b4b9b]
                 [UInt256 . 27]] 
                 &validate-sig-data
                 ))

   (test-case "&unsafe-post-increment-at!"
     (evm-test [[UInt256 . 27] [UInt256 . 0] ] (&begin MSTORE  (&unsafe-post-increment-at! 0 20)) [[UInt256 . 47]] copied: #t))

  (test-case "&unsafe-post-increment-at! spurious value"
     (evm-test-failure [[UInt256  (expt 2 255)...] [UInt256 . 0] ] (&begin MSTORE  (&unsafe-post-increment-at! 0  (expt 2 255))) ))

  (test-case "&memcpy/const-size destination second"
      (evm-test [] 
        (&begin 85 0 MSTORE 95 32 MSTORE 105 64 MSTORE 96 0 (&memcpy/const-size 96)) [[UInt256 . 105]] start: 160))

  
  (test-case "&memcpy/const-size destination first"
      (evm-test [] 
        (&begin 85 0 MSTORE 95 32 MSTORE 105 64 MSTORE 0 96 (&memcpy/const-size 96 dst-first?: #t)) [[UInt256 . 105]] start: 160))

  (test-case "&memcpy/const-size when n is not divisible by 32"
    (evm-test [] 
      (&begin 85 0 MSTORE 95 32 MSTORE 105 64 MSTORE 5 96  (&mstore 2) 0 128 (&memcpy/const-size 98 dst-first?: #t)) [[UInt16 . 5]] copied: #t start: 224))

  (test-case "&memcpy/const-size when n is not divisible by 32 and with overwrite-after? true"
    (evm-test [] 
      (&begin 85 0 MSTORE 95 32 MSTORE 105 64 MSTORE 5 96  (&mstore 2) 0 128 (&memcpy/const-size 98 overwrite-after?: #t dst-first?: #t)) [[UInt16 . 5]] copied: #t  start: 224))

  (test-case "&memcpy/const-size/const-src when n is not divisible by 32 and with overwrite-after? true"
    (evm-test [] 
      (&begin 85 0 MSTORE 95 32 MSTORE 105 64 MSTORE 5 96  (&mstore 2) 128 (&memcpy/const-size/const-src 0 98 overwrite-after?: #t)) [[UInt16 . 5]] copied: #t  start: 96))

  (test-case "&memcpy/const-size/expr-src when &addr is nat"
    (evm-test [] 
      (&begin 85 0 MSTORE 95 32 MSTORE 105 64 MSTORE 5 96  (&mstore 2) 128 (&memcpy/const-size/expr-src 0 98 overwrite-after?: #t)) [[UInt16 . 5]] copied: #t start: 224))

  (test-case "&check-sufficient-deposit  When deposit is GT"
    (evm-test-failure [] (&begin &check-sufficient-deposit)))


  (test-case "&deposit!"
    (evm-test [[UInt256 . 300]] (&begin &deposit! deposit &deposit! deposit) [[UInt256 . 600]]))

  (test-case "&brk-cons when n-bytes is 32"
    (evm-test [[UInt256 . 500]] (&brk-cons 32) [[UInt256 . 500]] copied: #t start: brk@))

  (test-case "&brk-cons when n-bytes is 1"
    (evm-test [[UInt8 . 50]] (&brk-cons 1) [[UInt8 . 50]] copied: #t start: brk@))


  (test-case "&brk-cons when n-bytes is 0"
    (evm-test [[UInt256 . 100] [UInt256 . 5]] (&brk-cons 0) [[UInt256 . 100]]))


  (test-case "&brk-cons when n-bytes is  not 0 , 1 or 30"
    (evm-test [[UInt16 . 5000]] (&brk-cons 2) [[UInt16 . 5000]] copied: #t start: brk@))

  ;; Not sure is this is not hacky and correct
  (test-case "&ecrecover0"
    (evm-test [[UInt256 . #x4f8ae3bd7535248d0bd448298cc2e2071e56992d0774dc340c368ae950852ada] 
                [UInt256 . #x9242685bf161793cc25603c231bc2f568eb630ea16aa137d2664ac8038825608]
                [UInt256 . 28]
                [UInt256 . #x1c8aff950685c2ed4bc3174f3472287b56d9517b9c948127319a09a7a36deac8]] 
                &ecrecover0          
                [[UInt256 . 1]] boolCheck: #t))


(test-case "&read-published-data-to-mem"
    (evm-test [[UInt256 . 5000]] (&begin NUMBER &read-published-data-to-mem) [[UInt256 . 5000]] copied: #t start: calldatapointer@))

;;(test-case "&SELFDESTRUCT"
  ;;  (evm-test [] (&begin bob (&SELFDESTRUCT)) [[UInt256 . 0]]))

(test-case "&start-timer!"
    (evm-test [] (&begin &start-timer! timer-start) [[UInt256 . 1]] boolCheck: #t))

(test-case "&stop-timer!"
    (evm-test [] (&begin &stop-timer! timer-start) [[UInt256 . 1]] boolCheck: #t))

(test-case "&marshal UInt256"
    (evm-test [] (&begin brk DUP1 DUP1  (&marshal UInt256  7)) [[UInt256 . 32]]))

(test-case "&marshal UInt8"
    (evm-test [] (&begin brk DUP1 DUP1  (&marshal UInt256  7)) [[UInt8 . 32]]))

      ))
