(export #t)

(import
  :std/srfi/1 :std/test  :std/format :std/sugar
  :clan/debug :clan/poo/poo :clan/number
  ../contract-runtime  ../assembly  ../json-rpc ../types
  
  ./signing-test ./30-transaction-integrationtest)

(def 90-contract-runtime-integrationtest
  (test-suite "integration test for ethereum/contract-runtime"
   
    (test-case "safe-sub when a operand equal b operand"
      (def contract-bytes
        (assemble/bytes
          (&begin
            8
            8
            &safe-sub
            (&mstoreat 0 1)
            1 0 RETURN
            [&jumpdest 'abort-contract-call] 0 DUP1 REVERT         
            )))
      (def result (evm-eval/offchain alice contract-bytes))
      (def unmarshaled-result (nat<-bytes result))
      (check-equal? (* 0 256) unmarshaled-result))

      (test-case "safe-sub when a operan less than b operand"
        (def contract-bytes
          (assemble/bytes
            (&begin
              42
              80
              &safe-sub
              (&mstoreat 0 1)
              2 0 RETURN
              [&jumpdest 'abort-contract-call] 0 DUP1 REVERT         
              )))
        (def result (evm-eval/offchain alice contract-bytes))
        (def unmarshaled-result (nat<-bytes result))
        (check-equal? (* 38 256) unmarshaled-result)) 
  
        (test-case "safe-sub when a operand greater than b operand"
          (def contract-bytes
            (assemble/bytes
              (&begin
                80
                42
                &safe-sub
                (&mstoreat 0 1)
                1 0 RETURN
                [&jumpdest 'abort-contract-call] 0 DUP1 REVERT         
                )))
          (def result 
            (let/cc return
              (try
                (return (evm-eval/offchain alice contract-bytes))
                (catch (e) (return -1)))))
          (check-equal? -1 result)) 



        (test-case "safe-add normal case"
          (def contract-bytes
            (assemble/bytes
              (&begin
                8
                8
                &safe-add
                (&mstoreat 0 1)
                2 0 RETURN
                [&jumpdest 'abort-contract-call] 0 DUP1 REVERT         
                )))
          (def result (evm-eval/offchain alice contract-bytes))
          (def unmarshaled-result (nat<-bytes result))
          (check-equal? (* 16 256) unmarshaled-result))


          (test-case "safe-add (unless (> 2**256 (+ x y)) (abort))"
            (def contract-bytes
              (assemble/bytes
                (&begin
                  (expt 2 255)
                  (expt 2 255)
                  &safe-add
                  (&mstoreat 0 32)
                  32 0 RETURN
                  [&jumpdest 'abort-contract-call] 0 DUP1 REVERT         
                  )))
                      (def result 
            (let/cc return
              (try
                (return (evm-eval/offchain alice contract-bytes))
                (catch (e) (return -1)))))
          (check-equal? -1 result)) 

          (test-case "safe-add (unless (>= (- 2**256 1) (+ x y)) (abort))"
            (def contract-bytes
              (assemble/bytes
                (&begin
                  (expt 2 255)
                  1
                  SUB
                  (expt 2 255)
                  &safe-add
                  (&mstoreat 0 32)
                  32 0 RETURN
                  [&jumpdest 'abort-contract-call] 0 DUP1 REVERT         
                  )))
            (def result 
              (let/cc return
                (try
                  (return (evm-eval/offchain alice contract-bytes))
                  (catch (e) (return -1)))))
            (check-equal? -1 result)) 


          (test-case "safe-add (unless (>= (- 2**256 1 x) y) (abort))"
            (def contract-bytes
              (assemble/bytes
                (&begin
                  (expt 2 250)
                  1
                  SUB
                  (expt 2 255)
                  &safe-add
                  (&mstoreat 0 32)
                  32 0 RETURN
                  [&jumpdest 'abort-contract-call] 0 DUP1 REVERT         
                  )))
            (def result 
              (let/cc return
                (try
                  (return (evm-eval/offchain alice contract-bytes))
                  (catch (e) (return -1)))))
            (check-equal? -1 result)) 


          (test-case "safe-add/n-bits n-bits equals 256"
            (def contract-bytes
              (assemble/bytes
                (&begin
                  (expt 2 25)
                  (expt 2 25)
                  (&safe-add/n-bits 256)
                  (&mstoreat 0 32)
                  32 0 RETURN
                  [&jumpdest 'abort-contract-call] 0 DUP1 REVERT         
                  )))
          (def result (evm-eval/offchain alice contract-bytes))
          (def unmarshaled-result (nat<-bytes result))
          (check-equal? (* 262144 256) unmarshaled-result))


          (test-case "safe-add/n-bits n-bits equals 0"
            (def contract-bytes
              (assemble/bytes
                (&begin
                  (expt 2 25)
                  (expt 2 25)
                  (&safe-add/n-bits 0)
                  (&mstoreat 0 32)
                  32 0 RETURN
                  [&jumpdest 'abort-contract-call] 0 DUP1 REVERT         
                  )))
          (def result (evm-eval/offchain alice contract-bytes))
          (def unmarshaled-result (nat<-bytes result))
          (check-equal? (* 131072 256) unmarshaled-result))


          (test-case "safe-add/n-bits n-bits under size"
            (def contract-bytes
              (assemble/bytes
                (&begin
                  (expt 2 25)
                  (expt 2 25)
                  (&safe-add/n-bits 10)
                  (&mstoreat 0 32)
                  32 0 RETURN
                  [&jumpdest 'abort-contract-call] 0 DUP1 REVERT         
                  )))
            (def result 
              (let/cc return
                (try
                  (return (evm-eval/offchain alice contract-bytes))
                  (catch (e) (return -1)))))
            (check-equal? -1 result)) 

          (test-case "safe-add/n-bits n-bits over size"
            (def contract-bytes
              (assemble/bytes
                (&begin
                  (expt 2 25)
                  (expt 2 25)
                  (&safe-add/n-bits 200)
                  (&mstoreat 0 32)
                  32 0 RETURN
                  [&jumpdest 'abort-contract-call] 0 DUP1 REVERT         
                  )))
            (def result (evm-eval/offchain alice contract-bytes))
            (def unmarshaled-result (nat<-bytes result))
            (check-equal? (* 262144 256) unmarshaled-result))


          (test-case "safe-mul normal case"
            (def contract-bytes
              (assemble/bytes
                (&begin
                  (expt 2 25)
                  (expt 2 25)
                  (&safe-mul)
                  (&mstoreat 0 32)
                  32 0 RETURN
                  [&jumpdest 'abort-contract-call] 0 DUP1 REVERT         
                  )))
            (def result (evm-eval/offchain alice contract-bytes))
            (def unmarshaled-result (nat<-bytes result))
            (check-equal? (* 4398046511104 256) unmarshaled-result))

          
          (test-case "safe-mul oveflow case"
            (def contract-bytes
              (assemble/bytes
                (&begin
                  (expt 2 250)
                  (expt 2 25)
                  (&safe-mul)
                  (&mstoreat 0 32)
                  32 0 RETURN
                  [&jumpdest 'abort-contract-call] 0 DUP1 REVERT         
                  )))
            (def result 
              (let/cc return
                (try
                  (return (evm-eval/offchain alice contract-bytes))
                  (catch (e) (return -1)))))
            (check-equal? -1 result)) 

            
        (test-case "validate-sig-data"
          (def contract-bytes
            (assemble/bytes
              (&begin
                #x69f945012f7ea7d3febf11eb1b78e1adc2d1c14c2cf48b25000938cc1860c83e
                #x9955af11969a2d2a7f860cb00e6a00cfa7c581f5df2dbe8ea16700b33f4b4b9b
                27
                &validate-sig-data
                (&mstoreat 0 2)
                2 0 RETURN
                [&jumpdest 'abort-contract-call] 0 DUP1 REVERT         
                )))
          (def result (evm-eval/offchain alice contract-bytes))
          (def unmarshaled-result (nat<-bytes result))
          (check-equal? 27 unmarshaled-result))


        (test-case "validate-sig-data with wrong v value"
          (def contract-bytes
            (assemble/bytes
              (&begin
                #x69f945012f7ea7d3febf11eb1b78e1adc2d1c14c2cf48b25000938cc1860c83e
                #x9955af11969a2d2a7f860cb00e6a00cfa7c581f5df2dbe8ea16700b33f4b4b9b
                30
                &validate-sig-data
                (&mstoreat 0 2)
                2 0 RETURN
                [&jumpdest 'abort-contract-call] 0 DUP1 REVERT         
                )))
          (def result 
            (let/cc return
              (try
                (return (evm-eval/offchain alice contract-bytes))
                (catch (e) (return -1)))))
          (check-equal? -1 result)) 

        (test-case "validate-sig-data with wrong s value"
          (def contract-bytes
            (assemble/bytes
              (&begin
                #x8Ff945012f7ea7d3febf11eb1b78e1adc2d1c14c2cf48b25000938cc1860c83e
                #x9955af11969a2d2a7f860cb00e6a00cfa7c581f5df2dbe8ea16700b33f4b4b9b
                27
                &validate-sig-data
                (&mstoreat 0 2)
                2 0 RETURN
                [&jumpdest 'abort-contract-call] 0 DUP1 REVERT         
                )))
          (def result 
            (let/cc return
              (try
                (return (evm-eval/offchain alice contract-bytes))
                (catch (e) (return -1)))))
          (check-equal? -1 result)) 


        (test-case "&unsafe-post-increment-at!"
          (def contract-bytes
            (assemble/bytes
              (&begin
                27
                0
                MSTORE
                (&unsafe-post-increment-at! 0 20)
                32 0 RETURN       
                )))
          (def result (evm-eval/offchain alice contract-bytes))
          (def unmarshaled-result (nat<-bytes result))
          (check-equal? 47 unmarshaled-result))

        (test-case "&unsafe-post-increment-at! spurious value"
          (def contract-bytes
            (assemble/bytes
              (&begin
                (expt 2 255)
                0
                MSTORE
                (&unsafe-post-increment-at! 0  (expt 2 255))
                32 0 RETURN       
                )))
          (def result 
          (let/cc return
            (try
              (return (evm-eval/offchain alice contract-bytes))
              (catch (e) (return -1)))))
        (check-equal? 0 (nat<-bytes result))) 
      
      
      (test-case "&memcpy/const-size destination second"
        (def contract-bytes
          (assemble/bytes
            (&begin
              85 
              0
              MSTORE
              95
              32
              MSTORE
              105
              64
              MSTORE
              96
              0
              (&memcpy/const-size 96)
              32 160 RETURN     
              )))
        (def result (evm-eval/offchain alice contract-bytes))
        (def unmarshaled-result (nat<-bytes result))
        (check-equal? 105 unmarshaled-result))

          (test-case "&memcpy/const-size destination first"
            (def contract-bytes
              (assemble/bytes
                (&begin
                  85 
                  0
                  MSTORE
                  95
                  32
                  MSTORE
                  105
                  64
                  MSTORE
                  0
                  96
                 (&memcpy/const-size 96 dst-first?: #t)
                  32 160 RETURN     
                  )))
            (def result (evm-eval/offchain alice contract-bytes))
            (def unmarshaled-result (nat<-bytes result))
            (check-equal? 105 unmarshaled-result))

          (test-case "&memcpy/const-size when n is not divisible by 32"
            (def contract-bytes
              (assemble/bytes
                (&begin
                  85 
                  0
                  MSTORE
                  95
                  32
                  MSTORE
                  105
                  64
                  MSTORE
                  5
                  96
                  (&mstore 2)
                  0
                  128
                 (&memcpy/const-size 98 dst-first?: #t)
                  2 224 RETURN     
                  )))
            (def result (evm-eval/offchain alice contract-bytes))
            (def unmarshaled-result (nat<-bytes result))
            (check-equal? 5 unmarshaled-result))

          (test-case "&memcpy/const-size when n is not divisible by 32 and with overwrite-after? true"
            (def contract-bytes
              (assemble/bytes
                (&begin
                  85 
                  0
                  MSTORE
                  95
                  32
                  MSTORE
                  105
                  64
                  MSTORE
                  5
                  96
                  (&mstore 2)
                  0
                  128
                 (&memcpy/const-size 98 overwrite-after?: #t dst-first?: #t)
                  2 224 RETURN     
                  )))
            (def result (evm-eval/offchain alice contract-bytes))
            (def unmarshaled-result (nat<-bytes result))
            (check-equal? 5 unmarshaled-result))

          (test-case "&memcpy/const-size/const-src when n is not divisible by 32 and with overwrite-after? true"
            (def contract-bytes
              (assemble/bytes
                (&begin
                  85 
                  0
                  MSTORE
                  95
                  32
                  MSTORE
                  105
                  64
                  MSTORE
                  5
                  96
                  (&mstore 2)
                  128
                 (&memcpy/const-size/const-src 0 98 overwrite-after?: #t)
                  2 224 RETURN     
                  )))
            (def result (evm-eval/offchain alice contract-bytes))
            (def unmarshaled-result (nat<-bytes result))
            (check-equal? 5 unmarshaled-result))


          (test-case "&memcpy/const-size/expr-src when &addr is nat"
            (def contract-bytes
              (assemble/bytes
                (&begin
                  85 
                  0
                  MSTORE
                  95
                  32
                  MSTORE
                  105
                  64
                  MSTORE
                  5
                  96
                  (&mstore 2)
                  128
                 (&memcpy/const-size/expr-src 0 98 overwrite-after?: #t)
                  2 224 RETURN     
                  )))
            (def result (evm-eval/offchain alice contract-bytes))
            (def unmarshaled-result (nat<-bytes result))
            (check-equal? 5 unmarshaled-result))

          ;; Can memory address be non nat? If not then the if expression is dormant Ask Fare later
         ;; (test-case "&memcpy/const-size/expr-src when &addr is not nat"
         ;;   (def contract-bytes
         ;;     (assemble/bytes
         ;;       (&begin
         ;;         85 
         ;;         0
         ;;         MSTORE
         ;;         95
         ;;         32
         ;;         MSTORE
         ;;         105
         ;;         64
         ;;         MSTORE
         ;;         5
         ;;         96
         ;;         (&mstore 2)
         ;;         128
         ;;        (&memcpy/const-size/expr-src -1 98 overwrite-after?: #t)
         ;;         2 224 RETURN     
         ;;         )))
         ;;   (def result (evm-eval/offchain alice contract-bytes))
         ;;   (def unmarshaled-result (nat<-bytes result))
         ;;   (check-equal? 5 unmarshaled-result))

      ;; Failing with exception ... invalid jump destination
      ;; There is an inherent error I can not fathom. 
     ;; (test-case "&define-unsafe-memcopy"
     ;;   (def contract-bytes
     ;;     (assemble/bytes
     ;;       (&begin
     ;;         85 
      ;;        0
     ;;         MSTORE
     ;;         95
     ;;         32
     ;;         MSTORE
     ;;         105
     ;;         64
     ;;         MSTORE
     ;;         5
     ;;         96
     ;;         MSTORE
     ;;         0
     ;;         0
      ;;        128
     ;;         4
     ;;         &define-unsafe-memcopy
      ;;        32 0 RETURN
     ;;         [&jumpdest 'abort-contract-call] 0 DUP1 REVERT      
     ;;         )))
     ;;   (def result (evm-eval/offchain alice contract-bytes))
     ;;   (def unmarshaled-result (nat<-bytes result))
     ;;   (check-equal? 5 unmarshaled-result))


      (test-case "&check-sufficient-deposit  When deposit is GT"
        (def contract-bytes
          (assemble/bytes
            (&begin
              &check-sufficient-deposit   
              [&jumpdest 'abort-contract-call] 0 DUP1 REVERT 
              )))
        (def result 
          (let/cc return
            (try
              (return (evm-eval/offchain alice contract-bytes))
              (catch (e) (return -1)))))
        (check-equal? -1 result)) 

        ;; Pending when CALLVALUE is greater

      (test-case "&check-sufficient-deposit  When deposit is LT"
        (def contract-bytes
          (assemble/bytes
            (&begin
              &check-sufficient-deposit   
              [&jumpdest 'abort-contract-call] 0 DUP1 REVERT 
              )))
        (def result 
          (let/cc return
            (try
              (return (evm-eval/offchain alice contract-bytes value: 2340000))
              (catch (e) (return -1)))))
        (check-equal? -1 result)) 


      ;; Underflow error message
     ;; (test-case "&check-participant!"
     ;;   (def contract-bytes
     ;;     (assemble/bytes
     ;;       (&begin
      ;;        CALLER
      ;;        &check-participant!
      ;;        (&mstoreat 0 2)
      ;;         2 0 RETURN     
       ;;       [&jumpdest 'abort-contract-call] 0 DUP1 REVERT
       ;;       )))
       ;; (def result (evm-eval/offchain alice contract-bytes))
      ;;  (def unmarshaled-result (nat<-bytes result))
      ;;  (check-equal? 5 unmarshaled-result))

        (def &deposit!
  ;; Scheme pseudocode: (lambda (amount) (increment! deposit amount))
  ;; TODO: can we statically prove it's always within range and make the &safe-add an ADD ???
  (&begin deposit &safe-add deposit-set!)) 

      (test-case "&deposit!"
        (def contract-bytes
          (assemble/bytes
            (&begin
              300
              &deposit!
              deposit
              &deposit!
              deposit
              (&mstoreat 0 32)
               32 0 RETURN  
               [&jumpdest 'abort-contract-call] 0 DUP1 REVERT    
              )))
        (def result (evm-eval/offchain alice contract-bytes))
        (def unmarshaled-result (nat<-bytes result))
        (check-equal? 600 unmarshaled-result))


;; TESTING STATUS: Wholly untested.

  ;(def prefunded-addresses [bob])
   ;;   (test-case "&send-ethers!"
   ;;     (def balances-before (map (cut eth_getBalance <> 'latest) prefunded-addresses))
   ;;     (def contract-bytes
    ;;      (assemble/bytes
    ;;        (&begin
    ;;          SELFBALANCE
    ;;          bob
    ;;          &send-ethers!
     ;;         [&jumpdest 'abort-contract-call] 0 DUP1 REVERT    
     ;;         )))
    ;;    (def result (evm-eval/offchain alice contract-bytes))
     ;;    (def balances-after (map (cut eth_getBalance <> 'latest) prefunded-addresses))
     ;;   (def unmarshaled-result (nat<-bytes result))
    ;;    (check-equal? (+ (car balances-before) 300) (car balances-after)))


    ;;  (test-case "&withdraw!"
      ;;  (def balances-before (map (cut eth_getBalance <> 'latest) prefunded-addresses))
      ;; (def contract-bytes
      ;;  (assemble/bytes
      ;;     (&begin
      ;;        300
      ;;        bob
      ;;        &send-ethers!
      ;;        [&jumpdest 'abort-contract-call] 0 DUP1 REVERT    
      ;;       )))
      ;;  (def result (evm-eval/offchain alice contract-bytes))
      ;;   (def balances-after (map (cut eth_getBalance <> 'latest) prefunded-addresses))
      ;;  (def unmarshaled-result (nat<-bytes result))
      ;;  (check-equal? (+ (car balances-before) 300) (car balances-after)))

      
      (test-case "&brk-cons when n-bytes is 32"
        (def contract-bytes
          (assemble/bytes
            (&begin
              500
              (&brk-cons 32)
              32 brk@ RETURN
              )))
        (def result (evm-eval/offchain alice contract-bytes))
        (def unmarshaled-result (nat<-bytes result))
        (check-equal? 500 unmarshaled-result))

      (test-case "&brk-cons when n-bytes is 1"
        (def contract-bytes
          (assemble/bytes
            (&begin
              5
              (&brk-cons 1)
              1 brk@ RETURN
              )))
        (def result (evm-eval/offchain alice contract-bytes))
        (def unmarshaled-result (nat<-bytes result))
        (check-equal? 5 unmarshaled-result))

      (test-case "&brk-cons when n-bytes is 0"
        (def contract-bytes
          (assemble/bytes
            (&begin
              100
              5
              (&brk-cons 0)
              (&mstoreat 0 32)
              32 0 RETURN
              )))
        (def result (evm-eval/offchain alice contract-bytes))
        (def unmarshaled-result (nat<-bytes result))
        (check-equal? 100 unmarshaled-result))

      (test-case "&brk-cons when n-bytes is  not 0 , 1 or 30"
        (def contract-bytes
          (assemble/bytes
            (&begin
              5000
              (&brk-cons 8)
              8 brk@ RETURN
              )))
        (def result (evm-eval/offchain alice contract-bytes))
        (def unmarshaled-result (nat<-bytes result))
        (check-equal? 5000 unmarshaled-result))


      (test-case "&ecrecover0"
        (def contract-bytes
         (assemble/bytes
            (&begin
              #x4f8ae3bd7535248d0bd448298cc2e2071e56992d0774dc340c368ae950852ada
              #x9242685bf161793cc25603c231bc2f568eb630ea16aa137d2664ac8038825608
              28
              #x1c8aff950685c2ed4bc3174f3472287b56d9517b9c948127319a09a7a36deac8
              &ecrecover0
              (&mstoreat 0 32)
              32 0 RETURN

              )))
        (def result (evm-eval/offchain alice contract-bytes))
        (def unmarshaled-result (nat<-bytes result))
        (check-equal? ( > unmarshaled-result 0) #t))

       ;; TESTING STATUS: Wholly untested.

      ;; Failed to run
      ;;(test-case "&read-published-datum"
     ;;   (def contract-bytes
      ;;    (assemble/bytes
      ;;      (&begin
      ;;        800
      ;;        NUMBER
      ;;        NUMBER
      ;;        (&read-published-datum 32)
      ;;        32 calldatapointer@ RETURN
      ;;        )))
      ;;  (def result (evm-eval/offchain alice contract-bytes))
      ;;  (def unmarshaled-result (nat<-bytes result))
      ;;  (check-equal? 800 unmarshaled-result))

      ;; Could not figure out what calldatapointer@ is
      (test-case "&read-published-data-to-mem"
        (def contract-bytes
          (assemble/bytes
            (&begin
              900
              NUMBER
              &read-published-data-to-mem
              32 calldatapointer@ RETURN
              )))
        (def result (evm-eval/offchain alice contract-bytes))
        (def unmarshaled-result (nat<-bytes result))
        (check-equal? 900 unmarshaled-result))

      (test-case "&SELFDESTRUCT"
        (def contract-bytes
          (assemble/bytes
            (&begin
              bob
              (&SELFDESTRUCT)
              )))
        (def result (evm-eval/offchain alice contract-bytes))
        (def unmarshaled-result (nat<-bytes result))
        (check-equal? 0 unmarshaled-result))

      (test-case "&start-timer!"
        (def contract-bytes
          (assemble/bytes
            (&begin
              &start-timer!
              timer-start
              (&mstoreat 0 32)
               32 0 RETURN  
               [&jumpdest 'abort-contract-call] 0 DUP1 REVERT    
              )))
        (def result (evm-eval/offchain alice contract-bytes))
        (def unmarshaled-result (nat<-bytes result))
        (check-equal? (> unmarshaled-result 0) #t))

      (test-case "&stop-timer!"
        (def contract-bytes
          (assemble/bytes
            (&begin
              &stop-timer!
              timer-start
              (&mstoreat 0 32)
               32 0 RETURN  
               [&jumpdest 'abort-contract-call] 0 DUP1 REVERT    
              )))
        (def result (evm-eval/offchain alice contract-bytes))
        (def unmarshaled-result (nat<-bytes result))
        (check-equal? (> unmarshaled-result 0) #t))


      
      (test-case "&marshal UInt256"
        (def contract-bytes
          (assemble/bytes
            (&begin
              brk 
              DUP1 
              DUP1
              (&marshal UInt256  7 ) 
              (&mstoreat 0 32)
               32 0 RETURN  
              )))
        (def result (evm-eval/offchain alice contract-bytes))
        (def unmarshaled-result (nat<-bytes result))
        (check-equal? (= unmarshaled-result (param-length UInt256)) #t))

      (test-case "&marshal UInt8"
        (def contract-bytes
          (assemble/bytes
            (&begin
              brk 
              DUP1 
              DUP1
              (&marshal UInt8  7) 
              (&mstoreat 0 32)
               32 0 RETURN  
              )))
        (def result (evm-eval/offchain alice contract-bytes))
        (def unmarshaled-result (nat<-bytes result))
        (check-equal? (= unmarshaled-result (param-length UInt8)) #t))


      ))
