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



      
      
      
      
      
      
      
      
      
      
      
      ))
