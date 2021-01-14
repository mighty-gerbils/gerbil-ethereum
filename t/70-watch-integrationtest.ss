(export #t)

(import
  :std/srfi/1 :std/test
  :clan/debug :clan/poo/poo
  ../watch ../json-rpc ../nonce-tracker ../tx-tracker  ../transaction ../batch-call ../ethereum ../network-config
  ./signing-test ./30-transaction-integrationtest)

(def (process-filter filter)
  (display filter))

(def 70-watch-integrationtest
  (test-suite "integration test for ethereum/watch"
    (reset-nonce croesus) (DBG nonce: (peek-nonce croesus))
    (def trivial-logger (.@ (ensure-trivial-logger-contract croesus) contract-address))
    (test-case "watch-contract-step-normal-case"
      (def current-block (eth_blockNumber))
      (begin 
        (debug-send-tx (call-function croesus trivial-logger (string->bytes "hello, world")))
        (let ((values a b) (watch-contract-step trivial-logger current-block (+ current-block (* 4 (ethereum-confirmations-wanted-in-blocks))))) ;; Multiplication is there so that it would wait till the contract it completed if not it returns empty list.
          (check-equal? (null? a) #f))))

    (test-case "watch-contract-step-spurious from-block"
      (def current-block (eth_blockNumber))
      (def to-block (+ current-block (ethereum-confirmations-wanted-in-blocks)))
        (begin 
        (debug-send-tx (call-function croesus trivial-logger (string->bytes "hello, world"))) 
        (let ((values a b) (watch-contract-step trivial-logger to-block to-block ))
            (check-equal? (null? a) #t))))

    (test-case "watch-contract"
      (def current-block (eth_blockNumber))
      (def to-block (+ current-block (ethereum-confirmations-wanted-in-blocks)))
      (def present-block current-block)
       (begin
         (debug-send-tx (call-function croesus trivial-logger (string->bytes "hello, world")))
         (watch-contract identity trivial-logger current-block to-block)      
            (check-equal? (= (eth_blockNumber) current-block) #f)))
    ;(test-case "watchBlockchain"
      ;(def fromBlock (eth_blockNumber))
      ;(def receipt (batch-call croesus [[trivial-logger 0 (string->bytes "Nothing here")]
      ;                                  [trivial-logger (wei<-gwei 1) (string->bytes "Just lost one gwei")]]))
      ;(if (successful-receipt? receipt)
       ; (begin
        ;  (register-confirmed-event-hook "trivial-family" fromBlock  receipt process-filter)
        ;  (watchBlockchain)
        ;  (check-equal? (next-unprocessed-block)  (1+ fromBlock)))))
          ))
