(export #t)

(import
  :std/srfi/1 :std/test
  :clan/debug :clan/poo/poo
  ../watch ../json-rpc ../nonce-tracker ../tx-tracker  ../transaction ../batch-call ../ethereum
  ./signing-test ./30-transaction-integrationtest)

(def (process-filter filter)
  (display filter))

(def 70-watch-integrationtest
  (test-suite "integration test for ethereum/watch"
    (reset-nonce croesus) (DBG nonce: (peek-nonce croesus))
    (def trivial-logger (.@ (ensure-trivial-logger-contract croesus) contract-address))
    (test-case "watch-contract-step-normal-case"
      (def receipt (post-transaction (call-function croesus trivial-logger (string->bytes "hello, world"))))  
      (def current-block (eth_blockNumber))
      (if (successful-receipt? receipt)
           (let ((values a b) (watch-contract-step trivial-logger current-block (+ current-block 10) confirmations: 1))
              (check-equal? (null? a) #f))))

    (test-case "watch-contract-step-spurious from-block"
      (def current-block (eth_blockNumber))
      (def to-block (+ current-block 10)) ;;I made up 10
      (def receipt (post-transaction (call-function croesus trivial-logger (string->bytes "hello, world"))))  
      (if (successful-receipt? receipt)
        (let ((values a b) (watch-contract-step trivial-logger (+ current-block 20) to-block confirmations: 1))
            (check-equal? b to-block)
            (check-equal? (null? a) #t))))

    (test-case "watch-contract"
      (def current-block (eth_blockNumber))
      (def to-block (+ current-block 10)) ;;I made up 10
      (def present-block current-block)
      (def receipt (post-transaction (call-function croesus trivial-logger (string->bytes "hello, world"))))
      (if (successful-receipt? receipt)
       (begin
         (watch-contract identity trivial-logger current-block to-block confirmations: 1)
         (check-equal? (- (eth_blockNumber) 1) to-block))))
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
