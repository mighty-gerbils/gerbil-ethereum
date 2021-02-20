(export #t)

(import
  :std/misc/list :std/srfi/1 :std/test
  :clan/debug :clan/poo/object
  ../watch ../json-rpc ../transaction ../nonce-tracker ../testing ../simple-apps
  ./30-transaction-integrationtest)

(def (process-filter filter)
  (display filter))

(def 70-watch-integrationtest
  (test-suite "integration test for ethereum/watch"
    (reset-nonce croesus) (DBG nonce: (peek-nonce croesus))
    (def trivial-logger (.@ (ensure-trivial-logger-contract croesus) contract-address))
    (test-case "watch-contract-step"
      (def before-block (eth_blockNumber))
      (debug-send-tx (call-function croesus trivial-logger (string->bytes "hello, world")))
      (def after-block (eth_blockNumber))
      (defvalues (logs confirmed-block) (watch-contract-step trivial-logger before-block after-block))
      (check-equal? (length logs) 1)
      (check-equal? (<= before-block confirmed-block after-block) #t)
      (check-equal? (bytes->string (.@ (car logs) data)) "hello, world")
      (defvalues (more-logs more-confirmed-block)
        (watch-contract-step trivial-logger (1+ confirmed-block) confirmed-block))
      (check-equal? more-logs '())
      (check-equal? more-confirmed-block confirmed-block))

    (test-case "watch-contract"
      (def before-block (eth_blockNumber))
      (debug-send-tx (call-function croesus trivial-logger (string->bytes "hello, world")))
      (def after-block (eth_blockNumber))
      (def logs '())
      (def (process-log log) (push! log logs))
      (watch-contract process-log trivial-logger before-block after-block)
      (check-equal? (length logs) 1)
      (check-equal? (bytes->string (.@ (car logs) data)) "hello, world"))

#|
    (test-case "watchBlockchain"
      (def fromBlock (eth_blockNumber))
      (def receipt
        (batch-txs croesus
                   [(batched-call 0 trivial-logger (string->bytes "Nothing here"))
                    (batched-call (wei<-gwei 1) trivial-logger (string->bytes "Just lost one gwei"))]))
      (if (successful-receipt? receipt)
        (begin
          (register-confirmed-event-hook "trivial-family" fromBlock  receipt process-filter)
          (watchBlockchain)
          (check-equal? (next-unprocessed-block)  (1+ fromBlock)))))
|#
          ))
