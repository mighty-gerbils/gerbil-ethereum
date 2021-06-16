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
    (def pc-logger (.@ (ensure-pc-logger-contract croesus) contract-address))

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

    ;; Test Utilites for `watch-contract`
    (def (watch-result->string watch-result) (bytes->string (.@ watch-result log data)))

    (def (check-block-ordering? watch-results before-block after-block)
      (for-each! watch-results
        (lambda (watch-result)
          (def next-block (.@ watch-result next-block))
          (def log-block (.@ watch-result log blockNumber))
          (check-equal? (<= log-block next-block) #t)))

      (def watch-log-blocks (map (cut .@ <> log blockNumber) watch-results))
      (check-equal? (apply <= [before-block watch-log-blocks ... after-block]) #t))

    (test-case "watch-contract: single log"
      (def before-block (eth_blockNumber))
      (def log-data "hello, world")
      (debug-send-tx (call-function croesus trivial-logger (string->bytes log-data)))
      (def after-block (eth_blockNumber))

      (def (callback watch-result)
        (def-slots (log next-block) watch-result)
        (def watch-log-data (bytes->string (.@ log data)))
        (check-equal? watch-log-data log-data)

        (def watch-log-block (.@ log blockNumber))
        (check-equal? (<= watch-log-block next-block) #t)
        (check-equal? (<= before-block watch-log-block after-block) #t))

      (watch-contract callback trivial-logger before-block after-block 0))

    (test-case "watch-contract: multiple distinct logs from multiple calls"
      (def before-block (eth_blockNumber))
      (def logs (list "hello, world 1" "hello, world 2" "hello, world 3"))
      (for-each! logs (lambda (l) (debug-send-tx (call-function croesus trivial-logger (string->bytes l)))))
      (def after-block (eth_blockNumber))

      (with-list-builder (push-res! get-res!)
        (watch-contract push-res! trivial-logger before-block after-block 0)
        (def watch-results (get-res!))
        (def watch-logs (map watch-result->string watch-results))
        (check-equal? watch-logs logs)
        (check-block-ordering? watch-results before-block after-block))
      )

    (test-case "watch-contract: resuming watch from unprocessed events"
      (def before-block (eth_blockNumber))
      (def logs (list "hello, world 1" "hello, world 2" "hello, world 3"))
      (for-each (lambda (l) (debug-send-tx (call-function croesus trivial-logger (string->bytes l)))) logs)
      (def after-block (eth_blockNumber))

      (with-list-builder (push-res! get-res!)
        (def unprocessed-event-in-block 1)
        (watch-contract push-res! trivial-logger before-block after-block unprocessed-event-in-block)
        (def watch-results (get-res!))
        (def watch-logs (map watch-result->string watch-results))
        (check-equal? watch-logs (cdr logs))
        (check-block-ordering? watch-results before-block after-block))
      )

    (test-case "watch-contract: multiple distinct logs from a single contract call"
      (def before-block (eth_blockNumber))
      (debug-send-tx (call-function croesus pc-logger (string->bytes "hello, world")))
      (def after-block (eth_blockNumber))

      (with-list-builder (push-res! get-res!)
        (watch-contract push-res! pc-logger before-block after-block 0)
        (def watch-results (get-res!))
        (def watch-logs (map (cut .@ <> log data) watch-results))
        (check-equal? watch-logs
          (list #u8(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                #u8(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 13)
                #u8(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 27)))
        (check-block-ordering? watch-results before-block after-block))
      )

    ;; TODO (test-case "watch-contract: large batches of logs / request timeout")

    ;; TODO (test-case "watch-contract: other errors")

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
