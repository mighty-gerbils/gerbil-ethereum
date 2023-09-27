(export #t)

(import
  :gerbil/gambit
  :std/assert :std/misc/list :std/srfi/1 :std/test
  :clan/base :clan/debug :clan/order :clan/poo/object :clan/poo/mop
  ../watch ../json-rpc ../known-addresses ../transaction ../nonce-tracker ../contract-config
  ../ethereum ../assembly ../evm-runtime ../simple-apps ../logger ../testing
  ./30-transaction-integrationtest)

(def (watch-contract-logs contract-address start-block end-block (next-event 0))
  (with-list-builder (c)
    (def prev [-1 0])
    (watch-contract
      (lambda (log)
        (def curr [(.@ log blockNumber) (.@ log logIndex)])
        (check-equal? (<= start-block (.@ log blockNumber) end-block) #t)
        (check-equal? (lexicographic<? < prev curr) #t)
        (set! prev curr)
        (c log))
      contract-address start-block end-block next-event)))

(def (log->string log) (bytes->string (.@ log data)))

(def (watch-contract-log-strings contract-address start-block end-block (next-event 0))
  (map log->string (watch-contract-logs contract-address start-block end-block next-event)))

(def (bytes32<-string s)
  (def b (string->bytes s))
  (def l (u8vector-length b))
  (assert! (<= l 32))
  (u8vector-append b (make-u8vector (- 32 l) 0)))

;; Used to test `watch` with multiple ordered logs.
(def (string-logger-runtime strings)
  (def (log-string s)
    [(bytes32<-string s) 0 MSTORE CALLER (u8vector-length (string->bytes s)) 0 LOG1])
  (assemble/bytes [(append-map log-string strings)... STOP]))

;; : Bytes <-
(def string-logger-init
  (compose stateless-contract-init string-logger-runtime))

;; : Bytes <-
(def (ensure-string-logger-contract owner strings log: (log eth-log))
  (ensure-contract-config/db
   (string->bytes "string-logger-contract")
   (create-contract owner (string-logger-init strings))
   log: log))

(def 70-watch-integrationtest
  (test-suite "integration test for ethereum/watch"
    (reset-nonce croesus) (DBG nonce: (peek-nonce croesus))
    (def string-logs ["The Biggest City"
                      "Only makes you truly feel"
                      "Crushed by loneliness."])
    (def trivial-logs ["The Greatest Distance"
                       "I pride in having traveled:"
                       "My heart to your heart."])
    (def all-logs (append trivial-logs string-logs))

    (def trivial-logger (.@ (ensure-trivial-logger-contract croesus) contract-address))
    (def string-logger (.@ (ensure-string-logger-contract croesus string-logs) contract-address))

    (def receipt
      (batch-txs croesus
                 [(map (lambda (s) (batched-call 0 trivial-logger (string->bytes s))) trivial-logs)...
                  (batched-call (wei<-gwei 1) string-logger (string->bytes "Just lost one gwei"))]))
    (def block-number (.@ receipt blockNumber))

    (test-case "watch-contract: batch"
      ;; watch a contract, or all contracts
      (check-equal? (watch-contract-log-strings trivial-logger block-number block-number)
                    trivial-logs)
      (check-equal? (watch-contract-log-strings string-logger block-number block-number)
                    string-logs)
      (check-equal? (watch-contract-log-strings (void) block-number block-number)
                    all-logs)
      ;; resume watch after first entry, for a contract, or all of them
      (check-equal? (watch-contract-log-strings trivial-logger block-number block-number 1)
                    (cdr trivial-logs))
      (check-equal? (watch-contract-log-strings string-logger block-number block-number 4)
                    (cdr string-logs))
      (check-equal? (watch-contract-log-strings (void) block-number block-number 1)
                    (cdr all-logs)))

    (test-case "watch-contract: blah"
      (def before-block (eth_blockNumber))
      (def receipts (map-in-order
                     (lambda (l) (debug-send-tx (call-function croesus trivial-logger (string->bytes l))))
                     trivial-logs))
      (def after-block (eth_blockNumber))
      (check-equal? (watch-contract-log-strings trivial-logger (1+ before-block) after-block 0)
                    trivial-logs))

    ;; TODO (test-case "watch-contract: large batches of logs / request timeout")
    ;; TODO (test-case "watch-contract: other errors")

    (void)))
