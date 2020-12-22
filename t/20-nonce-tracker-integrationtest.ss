(export #t)

(import
  :std/sugar :std/test
  :clan/list :clan/path-config
  :clan/poo/poo
  :clan/persist/db
  ../signing ../json-rpc ../transaction ../nonce-tracker
  ./signing-test)

;; Use the test database
(displayln "Connecting to the test database...")
(ensure-db-connection (run-path "testdb"))

;; TODO: validate that the testdb indeed corresponds to this test net?
;; - At minimum, check that the last confirmed nonces for croesus, etc.,
;; are not past the nonce from getTransactionCount.
;; - Maybe even check that the blocks mentioned in the transaction Confirmations still exist.

(def 20-nonce-tracker-integrationtest
  (test-suite "integration test for ethereum/nonce-tracker"
    (test-case "Croesus nonce tracker"
      (.call NonceTracker reset croesus)
      (def initial (.call NonceTracker peek croesus))
      (check initial ? (cut <= 0 <>))
      (.call NonceTracker next croesus)
      (.call NonceTracker next croesus)
      (.call NonceTracker next croesus)
      (.call NonceTracker next croesus)
      (check-equal? (.call NonceTracker peek croesus) (+ 4 initial))
      (.call NonceTracker reset croesus)
      (check-equal? (.call NonceTracker peek croesus) initial))))
