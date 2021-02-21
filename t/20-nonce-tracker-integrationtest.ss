(export #t)

(import
  :std/sugar :std/test
  :clan/list :clan/path-config
  :clan/persist/db
  ../ethereum ../json-rpc ../transaction ../nonce-tracker ../testing)

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
      (reset-nonce croesus)
      (def initial (peek-nonce croesus))
      (check initial ? (cut <= 0 <>))
      (next-nonce croesus)
      (next-nonce croesus)
      (next-nonce croesus)
      (next-nonce croesus)
      (check-equal? (peek-nonce croesus) (+ 4 initial))
      (reset-nonce croesus)
      (check-equal? (peek-nonce croesus) initial))))
