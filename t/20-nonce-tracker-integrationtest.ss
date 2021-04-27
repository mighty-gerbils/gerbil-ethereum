(export #t)

(import
  :std/sugar :std/test
  :clan/list :clan/path-config
  :clan/persist/db
  ../ethereum ../json-rpc ../transaction ../nonce-tracker ../testing
  ./10-json-rpc-integrationtest ./15-db-integrationtest)

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
