(export #t)

(import
  :gerbil/gambit/exceptions
  :std/test
  :clan/exception :clan/poo/poo
  ../ethereum ../json-rpc ../nonce-tracker ../transaction ../tx-tracker
  ./signing-test ./30-transaction-integrationtest)

(def 40-tx-tracker-integrationtest
  (test-suite "integration test for ethereum/tx-tracker"
    (test-case "Simple transfer"
      (.call NonceTracker reset croesus)
      (def before (eth_getBalance alice 'latest))
      (def value (* 1/100 one-ether-in-wei))
      (def _receipt (post-transaction (transfer-tokens from: croesus to: alice value: value)))
      (def after (eth_getBalance alice 'latest))
      (check-equal? (- after before) value))))
