(export #t)

(import
  :gerbil/gambit/exceptions
  :std/test
  :clan/exception :clan/poo/poo
  ../ethereum ../json-rpc ../transaction ../tx-tracker
  ./signing-test ./transaction-integrationtest)

(def tx-tracker-integrationtest
  (test-suite "integration test for ethereum/tx-tracker"
    (test-case "Simple transfer"
      (def before (eth_getBalance alice 'latest))
      (def value (* 1/100 one-ether-in-wei))
      (def _receipt (post-transaction (transfer-tokens from: croesus to: alice value: value)))
      (def after (eth_getBalance alice 'latest))
      (check-equal? (- after before) value))))
