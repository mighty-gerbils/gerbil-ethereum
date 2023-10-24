(export #t)

(import
  :gerbil/gambit
  :std/test
  :clan/debug :clan/exception
  ../ethereum ../json-rpc ../nonce-tracker ../transaction ../tx-tracker ../testing
  ./30-transaction-integrationtest)

(def 40-tx-tracker-integrationtest
  (test-suite "integration test for ethereum/tx-tracker"
    (test-case "Simple transfer"
      (reset-nonce croesus) (DBG nonce: (peek-nonce croesus))
      (def before (eth_getBalance alice 'latest))
      (def value (* 1/100 one-ether-in-wei))
      (def _receipt (post-transaction (transfer-tokens from: croesus to: alice value: value)))
      (def after (eth_getBalance alice 'latest))
      (check-equal? (- after before) value))))
