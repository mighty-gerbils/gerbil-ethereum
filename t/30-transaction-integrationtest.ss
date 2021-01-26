(export #t)

(import
  :gerbil/gambit/threads
  :std/format :std/sugar :std/test
  :clan/base :clan/debug :clan/failure :clan/json :clan/option :clan/path
  :clan/poo/poo :clan/poo/io :clan/poo/debug :clan/poo/brace
  :clan/persist/db
  ../hex ../types ../network-config ../signing ../testing ../known-addresses
  ../ethereum ../json-rpc ../nonce-tracker ../transaction ../watch ../contract-runtime
  ./10-json-rpc-integrationtest ./20-nonce-tracker-integrationtest)

(def 30-transaction-integrationtest
  (test-suite "integration test for ethereum/transaction"
    (test-case "Send tokens from Croesus to Trent"
      (reset-nonce croesus) (DDT nonce: Any (peek-nonce croesus))
      (def value (wei<-ether 2))
      (def before (eth_getBalance trent 'latest))
      (debug-send-tx (transfer-tokens from: croesus to: trent value: value))
      (def after (eth_getBalance trent 'latest))
      (check-equal? (- after before) value))))
