(export #t)

(import
  :std/srfi/1 :std/test
  :clan/debug :clan/poo/poo
  ../watch ../json-rpc ../nonce-tracker ../testing
  ./30-transaction-integrationtest)

(def 70-watch-integrationtest
  (test-suite "integration test for ethereum/watch"
    (reset-nonce croesus) (DBG nonce: (peek-nonce croesus))
    (test-case "foo"
;;XXX
      (void))))
