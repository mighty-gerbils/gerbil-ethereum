(export #t)

(import
  :std/srfi/1 :std/test
  ../watch ../json-rpc ../nonce-tracker
  ./signing-test ./30-transaction-integrationtest)

(def 70-watch-integrationtest
  (test-suite "integration test for ethereum/watch"
    (test-case "foo"
;;XXX
      (void))))
