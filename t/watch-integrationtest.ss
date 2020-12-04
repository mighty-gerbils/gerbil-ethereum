(export #t)

(import
  :std/srfi/1 :std/test
  ../watch ../json-rpc
  ./signing-test ./transaction-integrationtest)

(def watch-integrationtest
  (test-suite "integration test for ethereum/watch"
    (test-case "foo"
;;XXX
      (void))))
