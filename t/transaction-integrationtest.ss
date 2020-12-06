(export #t)

(import
  :std/format :std/test
  :clan/base :clan/concurrency :clan/json :clan/path :clan/path-config
  :clan/poo/io
  :clan/persist/db
  ../types ../network-config ../signing ../known-addresses ../json-rpc ../transaction
  ./signing-test)

;; Use our Private Ethereum Testnet
(ensure-ethereum-network "pet")

;; Poll for ethereum server
(retry retry-window: 0.05 max-window: 1.0 max-retries: 10
       (lambda () (displayln "Connecting to the private ethereum test net...")
          (eth_blockNumber timeout: 1.0)))

;; Use the test database
(displayln "Connecting to the test database...")
(ensure-db-connection (run-path "testdb"))

(def transaction-integrationtest
  (test-suite "integration test for ethereum/transaction"
    (test-case "Nothing left to do"
      (void))))
