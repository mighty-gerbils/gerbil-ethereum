(export #t)

(import
  :std/format :std/test
  :clan/base :clan/concurrency :clan/path :clan/path-config
  :clan/poo/io
  :clan/persist/db
  ../types ../network-config ../signing ../known-addresses ../json-rpc ../transaction
  ./signing-test)

;; Use the Private Ethereum Testnet
(load-ethereum-networks (config-path "ethereum_networks.json"))
(ensure-ethereum-network "pet")

;; Poll for ethereum server
(retry retry-window: 0.05 max-window: 1.0 max-retries: 10
       (cut eth_blockNumber timeout: 1.0))

;; Use the test database
(ensure-db-connection (run-path "testdb"))

;; Create a key for the initial have-it-all user of the Geth test network
(def croesus (get-first-account))

(register-keypair
 "Croesus"
 (keypair ;; SPECIAL: no public and secret key data. We only use the address via Geth withits password
  ;; TODO: extract the private data from geth storage somehow?
  croesus #f #f (import-password/string "")))

;; Ensure Geth can issue transactions for all test accounts
(for-each ensure-eth-signing-key test-addresses)

(def transaction-integrationtest
  (test-suite "integration test for ethereum/transaction"
    (test-case "Nothing left to do"
      (void))))
