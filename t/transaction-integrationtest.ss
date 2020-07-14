(export #t)

(import
  :std/format :std/test
  :utils/base :utils/concurrency :utils/path :utils/path-config
  :poo/io
  :persist/db
  ../types ../network-config ../signing ../known-addresses ../json-rpc ../transaction
  ./path-config ./signing-test)

;; Use the Private Ethereum Testnet
(load-ethereum-networks (subpath gerbil-ethereum-src "t/ethereum_networks.json"))
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
 (keypair ;; KLUGE: Fake public and secret key data. We only use the address via Geth.
  croesus
  (<-json PublicKey "0x020000000000000000000000000000000000000000000000000000000000000001")
  (<-json SecretKey "0x0000000000000000000000000000000000000000000000000000000000000001")
  ""))

;; Ensure Geth can issue transactions for all test accounts
(for-each ensure-eth-signing-key test-addresses)

(def transaction-integrationtest
  (test-suite "integration test for ethereum/transaction"
    (test-case "Nothing left to do"
      (void))))
