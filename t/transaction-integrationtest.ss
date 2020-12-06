(export #t)

(import
  :std/format :std/test
  :clan/base :clan/concurrency :clan/failure :clan/json :clan/option :clan/path :clan/path-config
  :clan/poo/poo :clan/poo/io
  :clan/persist/db
  ../types ../network-config ../signing ../known-addresses ../json-rpc ../transaction
  ./signing-test)

;; Use our Private Ethereum Testnet
(ensure-ethereum-network "pet")

;; Poll for ethereum server
(retry retry-window: 0.05 max-window: 1.0 max-retries: 10
       (lambda () (displayln "Connecting to the private ethereum test net...")
          (eth_blockNumber timeout: 1.0)))

;; Detect if we're using a node that doesn't recognize EIP-155, at which point we'll set the chainId to 0.
(def configured-chain-id (ethereum-chain-id))
(def err #f)
(def (adjust-chain-id)
  (match (with-result (eth_chainId))
    ((some (?(cut equal? <> configured-chain-id) id))
     (printf "The node's chainId and the configured one are both ~d. Good.\n" id))
    ((some id)
     (printf "The node's chainId is ~d but the configured one is ~d. Bad. Using ~d for tests.\n"
             id configured-chain-id id)
     (current-ethereum-network (.cc (current-ethereum-network) chainId: id)))
    ((failure e)
     (set! err e)
     (printf "The node doesn't report knowledge of EIP-155 style chainId, treating it as 0.\n")
     (if (zero? configured-chain-id)
       (printf "The configuration agrees.\n")
       (begin
         (printf "However, the configuration said ~d. Using 0 instead.\n" configured-chain-id)
         (current-ethereum-network (.cc (current-ethereum-network) chainId: 0)))))))
(adjust-chain-id)

;; Use the test database
(displayln "Connecting to the test database...")
(ensure-db-connection (run-path "testdb"))

(def transaction-integrationtest
  (test-suite "integration test for ethereum/transaction"
    (test-case "Nothing left to do"
      (void))))
