(export #t)

(import :std/test ../json-rpc)

;; Use our Private Ethereum Testnet
(ensure-ethereum-connection "pet")

(def json-rpc-integrationtest
  (test-suite "integration test for ethereum/json-rpc"
    (test-case "eth-latest-block get the current latest block"
      ;; Just checks that the block number is non-negative
      (check (eth_blockNumber timeout: 1.0) ? (cut <= 0 <>)))))
