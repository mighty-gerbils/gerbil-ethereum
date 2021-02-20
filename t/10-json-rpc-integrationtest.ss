(export #t)

(import :std/test :clan/poo/debug ../network-config ../json-rpc)

;; Use our Private Ethereum Testnet
(ensure-ethereum-connection (getenv "GERBIL_ETHEREUM_TEST_NETWORK" "pet"))

(def 10-json-rpc-integrationtest
  (test-suite "integration test for ethereum/json-rpc"
    (test-case "eth-latest-block get the current latest block"
      ;; Dump information about the Ethereum JSON RPC configuration
      (DDT json-rpc-integrationtest:
           EthereumNetworkConfig (ethereum-config)
           EthereumNetworkConnection (ethereum-connection))
      ;; Just checks that the block number is non-negative
      (check (eth_blockNumber timeout: 1.0) ? (cut <= 0 <>)))))
