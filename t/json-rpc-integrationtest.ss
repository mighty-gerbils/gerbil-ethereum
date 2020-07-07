(export #t)

(import
  :std/test
  ../network-config ../json-rpc)

(def json-rpc-integrationtest
  (test-suite "integration test for ethereum/json-rpc"
    (ensure-ethereum-config)
    (test-case "eth-latest-block get the current latest block"
      ;; Just checks that the block number is non-negative
      (check (eth_blockNumber timeout: 1.0) ? (cut <= 0 <>)))))
