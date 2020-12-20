(export #t)

(import
  :std/format :std/test
  :clan/base :clan/debug :clan/failure :clan/json
  :clan/option :clan/path :clan/path-config
  :clan/poo/poo :clan/poo/io
  :clan/persist/db
  ../types ../network-config ../signing ../known-addresses
  ../ethereum ../json-rpc ../transaction ../watch
  ./signing-test ./json-rpc-integrationtest)

;; Use the test database
(displayln "Connecting to the test database...")
(ensure-db-connection (run-path "testdb"))

;; TODO: validate that the testdb indeed corresponds to this test net?

(def transaction-integrationtest
  (test-suite "integration test for ethereum/transaction"
    (test-case "Send tokens from Croesus to Trent"
      (def signed (make-signed-transaction (transfer-tokens from: croesus to: trent value: (wei<-ether 2))))
      (def hash (.@ signed tx hash))
      (DBG foo: (sexp<- SignedTransaction signed))
      (ignore-errors (send-and-confirm-transaction croesus signed))
      (def current-block (eth_blockNumber))
      (def confirmationsWantedInBlocks (ethereum-confirmations-wanted-in-blocks))
      (DBG waiting-for-confirmation: current-block confirmationsWantedInBlocks)
      (def target-block (+ current-block confirmationsWantedInBlocks))
      (wait-until-block target-block)
      (def receipt (eth_getTransactionReceipt hash))
      (unless (poo? receipt) (error "No receipt for tx" hash receipt))
      (DBG receipt: (sexp<- TransactionReceipt receipt))
      (def confirmations (confirmations<-receipt receipt target-block))
      (DBG confirmations: confirmations)
      (unless (<= 0 confirmations) (error "Failed tx" hash receipt))
      (def new-target-block (+ target-block (- confirmationsWantedInBlocks confirmations)))
      (wait-until-block new-target-block)
      (DBG new-block: (eth_blockNumber))
      (send-and-confirm-transaction croesus signed))))
