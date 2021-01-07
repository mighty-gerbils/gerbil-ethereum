(export #t)

(import
  :gerbil/gambit/threads
  :std/format :std/sugar :std/test
  :clan/base :clan/debug :clan/failure :clan/json :clan/option :clan/path
  :clan/poo/poo :clan/poo/io
  :clan/persist/db
  ../hex ../types ../network-config ../signing ../known-addresses
  ../ethereum ../json-rpc ../nonce-tracker ../transaction ../watch ../contract-runtime
  ./signing-test ./10-json-rpc-integrationtest ./20-nonce-tracker-integrationtest)

;; Send a tx, not robust, but useful for debugging
(def (debug-send-tx tx)
  (def from (.@ tx from))
  (reset-nonce from)
  (def signed (make-signed-transaction tx))
  (ignore-errors (send-raw-transaction from signed))
  (def info (.cc (.@ signed tx) from: from))
  (def hash (.@ signed tx hash))
  (def receipt
    (let/cc return
      (while #t
        (try
         (return (confirmed-receipt<-transaction info hash confirmations: 0))
         (catch StillPending? => void)
         (catch (TransactionRejected? e) (return (TransactionRejected-receipt e))))
        (thread-sleep! (ethereum-block-polling-period-in-seconds)))))
  [success?: (successful-receipt? receipt)
   info: (sexp<- SignedTransactionInfo info)
   receipt: (sexp<- TransactionReceipt receipt)])


;; Block can be a block number, latest, earliest, pending, or commit.
;; if commit, then commit the evaluation to be inspected with remix.ethereum.org

;; Bytes <- Address Bytes value:?(Maybe Quantity) block:?(Or BlockParameter (Enum commit))
(def (evm-eval/offchain from code value: (value (void)) block: (block 'pending))
  (eth_call {from data: code value} block))

;; TransactionReceipt <- Address Bytes value:?(Maybe Quantity)
(def (evm-eval/onchain from code value: (value (void)))
  ;; Create a contract with the code
  (def receipt (send-and-confirm-transaction
                from (make-signed-transaction {from data: (stateless-contract-init code) value})))
  (def contract (.@ receipt contractAddress))
  ;; Call the contract with the value
  (debug-send-tx {from to: contract value}))

(def 30-transaction-integrationtest
  (test-suite "integration test for ethereum/transaction"
    (test-case "Send tokens from Croesus to Trent"
      (reset-nonce croesus) (DBG nonce: (peek-nonce croesus))
      (def signed
        (make-signed-transaction (transfer-tokens from: croesus to: trent value: (wei<-ether 2))))
      (def hash (.@ signed tx hash))
      (DBG signed: (sexp<- SignedTransaction signed))
      (ignore-errors (send-and-confirm-transaction croesus signed))
      (def current-block (eth_blockNumber))
      (def confirmationsWantedInBlocks (ethereum-confirmations-wanted-in-blocks))
      (DBG waiting-for-confirmation: current-block confirmationsWantedInBlocks)
      (def target-block (+ current-block confirmationsWantedInBlocks))
      (wait-until-block target-block)
      (def receipt (eth_getTransactionReceipt hash))
      (unless (poo? receipt) (error "No receipt for tx" (0x<-bytes hash) (repr receipt)))
      (DBG receipt: (sexp<- TransactionReceipt receipt))
      (def confirmations (confirmations<-receipt receipt target-block))
      (DBG confirmations: confirmations)
      (unless (<= 0 confirmations) (error "Failed tx" (0x<-bytes hash) (sexp<- TransactionReceipt receipt)))
      (def new-target-block (+ target-block (- confirmationsWantedInBlocks confirmations)))
      (wait-until-block new-target-block)
      (DBG new-block: (eth_blockNumber))
      (send-and-confirm-transaction croesus signed)
      (reset-nonce croesus))))
