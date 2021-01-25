(export #t)

(import
  :std/sugar
  :clan/exception :clan/json :clan/path-config
  :clan/poo/poo :clan/poo/brace :clan/poo/io
  :clan/persist/db
  :clan/crypto/keccak
  ./hex ./types ./known-addresses ./signing ./ethereum ./logger ./json-rpc ./transaction ./tx-tracker)

(define-type ContractConfig
  (Record
   contract-address: [Address]
   code-hash: [Digest]
   creation-hash: [Digest]
   creation-block: [Quantity]))

(def (contract-config<-file config-filename)
  (<-json ContractConfig (read-file-json (config-path config-filename))))

(def (file<-contract-config config-filename config)
  (write-file-json (config-path config-filename) (json<- ContractConfig config)))

(def (contract-config<-db db-key)
  (<-bytes ContractConfig
           (or (with-tx (tx) (db-get db-key tx))
               (error "No contract configuration in DB" db-key))))

(def (db<-contract-config db-key config)
  (with-tx (tx) (db-put! db-key (bytes<- ContractConfig config)) tx))

;; Query the Ethereum for the configuration given the hash of the transaction creating the contract
;; ContractConfig <- TransactionReceipt
(def (contract-config<-creation-receipt receipt)
  (unless (successful-receipt? receipt)
    (error "No receipt for contract creation tx" receipt))
  (def creation-hash (.@ receipt transactionHash))
  (def transaction-info (eth_getTransactionByHash creation-hash))
  (def contract-address (.@ receipt contractAddress))
  (def creation-block (.@ receipt blockNumber))
  (def code-hash (keccak256<-bytes (.@ transaction-info input)))
  {contract-address code-hash creation-hash creation-block})

;; Digest <- PreTransaction
(def (code-hash<-create-contract pretx)
  (keccak256<-bytes (.@ pretx data)))

;; <- ContractConfig PreTransaction
(def (verify-contract-config config pretx)
  (def chain-config (contract-config<-creation-receipt
                     (eth_getTransactionReceipt (.@ config creation-hash))))
  ;; TODO: automatically implement equality for records, better than that.
  (display-poo-ln verify-contract-config: "\n"
                  config: ContractConfig config "\n"
                  chain-config: ContractConfig chain-config)
  (unless (equal? (bytes<- ContractConfig config)
                  (bytes<- ContractConfig chain-config))
    (error "Contract configuration not matched by on-chain transaction"
      (sexp<- ContractConfig config) (sexp<- ContractConfig chain-config)))
  (unless (equal? (code-hash<-create-contract pretx) (.@ chain-config code-hash))
    (error "Contract configuration doesn't match expected transaction"
      (sexp<- ContractConfig config) (sexp<- PreTransaction pretx))))

;; : ContractConfig <-
;;     (ContractConfig <- 'a) (Unit <- 'a ContractConfig) 'a
;;     PreTransaction log:(OrFalse (<- Json))
(def (ensure-contract getter setter arg pretx log: (log eth-log))
  (def creator (.@ pretx from))
  (log ['ensure-contract
        creator: (0x<-address creator)
        nickname: (nickname<-address creator)
        code-hash: (json<- Digest (code-hash<-create-contract pretx))])
  (try
   (def previous-config (getter arg))
   (log ['ensure-contract-found (json<- ContractConfig previous-config)])
   (verify-contract-config previous-config pretx)
   (log ['ensure-contract-valid (json<- ContractConfig previous-config)])
   previous-config
   (catch (e)
     (log ['ensure-contract-create-because (string<-exception e)])
     (def creation-receipt (post-transaction pretx))
     (def config (contract-config<-creation-receipt creation-receipt))
     (log ['ensure-contract-created
           creator: (0x<-address creator)
           nickname: (nickname<-address creator)
           config: (json<- ContractConfig config)])
     (setter arg config)
     config)))

;; : ContractConfig <- PathString (Digest <-)
(def (ensure-contract-config/file filename pretx log: (log #f))
  (ensure-contract contract-config<-file file<-contract-config filename pretx log: log))

;; : ContractConfig <- DBKey (Digest <-)
(def (ensure-contract-config/db db-key pretx log: (log #f))
  (ensure-contract contract-config<-db db<-contract-config db-key pretx log: log))
