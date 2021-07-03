;; Ethereum JSON-RPC API
;;
;; The reference documentation for the Ethereum JSON RPC protocol is now at:
;;   https://eth.wiki/json-rpc/API
;; We support all non-deprecated methods in the standard protocol as of 2020-07-05,
;;
;; Geth extensions are documented here:
;;   https://geth.ethereum.org/docs/rpc/server
;; We only support a few of the Geth extensions.
;;
;; The OpenEthereum implementation by Parity has its JSON RPC documentation here:
;;   https://openethereum.wiki/JSONRPC/
;;
;; Infura documents its API at:
;;   https://infura.io/docs/ethereum/json-rpc/eth-chainId
;;
;; TODO: compare with https://playground.open-rpc.org/?schemaUrl=https://raw.githubusercontent.com/etclabscore/ethereum-json-rpc-specification/master/openrpc.json&uiSchema%5BappBar%5D%5Bui:input%5D=false
;; https://raw.githubusercontent.com/etclabscore/ethereum-json-rpc-specification/master/openrpc.json
;;
;; TODO:
;; - Resolve all the dark spots.
;; - Systematically lift all the Geth extensions.
;; - Add plenty of tests everywhere.
;; - Support multiple eth-like networks from a same client.

(export #t)

(import
  :gerbil/gambit/bytes :gerbil/gambit/os :gerbil/gambit/ports :gerbil/gambit/threads
  (for-syntax :std/format)
  :std/format :std/iter :std/lazy :std/pregexp :std/sugar :std/srfi/13
  :clan/base :clan/concurrency :clan/failure :clan/json :clan/logger :clan/maybe :clan/option :clan/syntax
  :clan/net/json-rpc
  :clan/poo/object :clan/poo/brace :clan/poo/io
  :clan/crypto/secp256k1
  ./types ./ethereum ./network-config ./logger)

;; This  file contains API Key of end nodes like infura and others.
;; File contain `{"END_NODE_API_KEY": "0123456789abcdef0123456789abcdef"}` . Remember to use a valid key
(def api-key-file "url_substitutions.json")

;; We use a mutex for access to the ethereum node, not to overload it and get timeouts.
;; TODO: Have a pool of a small number of connections to the node rather than just one.
(def ethereum-mutex (make-mutex 'ethereum))

(def (ethereum-json-rpc method-name result-decoder param-encoder
                        timeout: (timeout #f) log: (log eth-log) url: (url (ethereum-url))
                        params)
  (with-lock ethereum-mutex
             (cut json-rpc url method-name params
                  result-decoder: result-decoder
                  param-encoder: param-encoder
                  timeout: timeout log: log)))

(defsyntax (define-ethereum-api stx)
  (syntax-case stx (<-)
    ((_ namespace method result-type <- argument-type ...)
     (let*-values (((method-name method-formals args-vector)
                    (syntax-case #'method ()
                      ((name . formals)
                       (values #'name #'formals (call<-formals #'(vector) #'formals)))
                      (name
                       (let* ((n (length (syntax->datum #'(argument-type ...))))
                              (vars (formals<-nat n)))
                         (values #'name vars (cons 'vector vars))))))
                   ((method-string) (format "~a_~a" (syntax->datum #'namespace)
                                            (syntax->datum method-name)))
                   ((fun-id) (datum->syntax (stx-car stx) (string->symbol method-string))))
       (with-syntax (((formals ...) method-formals)
                     (args-vector args-vector)
                     (method-string method-string)
                     (fun-id fun-id))
         #'(begin
             (def params-type (Tuple argument-type ...))
             (def (fun-id timeout: (timeout #f) log: (log eth-log) url: (url (ethereum-url)) formals ...)
                 (ethereum-json-rpc method-string
                                    (.@ result-type .<-json)
                                    (.@ params-type .json<-) args-vector
                                    timeout: timeout log: log url: url))))))))

(define-ethereum-api web3 clientVersion
  String <-)
(define-ethereum-api web3 sha3
  Bytes32 <- Bytes) ;; keccak256

(define-ethereum-api net version
  String <-) ;; a decimal number in a String
(define-ethereum-api net listening
  Bool <-)
(define-ethereum-api net peerCount
  Quantity <-)

(define-ethereum-api eth protocolVersion
  String <-) ;; a decimal number

(define-type SyncingStatus
  (Record startingBlock: [Quantity]
          currentBlock: [Quantity]
          highestBlock: [Quantity]))
(define-ethereum-api eth syncing
  (OrFalse SyncingStatus) <-)

(define-ethereum-api eth coinbase
  Address <-)
(define-ethereum-api eth mining
  Bool <-)
(define-ethereum-api eth hashrate
  Quantity <-)

(define-type BlockParameter
  (Union
   Quantity ;; block number as 0x string. In practice, should fit 32-bit
   (Enum latest earliest pending)))

(define-type TransactionCondition
  (Union
   (Record block: [Quantity]) ;; block number as 0x string.
   (Record time: [Quantity]) ;; time in seconds-since-epoch as 0x string
   Unit)) ;; JSON null, isomorphic to unit, but its own thing for faithful FFI purposes. (void) in Gerbil.

;; TODO: Implement Record inheritance, and have it be just Transaction plus a condition
(define-type TransactionParameters
  (Record
   from: [Address]
   to: [(Maybe Address) optional: #t default: (void)]
   gas: [(Maybe Quantity) optional: #t default: (void)] ; in gas
   gasPrice: [(Maybe Quantity) optional: #t default: (void)] ; in wei/gas
   value: [(Maybe Quantity) optional: #t default: (void)] ; in wei
   data: [(Maybe Bytes) optional: #t default: (void)]
   nonce: [(Maybe Quantity) optional: #t default: (void)]
   condition: [(Maybe TransactionCondition) optional: #t default: (void)]))

(def (TransactionParameters<-PreTransaction tx)
  (def-slots (from to data value nonce gas gasPrice) tx)
  {from to data value nonce gas gasPrice condition: (void)})

(define-type TransactionInformation
  (Record
   hash: [Digest]
   nonce: [Quantity]
   blockHash: [(Maybe Digest) optional: #t default: (void)]
   blockNumber: [(Maybe Quantity) optional: #t default: (void)]
   transactionIndex: [(Maybe Quantity) optional: #t default: (void)]
   from: [(Maybe Address) optional: #t default: (void)]
   to: [(Maybe Address) optional: #t default: (void)]
   value: [Quantity]
   gasPrice: [Quantity]
   gas: [Quantity]
   input: [Bytes] ;; TODO: make sure *we* are always translating input: to/from data: where needed
   v: [(Maybe Quantity) optional: #t default: (void)]
   standard-v: [(Maybe Quantity) optional: #t default: (void)]
   r: [(Maybe UInt256) optional: #t default: (void)]
   s: [(Maybe UInt256) optional: #t default: (void)]
   raw: [(Maybe Data) optional: #t default: (void)]
   publicKey: [(Maybe PublicKey) optional: #t default: (void)]
   networkID: [(Maybe Quantity) optional: #t default: (void)]
   creates: [(Maybe Digest) optional: #t default: (void)]
   condition: [Json optional: #t default: (void)])) ;; is this any JSON, or a TransactionCondition above??

(define-type SignTransactionResult
  (Record
   data: [Bytes]
   signed: [TransactionInformation]))

(define-type Bloom Bytes256)

(define-type LogObject
  (Record
   removed: [(Maybe Bool) optional: #t default: (void)] ;; not present on Mantis (at least outside eth_newFilter)
   logIndex: [(Maybe Quantity) optional: #t default: (void)]
   transactionIndex: [(Maybe Quantity) optional: #t default: (void)]
   transactionHash: [(Maybe Digest) optional: #t default: (void)]
   blockNumber: [(Maybe Quantity) optional: #t default: (void)]
   blockHash: [(Maybe Digest) optional: #t default: (void)]
   address: [Address]
   data: [Bytes]
   topics: [(List Bytes32)]))
(define-type LogObjectList (List LogObject))

(define-type TransactionReceipt
  (Record
   blockHash: [Digest]
   blockNumber: [Quantity]
   contractAddress: [(Maybe Address) optional: #t default: (void)]
   cumulativeGasUsed: [Quantity]
   from: [(Maybe Address) optional: #t default: (void)] ;; in geth, not in mantis
   to: [(Maybe Address) optional: #t default: (void)] ;; in geth, not in mantis
   gasUsed: [Quantity]
   logs: [LogObjectList]
   logsBloom: [(Maybe Bloom) optional: #t default: (void)] ;; in geth, not mantis
   status: [(Maybe Quantity) optional: #t default: (void)] ;; in geth, not mantis: 1 success, 0 failure
   statusCode: [(Maybe Quantity) optional: #t default: (void)] ;; in mantis, not geth: 0 success, >0 error code
   transactionHash: [Digest]
   transactionIndex: [Quantity]
   returnData: [(Maybe Data) optional: #t default: (void)])) ;; in mantis, not geth.
;; Mantis statusCode decoding:
;;    0x00: success
;;    0x01: function does not exist
;;    0x02: function has wrong signature
;;    0x03: function does not exist on empty account
;;    0x04: execution of instructions led to failure
;;    0x05: out of gas
;;    0x06: deploying to an account that already exists
;;    0x07: insufficient balance to transfer
;;    0x08: negative balance or gas limit or call depth exceeded
;;    0x09: contract being uploaded to blockchain is not well formed

(def (Confirmation<-TransactionReceipt tr)
  (def-slots (transactionHash transactionIndex blockNumber blockHash status) tr)
  (if (zero? status)
    (error "receipt indicates transaction failed" transactionHash)
    {transactionHash transactionIndex blockNumber blockHash})) ;; Confirmation

;; Returns a list of address owned by the client
(define-ethereum-api eth accounts
  (List Address) <-)

;; same as Transaction, but without nonce
(define-type CallParameters
  (Record
   from: [Address]
   to: [(Maybe Address) optional: #t default: (void)]
   gas: [(Maybe Quantity) optional: #t default: (void)]
   gasPrice: [(Maybe Quantity) optional: #t default: (void)]
   value: [(Maybe Quantity) optional: #t default: (void)]
   data: [(Maybe Bytes) optional: #t default: (void)]))

(define-type StateOverrideSet ;; contract data to override before executing the call
  (Record
   balance: [Quantity optional: #t]
   nonce: [Quantity optional: #t]
   code: [Bytes optional: #t]
   state: [(Map Bytes32 <- Quantity) optional: #t] ;; keys are 0x quantity
   stateDiff: [(Map Bytes32 <- Quantity) optional: #t])) ;; override individual slots in account storage

;; TODO: Geth has an optional third parameter StateOverrideSet
(define-ethereum-api eth (call params (block 'latest))
  Data <- CallParameters BlockParameter)

(define-ethereum-api eth chainId
  (Maybe UInt256) <-)

;; SignedTransactionData + {hash}
(define-type SignedTx
  (Record
   nonce: [Quantity]
   gasPrice: [Quantity]
   gas: [Quantity]
   to: [(Maybe Address) optional: #t default: (void)]
   value: [Quantity]
   input: [Bytes]
   v: [UInt256] ;; why did an earlier version of our code indicate v, r, s as optional???
   r: [UInt256]
   s: [UInt256]
   hash: [Digest]))

(define-type SignedTransaction
  (Record
   raw: [Bytes]
   tx: [SignedTx]))

;; Returns estimate of gas needed for transaction
(define-ethereum-api eth estimateGas
  Quantity <- TransactionParameters)

;; Get the current gas price in wei
(define-ethereum-api eth gasPrice
  Quantity <-)

;; Returns the balance of the account of given address (and block)
(define-ethereum-api eth (getBalance address (block 'latest))
  Quantity <- Address BlockParameter)

;; Returns the content of storage in given contract at given memory position, given block
(define-ethereum-api eth (getStorageAt address position (block 'latest))
  Bytes32 <- Address Quantity BlockParameter)

;; Returns the code of given address (and block)
(define-ethereum-api eth (getCode contract-address (block 'latest))
  Bytes <- Address BlockParameter)

;; Returns a transaction by the hash code
(define-ethereum-api eth getTransactionByHash
  (Maybe TransactionInformation) <- Digest)

;; Returns a transaction by block hash and transaction index position
(define-ethereum-api eth getTransactionByBlockHashAndIndex
  TransactionInformation <- Digest Quantity)

;; Returns a transaction by block height and transaction index position
(define-ethereum-api eth getTransactionByBlockNumberAndIndex
  TransactionInformation <- BlockParameter Quantity)

;; Returns the number of transaction at address (and transaction)
(define-ethereum-api eth (getTransactionCount address (block 'latest))
  Quantity <- Address BlockParameter)

;; Returns the number of transactions in a block found by its hash code
(define-ethereum-api eth getTransactionCountByHash
  Quantity <- Digest)

;; Returns the number of transactions in a block found by its height
(define-ethereum-api eth getTransactionCountByNumber
  Quantity <- BlockParameter)

;; Returns the number of uncles in a block found by its hash
(define-ethereum-api eth getUncleCountByBlockHash
  Quantity <- Digest)

;; Returns the number of uncles in a block found by its height
(define-ethereum-api eth getUncleCountByNumber
  Quantity <- BlockParameter)

;; Returns uncle information
(define-ethereum-api eth getUncleByBlockHashAndIndex
  BlockInformation <- Digest Quantity)
(define-ethereum-api eth getUncleByBlockNumberAndIndex
  BlockInformation <- BlockParameter Quantity)

;; Returns a receipt of transaction by transaction hash (not available if transaction still pending)
(define-ethereum-api eth getTransactionReceipt
  (Maybe TransactionReceipt) <- Digest)

;; Create new message call transaction or a contract creation for signed transaction
(define-ethereum-api eth sendRawTransaction
  Digest <- Data)

;; NB: Not to be used in our code, it's too flaky wrt various attacks.
;; Creates new message call transaction or a contract creation if the datafield contains code.
(define-ethereum-api eth sendTransaction
  Digest <- TransactionParameters)

;; Computes an eth signature of (eth-sign-prefix message)
(define-ethereum-api eth sign
  Data <- Address Data)
(def (eth-sign-prefix message)
  (format "\x19;Ethereum Signed Message:\n~a~a" (string-length message) message))

;; This is the thing specified (and used?) by Geth:
(define-ethereum-api eth signTransaction
  Bytes <- TransactionParameters)
;; However, parity's OpenEthereum documents this richer return type:
;;(define-ethereum-api eth signTransaction SignTransactionResult <- TransactionParameters)
;; And it's not supported by Mantis.

(define-type BlockInformation
  (Record number: [(Maybe Quantity)]
          hash: [(or Digest (void))]
          parentHash: [Digest]
          nonce: [(Maybe Bytes8)]
          sha3Uncles: [Digest]
          logsBloom: [Bloom]
          transactionsRoot: [Digest]
          stateRoot: [Digest]
          receiptsRoot: [Digest]
          miner: [Address]
          difficulty: [Quantity]
          totalDifficulty: [Quantity]
          extraData: [Bytes]
          size: [Quantity]
          gasLimit: [Quantity]
          gasUsed: [Quantity]
          timestamp: [Quantity] ;; unix timestamp
          transactions: [(Or (List TransactionInformation) (List Digest))]
          gasUsed: [Quantity]
          uncles: [(List Digest)]))

;; boolean: true for full tx objects, false for txhashes only
(define-ethereum-api eth getBlockByHash
  (Maybe BlockInformation) <- Digest Bool)
(define-ethereum-api eth getBlockByNumber
  (Maybe BlockInformation) <- BlockParameter Bool)

(define-ethereum-api eth blockNumber
  Quantity <-)

(define-type newFilterOptions ;; for newFilter
  (Record fromBlock: [BlockParameter optional: #t default: 'latest]
          toBlock: [BlockParameter optional: #t default: 'latest]
          address: [(Or Address (List Address) Unit) optional: #t default: (void)]
          topics: [(Maybe (List (Maybe (Or Bytes32 (List Bytes32))))) optional: #t default: (void)]))
(define-type getLogsFilterOptions ;; for getLogs
  (Record fromBlock: [(Maybe BlockParameter) optional: #t default: 'latest]
          toBlock: [(Maybe BlockParameter) optional: #t default: 'latest]
          address: [(Or Address (List Address) Unit) optional: #t default: (void)]
          topics: [(Maybe (List (Or Bytes32 Unit (List Bytes32)))) optional: #t default: (void)]
          blockhash: [(Maybe Digest) optional: #t default: (void)]))

(define-ethereum-api eth newFilter
  Quantity <- newFilterOptions)
(define-ethereum-api eth newBlockFilter
  Quantity <-)
(define-ethereum-api eth newPendingTransactionFilter
  Quantity <-)
(define-ethereum-api eth uninstallFilter
  Bool <- Quantity)
(define-ethereum-api eth getFilterChanges
  (Or (List Digest) ;; for newBlockFilter (block hashes), newPendingTransactionFilter (tx hashes)
      LogObjectList) ;; for newFilter
  <- Quantity)
(define-ethereum-api eth getFilterLogs
  (Or (List Digest) ;; for newBlockFilter (block hashes), newPendingTransactionFilter (tx hashes)
      LogObjectList) ;; for newFilter
  <- Quantity)
;; TODO: Check that it is coherent
;; Get a list of matchings blocks
(define-ethereum-api eth getLogs
  LogObjectList <- getLogsFilterOptions)

;; returns: 1. current block header pow-hash, 2. seed hash used for the DAG,
;; 3. boundary condition (“target”), 2^256 / difficulty.
(define-ethereum-api eth getWork
  (Tuple Bytes32 Bytes32 Bytes32) <-)
(define-ethereum-api eth submitWork
  Bool <- Bytes32 Bytes32 Bytes32)

(define-ethereum-api shh version
  String <-)
(define-type ShhMessageSent
  (Record
   from: [(Maybe Bytes60)]
   to: [(Maybe Bytes60)]
   topics: [(List Bytes)]
   payload: [Bytes]
   priority: [Quantity]
   ttl: [Quantity])) ;; time to live in seconds.
(define-ethereum-api shh post
  Bool <- ShhMessageSent)
(define-ethereum-api shh newIdentity
  Bytes60 <-)
(define-ethereum-api shh hasIdentity
  Bool <- Bytes60)
(define-ethereum-api shh newGroup
  Bytes60 <-)
(define-ethereum-api shh addToGroup
  Bool <- Bytes60)
(define-type ShhFilter
  (Record
   to: [(Maybe Bytes60)]
   topics: [(List (Or Bytes Unit (List Bytes)))]))
(define-ethereum-api shh newFilter
  Quantity <- ShhFilter)
(define-ethereum-api shh uninstallFilter
  Bool <- Quantity)
(define-type ShhMessageReceived
  (Record
   hash: [Digest]
   from: [(Maybe Bytes60)]
   to: [(Maybe Bytes60)]
   expiry: [Quantity] ;; Integer of the time in seconds when this message should expire (?).
   ttl: [Quantity] ;; Integer of the time the message should float in the system in seconds (?).
   sent: [Quantity] ;; Integer of the unix timestamp when the message was sent.
   topics: [(List Bytes)] ;; Array of DATA topics the message contained.
   payload: [Bytes] ;; The payload of the message.
   workProved: [Quantity])) ;; Integer of the work this message required before it was send (?).
(define-ethereum-api shh getFilterChanges
  (List ShhMessageReceived) <- Quantity)
(define-ethereum-api shh getMessages
  (List ShhMessageReceived) <- Quantity)

;;;; Geth extensions, Personal Namespace https://geth.ethereum.org/docs/rpc/ns-personal
;; Not present in Mantis. Is it present in Parity, though I haven't looked for discrepancies.

;; Arguments: (1) SecretKey as hex string, no 0x prefix, (2) passphrase.
(define-ethereum-api personal importRawKey
  Address <- String String)

(define-ethereum-api personal listAccounts
  (List Address) <-)

(define-ethereum-api personal lockAccount
  Bool <- Address) ;; returns true if account found (?)

(define-ethereum-api personal newAccount
  Address <- String) ;; argument is passphrase

(define-ethereum-api personal unlockAccount
  Bool <- ;; returns true if found?
  Address
  String ;; passphrase
  (Maybe JsInt)) ;; duration in seconds (default 300)

(define-ethereum-api personal sendTransaction
  Digest <- TransactionParameters String) ;; passphrase

;;; TODO: translate the 0x Bytes into a Signature.
;;; If so, some translation is required.
;;; The sign method calculates an Ethereum specific signature of:
;;; (keccak256<-bytes (ethereum-sign-message-wrapper message))
(define-ethereum-api personal sign
  Bytes <- String Address String) ;; message address passphrase
;;; Looking at the code in go-ethereum, the length that matters is the length in bytes.
;;; However, the JSON RPC API passes the string as JSON, which will be UTF-8 encoded,
;;; so it might be "interesting" to try to sign arbitrary bytes that are not valid JSON string.
(def ethereum-sign-message-prefix
  (bytes-append #u8(19) (string->bytes "Ethereum Signed Message:")))
(def (ethereum-sign-message-wrapper/bytes message)
  (call-with-output-u8vector
   (lambda (p)
     (write-bytes ethereum-sign-message-prefix p)
     (write-bytes (object->string (bytes-length message)))
     (write-bytes message p)
     (write-byte 10 p))))
(def ethereum-sign-message-wrapper
  (compose ethereum-sign-message-wrapper/bytes string->bytes))

;;; Recover the signer of a message signed with personal_sign
(define-ethereum-api personal ecRecover
  Address <- String Signature) ;; message signature

;; https://github.com/ethereum/go-ethereum/pull/15971/files
(define-ethereum-api personal signTransaction
  SignedTransaction <- TransactionParameters String)

;; txpool namespace https://geth.ethereum.org/docs/rpc/ns-txpool
(define-type TxPoolEntry
  (Record
   blockHash: [Digest]
   blockNumber: [(Maybe Quantity)]
   from: [Address]
   gas: [Quantity]
   gasPrice: [Quantity]
   hash: [Digest]
   input: [Bytes]
   nonce: [Quantity]
   to: [Address]
   transactionIndex: [(Maybe Quantity)]
   value: [Quantity]))

#;
(define-type TxPoolContent
  (Record
   pending: [(Hash Address -> (Hash Decimal -> TxPoolEntry))] ; Decimal is a Nonce in decimal
   queued: [(Hash Address -> (Hash Decimal -> TxPoolEntry))]))

;; https://github.com/ethereum/go-ethereum/wiki/Management-APIs#txpool-content
#;(define-ethereum-api txpool content TxPoolContent <-)

;; https://geth.ethereum.org/docs/rpc/pubsub -- we need use websocket for that.
;; eth_subscribe, eth_unsubscribe

;; Poll the ethereum node until it's ready
(def (poll-for-ethereum-node
      url
      message: (message ".")
      retry-window: (retry-window 0.05)
      max-window: (max-window 1.0)
      max-retries: (max-retries 10))
  (retry retry-window: retry-window max-window: max-window max-retries: max-retries
         (lambda () (display message) (eth_blockNumber url: url timeout: 1.0))))

(defstruct token (start word end))
 
;; Extract variable between `${`and `}`. 
;; List[token] <- String
(def (char-scanner str)
  (def start -1)
  (def found #f)
  (def accumulate #f)
  (def char-list [])
  (def token-list [])
  (for ((i (in-iota (string-length str))))
      (let (char (string-ref str i))
          (cond
              ((char=? #\$ char) (set! found #t))
              ((and (char=? #\{ char) found) (begin
                                                  (set! start (1- i))
                                                  (set! found #f) 
                                                  (set! accumulate #t)))
              ((char=? #\} char) (begin 
                                      (set! accumulate #f)
                                      (set! token-list (append token-list [(make-token start (apply string char-list) i)]))
                                      (set! start -1)
                                      (set! char-list [])))                         
              (else (if accumulate 
                          (set! char-list (append char-list [char]))
                          (begin 
                              (set! found #f)
                              (set! char-list [])
                              (set! start -1))))))) 
  token-list)

;; url[String] <- url[String] key[String]
;; Perform string interpolation
;; Example
;; (url-with-api-key "https://mainnet.infura.io/v3/${INFURA_API_KEY}" key: "90445523551235")
(def (url-with-api-key url key: (key #f))
  (cond 
    ((string-contains url "${INFURA_API_KEY}")     
      (cond 
        ((not key) (error "Missing API key" url))
        (else (pregexp-replace "\\$\\{INFURA_API_KEY\\}" url key))))
    (else url)))

;; url[String] <- config[EthereumNetworkConfig] filename[String]?
;; File(json) name `url_substitutions.json`
;; File contain `{"END_NODE_API_KEY": "0123456789abcdef0123456789abcdef"}` . Remember to use a valid key
;; File is expected to be already created and inside ` ~/.config/glow/` folder
;; Example
;; (ethereum-url<-config config infura-api-file: "url_substitutions.json")
;; For more information on `config` argument visit network-config.ss
(def (ethereum-url<-config config api-key-file: (api-key-file #f))
  (let (url (car (.@ config rpc)))
    (if (or api-key-file (getenv "END_NODE_API_KEY" #f))
      (url-with-api-key url key: (or (getenv "END_NODE_API_KEY" #f) (hash-get (api-key-map<-file api-key-file) 'END_NODE_API_KEY)))
      url)))

(def (current-ethereum-connection-for? name)
  (match (current-ethereum-network)
    ((ethereum-network (? object? config) (? object? connection))
     (equal? name (.@ config name)))
    (_ #f)))

(def (ensure-ethereum-connection name poll: (poll #t))
  (unless (current-ethereum-connection-for? name)
    (init-ethereum-connection name poll: poll)))

(def (init-ethereum-connection name poll: poll)
  (def network (ensure-ethereum-network name))
  (def config (ethereum-network-config network))
  (def url (ethereum-url<-config config api-key-file: api-key-file))
  (when poll
    (poll-for-ethereum-node
     url message: (format "Connecting to the ~a at ~a ...\n" (.@ config name) url)))
  (def client-version (web3_clientVersion url: url))
  (def mantis? (string-prefix? "mantis/" client-version))
  (def network-id (.@ config networkId))
  (def server-network-id
    (with-catch (lambda _ (eth-log "The node doesn't support net_version. Assuming 0.") 0)
                (cut <-string JsInt (net_version url: url))))
  (eth-log (if (equal? server-network-id network-id)
             "The server and configuration agree on networkId. Good."
             "The server and configuration disagree on networkId. BAD. Trusting configuration."))
  (def chain-id (.@ config chainId))
  (def server-chain-id
    (with-catch (lambda _ (eth-log "The node doesn't support eth_chainId. Assuming 0 (no EIP-155).") 0)
                (cut validate JsInt (eth_chainId url: url))))
  (eth-log (if (equal? server-chain-id chain-id)
             "The server and configuration agree on chainId. Good."
             "The server and configuration disagree on chainId. BAD. Trusting configuration."))
  (def connection {url client-version network-id server-network-id chain-id server-chain-id mantis?})
  (eth-log ["EthereumNetworkConnection" (list->hash-table (.alist connection))])
  (set! (ethereum-network-connection network) connection))
