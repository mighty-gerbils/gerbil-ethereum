;; Somewhat higher-level wrappers around the basic functionality in ./json-rpc
(export #t)

(import
  :gerbil/gambit/bits :gerbil/gambit/bytes
  :std/error :std/iter :std/misc/list :std/misc/number :std/sugar :std/text/hex
  :clan/assert :clan/failure :clan/number :clan/option :clan/with-id
  :clan/net/json-rpc
  :clan/poo/object :clan/poo/io :clan/poo/brace
  :clan/crypto/keccak :clan/crypto/secp256k1
  :clan/persist/db
  ./hex ./types ./rlp ./ethereum ./known-addresses
  ./logger ./network-config ./json-rpc ./nonce-tracker)

;; Signing transactions, based on spec in EIP-155
;; https://github.com/ethereum/EIPs/blob/master/EIPS/eip-155.md
;; For Ethereum main net,
;; for block >= FORK_BLKNUM (2675000), use CHAIN_ID: 1 (ETH) or 61 (ETC),
;; otherwise use 0.
;; Also Ropsten 3, Rinkeby 4, Goerli 5, Kovan 42, default Geth private chain 1337.
;; Find more chain ID's:
;; https://chainid.network
;; https://github.com/ethereum-lists/chains
;; https://github.com/ethereum/go-ethereum/blob/master/cmd/utils/flags.go

;; This function encodes a transaction into a sequence of bytes fit for signing and/or messaging
;; When signing, v should be the chainid, and r and s 0.
;; When messaging, v, r, s are from the secp256k1 signature, v amended as per eip155.
;; : Bytes <- Quantity Quantity Quantity Address Quantity Bytes Quantity Quantity Quantity
(def (signed-tx-bytes<- nonce gasPrice gas to value data v r s)
  (rlpbytes<-rlp
   [(rlp<-nat nonce) (rlp<-nat gasPrice) (rlp<-nat gas)
    (if (address? to) (bytes<- Address to) #u8()) (rlp<-nat value) data
    (when/list (not (zero? v)) [(rlp<-nat v) (rlp<-nat r) (rlp<-nat s)]) ...]))

;; : Quantity <- Quantity
(def (chainid<-v v)
  (cond
   ((<= 27 v 28) 0)
   ((<= 37 v) (arithmetic-shift (- v 35) -1))
   (else (error "invalid v" v))))

;; : (OrFalse SignedTransactionInfo) <- Bytes
(def (decode-signed-tx-bytes bytes)
  (with-catch false
    (lambda ()
      (def-slots (nonce gasPrice gas to value data v r s) (<-rlpbytes SignedTransactionData bytes))
      (def y-parity+27 (- 28 (bitwise-and v 1)))
      (def chainid (chainid<-v v))
      (def signature (signature<-vrs y-parity+27 r s))
      (def signed-tx-bytes (signed-tx-bytes<- nonce gasPrice gas to value data chainid 0 0))
      (def hash (keccak256<-bytes bytes))
      (def from (recover-signer-address signature hash))
      (and from {from nonce gasPrice gas to value data v r s hash}))))

;; This function computes the v value to put in the signed transaction data,
;; based on the v returned by the secp256k1 primitive (which is y-element parity+27)
;; and the chainid and whether the eip155 is activated. NB: 8+27=35
;; : Quantity <- UInt8 Quantity
(def (eip155-v yparity+27 chainid)
  (if (zero? chainid)
    yparity+27
    (+ 8 yparity+27 (* 2 chainid))))

;; : (Values Quantity Quantity Quantity) <- \
;;     SecretKey Quantity Quantity Quantity Address Quantity Bytes Quantity
(def (vrs<-tx secret-key nonce gasPrice gas to value data chainid)
  (def bytes (signed-tx-bytes<- nonce gasPrice gas to value data chainid 0 0))
  (def signature (make-message-signature secret-key (keccak256<-bytes bytes)))
  (defvalues (v r s) (vrs<-signature signature))
  (values (eip155-v v chainid) r s))

(defstruct (TransactionRejected exception) (receipt)) ;; (Or TransactionReceipt String)
(defstruct (StillPending exception) ())
(defstruct (ReplacementTransactionUnderpriced exception) ())
(defstruct (IntrinsicGasTooLow exception) ())
(defstruct (NonceTooLow exception) ())

;; TODO: Send Notification to end-user via UI!
;; : Bottom <- Address
(def (nonce-too-low address)
  (reset-nonce address)
  (raise (NonceTooLow)))

;; : Unit <- Address timeout:?(OrFalse Real) log:?(Fun Unit <- Json)
(def (ensure-eth-signing-key timeout: (timeout #f) log: (log #f) address password)
  (when log (log ['ensure-eth-signing-key (0x<-address address)]))
  (def keypair (keypair<-address address))
  (unless keypair
    (error "No registered keypair for address" 'ensure-eth-signing-key address))
  (try
   (personal_importRawKey (hex-encode (export-secret-key/bytes (keypair-secret-key keypair))) password
                          timeout: timeout log: log)
   (catch (json-rpc-error? e)
     (unless (equal? (json-rpc-error-message e) "account already exists")
       (raise e)))))

;; : Bool <- TransactionReceipt
(def (successful-receipt? receipt)
  (and (object? receipt)
       (if (ethereum-mantis?)
         (let (code (.@ receipt statusCode)) (or (equal? code 0) (equal? code (void)))) ;; success on Mantis -- void seems to be a bug
         (equal? (.@ receipt status) 1)))) ;; success on Geth

;; Given some Transaction data and Digest of the online transaction,
;; as well as a minimum number of confirmations wanted (in blocks), return
;; a TransactionReceipt for the transaction *if and only if* the Transaction
;; was indeed included on the blockchain with sufficient confirmations.
;; Otherwise, raise an appropriate exception that details the situation.
;; : TransactionReceipt <- SignedTransactionInfo confirmations:(OrFalse Nat) nonce-too-low?:Bool
(def (confirmed-receipt<-transaction
      tx
      confirmations: (confirmations (ethereum-confirmations-wanted-in-blocks))
      nonce-too-low?: (nonce-too-low? #f))
  (def-prefixed-slots (t- from to gas hash) tx)
  (def receipt (eth_getTransactionReceipt t-hash))
  (cond
   ((successful-receipt? receipt)
    (let ()
      (def-prefixed-slots (r- from to transactionHash gasUsed) receipt)
      (unless (and (or (ethereum-mantis?) ;; Mantis doesn't carry these fields in receipt
                       (and (equal? t-from r-from)
                            (equal? t-to r-to)))
                   (equal? t-hash r-transactionHash)
                   (>= t-gas r-gasUsed))
        (error "receipt doesn't match transaction sent"))
      (when (and confirmations (> confirmations (confirmations<-receipt receipt)))
        (raise (StillPending)))
      receipt))
   ((object? receipt)
    (raise (TransactionRejected receipt)))
   (else
    (with-slots (from nonce) tx
      (def sender-nonce (eth_getTransactionCount from 'latest))
      (cond
       ((>= sender-nonce nonce)
        (if nonce-too-low? (nonce-too-low from) (raise (StillPending))))
       ((< sender-nonce nonce)
        (error (TransactionRejected "BEWARE: nonce too high. Are you queueing transactions? Did you reset a test network?"))))))))

;; Count the number of confirmations for a transaction given by its hash.
;; Return -1 if the transaction is (yet) unconfirmed, -2 if it is failed.
;; : Integer <- TransactionReceipt
(def (confirmations<-receipt receipt (block-number (eth_blockNumber)))
  (cond
   ((successful-receipt? receipt)
    (- block-number (.@ receipt blockNumber)))
   ((object? receipt)
    -2)
   (else -1)))

;; Given a putative sender, some transaction data, and a confirmation,
;; make sure that it all matches.
;; TODO: Make sure we can verify the confirmation from the Ethereum contract,
;; by checking the merkle tree and using e.g. Andrew Miller's contract to access old
;; block hashes https://github.com/amiller/ethereum-blockhashes
;; : Unit <- sender: Address recipient: (or Address Unit) TransactionInfo Confirmation
(def (check-transaction-confirmation
      tx confirmation
      confirmations: (confirmations (ethereum-confirmations-wanted-in-blocks)))
  (def-prefixed-slots (c- transactionHash transactionIndex blockNumber blockHash) confirmation)
  (unless (equal? (.@ tx hash) c-transactionHash)
    (error "Malformed request" "Transaction data digest does not match the provided confirmation"))
  (def receipt (confirmed-receipt<-transaction tx confirmations: confirmations))
  (def-prefixed-slots (r- transactionHash transactionIndex blockNumber blockHash) receipt)
  (unless (and (equal? c-transactionHash r-transactionHash)
               (equal? c-transactionIndex r-transactionIndex)
               (equal? c-blockNumber r-blockNumber)
               (equal? c-blockHash r-blockHash))
    (error "confirmation doesn't match receipt information")))

;; Prepare a signed transaction, that you may later issue onto Ethereum network,
;; from a given pre-transaction.
;; : SignedTransactionInfo <- PreTransaction ?Nat
(def (sign-transaction tx (chainid (ethereum-chain-id)))
  (def-slots (from nonce gasPrice gas to value data) (complete-transaction tx))
  (def keypair (or (keypair<-address from)
                   (error "Couldn't find registered keypair" (json<- Address from))))
  (defvalues (v r s) (vrs<-tx (keypair-secret-key keypair)
                              nonce gasPrice gas to value data chainid))
  (def raw (signed-tx-bytes<- nonce gasPrice gas to value data v r s))
  (def hash (keccak256<-bytes raw))
  {from nonce gasPrice gas to value data v r s hash})

;; : Bytes <- Transaction Quantity
(def (bytes<-signed-tx tx)
  (def-slots (nonce gasPrice gas to value data v r s) tx)
  (signed-tx-bytes<- nonce gasPrice gas to value data v r s))

(def (complete-tx-field tx name valid? mandatory? (default void))
  (def v (with-catch void (cut .ref tx name)))
  (cond
   ((valid? v) v)
   ((or mandatory? (not (void? v)))
    (error "Missing or invalid transaction field" name tx))
   (else (default))))
(defrule (def-field name tx args ...)
  (with-id def-field ((complete 'complete-tx- #'name))
    (def name (complete tx args ...))))

(def (complete-tx-from tx)
  (complete-tx-field tx 'from address? #t))
(def (complete-tx-to tx)
  (complete-tx-field tx 'to address? #f))
(def (complete-tx-data tx)
  (complete-tx-field tx 'data bytes? #f (lambda () #u8())))
(def (complete-tx-value tx)
  (complete-tx-field tx 'value nat? #f (lambda () 0)))
(def (complete-tx-nonce tx from) ;; NB: beware concurrency/queueing in transactions from the same person.
  (complete-tx-field tx 'nonce nat? #f (cut eth_getTransactionCount from 'latest)))
(def (complete-tx-gas tx from to data value)
  (complete-tx-field tx 'gas nat? #f (cut gas-estimate from to data value)))
(def (complete-tx-gasPrice tx gas)
  (complete-tx-field tx 'gasPrice nat? #f (cut gas-price-estimate gas)))

(def (complete-transaction tx)
  (def-field from tx)
  (def-field to tx)
  (def-field data tx)
  (def-field value tx)
  (def-field nonce tx from)
  (def-field gas tx from to data value)
  (def-field gasPrice tx gas)
  {from to data value gas nonce gasPrice})

;; : Transaction <- PreTransaction
(def (complete-pre-transaction tx)
  (def-field from tx)
  (def-field to tx)
  (def-field data tx)
  (def-field value tx)
  (def-field gas tx from to data value)
  (def nonce (with-catch void (cut .ref tx 'nonce)))
  (def gasPrice (with-catch void (cut .ref tx 'gasPrice)))
  {from to data value gas nonce gasPrice})

;; : TransactionReceipt <- SignedTransactionInfo
(def (send-signed-transaction tx)
  (def-slots (hash) tx)
  (eth-log ["send-signed-transaction" (json<- SignedTransactionInfo tx)])
  (match (with-result (eth_sendRawTransaction (bytes<-signed-tx tx)))
    ((some transaction-hash)
     (unless (equal? transaction-hash hash)
       (error "eth-send-raw-transaction: invalid hash" transaction-hash hash))
     (confirmed-receipt<-transaction tx confirmations: #f))
    ((failure (json-rpc-error code: -32000 message: "nonce too low"))
     (confirmed-receipt<-transaction tx confirmations: #f nonce-too-low?: #t))
    ((failure (json-rpc-error code: -32000 message: "replacement transaction underpriced"))
     (raise (ReplacementTransactionUnderpriced)))
    ((failure (json-rpc-error code: -32000 message: "intrinsic gas too low"))
     (raise (IntrinsicGasTooLow)))
    ((failure (json-rpc-error code: -32000 message:
                              (? (let (m (string-append "known transaction: " (hex-encode hash)))
                                   (cut equal? <> m)))))
     (confirmed-receipt<-transaction tx confirmations: #f))
    ((failure e)
     (raise e))))

;; Gas used for a transfer transaction. Hardcoded value defined in the Yellowpaper.
;; : Quantity
(def transfer-gas-used 21000)

(def (bytes-count-zeroes bytes)
  (def c 0)
  (for (i (in-iota (bytes-length bytes)))
    (when (zero? (bytes-ref bytes i))
      (increment! c)))
  c)

(def Gtxdatazero 4) ;; name used in evm.md
(def Gtxdatanonzero 16) ;; NB: used to be 68 before EIP-2028

;; Quantity <- Bytes
(def (intrinsic-gas<-bytes
      data base: (base transfer-gas-used) zero: (zero Gtxdatazero) nonzero: (nonzero Gtxdatanonzero))
  (def cz (bytes-count-zeroes data))
  (def cnz (- (bytes-length data) cz))
  (+ (* zero cz) (* nonzero cnz) base))

;; Inputs must be normalized
;; : Quantity <- Address (Maybe Address) Bytes Quantity
(def (gas-estimate from to data value)
  (if (and (address? to) (equal? data #u8()))
    transfer-gas-used
    (let ((intrinsic-gas (intrinsic-gas<-bytes data))
          (estimate (eth_estimateGas {from to data value})))
      (when (<= estimate intrinsic-gas)
        ;; Mantis is sometimes deeply confused
        (eth-log ["node returned bogus gas estimate" estimate
                  "intrinsic-gas" intrinsic-gas
                  "from" (json<- Address from) "to" (json<- (Maybe Address) to)
                  "data" (json<- (Maybe Bytes) data) "value" (json<- Quantity value)])
        (set! estimate (max intrinsic-gas 2000000))) ;; arbitrary number, hopefully large enough.
      ;; Sometimes the geth estimate is not enough, so we arbitrarily double it.
      ;; TODO: improve on this doubling
      (* 2 estimate))))

;; TODO: in the future, take into account the market (especially in case of block-buying attack)
;; and how much gas this is for to compute an estimate of the gas price.
(def minimum-gas-price (values 1)) ;; NB: set it 0 for no effect

(def (gas-price-estimate gas)
  (def node-price (eth_gasPrice))
  (max node-price minimum-gas-price))

;; : Transaction <- Address Quantity
(def (transfer-tokens from: from to: to value: value
        gasPrice: (gasPrice (void)) nonce: (nonce (void)))
  {from value to data: (void) gas: transfer-gas-used gasPrice nonce})

;; : Transaction <- Address Bytes value: ?Quantity gas: ?(Maybe Quantity) gasPrice: ?(Maybe Quantity) nonce: ?(Maybe Quantity)
(def (create-contract creator initcode
      value: (value 0) gas: (gas (void)) gasPrice: (gasPrice (void)) nonce: (nonce (void)))
  (complete-pre-transaction {from: creator to: (void) data: initcode value gas gasPrice nonce}))

;; : Transaction <- Address Address Bytes value: ?Quantity gas: ?(Maybe Quantity) gasPrice: ?(Maybe Quantity) nonce: ?(Maybe Quantity)
(def (call-function caller contract calldata
       value: (value 0) gas: (gas (void)) gasPrice: (gasPrice (void)) nonce: (nonce (void)))
  (complete-pre-transaction {from: caller to: contract data: calldata value gas gasPrice nonce}))
