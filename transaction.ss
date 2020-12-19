;; Somewhat higher-level wrappers around the basic functionality in ./json-rpc
(export #t)

(import
  :gerbil/gambit/bits :gerbil/gambit/bytes
  :std/error :std/misc/list :std/sugar :std/text/hex
  :clan/assert :clan/failure :clan/number :clan/option :clan/with-id
  :clan/net/json-rpc
  :clan/poo/poo :clan/poo/io :clan/poo/brace
  :clan/crypto/keccak :clan/persist/db
  ./hex ./types ./rlp ./ethereum ./signing ./known-addresses
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
      (def from (recover-signer-address signature (keccak256<-bytes signed-tx-bytes)))
      (and from
           {from nonce gasPrice gas to value data v r s}))))

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

;; : Bytes <- Transaction Quantity
(def (raw-sign-transaction transaction (chainid (ethereum-chain-id)))
  (def-slots (from nonce gasPrice gas to value data) transaction)
  (when (void? value) (set! value 0))
  (when (void? data) (set! data #u8()))
  (def keypair (or (keypair<-address from)
                   (error "Couldn't find registered keypair" (json<- Address from))))
  (defvalues (v r s) (vrs<-tx (keypair-secret-key keypair)
                              nonce gasPrice gas to value data chainid))
  (signed-tx-bytes<- nonce gasPrice gas to value data v r s))

(defstruct (TransactionRejected exception) (receipt)) ;; (Or TransactionReceipt String Unit)
(defstruct (StillPending exception) ())
(defstruct (ReplacementTransactionUnderpriced exception) ())
(defstruct (IntrinsicGasTooLow exception) ())
(defstruct (NonceTooLow exception) ())

;; TODO: Send Notification to end-user via UI!
;; : Bottom <- Address
(def (nonce-too-low address)
  (.call NonceTracker reset address)
  (raise (NonceTooLow)))

;; : Unit <- Address timeout:?(OrFalse Real) log:?(Fun Unit <- Json)
(def (ensure-eth-signing-key timeout: (timeout #f) log: (log #f) address)
  (when log (log ['ensure-eth-signing-key (0x<-address address)]))
  (def keypair (keypair<-address address))
  (unless keypair
    (error "No registered keypair for address" 'ensure-eth-signing-key address))
  (try
   (personal_importRawKey (hex-encode (export-secret-key/bytes (keypair-secret-key keypair)))
                          (export-password/string (keypair-password keypair))
                          timeout: timeout log: log)
   (catch (json-rpc-error? e)
     (unless (equal? (json-rpc-error-message e) "account already exists")
       (raise e)))))

;; NB: geth will refuse to unlock via http (vs https (?)), anyway
;; : Unit <- Address duration:?Real log:?(Fun Unit <- Json)
(def (unlock-account address duration: (duration 5) log: (log #f))
  (when log (log ['unlock-account (0x<-address address) duration]))
  (def keypair (keypair<-address address))
  (personal_unlockAccount address (export-password/string (keypair-password keypair)) duration))

;; : Bool <- TransactionReceipt
(def (successful-receipt? receipt)
  (and (poo? receipt)
       (if (ethereum-mantis?)
         (member (.@ receipt statusCode) [(void) 0]) ;; success on Mantis -- void seems to be a bug
         (equal? (.@ receipt status) 1)))) ;; success on Geth

;; Given some Transaction data and Digest of the online transaction,
;; as well as a minimum number of confirmations wanted (in blocks), return
;; a TransactionReceipt for the transaction *if and only if* the Transaction
;; was indeed included on the blockchain with sufficient confirmations.
;; Otherwise, raise an appropriate exception that details the situation.
;; : TransactionReceipt <- Transaction Digest confirmations:(OrFalse Nat) nonce-too-low?:Bool
(def (confirmed-receipt<-transaction
      tx hash
      confirmations: (confirmations (ethereum-confirmations-wanted-in-blocks))
      nonce-too-low?: (nonce-too-low? #f))
  (def receipt (eth_getTransactionReceipt hash))
  (cond
   ((successful-receipt? receipt)
    (let ()
      (def-prefixed-slots (r- from to transactionHash gasUsed) receipt)
      (def-prefixed-slots (t- from to hash gas) tx)
      (unless (and (or (ethereum-mantis?) ;; Mantis doesn't carry these fields in receipt
                       (and (equal? t-from r-from)
                            (equal? t-to r-to)))
                   (equal? t-hash r-transactionHash)
                   (>= t-gas r-gasUsed))
        (error "receipt doesn't match transaction sent"))
      (when (and confirmations (> confirmations (confirmations<-receipt receipt)))
        (raise (StillPending)))
      receipt))
   ((poo? receipt)
    (raise (TransactionRejected receipt)))
   (else
    (with-slots (from nonce) tx
      (def sender-nonce (eth_getTransactionCount from 'latest))
      (cond
       ((> sender-nonce nonce) (if nonce-too-low? (nonce-too-low from) (raise (StillPending))))
       ((= sender-nonce nonce) (error (TransactionRejected "Reason unknown (nonce didn't change)")))
       ((< sender-nonce nonce) (error (TransactionRejected "BEWARE: nonce too high. Are you queueing transactions? Did you reset a test network?"))))))))

;; Count the number of confirmations for a transaction given by its hash.
;; Return -1 if the transaction is (yet) unconfirmed, -2 if it is failed.
;; : Integer <- TransactionReceipt
(def (confirmations<-receipt receipt (block-number (eth_blockNumber)))
  (cond
   ((successful-receipt? receipt)
    (- block-number (.@ receipt blockNumber)))
   ((poo? receipt)
    -2)
   (else -1)))

;; Given a putative sender, some transaction data, and a confirmation,
;; make sure that it all matches.
;; TODO: Make sure we can verify the confirmation from the Ethereum contract,
;; by checking the merkle tree and using e.g. Andrew Miller's contract to access old
;; block hashes https://github.com/amiller/ethereum-blockhashes
;; : Unit <- sender: Address recipient: (or Address Unit) Transaction Confirmation
(def (check-transaction-confirmation
      tx tx-hash confirmation
      confirmations: (confirmations (ethereum-confirmations-wanted-in-blocks)))
  (def-prefixed-slots (c- transactionHash transactionIndex blockNumber blockHash) confirmation)
  (unless (equal? tx-hash c-transactionHash)
    (error "Malformed request" "Transaction data digest does not match the provided confirmation"))
  (def receipt (confirmed-receipt<-transaction tx tx-hash confirmations: confirmations))
  (def-prefixed-slots (r- transactionHash transactionIndex blockNumber blockHash) receipt)
  (unless (and (equal? c-transactionHash r-transactionHash)
               (equal? c-transactionIndex r-transactionIndex)
               (equal? c-blockNumber r-blockNumber)
               (equal? c-blockHash r-blockHash))
    (error "confirmation doesn't match receipt information")))

;; Previous implementation, that signs through geth
;; : SignedTransaction <- Transaction
#;
(def (sign-transaction transaction)
  (def sender (.@ transaction from))
  (def kp (keypair<-address sender))
  (unless kp (error "Couldn't find registered keypair" (json<- Address sender)))
  (personal_signTransaction (TransactionParameters<-PreTransaction transaction)
                            (export-password/string (keypair-password kp))))

;; New implementation with the same interface as the previous one above.
;; : SignedTransaction <- Transaction ?Nat
(def (sign-transaction transaction (chainid (ethereum-chain-id)))
  (def raw (raw-sign-transaction transaction chainid))
  (def-slots (nonce gasPrice gas to value data v r s) (<-rlpbytes SignedTransactionData raw))
  (def tx {nonce gasPrice gas to value input: data v r s hash: (keccak256<-bytes raw)})
  {raw tx})

(def minimum-gas-price (values 1))

;; Prepare a signed transaction, that you may later issue onto Ethereum network,
;; from given address, with given operation, value and gas-limit
;; : SignedTransaction <- PreTransaction
(def (make-signed-transaction tx)
  (sign-transaction (complete-transaction tx)))

(def (complete-tx-field tx name valid? mandatory? (default void))
  (def v (.ref tx name void))
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
(def (complete-tx-nonce tx from)
  (complete-tx-field tx 'nonce nat? #f (cut .call NonceTracker next from)))
(def (complete-tx-gas tx from to data value)
  (complete-tx-field tx 'gas nat? #f (cut gas-estimate from to data value)))
(def (complete-tx-gasPrice tx)
  (complete-tx-field tx 'gasPrice nat? #f (cut max minimum-gas-price (eth_gasPrice))))

(def (complete-transaction tx)
  (def-field from tx)
  (def-field to tx)
  (def-field data tx)
  (def-field value tx)
  (def-field nonce tx from)
  (def-field gas tx from to data value)
  (def-field gasPrice tx)
  {from to data value gas nonce gasPrice})

;; : Transaction <- PreTransaction
(def (complete-pre-transaction tx)
  (def-field from tx)
  (def-field to tx)
  (def-field data tx)
  (def-field value tx)
  (def-field gas tx from to data value)
  (def nonce (.ref tx 'nonce))
  (def gasPrice (.ref tx 'gasPrice void))
  {from to data value gas nonce gasPrice})

;; : Digest <- Address SignedTransaction
(def (send-raw-transaction sender signed)
  (def data (.@ signed raw))
  (def tx (.cc (.@ signed tx) from: sender))
  (def hash (.@ tx hash))
  (match (with-result (eth_sendRawTransaction data))
    ((some transaction-hash)
     (unless (equal? transaction-hash hash)
       (error "eth-send-raw-transaction: invalid hash" transaction-hash hash))
     hash)
    ((failure (json-rpc-error code: -32000 message: "nonce too low"))
     (confirmed-receipt<-transaction tx hash confirmations: #f)
     hash)
    ((failure (json-rpc-error code: -32000 message: "replacement transaction underpriced"))
     (raise (ReplacementTransactionUnderpriced)))
    ((failure (json-rpc-error code: -32000 message: "intrinsic gas too low"))
     (raise (IntrinsicGasTooLow)))
    ((failure (json-rpc-error code: -32000 message:
                              (? (let (m (string-append "known transaction: " (hex-encode hash)))
                                   (cut equal? <> m)))))
     (confirmed-receipt<-transaction tx hash confirmations: #f)
     hash)
    ((failure e)
     (raise e))))

;; : TransactionReceipt <- Transaction SignedTransaction
(def (send-and-confirm-transaction sender signed)
  (def hash (send-raw-transaction sender signed))
  (def tx (.cc (.@ signed tx) from: sender))
  (assert-equal! hash (.@ tx hash))
  (confirmed-receipt<-transaction tx hash confirmations: #f))

;; Gas used for a transfer transaction. Hardcoded value defined in the Yellowpaper.
;; : Quantity
(def transfer-gas-used 21000)

;; Inputs must be normalized
;; : Quantity <- Address (Maybe Address) Bytes Quantity
(def (gas-estimate from to data value)
  (if (and (address? to) (equal? data #u8()))
    transfer-gas-used
    (let (pre-estimate (eth_estimateGas {from to data value}))
      ;; TODO: improve on this doubling
      (* 2 pre-estimate))))

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
