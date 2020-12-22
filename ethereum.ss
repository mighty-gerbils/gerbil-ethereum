(export #t)

(import
  :std/sugar
  :clan/maybe :clan/number :clan/decimal
  :clan/poo/poo :clan/poo/io :clan/poo/brace
  (only-in :clan/poo/mop sexp<-)
  (only-in :clan/poo/type Sum define-sum-constructors)
  ./types ./hex ./signing)

;; TODO: implement and use a "newtype"
(define-type Quantity UInt256)
(define-type UInt UInt256)
(define-type Digest Bytes32)
(define-type Data Bytes)
(register-simple-eth-type Address)

;; These limits are from EIP-106
(define-type GasQuantity UInt63) ;; in practice, for the next years, will fit UInt32
(define-type Block UInt63) ;; in practice, for the next century, will fit UInt32
(define-type BufferSize UInt32) ;; in practice, for the next years, will fit UInt24

;; TODO: mixin the prototype with a formula that caches the abi bytes4 selector.
(define-type EthFunction
  (.mix (Record contract: [Address] selector: [Bytes4])
        {ethabi: "function"}))

;; : Nat
(def one-ether-in-wei (expt 10 18)) ;; 1 ETH = 10^18 wei

;; : Nat
(def one-gwei-in-wei (expt 10 9)) ;; 1 gwei = 10^9 wei

;; : Nat <- Real
(def (wei<-ether ether-amount)
  (integer-part (* ether-amount one-ether-in-wei))) ;; allow floating point, round to integer

;; : Decimal <- Nat
(def (ether<-wei wei-amount)
  (/ wei-amount one-ether-in-wei))

;; : Nat <- Real
(def (wei<-gwei gwei-amount)
  (integer-part (* gwei-amount one-gwei-in-wei))) ;; allow floating point, round to integer

;; : Decimal <- Nat
(def (gwei<-wei wei-amount)
  (/ wei-amount one-gwei-in-wei))

;; : String <- Nat
(def (decimal-string-ether<-wei wei-amount)
  (string<-decimal (ether<-wei wei-amount)))

;; : String <- Nat
(def (decimal-string-gwei<-wei wei-amount)
  (string<-decimal (gwei<-wei wei-amount)))

(define-type Confirmation
  (Record
   transactionHash: [Digest]
   transactionIndex: [Quantity]
   blockNumber: [Quantity]
   blockHash: [Digest]))

;; Three kinds of operations that may be posted
(define-type Operation
  (Record
   to: [(Maybe Address) default: (void)] ;; absent or (void) is for CreateContract
   data: [(Maybe Bytes) default: (void)])) ;; absent or (void) are equivalent to #u8()
(def (TransferTokens recipient) {to: recipient data: (void)})
(def (CreateContract initialization-code) {to: (void) data: initialization-code})
(def (CallFunction contract call-data) {to: contract data: call-data})

(define-type PreTransaction
  (Record
   from: [(Maybe Address) default: (void)] ;; absent or void means not decoded yet
   to: [(Maybe Address) default: (void)] ;; as per Operation
   data: [(Maybe Bytes) default: (void)] ;; as per Operation, absent or void means #u8()
   value: [(Maybe Quantity) default: (void)] ;; in wei, absent or void means 0
   gas: [(Maybe Quantity) default: (void)] ;; in gas, absent or void means auto estimate
   gasPrice: [(Maybe Quantity) default: (void)] ;; in wei/gas, absent or void means get from the environment
   nonce: [(Maybe Quantity) default: (void)])) ;; absent or void means get from tracker

;; Transaction (to be) signed and posted to the chain Ethereum
(define-type Transaction
  (Record
   from: [Address]
   to: [(Maybe Address) optional: #t default: (void)] ; void means contract creation
   gas: [(Maybe Quantity) optional: #t default: (void)] ; in gas -- upper limit, refund of excess, void means autodetect
   gasPrice: [(Maybe Quantity) optional: #t default: (void)] ; in wei/gas, void means autodetect
   value: [(Maybe Quantity) optional: #t default: (void)] ; in wei, void means 0
   data: [(Maybe Bytes) optional: #t default: (void)] ; void means empty bytes
   nonce: [(Maybe Quantity) optional: #t default: (void)])) ; void means autodetect


(def (PreTransaction<-Transaction tx)
  (def-slots (from to data value gas) tx)
  {from to data value gas})

;; Data that gets signed in Ethereum pre EIP-155,
;; and still so in ETC (?) --- in the Cardano KEVM test net definitely.
;; The fields are in the order in which they are included in the RLP encoding that gets signed.
(define-type ShortTransactionData
  (Record
   nonce: [Quantity]
   gasPrice: [Quantity]
   gas: [Quantity]
   to: [(Maybe Address) default: (void)]
   value: [Quantity default: 0]
   data: [Bytes default: #u8()]))

;; This structure represents the information that is sent over the network, and also,
;; post EIP-155, information that gets signed, with slightly different v, r, s values.
;; Note how it does NOT contain a from: field with the sender address. Instead,
;; this address is recovered from the signature as reconstituted from the v, r, s fields.
;; The fields are in the order in which they are included in the RLP encoding
;; that gets sent and/or signed.
(define-type SignedTransactionData
  (Record
   nonce: [Quantity]
   gasPrice: [Quantity]
   gas: [Quantity]
   to: [(Maybe Address) default: (void)]
   value: [Quantity default: 0]
   data: [Bytes default: #u8()]
   v: [Quantity] ;; actually UInt8... plus offset from chainId, can actually be large.
   r: [Quantity] ;; UInt256
   s: [Quantity])) ;; UInt256

;; Same as above, but includes a "from:" field as in a Transaction.
(define-type SignedTransactionInfo
  (Record
   from: [Address]
   nonce: [Quantity]
   gasPrice: [Quantity]
   gas: [Quantity]
   to: [(Maybe Address) optional: #t default: (void)]
   value: [Quantity]
   data: [Bytes optional: #t default: #u8()]
   v: [Quantity] ;; actually UInt8... plus offset from chainId!
   r: [Quantity] ;; UInt256
   s: [Quantity])) ;; UInt256

;; TODO: implement single and/or multiple inheritance for records and have something like:
;;
;; Transaction <: PreTransaction CallParameters ;; e.g. CallParameters has no nonce but mandatory from
;; TransactionParameters <: PreTransaction
;; Signature <: VRS
;; SignedTransactionData <: ShortTransactionData VRS Transaction
;; SignedTransactionInfo <: {From} SignedTransactionData
;; SignedTx <: SignedTransactionData {Hash}
;; TransactionInformation <: SignedTransactionInfo SignedTx
;;
;; Actually, there might be additional types with a slightly more complex hierarchy
;; to properly take into account field nullability. Ouch. Or not.
