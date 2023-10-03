(export #t)

(import
  (only-in :gerbil/gambit object->serial-number)
  (only-in :std/format format)
  (only-in :std/misc/number integer-part)
  (only-in :std/misc/decimal decimal->string)
  (only-in :clan/base !> compose)
  (only-in :clan/crypto/keccak keccak256<-bytes)
  (only-in :clan/crypto/secp256k1 recover-signer-public-key make-message-signature PublicKey)
  (only-in :clan/poo/object defmethod def-slots .mix .@)
  (only-in :clan/poo/io bytes<- methods.marshal<-bytes)
  (only-in :clan/poo/brace @method @@method)
  (only-in :clan/poo/mop sexp<- define-type validate Type.)
  (only-in :clan/poo/type Sum define-sum-constructors Maybe)
  (only-in ./types Record Bytes Bytes4 Bytes20 Bytes32 UInt32 UInt63 UInt256
           register-simple-eth-type ensure-zeroes)
  (only-in ./hex address-bytes<-0x bytes<-0x 0x<-address-bytes)
  (only-in ./rlp rlpbytes<-rlp rlp<-nat))

;; Types used by Ethereum APIs
(define-type Quantity UInt256)
(define-type UInt UInt256)
(define-type Digest Bytes32)
(define-type Data Bytes)

;; These limits are from EIP-106
(define-type GasQuantity UInt63) ;; in practice, for the next years, will fit UInt32
(define-type Block UInt63) ;; in practice, for the next century, will fit UInt32
(define-type BufferSize UInt32) ;; in practice, for the next years, will fit UInt24

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
  (decimal->string (ether<-wei wei-amount)))

;; : String <- Nat
(def (decimal-string-gwei<-wei wei-amount)
  (decimal->string (gwei<-wei wei-amount)))


;;; Addresses

;; TODO: Should we define a :wr method to print the address data, or keep it opaque for PII privacy?
(defstruct address (bytes) print: #f equal: #t)

;; : 0xString <- Address
(def 0x<-address (compose 0x<-address-bytes address-bytes))
(defmethod (@@method :json address) 0x<-address)

;; : Address <- 0xString
(def address<-0x (compose make-address (.@ Bytes20 .validate) bytes<-0x))

;; Enforce EIP-55
;; : Address <- 0xString
(def address<-0x/strict (compose make-address address-bytes<-0x))

;; Null address
;; : Address
(def null-address (make-address (make-u8vector 20 0)))

(defmethod (@@method :wr address)
  (lambda (self we)
    (unless (eq? (write-style we) 'mark)
      (let ()
        (##wr-str we (format "#~d" (object->serial-number self)))
        (##wr-str we " ")
        (##wr-str we "#;")
        (##wr we `(address<-0x ,(0x<-address self)))))))

;; Address on the specified network, e.g. (address<-0x "0xb0bb1ed229f5Ed588495AC9739eD1555f5c3aabD")
(define-type Address
  {(:: @ [methods.marshal<-bytes Type.])
   .Bytes: Bytes20
   .element?: address?
   .json<-: 0x<-address
   .<-json: (compose make-address (.@ Bytes20 .<-json))
   .string<-: 0x<-address
   .<-string: address<-0x
   .sexp<-: (lambda (x) `(address<-0x ,(0x<-address x)))
   .<-bytes: (compose make-address (.@ Bytes20 .validate))
   .bytes<-: address-bytes
   .<-rlp: .<-bytes
   .rlp<-: .bytes<-
   .length-in-bytes: 20
   .ethabi-name: "address"
   .ethabi-display-type: (cut display .ethabi-name <>)
   .ethabi-head-length: 32
   .ethabi-padding: 12
   .ethabi-tail-length: (lambda (_) 0)
   ;; https://docs.soliditylang.org/en/develop/abi-spec.html
   ;; address: equivalent to uint160, except for the assumed interpretation and language typing.
   ;; The above means left-padding with 0s
   .ethabi-encode-into:
   (lambda (x bytes _start head _get-tail _set-tail!)
      (subu8vector-move! (address-bytes x) 0 20 bytes (+ head 12)))
   .ethabi-decode-from:
   (lambda (bytes _start head _get-tail _set-tail!)
      (ensure-zeroes bytes head 12)
      (make-address (subu8vector bytes (+ head 12) (+ head 32))))
   })
(register-simple-eth-type Address)

;; Internal function to compute an address from bytes
;; : Address <- Bytes
(def (address<-data data)
  (!> data
      keccak256<-bytes
      (cut subu8vector <> 12 32)
      make-address))

;; Address of a user given his public key
;; : Address <- PublicKey
(def (address<-public-key pubkey)
  (address<-data (bytes<- PublicKey pubkey)))

;; Current contract address from transaction
;; : Address <- Address UInt256
(def (address<-creator-nonce creator nonce)
  (address<-data (rlpbytes<-rlp [(bytes<- Address creator) (rlp<-nat nonce)])))

;; Address from CREATE2 given creator and nonce
;; NB: when executing from a transaction, the creator is not the CALLER,
;; but the ADDRESS computed from address<-creator-nonce with CALLER and nonce above.
;; https://eips.ethereum.org/EIPS/eip-1014
;; : Address <- Address UInt256
(def (address<-create2 creator salt init-code)
  (address<-data (u8vector-append #u8(#xff) (bytes<- Address creator)
                                  (validate Bytes32 salt)
                                  (keccak256<-bytes init-code))))

;; Signature <- 'a:Type SecKey 'a
(def (make-signature type secret-key data)
  (make-message-signature secret-key (keccak256<-bytes (bytes<- type data))))

;; (OrFalse Address) <- Signature Digest
(def (recover-signer-address signature message32)
  (let (pubkey (recover-signer-public-key signature message32))
    (and pubkey (address<-public-key pubkey))))

;; Bool <- Address Signature Digest
(def (message-signature-valid? address signature message32)
  (equal? address (recover-signer-address signature message32)))

;; Bool <- 'a:Type Address Signature 'a
(def (signature-valid? type address signature data)
  (message-signature-valid? address signature (keccak256<-bytes (bytes<- type data))))

;; TODO: mixin the prototype with a formula that caches the abi bytes4 selector.
(define-type EthFunction
  (.mix (Record contract: [Address] selector: [Bytes4])
        {ethabi: "function"}))

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
   to: [(Maybe Address) optional: #t default: (void)] ;; as per Operation
   data: [(Maybe Bytes) optional: #t default: (void)] ;; as per Operation, absent or void means #u8()
   value: [(Maybe Quantity) optional: #t default: (void)] ;; in wei, absent or void means 0
   gas: [(Maybe Quantity) optional: #t default: (void)] ;; in gas, absent or void means auto estimate
   gasPrice: [(Maybe Quantity) optional: #t default: (void)] ;; in wei/gas, absent or void means get from the environment
   nonce: [(Maybe Quantity) optional: #t default: (void)])) ;; absent or void means get from tracker

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

;; Includes from: and hash: fields but not v: r: s:
(define-type TransactionInfo
  (Record
   from: [Address]
   nonce: [Quantity]
   gasPrice: [Quantity]
   gas: [Quantity]
   to: [(Maybe Address) optional: #t default: (void)]
   value: [Quantity]
   data: [Bytes optional: #t default: #u8()]
   hash: [Digest]))

;; Same as above, but includes a from: field as in a Transaction, and a hash: field as in SignedTx
(define-type SignedTransactionInfo
  (Record
   from: [Address]
   nonce: [Quantity]
   gasPrice: [Quantity]
   gas: [Quantity]
   to: [(Maybe Address) optional: #t default: (void)]
   value: [Quantity]
   data: [Bytes optional: #t default: #u8()]
   hash: [Digest]
   v: [Quantity] ;; actually UInt8... plus offset from chainId!
   r: [Quantity] ;; UInt256
   s: [Quantity])) ;; UInt256

;; TODO: implement single and/or multiple inheritance for records and have something like:
;;
;; Transaction <: PreTransaction CallParameters ;; e.g. CallParameters has no nonce but mandatory from
;; TransactionParameters <: PreTransaction
;; TransactionInfo <: Transaction {Hash}
;; Signature <: VRS
;; SignedTransactionData <: ShortTransactionData VRS Transaction
;; SignedTx <: SignedTransactionData {Hash} [RENAMED-SLOT input: <= data:]
;; SignedTransactionInfo <: SignedTx TransactionInfo
;; TransactionInformation <: SignedTransactionInfo [RENAMED-SLOT input: <= data:]
;;
;; Actually, there might be additional types with a slightly more complex hierarchy
;; to properly take into account field nullability. Ouch. Or not.
