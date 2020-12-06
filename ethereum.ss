(export #t)

(import
  :std/sugar
  :clan/maybe
  :clan/poo/poo :clan/poo/io :clan/poo/brace
  (only-in :clan/poo/mop sexp<-)
  (only-in :clan/poo/type Sum define-sum-constructors)
  ./types ./hex ./signing)

;; TODO: implement and use a "newtype"
(define-type Quantity UInt256)
(define-type UInt UInt256)
(define-type Digest Bytes32)
(define-type Data Bytes)
(define-type Block UInt32) ;; block number: in practice fits in 32 bits, though officially UInt256
(register-simple-eth-type Address)

;; TODO: mixin the prototype with a formula that caches the abi bytes4 selector.
(define-type EthFunction
  (.mix (Record contract: [Address] selector: [Bytes4])
        {ethabi: "function"}))


(def one-ether-in-wei (expt 10 18)) ;; 1 ETH = 10^18 wei
(def one-gwei-in-wei (expt 10 9)) ;; 1 gwei = 10^9 wei
(def (wei<-ether ether) (* ether one-ether-in-wei))
(def (ether<-wei wei) (/ wei one-ether-in-wei))
(def (wei<-gwei gwei) (* gwei one-gwei-in-wei))
(def (gwei<-wei wei) (/ wei one-gwei-in-wei))


(define-type Confirmation
  (Record
   transactionHash: [Digest]
   transactionIndex: [Quantity]
   blockNumber: [Quantity]
   blockHash: [Digest]))

(define-type TxHeader
  (Record
   sender: [Address]
   nonce: [Quantity]
   gasPrice: [Quantity] ;; in wei per gas
   gas: [Quantity] ;; upper limit, there will be a refund
   value: [Quantity])) ;; in wei}

;; Three kinds of operations that may be posted
(define-type Operation
  (Sum
   TransferTokens: Address ; to
   CreateContract: Bytes ; data
   CallFunction: (Record to: [Address] data: [Bytes])))
(define-sum-constructors Operation TransferTokens CreateContract CallFunction)
(def operation-to
  (match <>
    ((Operation-TransferTokens to) to)
    ((Operation-CreateContract _) #f)
    ((Operation-CallFunction {(to)}) to)))
(def operation-data
  (match <>
    ((Operation-TransferTokens _) #f)
    ((Operation-CreateContract data) data)
    ((Operation-CallFunction {(data)}) data)))

(define-type PreTransaction
  (Record
   sender: [Address]
   operation: [Operation]
   value: [Quantity] ;; in wei
   gas: [Quantity])) ;; in gas

;; Transaction (to be) posted to the chain Ethereum
;; TODO: merge the fields of tx-header and operation instead, just with a new type tag.
(define-type Transaction
  (Record
   tx-header: [TxHeader]
   operation: [Operation]))

(def (PreTransaction<-Transaction tx)
  {sender: (.@ tx tx-header sender)
   operation: (.@ tx operation)
   value: (.@ tx tx-header value)
   gas: (.@ tx tx-header gas)})

(define-type ShortTransactionData
  (Record
   nonce: [Quantity]
   gasPrice: [Quantity]
   gas: [Quantity]
   to: [(Maybe Address) optional: #t default: (void)]
   value: [Quantity]
   data: [Bytes optional: #t default: #u8()]))

(define-type SignedTransactionData
  (Record
   nonce: [Quantity]
   gasPrice: [Quantity]
   gas: [Quantity]
   to: [(Maybe Address) optional: #t default: (void)]
   value: [Quantity]
   data: [Bytes optional: #t default: #u8()]
   v: [Quantity] ;; actually UInt8... plus offset from chainId!
   r: [Quantity] ;; UInt256
   s: [Quantity])) ;; UInt256
