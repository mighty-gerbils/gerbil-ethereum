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
(def (wei<-ether ether) (* one-ether-in-wei))
(def (wei<-gwei gwei) (* gwei one-gwei-in-wei))


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

(define-type SignedTransactionData
  (Record
   nonce: [Quantity]
   gasPrice: [Quantity]
   gas: [Quantity]
   to: [Address]
   value: [Quantity]
   data: [Bytes]
   v: [Quantity] ;; actually UInt8... plus offset from chainId!
   r: [Quantity] ;; UInt256
   s: [Quantity])) ;; UInt256


;; TODO: Sign transactions, based on spec in EIP-155
;; https://github.com/ethereum/EIPs/blob/master/EIPS/eip-155.md
;; For Ethereum main net, for block >= FORK_BLKNUM (2675000), use CHAIN_ID: 1 (ETH) or 61 (ETC)
;; Also Ropsten 3, Rinkeby 4, Goerli 5, Kovan 42, default Geth private chain 1337.
;; Find more chain ID's:
;; https://chainid.network
;; https://github.com/ethereum-lists/chains
;; https://github.com/ethereum/go-ethereum/blob/master/cmd/utils/flags.go

;; This function selects between the SignedTransactionData and ShortTransactionData
;; depending on whether eip155 was activated.
(def (txsigndata<- nonce gasprice startgas to value data chainid (eip155? #t))
  (identity ;; rlp-encode <-- TODO: use rlp-encode appropriately when available
   (if (and chainid eip155?)
     [nonce gasprice startgas to value data chainid 0 0]
     [nonce gasprice startgas to value data])))

;; This function computes the v value to put in the signed transaction data,
;; based on the v returned by the secp256k1 primitive (which is y-element parity+27)
;; and the chainid and whether the eip155 is activated. NB: 8+27=35
(def (eip155-v y-parity+27 chainid (eip155? #t))
  (if (and chainid eip155?)
    (+ 8 y-parity+27 (* 2 chainid))
    y-parity+27))
