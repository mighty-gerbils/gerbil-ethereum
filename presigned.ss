;;;; Presigned transactions, for some contracts to have the same address on all EVM blockchains
;;
;; An author may generate a random address for a creator account, and
;; have it sign transactions to create a contract or a series of contracts,
;; starting with nonce 0, for all possible values of the gasPrice
;; on a logarithmic scale with step sqrt(2),
;; so that whatever the present or future level of the gasPrice, you can always
;; fund the creator account and have it post the contract-creating transaction
;; with at most 42% of lossage in gas costs.
;; Then, one should delete that address.
;;
;; Do you have to trust the author to have deleted the creator address?
;; No, you don't: you can and must verify for each blockchain where you intend
;; to rely on it that the contract either exists with the correct content, or
;; that creator address was never used on the blockchain, in which case you
;; should use the presigned transactions to create the contract yourself at
;; the expected address, and wait for confirmation before you rely on it.
;; If those conditions aren't satisfied, then either:
;; (1) the blockchain has a starting nonce other than 0,
;; (2) someone broke the encryption (then all blockchains are broken), or
;; (3) the author lied to you.
;; The most the author can steal by lying is the gas to create the contract
;; (minus the tx fee), and then again only if they are sophisticated enough
;; to race your transactions between the one that funds the creator address
;; and the one that creates the contract, and then still, only on a single
;; blockchain, at which point everyone knows they are a cheat and won't
;; believe them ever on any blockchain or for anything else.
;; That would be a piss poor reward for losing their reputation forever.
;; But, again, please don't take any author's word: verify there was no abuse
;; before you rely on the address of a contract with presigned transactions.

(export #t)

(import
  :gerbil/gambit
  (only-in :std/error check-argument)
  (only-in :std/format fprintf)
  :std/iter
  :std/misc/number
  :std/srfi/1
  (only-in :std/sugar check-argument-uint)
  :std/text/hex
  :clan/base
  :clan/poo/object :clan/poo/brace :clan/poo/io
  :clan/crypto/secp256k1
  ./logger ./hex ./types ./ethereum ./known-addresses ./json-rpc
  ./transaction ./tx-tracker ./testing)

;; Return the nth power of sqrt(2), rounded down to the nearest integer
;; : Nat <- Nat
(def (integer-floor-sqrt2expt n)
  (check-argument-uint n)
  (if (odd? n) (integer-sqrt (arithmetic-shift 1 n))
      (arithmetic-shift 1 (half n))))

;; integer-length: il such that (< (1- (expt 2 (1- il))) n (expt 2 il)) = (ceiling (log (1+ n) 2))
;; integer-floor-log2: the largest l such that (<= (expt 2 l) n) = (floor (log n) 2)
;;(def (integer-floor-log2 n) (1- (integer-length n)))

;; Return the largest l such that (<= (integer-sqrt2expt l) n)
;; (every (lambda (i) (<= (integer-floor-sqrt2expt (integer-floor-logsqrt2 i)) i (1- (integer-floor-sqrt2expt (1+ (integer-floor-logsqrt2 i)))))) (iota 500 1))
;; : Nat <- Nat+
(def (integer-floor-logsqrt2 n)
  (check-argument-positive-integer n)
  (def j (* 2 (1- (integer-length n))))
  #;(DBG icl: n j (integer-floor-sqrt2expt j) (integer-floor-sqrt2expt (1+ j)) (integer-floor-sqrt2expt (+ j 2)))
  #;(assert! (<= (integer-floor-sqrt2expt j) n (1- (integer-floor-sqrt2expt (+ j 2)))))
  (if (<= (integer-floor-sqrt2expt (1+ j)) n) (1+ j) j))

;; For n>=2, return the smallest l such that (<= n (integer-sqrt2expt l))
;; For n in 0 or 1, return n
#;(every (lambda (i) (<= (1+ (integer-floor-sqrt2expt (1- (integer-ceiling-logsqrt2 i)))) i (integer-floor-sqrt2expt (integer-ceiling-logsqrt2 i)))) (iota 500 2))
;; : Nat <- Nat
(def (integer-ceiling-logsqrt2 n)
  (check-argument-uint n)
  (if (< n 2) n (1+ (integer-floor-logsqrt2 (1- n)))))

;; Treat 0 specially, mapping it to 0
;; TokenAmount <- UInt9
(def (zero-or-integer-floor-sqrt2expt i)
  (if (zero? i) 0 (integer-floor-sqrt2expt i)))

;; Incomplete prototype for a presigned transaction
;; : PresignedTransaction
(.def (presigned-tx. @ [] from)
   to: (void)
   data: (void)
   value: 0
   nonce: 0
   gas: (gas-estimate from to data value 1.5)
   sigs: (list->vector
          (for/collect (i (in-range 512))
            (def gasPrice (zero-or-integer-floor-sqrt2expt i))
            (def-slots (v r s) (sign-transaction {from to data value nonce gas gasPrice} 0))
            (bytes<- Signature (signature<-vrs v r s)))))

;; Create a presigned transaction for a pre-transaction
;; : PresignedTransaction <- PreTransaction
(def (presign-transaction pretx)
  (.mix pretx presigned-tx.))

;; Given a PresignedTransaction data structure and some gas prices (default: current),
;; return the presigned Transaction that fits for said level of gas price.
;; : Transaction <- PresignedTransaction gasPrice: ? TokenAmount
(def (tx<-presigned presigned gasPrice: (gasPrice (eth_gasPrice)))
  (def i (integer-ceiling-logsqrt2 gasPrice))
  (set! gasPrice (zero-or-integer-floor-sqrt2expt i))
  (def-slots (from to data value nonce gas sigs) presigned)
  (defvalues (v r s) (vrs<-signature (<-bytes Signature (vector-ref sigs i))))
  (verify-signed-tx! (make-signed-transaction from nonce gasPrice gas to value data v r s)))

;; : Transaction <- PresignedTransaction gasPrice: ? TokenAmount
(def (verify-presigned presigned (i #f))
  (if i
    (tx<-presigned presigned gasPrice: (if (< i 2) i (integer-floor-sqrt2expt i)))
    (for ((i (in-iota 512))) (verify-presigned presigned i))))

;; Raw transactions from a presigned transaction and an optional gas price index,
;; so you can more easily track in the ethereum logs if/when the transaction appears.
;; : (Vector String 512) <- PresignedTransaction
(def (raw<-presigned presigned)
  (for/collect (i (in-iota 512))
    (hex-encode (bytes<-signed-tx (tx<-presigned presigned gasPrice: (if (< i 2) i (integer-floor-sqrt2expt i)))))))

;; Use this function to create a presigned transaction, usually to create a given contract.
;; : PresignedTransaction <- Bytes
(def (presign-contract-creation code)
  (def creator-name "presigning-account")
  (register-keypair creator-name (generate-keypair scoring: (scoring<-prefix "8e7a")))
  (def creator (address<-nickname creator-name))
  (begin0
      (force-object (presign-transaction {from: creator data: code}))
    (unregister-keypair creator-name)))

;; Send a presigned transaction to the current blockchain
(def (send-presigned presigned
      funder: (funder croesus) gasPrice: (gasPrice (void)) log: (log eth-log))
  (def-slots (from to data nonce value gas sigs) presigned)
  (def creator from)
  (def block (eth_blockNumber))
  (unless (uint256? gasPrice)
    (set! gasPrice (max 1 (eth_gasPrice))))
  (unless (equal? (eth_getTransactionCount creator block) nonce)
    (error "Creator address was already used or initial nonce > 0"))
  (def tx (tx<-presigned presigned gasPrice: gasPrice))
  (def balance (eth_getBalance creator block))
  (def missing (- (* gas (.@ tx gasPrice)) balance))
  (when (positive? missing)
    (post-transaction {from: funder to: creator value: missing}))
  (log ["send-presigned" (json<- SignedTransactionInfo tx)])
  (post-transaction tx))

;; Create a new list by alternate between entries of a and b (a first)
;; until either list runs out, at which point append the other one.
;; List <- List List ?List
(def (alternate-lists a b (r-head '()))
  (cond
   ((null? a) (append-reverse r-head b))
   ((null? b) (append-reverse r-head a))
   (else (alternate-lists (cdr a) (cdr b) [(car b) (car a) r-head ...]))))

;; (List UInt9) <- ?Quantity
(def (likely-gasPrice-indices (gasPrice (eth_gasPrice)))
  (def current (integer-ceiling-logsqrt2 gasPrice))
  (alternate-lists (iota (- 512 current) current) (iota current)))

;; Address <- PresignedTransaction ?String ?Address ?Quantity ?(<- Jsonable)
(def (ensure-presigned-contract presigned
      name: (name "presigned contract")
      funder: (funder croesus) gasPrice: (gasPrice (void)) log: (log eth-log))
  (def-slots (from to data nonce value gas sigs) presigned)
  (check-argument (equal? [to nonce value] [(void) 0 0]) "zero" [to nonce value])
  (def creator from)
  (def address (address<-creator-nonce creator nonce))
  (match (eth_getTransactionCount creator 'latest)
    (0
     ;; TODO: be ready to raise the gasPrice
     ;; if the transaction doesn't go through after a while...
     (send-presigned presigned funder: funder gasPrice: gasPrice log: log))
    (1
     ;; TODO: handle the case where the transaction wasn't sufficiently confirmed (yet)
     (def i (any (lambda (i)
                   (def gasPrice (zero-or-integer-floor-sqrt2expt i))
                   (def tx (tx<-presigned presigned gasPrice: gasPrice))
                   (def tr (eth_getTransactionReceipt (.@ tx hash)))
                   (successful-receipt? tr))
                 (likely-gasPrice-indices)))
     (unless i
       (error "Bad contract created for " name address data)))
    (n (error "Creator address was used more than once(?) or initial nonce > 0" address n)))
  address)

(define-type PreSigs (Vector Bytes65 512))

;; : <- PresignedTrasaction ?OutputPort
(def (display-presigned presigned (port (current-output-port)))
  (with-slots (from to value nonce gas data sigs) presigned
    (fprintf port " {from: ~s
  to: ~r value: ~r nonce: ~r gas: ~r
  data: ~s
  sigs: (<-bytes PreSigs (bytes<-0x ~s))}\n"
             (sexp<- Address from) to value nonce gas
             (sexp<- Bytes data)
             (0x<-bytes (bytes<- PreSigs sigs)))))
