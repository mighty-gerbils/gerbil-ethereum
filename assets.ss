;; Support for multiple classes of assets in a contract
;; TODO: add support on the client side in the same classes, too.
;; TODO: make that part of support of assets on multiple blockchains!

(export #t)
(import
  :std/sugar :std/format :std/misc/list :std/misc/string :std/misc/hash :std/srfi/1 :std/srfi/13 :std/iter
  :clan/base :clan/basic-parsers :clan/decimal :clan/string
  :clan/poo/object
  ./assembly ./types ./ethereum ./abi ./evm-runtime ./network-config ./json-rpc ./erc20 ./simple-apps)

;; TODO: rename asset to resource
;; for ERC721s, multiple resources in a resource-directory or resource-collection?

;; keys are uppercase symbols such as ETH, PET, CED, QASPET, RBTPET, etc.
(def asset-table (hash))
;; lookup-asset : Symbol -> AssetType
(def (lookup-asset s)
  (hash-ref/default asset-table s
    (lambda () (error 'lookup-asset s "not found in" (hash-keys asset-table)))))
;; register-asset! : AssetType -> Void
(def (register-asset! a) (hash-put! asset-table (.@ a .symbol) a))

;; Abstract interface for an asset type.
(.def (Asset @ [Type.])
  .element?: (lambda (v)
               (and (object? v) (.has? v .symbol) (hash-key? asset-table (.@ v .symbol))))
  .sexp<-: (lambda (a) `(lookup-asset ',(.@ a .symbol)))
  .json<-: (lambda (a) (symbol->string (.@ a .symbol)))
  .<-json: (lambda (j) (lookup-asset (string->symbol j)))
  .string<-: (lambda (a) (symbol->string (.@ a .symbol)))
  .<-string: (lambda (s) (lookup-asset (string->symbol s)))
  .bytes<-: (lambda (a) (string->bytes (symbol->string (.@ a .symbol))))
  .<-bytes: (lambda (b) (lookup-asset (string->symbol (bytes->string b))))

  ;; Implementations should additionally define:

  ;; Query the current balance of this asset for an address.
  ;; .get-balance : UInt256 <- Address

  ;; (.transfer sender recipient amount) transfers 'amount' funds from 'sender' to
  ;; 'recipient'. Caller must be authorized to act on behalf of the sender.
  ;;
  ;; .transfer : <- Address Address UInt16

  ;; (.commit-deposit! amount) generates EVM code to finalize/verify a deposit
  ;; of 'amount' into the consensus. This will be called once per asset type
  ;; at transaction commit.
  ;;
  ;; .commit-deposit! : (EVMThunk <-) <- (EVMThunk Amount <-)

  ;; (.commit-withdraw! recipient amount balance-var) is generates EVM code to
  ;; finalize/verify a withdrawal of 'amount' from the consensus. 'recipient' is the
  ;; participant making the withdrawal, and balance-var is the static variable holding
  ;; the balance for this (recipient, asset type) pair. called at transaction commit
  ;; one for each such pair.
  ;;
  ;; .commit-withdraw!: ;; (EVMThunk <-) <- (EVMThunk .Address <-) (EVMThunk @ <-) StaticVar

  ;; (.approve-deposit! sender recipient amount) pre-approves a deposit into 'recipient'
  ;; from account 'sender', with the given amount, if necessary.
  ;;
  ;; .approve-deposit! : <- Address Address Unit256
  )

(.def (TokenAmount @ [] .decimals .validate .symbol)
  .denominator: (expt 10 .decimals)
  ;; TODO: should we be including the name of the token in the string? after the number?
  .string<-: (lambda (x) (format "~a ~a"
                            (string<-decimal (/ x .denominator))
                            .symbol))
  .<-string: (lambda (s)
               (assert! (string-suffix? (format " ~a" .symbol) s))
               (.validate (*
                           .denominator
                           (decimal<-string
                            s
                            sign-allowed?: #t
                            exponent-allowed: #t
                            start: 0
                            end: (- (string-length s) (string-length (symbol->string .symbol)) 1))))))

(.def (Ether @ [TokenAmount UInt256] ;; or should it just be UInt96 ???
       .length-in-bytes .length-in-bits)
  .asset-code: 0
  .network: 'eth
  .name: "Ether"
  .symbol: 'ETH
  .decimals: 18
  .Address: Address
  .get-balance:
  (lambda (address) ;; UInt256 <- Address
    (eth_getBalance address 'latest))
  .transfer:
    (lambda (sender recipient amount)
      (batch-txs
        sender
        [(batched-transfer amount recipient)]
        gas: 400000))
  ;; NB: The above crucially depends on the end-of-transaction code including the below check,
  ;; that must be AND'ed with all other checks before [&require!]
  .commit-deposit!: ;; (EVMThunk <-) <- (EVMThunk Amount <-) UInt16
  (lambda (amount)
    (&begin amount CALLVALUE EQ &require!))
  .commit-withdraw!: ;; (EVMThunk <-) <- (EVMThunk .Address <-) (EVMThunk @ <-) (EVMThunk <- @) UInt16
  (lambda (recipient amount balance-var)
    (&begin amount recipient DUP2 (&sub-var! balance-var) &send-ethers!)) ;; Transfer!
  .approve-deposit!:
  (lambda (sender recipient amount) (void)))

(register-asset! Ether)

(.def (ERC20 @ [TokenAmount UInt256] ;; https://eips.ethereum.org/EIPS/eip-20
       .contract-address ;; : Address
       .name ;; : String ;; full name, e.g. "FooToken"
       .symbol ;; : Symbol ;; symbol, typically a TLA, e.g. 'FOO
       .decimals) ;; : Nat ;; number of decimals by which to divide the integer amount to get token amount
  .asset-code: .contract-address
  .Address: Address
  .get-balance:
  (lambda (address) ;; UInt256 <- Address
    (erc20-balance .contract-address address))
  .transfer:
    (lambda (sender recipient amount)
      (erc20-transfer .contract-address sender recipient amount))
  .commit-deposit!: ;; (EVMThunk <-) <- (EVMThunk Amount <-) UInt16
  (lambda (amount) ;; tmp@ is the constant offset to a 100-byte scratch buffer
    (&begin
     transferFrom-selector (&mstoreat/overwrite-after tmp100@ 4)
     CALLER (&mstoreat (+ tmp100@ 4))
     ADDRESS (&mstoreat (+ tmp100@ 36))
     amount (&mstoreat (+ tmp100@ 68))
     32 tmp100@ 100 DUP2 0 .contract-address GAS CALL
     ;; check that both the was successful and its boolean result true:
     (&mloadat tmp100@) AND &require!))
  .commit-withdraw!: ;; (EVMThunk <-) <- (EVMThunk .Address <-) (EVMThunk @ <-) (EVMThunk <- @) UInt16
  (lambda (recipient amount balance-var)
    (&begin
     transfer-selector (&mstoreat/overwrite-after tmp100@ 4)
     recipient (&mstoreat (+ tmp100@ 4))
     amount DUP1 (&sub-var! balance-var) (&mstoreat (+ tmp100@ 36))
     32 tmp100@ 68 DUP2 0 .contract-address GAS CALL
     ;; check that both the call was successful and that its boolean result was true:
     (&mloadat tmp100@) AND &require!))
  .approve-deposit!:
  (lambda (sender recipient amount)
    (erc20-approve .contract-address sender recipient amount)))

(def (expect-asset-amount port)
  (def asset ((expect-one-or-more-of char-ascii-alphabetic?) port))
  (expect-and-skip-any-whitespace port)
  (def amount (expect-decimal port))
  (cons asset amount))

(def (asset-amount<-string string trim-spaces?: (trim-spaces? #t))
  (parse-string (if trim-spaces? (string-trim-spaces string) string) expect-asset-amount
                "asset-amount"))

(def (display-asset-amount asset-amount port)
  (with ([asset . amount] asset-amount)
    (display asset port) (write-char #\space port) (write-decimal amount port)))

(def (string<-asset-amount asset-amount)
  (call-with-output-string (cut display-asset-amount asset-amount <>)))

(def (asset->network a)
  (hash-ref ethereum-networks (symbol->string (.@ a .network))))

;; native-asset? : Bool <- Asset
;; Produces true if `a` is the native asset of its associated network
(def (native-asset? a)
  (def network (asset->network a))
  (def native-name (.@ network nativeCurrency symbol))
  (equal? (.@ a .symbol) native-name))

;; lookup-native-asset : Asset <- EthereumConfig
;; Produces the native asset of the network from (ethereum-config)
(def (lookup-native-asset (ec (ethereum-config)))
  (lookup-asset (.@ ec nativeCurrency symbol)))

;; find-network-assets : Network -> [Listof Asset]
(def (find-network-assets (network (ethereum-config)))
  (for/collect ((p (hash->list/sort asset-table symbol<?))
                when (equal? (asset->network (cdr p)) network))
    (cdr p)))
