;; Support for multiple classes of assets in a contract
;; TODO: add support on the client side in the same classes, too.
;; TODO: make that part of support of assets on multiple blockchains!

(export #t)
(import
  :std/sugar :std/format :std/misc/list :std/misc/string :std/misc/hash :std/srfi/1 :std/srfi/13 :std/iter
  :clan/base :clan/basic-parsers :clan/decimal :clan/string
  :clan/poo/object
  ./assembly ./types ./ethereum ./abi ./evm-runtime ./network-config ./json-rpc ./erc20 ./simple-apps
  ./transaction ./tx-tracker)

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
  ;;
  ;; .get-balance : @ <- .Address

  ;; (.transfer sender recipient amount) transfers 'amount' funds from 'sender' to
  ;; 'recipient'. Caller must be authorized to act on behalf of the sender.
  ;;
  ;; .transfer : <- .Address .Address @

  ;; (.commit-deposit! amount) generates EVM code to finalize/verify a deposit
  ;; of 'amount' into the consensus. This will be called once per asset type
  ;; at transaction commit.
  ;;
  ;; .commit-deposit! : (EVMThunk <-) <- (EVMThunk Amount <-)

  ;; (.commit-withdraw! recipient amount balance-var) is generates EVM code to
  ;; finalize/verify a withdrawal of 'amount' from the consensus. 'recipient' is the
  ;; participant making the withdrawal, and balance-var is the static variable holding
  ;; the balance for this (recipient, asset type) pair. called at transaction commit
  ;; once for each such pair.
  ;;
  ;; .commit-withdraw!: ;; (EVMThunk <-) <- (EVMThunk .Address <-) (EVMThunk @ <-) StaticVar

  ;; .commit-withdraw-all! is like .commit-withdraw!, but:
  ;;
  ;; - Instead of taking the participant as a (scheme) parameter, it is expected
  ;;   to be at the top of the stack.
  ;; - It doesn't take an amount; instead, the entire balance is withdrawn.
  ;;
  ;; .commit-withdraw-all!: (EVMThunk <- .Address) <- StaticVar

  ;; (.approve-deposit! sender recipient amount) pre-approves a deposit into 'recipient'
  ;; from account 'sender', with the given amount, if necessary.
  ;;
  ;; .approve-deposit! : <- .Address .Address @
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
  .get-balance: ;; @ <- .Address
  (lambda (address) (eth_getBalance address 'latest))
  .transfer:
    (lambda (sender recipient amount)
      (post-transaction (transfer-tokens
                          from: sender
                          to: recipient
                          value: amount)))
  ;; NB: The above crucially depends on the end-of-transaction code including the below check,
  ;; that must be AND'ed with all other checks before [&require!]
  .commit-deposit!: ;; (EVMThunk <-) <- (EVMThunk @ <-)
  (lambda (amount)
    (&begin amount CALLVALUE EQ &require!))
  .commit-withdraw!: ;; (EVMThunk <-) <- (EVMThunk .Address <-) (EVMThunk @ <-) StaticVar
  (lambda (recipient amount balance-var)
    (&begin amount recipient DUP2 (&sub-var! balance-var) &send-ethers!)) ;; Transfer!
  .commit-withdraw-all!:
  (lambda (balance-var) ;; (EVMThunk <- .Address) <- StaticVar
    (&begin (.@ balance-var get) SWAP1 &send-ethers! 0 (.@ balance-var set!)))
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
  .get-balance: ;; @ <- .Address
  (lambda (address) (erc20-balance .contract-address address))
  .transfer:
    (lambda (sender recipient amount)
      (erc20-transfer .contract-address sender recipient amount))
  .commit-deposit!: ;; (EVMThunk <-) <- (EVMThunk @ <-)
  (lambda (amount) ;; tmp@ is the constant offset to a 100-byte scratch buffer
    (&begin
     transferFrom-selector (&mstoreat/overwrite-after tmp100@ 4)
     CALLER (&mstoreat (+ tmp100@ 4))
     ADDRESS (&mstoreat (+ tmp100@ 36))
     amount (&mstoreat (+ tmp100@ 68))
     32 tmp100@ 100 DUP2 0 .contract-address GAS CALL
     ;; check that both the was successful and its boolean result true:
     (&mloadat tmp100@) AND &require!))
  .commit-withdraw!: ;; (EVMThunk <-) <- (EVMThunk .Address <-) (EVMThunk @ <-) StaticVar
  (lambda (recipient amount balance-var)
    (&begin
      recipient
      (&erc20-commit-withdraw
        .contract-address
        amount
        balance-var)))
  .commit-withdraw-all!:
  (lambda (balance-var) ;; (EVMThunk <- .Address) <- StaticVar
    (&erc20-commit-withdraw
      .contract-address
      (.@ balance-var get)
      balance-var))
  .approve-deposit!:
  (lambda (sender recipient amount)
    (erc20-approve .contract-address sender recipient amount)))

;; TODO: *if/when* we have a shared contract between multiple Glow interactions,
;; without a consensus on which ERC20 contracts can be trusted not to be exploit vectors,
;; then we need to add a flag to prevent re-entrancy (cost: ~5000 GAS) before we call out to
;; token contracts for withdrawals. Alternatively, if there's only one state variable at stake,
;; we can check that the state variable wasn't modified by a recursive call before we modify it,
;; (cost: ~700 gas? 2100?) which is slightly cheaper.

;; &erc20-commit-withdraw : (EVMThunk <- Address) <- Address (EVMThunk TokenAmount <-) StaticVar
;;
;; Sends funds for an erc20 token to a participant. Parameters:
;;
;; * contract-address is the address of the erc20 contract.
;; * amount pushes the amount to send
;; * balance-var is the balance variable to update.
;;
;; The resulting EVMThunk expects the participant address at the top of the stack.
(def (&erc20-commit-withdraw contract-address amount balance-var)
  (&begin
   transfer-selector (&mstoreat/overwrite-after tmp100@ 4)
   ;; recipient is on top of the stack already.
   (&mstoreat (+ tmp100@ 4))
   amount DUP1 (&sub-var! balance-var) (&mstoreat (+ tmp100@ 36))
   32 tmp100@ 68 DUP2 0 contract-address GAS CALL
   ;; check that both the call was successful and that its boolean result was true:
   (&mloadat tmp100@) AND &require!))

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
