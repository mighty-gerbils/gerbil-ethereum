(export #t)

(import
  :gerbil/gambit/bytes :gerbil/gambit/exceptions :gerbil/gambit/threads
  :std/error :std/misc/completion :std/text/hex
  :clan/base :clan/concurrency :clan/exception
  :clan/failure :clan/option
  :clan/net/json-rpc
  :clan/poo/object :clan/poo/brace :clan/poo/io :clan/poo/trie
  :clan/persist/db :clan/persist/persist
  :clan/debug :clan/poo/debug
  ./hex ./types ./known-addresses ./ethereum ./json-rpc ./nonce-tracker ./transaction)

;; TODO: Invoking a *persistent* continuation upon completion.
;;
;; TODO: A much better state machine to get wanted transactions confirmed.
;;
;; It is a very bad idea to have more than one ongoing transaction in the mempool:
;; you might hope everything goes right and they are added in the correct order,
;; but in practice so many things can go wrong and then the mitigations become hell,
;; and attackers can get you to fail to transact, to deadlock, to replay your spending,
;; to fail to meet a deadline, or more generally fail to meet your contractual obligations.
;;
;; Obvious strategies don't work:
;; - If you never re-send a transaction, but for some reason one transaction doesn't go through
;; because it was received out-of-order by the winning PoW nodes and dropped on the ground,
;; or otherwise was lost in the shuffle of network packet drops, then you deadlock
;; - If you always re-send a transaction, but never update the nonce, and some other client
;; using the same private key (WHY? That should be a red alert anyway, unless it's actually
;; another copy of yourself sending another variant of the same transaction due to netsplit,
;; and you re-synch after netmerge), or some other transaction on the same client races me
;; (if you fail to sequentialize transactions one at a time through a single thread),
;; then you deadlock.
;; - If you always re-send a transaction, and you update the nonce if you see yours is out-of-date,
;; then you can race yourself and/or other transactions into sending multiple copies of a same
;; transaction and end up spending many times over what you wanted to spend (very bad).
;; - Whatever decisions you make based on what the ethereum node tells you, it can give you _hints_
;; about things that are going on, but nothing it says is authoritative until it is, which is only
;; 30 minutes later (or say 10 minutes, if you accept the risk of lower security).
;; Until confirmation, whatever it says is subject to revision.
;; - The safest would be to nurse each and every transaction to either completion or definite failure
;; before even attempting the next one, but then that's only one transaction per account every 30
;; minutes minimum (NB: binance is OK with 36 confirmations ~10 minutes),
;; and maybe much worse depending on how "definite failure" is defined.
;; This suggests that having multiple accounts could be a requirement for playing safe
;; with smart contracts: each "system" (itself distributed with redundant workers for reliability)
;; has its own private key that won't race with other systems.
;;
;; One problem is that your local ethereum node (and/or, in a real network, whichever remote node
;; will eventually issue the blocks), sometimes will just drop your signed transactions,
;; for whatever reasons: not enough ether, not enough gas, gas price too low, nonce out of synch,
;; network error, network split, denial-of-service attack, selective censorship, block-buying
;; attack, local reverts due to PoW attacks even less than 33%, etc.
;; You have to resend, sometimes with updated gas price, sometimes with updated nonce,
;; sometimes even with updated contract parameters, etc.
;; Yet, you should be wary of changing anything substantive (to your application)
;; about a transaction being sent, or you can race yourselves, and end up paying twice
;; (or many more times) to receive a counterpart only once (or not at all).
;;
;; A good strategy might take into account what did or didn't happen in not-fully-confirmed blocks,
;; yet (obviously) would not consider anything confirmed until it's confirmed.
;; It is unclear how best to deal with multiple queued transactions —
;; the happy case of sending consecutive nonces automatically is great,
;; but when things break down (including due to the aforementioned re-send issues)
;; it's a hell that's hard to recover from, since new transactions will race the old ones,
;; and any sequential dependency between them becomes quite tricky to enforce.
;;
;; One solution: a *batching contract*.
;; Multiple transactions are sent atomically via a single call to some generic contract
;; that plays them in sequence. Caveat: you better get you gas computation damn right!
;; Also mind the size limits to your overall transaction, the possibly more complex gas
;; price computation to convince miners to get it through in a timely fashion, etc.
;; If you do it right, though, you only have to deal with a single network event,
;; which makes the limit of one nursed transaction per 30 minute much more bearable.
;; This strategy implies or at least suggests developing a better-than-trivial batching strategy
;; to group transactions, similar to what we use in db.ml for batching database writes,
;; possibly with its own notion of atomic "transaction sets" that group "transactions" together.
;;
;; Additional feature: a *replay barrier*.
;; The same generic contract can also help, onerously, with avoiding to replay a transaction multiple
;; time in the context where you do want to be able to race yourself.
;; Good reasons to race yourself is when you have strong obligations to fulfill in a short deadline,
;; but the current chain is wobbly due to some attack, particularly network splits:
;; multiple of your workers might be victims of the network split (maybe targetted!),
;; and would trigger racing variants of the queued transactions.
;; In this case, the contract may associate a semaphore to each application-defined
;; atomic set of transactions (for a shared multi-user contract, salted with sender ID);
;; it would check that the semaphore wasn't set before to actually play the transaction set,
;; and set the semaphore afterwards. Once again, miscompute worst-case gas and you're dead.
;; You need to pay extra gas to read and write a semaphore, and will lose gas in case that
;; multiple copies make it to the blockchain; but at least you won't lose the principal.
;; To avoid the need for a replay barrier, you must always wait for *some* transaction
;; with the given nonce to be fully confirmed before you start using the next nonce.
;;
;; When you have a tight deadline for some transactions and not others,
;; you may have to up the gas price for the transactions you really want to get through,
;; with an understanding of the algorithm used by the miners and of the strategy used
;; by whoever is trying to bribe the miners out of including your transaction.
;; Maybe you have to fill the block gas limit. Or maybe you have to split your transaction
;; batch into smaller ones. An experimental study may be necessary, as well as regular
;; updates to the software agents --- quite unlike the contracts that are immutable by design,
;; transaction posting strategies may have to be mutable and evolving by design
;;
;; In some case, the right thing to do might be to consult back with the user,
;; and ask them to add more ether, to update their gas price strategy, to deal with potentially
;; or actually broken contracts, to watch the missing nodes of their personal database, etc.
;; By default, we probably want to queue transactions one by one to avoid nonce-overlap issues;
;; we may be more or less aggressive in terms of using nonces without partial or total confirmation
;; from previous transactions, depending on the nature of the transactions.
;;
;; In any case, to definitely want a single system (even if possibly split into partitions)
;; to issue transactions from a given address to minimize races.
;;
;; There is potentially a LOT of complexity, and if possible we want to partner with other people
;; to define sound strategies... just that is a topic for itself, and there are sometimes games
;; that people can play with lock out strategy via paying extra in gas to buy enough blocks to
;; lock rivals out of a contract, etc. A generic strategy, DSL for strategies, etc., could be
;; a research topic in itself. Sigh.
;;
;; Most people don't hit this issue, because they don't abide by a contract binding them to partake
;; in distributed transactions across multiple blockchains with a priori untrustworthy other parties
;; within tight deadlines. And most of the few who do possibly haven't thought deep enough
;; about the ins and outs of these issues. Scary.
;;
;; Here, for now, we follow a very dumb strategy, of having only one active transaction per address.
;; Furthermore, we only sign once, and resending blindly afterwards,
;; trusting the gas computation, and trusting the nonce until it's found to be too low.
;;
;; TODO: implement an asynchronous way for the UI to peek at the status of a transaction
;; while it's going along its slow progress.
;;
;; TODO: look at how OMiseGo does it, Andrew Redden tells me they have something public
;; (and he has something private).

(define-type ExceptionOrString
  {(:: @ [methods.bytes<-marshal Type.])
   .element?: (lambda (e) (or (exception? e) (Exception? e) (string? e)))
   .string<-: string<-exception
   .sexp<-: .string<-
   .<-string: identity
   .<-json: .string<-
   .json<-: .string<-
   .marshal: (lambda (e port) (marshal String (.string<- e) port))
   .unmarshal: (lambda (port) (unmarshal String port))})

(define-type TransactionStatus
  (Sum
   TxWanted: PreTransaction
   TxSigned: (Tuple PreTransaction SignedTransactionInfo)
   TxConfirmed: (Tuple PreTransaction SignedTransactionInfo TransactionReceipt)
   TxFailed: (Tuple (delay-type TransactionStatus) ExceptionOrString)))
(define-sum-constructors TransactionStatus TxWanted TxSigned TxConfirmed TxFailed)

(def transaction-status-ongoing?
  (match <>
    ((TransactionStatus-TxWanted _) #t)
    ((TransactionStatus-TxSigned _) #t)
    (_ #f)))

(def transaction-status-final?
  (match <>
    ((TransactionStatus-TxConfirmed _) #t)
    ((TransactionStatus-TxFailed _) #t)
    (_ #f)))

(def PreTransaction<-TransactionStatus
  (match <>
    ((TransactionStatus-TxWanted preTx) preTx)
    ((TransactionStatus-TxSigned (vector preTx _)) preTx)
    ((TransactionStatus-TxConfirmed (vector preTx _ _)) preTx)
    ((TransactionStatus-TxFailed (vector ots _)) (PreTransaction<-TransactionStatus ots)))) ;; ots must be TxWanted or TxSigned

(define-type TransactionTrackerKey
  (Record user: [Address] ;; user who issued the transaction
          txsn: [Nat])) ;; serial number for the tx

;; This activity safely tracks a transaction posted by this client
;; and nurses it until it is included on the blockchain.
;; The serial number is relative to the current application, and is NOT the blockchain "nonce":
;; Transactions successfully issued outside of the application may increase the nonce,
;; while transactions issued inside the application may fail and not increase the nonce.
(.def (TransactionTracker @ [(Record result: [Completion] manager: [Thread])
                             DebugPersistentActivity]
       <-key)
  sexp: 'TransactionTracker
  Key: TransactionTrackerKey
  key-prefix: (string->bytes "ETTT")
  State: TransactionStatus
  ;; Usage: make the activity, start the thread when ready, wait for the result
  ;; TODO: inspection? cancellation? merging with other transactions?
  ;; TODO: (1) for new activity, start the thread after the initial state was saved.
  ;;       (2) for resumed activity, start the thread immediately
  .restore:
  (lambda (key save! status _tx)
    (def user (.@ key user))
    (def txsn (.@ key txsn))
    (def result (make-completion))
    (def manager
      (without-tx
       (make-thread
        (thunk-with-logged-exceptions
         (fun (make-transaction-tracker)
           (def (update status)
             (with-committed-tx (tx) (save! status tx)))
           (let loop ((status status))
             (validate TransactionStatus status [[TT.loop: [@] key]])
             (def (continue status) (update status) (loop status))
             (def (invalidate transaction-status e)
               (reset-nonce user)
               (continue (TransactionStatus-TxFailed (vector transaction-status e))))
             (match status
               ((TransactionStatus-TxWanted pretx)
                (match (with-result (sign-transaction pretx))
                  ((failure e) (invalidate status e))
                  ((some stx) (continue (TransactionStatus-TxSigned (vector pretx stx))))))
               ((TransactionStatus-TxSigned (vector pretx signed))
                (match (with-result
                        (retry retry-window: 0.05 max-window: 30.0 max-retries: +inf.0
                          (fun (try-confirm)
                            (def result (with-result (send-signed-transaction signed)))
                            (match result
                              ((some _) result)
                              ((failure e)
                               (if (or (NonceTooLow? e)
                                       (TransactionRejected? e)
                                       (IntrinsicGasTooLow? e))
                                 result
                                 (raise e)))))))
                  ((some (some (? successful-receipt? receipt)))
                   (continue (TransactionStatus-TxConfirmed (vector pretx signed receipt))))
                  ((some (failure (? NonceTooLow?)))
                   (continue (TransactionStatus-TxWanted pretx)))
                  ((some (failure e))
                   (invalidate status e))
                  ((some (some x))
                   (invalidate status (Invalid ["unexpected result" x])))
                  ((failure e)
                   (invalidate status e))))
               (final
                ;; TODO: should we return the tx with the status in the completion-post! ???
                (.call UserTransactionsTracker remove-transaction user txsn)
                (completion-post! result final))))))
        [sexp (sexp<- Address user) txsn])))
    {result manager})

  ;; Activate the transaction tracker (1) for the given key, (2) in the context of the given TX.
  ;; This method must be called when all previous transactions by the same user are finalized;
  ;; it must only be called once and only once, so the caller must use suitable mutual exclusion.
  ;; : Unit <- Key
  activate:
  (lambda (key)
    (def manager (.@ (<-key key) manager))
    (when (thread-state-initialized? (thread-state manager))
      (thread-start! manager)))

  ;; Wait for a transaction to be successfully posted or to fail for good.
  ;; NB: You must (<-key key) the activity and wait for it from *outside* any transaction
  ;; (otherwise you'll hang the system).
  ;; : FinalTransactionStatus <- @
  wait: (lambda (tracker) (completion-wait! (.@ tracker result))))

(define-type UserTransactionsState
  (Record transaction-counter: [Nat]
          ongoing-transactions: [NatSet]))

(.def (UserTransactionsTracker @ [SavingDebug PersistentActor]
       <-key action async-action)
  sexp: 'UserTransactionsTracker
  Key: Address
  key-prefix: (string->bytes "ETUS")
  State: UserTransactionsState

  ;; : State <- Address
  make-default-state:
  (lambda (_user)
    {transaction-counter: 0
     ongoing-transactions: (.@ NatSet .empty)})

  ;; Resume the earliest transaction.
  ;; If more are pending, it will automatically cascade onto the next ones when done,
  ;; by recursively calling resume-transactions.
  ;; : Unit <- Address State
  resume-transactions:
  (lambda (user state)
    (without-tx
      (for-each/option (fun (for-txsn txsn) (.call TransactionTracker activate {user txsn}))
                       (.call NatSet .min-elt/opt (.@ state ongoing-transactions)))))

  ;; Remove a transaction, as a cleanup to call at the end of it when it's stable.
  ;; : Unit <- Address Nat
  remove-transaction:
  (lambda (user txsn)
    (action user
            (fun (remove-transaction get-state set-state! tx)
              (def state (get-state))
              (when (.call NatSet .elt? (.@ state ongoing-transactions) txsn)
                (let (new-state
                      (.call Lens .modify (slot-lens 'ongoing-transactions)
                             (cut .call NatSet .remove <> txsn) state))
                  (set-state! new-state)
                  (resume-transactions user new-state))))))

  ;; After we resume the activity, resume transactions
  ;; : @ <- Key State TX
  resume: =>
  (lambda (super)
    (fun (UserTransactionsTracker.resume user state tx)
      (begin0 (super user state tx)
        (action user (lambda (get-state _set-state! tx) (resume-transactions user (get-state)))))))

  ;; Add a transaction.
  ;; : TransactionTracker.Key TransactionTracker <- UserTransactionsTracker TransactionStatus
  add-transaction:
  (lambda (user transaction-status)
    (validate TransactionStatus transaction-status [[add-transaction: Address user]])
    (action
     user
     (fun (add-transaction get-state set-state! tx)
       (def state (get-state))
       (def txsn (.@ state transaction-counter))
       (def key {user txsn})
       (def (init . _) transaction-status)
       (def tracker (.call TransactionTracker make key init tx))
       (when (.call NatSet .empty? (.@ state ongoing-transactions))
         (.call TransactionTracker activate key))
       (def new-state
        (!> state
            (cut .call Lens .modify
                 (slot-lens 'ongoing-transactions) (cut .call NatSet .cons txsn <>) <>)
            (cut .call Lens .modify
                 (slot-lens 'transaction-counter) 1+ <>)))
       (set-state! new-state)
       (values key tracker)))))

;; TODO: do it transactionally. Reserve a ticket number first?
;; : TransactionTracker.Key TransactionTracker <- Address PreTransaction
(def (issue-pre-transaction pre)
  (def status (if (element? SignedTransactionInfo pre)
                (TransactionStatus-TxSigned (vector pre pre))
                (TransactionStatus-TxWanted pre)))
  (.call UserTransactionsTracker add-transaction (.@ pre from) status))

;; : Transaction SignedTransaction TransactionReceipt <- FinalTransactionStatus
(def (check-transaction-confirmed final-transaction-status)
  (match final-transaction-status
    ((TransactionStatus-TxConfirmed _) final-transaction-status)
    ((TransactionStatus-TxFailed _) (raise final-transaction-status))))

;; : Transaction SignedTransaction TransactionReceipt <- TransactionTracker
(def (track-transaction tracker)
  (check-transaction-confirmed (.call TransactionTracker wait tracker)))

;; : TransactionReceipt <- TransactionTracker
(def (receipt<-tracker tracker)
  (match (track-transaction tracker)
    ((TransactionStatus-TxConfirmed (vector _ _ TransactionReceipt))
     TransactionReceipt)))

;; : TransactionReceipt <- PreTransaction
(def (post-transaction pre-transaction)
  (defvalues (_key tracker) (issue-pre-transaction pre-transaction))
  (receipt<-tracker tracker))
