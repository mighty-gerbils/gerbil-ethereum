;; General-purpose Ethereum Blockchain Watcher
(export #t)

(import
  :gerbil/gambit
  :std/misc/number :std/net/json-rpc :std/sugar :std/srfi/1
  :clan/concurrency :clan/exception :clan/failure :clan/option :clan/timestamp
  :clan/poo/object :clan/poo/brace
  :clan/persist/persist
  ./types ./ethereum ./network-config ./json-rpc)

;; TODO: Handle "query returned more than 1000 results" when too many contracts created in interval!!!
;; TODO: Support watching multiple Ethereum-like networks in one image
;; Use "paging_options" https://explorer.energyweb.org/eth-rpc-api-docs -- available on geth???

;; Wait until at least target-block has been confirmed,
;; NB: due to polling intervals, current-block might be after our target-block
;; : BlockNumber <- BlockNumber
(def (wait-until-block target-block)
  (let loop ()
    (def current-block (eth_blockNumber))
    (unless (>= current-block target-block)
      (thread-sleep! (ethereum-block-polling-period-in-seconds))
      (loop))
    current-block))

;; Wait until the unix-timestamp for a block is greater or equal to given target.
;; : BlockNumber <- UnixTimestamp
(def (wait-until-block-unix-timestamp target-unix-timestamp)
  (let loop ()
    (def current-block (eth_blockNumber))
    (def polling-period (ethereum-block-polling-period-in-seconds))
    (unless (>= target-unix-timestamp
                (.@ (eth_getBlockByNumber current-block #f) timestamp))
      (thread-sleep! (max polling-period
                          (- target-unix-timestamp
                             (current-unix-timestamp) polling-period 1)))
      (loop))))

;; Have function f process in chronological order all log entries from contract at contract-address
;; from from-block to to-block (included), starting at the (next-event)th event in from-block.
;; If some blocks are in the future, wait until they happen to return.
;; Only process a block after a sufficient number of confirmations have passed.
;; Function f may throw and/or use continuations to cause an early exit.
;; https://infura.io/docs/ethereum/json-rpc/eth-getLogs
;; : <- (Fun <- LogObject) Address BlockNumber BlockNumber Nat confirmations: ?Nat
(def (watch-contract f contract-address from-block to-block (next-event 0)
                     confirmations: (confirmations (ethereum-confirmations-wanted-in-blocks)))
  (let loop ((start-block from-block) (next-event next-event))
    (when (<= start-block to-block)
      (let* ((current-block (wait-until-block (+ start-block confirmations)))
             (confirmed-block (- current-block confirmations))
             (end-block (min to-block confirmed-block)))
        ;; Get logs
        (get-logs-from-blocks f contract-address start-block end-block next-event)
        (loop (1+ end-block) 0)))))

;; Request logs between indicated blocks (inclusive).
;; Gets all logs between from-block and to-block (or starting a part-block)
;; Re-partitioning the getLogs request if we timeout / have too many logs in the repsonse.
;; : (Fun <- LogObject) (Maybe Address) BlockNumber BlockNumber ?Nat
(def (get-logs-from-blocks f contract-address
                           from-block to-block (next-event 0))
  (when (>= to-block from-block)
    (match (with-result (eth_getLogs {address: contract-address
                                      fromBlock: from-block
                                      toBlock: to-block}))
      ((some l)
       (for-each (lambda (o)
                   (unless (and (= from-block (.@ o blockNumber)) (< (.@ o logIndex) next-event))
                     (f o)))
                 l))
      ((failure err)
       (if (and (json-rpc-error? err) (equal? (json-rpc-error-code err) -32005)
                (< from-block to-block))
         ;; Recoverable error: Too many logs or query timed out.
         ;; Recovery strategy: Repartition to decrease request size.
         ;; See: https://infura.io/docs/ethereum/json-rpc/eth-getLogs#limitations
         ;; TODO: Test this.
         (let (part-block (half (+ from-block to-block)))
           (get-logs-from-blocks f contract-address from-block part-block next-event)
           (get-logs-from-blocks f contract-address (1+ part-block) to-block 0))
         ;; Unrecoverable errors: raise error
         (raise err)))))) ;; Too many logs in the same block. TODO: Add contract-address from-block ?


#| ;;; The code below was hand-translated from my previous CPS-based client JavaScript.
   ;;; It is untested and most probably needs to be rewritten / refactored in more direct style
   ;;; with more transactional persistence.

;; : Quantity
(define-persistent-variable next-unprocessed-block Nat "ETH.nextUnprocessedBlock" 0)

;; : (Table (Fun <- Quantity) <- String)
(def new-block-hooks (make-hash-table))

;; Process new blocks.
;; : Unit <-
(def (process-new-blocks)
  (def current-block (eth_blockNumber))
  (with-logged-exceptions ()
   (when (>= current-block (next-unprocessed-block))
     (hash-for-each (lambda (_ hook) (hook (next-unprocessed-block) current-block)) new-block-hooks)
     (set! (next-unprocessed-block) (1+ current-block)))))

;; : Unit <-
(def (watchBlockchain)
  (spawn/name/logged
   "watch-ethereum-blockchain"
   (while #t
     (process-new-blocks)
     (thread-sleep! (ethereum-block-polling-period-in-seconds)))))

;; hook to synchronously watch all events of some kind as the chain keeps getting updated
(def (process-events filter process)
  (lambda (fromBlock toBlock)
    (set! fromBlock (max fromBlock 0))
    (when (or (not (real? toBlock)) (<= fromBlock toBlock))
      (for-each process (eth_getLogs (.cc filter fromBlock: fromBlock toBlock: toBlock))))))

;; Register a confirmed event hook.
;; NB: *IF* some event has hooks for both confirmed and unconfirmed events, then
;; (1) the confirmed event hook must be registered *before*, and
;; (2) the name of the confirmed event hook must be lexicographically strictly less
;; than the name for the corresponding unconfirmed event hook.
(def (register-confirmed-event-hook
      name fromBlock filter process
      (confirmations (ethereum-confirmations-wanted-in-blocks)))
  (hash-put! new-block-hooks name
             (lambda (first-unprocessed-block last-unprocessed-block)
               ((process-events filter process)
                (- first-unprocessed-block confirmations)
                (- last-unprocessed-block confirmations))))
  ((process-events filter process) fromBlock (1- (next-unprocessed-block))))

(def (register-unconfirmed-event-hook
      name filter process
      (confirmations (ethereum-confirmations-wanted-in-blocks)))
  (def hook (lambda (first-unprocessed-block last-unprocessed-block)
              ((process-events filter process)
               (max first-unprocessed-block (- last-unprocessed-block confirmations -1))
               'latest)))
  (hash-put! new-block-hooks name hook)
  (def fromBlock (- (next-unprocessed-block) 1 confirmations))
  (hook fromBlock 'latest))

;; The code in the section below might belong to some library to manage multiple interactions.
;; Managing interactions

;; runs up from 0, in the table of interactions
(define-persistent-variable next-interaction-id Integer "ETH.nextInteractionId" 0)
;; runs down from -1, in the same table of interactions
(define-persistent-variable previous-unconfirmed-id Integer "ETH.previousUnconfirmedId" -1)
(define-persistent-variable active-interactions NatSet "ETH.activeInteractions" (.@ NatSet .empty))
(define-type MapNatFromDigest (Map Nat <- Digest))
(define-persistent-variable interactions-by-txhash MapNatFromDigest "ETH.interactionsByTxhash" (.@ MapNatFromDigest .empty))
|#
