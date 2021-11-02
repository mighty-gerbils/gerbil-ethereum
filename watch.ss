;; General-purpose Ethereum Blockchain Watcher
(export #t)

(import
  :gerbil/gambit/bits :gerbil/gambit/threads
  :std/sugar :std/srfi/1
  :clan/concurrency :clan/exception
  :clan/poo/object :clan/poo/brace
  :clan/persist/persist
  ./types ./ethereum ./network-config ./json-rpc)

;; TODO: Handle "query returned more than 1000 results" when too many contracts created in interval!!!
;; TODO: Support watching multiple Ethereum-like networks in one image
;; Use "paging_options" https://explorer.energyweb.org/eth-rpc-api-docs -- available on geth???

;; Wait until at least target-block has been confirmed,
(def (wait-until-block target-block)
  (def current-block #f)
  (def (get-current-block!) (set! current-block (eth_blockNumber)))
  (get-current-block!)
  (while (< current-block target-block)
    (thread-sleep! (ethereum-block-polling-period-in-seconds))
    (get-current-block!))
  current-block) ; NOTE: due to polling intervals,
                 ; current-block might be after our target-block

;; Watch the contract until we either find logs or we reach past the to-block.
;; Return two values: a list of log entries found, and the last block that was scanned.
(def (watch-contract-step
      contract-address from-block to-block
      confirmations: (confirmations (ethereum-confirmations-wanted-in-blocks)))
  (if (<= from-block to-block)
    (let ()
      (def current-block (wait-until-block (+ from-block confirmations)))
      (def confirmed-block (- current-block confirmations))
      ;; TODO: correctly process timeouts and/or overly long lists
      (values (eth_getLogs {address: contract-address
                            fromBlock: from-block
                            toBlock: (min to-block confirmed-block)})
              confirmed-block))
    (values '() to-block)))

;; Watch all logs from a contract from a block to another (included),
;; and process the log events and watch state through a function f.
;; If some blocks are in the future, wait until they happen to return.
;; Function f may throw and/or use continuations to cause an early exit.
;; https://infura.io/docs/ethereum/json-rpc/eth-getLogs
;; Unit <- (<- WatchResult) Address Block Block Quantity confirmations: ? Block)
(def (watch-contract f contract-address from-block to-block next-event
                     confirmations: (confirmations (ethereum-confirmations-wanted-in-blocks)))
  (while (<= from-block to-block)
    (let ()
      ;; Determine blocks to watch
      (def current-block (wait-until-block (+ from-block confirmations)))
      (def confirmed-block (- current-block confirmations))
      (def end-block (min to-block confirmed-block))

      ;; Get logs
      (get-logs-from-blocks f contract-address from-block end-block next-event)

      (set! from-block (1+ confirmed-block)))))

(.def (WatchResult @ [] log next-event next-block)
      .make: (lambda (log: log
                      next-event: next-event ; next unprocessed event in next-block (0-indexed)
                      next-block: next-block ; block with unprocessed events
                      )
               {(log) (next-event) (next-block)}))

;; Process logs between indicated blocks,
;; starting from event of index next-event in the from-block up to the end of to-block (included)
;; Unit <- (<- WatchResult) Address Block Block Quantity
(def (get-logs-from-blocks f contract-address from-block to-block next-event)
  (get-logs-from-blocks/tc f contract-address from-block to-block to-block next-event))

;; Process logs between indicated blocks,
;; starting from event of index next-event in the from-block up to the end of to-block (included),
;; with a first request querying up to part-block only, in case there are too many logs in the response.
;; Unit <- (<- WatchResult) Address Block Block Block Quantity
(def (get-logs-from-blocks/tc f contract-address from-block part-block to-block next-event)
  (if (> from-block part-block)
    ;; Watch second part
    ;; TODO: triple check and maybe refactor this branch
    (and (< part-block to-block)
         (get-logs-from-blocks/tc f contract-address part-block to-block to-block 0))

    ;; Watch first part
    (let (logs (eth_getLogs {address: contract-address
                             fromBlock: from-block
                             toBlock: part-block}))
      (match logs
        ;; Recoverable errors - TODO Test this.
        ;; Too many logs or query timed out. Repartition to decrease request size.
        ;; See: https://infura.io/docs/ethereum/json-rpc/eth-getLogs#limitations
        ({error: {code: -32005}}
         (if (= from-block part-block)
           (error "Too many events in a single block!")
           (let (new-part-block (arithmetic-shift (+ from-block part-block) -1))
             (get-logs-from-blocks/tc f contract-address from-block new-part-block to-block next-event))))

        ;; Log(s) found
        ([l . ls]
         ;; Use next-event to get the start of unprocessed-logs
         (and (< next-event (length logs))
           (let ()
             ;; Get the latest event
             (defvalues (_ unprocessed-logs) (split-at logs next-event))
             (match unprocessed-logs
               ([latest-log . remaining-logs]

                ;; TODO: refactor the below
                ;; Compute next-block, next-event
                (def latest-block (.@ latest-log blockNumber))
                (def next-remaining-block
                  (match remaining-logs ([] #f) ([x . _] (.@ x blockNumber))))
                (defvalues (new-next-block new-next-event)
                  (if (eqv? next-remaining-block latest-block)
                    (values latest-block (1+ next-event)) ; Unprocessed logs in the latest-block
                    (values (1+ latest-block) 0))) ; Unprocessed logs in subsequent blocks

                ;; Call f with log
                (f (.call WatchResult .make
                     log: latest-log
                     next-block: new-next-block
                     next-event: new-next-event))
                (get-logs-from-blocks/tc f contract-address
                  new-next-block part-block to-block new-next-event))
               (else [])))))

        ;; Unrecoverable errors / no logs found -> skip
        ;; TODO Log these errors
        (else [])))))


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
