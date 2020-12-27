(export #t)

(import
  :gerbil/gambit/bits :gerbil/gambit/bytes
  :clan/base :clan/poo/poo (only-in :clan/poo/mop) :clan/poo/io
  ./hex ./types ./signing ./known-addresses ./ethereum ./logger
  ./assembly ./transaction ./tx-tracker ./contract-config ./contract-runtime)

;;; EVM Contract for batch calls.
;;
;;  This contract generalizes the batch-send contract, allowing for arbitrary contract calls.
;;
;;  Instead of sending a series of N transactions, have a single transaction that does it all.
;;  The advantage is that you have only one transaction to nurse to completion,
;;  which is a much less complex thing to do, with fewer and simpler failure scenarios to handle.
;;  This is especially important if your transaction posting code needs to deal with
;;  FOMO3D-style block-buying attacks, or other rapid gas price increase events.
;;
;;  The input data format does not use the Solidity ABI; we use a simpler and cheaper style:
;;  Just a raw vector of 32-byte records each comprised of a 20-byte address and a 12-byte value,
;;  then a 2-byte message length, then as many specified bytes, to pass to the contract being called.
;;  (use length 0 for a simple send).
;;
;;  No 4-byte header to identify a "function" to call; there's only one "function".
;;  No 32-byte vector count as first implicit argument; the size is taken from CALLDATASIZE.
;;  We copy the message to memory so we can send it to the contract, to a buffer starting at offset 0.
;;
;;  We do NOT check a contract owner (maybe we should?). Be careful not to leave money in the contract,
;;  and be careful not to depend on the contract always having the same owner
;;  (or else, add an owner check).
;;
;;  If any single call in the batch fails (because it REVERTs, or lacks funds or gas, etc.),
;;  revert the entire transaction (which cancels all the transfers that previously succeeded,
;;  but still includes the transaction as "failed" on the blockchain, and transfers the gas
;;  from the transaction sender to the miners).

;; Create the runtime code for a batch contract associated to given owner
;; : Bytes <- Address
(def (batch-call-contract-runtime)
  (assemble/bytes
   [;; At instruction 0, so push 0 on stack while it's extra cheap!
    ;; a non-zero contract byte costs 220, a zero one only costs 204, so the GETPC optimization
    ;; is worth it if we actually use that 0 at least twice in the source code.
    GETPC #|0|# ; -- 0
    1 (arithmetic-shift 1 96) DUP2 #|1|# DUP2 #|2**96|# SUB CALLDATASIZE DUP5
    ;; -- 0 size 2**96-1 2**96 1 0
    [&jump1 'loop-entry] ;; jump to entry, skipping inter-loop action

    ;; The loop: inter-loop action
    [&jumpdest 'loop] ;; -- msgstart msgwidth cursor totalsize 2**96-1 2**96 1 0
    ADD SWAP1 POP ;; cursor totalsize 2**96-1 2**96 1 0

    ;; The entry point of the loop: check condition
    [&jumpdest 'loop-entry] ;; -- cursor size 2**96-1 2**96 1 0
    ;; If less then continue to loop-body, else return
    DUP2 #|size|# DUP2 #|cursor|# LT [&jumpi1 'loop-body] STOP

    ;; Loop body: take the next 256-bit argument.
    ;; Top 160 are address, lower 96 are value in wei.
    ;; Prepare the arguments to a transfer call.
    [&jumpdest 'loop-body] ;; -- cursor size 2**96-1 2**96 1 0
    DUP1 #|cursor|# 32 ADD CALLDATALOAD (&shr 240) #|msgwidth|#
    ;; -- msgwidth cursor totalsize 2**96-1 2**96 1 0
    DUP2 #|cursor|# 34 ADD #|msgstart = cursor + 34|#
    ;; -- msgstart msgwidth cursor totalsize 2**96-1 2**96 1 0
    DUP2 #|msgwidth|# DUP2 #|msgstart|# DUP10 #|0|# CALLDATACOPY
    ;; -- msgstart msgwidth cursor totalsize 2**96-1 2**96 1 0
    DUP8 #|0|# DUP1 #|0|# DUP4 #|msgwidth|# DUP2 #|0|# DUP7 #|cursor|# CALLDATALOAD
    ;; -- addr+value 0 msgwidth 0 0 msgstart msgwidth cursor totalsize 2**96-1 2**96 1 0
    DUP11 #|2**96|# DUP2 #|addr+value|# DUP12 #|2**96-1|# AND
    ;; -- value 2**96 data 0 msgwidth 0 0 msgstart msgwidth cursor totalsize 2**96-1 2**96 1 0
    SWAP2 #|data 2**96 value|# DIV #|address|#
    ;; -- address value 0 #|argstart|# msgwidth 0 #|retstart|# 0 #|retwidth|# msgstart msgwidth cursor totalsize 2**96-1 2**96 1 0
    GAS CALL
    ;; -- success? msgstart msgwidth cursor totalsize 2**96-1 2**96 1 0
    ;; loop if successful, revert everything if failed.
    [&jumpi1 'loop]
    ;; -- msgstart msgwidth cursor totalsize 2**96-1 2**96 1 0
    DUP8 DUP1 REVERT]))

;; Create the runtime code for a batch contract associated to given owner
(def batch-call-contract-init (rcompose batch-call-contract-runtime stateless-contract-init))

;; Ensure that there is a batch transfer contract associated with the owner
;; on the blockchain and saved to the working database, and
;; return the ContractConfig for that contract.
(def (ensure-batch-call-contract creator)
  (def config (ensure-contract-config/db
               (string->bytes "batch-call-contract")
               (create-contract creator (batch-call-contract-init))))
  (eth-log ['ensure-batch-call-contract (0x<-address creator) (nickname<-address creator)
                                        '=> (json<- ContractConfig config)])
  config)

(def (marshal-batch-calls calls port)
  (for-each (match <> ([address amount calldata]
                       (marshal Address address port)
                       (marshal UInt96 amount port)
                       (marshal BytesL16 calldata port)))
            calls))

;; : <- Address (Listof (TupleList Address UInt96 BytesL16))
(def (batch-call caller calls)
  (def data (call-with-output-u8vector (cut marshal-batch-calls calls <>)))
  (def value (foldl + 0 (map cadr calls)))
  (def transfer-description
    (match <> ([address amount calldata]
               [(0x<-address address) (nickname<-address address)
                (decimal-string-ether<-wei amount) (0x<-bytes calldata)])))
  (when (< 0 value)
    (eth-log ["batch calls" "total-value:" value
              "calls:" (map transfer-description calls)
              "data:" (0x<-bytes data)])
    (let (contract (.@ (ensure-batch-call-contract caller) contract-address))
      (post-transaction (call-function caller contract data value: value gas: 4000000)))))

;; Trivial contract that logs all its call data with LOG0 then commits the transaction with success.
;; Useful for testing, and not much else --- maybe move it to t/batch-call-integrationtest.ss
(def (trivial-logger-contract-runtime)
  (assemble/bytes
   [GETPC #|0|# CALLDATASIZE DUP2 #|0|# DUP1 #|0|# CALLDATACOPY CALLDATASIZE SWAP1 #|0 size|# LOG0 STOP]))
(def trivial-logger-contract-init (rcompose trivial-logger-contract-runtime stateless-contract-init))
(def (ensure-trivial-logger-contract creator)
  (def config (ensure-contract-config/db
               (string->bytes "trivial-logger-contract")
               (create-contract creator (trivial-logger-contract-init))))
  (eth-log ['ensure-trivial-logger-contract (0x<-address creator) (nickname<-address creator)
                                            '=> (json<- ContractConfig config)])
  config)
