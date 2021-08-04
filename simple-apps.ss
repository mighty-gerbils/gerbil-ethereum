;;; Simple Apps on top the EVM:
;;; - Transaction Batching, with or without a batching contract
;;; - Trivial wrapper for CREATE2 only, so
;;; - Trivial logger, useful for debugging contracts that call other contracts.

(export #t)

(import
  :gerbil/gambit/bits :gerbil/gambit/bytes
  :std/iter :std/misc/list
  :clan/base :clan/syntax
  :clan/poo/object (only-in :clan/poo/mop) :clan/poo/io
  :clan/crypto/secp256k1
  ./logger ./hex ./types ./ethereum ./known-addresses
  ./assembly ./transaction ./tx-tracker ./contract-config ./evm-runtime)

;;; EVM Contract for batch transactions.
;;
;;  This contract allows for arbitrary transactions of the following type:
;;  - simple transfers
;;  - arbitrary contract calls
;;  - contract creations with CREATE2
;;
;;  Why use this contract?
;;  1. Instead of sending a long series of N transactions, you can have a single
;;     transaction that does it all, so you only have one transaction at a time
;;     to nurture to completion as fully confirmed on the blockchain.
;;     This is a much less complex thing to do than nurturing N transactions,
;;     with fewer and simpler failure scenarios to handle.
;;     Reducing this complexity is especially important if you run applications
;;     that need to survive FOMO3D-style block-buying attacks,
;;     MEV-capturing robots, or other rapid gas price increase events.
;;     Furthermore, *if* physical security makes signing an expensive event
;;     e.g. involving airgapped hardware wallets, then this can also minimize
;;     the number of transactions to sign with a critical key.
;;
;;  Also, the contract exists in two variants, owned or unowned, and then
;;  2.a. IF the contract is *owned*, then only the owner can post transactions
;;       (we check at the beginning), at which point the contract can also
;;       be used as the recipient of transactions (either ETH or ERC20).
;;       *Furthermore*, transaction authorization by the owner can be checked
;;       via signature rather than directly by the blockchain, which allows for
;;       a secondary transaction posting market, e.g. using a ERC20 for GAS.
;;
;;  2.b. IF the contract is *unowned*, then it makes for a participant-neutral
;;       universal basis for CREATE2 contracts that any of the concerned parties
;;       can create and reuse in a huge pool.
;;       *Furthermore*, if the creator is a global constant across chains, then
;;       contract addresses can become global constants across chains, too.
;;
;;  Finally:
;;  3. The contract groups together these transactions in a master transaction,
;;     so they will all succeed or all fail together.
;;     The contract also eliminates race conditions with rival transactions
;;     from other users in-between the sequence of grouped transactions.
;;     This can be important when doing complex financial trades.
;;     Note that in case of failure, the transaction is included as "failed"
;;     on the blockchain, and the gas is still paid to the miners.
;;
;;  How to call this contract?
;;
;;  The input data format does not use the Solidity ABI. Instead we use a
;;  simpler and cheaper style, with just a raw vector of bytes,
;;  containing "virtual instructions":
;;  - Byte value 0 (instruction &transfer),
;;    followed by 20-byte address, followed by 11-byte value,
;;    for transfering up to 309M ethers at once (more than the foreseeable ETH supply for years).
;;  - Byte value 1 (instruction &call),
;;    followed by 20-byte address, followed by 11-byte value, followed by 2-byte length,
;;    followed by message of given length,
;;    to CALL a contract with same value limit and message.
;;  - Byte value 2 (instruction &delegatecall),
;;    followed by 20-byte address, followed by 2-byte length, followed by code of given length,
;;    to DELEGATECALL a contract.
;;  - Byte value 3 (instruction &create),
;;    followed by 11-byte value, followed by 2-byte length, followed by code of given length,
;;    to create a contract with CREATE.
;;  - Byte value 4 (instruction &create2),
;;    followed by 11-byte value, followed by 2-byte length, followed by 32-byte salt,
;;    followed by code of given length,
;;    to create a contract with CREATE2.
;;
;;  There is no 4-byte header to identify a "function" to call; there's only one "function".
;;  There is no 32-byte vector count as first implicit argument; the size is taken from CALLDATASIZE.
;;  We copy the message to memory so we can send it to the contract, to a buffer starting at offset 0.
;;  Do we need a variant that follows the Solidity ABI? No.
;;  This contract is supposed to be called by off-chain programs only,
;;  not by other on-chain contracts. We can just provide off-chain APIs for client languages.

;; Runtime code for a batch contract associated with given owner.
;; : Bytes <- (OrFalse Address)
(def (batch-contract-runtime (owner #f))
  (assemble/bytes
   [;; At instruction 0, so push 0 on stack while it's extra cheap!
    ;; a non-zero contract byte costs 220, a zero one only costs 204, so the GETPC optimization
    ;; is worth it if we actually use that 0 at least twice in the source code.
    GETPC #|0|# 32 ;; -- $CONSTANTS = 32 0
    CALLDATASIZE DUP3 #|0|# DUP1 #|0|#
    ;; -- 0 0 size $CONSTANTS
    (if owner
      (&begin owner CALLER EQ [&jumpi1 'loop] ;; jump to loop entry
              ;; Authorization via signature, with hash of previous tx stored at 0
              ;; as nonce against replay attacks.
              DUP3 #|size|# DUP2 #|0|# 64 CALLDATACOPY ;; copy message to memory
              97 MLOAD 255 AND DUP5 #|32|# MSTORE ;; store v
              DUP3 #|size|# 129 SUB DUP2 129 SHA3 DUP2 #|0|# MSTORE ;; store digest
              DUP1 #|0|# DUP1 #|0|# 128 DUP2 #|0|# 1 GAS STATICCALL ;; call ecrecover
              ;; -- success? 0 size $CONSTANTS
              97 SWAP2 #|0<->97|# MLOAD owner EQ AND ;; owner? 97 0 size $CONSTANTS
              DUP3 #|0|# SLOAD 65 MLOAD DUP1 #|hash|# SSTORE EQ AND ;; check that the hash matches
              [&jumpi1 'loop])
      (&begin [&jump1 'loop]))
    (&define-abort-contract-call) ;; abort if the above failed

    [&jumpdest '&transfer] ;; -- topword cursor size $CONSTANTS
    DUP4 #|32|# SWAP1 #|topword<->32|# DUP6 #|0|# DUP1 #|0|# ;; -- 0 0 topword 32 cursor size $CONSTANTS

    [&jumpdest '&call0] ;; common between &transfer and &call
    ;; where: newcursor == cursor0 + cursor1, 0=retstart=retwidth=argstart
    ;; -- argwidth 0 topword cursor0 cursor1 size $CONSTANTS
    DUP2 #|0|# DUP4 #|topword|# (1- (expt 2 88)) #|2**88-1|# AND
    ;; -- value 0 argwidth 0 topword cursor0 cursor1 size $CONSTANTS
    DUP2 #|0|# SWAP5 #|topword<->0|# (&shl 8) (&shr 96) GAS
    ;; -- gas address value 0 argwidth 0 0 cursor0 cursor1 size $CONSTANTS
    CALL &require!
    ;; fallthrough to 'loop

    ;; The entry point of the loop: check condition
    [&jumpdest 'loop] ;; -- cursor0 cursor1 size 2**96-1 2**96 1 0
    ADD
    ;; If less then continue to loop-body, else return
    DUP2 #|size|# DUP2 #|cursor|# LT [&jumpi1 'loop-body] STOP

    ;; Loop body: take the next 256-bit argument.
    ;; Top 160 are address, lower 96 are value in wei.
    ;; Prepare the arguments to a transfer call.
    [&jumpdest 'loop-body] ;; -- cursor size $CONSTANTS
    DUP1 CALLDATALOAD
    ;; Push a vector mapping virtual instruction numbers to EVM instruction counters
    PUSH10 [&fixup 80 `(+ (* ,(expt 2  0) &transfer)
                          (* ,(expt 2 16) &call)
                          (* ,(expt 2 32) &delegatecall)
                          (* ,(expt 2 48) &create)
                          (* ,(expt 2 64) &create2))]
    DUP2 (&shr 244) 48 AND SHR 65535 AND JUMP ;; extract the address of the virtual instruction
    ;; -- topword cursor size $CONSTANTS

    [&jumpdest '&call] ;; -- topword cursor size $CONSTANTS
    DUP2 #|cursor|# DUP5 #|32|# ADD CALLDATALOAD (&shr 240)
    ;; -- msgwidth topword cursor size $CONSTANTS
    SWAP2 #|cursor<->msgwidth|# 34 ADD #|msgstart = cursor + 34|#
    ;; -- msgstart topword msgwidth size $CONSTANTS
    SWAP1 ;; -- topword msgstart msgwidth size $CONSTANTS
    DUP3 #|msgwidth|# DUP3 #|msgstart|# DUP8 #|0|# CALLDATACOPY ;; copy message
    ;; -- topword msgstart msgwidth size $CONSTANTS
    DUP6 #|0|# DUP4 #|msgwidth|# ;; -- msgwidth 0 topword cursor0 cursor1 size $CONSTANTS
    [&jump1 '&call0] ;; jump to code shared with &transfer

    [&jumpdest '&delegatecall] ;; -- topword cursor size $CONSTANTS
    DUP1 #|topword|# (&shl 168) (&shr 240) ;; -- msgwidth
    ;; -- msgwidth topword cursor size $CONSTANTS
    SWAP2 #|cursor<->msgwidth|# 23 ADD #|msgstart = cursor + 23|#
    ;; -- msgstart topword msgwidth size $CONSTANTS
    SWAP1 ;; -- topword msgstart msgwidth size $CONSTANTS
    DUP3 #|msgwidth|# DUP3 #|msgstart|# DUP8 #|0|# CALLDATACOPY ;; copy message
    ;; -- topword msgstart msgwidth size $CONSTANTS
    DUP6 #|0|# DUP4 #|msgwidth|# ;; -- msgwidth 0 topword cursor0 cursor1 size $CONSTANTS
    ;; where: newcursor == cursor0 + cursor1, 0=retstart=retwidth=argstart
    ;; -- argwidth 0 topword cursor0 cursor1 size $CONSTANTS
    DUP2 #|0|# DUP1 #|0|# SWAP4 #|topword<->0|# (&shl 8) (&shr 96) GAS
    ;; -- gas address 0 argwidth 0 0 cursor0 cursor1 size $CONSTANTS
    DELEGATECALL [&jumpi1 'loop] [&jump1 'abort-contract-call]

    [&jumpdest '&create] ;; -- topword cursor size $CONSTANTS
    DUP1 #|topword|# (&shl 96) (&shr 240) #|msgwidth|# ;; -- msgwidth
    ;; -- msgwidth topword cursor size $CONSTANTS
    SWAP2 #|cursor<->msgwidth|#
    ;; -- cursor topword msgwidth size $CONSTANT
    12 ADD #|msgstart|# ;; -- msgstart
    ;; -- msgstart topword msgwidth size $CONSTANT
    SWAP1 #|topword<->msgstart|#
    ;; -- topword msgstart msgwidth size $CONSTANT
    DUP3 #|msgwidth|# DUP3 #|msgstart|# DUP8 #|0|# CALLDATACOPY ;; copy message
    ;; -- topword msgstart msgwidth size $CONSTANT
    DUP3 #|msgwidth|# DUP7 #|0|#
    ;; -- msgstart 0 msgwidth topword msgwidth size $CONSTANT
    SWAP3 #|topword<->msgstart|#
    ;; -- topword 0 msgwidth msgstart msgwidth size $CONSTANT
    (&shl 8) (&shr 168) ;; -- value 0 msgwidth msgstart msgwidth size $CONSTANT
    CREATE POP ;; TODO: does CREATE return 0 on failure?
    ;; -- msgstart msgwidth size $CONSTANTS
    [&jump1 'loop]

    [&jumpdest '&create2] ;; -- topword cursor size $CONSTANTS
    DUP1 #|topword|# (&shl 96) (&shr 240) #|msgwidth|# SWAP2 #|cursor<->msgwidth|#
    ;; -- cursor topword msgwidth size $CONSTANT
    46 ADD #|msgstart|# SWAP1 #|topword<->msgstart|#
    ;; -- topword msgstart msgwidth size $CONSTANT
    DUP3 #|msgwidth|# DUP3 #|msgstart|# DUP8 #|0|# CALLDATACOPY ;; copy message
    DUP3 #|msgwidth|# DUP7 #|0|#
    ;; -- 0 msgwidth topword msgstart msgwidth size $CONSTANT
    DUP4 #|msgstart|# DUP8 #|32|# SUB CALLDATALOAD SWAP3 #|topword<->salt|#
    ;; -- topword 0 msgwidth salt msgstart msgwidth size $CONSTANT
    (&shl 8) (&shr 168) ;; -- value 0 msgwidth salt msgstart msgwidth size $CONSTANT
    CREATE2 POP ;; TODO: does CREATE return 0 on failure?
    ;; -- msgstart msgwidth size $CONSTANTS
    [&jump1 'loop]]))

;; Create the runtime code for a batch contract associated to given owner
;; : Bytes <- (OrFalse Address)
(def batch-contract-init (rcompose batch-contract-runtime stateless-contract-init))

;; Ensure that there is a batch transfer contract associated with the owner
;; on the blockchain and saved to the working database, and
;; return the ContractConfig for that contract.
;; : ContractConfig <- Address owner:?(OrFalse Address) log:?(Fun Unit <- Json)
(def (ensure-batch-contract creator owner: (owner creator) log: (log eth-log))
  (def config (ensure-contract-config/db
               (apply bytes-append (string->bytes "batch:")
                      (when/list owner [(bytes<- Address owner)]))
               (create-contract owner (batch-contract-init owner))
               log: log))
  (log ['ensure-batch-contract (0x<-address owner) (nickname<-address owner)
                               '=> (json<- ContractConfig config)])
  config)

(defstruct batched-transaction (value) transparent: #t)
(defstruct (batched-transfer batched-transaction) (to) transparent: #t)
(defstruct (batched-call batched-transaction) (to data) transparent: #t)
(defstruct (batched-delegate-call batched-transaction) (to data) transparent: #t)
(defstruct (batched-create batched-transaction) (initcode) transparent: #t)
(defstruct (batched-create2 batched-transaction) (initcode salt) transparent: #t)
;; NOTE: for the delegate-call, the value spent as part of the call is NOT
;; explicitly passed as part of the on-chain call, yet must still be accounted
;; for by the off-chain code that computes the amounts to be transfered.

;; JSON description for a batched-transaction
;; : Json <- BatchedTransaction
(def batched-transaction-description
  (match <>
    ((batched-transfer value to)
     [(decimal-string-ether<-wei value) (0x<-address to) (nickname<-address to)])
    ((batched-call value to data)
     [(decimal-string-ether<-wei value) (0x<-address to) (nickname<-address to) (0x<-bytes data)])
    ((batched-delegate-call value to data)
     [(decimal-string-ether<-wei value) (0x<-address to) (nickname<-address to) (0x<-bytes data)])
    ((batched-create value initcode)
     [(decimal-string-ether<-wei value) (0x<-bytes initcode)])
    ((batched-create2 value initcode salt)
     [(decimal-string-ether<-wei value) (0x<-bytes initcode) (0x<-bytes salt)])))

;; Marshal a batched tx for use with a batch contract
;; : Unit <- BatchedTransaction OutPort
(def (marshal-batched-transaction tx port)
  (def (m.address address) (marshal Address (validate Address address) port))
  (def (m.value value) (marshal UInt88 (validate UInt88 value) port))
  (def (m.bytes-length bytes) (marshal UInt16 (bytes-length (validate BytesL16 bytes)) port))
  (def (m.bytes bytes) (write-bytes bytes port))
  (match tx
    ((batched-transfer value to)
     (write-byte 0 port) (m.address to) (m.value value))
    ((batched-call value to data)
     (write-byte 1 port) (m.address to) (m.value value) (m.bytes-length data) (m.bytes data))
    ((batched-delegate-call value to data)
     (write-byte 2 port) (m.address to) (m.bytes-length data) (m.bytes data))
    ((batched-create value initcode)
     (write-byte 3 port) (m.value value)
     (m.bytes-length initcode) (m.bytes initcode))
    ((batched-create2 value initcode salt)
     (write-byte 4 port) (m.value value) (m.bytes-length initcode)
     (m.bytes (validate Bytes32 salt)) (m.bytes initcode))))

;; Marshal a list of batched tx for use with a batch contract
;; : Bytes <- (Listof BatchedTransaction)
(def (bytes<-batched-transactions txs)
  (call-with-output-u8vector
   (lambda (port) (for-each (cut marshal-batched-transaction <> port) txs))))


;; Marshal a list of batched tx for use with a batch contract
;; : Unit <- (Listof BatchedTransaction) OutPort
(def (bytes<-batched-transactions/signed address txs)
  (def b (bytes<-batched-transactions txs))
  (def sig (make-signature Bytes (secret-key<-address address) b))
  (bytes-append (bytes<- Signature sig) b))

;; EVM code for a batched tx for use *without* a batch contract
;; : Directive <- BatchedTransaction UInt16
(def (batched-transaction-code tx n)
  (def label (symbolify "data" n))
  (match tx ;; 0 <-- 0
    ((batched-transfer value to)
     (&begin DUP1 DUP1 DUP1 DUP1 value to GAS
             (&unless CALL (&begin DUP1 DUP1 REVERT))))
    ((batched-call value to data)
     (&begin DUP1 DUP1 (bytes-length data)
             DUP1 [&push-label2 label] DUP4 CODECOPY
             DUP2 value to GAS
             (&unless CALL (&begin DUP1 DUP1 REVERT))))
    ((batched-create value initcode)
     (&begin (bytes-length initcode)
             DUP1 [&push-label2 label] DUP4 CODECOPY
             DUP2 value CREATE POP)) ;; TODO: does CREATE return 0 on failure?
    ((batched-create2 value initcode salt)
     (&begin [&push-bytes salt] (bytes-length initcode)
             DUP1 [&push-label2 label] DUP5 CODECOPY
             DUP3 value CREATE2 POP)))) ;; TODO: does CREATE2 return 0 on failure?

;; EVM ancillary data for a batched tx for use *without* a batch contract
;; : Directive <- BatchedTransaction UInt16
(def (batched-transaction-data tx n)
  (def label (symbolify "data" n))
  (match tx
    ((batched-transfer value to)
     (&begin))
    ((batched-call value to data)
     (&begin [&label label] [&bytes data]))
    ((batched-create value initcode)
     (&begin [&label label] [&bytes initcode]))
    ((batched-create2 value initcode salt)
     (&begin [&label label] [&bytes initcode]))))

;; EVM code for a list of batched tx for use *without* a batch contract
;; TODO: choose optimal backend based on number of calls of each type!
;; : Bytes <- (Listof BatchedTransaction)
(def (assemble-batched-transactions txs)
  (assemble/bytes
   (&begin GETPC ;; -- 0 ;; initialize 0
           (&begin* (for/collect ((tx txs) (i (in-naturals))) (batched-transaction-code tx i)))
           STOP
           (&begin* (for/collect ((tx txs) (i (in-naturals))) (batched-transaction-data tx i))))))

;; Batching many transactions in a single one.
;; This is useful to minimize the complexity of watching many transactions to completion;
;; also useful to minimize opportunities for race conditions between transactions in a given set.
;; batch-contract can specify the address of a contract (we suppose we are the owner, or there is none),
;; : <- Address (Listof BatchedTransaction) log:?(<- Json) batch-contract:(Or Address Bool)
(def (batch-txs caller txs log: (log eth-log) batch-contract: (batch-contract #f) gas: (gas (void)))
  (def value (foldl + 0 (map batched-transaction-value txs)))
  (when (eq? batch-contract #t)
    (set! batch-contract (.@ (ensure-batch-contract caller log: log) contract-address)))
  (when (< 0 value)
    (eth-log ["batch" "total-value:" value
              (when/list batch-contract ["batch-contract" (0x<-address batch-contract)])
              "txs:" (map batched-transaction-description txs)])
    (let (tx (if batch-contract
               (let (data (bytes<-batched-transactions txs))
                 (call-function caller batch-contract data value: value gas: gas))
               (let (data (assemble-batched-transactions txs))
                 (create-contract caller data value: value gas: gas))))
      (post-transaction tx))))

;; Trivial contract that logs all its call data with LOG1 (topic: the caller).
;; Useful for testing, and not much else --- maybe move it to testing.ss ?
;; : Bytes <-
(def (trivial-logger-runtime)
  (assemble/bytes
   [GETPC #|0|# ;; -- 0
    CALLDATASIZE DUP2 #|0|# DUP1 #|0|# CALLDATACOPY ;; -- 0
    CALLER CALLDATASIZE DUP3 #|0|# LOG1 ;; -- 0
    STOP]))

;; : Bytes <-
(def trivial-logger-init (rcompose trivial-logger-runtime stateless-contract-init))

;; : Bytes <-
(def (ensure-trivial-logger-contract owner log: (log eth-log))
  (def config (ensure-contract-config/db
               (string->bytes "trivial-logger-contract")
               (create-contract owner (trivial-logger-init))
               log: log))
  (log ['ensure-trivial-logger-contract (0x<-address owner) (nickname<-address owner)
                                        '=> (json<- ContractConfig config)])
  config)

;; Trivial CREATE2 wrapper runtime
;; : Bytes <-
(def (create2-wrapper-runtime)
  (assemble/bytes
   [GETPC ;; -- 0
    DUP1 CALLDATALOAD ;; -- salt 0
    32 CALLDATASIZE SUB ;; -- size salt 0
    DUP1 #|size|# 32 #|codestart|# DUP5 #|memstart==0|# CALLDATACOPY ;; -- size salt 0
    DUP3 #|0|# CALLVALUE CREATE2
    STOP]))

;; Trivial CREATE2 wrapper initcode
;; : Bytes <-
(def (create2-wrapper-init)
  (assemble/bytes (&trivial-contract-init (create2-wrapper-runtime))))

;; Ensure that there is a batch transfer contract associated with the owner
;; on the blockchain and saved to the working database, and
;; return the ContractConfig for that contract.
(def (ensure-create2-wrapper creator log: (log eth-log))
  (def config (ensure-contract-config/db
               (string->bytes "create2-contract")
               (create-contract creator (create2-wrapper-init))
               log: log))
  (log ['ensure-create2-wrapper
        (0x<-address creator) (nickname<-address creator)
        '=> (json<- ContractConfig config)])
  config)
