(export #t)

(import
  :gerbil/gambit/bits :gerbil/gambit/bytes
  :clan/base :clan/poo/poo (only-in :clan/poo/mop) :clan/poo/io
  ./hex ./types ./signing ./known-addresses ./ethereum ./logger
  ./assembly ./transaction ./tx-tracker ./contract-config ./contract-runtime)

;;; EVM Contract for batch transfers.
;;
;;  Instead of sending a series of N transactions, have a single transaction that does it all.
;;  The advantage is that you have only one transaction to nurse to completion,
;;  which is a much less complex thing to do, with fewer and simpler failure scenarios to handle.
;;  This is especially important if your transaction posting code needs to deal with
;;  FOMO3D-style block-buying attacks, or other rapid gas price increase events.
;;
;;  The input data format does not use the Solidity ABI; we use a simpler and cheaper style:
;;  Just a raw vector of 32-byte records each comprised of a 20-byte address and a 12-byte value.
;;  No 4-byte header to identify a "function" to call; there's only one "function".
;;  No 32-byte vector count as first implicit argument; the size is taken from CALLDATASIZE.
;;  We never copy anything to memory; we just extract data to the stack.
;;
;;  Before we execute any transfer, we check that the sender matches the declared owner.
;;  Thus, if any money is sent to the contract or left in it, only the sender can later
;;  transfer that money out of the contract.
;;
;;  If any single transfer in the batch fails (because of lack of either funds or gas),
;;  revert the entire transaction (which cancels all the transfers that previously succeeded).

;; Create the runtime code for a batch contract associated to given owner
;; : Bytes <- Address
(def (batch-contract-runtime owner)
  (assemble/bytes
   [;; At instruction 0, so push 0 on stack while it's extra cheap!
    ;; a non-zero contract byte costs 220, a zero one only costs 204, so the GETPC optimization
    ;; is worth it if we actually use that 0 at least twice in the source code.
    GETPC #|0|# GETPC #|1|# ; -- 1 0
    ;; Abort if the caller isn't the contract's owner
    owner CALLER EQ [&jumpi1 'loop-init]
    DUP2 #|0|# REVERT

    ;; Initialize the loop invariant
    [&jumpdest 'loop-init] ;; -- 1 0
    (expt 2 96) DUP2 #|1|# DUP2 #|2**96|# SUB CALLDATASIZE DUP5 #|0|#
    ;; -- 0 size 2**96-1 2**96 1 0
    [&jump1 'loop-entry] ;; jump to entry, skipping inter-loop action

    ;; The loop: inter-loop action
    [&jumpdest 'loop]
    32 ADD

    ;; The entry point of the loop: check condition
    [&jumpdest 'loop-entry] ;; -- cursor size 2**96-1 2**96 1 0
    ;; If less then continue to loop-body, else return
    DUP2 #|size|# DUP2 #|cursor|# LT [&jumpi1 'loop-body] STOP

    ;; Loop body: take the next 256-bit argument.
    ;; Top 160 are address, lower 96 are value in wei.
    ;; Prepare the arguments to a transfer call.
    [&jumpdest 'loop-body] ;; -- cursor size 2**96-1 2**96 1 0
    DUP6 #|0|# DUP1 #|0|# DUP1 #|0|# DUP1 #|0|# DUP5 #|cursor|# CALLDATALOAD
    ;; -- data 0 0 0 0 cursor size 2**96-1 2**96 1 0
    DUP9 #|2**96|# DUP2 #|data|# DUP10 #|2**96-1|# AND
    ;; -- value 2**96 data 0 0 0 0 cursor size 2**96-1 2**96 1 0
    SWAP2 #|data 2**96 value|# DIV #|address|#
    ;; Transfer! -- address value 0 0 0 0 cursor size 2**96-1 2**96 1 0
    GAS CALL ;; -- success? cursor size 2**96-1 2**96 1 0

    ;; loop if successful, revert everything if failed.
    [&jumpi1 'loop]
    ;; -- cursor size 2**96-1 2**96 1 0
    DUP6 DUP1 REVERT]))

;; Given a constant contract runtime of length below 255,
;; that doesn't need any memory initialization, and doesn't contain functions we call,
;; return a contract initialization string, to be passed as parameter
;; to a CreateContract operation, to register the contract.
;; Beware: the code produced is not relocatable.

;; Create the runtime code for a batch contract associated to given owner
(def batch-contract-init (rcompose batch-contract-runtime stateless-contract-init))

;; Ensure that there is a batch transfer contract associated with the owner
;; on the blockchain and saved to the working database, and
;; return the ContractConfig for that contract.
(def (ensure-batch-send-contract owner log: (log eth-log))
  (def config (ensure-contract-config/db
               (u8vector-append (string->bytes "BATC") (bytes<- Address owner))
               (create-contract owner (batch-contract-init owner))
               log: log))
  (log ['ensure-batch-send-contract (0x<-address owner) (nickname<-address owner)
                                    '=> (json<- ContractConfig config)])
  config)

;; : <- Address (Listof (List Address UInt96))
(def (batch-send sender transfers log: (log eth-log))
  (def (fmt address amount)
    (validate UInt96 amount)
    (bytes-append (bytes<- Address address)
                  (bytes<- UInt96 amount)))
  (def data (apply bytes-append (map (cut apply fmt <>) transfers)))
  (def value (foldl + 0 (map cadr transfers)))
  (def transfer-description
    (match <> ([address amount]
               [(0x<-address address) (nickname<-address address) (decimal-string-ether<-wei amount)])))
  (when (< 0 value)
    (log ["batch transfer" "total-value:" value
          "transfers:" (map transfer-description transfers)
          "data:" (0x<-bytes data)])
    (let (contract (.@ (ensure-batch-send-contract sender log: log) contract-address))
      (post-transaction (call-function sender contract data value: value)))))
