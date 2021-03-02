;;; Support for using and implementing an ERC20
;; https://eips.ethereum.org/EIPS/eip-20
(export #t)

(import
  :gerbil/gambit/bits :gerbil/gambit/bytes
  :std/srfi/1 :std/sugar
  :clan/base :clan/with-id
  :clan/poo/object (only-in :clan/poo/mop) :clan/poo/io
  ./logger ./hex ./types ./ethereum ./known-addresses ./abi ./json-rpc
  ./assembly ./transaction ./tx-tracker ./contract-config ./evm-runtime)

;;; OPTIONAL functions NOT implemented:
;;function name() public view returns (string)
;;function symbol() public view returns (string)
;;function decimals() public view returns (uint8)

(def totalSupply-selector ;;function totalSupply() public view returns (uint256)
  (selector<-function-signature ["totalSupply"]))
(def balanceOf-selector ;;function balanceOf(address _owner) public view returns (uint256 balance)
  (selector<-function-signature ["balanceOf" Address]))
(def transfer-selector ;;function transfer(address _to, uint256 _value) public returns (bool success)
  (selector<-function-signature ["transfer" Address UInt256]))
(def transferFrom-selector ;;function transferFrom(address _from, address _to, uint256 _value) public returns (bool success)
  (selector<-function-signature ["transferFrom" Address Address UInt256]))
(def approve-selector ;;function approve(address _spender, uint256 _value) public returns (bool success)
  (selector<-function-signature ["approve" Address UInt256]))
(def allowance-selector ;;function allowance(address _owner, address _spender) public view returns (uint256 remaining)
  (selector<-function-signature ["allowance" Address Address]))
(def abort-selector #u8(255 255 255 255))

(def Transfer-event ;;event Transfer(address indexed _from, address indexed _to, uint256 _value)
  (selector<-function-signature ["Transfer" Address Address UInt256]))
(def Approval-event ;;event Approval(address indexed _owner, address indexed _spender, uint256 _value)
  (selector<-function-signature ["Allowance" Address Address UInt256]))

;; shortest bit field that discriminates all selectors
(def (erc20-selector-index selector)
  (extract-bit-field 3 11 (<-bytes UInt32 selector)))
(def (erc20-selector-vector)
  (assert! (equal?
            (map erc20-selector-index [totalSupply-selector balanceOf-selector transfer-selector transferFrom-selector approve-selector allowance-selector])
            '(1 0 3 6 4 5)))
  `(+ ,@(map (lambda (sym selector) ['* sym (arithmetic-shift 1 (* 8 (erc20-selector-index selector)))])
             '(totalSupply balanceOf transfer transferFrom approve allowance abort-contract-call)
             [totalSupply-selector balanceOf-selector transfer-selector transferFrom-selector approve-selector allowance-selector])))

;; Create the runtime code for a simple ERC20 contract
;; : Bytes <- Address
(def (erc20-runtime total-supply)
  (def safe-sub (&begin DUP2 DUP2 LT [&jumpi1 '&failure] SUB))
  (def safe-add (&begin DUP2 DUP2 NOT LT [&jumpi1 '&failure] ADD))
  (assemble/bytes
   [;; At instruction 0, so push 0 on stack while it's extra cheap!
    ;; a non-zero contract byte costs 220, a zero one only costs 204, so the GETPC optimization
    ;; is worth it if we actually use that 0 at least twice in the source code.
    GETPC #|0|# 64 36 32 4 ;; -- 4 32 36 64 0 == $CONSTANTS
    DUP5 #|0|# CALLDATALOAD (&shr 224) ;; get selector
    PUSH7 [&fixup (* 6 8) (erc20-selector-vector)] ;; selector vector
    DUP2 #|selector|# (&shr 8) 56 AND SHR 255 AND JUMP ;; jump to selected function
    ;; jump to entry point with -- selector $CONSTANTS
    (&define-abort-contract-call)
    [&jumpdest '&failure] 0 [&jump1 '&return-bool]

    [&jumpdest 'totalSupply] ;;function totalSupply() public view returns (uint256)
    totalSupply-selector SUB &require-not!
    total-supply [&jump1 '&return-uint]

    [&jumpdest 'balanceOf] ;;function balanceOf(address _owner) public view returns (uint256 balance)
    balanceOf-selector SUB &require-not!
    DUP1 #|4|# CALLDATALOAD
    [&jumpdest 'getBalance]
    SLOAD
    [&jumpdest '&return-uint] ;; -- result $CONSTANTS
    DUP6 #|0|# MSTORE DUP3 #|32|# DUP7 #|0|# RETURN

    [&jumpdest 'transfer] ;;function transfer(address _to, uint256 _value) public returns (bool success)
    transfer-selector SUB &require-not!
    '&success ;; -- &success $CONSTANTS
    DUP4 #|36|# CALLDATALOAD DUP3 #|4|# CALLDATALOAD CALLER ;; -- from to value &success $CONSTANTS

    [&jumpdest 'doTransfer] ;; -- from to value ret $CONSTANTS
    DUP3 #|value|# DUP10 #|0|# MSTORE ;; save value for logging on success
    DUP3 #|value|# DUP3 #|to|# SLOAD ADD ;; safe-add not necessary because we're under the totalSupply.
    ;; -- new-to-value from to value ret $CONSTANTS
    SWAP3 #|value<->new-to-value|# DUP2 #|from|# SLOAD safe-sub SWAP1
    ;; -- from new-from-value to new-to-value ret $CONSTANTS
    DUP3 #|to|# DUP2 #|from|# Transfer-event DUP10 #|32|# DUP14 #|0|# LOG3
    SSTORE SSTORE JUMP

    [&jumpdest 'transferFrom] ;;function transferFrom(address _from, address _to, uint256 _value) public returns (bool success)
    transferFrom-selector SUB &require-not!
    ;; -- $CONSTANTS
    DUP1 #|4|# CALLDATALOAD DUP4 #|36|# CALLDATALOAD '&updateAllowance DUP6 #|68|# CALLDATALOAD
    ;; -- value &updateAllowance to from $CONSTANTS
    DUP4 #|from|# DUP10 #|0|# MSTORE CALLER #|to|# DUP7 #|32|# MSTORE DUP8 #|64|# DUP10 #|0|# SHA3
    ;; -- @allowance value &updateAllowance to from $CONSTANTS
    SWAP3 #|to<->@allowance|#
    ;; -- to value &updateAllowance @allowance from $CONSTANTS
    DUP2 #|value|# DUP5 #|@allowance|# SLOAD safe-sub ;;-- allowance-left
    ;; -- allowance-left to value &updateAllowance @allowance from $CONSTANTS
    SWAP5 #|from<->allowance-left|#
    ;; -- from to value &updateAllowance @allowance allowance-left $CONSTANTS
    [&jump 'doTransfer]
    [&jumpdest '&updateAllowance]
    SSTORE
    [&jumpdest '&success]
    1
    [&jumpdest '&return-bool] ;; -- bool
    0 MSTORE8 1 0 RETURN

    [&jumpdest 'approve] ;;function approve(address _spender, uint256 _value) public returns (bool success)
    approve-selector SUB &require-not!
    DUP3 #|36|# CALLDATALOAD ;;-- value $CONSTANTS
    DUP1 DUP7 #|0|# MSTORE ;; put value in memory as data for logging
    DUP2 #|4|# CALLDATALOAD ;;-- to value $CONSTANTS
    DUP1 #|to|# CALLER Approval-event DUP7 #|32|# DUP11 #|0|# LOG3
    CALLER DUP8 #|0|# MSTORE DUP4 #|32|# MSTORE DUP5 #|64|# DUP7 #|0|# SHA3 ;;-- @allowance value
    SSTORE [&jump1 '&success]

    [&jumpdest 'allowance] ;;function allowance(address _owner, address _spender) public view returns (uint256 remaining)
    allowance-selector SUB &require-not!
    DUP1 #|4|# CALLDATALOAD DUP6 #|0|# MSTORE
    DUP3 #|36|# CALLDATALOAD DUP3 #|32|# MSTORE
    DUP4 #|64|# DUP6 #|0|# SHA3 ;;-- @allowance
    [&jump1 'getBalance]]))

;; Given a constant contract runtime of length below 255,
;; that doesn't need any memory initialization, and doesn't contain functions we call,
;; return a contract initialization string, to be passed as parameter
;; to a CreateContract operation, to register the contract.
;; Beware: the code produced is not relocatable.

(def (erc20-init-claim claim)
  (with ([owner amount] claim)
    [amount owner SSTORE]))

;; Create the runtime code for an ERC contract with given list of initial claims
;; : Unit <- (List (Pair Address TokenAmount))
(def (erc20-init claims)
  (def total-supply (reduce + 0 (map cadr claims)))
  (assemble/bytes [(append-map erc20-init-claim claims)...
                   (&trivial-contract-init (erc20-runtime total-supply))]))

;; Ensure that there is a batch transfer contract associated with the owner
;; on the blockchain and saved to the working database, and
;; return the ContractConfig for that contract.
(def (ensure-erc20 name creator claims log: (log eth-log))
  (def config (ensure-contract-config/db
               (string->bytes (string-append "ERC20:" name))
               (create-contract creator (erc20-init claims))
               log: log))
  (log ['ensure-erc20-contract
        name (0x<-address creator) (nickname<-address creator)
        claims
        '=> (json<- ContractConfig config)])
  config)


;;; Functions to interact with an ERC20 contract as a client

(def (erc20-total-supply contract (block 'latest) requester: (requester null-address))
  (<-bytes UInt256 (eth_call (call-function requester contract totalSupply-selector) block)))

(def (erc20-balance contract account requester: (requester null-address))
  (!> (ethabi-encode [Address] [account] balanceOf-selector)
      (cut call-function requester contract <>)
      eth_call
      (cut <-bytes UInt256 <>)))

(def (erc20-allowance contract sender recipient requester: (requester null-address))
  (!> (ethabi-encode [Address Address] [sender recipient] allowance-selector)
      (cut call-function requester contract <>)
      eth_call
      (cut <-bytes UInt256 <>)))

(def (erc20-transfer contract sender recipient amount)
  (!> (ethabi-encode [Address UInt256] [recipient amount] transfer-selector)
      (cut call-function sender contract <>)
      post-transaction)) ;; TODO: check that it logged a Transfer (but it may have logged more than that!)

;; TODO: the function that ensures we confirm 0 before we set an approval to >0
;; NEVER APPROVE MORE THAN 0 UNLESS YOU CONFIRM IT TO 0 FIRST!
(def (erc20-approve contract sender recipient amount)
  (!> (ethabi-encode [Address UInt256] [recipient amount] approve-selector)
      (cut call-function sender contract <>)
      post-transaction)) ;; TODO: check that it logged an Approval (but it may have logged more than that!)

(def (erc20-transfer-from contract sender recipient amount requester: requester)
  (!> (ethabi-encode [Address Address UInt256] [sender recipient amount] transferFrom-selector)
      (cut call-function requester contract <>)
      post-transaction)) ;; TODO: check that it logged a Transfer (but it may have logged more than that!)
