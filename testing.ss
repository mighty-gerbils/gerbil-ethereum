(export #t)

(import
  :gerbil/gambit/bytes :gerbil/gambit/threads
  :std/format :std/iter :std/misc/list :std/srfi/1 :std/srfi/13 :std/sugar :std/test
  :clan/base :clan/json :clan/multicall :clan/path-config :clan/syntax :clan/with-id
  :clan/poo/object :clan/poo/debug :clan/poo/brace :clan/poo/io
  ./types ./ethereum ./known-addresses ./abi
  ./network-config ./json-rpc ./transaction ./nonce-tracker
  ./assembly ./evm-runtime ./simple-apps ./assets)

(def (capitalize name)
  (def Name (string-downcase (stringify name)))
  (string-set! Name 0 (char-upcase (string-ref Name 0)))
  Name)

(def test-keys (values []))
(def test-addresses (values []))

(defrule (defkeys ctx (name secret-key) ...)
  (begin
    (with-id ctx ((keys #'name '-keys)
                  (address #'name)
                  test-keypairs)
      (begin
        (def keys (keypair<-seckey-0x secret-key))
        (def address (keypair-address keys))
        (push! [(format "t/~a" (capitalize 'name)) keys] test-keys)
        (push! address test-addresses))) ...))

(defkeys test-addresses
  ;; These keys are chosen for a common name and recognizable prefix, for use on private test networks
  ;; With our naive algorithm, finding a 5-char hex prefix should take a few minutes,
  ;; a 6-char hex prefix an hour or two, a 7-char hex prefix a day or two.
  (alice    "0x33bbf7ff271c056cae4eba6503ad46d8cf6f4c35120ef97cc6ee719cf711e767") ;; 0xa71CE
  (bob      "0x30ce4a96f528bbfcd20d8c0c52f5c691f7e9675ef87e5a955e4e2d6f09c35ab0") ;; 0xb0bb1e
  (trent    "0x2d7d92a15f28bb6d56823a10c9a361e97bcd27714761dd95113765a9e5b33595") ;; 0x73e27
  ;; This is the penny collector for private test networks
  #;(penny    "0x06d14bc1a49f8fde1dd20f57beb4712bf708f8bc441e7c3b7a8ad396ed9db344") ;; 0xC0773c1
  ;; This key was chosen because it's got money on in genesis block for IOHK's Mantis docker image.
  ;; We now configure use of the same key for the "got all the money" account on our Geth genesis block.
  (croesus  "0x1167a41c432d1a494408b8fdeecd79bff89a5689925606dff8adf01f4bf92922"))

;; Display an account having the given balance given a way to print address, optional name and balance
;; : 'a <- ('a <- string string) Address TokenAmount
(def (display-balance display address balance)
  (display (nicknamed-string<-address address) balance))

(def (get-address-missing-amount min-balance target-balance address)
  (assert! (<= min-balance target-balance))
  (def balance (eth_getBalance address 'latest))
  (if (>= balance min-balance)
    (begin
      (printf "~a has ~a already. Good.\n"
              (nicknamed-string<-address address)
              (.call Ether .string<- balance))
      0)
    (begin
      (printf "~a has ~a ether only. Funding to ~a ether.\n"
              (nicknamed-string<-address address)
              (.call Ether .string<- balance)
              (.call Ether .string<- target-balance))
      (- target-balance balance))))

(def prefunded-addresses [alice bob trent])

;; target-balance is more than min-balance, so we can go faster by not re-funding everytime.
(def (ensure-addresses-prefunded
      from: (funder croesus)
      to: (addresses prefunded-addresses)
      min-balance: (min-balance one-ether-in-wei)
      target-balance: (target-balance (* 2 min-balance))
      batch-contract: (batch-contract #f))
  (def needful-transfers
    (with-list-builder (c)
      (for (a addresses)
        (unless (equal? a funder)
          (let (v (get-address-missing-amount min-balance target-balance a))
            (when (> v 0)
              (c (batched-transfer v a))))))))
  (batch-txs funder needful-transfers log: write-json-ln batch-contract: batch-contract gas: 400000))

;; Send a tx, not robust, but useful for debugging
;; : SignedTransactionInfo TransactionReceipt <- PreTransaction confirmations:?Nat
(def (debug-send-tx
      tx confirmations: (confirmations (ethereum-confirmations-wanted-in-blocks)))
  (def from (.@ tx from))
  (reset-nonce from)
  (def signed (sign-transaction tx))
  (def tx0 {to: ? (void) data: ? (void) value: ? 0 nonce: ? (void) gas: ? (void) gasPrice: ? (void)})
  (DDT debug-send-tx-0:
       PreTransaction (.mix tx tx0)
       SignedTransactionInfo signed)
  (def receipt
    (let/cc return
      (while #t
        (try
         (ignore-errors (send-signed-transaction signed))
         (return (confirmed-receipt<-transaction signed confirmations: confirmations))
         (catch StillPending? => void)
         (catch (TransactionRejected? e) (return (TransactionRejected-receipt e))))
        (thread-sleep! (ethereum-block-polling-period-in-seconds)))))
  (def success? (successful-receipt? receipt))
  (DDT debug-send-tx-1:
       Bool success?
       (Or TransactionReceipt Any) receipt)
  (unless success? (raise (TransactionRejected receipt)))
  (values signed receipt))

(def (debug-confirm-tx tx confirmations: (confirmations 0))
  (let/cc return
    (while #t
      (try
       (return (confirmed-receipt<-transaction tx confirmations: confirmations))
       (catch StillPending? => void)
       (catch (TransactionRejected? e) (return (TransactionRejected-receipt e))))
      (thread-sleep! (ethereum-block-polling-period-in-seconds)))))

;; Bytes <- Address Bytes value:?(Maybe Quantity) block:?(Or BlockParameter (Enum onchain))
;; Block can be a block number, latest, earliest, pending, or onchain.
;; if onchain, then commit the evaluation to be inspected with remix.ethereum.org
(def (evm-eval from code value: (value (void)) block: (block 'latest))
  (if (eq? block 'onchain)
    (let ()
      (defvalues (_ creation-receipt) (debug-send-tx {from data: code value gas: 4000000}))
      (def contract (.@ creation-receipt contractAddress))
      (eth_getCode contract 'latest))
    (eth_call {from data: code value} block)))

;; TODO: support boxed types
;; Directive <- t:Type t
(def (&evm-inline-input t v)
  (bytes<- t v))

;; Directive <- (Listof DependentPair)
(def (&evm-inline-inputs inputs)
  (&begin*
   (map (match <> ([t . v] (&evm-inline-input t v))) (reverse inputs))))

;; Directive <- Type
(def (&evm-inline-output t)
  (def len (param-length t))
  ;;(DDT &evm-inline-output: Type t poo.Nat len)
  (&begin ;; bufptr[incremented] <-- bufptr result-start result-start val:t
   SWAP1 SWAP3 DUP2 (&mstore/overwrite-after len) len ADD))

;; TODO: support boxed types as inputs (that may offset the start of the output?) and outputs
(def (&evm-inline-outputs outputs result-start: (result-start 0))
  (&begin
   result-start DUP1 DUP1 ;; start output buffer
   (&begin* (map (match <> ([t . _] (&evm-inline-output t))) outputs))
   SUB SWAP1))

(def (&evm-test-code inputs action outputs
                     result-in-memory?: (result-in-memory? #f)
                     result-start: (result-start 0))
  (&begin
   (&evm-inline-inputs inputs)
   action
   (if result-in-memory?
     (let (result-length (reduce + 0 (map (compose param-length car) outputs)))
       (&begin result-length result-start))
     (&evm-inline-outputs outputs result-start: result-start))
   RETURN
   [&jumpdest 'abort-contract-call] 0 DUP1 REVERT))

;; result-in-memory? true iff the action already stores its results in memory
;; result-start is offset of result in memory at the end of the contract
(def (evm-test inputs action outputs
               block: (block 'latest)
               result-in-memory?: (result-in-memory? #f)
               result-start: (result-start 0))
  (def code-bytes (assemble/bytes (&evm-test-code inputs action outputs
                                                  result-in-memory?: result-in-memory?
                                                  result-start: result-start)))
  ;;(DDT evm-test-1: Bytes code-bytes)
  (def result-bytes (evm-eval croesus code-bytes block: block))
  ;;(DDT evm-test-2: Bytes result-bytes)
  (def result-list
    (call-with-input-u8vector
     result-bytes
     (lambda (port)
       (map-in-order (lambda (tv) (def t (car tv)) (sexp<- t (unmarshal t port))) outputs))))
  (def expected-result-list
    (map (lambda (tv) (sexp<- (car tv) (cdr tv))) outputs))
  (check-equal? result-list expected-result-list))

(def (evm-test-failure inputs action block: (block 'latest))
  (def code-bytes (assemble/bytes (&evm-test-code inputs action [])))
  (if (ethereum-mantis?) ;; See bug CASC-99 in IOHK JIRA
    (check-equal? (evm-eval croesus code-bytes block: block) #u8())
    (check-equal?
     (with-catch true (lambda () (evm-eval croesus code-bytes block: block) #f))
     #t)))

(def (extracted-logger-log log)
  (def topics (.@ log topics))
  ;;(DDT ell0: LogObject log Any topics)
  [(0x<-address (.@ log address))
   (map (.@ Bytes32 .json<-) topics)
   (bytes->string (.@ log data))])
(def (expected-logger-log logger caller message)
  [(0x<-address logger)
   [(json<- Bytes32 (bytes-append (make-bytes 12) (bytes<- Address caller)))]
   message])

(def (expect-logger-logs receipt . expectations)
  ;;(DDT ell: TransactionReceipt receipt Any (.@ receipt logs) Any expectations)
  (def extracted-logs (map extracted-logger-log (.@ receipt logs)))
  (def expected-logs (map (cut apply expected-logger-log <>) expectations))
  (check-equal? extracted-logs expected-logs))

(def (precompile-contract source)
  (compile-solidity (source-path "t/solidity" source) (source-path "t/precompiled/")))

(define-entry-point (precompile-contracts)
  (help: "precompile solidity contracts used during testing"
   getopt: [])
  (precompile-contract "HelloWorld.sol")
  (precompile-contract "erc20/ERC20PresetFixedSupply.sol")
  (precompile-contract "erc721/ERC721PresetMinterPauserAutoId.sol"))


;; Create a contract using the Ethereum ABI for arguments
(def (abi-create owner contract-bytes (types []) (arguments []) value: (value 0))
  (defvalues (signed receipt)
    (!> (ethabi-encode types arguments contract-bytes)
        (cut create-contract owner <> value: value)
        debug-send-tx))
  (DDT create: TransactionReceipt receipt)
  (.@ receipt contractAddress))

(def (register-test-keys)
  ;; Register test keypairs
  (for-each (cut apply register-keypair <>) test-keys))
