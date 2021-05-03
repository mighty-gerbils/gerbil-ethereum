(export #t)

(import
  :gerbil/gambit/bytes :gerbil/gambit/os
  :std/misc/list :std/misc/ports :std/test :std/text/hex
  :clan/base :clan/debug :clan/filesystem :clan/path :clan/path-config :clan/poo/io
  :clan/poo/object :clan/poo/debug
  ../json-rpc ../transaction ../nonce-tracker ../testing  ../assembly
  ../abi  ../erc20 ../ethereum ../tx-tracker ../types ../evm-runtime
  ../meta-create2 ../test-contracts
  ./10-json-rpc-integrationtest  ./20-nonce-tracker-integrationtest
  ./30-transaction-integrationtest ./60-abi-integrationtest)

(def (erc20-balances contract accounts)
  (map (lambda (account) (DDT erc20-balance: Address contract Address account Any
                         (erc20-balance contract account))) accounts))

(def (check-balancesOf-addresses contract inputs outputs)
  (check-equal? (erc20-balances contract inputs) outputs))

(def 65-erc20-integrationtest
  (test-suite "integration test for ethereum/erc20"
    (reset-nonce croesus) (DBG nonce: (peek-nonce croesus))
    (ensure-addresses-prefunded)
    (def initial-supply 1000000000)
    (def name "Alice")
    (def symbol "ALI")
    (def contract (abi-create croesus
                              test-erc20-contract-bytes
                              [String String UInt256 Address]
                              [name symbol initial-supply alice]))

    #;(evm-eval croesus
              (assemble/bytes
               (&begin 32 0 (bytes-length reqbytes) 'foo DUP2 DUP2 0 CODECOPY
                       0 contract GAS CALL &require!
                       0 MLOAD 1000000000 EQ &require! STOP
                       [&label 'foo] [&bytes reqbytes] (&define-abort-contract-call)))
              block: 'onchain)
    (DDT contract: Address contract)
    (check-equal? (erc20-balance contract alice requester: croesus) initial-supply)

    (test-case "Call ERC20 contract function totalsupply"
      (check-equal? (erc20-total-supply contract) initial-supply))

    (test-case "Call ERC20 contract function transfer vs balance"
      (check-balancesOf-addresses contract [alice bob trent] [1000000000 0 0])
      (erc20-transfer contract alice bob 100000)
      (check-balancesOf-addresses contract [alice bob trent] [999900000 100000 0]))

    (test-case "Call ERC20 contract function approve vs allowance"
      (check-equal? (erc20-allowance contract bob trent requester: bob) 0)
      (erc20-approve contract bob trent 1000)
      (check-equal? (erc20-allowance contract bob trent requester: bob) 1000)
      (check-balancesOf-addresses contract [alice bob trent] [999900000 100000 0]))

    (test-case "Call ERC20 contract function transferFrom"
      (erc20-transfer-from contract bob trent 1000 requester: trent)
      (check-equal? (erc20-allowance contract bob trent requester: croesus) 0)
      (check-balancesOf-addresses contract [alice bob trent] [999900000 99000 1000]))

    (test-case "Call ERC20 contract function approve vs reset approve"
      (check-equal? (erc20-allowance contract bob trent requester: bob) 0)
      (erc20-approve contract bob trent 999)
      (check-equal? (erc20-allowance contract bob trent requester: bob) 999))

;; Optional functions

    (test-case "Call ERC20 contract optional functions without parameter"
      (check-equal? (erc20-optional-fn contract name-selector [String] requester: bob)
                    [name])
      (check-equal? (erc20-optional-fn contract symbol-selector [String] requester: bob)
                    [symbol])
      (check-equal? (erc20-optional-fn contract decimals-selector [UInt8] requester: bob)
                    [18]))
    ))
