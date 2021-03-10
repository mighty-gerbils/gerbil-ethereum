(export #t)

(import
  :gerbil/gambit/bytes :gerbil/gambit/os
  :std/misc/list :std/misc/ports :std/misc/process :std/srfi/1 :std/test :std/text/hex
  :clan/base :clan/debug :clan/filesystem :clan/path :clan/path-config
  :clan/poo/object :clan/poo/debug
  :clan/persist/db
  ../json-rpc ../transaction ../nonce-tracker ../testing ../simple-apps ../assembly ../evm-runtime
  ../abi  ../erc20 ../ethereum ../tx-tracker
  ../hex ../types ../signing ../network-config
  ./10-json-rpc-integrationtest  ./20-nonce-tracker-integrationtest
  ./30-transaction-integrationtest ./60-abi-integrationtest)


;; TODO: either install the damn file with the build, or be able to locate it via nix or gxpkg
(def test-erc20-contract-bin (source-path "t/precompiled/ERC20PresetFixedSupply.bin"))

(def (test-erc20-contract-bytes)
  (hex-decode (read-file-string test-erc20-contract-bin)))

(def (deploy-contract owner types arguments contract-bytes)
  (!> (ethabi-encode types arguments contract-bytes)
      (cut create-contract owner <>)
      post-transaction
      (cut .@ <> contractAddress)))

(def (erc20-balances contract accounts)
  (map (cut erc20-balance contract <>) accounts))

(def (check-balancesOf-addresses contract inputs outputs)
  (check-equal? (erc20-balances contract inputs) outputs))

(def 65-erc20-integrationtest
  (test-suite "integration test for ethereum/erc20"
    (def x 1)
    (reset-nonce croesus) (DBG nonce: (peek-nonce croesus))
    (ensure-addresses-prefunded)
    (def initial-supply 1000000000)
    (DBG pre-erc20: (eth_getBalance croesus) initial-supply)
    (def contract (deploy-contract croesus
                                   [String String UInt256 Address]
                                   ["Alice" "ALI" initial-supply alice]
                                   (test-erc20-contract-bytes)))
    (DDT erc20: Address contract)
    #;(def reqbytes (ethabi-encode [Address] [alice] balanceOf-selector))
    #;(evm-eval croesus
              (assemble/bytes
               (&begin 32 0 (bytes-length reqbytes) 'foo DUP2 DUP2 0 CODECOPY
                       0 contract GAS CALL &require!
                       0 MLOAD 1000000000 EQ &require! STOP
                       [&label 'foo] [&bytes reqbytes] (&define-abort-contract-call)))
              block: 'onchain)
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
    ))
