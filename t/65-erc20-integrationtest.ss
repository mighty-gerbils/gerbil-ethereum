(export #t)

(import
  :gerbil/gambit/os
  :std/misc/list :std/misc/ports :std/misc/process :std/srfi/1 :std/test :std/text/hex
  :clan/debug :clan/poo/object :clan/path :clan/path-config
  ../json-rpc ../transaction ../nonce-tracker ../testing ../simple-apps 
  :clan/debug :clan/persist/db :clan/path-config :clan/poo/debug
  ../abi  ../erc20 ../ethereum ../tx-tracker
  ../hex ../types ../signing ../network-config
  ./10-json-rpc-integrationtest  ./20-nonce-tracker-integrationtest
  ./30-transaction-integrationtest ./60-abi-integrationtest)


;; TODO: either install the damn file with the build, or be able to locate it via nix or gxpkg
(def test-contract-source (source-path "t/erc20/ERC20PresetFixedSupply.sol"))
(def test-contract-bin (cache-path "t/ethereum/ERC20PresetFixedSupply.bin"))

(def (modification-time file)
  (let/cc return
    (def info (with-catch false (cut file-info file #t)))
    (time->seconds (file-info-last-modification-time info))))

(def (test-contract-bytes)
  (unless (and (file-exists? test-contract-bin)
               (<= (or (modification-time test-contract-source) +inf.0)
                   (or (modification-time test-contract-bin) -inf.0)))
    (compile-solidity test-contract-source (path-parent test-contract-bin)))
  (hex-decode (read-file-string test-contract-bin)))

(def (deploy-contract owner types arguments contract-bytes)
  (let (receipt (post-transaction (create-contract owner (ethabi-encode types arguments contract-bytes))))
    (.@ receipt contractAddress)))

(def (get-balance contract user:(user croesus) addr) 
  (def input-data (ethabi-encode [Address] [addr] balanceOf-selector))
  (ethabi-decode [UInt256] (eth_call (call-function user contract input-data))))

(def (get-balances contract addrs) 
  (map (cut get-balance contract <>) addrs))

(def (check-balancesOf-addresses inputs outputs) 
  (def balances (map (cut get-balance (car inputs) <>) (cdr inputs)))
  (check-equal? balances outputs))

(def (get-query contract selector-fn user inputs)
 (let ((values types arguments) inputs)
    (def input-data (ethabi-encode types arguments selector-fn))
    (eth_call (call-function user contract input-data))))

(def (check-query contract selector-fn user inputs outputs)
  (check-equal? (car (cdr outputs)) (ethabi-decode (car outputs) (get-query contract selector-fn user inputs))))

(def (check-transaction contract selector-fn user inputs outputs)
  (def input-data
    (ethabi-encode (car inputs) (cdr inputs)  selector-fn))
  (def pretx (call-function user contract input-data))
  (def receipt (post-transaction pretx))
  (def block-number (.@ receipt blockNumber))
  (def data (eth_call pretx (1- block-number)))
  (check-equal-bytes? data (ethabi-encode (car outputs) (cdr outputs))))

(def 65-erc20-integrationtest
  (test-suite "integration test for ethereum/erc20"
    (reset-nonce croesus) (DBG nonce: (peek-nonce croesus))
    (def name "Alice")
    (def symbol "ALI")
    (def initial-supply 1000000000)
    (def balance (eth_getBalance croesus 'latest))
    (DBG money:
      balance)
    ;;(ensure-addresses-prefunded)
    (DBG args: initial-supply)
   ;; (def contract-bytes (test-contract-bytes))
    (DBG args: name)
    (def arguments [name symbol initial-supply croesus])
    (def types [String String UInt256 Address]) ;; Still can't use other address except croesus
    ;;(def contract (deploy-contract croesus types arguments contract-bytes))
    (ensure-contract name symbol initial-supply croesus)
    (DBG args: arguments)

    (test-case "Call ERC20 contract function totalsupply"
      (def data (eth_call (call-function croesus contract totalSupply-selector)))
      (check-equal-bytes? data (ethabi-encode [UInt256] [1000000000])))

    (test-case "Call ERC20 check balances  of trent, alice, and bob before transfer operation"
      (check-balancesOf-addresses [contract [alice trent bob]] [1000000000 0 0]))

    (test-case "Call ERC20 contract function transfer"
      (def inputs [[Address UInt256] [bob 100000]])
      (def outputs [[UInt256] [1]])
      (check-transaction contract transfer-selector alice inputs outputs))

    (test-case "Call ERC20 check balances  of trent, alice, and bob after transfer operation"
      (check-balancesOf-addresses [contract [alice trent bob]] [1000000000 0 100000]))

    (test-case "Call ERC20 contract function approve"
      (def inputs [[Address UInt256] [trent 1000]])
      (def outputs [[UInt256] [1]])
      (check-transaction contract transfer-selector bob inputs outputs))

    (test-case "Call ERC20 contract function allowance"
      (check-query contract allowance-selector bob (values [Address Address] [bob trent]) [[UInt256] [1000]]))

    (test-case "Call ERC20 contract function transferFrom"
      (def inputs0 [[Address UInt256] [trent 1000]])
      (def outputs0 [[UInt256] [1]])
      (check-transaction contract approve-selector bob inputs0 outputs0)

      (def inputs1 [[Address Address UInt256] [alice bob 1000]])
      (def outputs1 [[UInt256] [1]])
      (check-transaction contract transferFrom-selector bob inputs1 outputs1)

      (check-balancesOf-addresses [contract [alice trent bob]] [1000000000 1000 9000]))
  ))