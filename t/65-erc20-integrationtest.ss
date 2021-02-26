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



;;(def gerbil-ethereum-src (or (getenv "GERBIL_ETHEREUM_SRC" #f)
                          ;;   (source-directory)))

;; TODO: either install the damn file with the build, or be able to locate it via nix or gxpkg
(def test-contract-source (source-path "t/ERC20PresetFixedSupply.sol"))
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

(def contract #f)

(def (ensure-contract)
  (unless contract
    (let (receipt (post-transaction (create-contract croesus (ethabi-encode [String String UInt256 Bytes]  ["Bin" "Yu" 100000 (address-bytes (address<-0x "0xc61C92c6eF6277c67C2d2F6d973144e38cD66278"))] (test-contract-bytes) ))))
      (set! contract (.@ receipt contractAddress)))))



(def 65-erc20-integrationtest
  (test-suite "integration test for ethereum/erc20"
    (ensure-contract)
    (test-case "Call ERC20 contract function totalsupply"
      (def pretx (call-function croesus contract totalSupply-selector))
      (def receipt (post-transaction pretx))
      (def block-number (.@ receipt blockNumber))
      (def data (eth_call pretx (1- block-number)))
      (check-equal-bytes? data (ethabi-encode [UInt256] [100000]))
       
      (DDT 90-erc20-0:
        Address contract
        Address croesus
       TransactionReceipt receipt)
      
    
      
      
      )))
