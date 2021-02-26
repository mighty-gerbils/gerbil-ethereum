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

(def contract #f)

(def (ensure-contract)
  (unless contract
    (let (receipt (post-transaction (create-contract croesus (ethabi-encode [String String UInt256 Address]  ["Bin" "Yu" 1000000000 (address<-0x "0x25c0bb1A5203AF87869951AEf7cF3FEdD8E330fC")] (test-contract-bytes) ))))
      (set! contract (.@ receipt contractAddress)))))



(def 65-erc20-integrationtest
  (test-suite "integration test for ethereum/erc20"
    (ensure-contract)
    (test-case "Call ERC20 contract function totalsupply"
      (def pretx (call-function croesus contract totalSupply-selector))
      (def receipt (post-transaction pretx))
      (def block-number (.@ receipt blockNumber))
      (def data (eth_call pretx (1- block-number)))
      (check-equal-bytes? data (ethabi-encode [UInt256] [1000000000])))
       


      (test-case "Call ERC20 contract function transfer"
        (def input-data
          (ethabi-encode  [Address UInt256] [(address<-0x "0xf8Cc569fDAa507ddb81C979138187CfcaD39a1fE") 100] transfer-selector))
        (def pretx (call-function croesus contract input-data))
        (def receipt (post-transaction pretx))
        (def block-number (.@ receipt blockNumber))
        (def data (eth_call pretx (1- block-number)))
        (check-equal-bytes? data (ethabi-encode [UInt256] [1]))
      )
      
      (test-case "Call ERC20 contract function balanceOf"
        (def input-data
          (ethabi-encode  [Address] [(address<-0x "0xf8Cc569fDAa507ddb81C979138187CfcaD39a1fE")] balanceOf-selector))
        (def pretx (call-function croesus contract input-data))
        (def receipt (post-transaction pretx))
        (def block-number (.@ receipt blockNumber))
        (def data (eth_call pretx (1- block-number)))
        (check-equal-bytes? data (ethabi-encode [UInt256] [100]))
      )

      (test-case "Call ERC20 contract function approve"
        (def input-data
          (ethabi-encode  [Address UInt256] [(address<-0x "0xf8Cc569fDAa507ddb81C979138187CfcaD39a1fE") 100000] approve-selector))
        (def pretx (call-function croesus contract input-data))
        (def receipt (post-transaction pretx))
        (def block-number (.@ receipt blockNumber))
        (def data (eth_call pretx (1- block-number)))
        (check-equal-bytes? data (ethabi-encode [UInt256] [1]))

      )

      (test-case "Call ERC20 contract function allowance"
        (def input-data
          (ethabi-encode  [Address Address] [(address<-0x "0x25c0bb1A5203AF87869951AEf7cF3FEdD8E330fC")(address<-0x "0xf8Cc569fDAa507ddb81C979138187CfcaD39a1fE")] allowance-selector ))
        (def pretx (call-function croesus contract input-data))
        (def receipt (post-transaction pretx))
        (def block-number (.@ receipt blockNumber))
        (def data (eth_call pretx (1- block-number)))
        (check-equal-bytes? data (ethabi-encode [UInt256] [100000]))

      )
#|
      (test-case "Call ERC20 contract function transferFrom"
        (def input-data
          (ethabi-encode  [Address Address UInt256] [(address<-0x "0x25c0bb1A5203AF87869951AEf7cF3FEdD8E330fC")(address<-0x "0xf8Cc569fDAa507ddb81C979138187CfcaD39a1fE") 98] transferFrom-selector ))
        (def pretx (call-function croesus contract input-data))
        (def receipt (post-transaction pretx))
         (DDT 90-erc20-0: 
         TransactionReceipt receipt

         )
        (def block-number (.@ receipt blockNumber))
        (def data (eth_call pretx (1- block-number)))
       ;; (DBG data: data)
        ;;(DBG eco: (ethabi-encode [UInt256] [1]))
        (check-equal-bytes? data (ethabi-encode [UInt256] [1]))
        (def input-data1
          (ethabi-encode  [Address] [(address<-0x "0xf8Cc569fDAa507ddb81C979138187CfcaD39a1fE")] balanceOf-selector))
        (def pretx1 (call-function croesus contract input-data1))
        (def receipt1 (post-transaction pretx1))
        (def block-number1 (.@ receipt blockNumber))
        (def data1 (eth_call pretx1 (1- block-number1)))
        (check-equal-bytes? data1 (ethabi-encode [UInt256] [98])))
    
      (test-case "Call ERC20 contract function Transfer-event"
        (def input-data
          (ethabi-encode  [Address Address UInt256] [(address<-0x "0x25c0bb1A5203AF87869951AEf7cF3FEdD8E330fC")(address<-0x "0xf8Cc569fDAa507ddb81C979138187CfcaD39a1fE") 100] Transfer-event ))
        (def pretx (call-function croesus contract input-data))
        (def receipt (post-transaction pretx))
         (DDT 90-erc20-0: 
         TransactionReceipt receipt

         )
        (def block-number (.@ receipt blockNumber))
        (def data (eth_call pretx (1- block-number)))
       ;; (DBG data: data)
        ;;(DBG eco: (ethabi-encode [UInt256] [1]))
        (check-equal-bytes? data (ethabi-encode [UInt256] [100])))
 
      (test-case "Call ERC20 contract function Approval-event"
        (def input-data
          (ethabi-encode  [Address Address UInt256] [(address<-0x "0xf8Cc569fDAa507ddb81C979138187CfcaD39a1fE") (address<-0x "0x25c0bb1A5203AF87869951AEf7cF3FEdD8E330fC") 100] Approval-event ))
        (def pretx (call-function croesus contract input-data))
        (def receipt (post-transaction pretx))
         (DDT 90-erc20-0: 
         TransactionReceipt receipt

         )
        (def block-number (.@ receipt blockNumber))
        (def data (eth_call pretx (1- block-number)))
       ;; (DBG data: data)
        ;;(DBG eco: (ethabi-encode [UInt256] [1]))
        (check-equal-bytes? data (ethabi-encode [UInt256] [100])))
    |# 

      ))
