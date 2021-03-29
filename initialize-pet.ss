;;; Initialization for the PET network

(export #t)

(import
  (for-syntax :std/misc/ports :std/text/hex :clan/path :clan/source)
  :gerbil/gambit/exceptions
  :std/iter :std/sugar
  :clan/exception :clan/multicall :clan/path-config :clan/syntax
  :clan/poo/object
  :clan/persist/db
  ./types ./ethereum ./json-rpc ./abi ./transaction ./tx-tracker ./testing ./meta-create2 ./assets)

(def QASPET@ (address<-0x "0x8e0BE69f202e245221B6D679B58faaBe1e463100"))
(def RBTPET@ (address<-0x "0x9FC8935f73cf1481729FE787669c558a30E5B44B"))
(def HAMPET@ (address<-0x "0x990E192133b8A98e229b4f22Fc0C3c1F4d88162E"))

(defvalues (test-erc20-contract-bytes test-erc721-contract-bytes)
  (syntax-call
   (lambda (ctx)
     (def here (path-parent (vector-ref (stx-source ctx) 0)))
     (def test-erc20-contract-bin (subpath here "t/precompiled/ERC20PresetFixedSupply.bin"))
     (def test-erc721-contract-bin (subpath here "t/precompiled/ERC721PresetMinterPauserAutoId.bin"))
     `(values ,(hex-decode (read-file-string test-erc20-contract-bin))
              ,(hex-decode (read-file-string test-erc721-contract-bin))))))

(defkeys initialize-pet
  (initializer "0x9a0685cf801c0b16a839ec9c28b7dc7f461e70f3d33307f3a15da1d68c7f9d83"))

(define-entry-point (initialize-pet)
  (help: "initialize the pet network"
   getopt: [])
  (ensure-db-connection "testdb")
  (ensure-ethereum-connection "pet")
  (register-test-keys)
  (def (QASPET-bytes)
    (ethabi-encode [String String UInt256 Address]
                   ["Quality Assurance Specie on Private Ethereum Testnet" "QASPET" (expt 10 18) croesus]
                   test-erc20-contract-bytes))
  (def (RBTPET-bytes)
    (ethabi-encode [String String UInt256 Address]
                   ["Random Barter Token on Private Ethereum Testnet" "RBTPET" (expt 10 18) croesus]
                   test-erc20-contract-bytes))
  (def (HAMPET-bytes)
    (ethabi-encode [String String String]
                   ["Crypto-Hamsters on Private Ethereum Testnet" "HAMPET" "https://hampet.mukn.io/"]
                   test-erc721-contract-bytes))

  (ensure-presigned-create2-wrapper)

  ;; Trying to create the OpenZeppelin ERC20 contract with CREATE2 fails(!?)
  #;(def (QASPET (salt 0)) (abi-create2 croesus (bytes<- UInt256 salt) (QASPET-bytes)))

  (def transactions
    [["Funding initializer" (transfer-tokens from: croesus to: initializer value: (wei<-ether 1)) (void)]
     ["Creating QASPET (ERC20)" (create-contract initializer (QASPET-bytes)) QASPET@]
     ["Creating RBTPET (ERC20)" (create-contract initializer (RBTPET-bytes)) RBTPET@]
     ["Creating HAMPET (ERC721)" (create-contract initializer (HAMPET-bytes)) HAMPET@]])

  (assert! (equal? (eth_getTransactionCount initializer) 0))
  (try
   (for (([msg tx ca] transactions)
         (i (in-naturals)))
     (displayln msg (if (void? ca) "" (string-append " " (0x<-address ca) " ")) "... ")
     (def receipt (post-transaction tx))
     (assert! (equal? (.@ receipt contractAddress) ca)))
   (catch (TransactionFailed? e)
     (display-exception (TransactionFailed-exn e))
     (error "fail!")))
  (assert! (equal? (eth_getTransactionCount initializer) 3)))

(.def (PET @ Ether)
  .name: "PET Ether"
  .symbol: 'PET
  .decimals: 18
  .network: 'pet)

(.def (QASPET @ ERC20)
  .contract-address: QASPET@
  .name: "Quality Assurance Specie on Private Ethereum Testnet"
  .symbol: 'QASPET
  .decimals: 18
  .network: 'pet)

(.def (RBTPET @ ERC20)
  .contract-address: RBTPET@
  .name: "Random Barter Token on Private Ethereum Testnet"
  .symbol: 'RBTPET
  .decimals: 18
  .network: 'pet)
