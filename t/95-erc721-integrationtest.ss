(export #t)

(import
  :gerbil/gambit/bytes :gerbil/gambit/os
  :std/misc/list :std/misc/ports :std/test :std/text/hex
  :clan/base :clan/debug :clan/filesystem :clan/path :clan/path-config :clan/poo/io
  :clan/poo/object :clan/poo/debug
  ../json-rpc ../transaction ../nonce-tracker ../testing  ../assembly
  ../abi ../erc721 ../erc165 ../ethereum ../tx-tracker ../types ../evm-runtime
  ./10-json-rpc-integrationtest  ./20-nonce-tracker-integrationtest
  ./30-transaction-integrationtest ./60-abi-integrationtest)


;; TODO: either install the damn file with the build, or be able to locate it via nix or gxpkg
(def test-erc721-contract-bin (source-path "t/precompiled/ERC721PresetMinterPauserAutoId.bin"))

(def (test-erc721-contract-bytes)
  (hex-decode (read-file-string test-erc721-contract-bin)))

;; Deploys a contract to private test net
(def (deploy-contract owner types arguments contract-bytes)
  (!> (ethabi-encode types arguments contract-bytes)
      (cut create-contract owner <>)
      post-transaction
      (cut .@ <> contractAddress)))

(def (erc721-balances contract accounts)
  (map (cut erc721-balance contract <>) accounts))

(def (check-balancesOf-addresses contract inputs outputs)
  (check-equal? (erc721-balances contract inputs) outputs))

(def 95-erc721-integrationtest
  (test-suite "integration test for ethereum/erc721"
    (reset-nonce croesus) (DBG nonce: (peek-nonce croesus))
    (ensure-addresses-prefunded)
    (def initial-supply 0)
    (def name "MukN")
    (def symbol "MK")
    (def base-token-uri "mukn://baseOne")
    (def contract (deploy-contract croesus
                                   [String String String]
                                   [name symbol base-token-uri]
                                   (test-erc721-contract-bytes)))
  
    (check-equal? (erc721-balance contract alice requester: croesus) initial-supply)
 
    (test-case "Call ERC721 contract function totalsupply"
      (check-equal? (erc721-total-supply contract) initial-supply))

    (test-case "Call ERC721 contract function balance vs mint"
      (check-balancesOf-addresses contract [alice bob trent] [0 0 0])
      (erc721-mint contract croesus bob)
      (check-balancesOf-addresses contract [alice bob trent] [0 1 0]))

    (test-case "Call ERC721 contract function ownerOf"
      (erc721-mint contract croesus bob)
      (check-equal? (erc721-ownerOf contract 1 requester: croesus) (ethabi-encode [Address] [bob])))

    (test-case "Call ERC721 contract function approve vs getApproved"
      (erc721-approve contract bob trent 1)
      (check-equal? (erc721-getApproved contract 1 requester: bob) (ethabi-encode [Address] [trent])))
     
    (test-case "Call ERC721 contract function transferFrom"
      (erc721-transfer-from contract bob trent 1 requester: bob)
      (check-balancesOf-addresses contract [alice bob trent] [0 1 1]))

    (test-case "Call ERC721 contract function safe transferFrom"
      (erc721-mint contract croesus bob)
      (erc721-approve contract bob trent 2)
      (erc721-safe-transfer-from contract bob trent 2 requester: bob)
      (check-balancesOf-addresses contract [alice bob trent] [0 1 2]))

    (test-case "Call ERC721 contract function safe transferFrom with data"
      (erc721-mint contract croesus bob)
      (erc721-approve contract bob trent 3)
      (erc721-safeTransferFrom-with-data-selector contract bob trent 3 "Hello Mukn, Bob has some NFTs" requester: bob)
      (check-balancesOf-addresses contract [alice bob trent] [0 1 3]))
 
    (test-case "Call ERC721 contract function setApprovalForAll vs isApprovedForAll"
      (check-equal? (erc721-isApprovedForAll contract bob alice requester: bob) 
                    [#f])
      (erc721-setApprovalForAll contract bob alice #t)
      (check-equal? (erc721-isApprovedForAll contract bob alice requester: bob) 
                    [#t])
      (erc721-setApprovalForAll contract bob alice #f)
      (check-equal? (erc721-isApprovedForAll contract bob alice requester: bob) 
                    [#f]))
                    
    ;; Optional functions

    (test-case "Call ERC721 contract optional functions without parameter"
      (check-equal? (erc721-optional-fn-without-parameter contract name-selector [String] requester: bob)
                    [name])
      (check-equal? (erc721-optional-fn-without-parameter contract symbol-selector [String] requester: bob)
                    [symbol]))
      
    (test-case "Call ERC721 contract optional functions with parameter"
      (check-equal? (erc721-optional-fn-with-parameter  contract tokenURI-selector [UInt256] [1] [String] requester: bob)
                    ["mukn://baseOne1"])
      (check-equal? (erc721-optional-fn-with-parameter contract tokenByIndex-selector [UInt256] [1] [UInt256] requester: bob)
                    [1])
      (check-equal? (erc721-optional-fn-with-parameter contract tokenOfOwnerByIndex-selector [Address UInt256] [bob 0] [UInt256] requester: bob)
                    [0]))
                    
    (test-case "Call ERC721 supportsInterface (ERC165) Query if a contract implements an interface"
      (def erc721Metadata (<-string Bytes4 "0x5b5e139f")) ;; https://eips.ethereum.org/EIPS/eip-721 Implemented
      (def erc721TokenReceiver (<-string Bytes4 "0x150b7a02")) ;;https://eips.ethereum.org/EIPS/eip-721 Note Implemented
      (check-equal? (erci65-supportsInterface contract erc721Metadata requester: bob) 
                    [#t])
      (check-equal? (erci65-supportsInterface contract erc721TokenReceiver requester: bob) 
                    [#f]))           
    ))
