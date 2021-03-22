;;; Support for using and implementing an ERC721
;; https://eips.ethereum.org/EIPS/eip-721
(export #t)

(import
  :gerbil/gambit/bits :gerbil/gambit/bytes
  :std/srfi/1 :std/sugar
  :clan/base :clan/with-id
  :clan/poo/object (only-in :clan/poo/mop) :clan/poo/io
  ./logger ./hex ./types ./ethereum ./known-addresses ./abi ./json-rpc
  ./assembly ./transaction ./tx-tracker ./contract-config ./evm-runtime ./erc20) 


(def totalSupply-selector ;;function totalSupply() external view returns (uint256)
  (selector<-function-signature ["totalSupply"]))
(def name-selector ;;function name() public view returns (string)
  (selector<-function-signature ["name"]))
(def symbol-selector ;;function symbol() public view returns (string)
  (selector<-function-signature ["symbol"]))
(def tokenURI-selector ;;function tokenURI(uint256 _tokenId) external view returns (string)
  (selector<-function-signature ["tokenURI" UInt256]))
(def tokenByIndex-selector ;;function tokenByIndex(uint256 _index) external view returns (uint256)
  (selector<-function-signature ["tokenByIndex" UInt256]))
(def tokenOfOwnerByIndex-selector ;;tokenOfOwnerByIndex(address _owner, uint256 _index) external view returns (uint256)
  (selector<-function-signature ["tokenOfOwnerByIndex" Address UInt256]))

(def balanceOf-selector ;;function balanceOf(address _owner) external view returns (uint256)
  (selector<-function-signature ["balanceOf" Address]))
(def ownerOf-selector ;;function ownerOf(uint256 _tokenId) external view returns (address)
  (selector<-function-signature ["ownerOf" UInt256]))

(def safeTransferFrom-with-data-selector ;;function safeTransferFrom(address _from, address _to, uint256 _tokenId, bytes data) external payable
  (selector<-function-signature ["safeTransferFrom" Address Address UInt256 Bytes]))
(def safeTransferFrom-selector ;;function safeTransferFrom(address _from, address _to, uint256 _tokenId) external payable
  (selector<-function-signature ["safeTransferFrom" Address Address UInt256]))
(def transferFrom-selector ;;ffunction transferFrom(address _from, address _to, uint256 _tokenId) external payable
  (selector<-function-signature ["transferFrom" Address Address UInt256]))

(def approve-selector ;;function approve(address _approved, uint256 _tokenId) external payable
  (selector<-function-signature ["approve" Address UInt256]))
(def setApprovalForAll-selector ;;function setApprovalForAll(address _operator, bool _approved) external
  (selector<-function-signature ["setApprovalForAll" Address Bool]))
(def getApproved-selector ;;function getApproved(uint256 _tokenId) external view returns (address)
  (selector<-function-signature ["getApproved" UInt256]))
(def isApprovedForAll-selector ;;function isApprovedForAll(address _owner, address _operator) external view returns (bool)
  (selector<-function-signature ["isApprovedForAll" Address Address]))
(def abort-selector #u8(255 255 255 255))

(def Transfer-event ;;event Transfer(address indexed _from, address indexed _to, uint256 _value)
  (digest<-function-signature ["Transfer" Address Address UInt256]))
(def Approval-event ;;event Approval(address indexed _owner, address indexed _spender, uint256 _value)
  (digest<-function-signature ["Approval" Address Address UInt256]))
(def ApprovalForAll-event ;;event ApprovalForAll(address indexed _owner, address indexed _operator, bool _approved)
  (digest<-function-signature ["ApprovalForAll" Address Address Bool]))

(def mint-selector ;;function mint(address to) public virtual
  (selector<-function-signature ["mint" Address]))

;; Functions that process and match logs

;; : [Listof Any] <- Log
(def (erc721-extracted-logger-log log)
  [(.@ log address) (.@ log topics)])

;; : [Listof Any] <- Bytes4 Address Address Address UInt256
(def (erc721-expected-logger-log event-signature contract sender recipient amount)
  [contract
    [event-signature
      (ethabi-encode [Address] [sender])
      (ethabi-encode [Address] [recipient])
      (ethabi-encode [UInt256] [amount])]])

;; : Void <- TransactionReceipt [Listof Any]
(def (erc721-assert-log! receipt expectation)
  (def extracted-logs (map erc721-extracted-logger-log (.@ receipt logs)))
  (def expected (apply erc721-expected-logger-log expectation))
  (assert! (member expected extracted-logs)))

;; : Bool <- Address Address Address Bool
(def (erc721-setApprovalForAll-expected-logger-log event-signature contract sender recipient approved)
  [contract
    [event-signature
      (ethabi-encode [Address] [sender])
      (ethabi-encode [Address] [recipient])]
    (ethabi-encode [Bool] [approved])])

;; : Void <- TransactionReceipt [Listof Any]
(def (erc721-setApprovalForAll-assert-log! receipt expectation)
  (def extracted-logs (map erc20-extracted-logger-log (.@ receipt logs)))
  (def expected (apply erc721-setApprovalForAll-expected-logger-log expectation))
  (assert! (member expected extracted-logs)))

;;; Functions to interact with an ERC721 contract as a client

;; : UInt256 <- Address UInt256 requester: ? Address
(def (erc721-total-supply contract (block 'latest) requester: (requester null-address))
  (<-bytes UInt256 (eth_call (call-function requester contract totalSupply-selector) block)))

;; : UInt256 <- Address Address requester: ? Address
(def (erc721-balance contract account requester: (requester null-address))
  (!> (ethabi-encode [Address] [account] balanceOf-selector)
      (cut call-function requester contract <>)
      eth_call
      (cut <-bytes UInt256 <>)))

;; : Void <- Address Address Address UInt256
(def (erc721-approve-tx contract sender recipient tokenId)
  (!> (ethabi-encode [Address UInt256] [recipient tokenId] approve-selector)
      (cut call-function sender contract <>)
      post-transaction
      (cut erc721-assert-log! <> [Approval-event contract sender recipient tokenId])))

;;It only approves if the target is not 0
;; : Void <- Address Address Address
(def (erc721-approve contract sender recipient tokenId)
  (erc721-approve-tx contract sender recipient tokenId))

;; : Void <- Address Address Address UInt256 Bool
(def (erc721-setApprovalForAll contract sender operator approved)
  (!> (ethabi-encode [Address Bool] [operator approved] setApprovalForAll-selector)
    (cut call-function sender contract <>)
    post-transaction
    (cut erc721-setApprovalForAll-assert-log! <> [ApprovalForAll-event contract sender operator approved])))

;; : Bytes <- Address Address Address UInt256 requester: ? Address
(def (erc721-isApprovedForAll contract owner operator requester: (requester owner))
  (!> (ethabi-encode [Address Address] [owner operator] isApprovedForAll-selector)
      (cut call-function requester contract <>)
      eth_call
      (cut ethabi-decode [Bool] <>)))

;; : Void <- Address Address Address UInt256 requester: ? Address
(def (erc721-transfer-from contract sender recipient tokenId requester: (requester recipient))
  (!> (ethabi-encode [Address Address UInt256] [sender recipient tokenId] transferFrom-selector)
      (cut call-function requester contract <>)
      post-transaction
      (cut erc721-assert-log! <> [Transfer-event contract sender recipient tokenId])))

;; : Void <- Address Address Address UInt256 requester: ? Address
(def (erc721-safe-transfer-from contract sender recipient tokenId requester: (requester recipient))
  (!> (ethabi-encode [Address Address UInt256] [sender recipient tokenId] safeTransferFrom-selector)
      (cut call-function requester contract <>)
      post-transaction
      (cut erc721-assert-log! <> [Transfer-event contract sender recipient tokenId])))

;; : Void <- Address Address Address UInt256 Bytes requester: ? Address
(def (erc721-safeTransferFrom-with-data-selector contract sender recipient tokenId data requester: (requester recipient))
  (!> (ethabi-encode [Address Address UInt256 String] [sender recipient tokenId data] safeTransferFrom-with-data-selector)
      (cut call-function requester contract <>)
      post-transaction
      (cut erc721-assert-log! <> [Transfer-event contract sender recipient tokenId])))

;; : TransactionReceipt <- Address Address Address
(def (erc721-mint contract sender recipient)
  (!> (ethabi-encode [Address] [recipient] mint-selector)
      (cut call-function sender contract <>)
      post-transaction))

;; : Bytes <- Address UInt256 requester: ? Address
(def (erc721-ownerOf contract tokenId requester: (requester null-address))
  (!> (ethabi-encode [UInt256] [tokenId] ownerOf-selector)
      (cut call-function requester contract <>)
      eth_call))

;; : Bytes <- Address UInt256 requester: ? Address
(def (erc721-getApproved contract tokenId requester: (requester null-address))
  (!> (ethabi-encode [UInt256] [tokenId] getApproved-selector)
      (cut call-function requester contract <>)
      eth_call))

;; No parameter optional functions(Query)
;; : Bytes <- Address Bytes4 [Listof Any] requester: ? Address
(def (erc721-optional-fn-without-parameter contract selector return-type requester: (requester null-address))
  (!> (call-function requester contract selector)
      eth_call
      (cut ethabi-decode return-type <>)))

;; Optional functions with parameters(Query)
;; : Bytes <- Address Bytes4 [Listof Any] [Listof Any] [Listof Any] requster: ? Address
(def (erc721-optional-fn-with-parameter contract selector param-types param-values return-type requester: (requester null-address))
  (!> (ethabi-encode param-types param-values selector)
      (cut call-function requester contract <>)
      eth_call
      (cut ethabi-decode return-type <>)))
