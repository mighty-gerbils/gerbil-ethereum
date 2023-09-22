;;; Support for using and implementing an ERC165
;; https://eips.ethereum.org/EIPS/eip-165
(export #t)

(import
  :gerbil/gambit
  :std/sugar
  :clan/base
  :clan/poo/object (only-in :clan/poo/mop)
  ./types ./ethereum ./known-addresses ./abi ./json-rpc
  ./transaction ./erc20)


(def supportsInterface-selector ;;function supportsInterface(bytes4 interfaceID) external view returns (bool)
  (selector<-function-signature ["supportsInterface" Bytes4]))

;; : Bytes <- Address Bytes4 requester: ? Address
;; Query if a contract implements an interface
(def (erci65-supportsInterface contract interface-id requester: (requester null-address))
  (!> (ethabi-encode [Bytes4] [interface-id] supportsInterface-selector)
      (cut call-function requester contract <>)
      eth_call
      (cut ethabi-decode [Bool] <>)))

