;;; Initialization for the PET network

(export #t)

(import
  (for-syntax :std/text/hex
              :std/misc/path
              :std/misc/ports
              :std/stxutil)
  :std/stxutil
  :gerbil/gambit
  :std/assert
  :std/format
  :std/iter
  :std/srfi/13
  :std/source
  :std/sugar
  :clan/exception
  :clan/multicall
  :clan/syntax
  :clan/poo/object
  :clan/poo/cli
  :clan/persist/db
  ./types
  ./ethereum
  ./network-config
  ./json-rpc
  ./abi
  ./transaction
  ./tx-tracker
  ./testing
  ./meta-create2
  ./assets
  ./cli
  ./testing)

(def initial-supply (expt 10 27))

;; NB: based on creator, nonce
(def QASPET@ (address<-0x "0x8e0BE69f202e245221B6D679B58faaBe1e463100"))
(def RBTPET@ (address<-0x "0x9FC8935f73cf1481729FE787669c558a30E5B44B"))
(def HAMPET@ (address<-0x "0x990E192133b8A98e229b4f22Fc0C3c1F4d88162E"))
;; These have the same address as above, because they are created
;; using CREATE from same creator and nonce. However, unlike the above,
;; they are only created once (per reset of the CED network, which happened once already).
(def QASCED@ QASPET@)
(def RBTCED@ RBTPET@)
(def HAMCED@ HAMPET@)

;; NB: We wire-in those contracts into the binary, so run-ethereum-test-net does not depend on
;; the source code being available at runtime to locate the test files.
(begin-syntax
  (def (stx-source-hex stx relpath)
    (hex-decode (bytes->string (stx-source-content stx relpath)))))
(defsyntax-call (this-source-hex x relpath) (stx-source-hex x relpath))

(def (test-erc20-contract-bytes)
  (this-source-hex "t/precompiled/ERC20PresetFixedSupply.bin"))
(def (test-erc721-contract-bytes)
  (this-source-hex "t/precompiled//ERC721PresetMinterPauserAutoId.bin"))

(defkeys ensure-test-contracts
  (initializer "0x9a0685cf801c0b16a839ec9c28b7dc7f461e70f3d33307f3a15da1d68c7f9d83"))

(define-entry-point (ensure-test-contracts from: (from #f) test: (_ #t))
  (help: "initialize test contracts"
   getopt: (make-options [] [] [options/from options/test-only]))
  (def owner (or from croesus))
  (def name (string-upcase (.@ (ethereum-config) name)))
  (def net (.@ (ethereum-config) shortName))
  (def NET (string-upcase net))
  (def (QAS-bytes)
    (ethabi-encode [String String UInt256 Address]
                   [(string-append "Quality Assurance Specie on " name)
                    (string-append "QAS" NET)
                    initial-supply owner] ;; one billion total tokens with 1e-18 precision
                   (test-erc20-contract-bytes)))
  (def (RBT-bytes)
    (ethabi-encode [String String UInt256 Address]
                   [(string-append "Random Barter Token on " name)
                    (string-append "RBT" NET)
                    initial-supply owner] ;; one billion total tokens with 1e-18 precision
                   (test-erc20-contract-bytes)))
  (def (HAM-bytes)
    (ethabi-encode [String String String]
                   [(string-append "Crypto-Hamsters on " name)
                    (string-append "HAM" NET)
                    (format "https://ham~a.mukn.io/" net)]
                   (test-erc721-contract-bytes)))

  (displayln "Creating Universal CREATE2 wrapper...")
  (ensure-presigned-create2-wrapper)

  ;; Trying to create the OpenZeppelin ERC20 contract with CREATE2 fails(!?)
  #;(def (QASPET (salt 0)) (abi-create2 croesus (bytes<- UInt256 salt) (QASPET-bytes)))

  (def transactions
    [["Funding initializer" #f (transfer-tokens from: croesus to: initializer value: (wei<-ether 1))]
     ["Creating ~a (ERC20)" "QAS" (create-contract initializer (QAS-bytes))]
     ["Creating ~a (ERC20)" "RBT" (create-contract initializer (RBT-bytes))]
     ["Creating ~a (ERC721)" "HAM" (create-contract initializer (HAM-bytes))]])

  (assert! (equal? (eth_getTransactionCount initializer) 0))
  (try
   (for (([msg name tx] transactions)
         (i (in-naturals)))
     (def NAME (and name (string-append name NET)))
     (displayln (if NAME (format msg NAME) msg) "...")
     (def receipt (post-transaction tx))
     (def contract (.@ receipt contractAddress))
     (when (address? contract)
       (printf "... ~a contract created at address ~a\n" NAME (0x<-address contract))))
   (catch (TransactionFailed? e)
     (display-exception (TransactionFailed-exception e))
     (error "fail!")))
  (assert! (equal? (eth_getTransactionCount initializer) 3)))


(def PET (lookup-asset 'PET))
(def CED (lookup-asset 'CED))

(define-type (QASPET @ ERC20)
  .contract-address: QASPET@
  .name: "Quality Assurance Specie on Private Ethereum Testnet"
  .symbol: 'QASPET
  .decimals: 18
  .network: 'pet)

(define-type (RBTPET @ ERC20)
  .contract-address: RBTPET@
  .name: "Random Barter Token on Private Ethereum Testnet"
  .symbol: 'RBTPET
  .decimals: 18
  .network: 'pet)

(define-type (QASCED @ ERC20)
  .contract-address: QASCED@
  .name: "Quality Assurance Specie on Private Ethereum Testnet"
  .symbol: 'QASCED
  .decimals: 18
  .network: 'ced)

(define-type (RBTCED @ ERC20)
  .contract-address: RBTCED@
  .name: "Random Barter Token on Private Ethereum Testnet"
  .symbol: 'RBTCED
  .decimals: 18
  .network: 'ced)


(for-each register-asset!
          [QASPET RBTPET
           QASCED RBTCED])
