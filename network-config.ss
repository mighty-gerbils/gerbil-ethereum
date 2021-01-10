(export #t)

(import
  :std/sugar :std/text/json
  :clan/json :clan/path-config
  :clan/poo/poo :clan/poo/io :clan/poo/brace
  ./types ./signing ./ethereum ./logger)

(define-type EthereumNetworkConfig
  (Record
   ;; These fields are from https://chainid.network/chains.json
   name: [String] ;; the name of the network
   networkId: [JsInt] ;; from JSON RPC method net_version
   chainId: [JsInt] ;; from JSON RPC method eth_chainId
   shortName: [String] ;; short name for the network, often lowercase, often a tla, e.g. "eth"
   chain: [String] ;; the chain that this network is supporting, e.g. "ETH"
   network: [String] ;; "mainnet" if that's the main net for that chain, or else a test name
   nativeCurrency: [(Record name: [String] symbol: [String] decimals: [JsInt])]
   rpc: [(List String)] ;; RPC endpoint URLs
   faucets: [(List String)] ;; Faucet website URLs
   infoURL: [String]
   ;; These are mine own additions
   description: [String] ;; a description of the network
   timeoutInBlocks: [JsInt] ;; a safe timeout, in blocks
   timeoutString: [String] ;; a string description of how long the timeout lasts
   confirmationsWantedInBlocks: [JsInt] ;; how many confirmations needed for a transaction?
   confirmationsString: [String] ;; a string description of the above
   blockPollingPeriodInSeconds: [JsInt] ;; how many seconds to wait before polling for more blocks
   eip145: [Bool] ;; is EIP-145 available on this network? (TODO: if so should be a block number)
   txExplorerUrl: [String] ;; string-append 0xTxHash for transaction information
   addressExplorerUrl: [String] ;; string-append 0xAddress for address information
   pennyCollector: [Address])) ;; who will get the pennies leftover from self-destructing contracts.

(define-type EthereumNetworkConnection
  (Record
   url: [String]
   client-version: [String]
   chain-id: [JsInt]
   server-chain-id: [JsInt]
   mantis?: [Bool]))

;; (deftype EthereumNetwork ...)
(defstruct ethereum-network
  (config ;; : EthereumNetworkConfig
   connection) ;; : EthereumNetworkConnection
  transparent: #t)

;; : (Table EthereumNetworkConfig <- String)
(def ethereum-networks (make-hash-table))

;; : (Parameter EthereumNetwork)
(def current-ethereum-network (make-parameter (ethereum-network #f #f)))

(def (ensure-ethereum-network name)
  (def config (hash-get ethereum-networks name))
  (unless config (error "Ethereum configuration not found for network" name))
  (eth-log ["EthereumNetworkConfig" (json<- EthereumNetworkConfig config)])
  ;; TODO: At first network connection, checking web3_clientVersion and eth_chainId and update the config?
  (def network (ethereum-network config #f))
  (current-ethereum-network network)
  network)

(defrule (with-ethereum-network body ...)
  (with-catch
   (lambda (_) (error "No configured ethereum node connection."
            "Did you fail to use (ensure-ethereum-connection \"pet\") ?"))
   (lambda () body ...)))

(def (ethereum-config) (ethereum-network-config (current-ethereum-network)))
(def (ethereum-config-accessor field-name)
  (lambda () (with-ethereum-network (.ref (ethereum-config) field-name))))

(def (ethereum-connection) (ethereum-network-connection (current-ethereum-network)))
(def (ethereum-connection-accessor field-name)
  (lambda () (with-ethereum-network (.ref (ethereum-connection) field-name))))

(def ethereum-confirmations-wanted-in-blocks (ethereum-config-accessor 'confirmationsWantedInBlocks))
(def ethereum-block-polling-period-in-seconds (ethereum-config-accessor 'blockPollingPeriodInSeconds))
(def ethereum-timeout-in-blocks (ethereum-config-accessor 'timeoutInBlocks))
(def ethereum-penny-collector (ethereum-config-accessor 'pennyCollector))
(def ethereum-eip145 (ethereum-config-accessor 'eip145)) ;; The Mantis KEVM is too old for it, for now.

(def ethereum-url (ethereum-connection-accessor 'url))
(def ethereum-chain-id (ethereum-connection-accessor 'chain-id))
(def ethereum-mantis? (ethereum-connection-accessor 'mantis?))

(.def production-network
  timeoutInBlocks: 11520
  timeoutString: "about 48 hours"
  confirmationsWantedInBlocks: 100
  confirmationsString: "about 25 minutes"
  blockPollingPeriodInSeconds: 5
  eip145: #t
  faucets: []
  pennyCollector: (address<-0x "0x25a49e1444fF60ADB520B5Cf20712168E74Ad24f")) ;; MuKn production penny collector

(.def shared-test-network
  timeoutInBlocks: 10
  timeoutString: "about 2.5 minutes"
  confirmationsWantedInBlocks: 2
  confirmationsString: "about 30 seconds"
  blockPollingPeriodInSeconds: 3
  eip145: #t
  pennyCollector: (address<-0x "0x1406D9B823F201F07Ea27256597CD8CCB8126579")) ;; MuKn test penny collector

(.def private-test-network
  timeoutInBlocks: 5
  timeoutString: "about a minute and a half minutes"
  confirmationsWantedInBlocks: 1
  confirmationsString: "about 15 seconds"
  blockPollingPeriodInSeconds: 1
  eip145: #f ;; sometimes we test against Mantis with an old KEVM, so we don't use EIP-145 yet.
  faucets: []
  infoURL: "https://localhost/"
  pennyCollector: (address<-0x "0xc11498Fa7fd1C261121EC856D6e0056335bcE90e")) ;; user "penny" from t/signing-test.ss

(defrules def-eth-net ()
  ((_ (name opt ...) slotspec ...)
   (let (key (symbol->string 'name))
     (hash-put! ethereum-networks key
                (.o (:: opt ...) slotspec ...))))
  ((d sym slotspec ...) (d (sym @ []) slotspec ...)))

(def-eth-net (ethereum @ production-network)
  name: "Ethereum Mainnet"
  description: "The real thing, PoW mainnet"
  shortName: "eth" chain: "ETH" network: "mainnet"
  chainId: 1 networkId: 1
  nativeCurrency: {name: "Ether" symbol: "ETH" decimals: 18}
  rpc: ["https://mainnet.infura.io/v3/${INFURA_API_KEY}"
        "https://api.mycryptoapi.com/eth"]
  infoURL: "https://ethereum.org"
  txExplorerUrl: "https://etherscan.io/tx/"
  addressExplorerUrl: "https://etherscan.io/address/")

(def-eth-net (rinkeby @ shared-test-network)
  name: "Ethereum Testnet Rinkeby"
  description: "Rinkeby, the public Geth-only PoA testnet"
  chainId: 4 networkId: 4
  shortName: "rin" chain: "ETH" network: "rinkeby"
  nativeCurrency: {name: "Rinkeby Ether" symbol: "RIN" decimals: 18}
  rpc: ["https://rinkeby.infura.io/v3/${INFURA_API_KEY}"]
  faucets: ["https://faucet.rinkeby.io"]
  infoURL: "https://www.rinkeby.io"
  txExplorerUrl: "https://rinkeby.etherscan.io/tx/"
  addressExplorerUrl: "https://rinkeby.etherscan.io/address/")

(def-eth-net (kotti @ shared-test-network)
  name: "Ethereum Classic Testnet Kotti"
  description: "ETC PoA testnet"
  networkId: 6 chainId: 6
  shortName: "kot" chain: "ETC" network: "kotti"
  nativeCurrency: {name: "Kotti Ether" symbol:"KOT" decimals: 18}
  rpc: []
  faucets: [] ;; TODO: find the faucet
  infoURL: "https://explorer.jade.builders/?network=kotti"
  txExplorerUrl: "https://blockscout.com/etc/kotti/tx/"
  addressExplorerUrl: "https://blockscout.com/etc/kotti/address/")

(def-eth-net (david @ shared-test-network)
  name: "Cardano KEVM Devnet"
  description: "Cardano side-chain with Mantis KEVM client devnet"
  networkId: 41390 chainId: 105
  ;; The two lines below are made up by us -- TODO: find out if there's a better name
  shortName: "david" network: "david" chain: "Cardano"
  nativeCurrency: {name: "Cardano KEVM Devnet Ether" symbol: "DAVID" decimals: 18}
  eip145: #f ;; until the KEVM is updated, it won't support eip145 yet.
  rpc: ["https://david.kevm.dev-mantis.iohkdev.io:8546"]
  faucets: [] ;; TODO: find the faucet
  infoURL: "https://developers.cardano.org/en/virtual-machines/kevm/getting-started/using-the-kevm-devnet/"
  txExplorerUrl: "https://david.kevm.dev-mantis.iohkdev.io/tx/"
  addressExplorerUrl: "https://david.kevm.dev-mantis.iohkdev.io/address/")

(def-eth-net (pet @ private-test-network)
  name: "Private Ethereum Testnet"
  description: "Local PoA testnet"
  networkId: 17 chainId: 1337
  shortName: "pet" chain: "ETH" network: "petnet"
  nativeCurrency: {name: "Private Ether Test" symbol: "PET" decimals: 18}
  rpc: ["http://localhost:8545"]
  txExplorerUrl: "https://localhost/pet/pet/tx/"
  addressExplorerUrl: "https://localhost/pet/pet/address/")
