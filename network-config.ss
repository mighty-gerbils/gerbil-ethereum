(export #t)

(import
  :std/sugar :std/text/json
  :clan/json :clan/path-config
  :clan/poo/object :clan/poo/io :clan/poo/brace
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
   nativeCurrency: [(Record name: [String] symbol: [Symbol] decimals: [JsInt])]
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

(.def network-defaults
  blockPollingPeriodInSeconds: ? 5
  chainId: ? 0
  eip145: ? #t
  faucets: ? []
  pennyCollector: ? (address<-0x "0xC0113C7863cb7c972Bf6BEa2899D3dbae74a9a65")) ;; MuKn production penny collector

(.def (production-network @ network-defaults)
  timeoutInBlocks: 11520
  timeoutString: "about 48 hours"
  confirmationsWantedInBlocks: 100
  confirmationsString: "about 25 minutes")

(.def (shared-test-network @ network-defaults)
  timeoutInBlocks: 10
  timeoutString: "about 2.5 minutes"
  confirmationsWantedInBlocks: 2
  confirmationsString: "about 30 seconds"
  blockPollingPeriodInSeconds: 3
  pennyCollector: (address<-0x "0xc0773C1073863E8AA4B4b61548B64409F6A60E26")) ;; MuKn test penny collector

(.def (private-test-network @ network-defaults)
  timeoutInBlocks: 5
  timeoutString: "about a minute and a half minutes"
  confirmationsWantedInBlocks: 1
  confirmationsString: "about 15 seconds"
  blockPollingPeriodInSeconds: 1
  eip145: #t ;; NB: This will not work against a Mantis using an old KEVM, where you should use #f
  infoURL: "https://localhost/"
  pennyCollector: (address<-0x "0xC0773c13b36eB92813aFE5e89EE89b633c5B1F15")) ;; user "penny" from testing.ss

(defrules def-eth-net ()
  ((_ (name opt ...) slotspec ...)
   (let ((key (symbol->string 'name))
         (config {(:: opt ...) slotspec ...}))
     (hash-put! ethereum-networks key config)
     (hash-put! ethereum-networks (.@ config shortName) config)
     (hash-put! ethereum-networks (.@ config network) config)))
  ((d sym slotspec ...) (d (sym @ []) slotspec ...)))

(.def (etherscanable @ [] network)
  etherscanUrl:
  (apply string-append ["https://" (if (equal? network "mainnet") [] [network "."])... "etherscan.io/"])
  txExplorerUrl: (string-append etherscanUrl "tx/")
  addressExplorerUrl: (string-append etherscanUrl "address/"))

(def-eth-net (ethereum @ [production-network etherscanable])
  name: "Ethereum Mainnet"
  description: "The real thing, PoW mainnet"
  shortName: "eth" chain: "ETH" network: "mainnet" chainId: 1 networkId: 1
  nativeCurrency: {name: "Ether" symbol: 'ETH decimals: 18}
  rpc: ["https://mainnet.infura.io/v3/${INFURA_API_KEY}"
        "https://api.mycryptoapi.com/eth"]
  infoURL: "https://ethereum.org")

(def-eth-net (ropsten @ [shared-test-network etherscanable])
  name: "Ethereum Testnet Ropsten"
  shortName: "rop" chain: "ETH" network: "ropsten" chainId: 3 networkId: 3
  nativeCurrency: {name: "Ropsten Ether" symbol: "ROP" decimals: 18}
  rpc: ["https://ropsten.infura.io/v3/${INFURA_API_KEY}"
        "wss://ropsten.infura.io/ws/v3/${INFURA_API_KEY}"]
  faucets: ["https://faucet.ropsten.be/"
            "https://faucet.ropsten.be/donate/${ADDRESS}"]
  infoURL: "https://github.com/ethereum/ropsten")

(def-eth-net (rinkeby @ [shared-test-network etherscanable])
  name: "Ethereum Testnet Rinkeby"
  description: "Rinkeby, the public PoA (clique) testnet, Geth, Besu, Nethermind and OpenEthereum only"
  shortName: "rin" chain: "ETH" network: "rinkeby" chainId: 4 networkId: 4
  nativeCurrency: {name: "Rinkeby Ether" symbol: 'RIN decimals: 18}
  rpc: ["https://rinkeby.infura.io/v3/${INFURA_API_KEY}"]
  faucets: ["https://faucet.rinkeby.io"]
  infoURL: "https://www.rinkeby.io")

(def-eth-net (kovan @ [shared-test-network etherscanable])
  name: "Ethereum Testnet Kovan"
  description: "Kovan, the public PoA (authority round) testnet, OpenEthereum and Nethermind only"
  shortName: "kov" chain: "ETH" network: "kovan" chainId: 42 networkId: 42
  nativeCurrency: {name: "Kovan Ether" symbol: 'KOV decimals: 18}
  rpc: ["https://kovan.poa.network" "http://kovan.poa.network:8545" "https://kovan.infura.io/v3/${INFURA_API_KEY}" "wss://kovan.infura.io/ws/v3/${INFURA_API_KEY}" "ws://kovan.poa.network:8546"]
  faucets: ["https://faucet.kovan.network" "https://gitter.im/kovan-testnet/faucet"]
  infoURL: "https://kovan-testnet.github.io/website")

(def-eth-net (goerli @ [shared-test-network etherscanable])
  name: "Optimistic Ethereum Testnet Goerli"
  description: "Goerli, the public PoA (authority round) testnet, OpenEthereum and Nethermind only"
  shortName: "ogor" chain: "ETH" network: "goerli" chainId: 420 networkId: 420
  nativeCurrency: {name: "Görli Ether" symbol: 'GOR decimals: 18}
  rpc: ["https://goerli.infura.io/v3/${INFURA_API_KEY}" "wss://goerli.infura.io/ws/v3/${INFURA_API_KEY}" "ws://goerli.poa.network:8546"]
  faucets: ["https://goerli-faucet.slock.it/" "https://faucet.goerli.mudit.blog/"]
  infoURL: "https://goerli.net/")

(def-eth-net (kotti @ shared-test-network)
  name: "Ethereum Classic Testnet Kotti"
  description: "ETC PoA testnet"
  shortName: "kot" chain: "ETC" network: "kotti" networkId: 6 chainId: 6
  nativeCurrency: {name: "Kotti Ether" symbol: 'KOT decimals: 18}
  rpc: []
  faucets: [] ;; TODO: find the faucet
  infoURL: "https://explorer.jade.builders/?network=kotti"
  txExplorerUrl: "https://blockscout.com/etc/kotti/tx/"
  addressExplorerUrl: "https://blockscout.com/etc/kotti/address/")

(def-eth-net (david @ shared-test-network)
  name: "Cardano KEVM Devnet"
  description: "Cardano side-chain with Mantis KEVM client devnet"
  networkId: 41390 chainId: 105
  ;; The two lines below are made up by us -- TODO: find out if there are better names
  shortName: "david" chain: "Cardano" network: "david"
  nativeCurrency: {name: "Cardano KEVM Devnet Ether" symbol: 'DAVID decimals: 18}
  eip145: #f ;; until the KEVM is updated, it won't support eip145 yet.
  rpc: ["https://david.kevm.dev-mantis.iohkdev.io:8546"]
  faucets: [] ;; TODO: find the faucet
  infoURL: "https://developers.cardano.org/en/virtual-machines/kevm/getting-started/using-the-kevm-devnet/"
  txExplorerUrl: "https://david.kevm.dev-mantis.iohkdev.io/tx/"
  addressExplorerUrl: "https://david.kevm.dev-mantis.iohkdev.io/address/")

(def-eth-net (pet @ private-test-network)
  name: "Private Ethereum Testnet"
  description: "Local PoA testnet"
  shortName: "pet" chain: "ETH" network: "petnet" networkId: 17 chainId: 1337
  nativeCurrency: {name: "Private Ether Test" symbol: 'PET decimals: 18}
  rpc: ["http://localhost:8545"]
  txExplorerUrl: "https://localhost/pet/pet/tx/"
  addressExplorerUrl: "https://localhost/pet/pet/address/")
