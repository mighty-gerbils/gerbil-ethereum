(export #t)

(import
  :std/format :std/sugar :std/text/json
  :clan/json :clan/path-config
  :clan/poo/object :clan/poo/io :clan/poo/brace
  ./types ./ethereum ./logger)

(define-type EthereumExplorerConfig
  (Record name: [String] url: [String] standard: [String]))

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
   explorers: [(List EthereumExplorerConfig)]
   infoUrl: [String]
   ;; These are mine own additions
   description: [String] ;; a description of the network
   targetBlockTime: [JsInt default: 13] ;; target time duration between blocks
   timeoutInBlocks: [JsInt] ;; a safe timeout, in blocks
   timeoutString: [String] ;; a string description of how long the timeout lasts
   confirmationsWantedInBlocks: [JsInt] ;; how many confirmations needed for a transaction?
   confirmationsString: [String] ;; a string description of the above
   blockPollingPeriodInSeconds: [JsInt] ;; how many seconds to wait before polling for more blocks
   eip145: [Bool default: #t] ;; is EIP-145 available on this network? (TODO: if so should be a block number)
   pennyCollector: [Address])) ;; who will get the pennies leftover from self-destructing contracts.

(define-type EthereumNetworkConnection
  (Record
   url: [String]
   client-version: [String]
   network-id: [JsInt]
   server-network-id: [JsInt]
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
  faucets: ? []
  rpc: ? []
  explorers: ? []
  eip145: ? #t
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
  infoUrl: "https://localhost/"
  pennyCollector: (address<-0x "0xC0773c13b36eB92813aFE5e89EE89b633c5B1F15")) ;; user "penny" from testing.ss

(defrules def-eth-net ()
  ((_ (name opt ...) slotspec ...)
   (let ((key (symbol->string 'name))
         (config {(:: opt ...) slotspec ...}))
     (hash-put! ethereum-networks key config)
     (hash-put! ethereum-networks (.@ config shortName) config)
     (hash-put! ethereum-networks (.@ config network) config)))
  ((d sym slotspec ...) (d (sym @ []) slotspec ...)))

(def has-explorer
  (case-lambda ((url) (has-explorer "explorer" url))
          ((name url)
           (def url-fun (if (procedure? url) url (lambda _ url)))
           {(:: @ []) explorers: => (cut cons {name url: (url-fun @) standard: "EIP3091"} <>)})))

(def has-etherscan
  (has-explorer "etherscan" (lambda (@) (def-slots (network) @)
                               (if (equal? network "mainnet") "https://etherscan.io"
                                   (format "https://~a.etherscan.io" network)))))

(.def (has-infura @ [] network)
  rpc: => (cut cons* (format "https://~a.infura.io/v3/${INFURA_API_KEY}" network)
               (format "wss://~a.infura.io/ws/v3/${INFURA_API_KEY}" network) <>))

(def-eth-net (ethereum @ [production-network has-infura has-etherscan])
  name: "Ethereum Mainnet"
  description: "The real thing, PoW mainnet"
  shortName: "eth" chain: "ETH" network: "mainnet" chainId: 1 networkId: 1
  nativeCurrency: {name: "Ether" symbol: 'ETH decimals: 18}
  rpc: => (cut cons "https://api.mycryptoapi.com/eth" <>)
  targetBlockTime: 13
  infoUrl: "https://ethereum.org")

(def-eth-net (etc @ [production-network has-etherscan (has-explorer "https://etcblockexplorer.com/")])
  name: "Ethereum Classic Mainnet"
  description: "The original fork"
  shortName: "etc" chain: "ETC" network: "etcmainnet" chainId: 61 networkId: 1
  nativeCurrency: {name: "Ethereum Classic Ether" symbol: 'ETC decimals: 18}
  rpc: ["https://ethereumclassic.network" "https://www.ethercluster.com/etc"]
  targetBlockTime: 13
  infoUrl: "https://ethereumclassic.org")

(def-eth-net (ropsten @ [shared-test-network has-etherscan has-infura])
  name: "Ethereum Testnet Ropsten"
  shortName: "rop" chain: "ETH" network: "ropsten" chainId: 3 networkId: 3
  nativeCurrency: {name: "Ropsten Ether" symbol: 'ROP decimals: 18}
  faucets: ["https://faucet.ropsten.be/"
            "https://faucet.ropsten.be/donate/${ADDRESS}"]
  infoUrl: "https://github.com/ethereum/ropsten")

(def-eth-net (rinkeby @ [shared-test-network has-etherscan has-infura])
  name: "Ethereum Testnet Rinkeby"
  description: "Rinkeby, the public PoA (clique) testnet, Geth, Besu, Nethermind and OpenEthereum only"
  shortName: "rin" chain: "ETH" network: "rinkeby" chainId: 4 networkId: 4
  nativeCurrency: {name: "Rinkeby Ether" symbol: 'RIN decimals: 18}
  faucets: ["https://faucet.rinkeby.io"]
  infoUrl: "https://www.rinkeby.io")

(def-eth-net (kovan @ [shared-test-network has-etherscan has-infura])
  name: "Ethereum Testnet Kovan"
  description: "Kovan, the public PoA (authority round) testnet, OpenEthereum and Nethermind only"
  shortName: "kov" chain: "ETH" network: "kovan" chainId: 42 networkId: 42
  nativeCurrency: {name: "Kovan Ether" symbol: 'KOV decimals: 18}
  rpc: ["https://kovan.poa.network" "http://kovan.poa.network:8545" "ws://kovan.poa.network:8546"]
  faucets: ["https://faucet.kovan.network" "https://gitter.im/kovan-testnet/faucet"]
  infoUrl: "https://kovan-testnet.github.io/website")

(def-eth-net (goerli @ [shared-test-network has-etherscan has-infura])
  name: "Ethereum Testnet Görli"
  description: "Goerli, the public PoA (authority round) testnet, OpenEthereum and Nethermind only"
  shortName: "gor" chain: "ETH" network: "goerli" chainId: 5 networkId: 5
  nativeCurrency: {name: "Görli Ether" symbol: 'GOR decimals: 18}
  rpc: ["https://www.ethercluster.com/goerli" "ws://goerli.poa.network:8546" "https://rpc.goerli.mudit.blog/" "https://rpc.slock.it/goerli" "https://goerli.prylabs.net/"]
  faucets: ["https://goerli-faucet.slock.it/" "https://faucet.goerli.mudit.blog/"]
  ;;ens:{registry:"0x112234455c3a32fd11230c42e7bccd4a84e02010"}
  infoUrl: "https://goerli.net/")

(def-eth-net (kotti @ [shared-test-network (has-explorer "https://blockscout.com/etc/kotti/")])
  name: "Ethereum Classic Testnet Kotti"
  description: "ETC PoA testnet"
  shortName: "kot" chain: "ETC" network: "kotti" networkId: 6 chainId: 6
  nativeCurrency: {name: "Kotti Ether" symbol: 'KOT decimals: 18}
  rpc: ["https://www.ethercluster.com/kotti"]
  faucets: [] ;; TODO: find the faucet
  infoUrl: "https://explorer.jade.builders/?network=kotti")

(def-eth-net (ced @ [shared-test-network
                     (has-explorer "https://explorer-evm.portal.dev.cardano.org")])
  name: "Cardano EVM Devnet"
  description: "Cardano side-chain with Mantis EVM client devnet"
  networkId: 103 chainId: 103
  ;; The two lines below are made up by us -- TODO: find out if there are better names
  shortName: "ced" chain: "Cardano" network: "ced"
  nativeCurrency: {name: "Cardano EVM Devnet Ether" symbol: 'CED decimals: 18}
  rpc: ["https://rpc-evm.portal.dev.cardano.org/"]
  faucets: ["https://faucet-evm.portal.dev.cardano.org/"]
  web-faucets: ["https://faucet-web-evm.portal.dev.cardano.org/"]
  infoUrl: "https://developers.cardano.org/en/virtual-machines/kevm/getting-started/using-the-kevm-devnet/")

(def-eth-net (pet @ private-test-network)
  name: "Private Ethereum Testnet"
  description: "Local PoA testnet"
  shortName: "pet" chain: "ETH" network: "petnet" networkId: 17 chainId: 1212
  nativeCurrency: {name: "Private Ether Test" symbol: 'PET decimals: 18}
  targetBlockTime: 1
  rpc: [(getenv "PET_RPC_URL" "http://localhost:8545")])
