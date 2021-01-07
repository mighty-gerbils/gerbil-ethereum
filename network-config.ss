(export #t)

(import
  :std/sugar :std/text/json
  :clan/json :clan/path-config
  :clan/poo/poo :clan/poo/io
  ./types ./signing ./ethereum ./logger)

(define-type EthereumNetworkConfig
  (Record
   ;; These fields are from https://chainid.network/chains.json
   name: [String] ;; the name of the network
   networkId: [JsInt]
   chainId: [JsInt]
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

(defstruct ethereum-network
  (config ;; : EthereumNetworkConfig
   connection) ;; : EthereumNetworkConnection
  transparent: #t)

(def ethereum-networks #f)
(def (load-ethereum-networks-config (file (config-path "ethereum_networks.json")))
  (set! ethereum-networks (parse-json-file file (.@ (List EthereumNetworkConfig) .<-json))))

(def current-ethereum-network (make-parameter (ethereum-network #f #f)))
(def (ensure-ethereum-network name)
  (unless ethereum-networks (load-ethereum-networks-config))
  (def config (find (lambda (x) (equal? name (.@ x shortName))) ethereum-networks))
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

(def ethereum-url (ethereum-connection-accessor 'url))
(def ethereum-chain-id (ethereum-connection-accessor 'chain-id))
(def ethereum-mantis? (ethereum-connection-accessor 'mantis?))
