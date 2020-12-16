(export #t)

(import
  :std/text/json
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

(def current-ethereum-network (make-parameter #f))

(def ethereum-networks #f)

(def (load-ethereum-networks-config (file (config-path "ethereum_networks.json")))
  (set! ethereum-networks (parse-json-file file (.@ (List EthereumNetworkConfig) .<-json))))

(def (ensure-ethereum-network (name "pet"))
  (unless ethereum-networks (load-ethereum-networks-config))
  (def config (find (lambda (x) (equal? name (.@ x shortName))) ethereum-networks))
  (eth-log ["EthereumNetworkConfig" (json<- EthereumNetworkConfig config)])
  ;; TODO: At first network connection, checking web3_clientVersion and eth_chainId and update the config?
  (current-ethereum-network config))

(def (ethereum-rpc-config)
  (car (.@ (current-ethereum-network) rpc)))

(def (ethereum-chain-id)
  (.ref (current-ethereum-network) 'chainId (lambda _ 0)))
