(export #t)

(import
  :std/text/json
  :clan/json :clan/path-config
  :clan/poo/poo :clan/poo/io
  ./types ./signing ./ethereum)

;; TODO: move to another file.
;; TODO: for end-user reporting, use error contexts as ported from quux or cl-scripting.
;; Maybe also port that to Gerbil main, in a way backward compatible with std/test ?
(def (parse-file file parser (description #f))
  (with-catch (lambda (e) (error "while parsing" description file (error-message e)))
              (cut call-with-input-file file parser)))

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

(def (parse-ethereum-networks port)
  (<-json (List EthereumNetworkConfig) (json<-port port)))

(def ethereum-networks #f)

(def (load-ethereum-networks-config (file (config-path "ethereum_networks.json")))
  (set! ethereum-networks (parse-file file parse-ethereum-networks)))

(def (ensure-ethereum-network (network "pet"))
  (unless ethereum-networks (load-ethereum-networks-config))
  (current-ethereum-network (find (lambda (x) (equal? network (.@ x shortName))) ethereum-networks)))

(def (ethereum-rpc-config)
  (car (.@ (current-ethereum-network) rpc)))

(def (ethereum-chain-id)
  (.ref (current-ethereum-network) 'chainId (lambda _ 0)))
