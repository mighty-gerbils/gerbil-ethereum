(export #t)

(import
  :gerbil/expander
  :std/format :std/getopt :std/iter :std/misc/hash :std/sort :std/sugar
  :clan/cli :clan/decimal :clan/exit :clan/multicall :clan/path-config
  :clan/poo/object :clan/poo/brace :clan/poo/cli :clan/poo/debug
  :clan/persist/db
  ./network-config ./types ./ethereum ./known-addresses ./json-rpc)

(define-entry-point (list-ethereum-networks)
  "Show a list of available ethereum networks"
  (for-each
    (lambda (name) (displayln name)) ;; TODO: display more, more useful information, aligned.
    (sort (hash-keys ethereum-networks) string<?)))

(def (show-address-by-nickname h)
  (for/collect (([n . a] (hash->list/sort h string<?))) [n (0x<-address a)]))

(def (import-testing-module)
  #;(DDT import-testing-module-before: show-address-by-nickname address-by-nickname)
  (import-module ':mukn/ethereum/testing #t #t)
  #;(DDT import-testing-module-after: show-address-by-nickname address-by-nickname))

(def (cli-send-tx pretx confirmations: (confirmations (ethereum-confirmations-wanted-in-blocks)))
  (import-testing-module)
  ((eval 'mukn/ethereum/testing#debug-send-tx) pretx confirmations: confirmations))

(def options/test
  (make-options
   [(flag 'test "--test" help: "enable testing including test identities")]
   [(lambda-$ (when ($ test) (import-testing-module)))]))

;; TODO: allow the use of a URL instead of a name in the DB.
;; Then networkid and chainid are queried from the server.
(def options/evm-network
  (make-options
   [(option 'evm-network "-E" "--evm-network" default: #f
            help: "name of ethereum network")]
   [(lambda-$ (ensure-ethereum-connection
            (or ($ evm-network) (if ($ test) "pet" "rinkeby"))))]
   options/test))

(def options/database
  (make-options
   [(option 'database "-D" "--database" default: #f
            help: "path to local DApp state database")]
   [(lambda-$
     (ensure-db-connection (or ($ database)
                               (run-path (if ($ test) "testdb" "userdb")))))]))

(def options/from
  (make-options
   [(option 'from "-f" "--from" help: "sender (address or nickname)")] []
   [options/database options/evm-network]))

(def options/to
  (make-options
   [(option 'to "-t" "--to" help: "recipient (address or nickname)")] []
   [options/database options/evm-network]))

(define-entry-point (faucet . arguments)
  "Fund some accounts from the network faucet"
  (def args (process-options options/to arguments))
  (defrule ($ x) (hash-get args 'x))
  ;; TODO: find the faucet, use it.
  (def network (.@ (ethereum-config) network))
  (def faucets (.@ (ethereum-config) faucets))
  (cond
   ((equal? (.@ (ethereum-config) name) "Private Ethereum Testnet")
    (let ()
      (unless ($ to) (error "Missing recipient. Please use option --to"))
      (def to (parse-address ($ to)))
       ;; *after* the above, so we have croesus, but the user may have their own alice.
      (import-testing-module)
      (def value-in-ether 5)
      (def value (wei<-ether value-in-ether))
      (def token-symbol (.@ (ethereum-config) nativeCurrency symbol))
      (def from (address<-nickname "t/croesus"))
      (printf "\nSending ~a ~a from faucet ~a\n to ~a on network ~a:\n\n"
              value-in-ether token-symbol (0x<-address from) (0x<-address to) network)
      (cli-send-tx {from to value} confirmations: 0)
      (printf "\nFinal balance: ~a ~a\n\n" (decimal-string-ether<-wei (eth_getBalance to)) token-symbol)))
   ((not (null? faucets))
    (printf "\nVisit the following URL to get ethers on network ~a:\n\n\t~a\n\n"
            (car faucets) network))
   (else
    (printf "\nThere is no faucet for network ~a - Go earn tokens the hard way.\n\n" network))))

(def options/send
  (make-options
   [(option 'value "-v" "--value" help: "value to send in ether")] []
   [options/from options/to]))

(define-entry-point (transfer . arguments)
  "Send tokens from one account to the other"
  (backtrace-on-abort? #f)
  (def args (process-options options/send arguments))
  (defrule ($ x) (hash-get args 'x))
  (unless ($ from) (error "Missing sender. Please use option --from"))
  (unless ($ to) (error "Missing recipient. Please use option --to"))
  (unless ($ value) (error "Missing value. Please use option --value"))
  (def from (parse-address ($ from)))
  (def to (parse-address ($ to)))
  (def decimals (.@ (ethereum-config) nativeCurrency decimals))
  (def token-symbol (.@ (ethereum-config) nativeCurrency symbol))
  (def value (* (decimal<-string ($ value)) (expt 10 decimals))) ;; have a function for that?
  (def network (.@ (ethereum-config) network))
  (printf "\nSending ~a ~a from ~a to ~a on network ~a:\n"
          value token-symbol (0x<-address from) (0x<-address to) network)
  (printf "\nBalance before\n for ~a: ~a ~a,\n for ~a: ~a ~a\n"
          ;; TODO: use a function to correctly print with the right number of decimals,
          ;; with the correct token-symbol, depending on the current network and/or asset
          (0x<-address from) (decimal-string-ether<-wei (eth_getBalance from)) token-symbol
          (0x<-address to) (decimal-string-ether<-wei (eth_getBalance to)) token-symbol)
  (import-testing-module) ;; for debug-send-tx
  (cli-send-tx {from to value} confirmations: 0)
  (printf "\nBalance after\n for ~a: ~a ~a,\n for ~a: ~a ~a\n"
          ;; TODO: use a function to correctly print with the right number of decimals
          (0x<-address from) (decimal-string-ether<-wei (eth_getBalance from)) token-symbol
          (0x<-address to) (decimal-string-ether<-wei (eth_getBalance to)) token-symbol))
