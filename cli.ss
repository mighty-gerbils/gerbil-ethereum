(export #t)

(import
  :gerbil/expander
  :std/format :std/getopt :std/iter :std/misc/hash
  :std/sort :std/srfi/13 :std/sugar
  :clan/cli :clan/decimal :clan/exit :clan/hash :clan/list :clan/multicall :clan/path-config
  :clan/poo/object :clan/poo/brace :clan/poo/cli :clan/poo/debug
  :clan/persist/db
  ./network-config ./types ./ethereum ./known-addresses ./json-rpc)

(def (co-pad-strings strings)
  (def maxlen (extremum<-list < (map string-length strings) 0))
  (map (cut string-pad-right <> maxlen) strings))

(define-entry-point (list-evm-networks)
  (help: "Show a list of available EVM networks" getopt: [])
  (def keys (sort (hash-keys ethereum-networks) string<?))
  (def names (map (lambda (key) (.@ (hash-get ethereum-networks key) name)) keys))
  (def urls (map (lambda (key) (car (.@ (hash-get ethereum-networks key) rpc))) keys))
  (for-each (cut displayln <> "  " <> "  " <>)
            (co-pad-strings keys) (co-pad-strings names) urls))

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
   [(lambda (opt) (when (hash-get opt 'test) (import-testing-module)))]))

;; TODO: allow the use of a URL instead of a name in the DB.
;; Then networkid and chainid are queried from the server.
(def options/evm-network
  (make-options
   [(option 'evm-network "-E" "--evm-network" default: #f
            help: "name of EVM network")]
   [(lambda (opt) (ensure-ethereum-connection
                   (or (hash-removed opt 'evm-network)
                       (if (hash-get opt 'test) "pet" "ced"))))]
   options/test))

(def options/database
  (make-options
   [(option 'database "-D" "--database" default: #f
            help: "path to local DApp state database")]
   [(lambda (opt)
     (ensure-db-connection (or (hash-removed opt 'database)
                               (if (hash-get opt 'test) "testdb" "userdb"))))]
   [options/test]))

(def options/from
  (make-options
   [(option 'from "-f" "--from" help: "sender (address or nickname)")] []
   [options/database options/evm-network]))

(def options/to
  (make-options
   [(option 'to "-t" "--to" help: "recipient (address or nickname)")] []
   [options/database options/evm-network]))

(def options/send
  (make-options
   [(option 'value "-v" "--value" help: "decimal value to send in tokens")] []
   [options/from options/to]))

(def (parse-currency-value string currency)
  (* (decimal<-string string) (expt 10 (.@ currency decimals))))

(define-entry-point (transfer from: (from #f) to: (to #f) value: (value #f))
  (help: "Send tokens from one account to the other"
   getopt: (make-options [] [(cut hash-restrict-keys! <> '(from to value))] options/send))
  (def currency (.@ (ethereum-config) nativeCurrency))
  (def token-symbol (.@ currency symbol))
  (def network (.@ (ethereum-config) network))
  (set! from (parse-address (or from (error "Missing sender. Please use option --from"))))
  (set! to (parse-address (or to (error "Missing recipient. Please use option --to"))))
  (set! value (parse-currency-value
               (or value (error "Missing value. Please use option --value")) currency))
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
