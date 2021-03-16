(export #t)

(import
  :gerbil/expander
  :std/format :std/getopt :std/iter :std/misc/hash
  :std/sort :std/srfi/13 :std/sugar
  :clan/cli :clan/decimal :clan/exit :clan/hash :clan/multicall :clan/path-config :clan/string
  :clan/poo/object :clan/poo/brace :clan/poo/cli :clan/poo/debug
  :clan/persist/db
  ./network-config ./types ./ethereum ./known-addresses ./json-rpc)

;; Let's share the configuration and data directories with the rest of the Glow ecosystem
(set! application-name (lambda () "glow"))

(define-entry-point (list-evm-networks)
  (help: "Show a list of available EVM networks" getopt: [])
  (def keys (sort (hash-keys ethereum-networks) string<?))
  (def names (map (lambda (key) (.@ (hash-get ethereum-networks key) name)) keys))
  (def urls (map (lambda (key) (with-catch (lambda (_) "") (cut car (.@ (hash-get ethereum-networks key) rpc))))
                 keys))
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
   [options/test options/help]))

(def options/database
  (make-options
   [(option 'database "-D" "--database" default: #f
            help: "path to local DApp state database")]
   [(lambda (opt)
     (ensure-db-connection (or (hash-removed opt 'database)
                               (if (hash-get opt 'test) "testdb" "userdb"))))]
   [options/test options/help]))

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
