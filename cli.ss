(export #t)

(import
  :gerbil/expander
  :std/format :std/getopt :std/iter
  :std/misc/decimal :std/misc/hash :std/misc/list
  :std/sort :std/srfi/13 :std/sugar
  :clan/cli :clan/exit :clan/hash :clan/list
  :clan/multicall :clan/path-config :clan/string
  :clan/poo/object :clan/poo/brace :clan/poo/cli :clan/poo/debug
  :clan/persist/db
  ./network-config ./types ./ethereum ./known-addresses ./json-rpc ./testing)

;; Let's share the configuration and data directories with the rest of the Glow ecosystem
(set! application-name (lambda () "glow"))

(def (compare-strings-by-length-then-lexicographically< x y)
  (def lx (string-length x))
  (def ly (string-length y))
  (cond
   ((< lx ly) #t)
   ((> lx ly) #f)
   (else (string< x y))))

(define-entry-point (list-evm-networks)
  (help: "Show a list of available EVM networks" getopt: [])
  (def keys (sort (hash-keys ethereum-networks) string<?))
  (def (name<-key key) (.@ (hash-get ethereum-networks key) name))
  (def keys-by-name (map (cut sort <> compare-strings-by-length-then-lexicographically<)
                         (group-same keys key: name<-key)))
  (def keys-strings (map (cut string-join <> " ") keys-by-name))
  (def primary-keys (map car keys-by-name))
  (def urls (map (lambda (key) (with-catch (lambda (_) "") (cut car (.@ (hash-get ethereum-networks key) rpc))))
                 primary-keys))
  (def names (map name<-key primary-keys))
  (for-each (cut displayln <> "  " <> "  " <>)
            (co-pad-strings keys-strings) (co-pad-strings names) urls))

(def (show-address-by-nickname h)
  (for/collect (([n . a] (hash->list/sort h string<?))) [n (0x<-address a)]))

(def (cli-send-tx pretx confirmations: (confirmations (ethereum-confirmations-wanted-in-blocks)))
  (debug-send-tx pretx confirmations: confirmations))

(def options/test
  (make-options
   [(flag 'test "--test" help: "enable testing including test identities")]
   [(lambda (opt) (when (hash-get opt 'test) (register-test-keys)))]))

(def options/test-only
  (make-options [] [(lambda (opt) (hash-put! opt 'test #t) (register-test-keys))]))

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
  (* (string->decimal string group-separator: #\,)
     (expt 10 (.@ currency decimals))))
