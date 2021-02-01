(export #t)

(import
  :gerbil/expander
  :std/getopt :std/sort
  :clan/cli :clan/multicall :clan/path-config
  :clan/persist/db
  ./network-config ./json-rpc)

(define-entry-point (list-ethereum-networks)
  "Show a list of available ethereum networks"
  (for-each
    (lambda (name) (displayln name))
    (sort (hash-keys ethereum-networks) string<?)))

(def option/test
  (make-option-spec
   [(flag 'test "--test" help: "enable testing including test identities")]
   (lambda-opt (when {test} (import-module ':mukn/ethereum/testing #t #t)))))

(def option/ethereum-network
  (make-option-spec
   [(option 'ethereum-network "-E" "--ethereum-network" default: "pet"
            help: "name of ethereum network")]
   (lambda-opt (ensure-ethereum-connection {ethereum-network}))))

(def option/database
  (make-option-spec
   [(option 'database "-D" "--database" default: #f
            help: "path to local DApp state database")]
   (lambda-opt
    (ensure-db-connection (or {database}
                              (run-path (if {test} "testdb" "userdb")))))))

#;(def (process-ethereum-options opt
                               connect-ethereum?: (connect-ethereum? #t)
                               connect-database?: (connect-ethereum? #t))
  ((lambda-opt
    ...)
   opt))

#;(define-entry-point (faucet . arguments)
  "Fund some accounts from the network faucet"
  (def getopt-spec [])
  ;; TODO: find the faucet, use it.
  (void))
