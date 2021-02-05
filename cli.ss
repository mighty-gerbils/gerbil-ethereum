(export #t)

(import
  :gerbil/expander
  :std/format :std/getopt :std/sort :std/sugar
  :clan/cli :clan/decimal :clan/multicall :clan/path-config
  :clan/poo/object :clan/poo/brace :clan/poo/cli
  :clan/persist/db
  ./network-config ./ethereum ./signing ./known-addresses ./json-rpc)

(define-entry-point (list-ethereum-networks)
  "Show a list of available ethereum networks"
  (for-each
    (lambda (name) (displayln name))
    (sort (hash-keys ethereum-networks) string<?)))

(def (import-testing-module)
  (import-module ':mukn/ethereum/testing #t #t))

(def options/test
  (make-options
   [(flag 'test "--test" help: "enable testing including test identities")]
   [(lambda-opt (when {test} (import-testing-module)))]))

(def options/ethereum-network
  (make-options
   [(option 'ethereum-network "-E" "--ethereum-network" default: #f
            help: "name of ethereum network")]
   [(lambda-opt (ensure-ethereum-connection
            (or {ethereum-network} (if {test} "pet" "rinkeby"))))]
   options/test))

(def options/database
  (make-options
   [(option 'database "-D" "--database" default: #f
            help: "path to local DApp state database")]
   (lambda-opt
    (ensure-db-connection (or {database}
                              (run-path (if {test} "testdb" "userdb")))))))

(def options/from
  (make-options
   [(option 'from "-f" "--from" help: "sender (address or nickname)")] []
   [options/ethereum-network]))

(def options/to
  (make-options
   [(option 'to "-t" "--to" help: "recipient (address or nickname)")] []
   [options/ethereum-network]))

(define-entry-point (faucet . arguments)
  "Fund some accounts from the network faucet"
  (def opt (process-options options/to arguments))
  (defrule [x] (hash-get opt 'x)) ;; make is so [x] accesses the option.
  ;; TODO: find the faucet, use it.
  (def network (.@ (ethereum-config) network))
  (def faucets (.@ (ethereum-config) faucets))
  (cond
   ((equal? (.@ (ethereum-config) name) "Private Ethereum Testnet")
    (let ()
      (def to (parse-address [to]))
       ;; *after* the above, so we have croesus, but the user may have their own alice.
      (import-testing-module)
      (def value-in-ethers 5)
      (def token-symbol (.@ (ethereum-config) nativeCurrency symbol))
      (printf "\nSending ~a ~a to ~a on network ~a:\n\n"
              value-in-ethers token-symbol (0x<-address to) network)
      ((eval 'send-debug-tx) {from: (eval 'croesus) to: to value: (wei<-ether 5)})
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

(define-entry-point (send . arguments)
  "Fund some accounts from the network faucet"
  (def opt (process-options options/send arguments))
  (defrule [x] (hash-get opt 'x)) ;; make is so [x] accesses the option.
  ;; TODO: find the faucet, use it.
  (def from (parse-address [from]))
  (def to (parse-address [to]))
  (def decimals (.@ (ethereum-config) nativeCurrency decimals))
  (def token-symbol (.@ (ethereum-config) nativeCurrency symbol))
  (def value (* (decimal<-string [value]) (expt 10 decimals))) ;; have a function for that?
  (def network (.@ (ethereum-config) network))
  (printf "\nSending ~a ~a from ~a to ~a on network ~a:\n"
          value token-symbol (0x<-address from) (0x<-address to) network)
  (printf "\nBalance before for ~a: ~a, for ~a: ~a\n"
          ;; TODO: use a function to correctly print with the right number of decimals
          (0x<-address from) (decimal-string-ether<-wei (eth_getBalance from)) token-symbol
          (0x<-address to) (decimal-string-ether<-wei (eth_getBalance to)) token-symbol)
  (import-testing-module) ;; for send-debug-tx
  ((eval 'send-debug-tx) {from to value})
  (printf "\nBalance after for ~a: ~a, for ~a: ~a\n"
          ;; TODO: use a function to correctly print with the right number of decimals
          (0x<-address from) (decimal-string-ether<-wei (eth_getBalance from)) token-symbol
          (0x<-address to) (decimal-string-ether<-wei (eth_getBalance to)) token-symbol))
