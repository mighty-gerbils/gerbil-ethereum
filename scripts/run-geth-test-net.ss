#!/usr/bin/env gxi
;; Run your own local private copy of Go-Ethereum as a node on localhost, for testing purposes

;; TODO: learn from the following script: https://github.com/dapphub/dapptools/blob/master/src/dapp/libexec/dapp/dapp---testnet-launch
;; TODO: implement another script that starts an entire network with many nodes and some given geometry

(import
  :gerbil/gambit/exceptions :gerbil/gambit/ports :gerbil/gambit/threads
  :std/format :std/getopt :std/misc/list :std/misc/ports :std/misc/process :std/srfi/13 :std/sugar
  :clan/base :clan/files :clan/maybe :clan/multicall
  :clan/path :clan/path-config :clan/shell :clan/source
  :clan/net/json-rpc)

(def geth-port 30303) ;; port on which geth will be talking to peers(?)
(def geth-rpc-port 8545) ;; port on which geth will be listening to RPC requests

;; If the home directory isn't otherwise set, we must be running from unconfigured source code,
;; and we'll use the top of this source code hierarchy as home.
(def here (path-simplify-directory (this-source-file)))
(home-directory-default! (cut path-parent here))


;; We use `--dev.period 1` to prevent the `geth` miner from pausing
;; in the absence of pending transactions to process,
;; e.g. in the case of demo/test suite timeouts.
;;
;; Using the nominal block generation speed design goal one should expect to see
;; on the main net is painfully slow for demos/testing, so here we default to "1",
;; but CI should invoke this script with "12" for correctness' sake.
;;
;; https://blog.ethereum.org/2014/07/11/toward-a-12-second-block-time/
(def geth-dev-period 1)

;; First, kill any previous node.
(ignore-errors (run-process/batch ["killall" "geth"]))

;; Determine the runtime directory, create it if needed
(def geth-run-directory (path-expand "geth" (run-directory)))
(create-directory* geth-run-directory)
(current-directory geth-run-directory)

;; Determine the data directory, clear it, thus resetting the test blockchain to zero
(def geth-data-directory (path-expand "data" geth-run-directory))
(run-process/batch ["rm" "-rf" geth-data-directory])
(create-directory* geth-data-directory)

(def geth-logs-directory (path-expand "logs" geth-run-directory))
(run-process/batch ["rm" "-rf" geth-logs-directory])
(create-directory* geth-logs-directory)

(def geth-arguments
  ["--datadir" geth-data-directory
   "--identity" "GlowEthereumPrivateTestNet"
   "--verbosity" "4" ;; 3: info, 4: debug
   "--etherbase" "0x25c0bb1A5203AF87869951AEf7cF3FEdD8E330fC"
   "--nodiscover"
   "--maxpeers" "0"
   "--nousb"
   "--networkid" "17"
   "--nat" "any"
   "--vmdebug"])

(def (geth-command . args)
  (def cmd (escape-shell-tokens ["geth" geth-arguments ... args ...]))
  (format "(echo ~a ; ~a) < /dev/null >> ~a/geth.log 2>&1"
          cmd cmd geth-logs-directory))
(def (run-geth . args)
  (run-process/batch ["sh" "-c" (apply geth-command args)]))
(def (bg-geth args)
  (open-process
   [path: "sh" arguments: ["-c" (apply geth-command args)]
    stdin-redirection: #f
    stdout-redirection: #f
    stderr-redirection: #f
    show-console: #f]))

(run-geth
 "--unlock" "0x25c0bb1A5203AF87869951AEf7cF3FEdD8E330fC"
 "account" "import" "--password" "/dev/null" (subpath here "croesus.prv"))
(run-geth "init" (subpath here "genesis.json"))
(bg-geth
 ["--dev"
  (when/list (and geth-dev-period (< 0 geth-dev-period))
             ["--dev.period" (number->string geth-dev-period)])...
  "--fakepow" "--mine"
  "--http" "--http.api" "db,eth,net,debug,web3,light,personal,admin"
  "--http.port" (number->string geth-rpc-port) "--http.corsdomain" "*"
  "--port" (number->string geth-port)
  "--ipcpath" (subpath geth-run-directory "geth.ipc")])

(let loop ()
  (cond
   ((with-catch false
                (cut json-rpc (format "http://localhost:~d" geth-rpc-port)
                     "web3_clientVersion" (void)))
    => (lambda (version) (printf "Connected to geth ~a\n" version)))
   (else
    (printf "Waiting for geth to start...\n")
    (thread-sleep! 1)
    (loop))))
