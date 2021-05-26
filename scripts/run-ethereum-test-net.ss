#!/usr/bin/env gxi
;; Run your own local private copy of an Ethereum as a node on localhost, for testing purposes,
;; with either Geth  or Mantis
;;
;; To setup Geth
;; =============
;; If you use Nix to install Glow and its Gerbil dependencies, you can also use it to install geth:
;;
;; nixpkgs=https://github.com/muknio/nixpkgs/archive/alpha.tar.gz # Or whichever nixpkgs you use
;; nix-env -f $nixpkgs -iA go-ethereum
;;
;; Otherwise, see your distribution, e.g. apt install geth or brew install ethereum, or
;; follow the instructions at https://geth.ethereum.org/downloads/
;;
;;
;; To setup Mantis
;; ===============
;; See https://hub.docker.com/r/inputoutput/mantis for the latest available images
;; docker run -v $GERBIL_APPLICATION_HOME/run/mantis:/root/ -p 8546:8546 -it inputoutput/mantis:2020-kevm
;; or simply:
;; docker run -p 8545:8546 inputoutput/mantis:2020-kevm
;; connect via port 8546
;; To exec
;; docker exec -it $(run-mantis-test-net.ss mantis-container) bash

;; TODO: make it so that script is runnable from installation directories.
;; This implies able to find any ancillary data files it uses.

(import
  (for-syntax :std/misc/ports :clan/syntax)
  :gerbil/expander
  :gerbil/gambit/exceptions :gerbil/gambit/os :gerbil/gambit/ports :gerbil/gambit/threads
  :std/format :std/getopt :std/misc/list :std/misc/ports :std/misc/process :std/misc/string
  :std/pregexp :std/srfi/1 :std/srfi/13 :std/sugar :std/text/hex
  :clan/base :clan/files :clan/json :clan/maybe :clan/multicall
  :clan/path :clan/path-config :clan/shell :clan/syntax :clan/temporary-files
  :clan/net/json-rpc
  :clan/crypto/secp256k1
  :clan/persist/db
  :mukn/ethereum/hex :mukn/ethereum/ethereum :mukn/ethereum/known-addresses
  :mukn/ethereum/json-rpc :mukn/ethereum/testing :mukn/ethereum/test-contracts)


(with-catch void (cut import-module ':mukn/ethereum/version #t #t))

;; Let's share the configuration and data directories with the rest of the Glow ecosystem
(set! application-name (lambda () "glow"))

;; User-configurable variables
(def eth-rpc-port 8545) ;; NOTE: Mantis by default uses 8546, while Geth uses 8545
(def default-node "geth")

;; If the home directory isn't otherwise set, we must be running from unconfigured source code,
;; and we'll use the top of this source code hierarchy as home.
#;(def here (path-simplify-directory (syntax-call stx-source-file)))
#;(set-path-config-root! (subpath (path-parent here) "run/"))

(def (eth-rpc-url) (format "http://localhost:~d" eth-rpc-port))


;;;; Common support for both geth and mantis

;; Wipe out the runtime directory, thus resetting the test blockchain to zero,
;; and recreate an empty directory.
(define-entry-point (wipe-state-directories)
  (help: "Wipe any run directory" getopt: [])
  (nest (for-each! [(data-path "geth")
                    (persistent-path "testdb")
                    (log-path "geth")
                    (log-path "mantis")]) (lambda (path))
        (when (file-exists? path))
        (begin
          (unless (any (cut string-contains path <>)
                       '("/db/" "/log/" "/cache/" "/.cache/" "/data/"
                         "/.local/share/" "/Library/" "/LocalAppData/"))
            (error "Not resetting fishy state directory" path)))
        (ignore-errors)
        (run-process/batch ["rm" "-rf" path])))

(define-entry-point (wait-for-ethereum (name "ethereum"))
  (help: "Wait for the ethereum server to be ready to handle requests"
   getopt: [(optional-argument 'name help: "nickname of the network to wait for")])
  (let loop ()
    (cond
     ((with-catch false
                  (cut json-rpc (eth-rpc-url)
                       "web3_clientVersion" (void)))
      => (lambda (version) (printf "Connected to ~a\n" version)))
     (else
      (printf "Waiting for ~a to start at ~a ...\n" name (eth-rpc-url))
      (thread-sleep! 1)
      (loop)))))

(define-entry-point (start (type default-node))
  (help: "Start a fresh ethereum server"
   getopt: [(optional-argument 'type help: "type of server, 'geth' or 'mantis'" default: default-node)])
  (match (string-downcase type)
    ("geth" (start-geth))
    ("mantis" (start-mantis))
    (_ (error "Unrecognized ethereum server type" type))))

(define-entry-point (stop)
  (help: "Stop and wipe any current ethereum server" getopt: [])
  (stop-geth)
  (stop-mantis))

;; There are additional restrictions on a name that we don't care to check here
;; https://docs.docker.com/engine/reference/commandline/tag/
(define-entry-point (parse-docker-ps-line line)
  (help: "Parse a docker ps line"
   getopt: [(argument 'line help: "line to parse")])
  (match (pregexp-match "^([0-9a-f]+) +([-A-Za-z0-9_.-/:]+) +.* ([-A-Za-z0-9_./:]+)$" line)
    ([_ container-id image-name container-name] [container-id image-name container-name])
    (_ #f)))

;;;; Support for Geth

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

;; Determine the runtime directories
(def geth-data-directory (data-path "geth"))
(def geth-logs-directory (log-path "geth"))

(def geth-arguments
  ["--datadir" geth-data-directory
   "--identity" "GlowPrivateEthereumTestNet"
   "--verbosity" "4" ;; 3: info, 4: debug
   "--miner.etherbase" (0x<-address croesus)
   "--nodiscover"
   "--maxpeers" "0"
   "--nousb"
   "--networkid" "17"
   "--nat" "any"
   "--vmdebug"])

;;(def geth-port 30303) ;; port on which geth will be talking to peers(?)

(def (geth-command . args)
  (def cmd (escape-shell-tokens ["geth" geth-arguments ... args ...]))
  (format "(echo ~a ; ~a) < /dev/null >> ~a/geth.log 2>&1"
          cmd cmd geth-logs-directory))
(def (run-geth . args)
  (displayln "Running: " (apply geth-command args))
  (run-process/batch ["sh" "-c" (apply geth-command args)]))
(def (bg-geth args)
  (displayln "Running in background: " (apply geth-command args))
  (open-process
   [path: "sh" arguments: ["-c" (apply geth-command args)]
    stdin-redirection: #f
    stdout-redirection: #f
    stderr-redirection: #f
    show-console: #f]))

(def (get-geth-version)
  (def version (cadr (run-process ["geth" "version"] 
                                  stdin-redirection: #t stdout-redirection: #t stderr-redirection: #t
                                  coprocess: read-all-as-lines)))
  (string-trim-prefix "Version: " version))

(def (extra-geth-options)
  (def major 1)
  (def minor 10)
  (def splitted (string-split (get-geth-version) #\.))
  (if (or (< (string->number (car splitted)) major) 
          (< (string->number (cadr  splitted)) minor))
      ""
      "--rpc.allow-unprotected-txs=true"))

(define-entry-point (start-geth)
  (help: "Start a go-ethereum server, wiping any previous run data" getopt: [])
  ;; Zeroth, erase any previous blockchain data and accompanying testdb, and create new directories
  (stop)
  (wipe-state-directories)
  (create-directory* geth-data-directory)
  (create-directory* geth-logs-directory)
  ;; First, register the private key for croesus, so he can sign POA endorsements
  (call-with-temporary-file
   prefix: "croesus-tmp-" suffix: ".prv"
   while-open:
   (lambda (port _path)
     (display (hex-encode (secp256k1-seckey-data (keypair-secret-key croesus-keys))) port))
   after-close:
   (lambda (croesus.prv)
     (run-geth
      "--unlock" (0x<-address croesus)
      "account" "import" "--password" "/dev/null" croesus.prv)))
  ;; Second, initialize the state of the blockchain
  (call-with-temporary-file
   prefix: "genesis-tmp-" suffix: ".json"
   while-open:
   (lambda (port _path)
     (display (syntax-call (lambda (ctx) (read-file-string (stx-source-path ctx "genesis.json")))) port))
   after-close:
   (lambda (genesis.json)
     (run-geth "init" genesis.json)))
  ;; Then, run a geth server in the background
  (bg-geth
   ["--dev"
    (when/list (and geth-dev-period (< 0 geth-dev-period))
               ["--dev.period" (number->string geth-dev-period)])...
    "--fakepow" "--mine"
    "--http" "--http.api" "admin,db,debug,eth,light,net,personal,web3"
    "--http.port" (number->string eth-rpc-port)
    "--http.corsdomain" "https://remix.ethereum.org,http://remix.ethereum.org"
    (extra-geth-options)
    ;;"--rpc.allow-unprotected-txs=true" ;; allow the meta-create2 presigned signature
    ;;"--port" (number->string geth-port)
    "--vmdebug"
    "--ipcpath" (subpath geth-data-directory "geth.ipc")])
  ;; Finally, wait for the server to be ready to process requests
  (wait-for-ethereum "geth")
  (initialize-test-contracts))

(define-entry-point (initialize-test-contracts)
  (help: "Initialize contracts used during testing" getopt: [])
  (ensure-ethereum-connection "pet")
  (ensure-db-connection "testdb")
  (register-test-keys)
  (ensure-test-contracts))

(define-entry-point (stop-geth)
  (help: "Stop any currently running geth server" getopt: [])
  (ignore-errors (run-process/batch
                  ["killall" (cond-expand (linux ["-q"]) (else []))... "geth"])))

(define-entry-point (geth)
  (help: "alias for start-geth" getopt: [])
  (start-geth))

;;;; Support for Mantis: see the top of this file for setup instructions.
(def mantis-docker-image "inputoutput/mantis:2020-evm") ;; NB: there are both -evm and -kevm variants
(def mantis-yolo-conf "yolo-evm.conf") ;; our override file, also with -evm or -kevm
(def mantis-log-directory (log-path "mantis")) ;; Determine the log directory for mantis
;;(def mantis-data-directory (persistent-path "mantis")) ;; Determine the runtime directory
;; NB: When editing the configuration, compare to what's in production:
;; https://github.com/input-output-hk/mantis/blob/develop/src/main/resources/chains/etc-chain.conf
;; https://github.com/etclabscore/core-geth/blob/master/params/config_classic.go#L30

(define-entry-point (mantis-containers)
  (help: "List current Mantis docker containers" getopt: [])
  (append-map
   (lambda (l) (match (parse-docker-ps-line l)
            ([container-id image-name _]
             (if (equal? image-name mantis-docker-image) [container-id] []))
            (_
             [])))
   (cdr (run-process ["docker" "ps"]
                     stdin-redirection: #t stdout-redirection: #t stderr-redirection: #t
                     coprocess: read-all-as-lines))))

(define-entry-point (mantis-container)
  (help: "Print name of first Mantis docker container, if any" getopt: [])
  (def mcs (mantis-containers))
  (unless (null? mcs) (displayln (car mcs))))

(define-entry-point (stop-mantis)
  (help: "Stop any currently running Mantis docker container" getopt: [])
  (ignore-errors
    (def containers (mantis-containers))
    (unless (null? containers)
      (printf "Killing container ~a\n" (string-join containers " "))
      (run-process/batch ["docker" "stop" containers ...])
      (run-process/batch ["docker" "wait" containers ...]))))

;;; NB: We could have a log directory outside it and symlink the builtin path to it, but oh well.

;;; Do we actually need to tweak the genesis? We shouldn't need that anymore.
#;(def genesis
  (let (h (json<-string (syntax-call (lambda (ctx) (read-file-string (stx-source-path ctx "genesis.json"))))))
  (hash-remove! h "config")
  (hash-put! h "extraData" "0x11bbe8db4e347b4e8c937c1c8370e4b5ed33adb3db69cbdb7a38e1e50b1b82fa")
  h))

(define-entry-point (run-mantis)
  (help: "Start a Mantis docker image in the background" getopt: [])
  #;(create-directory* mantis-data-directory)
  (create-directory* mantis-log-directory)
  ;;; NB: I'd like to use this, but Docker seems to have no such option :-( Big security risk!
  ;;(def opt (let (u (user-info (user-name))) (format ":uid=~d,gid=~d" (user-info-uid u) (user-info-gid u))))
  (open-process
   [path: "docker"
    arguments: ["run"
                ;; We mount this directory so we can easily copy files to override configuration
                ;;"-v" (format "~a:/here" here)

                ;; Uncomment this line if you want to visibly persist the state on your local disk.
                ;; Beware: docker will create those files *owned by root*
                #;#;"-v" (format "~a:/root/.mantis" mantis-data-directory)
                ;; You could also try to mount some kind of persistent docker volume instead.

                ;; The ethash miner creates a reusable 1GB DAG file at startup.
                ;; By keeping it between runs, we save 10-20 minutes of initialization time
                ;; Beware: Docker will create it owned by root, but you should be able to chown it.
                "-v" (format "~a:/root/.ethash" (cache-path "ethash"))

                ;; Redirect the logs
                "-v" (format "~a:/root/.mantis/logs" (log-path "mantis"))

                ;; Outside the image, We use eth-rpc-port. *inside*, we let /conf say it's 8546.
                "-p" (format "~d:8546" eth-rpc-port)

                ;; We could try to persist the state of the docker container, but
                ;; we don't want to and that would require more management above.
                #;#;"--name" "mantis-testnet"

                ;;;; If we wanted an interactive terminal, we'd use that.
                #;"-it"

                ;;;; If we needed to pass parameters from host to image, we could use this:
                #;#;"-e" (format "GENESIS=~a" (string<-json genesis)) ;; not needed anymore

                ;;;; This is the image name. End of options, after that start of command to run.
                mantis-docker-image

                ;;;; Command to run
                "bash" ;; We could stop at bash for the interactive use (then see "-it" above).

                ;;;; Script to run at startup
                "-c" (string-append
                  "cd / && "

                  ;;;; This shouldn't be needed in the current image
                  #;"echo \"$GENESIS\" > /conf/genesis.json && "

                  ;;;; Make a backup of the configuration files we're going to override
                  "cp /conf/yolo.conf /here/" mantis-yolo-conf ".orig && "
                  ;;;; This file enables tracing of EVM on the 2020-evm image
                  #;"cp /conf/logback.xml /here/logback.xml.orig && "

                  ;;;; Override the IOG-provided configuration files
                  "cat /here/" mantis-yolo-conf " > /conf/yolo.conf && "
                  #;"cat /here/logback.xml > /conf/logback.xml && "

                  ;;;; Final command
                  "exec mantis")
                ]
    stdin-redirection: #f
    stdout-redirection: #f
    stderr-redirection: #f
    show-console: #f]))

(define-entry-point (start-mantis)
  (help: "Start a fresh Mantis docker image, wiping any previous run data" getopt: [])
  (stop)
  (wipe-state-directories)
  (run-mantis)
  (wait-for-ethereum "mantis")
  (initialize-test-contracts))

(define-entry-point (mantis)
  (help: "alias for start-mantis" getopt: [])
  (start-mantis))

#; ;; TODO: get mallet to work?
(define-entry-point (mallet)
  (help: "Run mallet against the Mantis docker image" getopt: [])
  ;; Download mallet at: https://github.com/input-output-hk/mallet
  ;; Works best with node 10.x.x
  (run-process/batch ["mallet" (eth-rpc-url) (string-append "--datadir=" mantis-data-directory)]))

(def initial-supply (expt 10 36))

(current-program "run-ethereum-test-net")
(set-default-entry-point! 'start)
(define-multicall-main)
