#!/usr/bin/env gxi
;; Run your own local private copy of an Ethereum as a node on localhost, for testing purposes,
;; either Geth or Mantis.

(import
  :gerbil/gambit/exceptions :gerbil/gambit/os :gerbil/gambit/ports :gerbil/gambit/threads
  :std/format :std/getopt :std/misc/list :std/misc/ports :std/misc/process :std/misc/string
  :std/pregexp :std/srfi/1 :std/srfi/13 :std/sugar
  :clan/base :clan/files :clan/json :clan/maybe :clan/multicall
  :clan/path :clan/path-config :clan/shell :clan/source
  :clan/net/json-rpc)

;; User-configurable variables
(def eth-rpc-port 8545) ;; NOTE: Mantis by default uses 8546, while Geth uses 8545
(def default-node "geth")

;; If the home directory isn't otherwise set, we must be running from unconfigured source code,
;; and we'll use the top of this source code hierarchy as home.
(def here (path-simplify-directory (this-source-file)))
(home-directory-default! (cut path-parent here))

(def (eth-rpc-url) (format "http://localhost:~d" eth-rpc-port))


;;;; Common support for both geth and mantis

;; Wipe out the runtime directory, thus resetting the test blockchain to zero,
;; and recreate an empty directory.
(define-entry-point (wipe-run-directory)
  "Wipe any run directory"
  (unless (string-suffix? "/run" (run-directory))
    ;; TODO: once there are production directories, insert test so that we don't wipe one.
    (error "Not resetting fishy run-directory" (run-directory)))
  (for-each
    (lambda (sub)
      (def path (run-path sub))
      (assert! (string-contains path "/run/"))
      (when (file-exists? path)
        (ignore-errors (run-process/batch ["rm" "-rf" path]))))
    ["geth" "mantis" "testdb" "t" "log"]))

(define-entry-point (wait-for-ethereum (name "ethereum"))
  "Wait for the ethereum server to be ready to handle requests"
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

(define-entry-point (start (name default-node))
  (format "Start a fresh ethereum server (default: ~a)" default-node)
  (match (string-downcase name)
    ("geth" (start-geth))
    ("mantis" (start-mantis))
    (_ (error "Unrecognized ethereum server name" name))))

(define-entry-point (stop)
  "Stop and wipe any current ethereum server"
  (stop-geth)
  (stop-mantis))

;; There are additional restrictions on a name that we don't care to check here
;; https://docs.docker.com/engine/reference/commandline/tag/
(define-entry-point (parse-docker-ps-line line)
  "Parse a docker ps line"
  (match (pregexp-match "^([0-9a-f]+) +([-A-Za-z0-9_.-/:]+) +.* ([-A-Za-z0-9_./:]+)$" line)
    ([_ container-id image-name container-name] [container-id image-name container-name])
    (_ #f)))

(set-default-entry-point! "start")
(def main call-entry-point)

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
(def geth-run-directory (run-path "geth"))
(def geth-data-directory (run-path "geth/data"))
(def geth-logs-directory (run-path "geth/logs"))

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

;;(def geth-port 30303) ;; port on which geth will be talking to peers(?)

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

(define-entry-point (start-geth)
  "Start a go-ethereum server, wiping any previous run data"
  ;; Zeroth, erase any previous blockchain data and accompanying testdb, and create new directories
  (stop)
  (wipe-run-directory)
  (create-directory* geth-run-directory)
  (create-directory* geth-data-directory)
  (create-directory* geth-logs-directory)
  ;;;;(current-directory geth-run-directory)
  ;; First, register the private key for croesus, so he can sign POA endorsements
  (run-geth
   "--unlock" "0x25c0bb1A5203AF87869951AEf7cF3FEdD8E330fC"
   "account" "import" "--password" "/dev/null" (subpath here "croesus.prv"))
  ;; Second, initialize the state of the blockchain
  (run-geth "init" (subpath here "genesis.json"))
  ;; Then, run a geth server in the background
  (bg-geth
   ["--dev"
    (when/list (and geth-dev-period (< 0 geth-dev-period))
               ["--dev.period" (number->string geth-dev-period)])...
    "--fakepow" "--mine"
    "--http" "--http.api" "admin,db,debug,eth,light,net,personal,web3"
    "--http.port" (number->string eth-rpc-port)
    "--http.corsdomain" "https://remix.ethereum.org,http://remix.ethereum.org"
    ;;"--port" (number->string geth-port)
    "--vmdebug"
    "--ipcpath" (subpath geth-run-directory "geth.ipc")])
  ;; Finally, wait for the server to be ready to process requests
  (wait-for-ethereum "geth"))

(define-entry-point (stop-geth)
  "Stop any currently running geth server"
  (ignore-errors (run-process/batch
                  ["killall" (cond-expand (linux ["-q"]) (else []))... "geth"])))

(define-entry-point (geth)
  "alias for start-geth"
  (start-geth))

;;;; Support for Mantis

;; See https://hub.docker.com/r/inputoutput/mantis for the latest available images
;; docker run -v $GERBIL_APPLICATION_HOME/run/mantis:/root/ -p 8546:8546 -it inputoutput/mantis:2020-kevm
;; or simply:
;; docker run -p 8545:8546 inputoutput/mantis:2020-kevm
;; connect via port 8546
;; To exec
;; docker exec -it $(run-mantis-test-net.ss mantis-container) bash
(def mantis-docker-image "inputoutput/mantis:2020-kevm") ;; NB: there are both -evm and -kevm variants
(def mantis-yolo-conf "yolo-kevm.conf") ;; our override file, also with -evm or -kevm
(def mantis-run-directory (run-path "mantis")) ;; Determine the runtime directory

(define-entry-point (mantis-containers)
  "List current Mantis docker containers"
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
  "Print name of first Mantis docker container, if any"
  (def mcs (mantis-containers))
  (unless (null? mcs) (displayln (car mcs))))

(define-entry-point (stop-mantis)
  "Stop any currently running Mantis docker container"
  (ignore-errors
    (def containers (mantis-containers))
    (unless (null? containers)
      (printf "Killing container ~a\n" (string-join containers " "))
      (run-process/batch ["docker" "stop" containers ...])
      (run-process/batch ["docker" "wait" containers ...]))))

;;; NB: We could have a log directory outside it and symlink the builtin path to it, but oh well.

;;; Do we actually need to tweak the genesis? We shouldn't need that anymore.
;;(def genesis
;;  (let (h (json<-string (read-file-string (subpath here "genesis.json"))))
;;    (hash-remove! h "config")
;;    (hash-put! h "extraData" "0x11bbe8db4e347b4e8c937c1c8370e4b5ed33adb3db69cbdb7a38e1e50b1b82fa")
;;    h))

(define-entry-point (run-mantis)
  "Start a Mantis docker image in the background"
  (create-directory* mantis-run-directory)
  ;;; NB: I'd like to use this, but Docker seems to have no such option :-( Big security risk!
  ;;(def opt (let (u (user-info (user-name))) (format ":uid=~d,gid=~d" (user-info-uid u) (user-info-gid u))))
  (open-process
   [path: "docker"
    arguments: ["run"
                ;; Mantis will create files under /root/.mantis and /root/.ethash, *owned by root* (!)
                ;;;;"-v" (format "~a:/root" mantis-run-directory) ;; followed by :${options} if needed
                "-v" (format "~a:/here" here)
                ;; Outside the image, use eth-rpc-port. *inside*, /conf says it's 8546.
                "-p" (format "~d:8546" eth-rpc-port)
                ;; We could try to persist the state inside the docker container, but
                ;; we don't want to and that would require more management above.
                ;;;;"--name" "mantis-testnet"
                ;; If we wanted an interactive terminal, we'd use that.
                ;;;;"-it"
                ;; If we needed to pass parameters from host to image, we could use this:
                ;;;;"-e" (format "GENESIS=~a" (string<-json genesis)) ;; not needed anymore
                mantis-docker-image
                ;; If we need to run a command before mantis...
                ;;;; "bash" ;; We could stop at bash for the interactive use.
                ;; This shouldn't be needed in the current image
                ;;;;"-c" "cd / ; echo \"$GENESIS\" > /conf/genesis.json ; exec mantis -Dconfig.file=/conf/yolo.conf"
                ;;;;"sh" "-c" "cd / && cat /here/yolo-evm.conf > /conf/yolo.conf && exec mantis"
                "sh" "-c" (format "cd / && cp /conf/yolo.conf /here/~a.orig ; cat /here/~a > /conf/yolo.conf && exec mantis" mantis-yolo-conf mantis-yolo-conf)
                ]
    stdin-redirection: #f
    stdout-redirection: #f
    stderr-redirection: #f
    show-console: #f]))

(define-entry-point (start-mantis)
  "Start a fresh Mantis docker image, wiping any previous run data"
  (stop)
  (wipe-run-directory)
  (run-mantis)
  (wait-for-ethereum "mantis"))

(define-entry-point (mantis)
  "alias for start-mantis"
  (start-mantis))

(define-entry-point (get-mantis-log (tx #f))
  "copy the mantis log to run/mantis/mantis.log"
  (def container-log (format "~a:/root/.mantis/logs/mantis.log" (car (mantis-containers))))
  (def host-log (run-path "mantis/mantis.log"))
  (run-process/batch ["docker" "cp" container-log host-log])
  (when tx
    (let (thex (string-trim-prefix "0x" tx))
      ;; No pass-through interactive execution from gambit :-(
      #;(run-process ["less" "-p" thex host-log] stdin-redirection: #t stdout-redirection: #t stderr-redirection: #t show-console: #t)
      (ignore-errors (run-process/batch ["grep" thex host-log]))
      (printf "less -p ~a ~a\n" thex host-log))))

;; TODO: get mallet to work?
#;(define-entry-point (mallet)
  "Run mallet against the Mantis docker image"
  ;; Download mallet at: https://github.com/input-output-hk/mallet
  ;; Works best with node 10.x.x
  (run-process/batch ["mallet" (eth-rpc-url) (string-append "--datadir=" mantis-run-directory)]))
