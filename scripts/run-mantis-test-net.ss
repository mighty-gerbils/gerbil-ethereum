#!/usr/bin/env gxi
;; Run your own local private copy of Mantis as a node on localhost, for testing purposes
;; docker run -v $GERBIL_APPLICATION_HOME/run/mantis:/root/ -p 8546:8546 -it inputoutput/mantis:qa-internal-vm
;; or simply:
;; docker run -p 8545:8546 inputoutput/mantis:qa-internal-vm
;; connect via port 8546

(import
  :gerbil/gambit/exceptions :gerbil/gambit/ports :gerbil/gambit/threads
  :std/format :std/getopt :std/misc/list :std/misc/ports :std/misc/process
  :std/pregexp :std/srfi/1 :std/srfi/13 :std/sugar
  :clan/base :clan/files :clan/maybe :clan/multicall
  :clan/path :clan/path-config :clan/shell :clan/source
  :clan/net/json-rpc)

(def mantis-rpc-port 8545) ;; NOTE: Mantis by default uses 8546, while Geth uses 8545
(def docker-image "inputoutput/mantis:qa-internal-vm") ;;"inputoutput/mantis:qa"


;; If the home directory isn't otherwise set, we must be running from unconfigured source code,
;; and we'll use the top of this source code hierarchy as home.
(def here (path-simplify-directory (this-source-file)))
(home-directory-default! (cut path-parent here))


;; First, kill any previous node.

;; There are additional restrictions on a name that we don't care to check here
;; https://docs.docker.com/engine/reference/commandline/tag/
(def (parse-docker-ps-line line)
  (match (pregexp-match "^([0-9a-f]+) +([A-Za-z0-9_.-/:]+) +.* ([A-Za-z0-9_.-/:]+)$" line)
    ([_ container-id image-name container-name] [container-id image-name container-name])
    (_ #f)))

(def mantis-containers
  (append-map
   (lambda (l) (match (parse-docker-ps-line l)
            ([container-id image-name _]
             (if (equal? image-name docker-image) [container-id] []))
            (_ [])))
   (cdr (run-process ["docker" "ps"]
                     stdin-redirection: #t stdout-redirection: #t stderr-redirection: #t
                     coprocess: read-all-as-lines))))

(unless (null? mantis-containers)
  (run-process ["docker" "stop" mantis-containers ...]
               stdin-redirection: #t stdout-redirection: #t stderr-redirection: #t))

;; Determine the runtime directory, create it if needed
(def mantis-run-directory (path-expand "mantis" (run-directory)))
(create-directory* mantis-run-directory)
(current-directory mantis-run-directory)

;; Determine the data directory, clear it, thus resetting the test blockchain to zero
(def mantis-data-directory (path-expand "data" mantis-run-directory))
(run-process ["rm" "-rf" mantis-data-directory]
             stdin-redirection: #t stdout-redirection: #t stderr-redirection: #t show-console: #f)
(create-directory* mantis-data-directory)

(def mantis-logs-directory (path-expand "logs" mantis-run-directory))
(run-process ["rm" "-rf" mantis-logs-directory]
             stdin-redirection: #t stdout-redirection: #t stderr-redirection: #t show-console: #f)
(create-directory* mantis-logs-directory)

(def mantis-process
  (open-process
   [path: "docker"
    arguments: ["run" ;; "-v" (format "~a:/root/" mantis-run-directory)
                "-p" (format "~d:8546" mantis-rpc-port) ;; NB: inside the image it's always 8546
                ;;"--name" "mantis-testnet"
                ;;"-it" We do NOT want it interactive!
                "-e" (string-append "GENESIS=" (read-file-string (subpath here "genesis.json")))
                docker-image
                "bash" "-c"
                "cd / ; echo \"$GENESIS\" > genesis.json ; exec mantis -Dconfig.file=/mantis.conf"
                ]
    stdin-redirection: #f
    stdout-redirection: #f
    stderr-redirection: #f
    show-console: #f]))

(let loop ()
  (cond
   ((with-catch false
                (cut json-rpc (format "http://localhost:~d" mantis-rpc-port)
                     "web3_clientVersion" (void)))
    => (lambda (version) (printf "Connected to mantis ~a\n" version)))
   (else
    (printf "Waiting for mantis to start...\n")
    (thread-sleep! 1)
    (loop))))
