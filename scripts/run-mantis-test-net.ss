#!/usr/bin/env gxi
;; Run your own local private copy of Mantis as a node on localhost, for testing purposes
;; docker run -v $GERBIL_APPLICATION_HOME/run/mantis:/root/ -p 8546:8546 -it inputoutput/mantis:qa
;; or simply:
;; docker run -p 8545:8546 inputoutput/mantis:qa
;; connect via port 8546

(import
  :gerbil/gambit/exceptions :gerbil/gambit/ports :gerbil/gambit/threads
  :std/format :std/getopt :std/misc/list :std/misc/ports :std/misc/process :std/srfi/13 :std/sugar
  :clan/base :clan/files :clan/maybe :clan/multicall
  :clan/path :clan/path-config :clan/shell :clan/source
  :clan/net/json-rpc)

;; If the home directory isn't otherwise set, we must be running from unconfigured source code,
;; and we'll use the top of this source code hierarchy as home.
(home-directory-default! (cut path-parent (path-simplify-directory (this-source-file))))

(def mantis-rpc-port 8545) ;; NOTE: Mantis by default uses 8546, while Geth uses 8545

;; TODO: kill existing image by ID, you need to find the ID with, e.g.
;; docker ps | grep inputoutput/mantis:qa | cut -d" " -f1

;; First, kill any previous node.
(ignore-errors
 (run-process ["docker" "stop" "inputoutput/mantis:qa"]
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
                ;;"-it" We do NOT want it interactive!
                "inputoutput/mantis:qa"]
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
