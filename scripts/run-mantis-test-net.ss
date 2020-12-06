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
  :clan/base :clan/files :clan/json :clan/maybe :clan/multicall
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
  (run-process/batch ["docker" "stop" mantis-containers ...])
  (run-process/batch ["docker" "wait" mantis-containers ...]))

;; Determine the runtime directory, wipe it clear, thus resetting the test blockchain to zero,
;; and recreate an empty one anew.
(def mantis-run-directory (path-expand "mantis" (run-directory)))
(run-process/batch ["rm" "-rf" mantis-run-directory])
(create-directory* mantis-run-directory)
(current-directory mantis-run-directory)

;;; NB: We could have a log directory outside it and symlink the builtin path to it, but oh well.

;; Do we actually need to tweak the genesis? Let's figure it out once we're done.
(def genesis
  (let (h (json<-string (read-file-string (subpath here "genesis.json"))))
    (hash-remove! h "config")
    (hash-put! h "extraData" "0x11bbe8db4e347b4e8c937c1c8370e4b5ed33adb3db69cbdb7a38e1e50b1b82fa")
    h))

(def mantis-process
  (open-process
   [path: "docker"
    arguments: ["run"
                ;; Doing the mount of /? works... but then bad things happen when I stop docker to restart it differently, resulting in files that cannot be removed. Sigh.
                ;;"-v" (format "~a:/?" mantis-run-directory) ;; NB: Bug in current docker image, /? is where Scala believe the ${user.home} is, because it's too stupid to believe $HOME and there's no /etc/passwd
                "-p" (format "~d:8546" mantis-rpc-port) ;; NB: inside the image it's always 8546
                ;;"--name" "mantis-testnet" ;; NB: We could use something like that to persist the state inside the docker container, but we don't want to and that would require more management above.
                ;;"-it" ;; If we wanted an interactive terminal, we'd use that.
                "-e" (format "GENESIS=~a" (string<-json genesis))
                docker-image
                "bash" ;; We could stop at bash for the interactive use.
                "-c" "cd / ; echo \"$GENESIS\" > genesis.json ; exec mantis -Dconfig.file=/mantis.conf"
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
