#!/usr/bin/env gxi
;; Run your own local private copy of Mantis as a node on localhost, for testing purposes
;; docker run -v $GERBIL_APPLICATION_HOME/run/mantis:/root/ -p 8546:8546 -it inputoutput/mantis:2020-kevm
;; or simply:
;; docker run -p 8545:8546 inputoutput/mantis:2020-kevm
;; connect via port 8546
;; To exec
;; docker exec -it $(run-mantis-test-net.ss mantis-container) bash

(import
  :gerbil/gambit/exceptions :gerbil/gambit/os :gerbil/gambit/ports :gerbil/gambit/threads
  :std/format :std/getopt :std/misc/list :std/misc/ports :std/misc/process
  :std/pregexp :std/srfi/1 :std/srfi/13 :std/sugar
  :clan/base :clan/files :clan/json :clan/maybe :clan/multicall
  :clan/path :clan/path-config :clan/shell :clan/source
  :clan/net/json-rpc)

(def mantis-rpc-port 8545) ;; NOTE: Mantis by default uses 8546, while Geth uses 8545
(def docker-image "inputoutput/mantis:2020-kevm")


;; If the home directory isn't otherwise set, we must be running from unconfigured source code,
;; and we'll use the top of this source code hierarchy as home.
(def here (path-simplify-directory (this-source-file)))
(home-directory-default! (cut path-parent here))
(def mantis-run-directory (path-expand "mantis" (run-directory))) ;; Determine the runtime directory


;; There are additional restrictions on a name that we don't care to check here
;; https://docs.docker.com/engine/reference/commandline/tag/
(define-entry-point (parse-docker-ps-line line)
  "Parse a docker ps line"
  (match (pregexp-match "^([0-9a-f]+) +([-A-Za-z0-9_.-/:]+) +.* ([-A-Za-z0-9_./:]+)$" line)
    ([_ container-id image-name container-name] [container-id image-name container-name])
    (_ #f)))

(define-entry-point (mantis-containers)
  "List current mantis docker containers"
  (append-map
   (lambda (l) (match (parse-docker-ps-line l)
            ([container-id image-name _]
             (if (equal? image-name docker-image) [container-id] []))
            (_
             [])))
   (cdr (run-process ["docker" "ps"]
                     stdin-redirection: #t stdout-redirection: #t stderr-redirection: #t
                     coprocess: read-all-as-lines))))

(define-entry-point (mantis-container)
  "Print name of first mantis container, if any"
  (def mcs (mantis-containers))
  (unless (null? mcs)
    (displayln (car mcs))))

(define-entry-point (kill-mantis)
  "Kill any currently running mantis docker image"
  (def containers (mantis-containers))
  (unless (null? containers)
    (printf "Killing container ~a\n" (string-join containers " "))
    (run-process/batch ["docker" "stop" containers ...])
    (run-process/batch ["docker" "wait" containers ...])))

;; Wipe out the runtime directory, thus resetting the test blockchain to zero,
;; and recreate an empty directory.
(define-entry-point (reset-mantis)
  "Reset the mantis directory"
  (unless (string-contains mantis-run-directory "/run/mantis")
    (error "Not resetting fishy mantis-run-directory" mantis-run-directory))
  (run-process/batch ["rm" "-rf" mantis-run-directory]))

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
                ;; Outside the image, use mantis-rpcport. *inside*, /conf says it's 8546.
                "-p" (format "~d:8546" mantis-rpc-port)
                ;; We could try to persist the state inside the docker container, but
                ;; we don't want to and that would require more management above.
                ;;;;"--name" "mantis-testnet"
                ;; If we wanted an interactive terminal, we'd use that.
                ;;;;"-it"
                ;; If we needed to pass parameters from host to image, we could use this:
                ;;;;"-e" (format "GENESIS=~a" (string<-json genesis)) ;; not needed anymore
                docker-image
                ;; If we need to run a command before mantis...
                ;;;; "bash" ;; We could stop at bash for the interactive use.
                ;; This shouldn't be needed in the current image
                ;;;;"-c" "cd / ; echo \"$GENESIS\" > /conf/genesis.json ; exec mantis -Dconfig.file=/conf/yolo.conf"
                ]
    stdin-redirection: #f
    stdout-redirection: #f
    stderr-redirection: #f
    show-console: #f]))

(def (mantis-url)
  (format "http://localhost:~d" mantis-rpc-port))

(define-entry-point (wait-for-mantis)
  "Wait for the mantis server to be ready to handle requests"
  (let loop ()
    (cond
     ((with-catch false
                  (cut json-rpc (mantis-url)
                       "web3_clientVersion" (void)))
      => (lambda (version) (printf "Connected to Mantis ~a\n" version)))
     (else
      (printf "Waiting for mantis to start...\n")
      (thread-sleep! 1)
      (loop)))))

(define-entry-point (start)
  "Start a fresh Mantis docker image, stop and wipe any previous one"
  (stop)
  (run-mantis)
  (wait-for-mantis))

(define-entry-point (stop)
  "Stop and wipe any current Mantis docker image"
  (kill-mantis)
  (reset-mantis))

(define-entry-point (mallet)
  "Run mallet against the docker image"
  ;; download it at: https://github.com/input-output-hk/mallet
  (run-process/batch ["mallet" (mantis-url) (string-append "--datadir=" mantis-run-directory)]))

(set-default-entry-point! "start")

(def main call-entry-point)
