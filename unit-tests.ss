#!/usr/bin/env gxi
;; To run tests, use: ./unit-tests.ss
;; You can even run tests without first running ./build.ss !

(import
  :std/misc/path
  :clan/testing
  :clan/path-config)

;; Let's share the configuration and data directories with the rest of the Glow ecosystem
;;(set! application-name (lambda () "glow"))

;; Initialize the test environment, and enable the associated command-line subcommands.
;; This also enables the loading of modules below even when not compiled.
(init-test-environment!)

;; Accept the command-line subcommands defined in the files below.
(import :clan/ethereum/version :clan/ethereum/testing :clan/ethereum/cli
        :clan/ethereum/test-contracts)

;; Define more commands
(import :std/misc/process
        :clan/multicall)

(define-entry-point (docker-test)
  (help: "Run integration test in Docker" getopt: [])
  (run-process/batch ["docker" "run"
                      "-v" (string-append (current-directory) ":/gerbil-ethereum:ro")
                      "mukn/glow:devel" "/gerbil-ethereum/unit-tests.ss" "%in-docker-copy-test"]))

(define-entry-point (%in-docker-copy-test)
  (help: "Internal command to copy source and start integration tests from inside Docker" getopt: [])
  (run-process/batch ["rsync" "-a" "--exclude" "run" "/gerbil-ethereum" "/root/"])
  (current-directory "/root/gerbil-ethereum/")
  (build-and-test))

(define-entry-point (build-and-test)
  (help: "Run all build and test commands" getopt: [])
  (run-process/batch ["./build.ss"])
  (run-process/batch ["./unit-tests.ss"])
  (run-process/batch ["./scripts/run-ethereum-test-net.ss"])
  (run-process/batch ["./unit-tests.ss" "integration"])
  (run-process/batch ["./scripts/run-ethereum-test-net.ss" "stop"]))
