#!/usr/bin/env gxi
;; To run tests, use: ./unit-tests.ss
;; You can even run tests without first running ./build.ss !

(import :clan/testing :clan/path-config :clan/path)

;; Let's share the configuration and data directories with the rest of the Glow ecosystem
(set! application-name (lambda () "glow"))

;; Initialize the test environment, and enable the associated command-line subcommands.
;; This also enables the loading of modules below even when not compiled.
(init-test-environment!)

;; Accept the command-line subcommands defined in the files below.
(import :mukn/ethereum/version :mukn/ethereum/testing :mukn/ethereum/cli
        :mukn/ethereum/test-contracts)
