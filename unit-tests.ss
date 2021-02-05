#!/usr/bin/env gxi
;; To run tests, use: ./unit-tests.ss
;; You can even run tests without first running ./build.ss !
(import :clan/testing :clan/path-config :clan/path)

(def gerbil-ethereum-src (path-parent (path-simplify-directory (this-source-file))))
(set! source-directory (lambda () gerbil-ethereum-src))
(set! home-directory (lambda () gerbil-ethereum-src))
(add-load-path gerbil-ethereum-src)

(init-test-environment!)
(import :mukn/ethereum/version :mukn/ethereum/cli)
