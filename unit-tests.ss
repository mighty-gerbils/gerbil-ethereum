#!/usr/bin/env gxi
;; To run tests, use: ./unit-tests.ss
;; You can even run tests without first building with ./build.ss !

(import :gerbil/expander "t/path-config.ss" :clan/utils/ports)
(in-gerbil-ethereum-src)
(import-module ':ethereum/t/unit-tests #t #t)
(def main (eval 'ethereum/t/unit-tests#main))
