(export #t)

(import
  :gerbil/gambit/os :gerbil/gambit/ports :gerbil/gambit/threads
  :std/format :std/srfi/1 :std/test :std/sugar :std/iter :std/text/json :std/misc/ports
  :clan/base :clan/concurrency :clan/debug :clan/decimal :clan/exception
  :clan/io :clan/json :clan/path-config :clan/ports :clan/number
  :clan/poo/poo :clan/poo/io :clan/poo/brace :clan/crypto/keccak
  ../ethereum ../known-addresses ../json-rpc ../tx-tracker ../batch-send
  ../network-config ../assets ../assembly ../transaction ../signing ../hex
  ../transaction ../contract-runtime ../types
  ./signing-test ./30-transaction-integrationtest)

(def evm-eval-test
  (test-suite "unit tests for evm functions"

    (test-case "returns a value"
      (def contract-bytes
        (assemble/bytes
          (&begin
            42 (&mstoreat 0 1) 2 0 RETURN)))
      (def result (evm-eval/offchain alice contract-bytes))
      (def unmarshaled-result (nat<-bytes result))
      (check-equal? (* 42 256) unmarshaled-result))

    (test-case "if works"
      (def contract-bytes
        (assemble/bytes
          (&begin
            (&if (&begin 1 2 GT)
              (&begin 0)
              (&begin 1))
            (&mstoreat 1 1)
            2 0 RETURN)))
      (def result (evm-eval/offchain alice contract-bytes))
      (check-equal? 0 (nat<-bytes result))

    (test-case "switch works"
      (def contract-bytes
        (assemble/bytes
          (&begin
            (&switch-test 1
              [0 [0]]
              [1 [1]]
              [2 [2]])
            (&mstoreat 1 1)
            2 0 RETURN)))
      (def result (evm-eval/offchain alice contract-bytes))
      (check-equal? 1 (nat<-bytes result))))))

(def (&switch-test comparison-value cases)
  (def reducer
    (λ (current-case next-case)
      (match current-case
        ([case-value case-code-block]
          (&if (&begin comparison-value case-value EQ)
            (&begin* case-code-block)
            next-case))
        (else
          (error "Invalid case in switch expression: " current-case)))))
  (def nested-ifs (cps-foldl reducer cases))
  (nested-ifs (&begin 0 DUP1 REVERT)))

;; TODO: Is this actually useful? As is, still impenetrable.
(def (cps-foldl reducer lst)
  (foldl
    (λ (cur continuation)
      (λ (next) (continuation (reducer cur next))))
    identity
    lst))