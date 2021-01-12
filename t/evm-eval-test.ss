(export #t)

(import
  :std/test :clan/number
  ../assembly
  ./signing-test
  ./10-json-rpc-integrationtest ./30-transaction-integrationtest ./50-batch-send-integrationtest)

(ensure-addresses-prefunded)

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
      (check-equal? 0 (nat<-bytes result)))

    (test-case "switch works"
      (def contract-bytes
        (assemble/bytes
          (&begin
            (&switch 1
              [[0 [0]]
               [1 [1]]
               [2 [2]]])
            (&mstoreat 1 1)
            2 0 RETURN)))
      (def result (evm-eval/offchain alice contract-bytes))
      (check-equal? 1 (nat<-bytes result)))))
