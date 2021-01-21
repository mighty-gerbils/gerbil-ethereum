(export #t)

(import
  :gerbil/gambit/bits :gerbil/gambit/bytes :gerbil/gambit/ports
  :std/iter :std/misc/bytes :std/test :clan/number
  :clan/poo/io :clan/persist/content-addressing
  ../assembly ../contract-runtime ../types ../ethereum ../signing
  ./signing-test
  ./10-json-rpc-integrationtest ./30-transaction-integrationtest ./50-batch-send-integrationtest)

(ensure-addresses-prefunded)

(def 80-evm-eval-integrationtest
  (test-suite "unit tests for evm functions"

    (test-case "returns a value"
      (def contract-bytes
        (assemble/bytes
          (&begin
            42
            (&mstoreat 0 1)
            2 0 RETURN)))
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
            (&mstoreat 0 1)
            1 0 RETURN)))
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
            (&mstoreat 0 1)
            1 0 RETURN)))
      (def result (evm-eval/offchain alice contract-bytes))
      (check-equal? 1 (nat<-bytes result)))

    (test-case "digest works with single value"
      (def digest-value
        [[UInt256 . 7]])
      (def contract-bytes
        (assemble/bytes
          (&begin
            (&digest<-tvps digest-value)
            (&mstoreat 0 32)
            32 0 RETURN)))
      (def result (evm-eval/offchain alice contract-bytes))
      (check-equal? (digest digest-value) result))

    (test-case "digest works with multiple values"
      (def digest-value
        [[UInt256 . 7]
         [UInt256 . 21]])
      (def contract-bytes
        (assemble/bytes
          (&begin
            (&digest<-tvps digest-value)
            (&mstoreat 0 32)
            32 0 RETURN)))
      (def result (evm-eval/offchain alice contract-bytes))
      (check-equal? (digest digest-value) result))

    (test-case "digest works with realistic frame state"
      (def digest-value
        [[UInt16 . 1014]
         [Block . 2185]
         [Address . alice]
         [Address . bob]
         [Digest . #u8(99 29 22 99 149 169 218 44 150 207 223 114 84 92 253 163 49 233 56 120 20 117 144 222 173 81 88 153 240 137 9 49)]
         [UInt256 . 1000000000]
         [UInt256 . 26494137127516106733148689316263385574755820242808037201403224934424794788569]
         [UInt256 . 1000000000]])
      (def contract-bytes
        (assemble/bytes
          (&begin
            (&digest<-tvps digest-value)
            (&mstoreat 0 32)
            32 0 RETURN)))
      (def result (evm-eval/offchain alice contract-bytes))
      (check-equal? (digest digest-value) result))))

(def (digest alst)
  (def out (open-output-u8vector))
  (for ((p alst))
    (with (([t . v] p)) (marshal t v out)))
  (digest<-bytes (get-output-u8vector out)))
