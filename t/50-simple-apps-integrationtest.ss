(export #t)

(import
  :gerbil/gambit/bytes
  :std/format :std/iter :std/misc/list-builder :std/srfi/1 :std/sugar :std/test
  :clan/decimal :clan/json
  :clan/poo/object :clan/poo/io
  :clan/crypto/random
  :clan/persist/db
  :clan/debug :clan/poo/debug
  ../types ../ethereum ../signing ../known-addresses ../json-rpc ../nonce-tracker
  ../transaction ../tx-tracker ../simple-apps ../testing ../assets
  ./10-json-rpc-integrationtest ./30-transaction-integrationtest)

(def 50-simple-apps-integrationtest
  (test-suite "integration test for ethereum/simple-apps"
    (reset-nonce croesus) (DBG nonce: (peek-nonce croesus))

    (def (test-transfer direct?)
      (def balances-before (map eth_getBalance prefunded-addresses))
      (def target-amount (+ (apply max balances-before) (wei<-ether 1/1000))) ;; add one thousandth of an ETH in wei
      (DDT before-batch-transfer:
           (cut map (.@ Ether .string<-) <>) balances-before
           (.@ Ether .string<-) target-amount)
      (ensure-addresses-prefunded from: croesus to: prefunded-addresses
                                  min-balance: target-amount target-balance: target-amount
                                  batch-contract: (not direct?))
      (def balances-after (map eth_getBalance prefunded-addresses))
      (DDT after-batch-transfer:
           (cut map (.@ Ether .string<-) <>) balances-after)
      (check-equal? balances-after (make-list (length prefunded-addresses) target-amount)))
#;    (test-case "direct batch transfer works"
      (test-transfer #f))
#;    (test-case "batch transfer contract works"
      (test-transfer #t))

#;    (test-case "trivial-logger works"
      (def trivial-logger
        (.@ (ensure-trivial-logger-contract croesus log: write-json-ln) contract-address))
      (DDT 50-simple-apps-0: Address trivial-logger)
      (def receipt (post-transaction (call-function croesus trivial-logger (string->bytes "hello, world"))))
      (def logs (.@ receipt logs))
      (check-equal? (length logs) 1)
      (def log (car logs))
      (check-equal? [(.@ log address) (bytes->string (.@ log data))]
                    [trivial-logger "hello, world"]))

    (test-case "batching works with and without a batch contract"
      (def alice-balance-before (eth_getBalance alice))
      (def bob-balance-before (eth_getBalance bob))
      (def amount (wei<-ether 1))
      (def salt (bytes<- UInt256 (randomUInt256)))

      (def creator-init (create2-wrapper-init))
      (def creator (address<-create2 croesus salt creator-init))
      (def universal-batcher-init (batch-contract-init #f))
      (def universal-batcher (address<-create2 creator salt universal-batcher-init))
      (def alice-batcher-init (batch-contract-init alice))
      (def alice-batcher (address<-create2 universal-batcher salt alice-batcher-init))
      (def logger-init (trivial-logger-contract-init))
      (def logger (address<-create2 universal-batcher salt logger-init))

      (def alice-txs
        [(batched-transfer (* 2 amount) alice)
         (batched-call 0 logger (string->bytes "Hello from Alice"))
         (batched-transfer (* 2 amount) bob)])
      (def alice-txs-bytes (bytes<-batched-transactions/signed alice alice-txs))

      (def universal-txs
        [(batched-transfer (* 1 amount) alice)
         (batched-call 0 logger (string->bytes "Hello World"))
         (batched-create2 (* 1 amount) alice-batcher-init salt)
         (batched-call (* 2 amount) alice-batcher alice-txs-bytes) ;; spending 4 but got 2 already
         (batched-transfer (* 1 amount) bob)])
      (def universal-batcher-msg (bytes<-batched-transactions universal-txs))

      (def txs
        [(batched-transfer amount universal-batcher)
         (batched-transfer amount alice-batcher)
         (batched-create2 0 creator-init salt)
         (batched-call amount creator (bytes-append salt universal-batcher-init))
         (batched-call (* 4 amount) universal-batcher universal-batcher-msg)])
      (def receipt (batch-txs croesus txs gas: 4000000))
      (def alice-balance-after (eth_getBalance alice))
      (def bob-balance-after (eth_getBalance bob))
      (def logs (.@ receipt logs))
      (check-equal? (length logs) 2)
      (def (get-foo log) [(0x<-address (.@ log address))
                          (map (.@ Bytes32 .json<-) (.@ log topics))
                          (bytes->string (.@ log data))])
      (def (expected-topics address)
        [(json<- Bytes32 (bytes-append (make-bytes 12) (bytes<- Address address)))])
      (check-equal? (get-foo (car logs))
                    [(0x<-address logger) (expected-topics universal-batcher) "Hello World"])
      (check-equal? (get-foo (cadr logs))
                    [(0x<-address logger) (expected-topics alice-batcher) "Hello from Alice"])
      ;; Check that the calls did deposit money onto the recipient contract
      (check-equal? (- alice-balance-after alice-balance-before) (* 3 amount))
      (check-equal? (- bob-balance-after bob-balance-before) (* 3 amount))
      )))
