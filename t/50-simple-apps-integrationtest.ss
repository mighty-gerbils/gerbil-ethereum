(export #t)

(import
  :gerbil/gambit
  :std/format :std/iter
  :std/misc/decimal :std/misc/list-builder :std/misc/number
  :std/srfi/1 :std/sugar :std/test
  :clan/json
  :clan/poo/object :clan/poo/io
  :clan/crypto/random
  :clan/persist/db
  :clan/debug :clan/poo/debug
  ../types ../ethereum ../known-addresses ../json-rpc ../nonce-tracker
  ../transaction ../tx-tracker ../simple-apps ../testing ../assets ../evm-runtime ../meta-create2
  ./10-json-rpc-integrationtest ./30-transaction-integrationtest)

(def (round-up-amount amount increment)
  (ceiling-align (+ amount (* 1/2 increment)) increment))

(def (test-prefund)
  (def balances-before (map eth_getBalance prefunded-addresses))
  ;; Round up to one thousandth of an ETH in wei, adding at least half that.
  (def target-amount (round-up-amount (apply max balances-before) (wei<-ether 1/1000)))
  (DDT before-batch-transfer:
       (cut map (.@ Ether .string<-) <>) balances-before
       (.@ Ether .string<-) target-amount)
  (ensure-addresses-prefunded from: croesus to: prefunded-addresses
                              min-balance: target-amount target-balance: target-amount)
  (def balances-after (map eth_getBalance prefunded-addresses))
  (DDT after-batch-transfer:
       (cut map (.@ Ether .string<-) <>) balances-after)
  (check-equal? balances-after (make-list (length prefunded-addresses) target-amount)))

(def 50-simple-apps-integrationtest
  (test-suite "integration test for ethereum/simple-apps"
    (reset-nonce croesus) (DBG nonce: (peek-nonce croesus))

    (test-case "Prefunding accounts works as expected."
      (test-prefund))

    (test-case "trivial-logger works"
      (def trivial-logger
        (.@ (ensure-trivial-logger-contract croesus log: write-json-ln) contract-address))
      (DDT 50-simple-apps-0: Address trivial-logger)
      (def receipt (post-transaction (call-function croesus trivial-logger (string->bytes "hello, world"))))
      (expect-logger-logs receipt [trivial-logger croesus "hello, world"]))

    (test-case "Presigned create2-wrapper works"
      (def salt (bytes<- UInt256 (randomUInt256)))
      (def create2-wrapper (ensure-presigned-create2-wrapper funder: croesus gasPrice: 100))
      (check-equal? (eth_getCode create2-wrapper) (create2-wrapper-runtime))
      (def logger2 (address<-create2 create2-wrapper salt (trivial-logger-init)))
      (DDT 50-create2-wrapper-0:
           Address create2-wrapper
           Address logger2
           TransactionReceipt
           (post-transaction
            (call-function croesus create2-wrapper (u8vector-append salt (trivial-logger-init)))))
      (check-equal? (eth_getCode logger2) (trivial-logger-runtime))
      (def receipt
        (DDT 50-create2-wrapper-1: TransactionReceipt
             (post-transaction (call-function croesus logger2 (string->bytes "hello, world")))))
      (expect-logger-logs receipt [logger2 croesus "hello, world"]))

    (test-case "batching without contract"
      (def alice-balance-before (eth_getBalance alice))
      (def bob-balance-before (eth_getBalance bob))
      (def amount (wei<-ether 1))
      (def salt (bytes<- UInt256 (randomUInt256)))

      (def creator (ensure-presigned-create2-wrapper funder: croesus gasPrice: 100))

      (def universal-batcher-init (batch-contract-init #f))
      (def universal-batcher (address<-create2 creator salt universal-batcher-init))

      (reset-nonce croesus)
      (def nonce (peek-nonce croesus))
      (def txaddress (address<-creator-nonce croesus nonce))
      (def logger (address<-creator-nonce txaddress 1))

      (def alice-batcher-init (batch-contract-init alice))
      (def alice-batcher (address<-create2 txaddress salt alice-batcher-init))

      (def txs
        [(batched-transfer amount alice) ;; tx 0
         (batched-create 0 (trivial-logger-init)) ;; tx 1
         (batched-call 0 logger (string->bytes "Hello from tx")) ;; tx 2: call contract create in tx 1 !!
         (batched-call amount creator (u8vector-append salt universal-batcher-init))
         (batched-create2 0 alice-batcher-init salt) ;; tx 3
         (batched-transfer amount bob)
         ])
      (def receipt (batch-txs croesus txs gas: 4000000))

      (def alice-balance-after (eth_getBalance alice))
      (def bob-balance-after (eth_getBalance bob))

      (check-equal? (eth_getCode logger) (trivial-logger-runtime))
      (expect-logger-logs receipt [logger txaddress "Hello from tx"])
      (check-equal? (eth_getCode universal-batcher) (batch-contract-runtime #f))
      (check-equal? (eth_getCode alice-batcher) (batch-contract-runtime alice))
      (check-equal? (- alice-balance-after alice-balance-before) (* 1 amount))
      (check-equal? (- bob-balance-after bob-balance-before) (* 1 amount)))

    #; ;; TODO: test this
    (test-case "batching with unowned contract"
      (def alice-balance-before (eth_getBalance alice))
      (def bob-balance-before (eth_getBalance bob))
      (def amount (wei<-ether 1))
      (def salt (bytes<- UInt256 (randomUInt256)))
      (def nonce (begin (reset-nonce croesus) (peek-nonce croesus)))
      (def txaddress (address<-creator-nonce croesus nonce))
      (def batcher-init (batch-contract-init #f))
      (def batcher (address<-creator-nonce txaddress 0))
      (def logger (address<-creator-nonce batcher 1))
      (def alice-batcher-init (batch-contract-init alice))
      (def alice-batcher (address<-create2 batcher salt alice-batcher-init))

      (def txs1
        [(batched-transfer (* 2 amount) alice-batcher) ;; tx 0
         (batched-create 0 (trivial-logger-init)) ;; tx 1
         (batched-call 0 logger (string->bytes "Hello from subtx")) ;; tx 2: call contract create in tx 1 !!
         (batched-delegate-call 0 logger (string->bytes "Hello from tx")) ;; tx 3
         (batched-create2 0 alice-batcher-init salt) ;; tx 4
         (batched-transfer (* 2 amount) bob)])
      (def txs0
        [(batched-create amount (batch-contract-runtime #f))
         (batched-call (* 3 amount) batcher (bytes<-batched-transactions txs1))])

      (def receipt (batch-txs croesus txs0 gas: 4000000))

      (def alice-balance-after (eth_getBalance alice))
      (def bob-balance-after (eth_getBalance bob))

      (expect-logger-logs receipt
                          [logger batcher "Hello from subtx"]
                          [batcher txaddress "Hello from tx"])
      (check-equal? (eth_getCode batcher) (batch-contract-runtime #f))
      (check-equal? (eth_getCode logger) (trivial-logger-runtime #f))
      (check-equal? (eth_getCode alice-batcher) (batch-contract-runtime alice))
      (check-equal? (- alice-balance-after alice-balance-before) (* 2 amount))
      (check-equal? (- bob-balance-after bob-balance-before) (* 2 amount)))))
