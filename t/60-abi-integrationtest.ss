(export #t)

(import
  :gerbil/gambit/os
  :std/misc/list :std/misc/ports :std/srfi/1 :std/sugar :std/test :std/text/hex
  :clan/debug :clan/filesystem :clan/list :clan/path :clan/path-config
  :clan/poo/object
  ../hex ../types ../network-config
  ../json-rpc ../nonce-tracker ../transaction ../abi ../tx-tracker ../testing
  ./30-transaction-integrationtest)

(def test-hello-contract-bin (source-path "t/precompiled/HelloWorld.bin"))

(def (test-hello-contract-bytes)
  (hex-decode (read-file-string test-hello-contract-bin)))

(def hello-contract #f)

(def (ensure-hello-contract)
  (unless hello-contract
    (let (receipt (post-transaction (create-contract croesus (test-hello-contract-bytes))))
      (set! hello-contract (.@ receipt contractAddress)))))

(defrule (check-equal-bytes? x y) (check-equal? (0x<-bytes x) (0x<-bytes y)))

(def 60-abi-integrationtest
  (test-suite "integration test for ethereum/abi"
    (reset-nonce croesus) (DBG nonce: (peek-nonce croesus))
    (unless (ethereum-mantis?)
      ;; TODO: Somehow reenable these tests on Mantis.
      ;;   1. We can't just run negative tests against Mantis, because it both accepts and rejects
      ;;      bad transactions, especially when the gas is below the intrinsic gas.
      ;;   2. Then the sender account becomes unusable, never ever again capable of sending any
      ;;      transaction, its funds forever locked away (hopefully Mantis will at least do the
      ;;      right thing if another node submits a block with rival transactions that
      ;;      update the nonce, etc. --- but does it?).
      ;;   3. Plus, there is no good way to detect failure, anyway. Mantis never tells
      ;;      the client anything went wrong, so the client can hang forever waiting for confirmation.
      ;;   4. Even if the client reacts and tries to send an alternative, rival transaction with the
      ;;      same nonce, the latter will end up rejected by Mantis for conflicting with a transaction
      ;;      that will never be committed.
      ;;   5. At the very least, when you experiment, don't run the below test with Croesus as the sender,
      ;;      but use a throwaway account, because you WILL have to throw it away...
      ;;   6. Something in the KEVM gets thrown off by the contracts produced by solc 0.7.4
      ;;      (as opposed to what? 0.5.1?)
      ;; All in all, we disable these tests on Mantis until further notice,
      ;; and we don't recommend using Mantis in production at this time.
      (test-case "Contract creation failure due to insufficient gas"
        (unless (ethereum-mantis?)
          ;; Mantis never accepts the transaction, and even logs a message why it won't,
          ;; but its JSON RPC API doesn't give us any way to tell it's failed.
          (DBG create-hello-contract-too-little-gas:)
          (check-exception (post-transaction (create-contract croesus (test-hello-contract-bytes) gas: 21000))
                           (match <> ((TransactionStatus-TxFailed (vector _ exn))
                                      (if (ethereum-mantis?)
                                        ;; NB: this error has changed on our mantis support, anyway
                                        (and (TransactionRejected? exn)
                                             (equal? (TransactionRejected-receipt exn)
                                                     "Reason unknown (nonce didn't change)"))
                                        (IntrinsicGasTooLow? exn)))
                                  (_ #f)))
          ;; Mantis never accepts the transaction, and doesn't even log a message why it won't,
          ;; but its JSON RPC API doesn't give us any way to tell it's failed.
          (DBG create-hello-contract:)
          (check-exception (post-transaction (create-contract croesus (test-hello-contract-bytes) gas: 100000))
                           (match <> ((TransactionStatus-TxFailed (vector _ (? TransactionRejected?))) #t)
                                  (_ #f)))))
      (test-case "Call contract function hello with no argument"
        (ensure-hello-contract)
        (def pretx (call-function croesus hello-contract
                                  (bytes<-ethereum-function-call ["hello"] [])))
        (def receipt (post-transaction pretx))
        (def block-number (.@ receipt blockNumber))
        (def data (eth_call pretx (1- block-number)))
        (check-equal-bytes? data (ethabi-encode [String] ["Hello, World!"])))
      (test-case "call contract function mul42 with one number argument"
        (def pretx (call-function croesus hello-contract
                                  (bytes<-ethereum-function-call ["mul42" UInt256] [47])))
        (def receipt (post-transaction pretx))
        (def block-number (.@ receipt blockNumber))
        (def data (eth_call pretx (1- block-number)))
        (check-equal-bytes? data (ethabi-encode [UInt256] [1974])))
      (test-case "call contract function greetings with one string argument"
        (def pretx (call-function croesus hello-contract
                                  (bytes<-ethereum-function-call ["greetings" String] ["Croesus"])))
        (def receipt (post-transaction pretx))
        (def block-number (.@ receipt blockNumber))
        (def logs (.@ receipt logs))
        (def receipt-log (first-and-only logs))
        (def log-contract-address (.@ receipt-log address))
        (check-equal? log-contract-address hello-contract)
        (def topic-event (first-and-only (.@ receipt-log topics)))
        (check-equal-bytes? topic-event (digest<-function-signature ["greetingsEvent" String]))
        ;; the log data is the encoding of the parameter passed to the event
        (def data (.@ receipt-log data))
        (def result (eth_call pretx (1- block-number)))
        (check-equal-bytes? data result)
        (check-equal-bytes? data (ethabi-encode [String] ["Greetings, Croesus"]))))))

;; TODO: add a stateful function, and check the behavior of eth-call wrt block-number
;; TODO: test the parsing of the HelloWorld.abi JSON descriptor
