(export #t)

(import
  :std/format :std/misc/list-builder :std/srfi/1 :std/sugar :std/test
  :clan/debug
  :clan/poo/object :clan/poo/debug
  :clan/persist/db
  ../ethereum ../known-addresses ../json-rpc ../nonce-tracker ../batch-send ../testing ../assets
  ./30-transaction-integrationtest)

(def 50-batch-send-integrationtest
  (test-suite "integration test for ethereum/batch-send"
    (test-case "batch transfer works"
      (reset-nonce croesus) (DBG nonce: (peek-nonce croesus))
      (def balances-before (map (cut eth_getBalance <> 'latest) prefunded-addresses))
      (def target-amount (+ (apply max balances-before) (wei<-ether 1/1000))) ;; add one thousandth of an ETH in wei
      (DDT before-batch-transfer:
           (cut map (.@ Ether .string<-) <>) balances-before
           (.@ Ether .string<-) target-amount)
      (ensure-addresses-prefunded from: croesus to: prefunded-addresses
                                  min-balance: target-amount target-balance: target-amount)
      (def balances-after (map (cut eth_getBalance <> 'latest) prefunded-addresses))
      (DDT after-batch-transfer:
           (cut map (.@ Ether .string<-) <>) balances-after)
      (check-equal? balances-after (make-list (length prefunded-addresses) target-amount)))))
