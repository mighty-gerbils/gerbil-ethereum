(export #t)

(import
  :std/format :std/iter :std/misc/list-builder :std/srfi/1 :std/sugar :std/test
  :clan/poo/debug
  :clan/decimal :clan/debug :clan/json :clan/persist/db
  ../ethereum ../known-addresses ../json-rpc ../nonce-tracker ../batch-send
  ./signing-test ./30-transaction-integrationtest)

;; Display an account having the given balance given a way to print address, optional name and balance
;; : 'a <- ('a <- string string) Address TokenAmount
(def (display-balance display address balance)
  (display (nicknamed-string<-address address) balance))

(def (get-address-missing-amount min-balance target-balance address)
  (assert! (<= min-balance target-balance))
  (def balance (eth_getBalance address 'pending))
  (if (>= balance min-balance)
    (begin
      (printf "~a has ~a ether already. Good.\n"
              (nicknamed-string<-address address)
              (string<-decimal (ether<-wei balance)))
      0)
    (begin
      (printf "~a has ~a ether only. Funding to ~a ether.\n"
              (nicknamed-string<-address address)
              (string<-decimal (ether<-wei balance))
              (string<-decimal (ether<-wei target-balance)))
      (- target-balance balance))))

(def prefunded-addresses [alice bob trent])

;; target-balance is more than min-balance, so we can go faster by not re-funding everytime.
(def (ensure-addresses-prefunded
      from: (funder croesus)
      to: (addresses prefunded-addresses)
      min-balance: (min-balance one-ether-in-wei)
      target-balance: (target-balance (* 2 min-balance)))
  (def needful-transfers
    (with-list-builder (c)
      (for (a addresses)
        (unless (equal? a funder)
          (let (b (get-address-missing-amount min-balance target-balance a))
            (when (> b 0)
              (c [a b])))))))
  (batch-send funder needful-transfers log: write-json-ln))

(def 50-batch-send-integrationtest
  (test-suite "integration test for ethereum/batch-send"
    (test-case "batch transfer works"
      (reset-nonce croesus) (DBG nonce: (peek-nonce croesus))
      (def balances-before (map (cut eth_getBalance <> 'pending) prefunded-addresses))
      (def target-amount (+ (apply max balances-before) (wei<-ether 1/1000))) ;; add one thousandth of an ETH in wei
      (DBG before-batch-transfer: balances-before target-amount
           (map decimal-string-ether<-wei balances-before) (decimal-string-ether<-wei target-amount))
      (ensure-addresses-prefunded from: croesus to: prefunded-addresses
                                  min-balance: target-amount target-balance: target-amount)
      (def balances-after (map (cut eth_getBalance <> 'pending) prefunded-addresses))
      (DBG after-batch-transfer: balances-after (map decimal-string-ether<-wei balances-after))
      (check-equal? balances-after (make-list (length prefunded-addresses) target-amount)))))
