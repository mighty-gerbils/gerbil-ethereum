(export #t)

(import
  :std/format :std/iter :std/misc/list-builder :std/srfi/1 :std/sugar :std/test
  :clan/decimal :clan/json :clan/persist/db
  ../ethereum ../known-addresses ../json-rpc ../batch-send
  ./signing-test ./transaction-integrationtest)

;; Display an account having the given balance given a way to print address, optional name and balance
;; : 'a <- ('a <- string string) Address TokenAmount
(def (display-balance display address balance)
  (display (nicknamed-string<-address address) balance))

(def (get-address-missing-amount min-balance target-balance address)
  (assert! (<= min-balance target-balance))
  (def balance (eth_getBalance address 'pending))
  (printf (if (>= balance min-balance)
            "~a has ~a ether already. Good.\n"
            "~a has ~a ether only. Funding.\n")
          (nicknamed-string<-address address) (string<-decimal (/ balance one-ether-in-wei)))
  (if (>= balance min-balance) 0 (- target-balance balance)))

;; target-balance is more than min-balance, so we can go faster by not re-funding everytime.
(def (ensure-addresses-prefunded
      from: (croesus croesus)
      to: (addresses test-addresses)
      min-balance: (min-balance one-ether-in-wei)
      target-balance: (target-balance (* 2 min-balance)))
  (def needful-transfers
    (with-list-builder (c)
      (for (a addresses)
        (let (b (get-address-missing-amount min-balance target-balance a))
          (when (> b 0) (c [a b]))))))
  (batch-send croesus needful-transfers log: write-json-ln))

(ensure-addresses-prefunded)

(def batch-send-integrationtest
  (test-suite "integration test for ethereum/batch-send"
    (test-case "batch transfer works"
      (def addresses test-addresses)
      (def balances-before (map (cut eth_getBalance <> 'pending) addresses))
      (def target-amount (+ (apply max balances-before) (expt 10 15))) ;; add one thousandth of an ETH in wei
      (printf "target-amount: ~a\n" target-amount)
      (ensure-addresses-prefunded from: croesus to: addresses
                                  min-balance: target-amount target-balance: target-amount)
      (def balances-after (map (cut eth_getBalance <> 'pending) addresses))
      (check-equal? balances-after (make-list (length addresses) target-amount)))))
