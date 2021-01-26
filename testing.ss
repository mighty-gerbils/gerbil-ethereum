(export #t)

(import
  :std/format :std/iter :std/misc/list :std/srfi/13 :std/sugar
  :clan/json :clan/syntax :clan/with-id
  :clan/poo/poo
  ./ethereum ./json-rpc ./known-addresses ./batch-send ./assets)

(def (capitalize name)
  (def Name (string-downcase (stringify name)))
  (string-set! Name 0 (char-upcase (string-ref Name 0)))
  Name)

(def test-keys [])
(def test-addresses [])

(defrule (defkeys ctx (name secret-key) ...)
  (begin
    (with-id ctx ((keys #'name '-keys)
                  (address #'name)
                  test-keypairs)
      (begin
        (def keys (keypair<-seckey-0x secret-key ""))
        (def address (keypair-address keys))
        (push! [(capitalize 'name) keys] test-keys)
        (push! address test-addresses))) ...))

(defkeys test-addresses
  ;; These keys are chosen for a common name and recognizable prefix, for use on private test networks
  ;; With our naive algorithm, finding a 5-char hex prefix should take a few minutes,
  ;; a 6-char hex prefix an hour or two, a 7-char hex prefix a day or two.
  (alice    "0x33bbf7ff271c056cae4eba6503ad46d8cf6f4c35120ef97cc6ee719cf711e767") ;; 0xa71CE
  (bob      "0x30ce4a96f528bbfcd20d8c0c52f5c691f7e9675ef87e5a955e4e2d6f09c35ab0") ;; 0xb0bb1e
  (trent    "0x2d7d92a15f28bb6d56823a10c9a361e97bcd27714761dd95113765a9e5b33595") ;; 0x73e27
  ;; This is the penny collector for private test networks
  (penny    "0x59f140e92149d1dc39f00967b5c881e01e6fc1eafa4c06d91075fdc09831ca94") ;; 0xc077ec7
  ;; This key is used in some of Ethereum standard test-case
  (fortysix "0x4646464646464646464646464646464646464646464646464646464646464646")
  ;; This key was chosen because it's got money on in genesis block for IOHK's Mantis docker image.
  ;; We now use the same key as the "got all the money" account on our Geth genesis block.
  (croesus  "0x1167a41c432d1a494408b8fdeecd79bff89a5689925606dff8adf01f4bf92922"))

;; Register test keypairs
(for-each (cut apply register-keypair <>) test-keys)

;; Display an account having the given balance given a way to print address, optional name and balance
;; : 'a <- ('a <- string string) Address TokenAmount
(def (display-balance display address balance)
  (display (nicknamed-string<-address address) balance))

(def (get-address-missing-amount min-balance target-balance address)
  (assert! (<= min-balance target-balance))
  (def balance (eth_getBalance address 'pending))
  (if (>= balance min-balance)
    (begin
      (printf "~a has ~a already. Good.\n"
              (nicknamed-string<-address address)
              (.call Ether .string<- balance))
      0)
    (begin
      (printf "~a has ~a ether only. Funding to ~a ether.\n"
              (nicknamed-string<-address address)
              (.call Ether .string<- balance)
              (.call Ether .string<- target-balance))
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
