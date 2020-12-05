(export #t)

(import
  :gerbil/gambit/exceptions :gerbil/gambit/ports
  :std/format :std/misc/list :std/misc/repr :std/srfi/13 :std/sugar :std/test
  :clan/exception :clan/syntax :clan/with-id
  :clan/poo/poo :clan/poo/brace :clan/poo/io (only-in :clan/poo/mop sexp<- json<-)
  :clan/crypto/keccak
  ../hex ../rlp ../types ../ethereum ../known-addresses ../signing)

(def (capitalize name)
  (def Name (string-downcase (stringify name)))
  (string-set! Name 0 (char-upcase (string-ref Name 0)))
  Name)

(def test-keys [])
(def test-addresses [])

(defrule (defkeys ctx (name secret-key) ...)
  (begin
    (with-id ctx ((keys #'name '-keys)
                  (address #'name '-address)
                  test-keypairs)
      (begin
        (def keys (keypair<-secret-key secret-key ""))
        (def address (keypair-address keys))
        (push! [(capitalize 'name) keys] test-keys)
        (push! address test-addresses))) ...))

(defkeys test-addresses
  (trent    "0xb6fb0b7e61363ee2f748161338f56953e8aa42642e9990eff17e7de9aa895786")
  (fortysix "0x4646464646464646464646464646464646464646464646464646464646464646")
  (alice    "0xfdc8f15b2dd9229b0b9246094393afc23b3b705c07e674f6cb614120d1627818")
  (bob      "0x9b21b9b06ba77824b8ba6a815f5a075229a708ae88ba7fd935c968fe2c3df172")
  (yolanda  "0xaedcdea2b91de24d1fe2c8ae4b60687fb3826612962553fa3d0b8486e322aaa7")
  (zander   "0x4884b1bdef8281b40cad15f5525d72a5c9a5db18f213abf28a46bfab8bff2a5f")
  ;; This key was chosen because it's got money on the Mantis test image.
  ;; Strategically placed last, so it shows up first in test-addresses,
  ;; gets registered first if undefined, and then also becomes croesus as per get-first-account.
  (croesus "0x1167a41c432d1a494408b8fdeecd79bff89a5689925606dff8adf01f4bf92922"))

;; Register test keypairs
(for-each (cut apply register-keypair <>) test-keys)

(def (string<-exception e)
  (call-with-output-string (cut display-exception e <>)))

(def (show-representations name x (type #f))
  (printf "~a:\n  display: ~a\n  write: ~s\n  pr: ~r\n" name x x x)
  (defrule (X foo) (with-catch string<-exception (lambda () foo)))
  (when type (printf "  json: ~a\n  sexp: ~a\n"
                     (X (json-string<- type x)) (X (object->string (sexp<- type x))))))

(def signing-test
  (test-suite "Test suite for ethereum/signing"
    (test-case "check test users"
      (defrule (check-user name addressj pubkeyj)
        (with-id signing-test
          ((keys #'name '-keys)
           (address #'name '-address)
           (pubkey #'name '-pubkey)
           (seckey #'name '-seckey)
           (passwd #'name '-passwd))
          (def Name (capitalize 'name))
          (show-representations keypair: keys Keypair)
          (def data (format "some arbitrary string for ~a to sign" Name))
          (check-equal? (keypair-address keys) address)
          (show-representations address: address Address)
          (def pubkey (keypair-public-key keys))
          (show-representations pubkey: pubkey PublicKey)
          (def seckey (keypair-secret-key keys))
          (show-representations seckey: seckey SecretKey)
          (def passwd (keypair-password keys))
          (show-representations password: passwd Password)
          (def signature (make-signature String seckey data))
          (show-representations signature: signature Signature)
          (check-equal? (json<- PublicKey pubkey) pubkeyj)
          (check-equal? (json<- Address address) addressj)
          (check-equal? (signature-valid? String address signature data) #t)))
      (check-user croesus "0x25c0bb1A5203AF87869951AEf7cF3FEdD8E330fC" ;; "0x000d836201318ec6899a67540690382780743280"
                  "0x3dfbd16d74816ad656f6c98e2a6634ca1930b5fc450eb93ca0a92574a30d00ff8eefd9d1cc3cd81cbb021b3f29abbbabfd29da7feef93f40f63a1e512c240517")
      (check-user alice "0xC54e86DFFb87B9736E2E35DD85c775358F1c31CE"
                  "0x5562695c85f88f6cbaec121d2a3da6666c5dc8540d86358bd569a1882bbe6ddcf45b76f5643133939c8e7a339947ca1b115290d577343023d79c256dbc54bc97")
      (check-user bob "0x9CcaEd210CE8c0Cb49c5Ad1C4f583406c264BA69"
                  "0x9e0a7e3c05e3328c603b0c27fbfdfc5030c95d9ad179a431c14f81e30a64ce95f625447e182a8be718d45f9ab9723f9b8571dd5c5752daa66feb84938b095805")
      (check-user trent "0xF47408143d327e4bc6A87EF4a70A4E0aF09b9A1C"
                  "0x26bd9885f2c9e23d18c3025da70e71a4f7ce237124352882eafbd1cbb1e9742c4fe3847ce1a56a0d19df7a7d385a2134be05208b5d1ccc5d015f5e9a3ba0d7df"))
    (test-case "transaction signature"
      ;; TODO: complete the test of this example from EIP 155
      ;; https://github.com/ethereum/EIPs/blob/master/EIPS/eip-155.md
      (def sender-keys (keypair<-secret-key "0x4646464646464646464646464646464646464646464646464646464646464646" ""))
      (def sender-address (keypair-address sender-keys))
      (def recipient-address (address<-0x "0x3535353535353535353535353535353535353535"))
      (def chain-id 1)
      (def tx-data
        {nonce: 9 gasPrice: (* 20 (expt 10 9)) gas: 21000
         to: recipient-address value: (wei<-ether 1) data: (bytes<-0x "0x")
         v: chain-id r: 0 s: 0})
      (def tx-data-bytes (rlpbytes<- SignedTransactionData tx-data))
      (check-equal? (0x<-bytes tx-data-bytes) "0xec098504a817c800825208943535353535353535353535353535353535353535880de0b6b3a764000080018080")
      (def tx-data-digest (keccak256<-bytes tx-data-bytes))
      (check-equal? (0x<-bytes tx-data-digest)
                    "0xdaf5a779ae972f972197303d7b574746c7ef83eadac0f2791ad23db92e4c8e53")
      (def signature (make-message-signature (keypair-secret-key sender-keys) tx-data-digest))
      (defvalues (v r s) (vrs<-signature signature))
      (def v2 (eip155-v v chain-id #t))
      (check-equal? [v2 r s]
                    [37 18515461264373351373200002665853028612451056578545711640558177340181847433846 46948507304638947509940763649030358759909902576025900602547168820602576006531])
      (def signed-tx-data (.cc tx-data v: v2 r: r s: s))
      (def signed-tx-data-bytes (rlpbytes<- SignedTransactionData signed-tx-data))
      (check-equal? (0x<-bytes signed-tx-data-bytes) "0xf86c098504a817c800825208943535353535353535353535353535353535353535880de0b6b3a76400008025a028ef61340bd939bc2195fe537567866003e1a15d3c71ff63e1590620aa636276a067cbe9d8997f761aecb703304b3800ccf555c9f3dc64214b297fb1966a3b6d83"))))
