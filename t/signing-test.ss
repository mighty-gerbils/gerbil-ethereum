(export #t)

(import
  :gerbil/gambit/bytes :gerbil/gambit/exceptions :gerbil/gambit/ports
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
                  (address #'name)
                  test-keypairs)
      (begin
        (def keys (keypair<-seckey-0x secret-key ""))
        (def address (keypair-address keys))
        (push! [(capitalize 'name) keys] test-keys)
        (push! address test-addresses))) ...))

(defkeys test-addresses
  (trent    "0xb6fb0b7e61363ee2f748161338f56953e8aa42642e9990eff17e7de9aa895786")
  (fortysix "0x4646464646464646464646464646464646464646464646464646464646464646")
  (alice    "0xfdc8f15b2dd9229b0b9246094393afc23b3b705c07e674f6cb614120d1627818")
  (bob      "0x9b21b9b06ba77824b8ba6a815f5a075229a708ae88ba7fd935c968fe2c3df172")
  (penny    "0x03e389119c5322a9d25bc4da7bc84307762b61fd5e290874843011cacf3723d9")
  (yolanda  "0xaedcdea2b91de24d1fe2c8ae4b60687fb3826612962553fa3d0b8486e322aaa7")
  (zander   "0x4884b1bdef8281b40cad15f5525d72a5c9a5db18f213abf28a46bfab8bff2a5f")
  ;; This key was chosen because it's got money on in genesis block for IOHK's Mantis docker image.
  ;; We now use the same key as the "got all the money" account on our Geth genesis block.
  (croesus  "0x1167a41c432d1a494408b8fdeecd79bff89a5689925606dff8adf01f4bf92922"))

;; Register test keypairs
(for-each (cut apply register-keypair <>) test-keys)

(def (string<-exception e)
  (call-with-output-string (cut display-exception e <>)))

(def (show-representations name x (type #f))
  (printf "~a:\n  display: ~a\n  write: ~s\n  pr: ~r\n" name x x x)
  (defrule (X foo) (with-catch string<-exception (lambda () foo)))
  (when type (printf "  json: ~a\n  sexp: ~a\n"
                     (X (json-string<- type x)) (X (object->string (sexp<- type x))))))

(def (make-signature-wrong sig)
  (def bytes (bytes<- Signature sig))
  (bytes-set! bytes 4 (1- (bytes-ref bytes 4))) ;; arbitrarily decrement the fifth byte
  (<-bytes Signature bytes))

(def signing-test
  (test-suite "Test suite for ethereum/signing"
    (test-case "check test users"
      (defrule (check-user name addressj pubkeyj)
        (with-id signing-test
          ((keys #'name '-keys)
           (address #'name)
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
          (check-equal? (signature-valid? String address signature data) #t)
          (check-equal? (signature-valid? String address (make-signature-wrong signature) data) #f)))
      (check-user croesus "0x25c0bb1A5203AF87869951AEf7cF3FEdD8E330fC" ;; "0x000d836201318ec6899a67540690382780743280"
                  "0x3dfbd16d74816ad656f6c98e2a6634ca1930b5fc450eb93ca0a92574a30d00ff8eefd9d1cc3cd81cbb021b3f29abbbabfd29da7feef93f40f63a1e512c240517")
      (check-user alice "0xC54e86DFFb87B9736E2E35DD85c775358F1c31CE"
                  "0x5562695c85f88f6cbaec121d2a3da6666c5dc8540d86358bd569a1882bbe6ddcf45b76f5643133939c8e7a339947ca1b115290d577343023d79c256dbc54bc97")
      (check-user bob "0x9CcaEd210CE8c0Cb49c5Ad1C4f583406c264BA69"
                  "0x9e0a7e3c05e3328c603b0c27fbfdfc5030c95d9ad179a431c14f81e30a64ce95f625447e182a8be718d45f9ab9723f9b8571dd5c5752daa66feb84938b095805")
      (check-user penny "0xc11498Fa7fd1C261121EC856D6e0056335bcE90e"
                  "0x5c69b02d8d311481c8afa3df0697b4118cb8cd9833c468def4d636ab03d5d1e7a36d64b335eba27409bbfd324c437c4af70aa017bdf735dcc03f208f170a7fd1")
      (check-user trent "0xF47408143d327e4bc6A87EF4a70A4E0aF09b9A1C"
                  "0x26bd9885f2c9e23d18c3025da70e71a4f7ce237124352882eafbd1cbb1e9742c4fe3847ce1a56a0d19df7a7d385a2134be05208b5d1ccc5d015f5e9a3ba0d7df"))
    (test-case "check create2 address"
      (def (test-vector name addr salt init-code)
        (0x<-address (address<-create2
                      (address<-0x addr) (bytes<-0x salt) (bytes<-0x init-code))))
      (defrule (tv name addr salt init-code result) ;; test vector from EIP-1014
        (check-equal? (test-vector name addr salt init-code) (validate-address-0x result)))
      (tv "Example 0" "0x0000000000000000000000000000000000000000"
          "0x0000000000000000000000000000000000000000000000000000000000000000"
          "0x00"
          "0x4D1A2e2bB4F88F0250f26Ffff098B0b30B26BF38")
      (tv "Example 1" "0xdeadbeef00000000000000000000000000000000"
          "0x0000000000000000000000000000000000000000000000000000000000000000"
          "0x00"
          "0xB928f69Bb1D91Cd65274e3c79d8986362984fDA3")
      (tv "Example 2" "0xdeadbeef00000000000000000000000000000000"
          "0x000000000000000000000000feed000000000000000000000000000000000000"
          "0x00"
          "0xD04116cDd17beBE565EB2422F2497E06cC1C9833")
      (tv "Example 3" "0x0000000000000000000000000000000000000000"
          "0x0000000000000000000000000000000000000000000000000000000000000000"
          "0xdeadbeef"
          "0x70f2b2914A2a4b783FaEFb75f459A580616Fcb5e")
      (tv "Example 4" "0x00000000000000000000000000000000deadbeef"
          "0x00000000000000000000000000000000000000000000000000000000cafebabe"
          "0xdeadbeef"
          "0x60f3f640a8508fC6a86d45DF051962668E1e8AC7")
      (tv "Example 5" "0x00000000000000000000000000000000deadbeef"
          "0x00000000000000000000000000000000000000000000000000000000cafebabe"
          "0xdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeef"
          "0x1d8bfDC5D46DC4f61D6b6115972536eBE6A8854C")
      (tv "Example 6" "0x0000000000000000000000000000000000000000"
          "0x0000000000000000000000000000000000000000000000000000000000000000"
          "0x"
          "0xE33C0C7F7df4809055C3ebA6c09CFe4BaF1BD9e0"))))
