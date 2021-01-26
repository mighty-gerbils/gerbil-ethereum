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
  ;; These keys are chosen for a common name and recognizable prefix, for use on private test networks
  ;; With our naive algorithm, finding a 5-char hex prefix should take a few minutes,
  ;; a 6-char hex prefix an hour or two, a 7-char hex prefix a day or two.
  (alice    "0x33bbf7ff271c056cae4eba6503ad46d8cf6f4c35120ef97cc6ee719cf711e767") ;; 0xa71CE
  (bob      "0x30ce4a96f528bbfcd20d8c0c52f5c691f7e9675ef87e5a955e4e2d6f09c35ab0") ;; 0xb0bb1e
  (trent    "0x2d7d92a15f28bb6d56823a10c9a361e97bcd27714761dd95113765a9e5b33595") ;; 0x73e27
  ;; This is the penny collector for private test networks
  (penny    "0x658cb9b4e92593a1eb186ba5fad03e95cadd11e49f39452bb99afdc79d28031f") ;; 0xc011e ;;... c7 ?
  ;; This key is used in some of Ethereum standard test-case
  (fortysix "0x4646464646464646464646464646464646464646464646464646464646464646")
  ;; This key was chosen because it's got money on in genesis block for IOHK's Mantis docker image.
  ;; We now use the same key as the "got all the money" account on our Geth genesis block.
  (croesus  "0x1167a41c432d1a494408b8fdeecd79bff89a5689925606dff8adf01f4bf92922"))

;; Register test keypairs
(for-each (cut apply register-keypair <>) test-keys)

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
      (check-user alice "0xa71CEb0990dD1f29C2a064c29392Fe66baf05aE1"
                  "0xfea1970515a4c6e14e17e412b9e1d3ab9150f829c7df2d1d99726036e45d7ff01a6da0110b702550d4caecd30c6a4a8567280f0746048dc8e19fc7604fa55bfc")
      (check-user bob "0xb0bb1ed229f5Ed588495AC9739eD1555f5c3aabD"
                  "0xebc17d370fb860face57b9c4b48e3378604ec333e894b02f8c113e7c2325c5e564e968cba4bae5b1b0466bef65b49a291573508f463075eeccfc31577921c0bf")
      (check-user penny "0xC011edE680f0532044a6ca87aF226741e981e522"
                  "0x822985956cfab1735ea1dd0144a62418167f1b768607f79b7d709b099f8f9a8396f92ec6d02329131d8cf1afed0773842b86db19ba118b11463c16e1a237995a")
      (check-user trent "0x73e27C9B8BF6F00A38cD654079413aA3eDBC771A"
                  "0x4d484f5419fa4745a6b7e7a9eef7563435543d68250f02fa88c889666578daf600b1935a0624fef6b760a691db1e976d0332786277bbf7c13f39bc54114b58bf"))
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
