(export #t)

(import
  (only-in :gerbil/gambit object->string)
  (only-in :std/format format printf)
  (only-in :std/sugar defrule with-id)
  (only-in :std/test test-suite test-case check-equal?)
  (only-in :clan/exception string<-exception)
  (only-in :clan/poo/io <-bytes bytes<- json-string<-)
  (only-in :clan/crypto/keccak keccak256<-string)
  (only-in :clan/crypto/secp256k1 PublicKey SecretKey Signature recover-signer-public-key)
  (only-in ../hex validate-address-0x bytes<-0x)
  (only-in ../types json<- String sexp<-)
  (only-in ../known-addresses Keypair keypair-address keypair-secret-key keypair-public-key)
  (only-in ../ethereum Address signature-valid? make-signature address<-create2
           0x<-address address<-0x)
  (only-in ../testing croesus croesus-keys alice alice-keys bob bob-keys trent trent-keys
           capitalize))

(def (show-representations name x (type #f))
  (printf "~a:\n  display: ~a\n  write: ~s\n  pr: ~r\n" name x x x)
  (force-output)
  (defrule (X foo) (with-catch string<-exception (lambda () foo)))
  (when type (printf "  json: ~a\n  sexp: ~a\n"
                     (X (json-string<- type x)) (X (object->string (sexp<- type x))))))

(def (make-signature-wrong sig)
  (def bytes (bytes<- Signature sig))
  (u8vector-set! bytes 4 (1- (u8vector-ref bytes 4))) ;; arbitrarily decrement the fifth byte
  (<-bytes Signature bytes))

(def ethereum-test
  (test-suite "test suite for ethereum/ethereum"
    (test-case "check test users"
      (defrule (check-user name addressj pubkeyj)
        (with-id ethereum-test
          ((keys #'name '-keys)
           (address #'name)
           (pubkey #'name '-pubkey)
           (seckey #'name '-seckey))
          (def Name (capitalize 'name))
          (show-representations keypair: keys Keypair)
          (def data (format "some arbitrary string for ~a to sign" Name))
          (check-equal? (keypair-address keys) address)
          (show-representations address: address Address)
          (def pubkey (keypair-public-key keys))
          (show-representations pubkey: pubkey PublicKey)
          (def seckey (keypair-secret-key keys))
          (show-representations seckey: seckey SecretKey)
          (def signature (make-signature String seckey data))
          (show-representations signature: signature Signature)
          (check-equal? (json<- PublicKey pubkey) pubkeyj)
          (check-equal? (json<- Address address) addressj)
          (check-equal? (json<- PublicKey (recover-signer-public-key signature (keccak256<-string data)))
                        (json<- PublicKey pubkey))
          (check-equal? (signature-valid? String address signature data) #t)
          (check-equal? (signature-valid? String address (make-signature-wrong signature) data) #f)))
      (check-user croesus "0x25c0bb1A5203AF87869951AEf7cF3FEdD8E330fC" ;; "0x000d836201318ec6899a67540690382780743280"
                  "3dfbd16d74816ad656f6c98e2a6634ca1930b5fc450eb93ca0a92574a30d00ff8eefd9d1cc3cd81cbb021b3f29abbbabfd29da7feef93f40f63a1e512c240517")
      (check-user alice "0xa71CEb0990dD1f29C2a064c29392Fe66baf05aE1"
                  "fea1970515a4c6e14e17e412b9e1d3ab9150f829c7df2d1d99726036e45d7ff01a6da0110b702550d4caecd30c6a4a8567280f0746048dc8e19fc7604fa55bfc")
      (check-user bob "0xb0bb1ed229f5Ed588495AC9739eD1555f5c3aabD"
                  "ebc17d370fb860face57b9c4b48e3378604ec333e894b02f8c113e7c2325c5e564e968cba4bae5b1b0466bef65b49a291573508f463075eeccfc31577921c0bf")
      #;(check-user penny "0xC0773c13b36eB92813aFE5e89EE89b633c5B1F15"
                  "214ca55e0820309901dfa488caf451941db517c881078512e9e0c883b4647dbb7208661848d780b040e718a4487e2ee8fcb3d41d6f3d344c9af3cb59f20ca2e0")
      (check-user trent "0x73e27C9B8BF6F00A38cD654079413aA3eDBC771A"
                  "4d484f5419fa4745a6b7e7a9eef7563435543d68250f02fa88c889666578daf600b1935a0624fef6b760a691db1e976d0332786277bbf7c13f39bc54114b58bf"))
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
