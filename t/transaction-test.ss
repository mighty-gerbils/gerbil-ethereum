(export #t)

(import
  :gerbil/gambit/exceptions :gerbil/gambit/ports
  :std/format :std/misc/list :std/misc/repr :std/srfi/13 :std/sugar :std/test
  :clan/exception :clan/syntax :clan/with-id
  :clan/poo/poo :clan/poo/brace :clan/poo/io
  :clan/crypto/keccak
  ../hex ../rlp ../types ../ethereum ../known-addresses ../signing ../transaction
  ./signing-test)

(def transaction-test
  (test-suite "Test suite for ethereum/transaction"
    (test-case "transaction signature"
;;(with-logged-exceptions ()
      ;; TODO: complete the test of this example from EIP 155
      ;; https://github.com/ethereum/EIPs/blob/master/EIPS/eip-155.md
      (def sender-keys
        (keypair<-seckey-0x "0x4646464646464646464646464646464646464646464646464646464646464646" ""))
      (def from (keypair-address sender-keys))
      (def nonce 9)
      (def gasPrice (wei<-gwei 20))
      (def gas 21000)
      (def to (address<-0x "0x3535353535353535353535353535353535353535"))
      (def value (wei<-ether 1))
      (def data (bytes<-0x "0x"))
      (def chainid 1)
      (def tx-params {from nonce gasPrice gas to value data})
      (def tx-data-bytes (signed-tx-bytes<- nonce gasPrice gas to value data chainid 0 0))
      (check-equal? tx-data-bytes (rlpbytes<- SignedTransactionData { (:: @ tx-params) v: chainid r: 0 s: 0 }))
      (check-equal? (0x<-bytes tx-data-bytes) "0xec098504a817c800825208943535353535353535353535353535353535353535880de0b6b3a764000080018080")
      (def tx-data-digest (keccak256<-bytes tx-data-bytes))
      (check-equal? (0x<-bytes tx-data-digest)
                    "0xdaf5a779ae972f972197303d7b574746c7ef83eadac0f2791ad23db92e4c8e53")
      (def signature (make-message-signature (keypair-secret-key sender-keys) tx-data-digest))
      (defvalues (v r s) (vrs<-signature signature))
      (def v2 (eip155-v v chainid))
      (check-equal? [v2 r s]
                    [37 18515461264373351373200002665853028612451056578545711640558177340181847433846 46948507304638947509940763649030358759909902576025900602547168820602576006531])
      (def signed-tx-data-bytes (signed-tx-bytes<- nonce gasPrice gas to value data v2 r s))
      (check-equal? (0x<-bytes signed-tx-data-bytes) "0xf86c098504a817c800825208943535353535353535353535353535353535353535880de0b6b3a76400008025a028ef61340bd939bc2195fe537567866003e1a15d3c71ff63e1590620aa636276a067cbe9d8997f761aecb703304b3800ccf555c9f3dc64214b297fb1966a3b6d83")
      (check-equal? (0x<-bytes (bytes<-signed-tx (sign-transaction tx-params 1)))
                    (0x<-bytes signed-tx-data-bytes)))))
;;)
