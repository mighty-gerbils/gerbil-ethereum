(export #t)
(import
  :gerbil/gambit/bits :gerbil/gambit/bytes :gerbil/gambit/foreign
  :std/sugar
  :clan/base :clan/poo/poo (only-in :clan/poo/mop Any Type. define-type) :clan/poo/brace :clan/poo/io
  :clan/crypto/keccak :clan/crypto/secp256k1
  ./types
  )

(define-type SecretKey Bytes32)

;; Should we store the pubkey as a foreign object, or as bytes to be parsed each time?
(define-type PublicKey
  {(:: @ [methods.marshal<-bytes Type.])
   .element?: (lambda (x) (and (foreign? x) (equal? (foreign-tags x) '(secp256k1-pubkey*))))
   .bytes<-: bytes<-secp256k1-pubkey
   .<-bytes: secp256k1-pubkey<-bytes
   .json<-: (lambda (x) (json<- Bytes (.bytes<- x)))
   .<-json: (lambda (x) (.<-bytes (<-json Bytes x)))})

(defstruct keypair (address public-key secret-key password) transparent: #t)

(define-type Keypair
  {(:: @ Type.)
    .element?: keypair?
    .json<-: (lambda (kp) (hash ("seckey" (json<- SecretKey (keypair-secret-key kp)))
                           ("password" (keypair-password kp))))
    .<-json: (lambda (h) (keypair<-secret-key (<-json SecretKey (hash-get h "seckey"))
                                         (hash-get h "password")))})

#;(Record
   address: [Address]
   public-key: [PublicKey]
   secret-key: [SecretKey]
   password: [String])

(def (address<-public-key pubkey)
  ;; uncompressed public key has an extra byte at the beginning, which we remove:
  ;; https://bitcoin.stackexchange.com/questions/57855/c-secp256k1-what-do-prefixes-0x06-and-0x07-in-an-uncompressed-public-key-signif
  (!> (bytes<-secp256k1-pubkey pubkey)
      (cut subu8vector <> 1 65)
      keccak256<-bytes
      (cut subu8vector <> 12 32)))

(def (keypair<-secret-key seckey password)
  (def public-key (secp256k1-pubkey<-seckey seckey))
  (def address (address<-public-key public-key))
  (keypair address public-key seckey password))

(def (marshal-signature signature port)
  (defvalues (bytes recid) (bytes<-secp256k1-recoverable-signature signature))
  (write-bytes bytes port)
  (write-byte (+ recid 27) port)) ;; TODO: handle the way that ethereum uses an offset different from 27?

(def (unmarshal-signature port)
  (def compact (read-bytes 64 port))
  (def recid (- (read-byte port) 27))
  (secp256k1-recoverable-signature<-bytes compact recid))

(.def (Signature @ [methods.bytes<-marshal Type.])
   sexp: 'Signature
   .length-in-bytes: 65
   .marshal: marshal-signature
   .unmarshal: unmarshal-signature)

(define-type Signed
  (Record payload: [Any] signature: [Signature]))

;; Signature <- 'a:Type SecKey 'a
(def (make-message-signature secret-key message32)
  (make-secp256k1-recoverable-signature message32 secret-key))

;; Signature <- 'a:Type SecKey 'a
(def (make-signature type secret-key data)
  (make-message-signature secret-key (keccak256<-bytes (bytes<- type data))))

;; Bool <- Address Signature Digest
(def (message-signature-valid? address signature message32)
  (with-catch false
    (lambda ()
      (def pubkey (secp256k1-recover signature message32))
      (equal? address (address<-public-key pubkey)))))

;; Bool <- 'a:Type Address Signature 'a
(def (signature-valid? type address signature data)
  (message-signature-valid? address signature (keccak256<-bytes (bytes<- type data))))
