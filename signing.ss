(export #t)
(import
  :gerbil/gambit/bits :gerbil/gambit/bytes :gerbil/gambit/foreign
  :std/sugar :std/misc/repr
  :clan/base :clan/poo/poo (only-in :clan/poo/mop Any Type. define-type) :clan/poo/brace :clan/poo/io
  :clan/crypto/keccak :clan/crypto/secp256k1
  ./types ./hex
  )

;; NB: We hide secret keys behind an opaque data structure, so the data won't leak as easily.
(defstruct secp256k1-secret-key (data) print: #f equal: #t)
(define-type SecretKey
  {(:: @ [Type.])
   sexp: 'SecretKey
   .length-in-bytes: 32
   .element?: (lambda (x) (and (secp256k1-secret-key? x) (element? Bytes32 (secp256k1-secret-key-data x))))
   .bytes<-: (λ (x) (secp256k1-secret-key-data x))
   .<-bytes: (λ (b) (secp256k1-secret-key (validate Bytes32 b)))
   .marshal: (λ (x port) (marshal Bytes32 (secp256k1-secret-key-data x) port))
   .unmarshal: (λ (port) (secp256k1-secret-key (unmarshal Bytes32 port)))
   .sexp<-: (lambda (x) `(<-bytes SecretKey ,(.bytes<- x)))
   .json<-: (compose 0x<-bytes .bytes<-)
   .<-json: (compose .<-bytes bytes<-0x)
   .string<-: .json<-
   .<-string: .<-json})

;; Should we store the pubkey as a foreign object, or as bytes to be parsed each time?
;; TODO: implement :pr methods so we can have easy access to the 0x representation.
(define-type PublicKey
  {(:: @ [methods.marshal<-bytes Type.])
   .element?: (lambda (x) (and (foreign? x) (equal? (foreign-tags x) '(secp256k1-pubkey*))))
   .bytes<-: bytes<-secp256k1-pubkey
   .<-bytes: secp256k1-pubkey<-bytes
   .json<-: (lambda (x) (json<- Bytes (.bytes<- x)))
   .<-json: (lambda (x) (.<-bytes (<-json Bytes x)))
   .string<-: .json<-
   .<-string: .<-json})

(defstruct password (string) print: #f equal: #t)
(define-type Password
  {(:: @ [methods.marshal<-bytes Type.])
   sexp: 'Password
   .element?: (lambda (x) (and (password? x) (element? String (password-string x))))
   .string<-: password-string
   .<-string: (compose make-password (.@ String .validate))
   .bytes<-: (compose (.@ String .bytes<-) .string<-)
   .<-bytes: (compose .string<- (.@ String .<-bytes))
   .sexp<-: (lambda (x) `(<-string Password ,(.string<- x)))
   .json<-: .string<-
   .<-json: .<-string})

(defstruct keypair (address public-key secret-key password) transparent: #t)

(define-type Keypair
  {(:: @ Type.)
    .element?: keypair?
    .json<-: (lambda (kp) (hash ("seckey" (json<- SecretKey (keypair-secret-key kp)))
                           ("password" (json<- Password (keypair-password kp)))))
    .<-json: (lambda (h) (keypair<-secret-key (<-json SecretKey (hash-get h "seckey"))
                                         (<-json Password (hash-get h "password"))))})

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

(defstruct secp256k1-sig (data) print: #f equal: #t)

;; TODO: Handle decoding to/from Ethereum-style v,r,s with magic chain-id dependent v.
(def (marshal-signature signature port)
  (defvalues (bytes recid) (bytes<-secp256k1-recoverable-signature (secp256k1-sig-data signature)))
  (write-bytes bytes port)
  (write-byte (+ recid 27) port))

(def (unmarshal-signature port)
  (def compact (read-bytes 64 port))
  (def recid (- (read-byte port) 27))
  (secp256k1-sig (secp256k1-recoverable-signature<-bytes compact recid)))

(.def (Signature @ [methods.bytes<-marshal Type.] .bytes<- .<-bytes)
   sexp: 'Signature
   .length-in-bytes: 65
   .element?: (lambda (x) (and (secp256k1-sig? x) (element? Bytes65 (secp256k1-sig-data x))))
   .marshal: marshal-signature
   .unmarshal: unmarshal-signature
   .sexp<-: (lambda (x) `(<-bytes Signature ,(.bytes<- x)))
   .json<-: (compose 0x<-bytes .bytes<-)
   .<-json: (compose .<-bytes bytes<-0x))

#; ;;TODO: figure out why this message will work at the REPL but not here even with (import :std/misc/repr) (import :clan/poo/brace) and/or (import (prefix-in (only-in <MOP> @method) @))
(defmethod (@@method :pr secp256k1-sig)
  (λ (self (port (current-output-port)) (options (current-representation-options)))
    (write (sexp<- Signature self) port)))

(define-type Signed
  (Record payload: [Any] signature: [Signature]))

;; Signature <- 'a:Type SecKey 'a
(def (make-message-signature secret-key message32)
  (secp256k1-sig
   (make-secp256k1-recoverable-signature message32 secret-key)))

;; Signature <- 'a:Type SecKey 'a
(def (make-signature type secret-key data)
  (make-message-signature secret-key (keccak256<-bytes (bytes<- type data))))

;; Bool <- Address Signature Digest
(def (message-signature-valid? address signature message32)
  (with-catch false
    (lambda ()
      (def pubkey (secp256k1-recover (secp256k1-sig-data signature) message32))
      (equal? address (address<-public-key pubkey)))))

;; Bool <- 'a:Type Address Signature 'a
(def (signature-valid? type address signature data)
  (message-signature-valid? address signature (keccak256<-bytes (bytes<- type data))))
