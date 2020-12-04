(export #t)
(import
  :gerbil/gambit/bits :gerbil/gambit/bytes :gerbil/gambit/foreign
  :std/misc/repr
  :clan/base :clan/poo/poo (only-in :clan/poo/mop Any Type. define-type sexp<-)
  :clan/poo/brace :clan/poo/io
  :clan/crypto/keccak :clan/crypto/secp256k1
  ./types ./hex
  )

;; NB: We hide secret keys behind an opaque data structure, so the data won't leak as easily.
(defstruct secp256k1-seckey (data) print: #f equal: #t)
(define-type SecretKey
  {(:: @ [Type.])
   sexp: 'SecretKey
   .length-in-bytes: 32
   .element?: (lambda (x) (and (secp256k1-seckey? x) (element? Bytes32 (secp256k1-seckey-data x))))
   .string<-: repr
   .<-string: invalid
   .bytes<-: invalid
   .<-bytes: invalid
   .sexp<-: (lambda (_) '(invalid "Not showing secret key"))
   .json<-: invalid
   .<-json: invalid})

;; USE WITH CAUTION.
;; Do not leak such data to the outside world. In the future, keep it even tighter locked.
(def (import-secret-key/bytes b) (secp256k1-seckey (validate Bytes32 b)))
(def (export-secret-key/bytes x) (secp256k1-seckey-data x))
(def (import-secret-key/json j) (import-secret-key/bytes (<-json Bytes32 j)))
(def (export-secret-key/json x) (json<- Bytes32 (export-secret-key/bytes x)))

;; Should we store the pubkey as a foreign object, or as bytes to be parsed each time?
;; TODO: implement :pr methods so we can have easy access to the 0x representation.
(define-type PublicKey
  {(:: @ [methods.marshal<-bytes Type.])
   .Bytes: Bytes64
   .element?: (lambda (x) (and (foreign? x) (equal? (foreign-tags x) '(secp256k1-pubkey*))))
   .sexp<-: (lambda (k) `(<-json PublicKey ,(.json<- k)))
   .bytes<-: (lambda (k) (subu8vector (bytes<-secp256k1-pubkey k) 1 65))
   .<-bytes: (lambda (b) (secp256k1-pubkey<-bytes (bytes-append #u8(4) b)))
   .json<-: (lambda (x) (json<- Bytes (.bytes<- x)))
   .<-json: (lambda (x) (.<-bytes (<-json Bytes x)))
   .string<-: .json<-
   .<-string: .<-json})

(defstruct password (string) print: #f equal: #t)
(define-type Password
  {(:: @ [Type.])
   sexp: 'Password
   .element?: (lambda (x) (and (password? x) (element? String (password-string x))))
   .string<-: repr
   .<-string: invalid
   .bytes<-: invalid
   .<-bytes: invalid
   .marshal: invalid
   .unmarshal: invalid
   .sexp<-: (lambda (_) '(invalid "Not showing password"))
   .json<-: invalid
   .<-json: invalid})

;; USE WITH CAUTION.
;; Do not leak such data to the outside world. In the future, keep it even tighter locked.
(def (import-password/string j) (password (validate String j)))
(def (export-password/string j) (password-string j))


(defstruct address (bytes) print: #f equal: #t)
(def 0x<-address (compose 0x<-address-bytes address-bytes))
(def address<-0x (compose make-address (.@ Bytes20 .validate) bytes<-0x))
(def address<-0x/strict (compose make-address address-bytes<-0x))
(define-type Address
  {(:: @ [methods.marshal<-bytes Type.])
   .Bytes: Bytes20
   .element?: address?
   .json<-: 0x<-address
   .<-json: (compose make-address (.@ Bytes20 .<-json))
   .sexp<-: (lambda (x) `(address<-0x ,(0x<-address x)))
   .<-bytes: (compose make-address (.@ Bytes20 .validate))
   .bytes<-: address-bytes
   .length-in-bytes: 20
   .ethabi-name: "address"
   .ethabi-display-type: (cut display .ethabi-name <>)
   .ethabi-head-length: 32
   .ethabi-padding: (- 32 .length-in-bytes)
   .ethabi-tail-length: (lambda (_) 0)
   .ethabi-encode-into:
   (lambda (x bytes start head get-tail set-tail!)
     (.call Bytes20 .ethabi-encode-into x bytes start head get-tail set-tail!))
   .ethabi-decode-from:
   (lambda (bytes start head get-tail set-tail!)
     (.call Bytes20 .ethabi-decode-from bytes start head get-tail set-tail!))
  })

(def (address<-public-key pubkey)
  ;; uncompressed public key has an extra byte at the beginning, which we remove:
  ;; https://bitcoin.stackexchange.com/questions/57855/c-secp256k1-what-do-prefixes-0x06-and-0x07-in-an-uncompressed-public-key-signif
  (!> (bytes<- PublicKey pubkey)
      keccak256<-bytes
      (cut subu8vector <> 12 32)
      make-address))

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
   .sexp<-: (lambda (x) `(<-json Signature ,(.json<- x)))
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
   (make-secp256k1-recoverable-signature message32 (secp256k1-seckey-data secret-key))))

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
