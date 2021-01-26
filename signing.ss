(export #t)
(import
  :gerbil/gambit/bits :gerbil/gambit/bytes :gerbil/gambit/foreign :gerbil/gambit/random
  :std/misc/bytes :std/misc/repr
  :clan/base :clan/io :clan/poo/poo
  :clan/poo/brace :clan/poo/io
  :clan/crypto/keccak :clan/crypto/secp256k1
  ./types ./hex)

;; TODO(2023): Make sure private keys never stay in RAM too long (requires support in gerbil-crypto).
;; Use some hardware key management system so a master key is used to encrypt the keys
;; (using a fast symmetric cypher plus salt), and those keys are kept encrypted,
;; and only decrypted temporarily in buffer that gets overwritten with random noise
;; as soon as they are not used anymore. This reduces the odds of a breach causing keys to leak
;; (SPECTER attack, RAM extraction attack, etc.). Same for any required clear-text password
;; or security token, and for keys for other cyphers -- thus many shared functions and macros.

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

(def secp256k1-p #xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFC2F)
(def secp256k1-order #xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEBAAEDCE6AF48A03BBFD25E8CD0364141)

(def (randomUInt256)
  (def r (random-integer (arithmetic-shift 1 256)))
  (if (file-exists? "/dev/urandom")
    (+ r (<-bytes UInt256 (call-with-input-file "/dev/urandom" (cut read-bytes 32 <>))))
    r))

(def (generate-secret-key-data)
  (bytes<- UInt256 (modulo (randomUInt256) secp256k1-order)))

;; Right now, we use the foreign object as the "canonical" in-memory representation.
;; Should we instead use canonical bytes that parsed into a foreign object on a need basis?
;; TODO: implement :pr methods so we can have easy access to the 0x representation.
(define-type PublicKey
  {(:: @ [methods.marshal<-bytes Type.])
   .Bytes: Bytes64
   .element?: (lambda (x) (and (foreign? x) (equal? (foreign-tags x) '(secp256k1-pubkey*))))
   .sexp<-: (lambda (k) `(<-json PublicKey ,(.json<- k)))
   ;; uncompressed public key has an extra byte at the beginning, which we remove:
   ;; https://bitcoin.stackexchange.com/questions/57855/c-secp256k1-what-do-prefixes-0x06-and-0x07-in-an-uncompressed-public-key-signif
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
   .<-rlp: .<-bytes
   .rlp<-: .bytes<-
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

(def (address<-data data)
  (!> data
      keccak256<-bytes
      (cut subu8vector <> 12 32)
      make-address))

(def (address<-public-key pubkey)
  (address<-data (bytes<- PublicKey pubkey)))

;; https://eips.ethereum.org/EIPS/eip-1014
(def (address<-create2 creator salt init-code)
  (address<-data (bytes-append #u8(#xff) (bytes<- Address creator) salt (keccak256<-bytes init-code))))


(defstruct secp256k1-sig (data) print: #f equal: #t)

;; TODO: Handle decoding to/from Ethereum-style v,r,s with magic chain-id dependent v.
(def (marshal-signature signature port)
  (defvalues (bytes recid) (bytes<-secp256k1-recoverable-signature (secp256k1-sig-data signature)))
  (write-bytes bytes port)
  (write-byte (+ recid 27) port))

(def (unmarshal-signature port)
  (def compact (unmarshal-n-bytes 64 port))
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

(def (vrs<-signature sig)
  (def bytes (bytes<- Signature sig))
  (def v (bytes-ref bytes 64))
  (def r (u8vector-uint-ref bytes 0 big 32))
  (def s (u8vector-uint-ref bytes 32 big 32))
  (values v r s))

(def (signature<-vrs v r s)
  (def bytes (make-bytes 65))
  (u8vector-uint-set! bytes 0 r big 32)
  (u8vector-uint-set! bytes 32 s big 32)
  (bytes-set! bytes 64 v)
  (<-bytes Signature bytes))

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

;; (OrFalse Address) <- Signature Digest
(def (recover-signer-address signature message32)
  (with-catch false
    (lambda ()
      (address<-public-key (secp256k1-recover (secp256k1-sig-data signature) message32)))))

;; Bool <- Address Signature Digest
(def (message-signature-valid? address signature message32)
  (equal? address (recover-signer-address signature message32)))

;; Bool <- 'a:Type Address Signature 'a
(def (signature-valid? type address signature data)
  (message-signature-valid? address signature (keccak256<-bytes (bytes<- type data))))
