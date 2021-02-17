(export #t (import: :clan/crypto/secp256k1))
(import
  :gerbil/gambit/bits :gerbil/gambit/bytes :gerbil/gambit/foreign
  :std/misc/bytes :std/misc/repr
  :clan/base :clan/io :clan/poo/object
  :clan/crypto/random
  :clan/poo/brace :clan/poo/io
  :clan/crypto/keccak :clan/crypto/secp256k1 :clan/crypto/secp256k1-ffi
  ./types ./hex)

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
   .string<-: 0x<-address
   .<-string: address<-0x
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
