(export <-rlp
        rlp<-
        <-rlpbytes
        rlpbytes<-
        rlp<-nat
        nat<-rlp
        rlpbytes<-rlp
        rlp-write
        rlp<-rlpbytes
        rlp-read)

(import :std/iter
        :std/misc/bytes
        (only-in :gerbil/gambit/bytes bytes?)
        :gerbil/gambit/ports
        :clan/base
        :clan/poo/poo
        :clan/poo/mop
        (only-in :clan/number bytes<-nat nat<-bytes integer-length-in-bytes))

;; An Rlp is one of:
;;  - Bytes
;;  - [Listof Rlp]

;; An RlpBytes is a Bytes encoding of an Rlp

;; read-u8vector : Nat InputPort -> Bytes
(def (read-u8vector n in)
  (def v (make-u8vector n))
  (read-subu8vector v 0 n in n)
  v)

;; --------------------------------------------------------

;; generic functions to convert to and from rlp
(.defgeneric (<-rlp type r) slot: .<-rlp)
(.defgeneric (rlp<- type x) slot: .rlp<-)

(def (rlpbytes<- type x) (rlpbytes<-rlp (rlp<- type x)))
(def (<-rlpbytes type x) (<-rlp type (rlp<-rlpbytes x)))

(.def (methods.rlpbytes<-rlp @ [] .rlp<- .<-rlp)
  .rlpbytes<-: (compose rlpbytes<-rlp .rlp<-)
  .<-rlpbytes: (compose .<-rlp rlp<-rlpbytes))

;; --------------------------------------------------------

;; rlp<-nat : Rlp <- Nat
(def (rlp<-nat n)
  (unless (<= 0 n)
    (error 'rlp<-nat "expected a nonnegative integer, given" n))
  (bytes<-nat n))

;; nat<-rlp : Nat <- Rlp
(def (nat<-rlp bs)
  (unless (bytes? bs)
    (error 'nat<-rlp "expected bytes, given" bs))
  (nat<-bytes bs))

;; --------------------------------------------------------

;; rlpbytes<-rlp : RlpBytes <- Rlp
(def (rlpbytes<-rlp rlp)
  (def out (open-output-u8vector))
  (rlp-write rlp out)
  (get-output-u8vector out))

;; rlp-write-bytes : Bytes OutputPort -> Void
(def (rlp-write-bytes bs out)
  (def n (u8vector-length bs))
  (cond
    ((and (= n 1) (< (u8vector-ref bs 0) #x80))
     (write-u8 (u8vector-ref bs 0) out))
    ((< n 56)
     (write-u8 (+ #x80 n) out)
     (write-u8vector bs out))
    (else
     (let ()
       (def nn (integer-length-in-bytes n))
       (write-u8 (+ #xb7 nn) out)
       (write-u8vector (rlp<-nat n) out)
       (write-u8vector bs out)))))

;; rlp-write : Rlp OutputPort <- Void
(def (rlp-write rlp out)
  (cond
    ((bytes? rlp) (rlp-write-bytes rlp out))
    (else
     (let ()
       (def payload* (open-output-u8vector))
       (for ((e rlp)) (rlp-write e payload*))
       (def payload (get-output-u8vector payload*))
       (def n (u8vector-length payload))
       (cond
         ((< n 56)
          (write-u8 (+ #xc0 n) out)
          (write-u8vector payload out))
         (else
          (let ()
            (def nn (integer-length-in-bytes n))
            (write-u8 (+ #xf7 nn) out)
            (write-u8vector (rlp<-nat n) out)
            (write-u8vector payload))))))))

;; --------------------------------------------------------

;; rlp<-rlpbytes : Rlp <- RlpBytes
(def (rlp<-rlpbytes bs)
  (def in (open-input-u8vector bs))
  (def rlp (rlp-read in))
  (when (eof-object? rlp)
    (error 'rlp<-rlpbytes "expected an rlp item, got eof" rlp))
  (def next (read-u8 in))
  (unless (eof-object? next)
    (error 'rlp<-rlpbytes "expected eof after rlp item, got" next))
  (close-input-port in)
  rlp)

;; rlp-read : InputPort -> (U Rlp Eof)
(def (rlp-read in)
  (def first-byte (read-u8 in))
  (cond
    ((eof-object? first-byte) first-byte)
    ; item
    ((< first-byte #xc0)
     (cond
       ; 1 byte item
       ((< first-byte #x80) (u8vector first-byte))
       ; 0-55 byte item
       ((< first-byte #xb8)
        (let ()
          (def n (- first-byte #x80))
          (read-u8vector n in)))
       ; >55 byte item
       (else
        (let ()
          (def nn (- first-byte #xb7))
          (def n (nat<-rlp (read-u8vector nn in)))
          (def bs (read-u8vector n in))
          (unless (< 55 n)
            (error 'rlp-read "item should be represented with length<=55 mode" bs))
          bs))))
    ; list
    (else
     (cond
       ;  0-55 byte list
       ((< first-byte #xf8)
        (let ()
          (def n (- first-byte #xc0))
          (def payload (read-u8vector n in))
          (rlp-read-list-payload (open-input-u8vector payload) [])))
       ; >55 byte list
       (else
        (let ()
          (def nn (- first-byte #xf7))
          (def n (nat<-rlp (read-u8vector nn in)))
          (def payload (read-u8vector n in))
          (def l (rlp-read-list-payload (open-input-u8vector payload) []))
          (unless (< 55 n)
            (error 'rlp-read "list should be represented with length<=55 mode" l))
          l))))))

;; rlp-read-list-payload : InputPort [Listof Rlp] -> [Listof Rlp]
;; ASSUME the `in` has an eof at the end of the list, does not go beyond
(def (rlp-read-list-payload in acc)
  (def next (rlp-read in))
  (cond
    ((eof-object? next) (reverse acc))
    (else               (rlp-read-list-payload in (cons next acc)))))

;; --------------------------------------------------------
