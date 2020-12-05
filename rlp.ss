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
        :gerbil/gambit/bytes
        :gerbil/gambit/ports
        :clan/base :clan/io
        :clan/poo/poo
        :clan/poo/mop
        (only-in :clan/number bytes<-nat nat<-bytes integer-length-in-bytes))

;; Here is a summary of the RLP specification, as lifted from
;; the Ethereum wiki and the Ethereum yellowpaper Appendix B on RLP,
;; compressed in a way that makes it easier to write code based on it.
;;   https://eth.wiki/en/fundamentals/rlp#definition
;;   https://ethereum.github.io/yellowpaper/paper.pdf#appendix.B
;;
;; An Rlp is one of:
;;  - Bytes
;;  - [Listof Rlp]
;; interp. an Rlp Item from:
;; https://eth.wiki/en/fundamentals/rlp#definition
;;
;; An RlpBytes is a Bytes encoding of an Rlp
;; interp. an Rlp Encoding from:
;; https://eth.wiki/en/fundamentals/rlp#definition
;;
;; RLP encoding is defined as follows:
;;  - Item:
;;     - 1 byte in the range [0x00, 0x7f]:
;;        * byte is its own RLP encoding, in the range [0x00, 0x7f]
;;     - 0-55 bytes:
;;        * byte with value (0x80 + length), in the range [0x80, 0xb7]
;;        * bytes of the item
;;     - >55 bytes:
;;        * byte with value (0xb7 + length-of-length), in the range [0xb8, 0xbf]
;;        * length encoded in big-endian bytes
;;        * bytes of the item
;;       For example, a length-1024 string would be encoded as
;;       \xb9\x04\x00 followed by the string. 0xb9 comes from 0xb7 + 2.
;;  - List:
;;    let payload = bytes of the rlp-encoded elements concatenated together
;;     - total payload 0-55 bytes:
;;        * byte with value (0xc0 + length), in the range [0xc0, 0xf7]
;;        * bytes of the payload
;;     - total payload >55 bytes:
;;        * byte with value (0xf7 + length-of-length), in the range [0xf8, 0xff]
;;        * length of payload encoded in big-endian bytes
;;        * bytes of the payload
;;
;; The first byte could be in one of these ranges:
;;  - [0x00, 0xbf]: item
;;     - [0x00, 0x7f]: 1 byte item
;;     - [0x80, 0xb7]: 0-55 byte item
;;     - [0xb8, 0xbf]: >55 byte item
;;  - [0xc0, 0xff]: list
;;     - [0xc0, 0xf7]: 0-55 byte list
;;     - [0xf8, 0xff]: >55 byte list
;;
;; The length-of-length is the integer-length-in-bytes from :clan/number,
;; which is also equal to ⎡log_{256} (1+n)⎤
;; (where ⎡x⎤ is the ceiling of x and log_b (x) is the logarithm base b of x)

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
     (write-bytes bs out))
    (else
     (let ()
       (def nn (integer-length-in-bytes n))
       (write-u8 (+ #xb7 nn) out)
       (write-bytes (rlp<-nat n) out)
       (write-bytes bs out)))))

;; rlp-write : Rlp OutputPort <- Void
;; Encodes the Rlp item and writes it to the given output port
;; https://eth.wiki/en/fundamentals/rlp#definition
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
          (write-bytes payload out))
         (else
          (let ()
            (def nn (integer-length-in-bytes n))
            (write-u8 (+ #xf7 nn) out)
            (write-bytes (rlp<-nat n) out)
            (write-bytes payload out))))))))

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
;; Decodes an Rlp item from the given input port, or eof
;; https://eth.wiki/en/fundamentals/rlp#rlp-decoding
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
          (read-bytes* n in)))
       ; >55 byte item
       (else
        (let ()
          (def nn (- first-byte #xb7))
          (def n (nat<-rlp (read-bytes* nn in)))
          (def bs (read-bytes* n in))
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
          (def payload (read-bytes* n in))
          (rlp-read-list-payload (open-input-u8vector payload) [])))
       ; >55 byte list
       (else
        (let ()
          (def nn (- first-byte #xf7))
          (def n (nat<-rlp (read-bytes* nn in)))
          (def payload (read-bytes* n in))
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
