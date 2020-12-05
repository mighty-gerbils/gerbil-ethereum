(export #t)

(import :std/iter
        :gerbil/gambit/bytes
        :std/sugar :std/test
        :clan/base :clan/poo/poo :clan/poo/io :clan/poo/brace
        ../rlp ../hex)

(def (bs . vs)
  (apply u8vector-append
    (for/collect ((v vs))
      (cond ((bytes? v) v)
            ((and (integer? v) (<= 0 v 255)) (u8vector v))
            ((string? v) (string->bytes v))
            (else (error 'bs))))))

(defrule (check-rep parse unparse rep obj)
  (begin ;;let ((rep rep) (obj obj))
    (check-equal? (parse rep) obj)
    (check-equal? (unparse obj) rep)))

(def rlp-test
  (test-suite "test suite for ethereum/rlp"
    (test-case "examples from the RLP section of the Ethereum wiki"
      ; https://eth.wiki/en/fundamentals/rlp#examples
      (check-rep rlpbytes<-rlp rlp<-rlpbytes (bs "dog") (bs #x83 "dog"))
      (check-rep rlpbytes<-rlp rlp<-rlpbytes [(bs "cat") (bs "dog")] (bs #xc8 #x83 "cat" #x83 "dog"))
      (check-rep rlpbytes<-rlp rlp<-rlpbytes (bs "") (bs #x80))
      (check-rep rlpbytes<-rlp rlp<-rlpbytes [] (bs #xc0))
      (check-rep rlpbytes<-rlp rlp<-rlpbytes (rlp<-nat 0) (bs #x80))
      (check-rep rlpbytes<-rlp rlp<-rlpbytes (rlp<-nat 15) (bs #x0f))
      (check-rep rlpbytes<-rlp rlp<-rlpbytes (rlp<-nat 1024) (bs #x82 #x04 #x00))
      (check-rep rlpbytes<-rlp rlp<-rlpbytes [[] [[]] [[] [[]]]] (bs #xc7 #xc0 #xc1 #xc0 #xc3 #xc0 #xc1 #xc0))
      (check-rep rlpbytes<-rlp rlp<-rlpbytes
                 (bs "Lorem ipsum dolor sit amet, consectetur adipisicing elit")
                 (bs #xb8 #x38 "Lorem ipsum dolor sit amet, consectetur adipisicing elit")))

    (test-case "example from eip-155"
      ; https://github.com/ethereum/EIPs/blob/master/EIPS/eip-155.md
      (check-rep rlpbytes<-rlp rlp<-rlpbytes
                 [(rlp<-nat 9)
                  (rlp<-nat (* 20 (expt 10 9)))
                  (rlp<-nat 21000)
                  (bytes<-0x "0x3535353535353535353535353535353535353535")
                  (rlp<-nat (expt 10 18))
                  #u8()
                  (rlp<-nat 1)
                  #u8()
                  #u8()]
                 (bytes<-0x "0xec098504a817c800825208943535353535353535353535353535353535353535880de0b6b3a764000080018080")))

    (test-case "other RLP encoding tests"
      (let ((bstr (bs "abcdefghi jklmnopqr stuvwxyz, 32abcdefghi jklmnopqr stuvwxyz, 64abcdefghi jklmnopqr stuvwxyz, 96"
                      "abcdefghi jklmnopqr stuvwxyz 128abcdefghi jklmnopqr stuvwxyz 160abcdefghi jklmnopqr stuvwxyz 192"
                      "abcdefghi jklmnopqr stuvwxyz 224abcdefghi jklmnopqr stuvwxyz 256abcdefghi jklmnopqr stuvwxyz 288"
                      "abcdefghi jklmnopqr stuvwxyz 320abcdefghi jklmnopqr stuvwxyz 352abcdefghi jklmnopqr stuvwxyz 384"
                      "abcdefghi jklmnopqr stuvwxyz 416abcdefghi jklmnopqr stuvwxyz 448abcdefghi jklmnopqr stuvwxyz 480"
                      "abcdefghi jklmnopqr stuvwxyz 512abcdefghi jklmnopqr stuvwxyz 544abcdefghi jklmnopqr stuvwxyz 576"
                      "abcdefghi jklmnopqr stuvwxyz 608abcdefghi jklmnopqr stuvwxyz 640abcdefghi jklmnopqr stuvwxyz 672"
                      "abcdefghi jklmnopqr stuvwxyz 704abcdefghi jklmnopqr stuvwxyz 736abcdefghi jklmnopqr stuvwxyz 768"
                      "abcdefghi jklmnopqr stuvwxyz 800abcdefghi jklmnopqr stuvwxyz 832abcdefghi jklmnopqr stuvwxyz 864"
                      "abcdefghi jklmnopqr stuvwxyz 896abcdefghi jklmnopqr stuvwxyz 928abcdefghi jklmnopqr stuvwxyz 960"
                      "abcdefghi jklmnopqr stuvwxyz 992abcdefghi jklmnopqr stuvwxyz1024")))
        (check-rep rlpbytes<-rlp rlp<-rlpbytes bstr (bs #xb9 #x04 #x00 bstr)))

      (check-rep rlpbytes<-rlp rlp<-rlpbytes (bs "2re3mi1do1do5sol") (bs #x90 "2re3mi1do1do5sol"))
      (check-rep rlpbytes<-rlp rlp<-rlpbytes (bs "5sol7ti2re5sol1do") (bs #x91 "5sol7ti2re5sol1do")))))
