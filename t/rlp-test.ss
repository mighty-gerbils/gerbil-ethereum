(export #t)

(import :std/iter
        :gerbil/gambit
        :std/sugar :std/test
        :clan/base :clan/poo/object :clan/poo/io :clan/poo/brace
        ../rlp ../hex ../types ../ethereum ../transaction)

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

(def ((poobytes=? t) a b) (equal? (bytes<- t a) (bytes<- t b)))

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
      (def nonce 9)
      (def gasPrice (wei<-gwei 20))
      (def gas 21000)
      (def to (address<-0x "0x3535353535353535353535353535353535353535"))
      (def value (wei<-ether 1))
      (def data (bytes<-0x "0x"))
      (def chainid 1)
      (check-rep rlpbytes<-rlp rlp<-rlpbytes
                 [(rlp<-nat nonce)
                  (rlp<-nat gasPrice)
                  (rlp<-nat gas)
                  (bytes<- Address to)
                  (rlp<-nat value)
                  data
                  (rlp<-nat chainid)
                  #u8()
                  #u8()]
                 (bytes<-0x "0xec098504a817c800825208943535353535353535353535353535353535353535880de0b6b3a764000080018080"))
      ; encoding
      (check-equal? (rlpbytes<- SignedTransactionData
                      {nonce gasPrice gas to value data v: chainid r: 0 s: 0})
                    (bytes<-0x "0xec098504a817c800825208943535353535353535353535353535353535353535880de0b6b3a764000080018080"))
      (check-equal? (signed-tx-bytes<- nonce gasPrice gas to value data chainid 0 0)
                    (bytes<-0x "0xec098504a817c800825208943535353535353535353535353535353535353535880de0b6b3a764000080018080"))
      (check-equal? (rlpbytes<- SignedTransactionData
                      {nonce gasPrice gas to: (void) value data v: chainid r: 0 s: 0})
                    (bytes<-0x "0xd8098504a817c80082520880880de0b6b3a764000080018080"))
      (check-equal? (signed-tx-bytes<- nonce gasPrice gas (void) value data chainid 0 0)
                    (bytes<-0x "0xd8098504a817c80082520880880de0b6b3a764000080018080"))
      ; decoding
      (check (poobytes=? SignedTransactionData)
             (<-rlpbytes SignedTransactionData
               (bytes<-0x "0xec098504a817c800825208943535353535353535353535353535353535353535880de0b6b3a764000080018080"))
             {nonce gasPrice gas to value data v: chainid r: 0 s: 0})
      (check (poobytes=? SignedTransactionData)
             (<-rlpbytes SignedTransactionData
               (bytes<-0x "0xd8098504a817c80082520880880de0b6b3a764000080018080"))
             {nonce gasPrice gas to: (void) value data v: chainid r: 0 s: 0}))

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
      (check-rep rlpbytes<-rlp rlp<-rlpbytes (bs "5sol7ti2re5sol1do") (bs #x91 "5sol7ti2re5sol1do")))

    (test-case "datatype RLP tests"
      (check-rep (.@ UInt .rlp<-) (.@ UInt .<-rlp) 0 (bs))
      (check-rep (.@ UInt .rlp<-) (.@ UInt .<-rlp) 15 (bs #x0f))
      (check-rep (.@ UInt .rlp<-) (.@ UInt .<-rlp) 1024 (bs #x04 #x00))
      (check-rep (.@ Address .rlp<-) (.@ Address .<-rlp)
                 (address<-0x "0x3535353535353535353535353535353535353535")
                 (bytes<-0x "0x3535353535353535353535353535353535353535"))
      (check-rep (.@ (Maybe Address) .rlp<-) (.@ (Maybe Address) .<-rlp)
                 (address<-0x "0x3535353535353535353535353535353535353535")
                 (bytes<-0x "0x3535353535353535353535353535353535353535"))
      (check-rep (.@ (Maybe Address) .rlp<-) (.@ (Maybe Address) .<-rlp) (void) (bs))
      (check-rep (.@ (Tuple UInt UInt UInt) .rlp<-) (.@ (Tuple UInt UInt UInt) .<-rlp)
                 (vector 3 4 5)
                 [(bs #x03) (bs #x04) (bs #x05)])
      (check-rep (.@ (Tuple UInt UInt UInt) .rlp<-) (.@ (Tuple UInt UInt UInt) .<-rlp)
                 (vector 68 285 293)
                 [(bs #x44) (bs #x01 #x1d) (bs #x01 #x25)])
      (check-equal? (rlp<- (Record a: [UInt] b: [UInt] c: [UInt]) {a: 3 b: 4 c: 5})
                    [(bs #x03) (bs #x04) (bs #x05)])
      (check-equal? (rlp<- (Record a: [UInt] b: [UInt] c: [UInt]) {a: 68 b: 285 c: 293})
                    [(bs #x44) (bs #x01 #x1d) (bs #x01 #x25)])
      (check-equal? (rlpbytes<- (Record a: [UInt] b: [UInt] c: [UInt]) {a: 68 b: 285 c: 293})
                    (bytes<-0x "0xc74482011d820125"))
      (check (poobytes=? (Record a: [UInt] b: [UInt] c: [UInt]))
             (<-rlpbytes (Record a: [UInt] b: [UInt] c: [UInt])
               (bytes<-0x "0xc74482011d820125"))
             {a: 68 b: 285 c: 293}))))
