(export hex-test)

(import
  :gerbil/gambit
  :std/error :std/text/hex :std/test :std/srfi/1 :std/sugar
  :clan/exception
  :clan/crypto/keccak
  ../hex)

(def hex-test
  (test-suite "test suite for ethereum/hex"
    (test-case "nat<-0x, 0x<-nat"
      (defrule (nat<->0x-checks (dec hex) ...)
        (begin
          (begin
            (check-equal? (nat<-0x hex) dec)
            (check-equal? (0x<-nat dec) hex)) ...))
      (nat<->0x-checks
       (0 "0x0")
       (10 "0xa")
       (3735928559 "0xdeadbeef")
       (291 "0x123")
       (8271117963530313756381553648673 "0x68656c6c6f2c20776f726c6421"))
      (defrule (nat<-0x-error-checks (hex err) ...)
        (begin
          (check-equal? (with-catch (lambda (e) (if (ContractViolation? e)
                                             (third (Error-irritants e))
                                             (Error-message e)))
                                    (cut nat<-0x hex))
                        err) ...))
      (nat<-0x-error-checks
       ("0" "bad argument; expected 0x prefix")
       ("" "bad argument; expected 0x prefix")
       ("0x" "bad argument; expected at least one hexit for 0x quantity")
       ("0x213Z" "invalid hex char")
       ("0x0123" "bad argument; expected no leading zero for 0x quantity")))

    (test-case "bytes<-0x, 0x<-bytes"
      (defrule (0x<->bytes-checks (dec hex) ...)
        (begin
          (begin
            (check-equal? (bytes<-0x hex) dec)
            (check-equal? (0x<-bytes dec) hex)) ...))
      (0x<->bytes-checks
       (#u8() "0x")
       (#u8(0) "0x00")
       (#u8(0 0) "0x0000")
       (#u8(1 35) "0x0123")
       ((@bytes "abcd") "0x61626364")
       ((@bytes "\r\n") "0x0d0a")
       ((@bytes "hello, world!") "0x68656c6c6f2c20776f726c6421")
       ((keccak256<-bytes #u8()) "0xc5d2460186f7233c927e7db2dcc703c0e500b653ca82273b7bfad8045d85a470"))
      (defrule (bytes<-0x-error-checks (hex err) ...)
        (begin
          (check-equal? (try (bytes<-0x hex)
                             (catch (ContractViolation? e) (third (Error-irritants e))))
                        err) ...))
      (bytes<-0x-error-checks
       ("0" "bad argument; expected 0x prefix")
       ("" "bad argument; expected 0x prefix")
       ("004200" "bad argument; expected 0x prefix")
       ("0x0" "bad argument; expected even number of digits in 0x string")
       ("0xf0f0f" "bad argument; expected even number of digits in 0x string")))
    (test-case "0x <-> address"
      (defrule (address<->0x-checks hex ...)
        (begin
          (begin
            (check-equal? (0x<-address-bytes (bytes<-0x hex)) hex)
            (check-equal? (address-bytes<-0x hex) (bytes<-0x hex))) ...))
      (address<->0x-checks
       ;; These 4 test vectors are from EIP-55 itself:
       "0x5aAeb6053F3E94C9b9A09f33669435E7Ef1BeAed"
       "0xfB6916095ca1df60bB79Ce92cE3Ea74c37c5d359"
       "0xdbF03B407c01E7cD3CBea99509d93f8DDDC8C6FB"
       "0xD1220A0cf47c7B9Be7A2E6BA89F429762e7b9aDb"
       ;; Random addresses
       "0x9797809415E4B8efEa0963E362ff68B9d98F9e00"
       "0x507877C2E26f1387432D067D2DaAfa7d0420d90a")
      (defrule (address<-0x-error-checks (hex msg irr ...) ...)
        (begin
          ;; as of Gerbil 0.17.0-311-g1f1307c2, the "argument" value isn't included in the error.
          (check-equal? (try (address-bytes<-0x hex)
                             (catch (ContractViolation? e) (third (Error-irritants e))))
                        msg) ...))
      (address<-0x-error-checks
       ("0x9797809415e4b8efea0963e362ff68b9d98f9e00"
        "bad argument; expected valid address checksum"
        "0x9797809415e4b8efea0963e362ff68b9d98f9e00" 12)
       ("0x507877C2E26f1387432D067D2DaAfa7D0420d90a"
        "bad argument; expected valid address checksum"
        "0x507877C2E26f1387432D067D2DaAfa7D0420d90a" 33)
       ("0x507877" "bad argument; expected 40 hexits for address" "0x507877")))))
