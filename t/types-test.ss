(export #t)

(import
  :gerbil/gambit/bytes
  :gerbil/gambit/exceptions
  :std/error :std/text/hex :std/text/json :std/sort :std/srfi/1 :std/sugar :std/test
  :std/misc/hash
  :clan/base :clan/json :clan/list
  :clan/poo/object :clan/poo/io :clan/poo/brace (only-in :clan/poo/mop define-type)
  ../types ../hex)

(define-type EthereumRpcConfig
  (Record
   scheme: [Symbol]
   host: [String]
   port: [UInt16]))

(define-type Zoth
  (Sum
   z: (Tuple)
   o: (Tuple UInt8)
   t: (Tuple UInt8 UInt8)
   h: (Tuple UInt8 UInt8 UInt8)))
(define-sum-constructors Zoth z o t h)

(define-type LOU8
  (Sum
   empty: (Tuple)
   cons: (Record first: [UInt8] rest: [(delay-type LOU8)])))
(define-sum-constructors LOU8 empty cons)

(def (list<-LOU8 lou8)
  (match lou8
    ((LOU8-empty #()) [])
    ((LOU8-cons {(first) (rest)}) (cons first (list<-LOU8 rest)))))
(def (LOU8<-list l)
  (match l
    ([] (LOU8-empty #()))
    ([first . rst] (LOU8-cons {(first) rest: (LOU8<-list rst)}))))

(def (sort-alist alist) (sort alist (comparing-key test: string<? key: car)))

(defrule (check-rep parse unparse rep obj)
  (begin ;;let ((rep rep) (obj obj))
    (check-equal? (parse rep) obj)
    (check-equal? (unparse obj) rep)))

(def types-test
  (test-suite "test suite for ethereum/types"
    (test-case "Nat"
      (check-equal? (<-json Nat "0x0") 0)
      (check-equal? (<-json Nat 0) 0)
      (check-equal? (<-json Nat "0x15") 21)
      (check-equal? (<-json Nat 21) 21)
      (check-rep (.@ Nat .<-json) (.@ Nat .json<-) "0x22" 34)
      (check-rep (.@ Nat .<-json) (.@ Nat .json<-) "0x37" 55)
      (check-rep (.@ Nat .<-json) (.@ Nat .json<-) "0x50" 80))
    (test-case "Record"
      (check-rep (compose .alist (.@ EthereumRpcConfig .<-json) list->hash-table)
                 (compose Alist-value (.@ EthereumRpcConfig .json<-) object<-alist)
                 '(("scheme" . "http") ("host" . "localhost") ("port" . "0x50"))
                 '((scheme . http) (host . "localhost") (port . 80)))
      (check-rep (compose .alist (.@ EthereumRpcConfig .<-bytes) bytes<-0x)
                 (compose 0x<-bytes (.@ EthereumRpcConfig .bytes<-) object<-alist)
                 "0x00046874747000096c6f63616c686f73740050"
                 '((scheme . http) (host . "localhost") (port . 80))))
    (test-case "Sum"
      (check-equal? (element? Zoth "this is not a poo") #f)
      (check-equal? (element? Zoth (Zoth-z #())) #t)
      (check-equal? (element? Zoth (Zoth-z #(1))) #f)
      (check-equal? (element? Zoth (Zoth-o #(1))) #t)
      (check-equal? (element? Zoth (Zoth-o #(1 2))) #f)
      (check-equal? (element? Zoth (Zoth-o #("this is not an int"))) #f)
      (check-equal? (element? Zoth (Zoth-t #(2 3))) #t)
      (check-equal? (element? Zoth (Zoth-t #(2 3 4))) #f)
      (check-equal? (element? Zoth (Zoth-t #(2 394))) #f)
      (check-equal? (element? Zoth (Zoth-h #(5 8 13))) #t)
      (check-equal? (element? Zoth (Zoth-h #(5 8 13 14))) #f)
      (check-equal? (element? Zoth (Zoth-h #(5 -1 13))) #f)
      (check-equal? (element? Zoth (<-json Zoth (json<-string "{\"tag\": \"z\", \"value\": []}"))) #t)
      (check-equal? (hash->list/sort (json<- Zoth (Zoth-z #())) string<?)
                    '(("tag" . "z") ("value" . ())))
      (check-equal? (.alist/sort (<-json Zoth (hash ("tag" "z") ("value" []))))
                    '((tag . z) (value . #())))
      (check-equal? (sort-alist (hash->list (.call Zoth .json<- (Zoth-z #()))))
                    '(("tag" . "z") ("value" . ())))
      (check-equal? (sort-alist (hash->list (.call Zoth .json<- (Zoth-o #(1)))))
                    '(("tag" . "o") ("value" . ("0x1"))))
      (check-equal? (sort-alist (hash->list (.call Zoth .json<- (Zoth-t #(2 3)))))
                    '(("tag" . "t") ("value" . ("0x2" "0x3"))))
      (check-equal? (sort-alist (hash->list (.call Zoth .json<- (Zoth-h #(5 8 13)))))
                    '(("tag" . "h") ("value" . ("0x5" "0x8" "0xd"))))
      (check-equal? (match (.call Zoth .<-json (hash ("tag" "t") ("value" ["0x15" "0x22"])))
                      ((Zoth-t (vector a b)) (+ a b)))
                    55)
      (def ja<-0x (compose sort-alist hash->list (.@ Zoth .json<-) (.@ Zoth .<-bytes) bytes<-0x))
      (def 0x<-ja (compose 0x<-bytes (.@ Zoth .bytes<-) (.@ Zoth .<-json) list->hash-table))
      (check-rep ja<-0x 0x<-ja "0x00" '(("tag" . "z") ("value" . ())))
      (check-rep ja<-0x 0x<-ja "0x0137" '(("tag" . "o") ("value" . ("0x37"))))
      (check-rep ja<-0x 0x<-ja "0x025990" '(("tag" . "t") ("value" . ("0x59" "0x90"))))
      (check-equal? (element? LOU8 (LOU8-empty #())) #t)
      (check-equal? (element? LOU8 (LOU8-cons {first: 21 rest: (LOU8-empty #())})) #t)
      (check-equal? (element? LOU8 (LOU8-cons {first: 21 rest: 34})) #f)
      (check-equal? (element? LOU8 (LOU8-cons {first: 21 rest: (LOU8-cons {first: 34 rest: (LOU8-empty #())})})) #t)
      (def 0xlou8<-list (compose 0x<-bytes (.@ LOU8 .bytes<-) LOU8<-list))
      (def list<-0xlou8 (compose list<-LOU8 (.@ LOU8 .<-bytes) bytes<-0x))
      (check-rep list<-0xlou8 0xlou8<-list "0x00" [])
      (check-rep list<-0xlou8 0xlou8<-list "0x013700" [55])
      (check-rep list<-0xlou8 0xlou8<-list "0x0159019001e900" [89 144 233])
      )))
