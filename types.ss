;; We are shadowing existing types. Should we monkey-patch them instead? Let's hope not.
(export #t)
(export (import: :clan/poo/mop)
        (import: :clan/poo/number)
        (import: :clan/poo/type))

;; For re-export
(import
  (except-in :clan/poo/mop Bool)
  (except-in :clan/poo/number Nat UInt. UInt)
  (except-in :clan/poo/type Maybe. BytesN. Symbol String Record Tuple. Enum.))

(import
  (for-syntax (only-in :std/iter for/collect)
              (only-in :std/stxutil symbolify))
  (only-in :std/assert assert!)
  (only-in :std/format format fprintf)
  (only-in :std/iter for in-range)
  (only-in :std/misc/bytes u8vector-uint-set! u8vector-uint-ref u8vector-sint-set! u8vector-sint-ref big)
  (only-in :std/misc/hash hash-ensure-ref)
  (only-in :std/misc/list-builder with-list-builder)
  (only-in :std/misc/number ceiling-align normalize-nat)
  (only-in :std/srfi/43 vector-every vector-fold vector-unfold vector-for-each)
  (only-in :std/sugar defrule)
  (only-in :std/text/json json-object->string)
  (only-in :clan/base compose rcompose λ)
  (only-in :clan/io marshal-uint16 unmarshal-uint16 marshal-sized16-u8vector unmarshal-sized16-u8vector)
  (only-in :std/stxutil symbolify maybe-intern-symbol)
  (only-in :clan/poo/object .def .@ .call .for-each! .mix)
  (only-in :clan/poo/io methods.bytes<-marshal)
  (only-in :clan/poo/rationaldict RationalSet)
  (only-in :clan/poo/mop Type. Type define-type raise-type-error validate json<-)
  (prefix-in (only-in :clan/poo/mop Bool) poo.)
  (prefix-in (only-in :clan/poo/number Nat UInt. UInt Int. Int) poo.)
  (only-in :clan/poo/type methods.bytes)
  (prefix-in (only-in :clan/poo/type Maybe. BytesN. Symbol String Record Tuple. Enum.) poo.)
  (only-in :clan/poo/brace @method)
  (only-in ./hex 0x<-bytes bytes<-0x 0x<-nat nat<-0x)
  (only-in ./abi ethabi-display-types ethabi-head-length ethabi-tail-length
           ethabi-encode-into ethabi-decode-from)
  (only-in ./rlp <-rlp rlp<- rlp<-nat nat<-rlp))

(.def (Maybe. @ [poo.Maybe.] type)
  .rlp<-: (lambda (x) (if (void? x) #u8() (rlp<- type x)))
  .<-rlp: (lambda (x) (if (equal? x #u8()) (void) (<-rlp type x))))
(def (Maybe type) {(:: @ Maybe.) type})

;; --- something for types in general, including Record, Union, Maybe
;; --- something for ethereum types in particular

(.def (DelayedType. @ [Type.] delayed-type)
  .element?: (.@ delayed-type .element?)
  .validate: (.@ delayed-type .validate)
  .sexp<-: (.@ delayed-type .sexp<-)
  .json<-: (.@ delayed-type .json<-)
  .<-json: (.@ delayed-type .<-json)
  .bytes<-: (.@ delayed-type .bytes)
  .<-bytes: (.@ delayed-type .<-bytes)
  .marshal: (.@ delayed-type .marshal)
  .unmarshal: (.@ delayed-type .unmarshal))

(defrule (delay-type delayed-type)
  {(:: @ [DelayedType.]) sexp: 'delayed-type delayed-type: delayed-type})

(def (number<-json j)
  (cond
    ((number? j) j)
    ;; TODO: if necessary, `"#5050"` and `"5050"` cases from
    ;; https://ethereum-tests.readthedocs.io/en/latest/test_types/rlp_tests.html
    ((string? j) (nat<-0x j))
    (else
     (error 'number<-json
       (format "expected a number or a string representing a number, given ~a" (json-object->string j))))))

;; Variable-length Nat
(.def (Nat @ [poo.Nat] .validate)
  .sexp<-: (lambda (x) `(nat<-0x ,(0x<-nat x)))
  .json<-: 0x<-nat
  .<-json: (compose .validate number<-json))

(define-type (NatSet @ RationalSet) Elt: Nat)

(def (ensure-zeroes bytes start len)
  (for (i (in-range len))
    (assert! (zero? (u8vector-ref bytes (+ start i))))))

(def simple-eth-types (make-hash-table))
(def (register-simple-eth-type type (name (.@ type .ethabi-name)))
  (hash-put! simple-eth-types name type))

;; Integer types
(.def (UInt. @ [poo.UInt.] .length-in-bits .length-in-bytes .validate)
  sexp: (symbolify "UInt" .length-in-bits)
  .json<-: 0x<-nat
  .<-json: (compose .validate number<-json)
  .rlp<-: rlp<-nat
  .<-rlp: (compose .validate nat<-rlp)
  .ethabi-name: (format "uint~d" .length-in-bits)
  .ethabi-display-type: (cut display .ethabi-name <>)
  .ethabi-head-length: 32
  .ethabi-padding: (- 32 .length-in-bytes)
  .ethabi-tail-length: (lambda (_) 0)
  .ethabi-encode-into:
  (lambda (x bytes start head get-tail set-tail!)
    (u8vector-uint-set! bytes (+ head .ethabi-padding) x big .length-in-bytes))
  .ethabi-decode-from:
  (lambda (bytes start head get-tail set-tail!)
    (ensure-zeroes bytes head .ethabi-padding)
    (u8vector-uint-ref bytes (+ head .ethabi-padding) big .length-in-bytes)))
(def UInt<-length-in-bits (make-hash-table))
(def (UIntN .length-in-bits)
  (hash-ensure-ref UInt<-length-in-bits .length-in-bits
                   (lambda () (.mix UInt. (poo.UInt .length-in-bits)))))

(.def (Int. @ [poo.Int.] .length-in-bits .length-in-bytes .normalize)
  sexp: (symbolify "Int" .length-in-bits)
  .nat<-: (cut normalize-nat <> .length-in-bits)
  .<-nat: .normalize
  .json<-: (compose 0x<-nat .nat<-)
  .<-json: (compose .normalize number<-json)
  .rlp<-: (compose rlp<-nat .nat<-)
  .<-rlp: (compose .normalize nat<-rlp)
  .ethabi-name: (format "int~d" .length-in-bits)
  .ethabi-display-type: (cut display .ethabi-name <>)
  .ethabi-head-length: 32
  .ethabi-padding: (- 32 .length-in-bytes)
  .ethabi-tail-length: (lambda (_) 0)
  .ethabi-encode-into:
  (lambda (x bytes start head get-tail set-tail!)
    (u8vector-sint-set! bytes (+ head .ethabi-padding) x big .length-in-bytes))
  .ethabi-decode-from:
  (lambda (bytes start head get-tail set-tail!)
    (ensure-zeroes bytes head .ethabi-padding)
    (u8vector-sint-ref bytes (+ head .ethabi-padding) big .length-in-bytes)))
(def Int<-length-in-bits (make-hash-table))
(def (IntN .length-in-bits)
  (hash-ensure-ref Int<-length-in-bits .length-in-bits
                   (lambda () (.mix Int. (poo.Int .length-in-bits)))))

(defsyntax (defXIntNs stx)
  (with-syntax ((((UIntX IntX x)...)
                 (for/collect (x (iota 32 8 8))
                   [(datum->syntax (stx-car stx) (symbolify "UInt" x))
                    (datum->syntax (stx-car stx) (symbolify "Int" x))
                    x])))
    #'(begin
        (begin
          (def UIntX (UIntN x))
          (register-simple-eth-type UIntX)
          (def IntX (IntN x))
          (register-simple-eth-type IntX))...)))
(defXIntNs)
(def UInt63 (UIntN 63))
(register-simple-eth-type UInt63)
(register-simple-eth-type Int256 "int")
(register-simple-eth-type UInt256 "uint")


;; Bytes types
(.def (methods.Bytes @ [methods.bytes Type.] .validate .ethabi-name)
  .ethabi-display-type: (cut display .ethabi-name <>)
  .ethabi-head-length: 32
  .sexp<-: (lambda (x) `(bytes<-0x ,(0x<-bytes x)))
  .json<-: 0x<-bytes
  .<-json: (compose .validate bytes<-0x)
  .<-string: bytes<-0x
  .rlp<-: identity
  .<-rlp: .validate)
(.def (BytesN. @ [methods.Bytes poo.BytesN.] n)
  sexp: `(BytesN ,n)
  .ethabi-name: (format "bytes~d" n)
  .ethabi-padding: (- 32 n)
  .ethabi-tail-length: (lambda (_) 0)
  .ethabi-encode-into:
  (lambda (x bytes start head get-tail set-tail!)
    (subu8vector-move! x 0 n bytes head))
  .ethabi-decode-from:
  (lambda (bytes start head get-tail set-tail!)
    (def end (+ head n))
    (ensure-zeroes bytes end .ethabi-padding)
    (subu8vector bytes head end)))

(defsyntax (defBytesNs stx)
  (def (foo n) [(datum->syntax (stx-car stx) (symbolify "Bytes" n)) n])
  (with-syntax* ((((rid ri)...) (map foo (iota 32 1)))
                 (((uid ui)...) (map foo [60 64 256])) ;; Shh id / PubKey / Bloom filter
                 (((id i)...) #'((rid ri)... (uid ui)...)))
    #'(begin
        (defrule (d name n) (define-type (name @ BytesN.) n: n))
        (d id i)...
        (register-simple-eth-type rid)...)))
(defBytesNs)

(.def (BytesL16 @ [methods.Bytes methods.bytes<-marshal])
   sexp: 'BytesL16
   .Length: UInt16
   .ethabi-name: "bytes"
   .element?: (λ (x) (and (u8vector? x) (<= (u8vector-length x) 65535)))
   .validate: (λ (x)
                (unless (u8vector? x) (raise-type-error @ x))
                (unless (<= (u8vector-length x) 65535)
                  (raise-type-error @ x
                    ["length too long: expected <=65535" given: (u8vector-length x)]))
                x)
   .marshal: marshal-sized16-u8vector
   .unmarshal: unmarshal-sized16-u8vector
   .ethabi-tail-length: (lambda (x) (+ 32 (ceiling-align (u8vector-length x) 32)))
   .ethabi-encode-into:
   (lambda (x bytes start head get-tail set-tail!)
     (def tail (get-tail))
     (u8vector-uint-set! bytes head (- tail start) big 32)
     (u8vector-uint-set! bytes tail (u8vector-length x) big 32)
     (subu8vector-move! x 0 (u8vector-length x) bytes (+ tail 32))
     (set-tail! (+ tail 32 (ceiling-align (u8vector-length x) 32))))
   .ethabi-decode-from:
   (lambda (bytes start head get-tail set-tail!)
     (def tail (+ start (u8vector-uint-ref bytes head big 32)))
     (assert! (= tail (get-tail)))
     (def data-start (+ tail 32))
     (set-tail! data-start)
     (def len (u8vector-uint-ref bytes tail big 32))
     (set-tail! (+ data-start (ceiling-align len 32)))
     (subu8vector bytes data-start (+ data-start len))))
(define-type Bytes BytesL16)
(register-simple-eth-type Bytes)

(define-type (String @ poo.String)
   .element?: (λ (x) (and (string? x)
                          (or (< (string-length x) 16384)
                              (and (not (< 65535 (string-length x)))
                                   (<= (u8vector-length (string->bytes x)) 65535)))))
   .ethabi-name: "string"
   .ethabi-display-type: (cut display .ethabi-name <>)
   .ethabi-head-length: 32
   .Bytes: Bytes
   .ethabi-tail-length: (rcompose string->bytes (.@ .Bytes .ethabi-tail-length))
   .ethabi-encode-into:
   (lambda (x bytes start head get-tail set-tail!)
     (.call .Bytes .ethabi-encode-into (string->bytes x) bytes start head get-tail set-tail!))
   .ethabi-decode-from:
   (lambda (bytes start head get-tail set-tail!)
     (bytes->string (.call .Bytes .ethabi-decode-from bytes start head get-tail set-tail!))))
(register-simple-eth-type String)

;; TODO: have a function that only interns the string to a symbol if already found?
(define-type (Symbol @ poo.Symbol)
  .ethabi-name: "string"
  .ethabi-display-type: (cut display .ethabi-name <>)
  .ethabi-head-length: 32
  .String: String
  .<-string: maybe-intern-symbol
  .ethabi-tail-length: (rcompose symbol->string (.@ .String .ethabi-tail-length))
  .ethabi-encode-into:
  (lambda (x bytes start head get-tail set-tail!)
    (.call .String .ethabi-encode-into (symbol->string x) bytes start head get-tail set-tail!))
  .ethabi-decode-from:
  (lambda (bytes start head get-tail set-tail!)
    (maybe-intern-symbol (.call .String .ethabi-decode-from bytes start head get-tail set-tail!))))

(define-type (Bool @ poo.Bool)
  .ethabi-name: "bool"
  .ethabi-display-type: (cut display .ethabi-name <>)
  .ethabi-head-length: 32
  .ethabi-padding: 31
  .ethabi-tail-length: (lambda (_) 0)
  .ethabi-encode-into:
  (lambda (x bytes start head get-tail set-tail!)
    (u8vector-set! bytes (+ head 31) (if x 1 0)))
  .ethabi-decode-from:
  (lambda (bytes start head get-tail set-tail!)
    (ensure-zeroes bytes head 31)
    (case (u8vector-ref bytes (+ head 31))
      ((1) #t) ((0) #f) (else (error "Invalid bool")))))
(register-simple-eth-type Bool)

;; Records
(def (Record . plist)
  {(:: @ [(apply poo.Record plist)] .tuple-list<- .<-tuple-list effective-slots)
   types: (with-list-builder (c) (.for-each! effective-slots (lambda (_ slot) (c (.@ slot type)))))
   .<-rlp: (lambda (r) (.<-tuple-list (map <-rlp types r)))
   .rlp<-: (lambda (x) (map rlp<- types (.tuple-list<- x)))
   .ethabi-display-type: (cut ethabi-display-types types <>)
   .ethabi-head-length: (ethabi-head-length types)
   .ethabi-tail-length: (lambda (x) (ethabi-tail-length types (.tuple-list<- x)))
   .ethabi-encode-into:
   (lambda (x bytes start head get-tail set-tail!)
     (ethabi-encode-into types (.tuple-list<- x) bytes start head get-tail set-tail!))
   .ethabi-decode-from:
   (lambda (bytes start head get-tail set-tail!)
     (.<-tuple-list (ethabi-decode-from types bytes start head get-tail set-tail!)))})

(.def (Tuple. @ poo.Tuple. type-list)
  .<-rlp: (lambda (r) (list->vector (map <-rlp type-list r)))
  .rlp<-: (lambda (x) (map rlp<- type-list (vector->list x)))
  .ethabi-display-type: (cut ethabi-display-types type-list <>)
  .ethabi-head-length: (ethabi-head-length type-list)
  .ethabi-tail-length: (lambda (x) (ethabi-tail-length type-list (vector->list x)))
  .ethabi-encode-into:
  (lambda (x bytes start head get-tail set-tail!)
    (ethabi-encode-into type-list (vector->list x) bytes start head get-tail set-tail!))
  .ethabi-decode-from:
  (lambda (bytes start head get-tail set-tail!)
    (list->vector (ethabi-decode-from type-list bytes start head get-tail set-tail!))))
(def (Tuple . types_) ;; type of tuples, heterogeneous arrays of given length and type
  (def types (list->vector (map (cut validate Type <>) types_)))
  {(:: @ Tuple.) (types)})

(.def (Enum. @ [poo.Enum.] vals .nat<- .<-nat .length-in-bytes)
  .ethabi-name: (format "uint~d" (* 8 .length-in-bytes))
  .ethabi-display-type: (cut display .ethabi-name <>)
  .ethabi-head-length: 32
  .ethabi-padding: (- 32 .length-in-bytes)
  .ethabi-tail-length: (lambda (_) 0)
  .ethabi-encode-into:
  (lambda (x bytes start head get-tail set-tail!)
    (u8vector-uint-set! bytes (+ head .ethabi-padding) (.nat<- x) big .length-in-bytes))
  .ethabi-decode-from:
  (lambda (bytes start head get-tail set-tail!)
    (ensure-zeroes bytes head .ethabi-padding)
    (.<-nat (u8vector-uint-ref bytes (+ head .ethabi-padding) big .length-in-bytes))))
(defrule (Enum values ...) {(:: @ Enum.) vals: '(values ...)})

(.def (FixedVector. @ [methods.bytes<-marshal Type.] type size)
  sexp: `(Vector ,(.@ type sexp) ,(.@ type size))
  .element?: (let (e? (.@ type .element?))
               (lambda (x) (and (vector? x) (= (vector-length x) size) (vector-every e? x))))
  .ethabi-display-type: (lambda (port) (.call type .ethabi-display-type port)
                           (fprintf port "[~d]" size))
  .ethabi-element-head-length: (.@ type .ethabi-head-length)
  .ethabi-head-length: (* .ethabi-element-head-length size)
  .ethabi-element-tail-length: (.@ type .ethabi-tail-length)
  .json<-: (lambda (v) (vector-map (lambda (_ x) (json<- type x)) v))
  .<-json: (lambda (j) (vector-unfold (lambda (_ l) (values (json<- type (car l)) (cdr l))) (length j) j))
  .marshal: (let (m (.@ type .marshal))
              (lambda (v port) (vector-for-each (lambda (_ x) (m x port)) v)))
  .unmarshal: (let (u (.@ type .unmarshal))
                (lambda (port) (vector-unfold (lambda (_) (u port)) size)))
  .ethabi-tail-length: (lambda (x) (vector-fold (lambda (_ acc v) (+ acc (.ethabi-element-tail-length v))) 0 x))
  .ethabi-encode-into:
  (lambda (x bytes start head get-tail set-tail!)
    (vector-for-each (lambda (i v)
                       (.call type .ethabi-encode-into
                              v bytes start (+ head (* i .ethabi-element-head-length))
                              get-tail set-tail!)) x))
  .ethabi-decode-from:
  (lambda (bytes start head get-tail set-tail!)
    (vector-unfold (lambda (i)
                     (.call type .ethabi-decode-from
                            bytes start (+ head (* i .ethabi-element-head-length))
                            get-tail set-tail!)) size)))
(.def (DynamicVector. @ [methods.bytes<-marshal Type.] type)
  sexp: `(Vector ,(.@ type sexp))
  .element?: (let (e? (.@ type .element?))
               (lambda (x) (and (vector? x) (vector-every e? x))))
  .ethabi-display-type: (lambda (port) (.call type .ethabi-display-type port) (display "[]" port))
  .ethabi-element-head-length: (.@ type .ethabi-head-length)
  .ethabi-head-length: 32
  .ethabi-element-tail-length: (.@ type .ethabi-tail-length)
  .json<-: (lambda (v) (vector-map (lambda (_ x) (json<- type x)) v))
  .<-json: (lambda (j) (vector-unfold (lambda (_ l) (values (json<- type (car l)) (cdr l))) (length j) j))
  .marshal: (let (m (.@ type .marshal))
              (lambda (v port) (marshal-uint16 (vector-length v) port)
                 (vector-for-each (lambda (_ x) (m x port)) v)))
  .unmarshal: (let (u (.@ type .unmarshal))
                (lambda (port)
                  (def size (unmarshal-uint16 port))
                  (vector-unfold (lambda (_) (u port)) size)))
  .ethabi-tail-length: (lambda (x) (vector-fold (lambda (_ acc v) (+ acc (.ethabi-element-tail-length v)))
                                           (+ 32 (* .ethabi-element-head-length (vector-length x))) x))
  .ethabi-encode-into:
  (lambda (x bytes start head get-tail set-tail!)
    (def tail (get-tail))
    (def new-start (+ tail 32))
    (def new-tail (+ new-start (* .ethabi-element-head-length (vector-length x))))
    (set-tail! new-tail)
    (u8vector-uint-set! bytes head (- tail start) big 32)
    (u8vector-uint-set! bytes tail (vector-length x) big 32)
    (vector-for-each (lambda (i v)
                       (.call type .ethabi-encode-into
                              v bytes new-start (+ new-start (* i .ethabi-element-head-length))
                              get-tail set-tail!)) x))
  .ethabi-decode-from:
  (lambda (bytes start head get-tail set-tail!)
    (def tail (get-tail))
    (assert! (= tail (+ start (u8vector-uint-ref bytes head big 32))))
    (def new-start (+ tail 32))
    (set-tail! new-start)
    (def size (u8vector-uint-ref bytes tail big 32))
    (set-tail! (+ new-start (* .ethabi-element-head-length size)))
    (vector-unfold (lambda (i)
                     (.call type .ethabi-decode-from
                            bytes new-start (+ new-start (* i .ethabi-element-head-length))
                            get-tail set-tail!))
                   size)))
(def (Vector type (size #f))
  (if size
    {(:: @ FixedVector.) (type) (size)}
    {(:: @ DynamicVector.) (type)}))
