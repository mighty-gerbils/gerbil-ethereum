(export #t)

(import
  :gerbil/gambit/bits :gerbil/gambit/bytes :gerbil/gambit/random
  :std/format :std/iter :std/sort :std/srfi/13 :std/sugar :std/text/hex
  :clan/base :clan/json :clan/number
  :clan/crypto/keccak
  :clan/poo/io :clan/poo/brace :clan/poo/poo
  :clan/crypto/secp256k1
  ./hex ./types ./signing)

(defstruct keypair (address public-key secret-key password) equal: #t)

(define-type Keypair
  {(:: @ Type.)
   ;;Reduced: (Record seckey: [SecretKey] password: [Password])
   .element?: keypair?
   .sexp<-: (lambda (kp) `(<-json Keypair ,(.json<- kp))) ;; do NOT export private data
   .json<-: (lambda (kp) (json<- Address (keypair-address kp)))
   .<-json: (lambda (j) (keypair<-address (<-json Address j)))})

(def (keypair-reducible? kp)
  (and (equal? (keypair-address kp)
               (address<-public-key (keypair-public-key kp)))
       (equal? (bytes<- PublicKey (keypair-public-key kp))
               (bytes<- PublicKey (secp256k1-pubkey<-seckey (secp256k1-seckey-data (keypair-secret-key kp)))))))

;; USE WITH CARE: this function exposes information that is meant to remain private.
;; Do NOT use lightly anywhere in production but in the most trusted wallet-management layer.
(def (export-keypair/json kp)
  (hash ("address" (json<- Address (keypair-address kp)))
        ("seckey" (json<- Bytes32 (export-secret-key/bytes (keypair-secret-key kp))))
        ("pubkey" (json<- PublicKey (keypair-public-key kp)))
        ("password" (json<- String (export-password/string (keypair-password kp))))))
(def (import-keypair/json j)
  (assert! (equal? (sort (hash-keys j) string<?) '("address" "password" "pubkey" "seckey")))
  (keypair (<-json Address (hash-get j "address"))
           (<-json SecretKey (hash-get j "seckey"))
           (<-json PublicKey (hash-get j "pubkey"))
           (<-json Password (hash-get j "password"))))
;;Why can't we do that???
;;(defmethod (@@method :pr Keypair)
;;  (λ (self (port (current-output-port)) (options (current-representation-options)))
;;    (write (sexp<- Keypair self) port)))

(def (keypair<-seckey-0x seckey-0x passwd)
  (def seckey-data (validate Bytes32 (bytes<-0x seckey-0x)))
  (keypair<-secret-key seckey-data passwd))

(def (keypair<-secret-key seckey-data passwd)
  (validate Bytes32 seckey-data)
  (validate String passwd)
  (def seckey (secp256k1-seckey seckey-data))
  (def pubkey (secp256k1-pubkey<-seckey seckey-data))
  (def address (address<-public-key pubkey))
  (keypair address pubkey seckey (password passwd)))

(def (nibble-ref bytes i)
  (def b (bytes-ref bytes (arithmetic-shift i -1)))
  (if (even? i) (arithmetic-shift b -4) (bitwise-and b 15)))

(def (scoring<-prefix prefix)
  (def len (string-length prefix))
  (unless (and (<= len 40) (string-every unhex* prefix))
    (error "Invalid keypair prefix" prefix))
  (def p (make-bytes len))
  (for ((i (in-range len))) (bytes-set! p i (unhex (string-ref prefix i))))
  [(lambda (b)
     (let/cc return
       (def l (min len (* 2 (bytes-length b))))
       (for ((i (in-naturals)))
         (unless (and (< i l) (eqv? (bytes-ref p i) (nibble-ref b i)))
           (return i)))))
   len])

(def trivial-scoring [(lambda (_) 0) 0])

(def (generate-keypair scoring: (scoring trivial-scoring) passwd: (passwd ""))
  (nest
    (let/cc return)
    (with ([score-function enough-score] scoring))
    (let ((best-score-so-far -inf.0)
          (seed (random-integer secp256k1-order))))
    (while #t)
    (let* ((seckey-data (bytes<- UInt256 seed))
           (seckey (secp256k1-seckey seckey-data))
           (pubkey (secp256k1-pubkey<-seckey seckey-data))
           (h (keccak256<-bytes (bytes<- PublicKey pubkey)))
           (address-bytes (subu8vector h 12 32))
           (s (score-function address-bytes)))
      (set! seed (modulo (+ seed (nat<-bytes h)) secp256k1-order)))
    (when (<= best-score-so-far s))
    (let (kp (keypair (make-address address-bytes) pubkey seckey (password passwd)))
      (set! best-score-so-far s)
      (write-json-ln (export-keypair/json kp)))
    (when (>= s enough-score))
    (return kp)))

;; TODO: handle collisions, exceptions.
;; TODO: make these tables Scheme parameters?
(def address-by-nickname (make-hash-table))
(def nickname-by-address (make-hash-table))
(def (register-address nickname address)
  (hash-put! nickname-by-address address nickname)
  (hash-put! address-by-nickname nickname address))
(def (nickname<-address address)
  (hash-get nickname-by-address address))
  ;; (or (get-nickname-of-address address) (error "No registered nickname for address" (0x<-address address)))
(def (address<-nickname nickname)
  (hash-get address-by-nickname nickname))
  ;; (or (address<-nickname nickname) (error "No registered nickname" nickname)))
(def (nicknamed-string<-address address)
  (def s (0x<-address address))
  (def n (nickname<-address address))
  (if n (format "~a (~a)" n s) s))
(def (unregister-address nickname)
  (def address (address<-nickname nickname))
  (hash-remove! address-by-nickname nickname)
  (hash-remove! nickname-by-address address))



;; TODO: make this table Scheme parameters?
(def keypair-by-address (make-hash-table))

(def (register-keypair nickname keypair)
  (def address (keypair-address keypair))
  (hash-put! keypair-by-address address keypair)
  (register-address nickname address))
(def (unregister-keypair nickname)
  (def address (address<-nickname nickname))
  (hash-remove! keypair-by-address address)
  (unregister-address nickname))
(def (keypair<-address address)
  (or (hash-get keypair-by-address address) ;; TODO: use hash-ref/default ?
      (error "No secret key configured for address" (0x<-address address))))
;; (or (keypair<-address address) (error "No registered keypair for address" address))
(def (secret-key<-address address)
  (keypair-secret-key (keypair<-address address)))

;; TODO: Add a layer of encryption for these files.
(def (register-file-keypairs file)
  (hash-for-each register-keypair (<-json (Map Keypair <- String) (read-file-json file))))

(def (addresses-with-registered-keypair)
  (hash-keys keypair-by-address))

(def (nicknames-with-registered-keypair)
  (filter identity (map nickname<-address (addresses-with-registered-keypair))))
