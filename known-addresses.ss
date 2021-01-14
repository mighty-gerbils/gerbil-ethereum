(export #t)

(import
  :std/format :std/sort :std/sugar
  :clan/json
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

(def (generate-keypair passwd)
  (keypair<-secret-key (generate-secret-key-data) passwd))


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
