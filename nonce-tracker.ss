(export #t)

(import
  :gerbil/gambit
  :std/error :std/misc/completion :std/net/json-rpc :std/text/hex
  :clan/base :clan/concurrency :clan/failure :clan/option :clan/maybe
  :clan/poo/object :clan/poo/brace :clan/poo/io
  :clan/persist/db :clan/persist/persist
  ./types ./ethereum ./json-rpc)

(define-type NonceOperation
  (Enum Peek Next Reset))

(.def (NonceTracker @ [(Fun (Maybe Quantity) <- NonceOperation) DebugPersistentActivity]
       <-key)
   sexp: 'NonceTracker
   .element?: procedure?
   Key: Address
   key-prefix: (string->bytes "ETNT")
   State: (Maybe Quantity)
   ;; Initial state: unknown, to be resynchronized
   make-default-state: void
   .restore:
   (lambda (address save! nonce _tx)
     (def (reset)
       (set! nonce
         (retry
          retry-window: 0.01
          max-window: 5.0
          max-retries: +inf.0
          (cut eth_getTransactionCount address 'latest))))
     (def (continue result n)
       (with-committed-tx (tx) (set! nonce n) (save! nonce tx))
       result)
     (def (next n)
       (continue n (1+ n)))
     (sequentialize
      ['nonce-tracker address]
      (lambda (op)
        (match op
          ('Reset (continue (void) (void)))
          ('Peek (when (void? nonce) (reset)) nonce)
          ('Next (when (void? nonce) (reset)) (next nonce))))))
   reset: (lambda (x) ((<-key x) 'Reset))
   peek: (lambda (x) ((<-key x) 'Peek))
   next: (lambda (x) ((<-key x) 'Next)))

(def (reset-nonce address) (.call NonceTracker reset address))
(def (peek-nonce address) (.call NonceTracker peek address))
(def (next-nonce address) (.call NonceTracker next address))
