;;-*- Gerbil -*-
;; Right after contract projection

(export #t)
(import
  :std/misc/number :std/sugar
  :clan/io
  :clan/poo/poo :clan/poo/brace :clan/poo/io
  :clan/persist/content-addressing
  ./assembly ./types ./ethereum ./signing ./known-addresses ./network-config
  ./json-rpc ./transaction ./tx-tracker
  ./abi ./contract-config ./contract-runtime ./assets)

;;TODO:
;; Start by a more stupid interpreter???
;; Wrap each transaction in a suitable step?
;; make it all persistent!
;; context threading is pure, so forks can be handled gracefully.
;; multiple messages can be batched, and aborted together.
;; define-interaction participants: state: frames: behavior
(defrule (define-interaction foo ...) (void))

;; receive-message : PersistentContinuation <- \
;;   context:CTX \
;;   reader:(Fun PersistentContinuation AssetDiffs <- Address InPort) \
;;   deadline-block:Block \
;;   on-timeout:(Fun PersistentContinuation <- Exception)
(def (receive-message . _) (void))

;; send-message : <-
;;   content:CTX \
;;   to:Address \
;;   writer:(Fun <- OutPort) \
;;   continuation:PersistentContinuation
;;
(def (send-message . _) (void))
(def (register-context-contract ctx contract-config) (void))

(def (&payForSignature--pc0)
  (define-frame-params &payForSignature--pc0
    (Buyer Address)
    (Seller Address)
    (digest0 Digest)
    (price Ether))
  (define-frame-locals &payForSignature--pc0 ;; NB: side-effects the brk-start@
    (signature Signature)
    #|(tmp Bool)|#)
  (&begin
   [&jumpdest 'payForSignature--pc0]
   (&check-participant-or-timeout! must-act: Seller@ or-end-in-favor-of: Buyer@)
   signature@ &read-published-data-to-mem
   Seller digest0 signature@ &isValidSignature #|tmp-set! tmp|# &require!
   Seller price &withdraw!
   &end-contract!))

(defvalues (payForSignature--contract-runtime payForSignature--labels)
  (parameterize ((brk-start (box params-start@)))
    (assemble
     (&begin
      (&simple-contract-prelude)
      &define-simple-logging
      (&define-check-participant-or-timeout)
      (&define-end-contract)
      (&payForSignature--pc0)
      [&label 'brk-start@ (unbox (brk-start))]))))

(def payForSignature--pc0
  (hash-get payForSignature--labels 'payForSignature--pc0))

(def (payForSignature/Buyer ctx Buyer Seller digest0 price)
  ;; TODO: properly persist all this stuff!
  ;; TODO: wrap in proper transactions?
  (def timeoutInBlocks (ethereum-timeout-in-blocks))
  ;; TODO: add some off-chain negotiation for the initial-block?
  (def initial-block ;; grant ourselves some time to post, then
    (+ (eth_blockNumber) timeoutInBlocks))
  (def initial-state
    (digest-product
     (payForSignature--pc0 UInt16)
     (initial-block Block)
     (Buyer Address)
     (Seller Address)
     (digest0 Digest)
     (price Ether)))
  (def contract-bytes
    (stateful-contract-init initial-state payForSignature--contract-runtime))

  (def pretx
    (create-contract Buyer contract-bytes value: price))
  (def receipt
    (post-transaction pretx))
  (def contract-config
    (contract-config<-creation-receipt receipt))
  (verify-contract-config contract-config pretx)
  ;; THOU SHALT ALWAYS PERSIST THY CONTINUATION BEFORE THOU EMITST ANY MESSAGE
  ;; TODO: send-message shall persist its continuation!!!
  (send-message context: ctx to: Seller
                writer: (lambda (port) (marshal-product
                                   (initial-block Block)
                                   (contract-config ContractConfig))))
  (register-context-contract ctx contract-config)
  (receive-message
   context: ctx
   reader: (lambda (from port value)
              (assert! (equal? from Seller))
              (def signature (unmarshal Signature port))
              (assert! (message-signature-valid? Seller signature digest0))
              (assert! (= value 0))
              {signature})
   deadline-block: (+ initial-block (ethereum-timeout-in-blocks))
   on-timeout: (lambda (e)
                 (send-message context: ctx data: #u8())
                 (raise e))))

(def (payForSignature/Seller ctx Buyer Seller digest0 price)
  (def timeoutInBlocks (ethereum-timeout-in-blocks))
  ;; TODO: add some off-chain negotiation for the initial-block?
  ;; grant the Buyer some time to post, then
  (def expected-initial-block (+ (eth_blockNumber) timeoutInBlocks))
  (receive-message
   context: ctx
   deadline-block: expected-initial-block
   on-timeout: raise
   reader: (lambda (from port value)
             (defvalues (initial-block contract-config)
               (unmarshal-product port Block ContractConfig))
             (def initial-state
               (digest-product
                (payForSignature--pc0 UInt16)
                (initial-block Block)
                (Buyer Address)
                (Seller Address)
                (digest0 Digest)
                (price Ether)))
             (def contract-bytes
               (stateful-contract-init initial-state payForSignature--contract-runtime))
             (def pretx
               (create-contract Buyer contract-bytes))
             (verify-contract-config contract-config pretx)
             (register-context-contract ctx contract-config)
             (def signature
               (make-message-signature (secret-key<-address Seller) digest0))
             (send-message context: ctx to: Seller
                           writer: (lambda (port) (marshal Signature signature port)))
             ;;TODO: assert that indeed we got the money out of it!
             (void))))

;; Entry point for the interaction -- from here on user must be Buyer
(define-interaction payForSignature
  participants: (Buyer Seller)
  state: ((digest0 Digest)
          (price Ether))
  frames: (payForSignature--pc0)
  behavior: {
    Buyer: payForSignature/Buyer
    Seller: payForSignature/Seller
 })

#|

(def/interaction (buy-sig/seller ...)
  (code-block-1)
  (code-block-2)
  (code-block-3))

(def (buy-sig/seller ...)
  (with-interaction (...)
    (code-block-1)
    (call-subroutine ...)
    (set-current-label 'cp0)
    (set-participant Buyer)
    (code-block-2)
    (code-block-3)))

(def (set-participant part)
  (if (equal? part (current-part))
    (begin (publish UInt8 0)
           (invoke-continuation continuation))
    (beign (publish UInt8 1)
           (call/cc (lambda (k) (flush-message ctx k))))))

(def (code-block-1 ctx)
  (with-code-block (ctx)
    (publish Signature sig)
    ...))

(defrule (with-code-block (ctx) body ...) (call-with-code-block ctx (lambda () body ...)))

(def (call-with-code-block ctx thunk)
  (defvalues (continue? continuation) (thunk))
  (if continue?
    (begin (publish UInt8 0)
           (invoke-continuation continuation))
    (beign (publish UInt8 1)
           (send-to-contract ctx (flush-message-buffer ctx) continuation))))

(def (call-with-interaction-transaction ctx thunk)
  (set! (ctx-message-buffer ctx) (open-output-u8vector))
  ...
|#
