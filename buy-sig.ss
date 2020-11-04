;;-*- Gerbil -*-
;; Right after contract projection

(export #t)
(import
  :std/misc/number :std/sugar
  :clan/base :clan/io
  :clan/poo/poo :clan/poo/brace :clan/poo/io (only-in :clan/poo/mop Any Type Fun)
  (only-in :clan/poo/type OrFalse)
  :clan/persist/content-addressing :clan/syntax
  ./assembly ./types ./ethereum ./signing ./known-addresses ./network-config
  ./json-rpc ./transaction ./tx-tracker
  ./abi ./contract-config ./contract-runtime ./assets)

;;TODO:
;; Start by a more stupid interpreter???
;; Wrap each transaction in a suitable step?
;; make it all persistent!
;; context threading is pure, so forks can be handled gracefully.
;; multiple messages can be batched, and aborted together.
;; I know some prior art in the Scheme, Smalltalk and CL communities on serializing continuations,
;; though I'm not sure how they handled transactionality.

;; define-interaction participants: state: frames: behavior
(defsyntax-stx (define-interaction name-formals
                 participants: participants
                 state: state
                 frames: frames
                 behavior: behavior)
  #'(def name-formals
      {participants: participants
       state: state
       frames: frames
       behavior: behavior}))

;; def/persist defines a persistent function with persistent frames
(defrule (def/persist (foo args ...) body ...)
  (def (foo args ...) body ...))

;; receive-message : PersistentContinuation <- \
;;   context:CTX \
;;   reader:(Fun PersistentContinuation AssetDiffs <- Address InPort) \
;;   deadline-block:Block \
;;   on-timeout:(Fun PersistentContinuation <- Exception)
(def (receive-message context: ctx reader: reader deadline-block: deadline-block on-timeout: on-timeout)
  (void))

;; contract-handshake : <-
;;   content:CTX \
;;   to:Address \
;;   writer:(Fun <- OutPort) \
;;   continuation:PersistentContinuation
(def (contract-handshake context: ctx to: to writer: writer continuation: continuation)
  (void))

;; register-context-contract : <- content:CTX contract-config:ContractConfig
(def (register-context-contract ctx contract-config)
  (void))

;; Publish a message to the consensus
;; publish-message : <-
;;   content:CTX \
;;   to:Address \
;;   writer:(Fun <- OutPort) \
;;   continuation:PersistentContinuation
(def (publish-message context: ctx to: to writer: writer continuation: continuation)
  (void))

(defrule (persistent-continuation context label ((var type) ...))
  (.o context: context ;; TODO: extract that at compile-time?!
      label: label
      vars: (list var ...) types: (list type ...)))
;;      fun: (lambda (var ...) fun)))

;; An activation frame is the combination of a code pointer and frame data.
;; A continuation / activation frame / etc.
;; The contract can be data-independent if it's a library contract,
;; e.g. as used with DELEGATECALL (accessing of modifying state) or STATICCALL (just verifying transition)
(define-type FrameShape
  (Record
   ;; Should we have a contract object pointing both to Glow source (optional, could be Solidity FFI)
   ;; AND an address (AND a txstatus to know whether it was confirmed on the blockchain yet).
   contract: [(OrFalse Address) optional: #t default: #f] ;; address of the corresponding contract, if data-independent
   label: [Symbol]
   role: [(OrFalse Symbol)]
   pc: [UInt16 optional: #t default: 0] ;; code location within the contract, if data-independent
   framedata: Type ;; type of the frame's data, to unserialize after the symbol information.
   fun: [(Fun <- Any)] ;; Reactivate frame from framedata, in the dynamic context of current thread
   ))

(defstruct FrameShape (outer-scope label types) transparent: #t)

(define-type Continuation
  (Record
   code-block: [CodeBlock]
   frame-parameters: [Any]
   active-participant: [Address]
   deadline: [UInt32])) ;; deadline in blocks

;; Map label to code block, etc.
;; : (Table CodeBlock <- Symbol))
(def CodeBlockTable
  (make-hash-table))

;; Should the context be dynamically given as a parameter?
(def PAC
  Any)


;; Invoke a continuation. Shouldn't it also have a (dynamic?) thread context?
;; Indeed, we atomically deactivate a previous frame as we activate a new one,
;; and we have a list/set of active threads to reactivate.
(def (continue pk . vals) [pk vals])

;; TODO: modify post-transaction in tx-tracker so that it includes persistent continuation handling
;; as part of its own regular flow.
(def (post-transaction% pre-tx pk) (post-transaction pre-tx) (continue pk))


(def (compile-frame frame-label frame-bepp frame-cpitable2)
  (def frame-param-offsets (compute-frame-param-offsets frame-label frame-cpitable2))
  (&begin*
   (map (cut compile-frame-statement frame-param-offsets <>) (instructions<-frame-bepp frame-bepp))))


(def (&payForSignature--cp0)
  (define-frame-params &payForSignature--cp0
    (Buyer Address)
    (Seller Address)
    (digest0 Digest)
    (price Ether))
  (define-frame-locals &payForSignature--cp0 ;; NB: side-effects the brk-start@
    (signature Signature)
    #|(tmp Bool)|#)
  (&begin
   [&jumpdest 'payForSignature--cp0]
   (&check-participant-or-timeout! must-act: Seller or-end-in-favor-of: Buyer)
   (&begin signature@ &read-published-data-to-mem)
   (&begin Seller digest0 signature@ &isValidSignature #|tmp-set! tmp|# &require!)
   (&begin Seller price &withdraw!)
   &end-contract!))

(defvalues (payForSignature--contract-runtime payForSignature--labels)
  (parameterize ((brk-start (box params-start@)))
    (assemble
     (&begin
      &simple-contract-prelude
      &define-simple-logging
      (&define-check-participant-or-timeout)
      (&define-end-contract)
      (&payForSignature--cp0)
      [&label 'brk-start@ (unbox (brk-start))]))))

(def payForSignature--cp0
  (hash-get payForSignature--labels 'payForSignature--cp0))

;; Or should the execution context and the continuation be implicit as a parameter?
(def/persist (payForSignature/Buyer ctx Buyer Seller digest0 price pk)
  ;; TODO: properly persist all this stuff!
  ;; TODO: wrap in proper transactions?
  ;; put the continuation support into the CTX object?
  begin0: ;; cp:
  (nest
   (begin
    (def timeoutInBlocks
      (.@ (current-ethereum-network) timeoutInBlocks))
    ;; TODO: add some off-chain negotiation for the initial-block?
    (def initial-block ;; grant ourselves some time to post, then
      (+ (eth_blockNumber) timeoutInBlocks))
    (def initial-state
      (digest-product
       (payForSignature--cp0 UInt16)
       (initial-block Block)
       (Buyer Address)
       (Seller Address)
       (digest0 Digest)
       (price Ether)))
    (def contract-bytes
      (stateful-contract-init initial-state payForSignature--contract-runtime))
    (def pretx
      (create-contract Buyer contract-bytes)))
   ;; THOU SHALT ALWAYS PERSIST THY CONTINUATION BEFORE THOU EMITST ANY MESSAGE,
   ;; thus post-transaction takes a persistent continuation as a parameter.
   (post-transaction% pretx)
   (persistent-continuation
    ;; symbol and number for persistence: (first one through syntax-parameter,
    ;; second one after hash-comparison for transitive/expanded source code ?)
    'cp0
    ;; live variables and types associated to it:
    ((ctx PAC) ;; Persistent Activity Context
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
              (assert! (equal? from Seller))
              (def signature (unmarshal Signature port))
              (assert! (message-signature-valid? Seller signature digest0))
              (assert! (= value 0))
              (continue pk {signature}))
    deadline-block: (+ initial-block timeout-in-blocks)
    on-timeout: (lambda (e)
                  (publish-message context: ctx data: #u8())
                  (raise e)))))


#|
   ;;; (pk Continuation)))) ;; contract on which to suicide? or contract to call with parameter?
  ;; Note that it implicit binds the above variables in the body as lazy macro accessors.
  ;; ALSO, how do we deal well with timeouts, here?
  begin0--0:
  (nest
   (lambda (receipt)
    (def contract-config
      (contract-config<-creation-receipt receipt))
    (verify-contract-config contract-config pretx))
   (contract-handshake
    context: ctx to: Seller
    writer: (cut marshal-product <>
                 (initial-block Block)
                 (contract-config ContractConfig))
    ;; NB: there must be some dynamic timeout information in the ctx already!
    k:)
   (persistent-continuation
    ;; symbol for persistence:
    'payForSignature/Buyer 1
    ((ctx PAC) ;; Persistent Activity Context, now extended with ContractConfig
     (initial-block Block)
     (Buyer Address)
     (Seller Address)
     (digest0 Digest)
     (price Ether)
     (pk Continuation))) ;; contract on which to suicide? or contract to call with parameter?
   (lambda ()
     (register-context-contract ctx contract-config))
   (receive-message
    context: ctx
    reader: (lambda (from port value)
|#


#;
(def (payForSignature/Seller ctx Buyer Seller digest0 price)
  (def timeoutInBlocks
    (.@ (current-ethereum-network) timeoutInBlocks))
  ;; TODO: add some off-chain negotiation for the initial-block?
  (def expected-initial-block ;; grant the Buyer some time to post, then
    (+ (eth_blockNumber) timeoutInBlocks))
  (receive-message
   context: ctx
   deadline-block: expected-initial-block
   on-timeout: raise
   reader: (lambda (from port value)
             (defvalues (initial-block contract-config)
               (unmarshal-product port Block ContractConfig))
             (def initial-state
               (digest-product
                (payForSignature--cp0 UInt16)
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
  frames: (payForSignature--cp0)
  behavior: {
    Buyer: payForSignature/Buyer
    Seller: payForSignature/Seller
 })
