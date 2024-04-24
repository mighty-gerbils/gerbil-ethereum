;;;; A presigned CREATE2-wrapper contract with the very same address on all EVM blockchains
;;
;; I pre-signed transactions to create a trivial CREATE2-wrapper contract
;; with nonce 0 from an address I randomly generated and subsequently deleted.
;; Whatever the gasPrice may be, you can fund the creator account and post a
;; transaction with at most 42% lossage in gas costs.
;; You don't have to trust me about it: see presigned.ss for details.
;; NB: I had to regenerate the signatures because at first I had failed to
;; leave enough margin for a gas price increase -- that happened due to EIP-3860
;; (Shanghai upgrade, 2023). I now added a 50% margin.

(export #t)

(import
  (for-syntax (only-in :clan/poo/io <-bytes)
              (only-in :clan/crypto/secp256k1 Bytes65)
              (only-in ./types Vector))
  (only-in :std/source stx-source-content)
  (only-in :std/sugar syntax-call)
  (only-in :clan/base !> defonce)
  (only-in :clan/poo/object .def with-slots)
  (only-in ./logger eth-log)
  (only-in ./hex bytes<-0x)
  (only-in ./ethereum address<-create2 address<-creator-nonce address<-0x)
  (only-in ./abi ethabi-encode)
  (only-in ./json-rpc eth_getCode)
  (only-in ./transaction call-function)
  (only-in ./tx-tracker post-transaction)
  (only-in ./presigned ensure-presigned-contract presign-contract-creation)
  (only-in ./testing croesus)
  (only-in ./simple-apps create2-wrapper-init))

;; I used this function once to create the presigned transactions below
;; (and once before with a precise gas limit that got invalidated)
;; No one needs to use it ever again:
#|
  (def presigned-create2-wrapper (presign-create2-wrapper))
  (display-presigned presigned-create2-wrapper)
  (write-file-u8vector "meta-create2.presig" (bytes<- (Vector Bytes65 512) (.@ presigned-create2-wrapper sigs)))
|#
;; : PresignedTransaction <-
(def (presign-create2-wrapper)
  (presign-contract-creation (create2-wrapper-init)))

;; Presigned transactions for the create2-wrapper contract.
;; : PresignedTransaction
(.def presigned-create2-wrapper
  from: (address<-0x "0x8e7a2D1c4fb39988f8af23dAa0e8F8e06413F70E")
  to: #!void value: 0 nonce: 0 gas: 84594
  data: (bytes<-0x "0x600f5f8160095f39f35f35602036038060205f375f34f500")
  sigs: (syntax-call (lambda (ctx) (<-bytes (Vector Bytes65 512)
                                       (stx-source-content ctx "meta-create2.presig")))))

;; Ensure that the create2-wrapper contract exists on the current blockchain.
;; : Address <- funder: ?Address gasPrice: ?Quantity log: ?(<- Jsonable)
(def (ensure-presigned-create2-wrapper
      funder: (funder croesus) gasPrice: (gasPrice (void)) log: (log eth-log))
  (ensure-presigned-contract presigned-create2-wrapper
                             funder: funder gasPrice: gasPrice log: log))

;; : Address
(defonce (create2-wrapper)
  (with-slots (from nonce) presigned-create2-wrapper
    (address<-creator-nonce from nonce)))

;; Deploys a contract to private test net
;; : Address <- Bytes32 Bytes (List Type) (List Any) funder: ?Address value: ?Quantity
(def (abi-create2 funder: (funder croesus)
                  salt contract-bytes (types []) (arguments []) value: (value 0))
  (ensure-presigned-create2-wrapper funder: funder)
  (def init-code (ethabi-encode types arguments contract-bytes))
  (def address (address<-create2 (create2-wrapper) salt init-code))
  (when (equal? #u8() (eth_getCode address))
    (!> (u8vector-append salt init-code)
        (cut call-function funder (create2-wrapper) <> value: value)
        post-transaction))
  address)
