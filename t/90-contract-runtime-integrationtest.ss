(export #t)

(import
  :std/srfi/1 :std/test  :std/format :std/sugar
  :clan/debug :clan/number
  :clan/poo/poo :clan/poo/io :clan/poo/debug
  ../contract-runtime  ../assembly  ../json-rpc ../types
  ./signing-test ./30-transaction-integrationtest)

;; TODO: support boxed types
;; Directive <- t:Type t
(def (&evm-inline-input t v)
  (bytes<- t v))

;; Directive <- (Listof DependentPair)
(def (&evm-inline-inputs inputs)
  (&begin*
   (map (match <> ([t . v] (&evm-inline-input t v))) inputs)))

;; Directive <- Type
(def (&evm-inline-output t copied: (copied #f))
  (def len (param-length t))
  (DDT &evm-inline-output: Type t poo.Nat len)
  (if copied
    (&begin 0 len ADD)
    (&begin ;; bufptr[incremented] <-- bufptr val:t
    DUP1 SWAP2 DUP2 (&mstore/overwrite-after len) len ADD)))
;; Try and load from memory here

;; TODO: support boxed types as inputs (that may offset the start of the output?) and outputs
(def (&evm-inline-outputs outputs copied: (copied #f) start: (start 0))
  (if (null? outputs) void
  (&begin
   0 ;; start output buffer
   (&begin* (map (match <> ([t . _] (&evm-inline-output t copied: copied))) outputs))
   start)))

(def (&evm-test-code inputs action outputs onchain: (onchain #f) copied: (copied #f) start: (start 0) )
  (&begin
   (&evm-inline-inputs inputs)
   action
   (&evm-inline-outputs outputs copied: copied start: start)
   (if (null? outputs) void RETURN) ;;(if onchain (&begin LOG0 STOP) RETURN)
   [&jumpdest 'abort-contract-call] 0 DUP1 REVERT))
;; How come affects jump to abort-contract-call?

(def (evm-test inputs action outputs onchain: (onchain #f) copied: (copied #f) start: (start 0) boolCheck: (boolCheck #f))
  (def code-bytes
    (assemble/bytes
     (&evm-test-code inputs action outputs onchain: onchain copied: copied start: start)))
  ;;(DDT evm-test-1: (.@ Bytes .json<-) code-bytes)
  (def result-bytes
    (if onchain
      (evm-eval/onchain croesus code-bytes)
      (evm-eval/offchain croesus code-bytes)))
  ;;(DDT evm-test-2: (.@ Bytes .json<-) result-bytes)
  (def result-list
    (call-with-input-u8vector
     result-bytes
     (lambda (port)
       (map-in-order (lambda (output-tv) (unmarshal (car output-tv) port)) outputs))))
  (def expected-result-list
    (map cdr outputs))
  (if boolCheck
    (check-equal? (> (car result-list) (car expected-result-list)) #t)
    (check-equal? result-list expected-result-list)))

(def (evm-test-failure inputs action onchain: (onchain #f))
  (def code-bytes
    (assemble/bytes
     (&evm-test-code inputs action [] onchain: onchain)))
  (or
    (try
     (if onchain
       (evm-eval/onchain croesus code-bytes)
       (evm-eval/offchain croesus code-bytes))
     #f
     (catch (_) #t))
    (error "Failed to fail" inputs action)))

(def 90-contract-runtime-integrationtest
  (test-suite "integration test for ethereum/contract-runtime"
    (test-case "safe-sub when a operand equals b operand"
      (evm-test [[UInt8 . 8] [UInt8 . 8]] &safe-sub [[UInt8 . 0]]))

    (test-case "safe-sub when b operand less than a operand"
      (evm-test-failure [[UInt16 . 80] [UInt16 . 42]] &safe-sub))

    (test-case "safe-sub when b operand greater than a operand"
      (evm-test [[UInt16 . 42] [UInt16 . 80]] &safe-sub [[UInt16 . 38]]))

    (test-case "safe-add normal case"
      (evm-test [[UInt8 . 8] [UInt8 . 8]] &safe-add [[UInt8 . 16]]))

    (test-case "safe-add (unless (> 2**256 (+ x y)) (abort))"
      (evm-test-failure [[UInt256 (expt 2 255)...] [UInt256 (expt 2 255)...]] &safe-add))

    (test-case "safe-add (unless (>= (- 2**256 1) (+ x y)) (abort))"
      (evm-test-failure [[UInt256 (- (expt 2 255) 1)...] [UInt256 (expt 2 255)...]] &safe-add))

    (test-case "safe-add (unless (>= (- 2**256 1 x) y) (abort))"
      (evm-test-failure [[UInt256 (- (expt 2 250) 1)...] [UInt256 (expt 2 255)...]] &safe-add))

    (test-case "safe-add/n-bits n-bits equals 256"
      (evm-test [[UInt256 . 4000] [UInt256 . 6000]] (&safe-add/n-bits 256) [[UInt256 . 10000]]))

    (test-case "safe-add/n-bits n-bits equals 0"
      (evm-test [[UInt256 (expt 2 25)...] [UInt256 (expt 2 25)...]] (&safe-add/n-bits 0) [[UInt256 . 33554432]]))

    (test-case "safe-add/n-bits n-bits under size"
      (evm-test-failure [[UInt256 (expt 2 25)...] [UInt256 (expt 2 25)...]] (&safe-add/n-bits 10)))

    (test-case "safe-add/n-bits n-bits over size"
      (evm-test [[UInt16 . 6000] [UInt16 . 4000]] (&safe-add/n-bits 200) [[UInt256 . 10000]]))

    (test-case "safe-mul normal case"
      (evm-test [[UInt256 (expt 2 25)...] [UInt256 (expt 2 25)...]] (&safe-mul) [[UInt256 . 1125899906842624]]))
    
    (test-case "safe-mul oveflow case"
      (evm-test-failure [[UInt256 (- (expt 2 250) 1)...] [UInt256 (expt 2 25)...]] (&safe-mul)))

    (test-case "validate-sig-data"
      (evm-test [[UInt256 . #x4e1ce8ea60bc6dfd4068a35462612495850cb645a1c9f475eb969bff21d0b0fb] 
                 [UInt256 . #x414112aaf13f01dd18a3527cb648cdd51b618ae49d4999112c33f86b7b26e973]
                 [UInt256 . #xb]] 
                 &validate-sig-data
                 [[UInt16 . 11]]))

    (test-case "validate-sig-data with wrong v value"
      (evm-test-failure [[UInt256 . #x69f945012f7ea7d3febf11eb1b78e1adc2d1c14c2cf48b25000938cc1860c83e] 
                 [UInt256 . #x9955af11969a2d2a7f860cb00e6a00cfa7c581f5df2dbe8ea16700b33f4b4b9b]
                 [UInt256 . 30]] 
                 &validate-sig-data
                 ))

    (test-case "validate-sig-data with wrong s value"
      (evm-test-failure [[UInt256 . #x8Ff945012f7ea7d3febf11eb1b78e1adc2d1c14c2cf48b25000938cc1860c83e] 
                 [UInt256 . #x9955af11969a2d2a7f860cb00e6a00cfa7c581f5df2dbe8ea16700b33f4b4b9b]
                 [UInt256 . 27]] 
                 &validate-sig-data
                 ))

   (test-case "&unsafe-post-increment-at!"
     (evm-test [[UInt256 . 27] [UInt256 . 0] ] (&begin MSTORE  (&unsafe-post-increment-at! 0 20)) [[UInt256 . 47]] copied: #t))

  (test-case "&unsafe-post-increment-at! spurious value"
     (evm-test-failure [[UInt256  (expt 2 255)...] [UInt256 . 0] ] (&begin MSTORE  (&unsafe-post-increment-at! 0  (expt 2 255))) ))

  (test-case "&memcpy/const-size destination second"
      (evm-test [] 
        (&begin 85 0 MSTORE 95 32 MSTORE 105 64 MSTORE 96 0 (&memcpy/const-size 96)) [[UInt256 . 105]] start: 160))

  
  (test-case "&memcpy/const-size destination first"
      (evm-test [] 
        (&begin 85 0 MSTORE 95 32 MSTORE 105 64 MSTORE 0 96 (&memcpy/const-size 96 dst-first?: #t)) [[UInt256 . 105]] start: 160))

  (test-case "&memcpy/const-size when n is not divisible by 32"
    (evm-test [] 
      (&begin 85 0 MSTORE 95 32 MSTORE 105 64 MSTORE 5 96  (&mstore 2) 0 128 (&memcpy/const-size 98 dst-first?: #t)) [[UInt16 . 5]] copied: #t start: 224))

  (test-case "&memcpy/const-size when n is not divisible by 32 and with overwrite-after? true"
    (evm-test [] 
      (&begin 85 0 MSTORE 95 32 MSTORE 105 64 MSTORE 5 96  (&mstore 2) 0 128 (&memcpy/const-size 98 overwrite-after?: #t dst-first?: #t)) [[UInt16 . 5]] copied: #t  start: 224))

  (test-case "&memcpy/const-size/const-src when n is not divisible by 32 and with overwrite-after? true"
    (evm-test [] 
      (&begin 85 0 MSTORE 95 32 MSTORE 105 64 MSTORE 5 96  (&mstore 2) 128 (&memcpy/const-size/const-src 0 98 overwrite-after?: #t)) [[UInt16 . 5]] copied: #t  start: 96))

  (test-case "&memcpy/const-size/expr-src when &addr is nat"
    (evm-test [] 
      (&begin 85 0 MSTORE 95 32 MSTORE 105 64 MSTORE 5 96  (&mstore 2) 128 (&memcpy/const-size/expr-src 0 98 overwrite-after?: #t)) [[UInt16 . 5]] copied: #t start: 224))

  (test-case "&check-sufficient-deposit  When deposit is GT"
    (evm-test-failure [] (&begin &check-sufficient-deposit)))


  (test-case "&deposit!"
    (evm-test [[UInt256 . 300]] (&begin &deposit! deposit &deposit! deposit) [[UInt256 . 600]]))

  (test-case "&brk-cons when n-bytes is 32"
    (evm-test [[UInt256 . 500]] (&brk-cons 32) [[UInt256 . 500]] copied: #t start: brk@))

  (test-case "&brk-cons when n-bytes is 1"
    (evm-test [[UInt8 . 50]] (&brk-cons 1) [[UInt8 . 50]] copied: #t start: brk@))


  (test-case "&brk-cons when n-bytes is 0"
    (evm-test [[UInt256 . 100] [UInt256 . 5]] (&brk-cons 0) [[UInt256 . 100]]))


  (test-case "&brk-cons when n-bytes is  not 0 , 1 or 30"
    (evm-test [[UInt16 . 5000]] (&brk-cons 2) [[UInt16 . 5000]] copied: #t start: brk@))

  ;; Not sure is this is not hacky and correct
  (test-case "&ecrecover0"
    (evm-test [[UInt256 . #x4f8ae3bd7535248d0bd448298cc2e2071e56992d0774dc340c368ae950852ada] 
                [UInt256 . #x9242685bf161793cc25603c231bc2f568eb630ea16aa137d2664ac8038825608]
                [UInt256 . 28]
                [UInt256 . #x1c8aff950685c2ed4bc3174f3472287b56d9517b9c948127319a09a7a36deac8]] 
                &ecrecover0          
                [[UInt256 . 1]] boolCheck: #t))


(test-case "&read-published-data-to-mem"
    (evm-test [[UInt256 . 5000]] (&begin NUMBER &read-published-data-to-mem) [[UInt256 . 5000]] copied: #t start: calldatapointer@))

;;(test-case "&SELFDESTRUCT"
  ;;  (evm-test [] (&begin bob (&SELFDESTRUCT)) [[UInt256 . 0]]))

(test-case "&start-timer!"
    (evm-test [] (&begin &start-timer! timer-start) [[UInt256 . 1]] boolCheck: #t))

(test-case "&stop-timer!"
    (evm-test [] (&begin &stop-timer! timer-start) [[UInt256 . 1]] boolCheck: #t))

(test-case "&marshal UInt256"
    (evm-test [] (&begin brk DUP1 DUP1  (&marshal UInt256  7)) [[UInt256 . 32]]))

(test-case "&marshal UInt16"
    (evm-test [] (&begin brk DUP1 DUP1  (&marshal UInt16   7)) [[UInt16 . 2]]))

(test-case "&marshal UInt8"
    (evm-test [] (&begin brk DUP1 DUP1  (&marshal UInt8   7)) [[UInt8 . 1]]))

      ))
