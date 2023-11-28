(export #t)

(import
  :std/misc/list :std/misc/process :std/sugar :std/test
  :clan/base :clan/path-config
  :clan/persist/db
  ../ethereum ../testing)

;; Erase the test database
(for-each!
 [(persistent-path "testdb")
  (log-path "ethereum")]
 (lambda (path)
   (when (file-exists? path)
     (ignore-errors (run-process/batch ["rm" "-rf" path])))))

;; Use the test database
(displayln "Connecting to the test database...")
(ensure-db-connection "testdb")

;; TODO: validate that the testdb indeed corresponds to this test net?
;; - At minimum, check that the last confirmed nonces for croesus, etc.,
;; are not past the nonce from getTransactionCount.
;; - Maybe even check that the blocks mentioned in the transaction Confirmations still exist.

(def 15-db-integrationtest
  (test-suite "integration test for ethereum db"
    (void)))
