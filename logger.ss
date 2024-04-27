(export #t)

(import
  (only-in :clan/logger json-logger))

(defmutable eth-log (json-logger "ethereum")) ;; or should it be a parameter?

;;For debugging: (import :clan/json)(def eth-log (values write-json-ln))
