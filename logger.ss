(export #t)

(import
  (only-in :clan/logger json-logger))

(def eth-log (json-logger "ethereum"))

;;For debugging: (import :clan/json)(def eth-log (values write-json-ln))
