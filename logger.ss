(export #t)

(import :clan/logger)

;;(def eth-log (json-logger "ethereum"))
(import :clan/json)(def eth-log (values write-json-ln))
