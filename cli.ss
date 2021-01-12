(export #t)

(import
  :std/sort
  :clan/multicall
  ./network-config
  )

(define-entry-point (list-ethereum-networks)
  "Show a list of available ethereum networks"
  (for-each
    (lambda (name) (displayln name))
    (sort (hash-keys ethereum-networks) string<?)))
