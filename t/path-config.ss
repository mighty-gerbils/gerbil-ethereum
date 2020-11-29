(export #t)

(import
  :std/sugar :gerbil/gambit/system
  :clan/path :clan/path-config :clan/source)

(def gerbil-ethereum-src
  (try
    (path-parent (path-normalized-directory (this-source-file)))
    (catch (_)
      (source-directory))))
(set! source-directory (lambda () gerbil-ethereum-src))
(set! home-directory (lambda () gerbil-ethereum-src))

(def (in-gerbil-ethereum-src)
  (current-directory gerbil-ethereum-src)
  ((eval 'add-load-path) gerbil-ethereum-src))
