(export #t)

(import
  :gerbil/gambit/system
  :clan/source :clan/path :clan/path-config)

(def gerbil-ethereum-src (path-parent (path-normalized-directory (this-source-file))))
(set! source-directory (lambda () gerbil-ethereum-src))
(set! home-directory (lambda () gerbil-ethereum-src))

(def (in-gerbil-ethereum-src)
  (current-directory gerbil-ethereum-src)
  ((eval 'add-load-path) gerbil-ethereum-src))
