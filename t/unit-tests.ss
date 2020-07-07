(import
  :gerbil/gambit/ports
  :std/format :std/iter :std/misc/process
  :clan/utils/exit :clan/utils/ports :clan/utils/version
  :clan/version :crypto/version :ethereum/version
  :clan/t/test-support
  :ethereum/t/path-config)

(set-current-ports-encoding-standard-unix!)

(def (gerbil.pkg)
  (with-catch false (lambda () (call-with-input-file (path-expand "gerbil.pkg" gerbil-ethereum-src) read))))

(def (git-origin-repo)
  (or (pgetq repo: (gerbil.pkg)) "origin"))

(def (git-origin-branch)
  (or (pgetq branch: (gerbil.pkg)) "master"))

(def (git-merge-base . commitishs)
  (run-process ["git" "merge-base" . commitishs] coprocess: read-line))

(def (main . args)
  (eval-print-exit
   (silent-exit
    (match args
      ([] (run-tests "."))
      (["meta"] (println "meta all test integration show-version check_git_up_to_date"))
      (["all"] (run-tests "." test-files: (find-test-files ".")))
      (["integration"] (run-tests "." test-files: (find-test-files "." "-integrationtest.ss$")))
      (["test" . files] (run-tests "." test-files: files))
      (["show-version"]
       (show-version complete: #t))
      (["check_git_up_to_date"]
       (def branch (git-origin-branch))
       (run-process ["git" "fetch" "--depth" "1" (git-origin-repo) branch])
       (def up-to-date? (equal? (git-merge-base "FETCH_HEAD" "FETCH_HEAD")
                                (with-catch false (cut git-merge-base "HEAD" "FETCH_HEAD"))))
       (printf "Checkout~a up-to-date with branch ~a\n" (if up-to-date? "" " not") branch)
       up-to-date?)))))
