;;; -*- lexical-binding: t -
;; run with emacs -batch -l ert -l compile-per-project-test.el -f ert-run-tests-batch-and-exit

(package-initialize)
(require 'f)
(add-to-list 'load-path (f-join (f-dirname (f-this-file)) "../src"))

(require 'compile-per-project)

;; (ert-deftest compile-per-project-test- ())

(provide 'compile-per-project-test)
