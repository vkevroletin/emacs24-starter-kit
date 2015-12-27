;;; -*- lexical-binding: t -
;; run with emacs -batch -l ert -l project-cache-test.el -f ert-run-tests-batch-and-exit

(package-initialize)
(require 'f)
(add-to-list 'load-path (f-dirname (f-this-file)))

(require 'project-cache)

(defun project-cache-test-init ()
  (let ((tmp-file (concat temporary-file-directory "/project-cache-test.cache")))
    (setq project-cache-file tmp-file)
    (project-cache-clear)
    (when (f-exists? tmp-file) (f-delete tmp-file))))

(ert-deftest project-cache-test--multiple-put-get ()
  (project-cache-test-init)

  (let ((values '(("a/b/c" . 10)
                  ("123" . 20)
                  ("/" . 30))))
    (dolist (x values)
      (project-cache-put (car x) (cdr x)))
    (dolist (x values)
      (should (equal (project-cache-get (car x))
                     (cdr x))))))

(ert-deftest project-cache-test--put-get-nil ()
  (project-cache-test-init)

  (project-cache-put '() 10)
  (should (null (project-cache-get '()))))

(ert-deftest project-cache-test--put-get-nil ()
  (project-cache-test-init)

  (project-cache-put '() 10)
  (should (null (project-cache-get '()))))

(ert-deftest project-cache-test--file-write-read ()
  (project-cache-test-init)

  (should (null (project-cache-get "a")))
  (project-cache-put "a" 10)
  (should (equal (project-cache-get "a") 10))
  (project-cache-write-to-file)
  (should (equal (project-cache-get "a") 10))
  (project-cache-clear)
  (should (null (project-cache-get "a")))
  (project-cache-read-from-file)
  (should (equal (project-cache-get "a") 10)))

(provide 'project-cache-test)
