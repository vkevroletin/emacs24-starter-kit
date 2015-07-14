;; Simple test runner
;; TODO:
;; + turn simple-test and expect into macro

(defvar simple-tests (list))

(defun simple-test-init ()
  (setq simple-tests (list)))

(defun simple-test (name body)
  (push (cons name body) simple-tests))

(defun expect (name body)
  (condition-case err
      (unless (funcall body)
        (error "result is nil"))
    (error
     (error (format "expectation %s failed: %s" name (nth 1 err))))))

(defun simple-tests-run ()
  (let ((ok-cnt 0)
        (fail-cnt 0))
    (dolist (test (reverse simple-tests))
      (message ">> Execute test %s:" (car test))
      (condition-case err
          (progn
            (funcall (cdr test))
            (setq ok-cnt (+ 1 ok-cnt))
            (message "   ok"))
        (error
         (setq fail-cnt (+ 1 fail-cnt))
         (message (format "   test %s failed:\n   %s" (car test) (nth 1 err))))))
    (message "Tests summary:")
    (message "           ok: %d" ok-cnt)
    (message "       failed: %d" fail-cnt)
    (format "ok: %d; failed %d" ok-cnt fail-cnt)))


(simple-test-init)

(simple-test "123" (lambda ()
                     (expect "true" (lambda () t))))

(simple-test "fail" (lambda ()
                     (expect "true" (lambda () nil))))

(simple-tests-run)

(provide 'simple-test)
