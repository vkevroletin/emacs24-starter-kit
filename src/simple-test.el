;; Simple test runner
;;
;; No macro. No pain. Jump to failed test and directly execute with C-M-x.
;;

(defvar simple-tests (list))

(defvar simple-tests-in-progress nil)

(defun simple-test-init ()
  (setq simple-tests-in-progress t)
  (setq simple-tests (list)))

(defun simple-test (name body)
  (if simple-tests-in-progress
      (push (cons name body) simple-tests)
    (let ((debug-on-error t)
          (debug-on-signal t)
          (debug-on-quit t)))
    (funcall body)))

(defun check (name body)
  (condition-case err
      (unless (funcall body)
        (error "result is nil"))
    (error
     (error (format "check '%s' failed: %s" name (nth 1 err))))))

(defun check-error (name body)
  (condition-case err
      (progn
        (funcall body)
        (error (format "check-error '%s' failed: code didn't throw error" name)))
    (error
     t)))

(defun simple-tests-run ()
  (when (not simple-tests-in-progress)
    (error "1) call simple-test-init 2) define tests 3) call simple-tests-run"))
  (let ((ok-cnt 0)
        (fail-cnt 0))
    (dolist (test (reverse simple-tests))
      (message ">> Execute test '%s':" (car test))
      (condition-case err
          (progn
            (funcall (cdr test))
            (setq ok-cnt (+ 1 ok-cnt))
            (message "   ok"))
        (error
         (setq fail-cnt (+ 1 fail-cnt))
         (message (format "   test '%s' failed:\n   %s" (car test) (nth 1 err))))))
    (let ((result (format  "Tests summary: ok %d, failed %d" ok-cnt fail-cnt)))
      (message result)
      result))
  (setq simple-tests-in-progress nil))

(provide 'simple-test)
