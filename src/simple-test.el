;; -*- lexical-binding: t -
;; Simple test runner
;;
;; No macro. No pain. Jump to failed test and directly execute with C-M-x.
;;

(defvar simple-test-list (list))

(defvar simple-test-in-progress nil)

(defun simple-test-init ()
  (setq simple-test-in-progress t)
  (setq simple-test-list (list)))

(defun simple-test (name body)
  (if simple-test-in-progress
      (push (cons name body) simple-test-list)
    (let ((debug-on-error t)
          (debug-on-signal t)
          (debug-on-quit t))
      (funcall body))))

(defun check (name body)
  (condition-case err
      (unless (funcall body)
        (error "result is nil"))
    (error
     (error (format "check '%s' failed: %s" name err)))))

(defun check-error (name body)
  (condition-case err
      (progn
        (funcall body)
        (error (format "check-error '%s' failed: code didn't throw error" name)))
    (error
     t)))

(defun with-new-functions (fname-body-list expr)
  "Let for several functions. Useful to make isolated test environment in case of
dynamic binding."
  (let* ((get-current-fun (lambda (x)
                            (list x (symbol-function x))))
         (old-funs (mapcar (lambda (x)
                             (funcall get-current-fun (car x)))
                           fname-body-list)))
    (unwind-protect
        (progn
          (dolist (x fname-body-list)
            (fset (car x) (nth 1 x)))
          (funcall expr))
      (dolist (x old-funs)
       (fset (car x) (nth 1 x))))))

(defun simple-test-run ()
  (unwind-protect
      (progn
        (when (not simple-test-in-progress)
          (error "1) call simple-test-init 2) define tests 3) call simple-test-run"))
        (let ((ok-cnt 0)
              (fail-cnt 0))
          (dolist (test (reverse simple-test-list))
            (message ">> Execute test '%s':" (car test))
            (condition-case err
                (progn
                  (funcall (cdr test))
                  (setq ok-cnt (+ 1 ok-cnt))
                  (message "   ok"))
              (error
               (setq fail-cnt (+ 1 fail-cnt))
               (message (format "   test '%s' failed:\n   %s" (car test) err)))))
          (let ((result (format  "Tests summary: ok %d, failed %d" ok-cnt fail-cnt)))
            (message result)
            result)))
    (setq simple-test-in-progress nil)))

(provide 'simple-test)
