(require 'compile-per-project)

;;; Tests

(ert-deftest my:test-init-map()
  "init-compile-cmd-map should clear my:compile-cmd-map."
  (let ((my:compile-cmd-map))
    (my:init-compile-cmd-map)
    (should (hash-table-p my:compile-cmd-map))
    (should (equal (hash-table-count my:compile-cmd-map) 0))))

(defun with-many-new-functions (fname-body-list expr)
  "Let for several functions."
  (let* ((get-old-fun (lambda (x) (list x (symbol-function x))))
         (old-funs (mapcar (lambda (x) (funcall get-old-fun (car x))) fname-body-list)))
    (unwind-protect
        (progn
          (dolist (x fname-body-list)
            (fset (car x) (nth 1 x)))
          (funcall expr))
      (dolist (x old-funs)
       (fset (car x) (nth 1 x))))))

(defun with-new-function (fname fbody &optional expr)
  "Let for single function."
  (if (not expr) (with-many-new-functions fname fbody)
    (progn
      (let ((old-fun-body (symbol-function fname)))
        (unwind-protect
            (progn
              (fset fname fbody)
              (funcall expr))
          (fset fname old-fun-body))))))

(ert-deftest my:test-recompile()
  "Ensure that my:recompile configures default-directory global variable
and calls compilation-start with right argument."
  (let ((compilation-directory))
    (with-new-function
     'compilation-start
     (lambda (command)
       (should (equal default-directory "/123/456"))
       (should (equal command "mmmega command")))
     (lambda ()
       (my:recompile (compilation-command "test"
                                          :directory "/123/456"
                                          :command "mmmega command"))))))

(ert-deftest my:test-ask-and-recompile ()
  "test-ask-and-recompile should not modify compilation command map"
  (let ((my:compile-cmd-map)
        (compile-called nil))
    (with-new-function
     '((read-directory-name (lambda (x y) "/123/456"))
       (compilation-read-command (lambda (x) "mmmega command"))
       (my:recompile (lambda (x) (setq compile-called t)))
       (my:project-dir (lambda () "/project/dir")))
     (lambda ()
       (my:init-compile-cmd-map)
       (should (compilation-command-p (my:ask-and-recompile nil)))
       (should (equal (hash-table-count my:compile-cmd-map) 0))))))

(provide 'compile-per-project-test)
