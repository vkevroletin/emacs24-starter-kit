;;; compile-per-project --- remember compilation command per project

;;; Commentary:

; TODO:
; + code will throw without loaded projectile
; + save cache to disk
; + clear loaded cache
; + organize test properly

;;; Code:

(require 'compile)

(defvar my:compile-cmd-map (make-hash-table :test 'equal))

(defun my:init-compile-cmd-map()
  (setq my:compile-cmd-map (make-hash-table :test 'equal)))

(defun my:project-dir()
  "Determines project root using Projectile package and returns absolute path.
Returns nil of there is no project."
  (let ((projectile-require-project-root t))
    (condition-case ()
        (projectile-project-root)
      (error ()))))

(defun my:recompile(compile-obj)
  (let ((compilation-directory (car compile-obj))
        (command (nth 1 compile-obj)))
    (compilation-start command)))

(defun my:ask-and-recompile(last-compile-obj)
  (let* ((dir (read-directory-name "Compile in: "
                                   (or (nth 0 last-compile-obj)
                                       compilation-directory
                                       default-directory)))
         (prompt-cmd (or (nth 1 last-compile-obj)
                         (eval compile-command))) ; eval is from recompile definition
         (cmd (compilation-read-command prompt-cmd))
         (new-compile-cmd (list dir cmd)))
    (puthash (my:project-dir) new-compile-cmd  my:compile-cmd-map)
    (my:recompile new-compile-cmd)))

(defun compile-per-project(&optional arg)
  "During first call (or with prefix argument) asks compilation
directory and command. All successive calls without prefix argument cause
recompilation"
  (interactive "P")
  (let ((last-compile-obj (gethash (my:project-dir) my:compile-cmd-map))
        (force-dialog (consp arg)))
    (if (and last-compile-obj
             (not force-dialog))
        (my:recompile last-compile-obj)
      (my:ask-and-recompile last-compile-obj))))

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
  "Ensure that my:recompile configures compilation-directory global variable
and calls compilation-start with right argument."
  (let ((compilation-directory))
    (with-new-function
     'compilation-start
     (lambda (command)
       (should (equal compilation-directory "/123/456"))
       (should (equal command "mmmega command")))
     (lambda ()
       (my:recompile (list "/123/456" "mmmega command"))))))

(ert-deftest my:test-ask-and-recompile ()
  "test-ask-and-recompile should add new compilation command to map"
  (let ((my:compile-cmd-map))
    (with-new-function
     '((read-directory-name (lambda (x y) "/123/456"))
       (compilation-read-command (lambda (x) "mmmega command"))
       (my:recompile (lambda (x)))
       (my:project-dir (lambda () "/project/dir")))
     (lambda ()
       (my:init-compile-cmd-map)
       (my:ask-and-recompile nil)
       (should (gethash "/project/dir" my:compile-cmd-map))))))

(provide 'compile-per-project)
;;; scratch.el ends here
