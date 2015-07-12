;;; compile-per-project --- remember compilation command per project

;;; Commentary:

;;; TODO:
;; Implementation:
;; + code will throw without loaded projectile
;; + move hook change into configuration files
;; + after loading cache ask user during first compilation
;; + fix naming
;; + flush cache from time to time to disk
;; Testing:
;; + use interfaces instead of dynamic scoping to make test environment
;; + organize tests properly
;;; Code:

(require 'compile)

;;; Customization
(defgroup compile-per-project nil
  "Improved compile command."
  :group 'tools
  :group 'convenience)

(defcustom cache-file
  (expand-file-name "compile-per-project.cache" user-emacs-directory)
  "The name of cache file."
  :group 'compile-per-project
  :type 'string)

;;; Implementation

(defclass compilation-command ()
  ((directory :documentation "Compilation directory"
              :initarg :directory
              :type string
              :accessor get-directory)
   (command :documentation "Compilation command"
            :initarg :command
            :type string
            :accessor get-command)
   (already-executed :documentation "Were command executed
during current session or no"
                     :type boolean
                     :initform nil
                     :initarg :already-executed
                     :accessor is-already-executed
                     :writer set-already-executed))
  "Data required to repeat build command.")

(defvar my:compile-cmd-map )

(defun my:init-compile-cmd-map()
  (setq my:compile-cmd-map (make-hash-table :test 'equal)))

;; reuse projectile-unserialize
(defun my:load-cache-file ()
  (setq my:compile-cmd-map
        (projectile-unserialize cache-file))
  (maphash (lambda (k v) (set-already-executed v nil))
           my:compile-cmd-map))

(defun my:store-cache-into-file ()
  (projectile-serialize my:compile-cmd-map cache-file))

(defun my:project-dir()
  "Determines project root using Projectile package and returns absolute path.
Returns nil of there is no project."
  (let ((projectile-require-project-root t))
    (condition-case ()
        (projectile-project-root)
      (error ()))))

(defun my:recompile(compile-obj)
  (let ((default-directory (get-directory compile-obj))
        (command (get-command compile-obj)))
    (compilation-start command))
  compile-obj)

(defun my:ask-and-recompile(last-compile-obj)
  (let* ((advice-dir (or (and last-compile-obj (get-directory last-compile-obj))
                         compilation-directory
                         default-directory))
         (advice-cmd (or (and last-compile-obj (get-command last-compile-obj))
                         (eval compile-command))) ; eval is from recompile definition
         (dir (read-directory-name "Compile in: " advice-dir))
         (cmd (compilation-read-command advice-cmd))
         (new-compile-obj (compilation-command "compilation-command"
                                               :directory dir :command cmd)))
    (my:recompile new-compile-obj)
    new-compile-obj))

(defun my:cache-executed-command (project-id compile-obj)
  (puthash project-id
           (clone compile-obj :already-executed t)
           my:compile-cmd-map))

(defun compile-per-project(&optional arg)
  (interactive)
  (recompile-per-project '(P)))

(defun recompile-per-project(&optional arg)
  "During first call (or with prefix argument) asks compilation
directory and command. All successive calls without prefix argument cause
recompilation"
  (interactive "P")
  (let* ((project-id (my:project-dir))
         (last-compile-obj (gethash  project-id my:compile-cmd-map))
         (is-force-dialog (consp arg)))
    (my:cache-executed-command
     project-id
     (if (and last-compile-obj
              (is-already-executed last-compile-obj)
              (not is-force-dialog))
         (my:recompile last-compile-obj)
       (my:ask-and-recompile last-compile-obj)))))

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


;; TODO: move hook change into configuration files
(my:load-cache-file)
(add-to-list 'kill-emacs-hook 'my:store-cache-into-file)

(provide 'compile-per-project)
;;; compile-per-project.el ends here
