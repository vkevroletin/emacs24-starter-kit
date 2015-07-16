;;; compile-per-project --- remember compilation command per project -*- lexical-binding: t -

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

(require 'simple-interface)
(require 'compile)
(require 'simple-test)


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

(defclass compile-per-project/compilation-command ()
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

(defvar compile-per-project/compile-cmd-map (make-hash-table :test 'equal))

(defun compile-per-project/init-compile-cmd-map ()
  (setq compile-per-project/compile-cmd-map (make-hash-table :test 'equal)))

(defun compile-per-project/print-compile-cmd-map ()
  (let ((result '()))
    (maphash (lambda (command proj-cache)
               (push (format "%s:\n" command) result)
               (maphash (lambda (proj value)
                          (push (format "    %s -> %s\n" proj value) result))
                        proj-cache))
             compile-per-project/compile-cmd-map)
    (concat result)))

;; reuse projectile-unserialize
(defun compile-per-project/load-cache-file ()
  (setq compile-per-project/compile-cmd-map
        (projectile-unserialize cache-file))
  (maphash (lambda (k1 cache)
             (maphash (lambda (k2 value)
                        (set-already-executed value nil))
                      cache))
           compile-per-project/compile-cmd-map))

(defun compile-per-project/store-cache-into-file ()
  (projectile-serialize compile-per-project/compile-cmd-map cache-file))

(defun compile-per-project/project-dir()
  "Determines project root using Projectile package and returns absolute path.
Returns nil of there is no project."
  (let ((projectile-require-project-root t))
    (condition-case ()
        (projectile-project-root)
      (error ()))))

(defun compile-per-project/ask-and-recompile(last-compile-obj recompile-body)
  (let* ((advice-dir (or (and last-compile-obj (get-directory last-compile-obj))
                         compilation-directory
                         default-directory))
         (advice-cmd (or (and last-compile-obj (get-command last-compile-obj))
                         (eval compile-command))) ; eval is from recompile definition
         (dir (read-directory-name "Compile in: " advice-dir))
         (cmd (compilation-read-command advice-cmd))
         (new-compile-obj (compile-per-project/compilation-command
                           "compilation-command"
                           :directory dir :command cmd)))
    (funcall recompile-body new-compile-obj)
    new-compile-obj))

(defun compile-per-project/cache-executed-command (wrapped-command-name project-id compile-obj)
  (let ((proj-cache (gethash project-id compile-per-project/compile-cmd-map)))
    (unless proj-cache
      (setq proj-cache (make-hash-table :test 'equal))
      (puthash project-id proj-cache compile-per-project/compile-cmd-map))
    (puthash wrapped-command-name
             (clone compile-obj :already-executed t)
             proj-cache)))

(defun compile-per-project/get-from-cache (wrapped-command-name project-name)
  (let ((proj-cache (gethash project-name compile-per-project/compile-cmd-map)))
    (if proj-cache
        (gethash wrapped-command-name proj-cache)
      nil)))

(defun compile-per-project/wrap-command (wrapped-command-name recompile-body)
  (lambda (is-force-dialog)
    (let* ((project-id (compile-per-project/project-dir))
           (last-compile-obj (compile-per-project/get-from-cache wrapped-command-name project-id))
           (executed-command (if (and last-compile-obj
                                      (is-already-executed last-compile-obj)
                                      (not is-force-dialog))
                                 (progn
                                   (funcall recompile-body last-compile-obj)
                                   last-compile-obj)
                               (compile-per-project/ask-and-recompile last-compile-obj
                                                                      recompile-body))))
      (compile-per-project/cache-executed-command wrapped-command-name
                                                  project-id
                                                  executed-command))))

(setq compile-per-project/recompile-impl-body
      (lambda (compile-obj)
        (let ((default-directory (get-directory compile-obj))
              (command (get-command compile-obj)))
          (compilation-start command))))

(setq compile-per-project/recompile-impl
      (compile-per-project/wrap-command "compile" compile-per-project/recompile-impl-body))

(setq compile-per-project/run-impl
      (compile-per-project/wrap-command "run" compile-per-project/recompile-impl-body))

(defun compile-per-project/recompile (&optional arg)
  (interactive "P")
  (funcall compile-per-project/recompile-impl (consp arg)))

(defun compile-per-project/compile ()
  (interactive)
  (compile-per-project/recompile '(t)))

(defun compile-per-project/run (&optional arg)
  (interactive "P")
  (funcall compile-per-project/run-impl (consp arg)))

;; TODO: move hook change into configuration files
;; (compile-per-project/load-cache-file)
;;(compile-per-project/init-compile-cmd-map)

(compile-per-project/load-cache-file)
(add-to-list 'kill-emacs-hook 'compile-per-project/store-cache-into-file)

;; TODO: fix old test
;; (simple-test-init)

;; (simple-test
;;  "check caching functions"
;;  (lambda ()
;;    (let ((compile-per-project/compile-cmd-map)
;;          (test-cmd (compile-per-project/compilation-command "test"
;;                                                             :directory "dir123"
;;                                                             :command "cmd123"
;;                                                             :already-executed nil)))
;;      (compile-per-project/init-compile-cmd-map)
;;      (compile-per-project/cache-executed-command "test" "proj1" test-cmd)
;;      (check "inserted command available"
;;             (lambda () (compile-per-project/get-from-cache "test" "proj1"))))))

;; (simple-test
;;  "dev"
;;  (lambda ()
;;    (let* ((test (lambda (cmd) (message "good")))
;;           (f (compile-per-project/wrap-command "test" test)))

;;      (funcall f t)

;;      t)))

;; (simple-test-run)

(provide 'compile-per-project)
;;; compile-per-project.el ends here
