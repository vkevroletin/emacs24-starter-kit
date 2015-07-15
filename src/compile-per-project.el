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

(defvar my:compile-cmd-map)

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

;; TODO: move hook change into configuration files
(my:load-cache-file)
(add-to-list 'kill-emacs-hook 'my:store-cache-into-file)

(provide 'compile-per-project)
;;; compile-per-project.el ends here
