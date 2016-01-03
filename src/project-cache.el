;;; project-cache --- attach values to current project -*- lexical-binding: t -

(require 'projectile)
(require 'dash)

(defcustom project-cache-file
  (expand-file-name ".project.cache" user-emacs-directory)
  "The name of cache file."
  :type 'string)

(defvar project-cache--data (make-hash-table :test 'equal))

(defun project-cache-put (key value)
  (let ((proj-id (projectile-project-p)))
    (project-cache--put-into proj-id key value)))

(defun project-cache-get (key)
  (let ((proj-id (projectile-project-p)))
    (project-cache--get-from proj-id key)))

;; reuse projectile-unserialize
(defun project-cache-read-from-file ()
  (setq project-cache--data (projectile-unserialize project-cache-file)))

(defun project-cache-write-to-file ()
  (projectile-serialize project-cache--data project-cache-file))

(defun project-cache-clear ()
  (setq project-cache--data (make-hash-table :test 'equal)))

(defun project-cache--print ()
  (let ((result '()))
    (maphash (lambda (command proj-cache)
               (push (format "%s:\n" command) result)
               (maphash (lambda (proj value)
                          (push (format "    %s -> %s\n" proj value) result))
                        proj-cache))
             project-cache--data)
    (concat result)))

(defun project-cache--put-into (proj-id key value)
  (let ((proj-cache (gethash proj-id project-cache--data)))
    (unless proj-cache
      (setq proj-cache (make-hash-table :test 'equal))
      (puthash proj-id proj-cache project-cache--data))
    (puthash key
             value
             proj-cache)))

(defun project-cache--get-from (proj-id key)
  (-when-let (proj-cache (gethash proj-id project-cache--data))
    (gethash key proj-cache)))

(condition-case nil
    (project-cache-read-from-file))
(unless (hash-table-p project-cache--data)
  (project-cache-clear))

(add-to-list 'kill-emacs-hook #'project-cache-write-to-file)

(provide 'project-cache)
