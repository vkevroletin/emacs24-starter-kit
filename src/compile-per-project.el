;;; compile-per-project --- remember compilation command per project -*- lexical-binding: t -

(require 'compile)
(require 'projectile)
(require 'dash)
(require 'project-cache)

;;; Implementation

(defgroup compile-per-project nil
  "Persistent compilation commands cache. It assosiates
compilation commands with Projectile projects."
  :group 'tools
  :group 'convenience)

(defcustom compile-per-project-ask-after-project-change t
  "If true then commands defined in this module will
interactively ask user in case they previously were issued from
other project. This option helps to prevent accidental execution
of previously remembered command when you swith from one project
to another."
  :group 'compile-per-project
  :type '(boolean))

(defun compile-per-project--ask-command (cmd-name last-compile-data)
  (let* ((advice-dir (or (and last-compile-data (plist-get last-compile-data :dir))
                         compilation-directory
                         default-directory))
         (advice-cmd (or (and last-compile-data (plist-get last-compile-data :cmd))
                         (eval compile-command))) ; eval is from recompile definition
         (dir (read-directory-name (concat cmd-name " in: ") advice-dir))
         ;; compilation-read-command uses default-directory to suggest
         ;; commands defined in directory (executables, scripts)
         (default-directory dir)
         (cmd (compilation-read-command advice-cmd)))
    (list :cmd cmd :dir dir)))

(defun compile-per-project--ask-and-call (cmd-name body force-dialog)
  (let ((last-compile-data (project-cache-get cmd-name)))
    (when (or (null last-compile-data) force-dialog)
      (setq last-compile-data (compile-per-project--ask-command cmd-name last-compile-data))
      (project-cache-put cmd-name last-compile-data))
    (funcall body last-compile-data)))

(defun compile-per-project--save-projectile-buffers ()
  "Similar to projectile-save-project-buffers but does not
save files which were deleted outside of emacs"
  (when (projectile-project-p)
    (--each (projectile-project-buffers)
      (with-current-buffer it
        (when (and buffer-file-name
                   (file-exists-p buffer-file-name)
                   (file-writable-p buffer-file-name))
          (save-buffer))))))

(defvar compile-per-project--last-project-name "#dummy")

(defun compile-per-project--detect-project-chage ()
  (when compile-per-project-ask-after-project-change
    (let ((current-project-name (projectile-project-p)))
      (unless (equal current-project-name compile-per-project--last-project-name)
        (setq compile-per-project--last-project-name current-project-name)
        t))))

(defun compile-per-project--compile-body (data)
  (let ((default-directory (plist-get data :dir))
        (command           (plist-get data :cmd)))
    (compile-per-project--save-projectile-buffers)
    (compilation-start command)))

(defun compile-per-project--compile (cmd-name force-dialog)
  (compile-per-project--ask-and-call cmd-name
                                     #'compile-per-project--compile-body
                                     (or force-dialog
                                         (compile-per-project--detect-project-chage))))

(defun compile-per-project-compile (&optional arg)
  (interactive "P")
  (compile-per-project--compile "Compile" (consp arg)))

(defun compile-per-project-run (&optional arg)
  (interactive "P")
  (compile-per-project--compile "Run" (consp arg)))

(defun compile-per-project-test (&optional arg)
  (interactive "P")
  (compile-per-project--compile "Test" (consp arg)))

(provide 'compile-per-project)
;;; compile-per-project.el ends here
