(require 'projectile)
(require 'dash)
(require 'ede)
(require 'flycheck)

(defun auto-cpp--match-cnt (regex string)
  (let ((start 0)
        (res   0))
    (while (string-match regex string start)
      (setq res (+ 1 res))
      (setq start (match-end 0)))
    res))

(defun auto-cpp--weight-function (path)
  (let ((dir (concat "/" (file-name-directory path))))
    (if (null dir) 0
      (let ((main-const
             (+ (* 2 (auto-cpp--match-cnt "/inc/" dir))
                (* 2 (auto-cpp--match-cnt "/include/" dir))
                (auto-cpp--match-cnt "/lib/" dir)
                (auto-cpp--match-cnt "/sdk/" dir)))
            (path-len (auto-cpp--match-cnt "/" path)))
        (- (* 100 main-const)
           path-len)))))

(defun auto-cpp--max-index (fun xs)
  (unless (null xs)
    (let ((best-val (funcall fun (car xs)))
          (best-idx 0)
          (rest     (cdr xs))
          (idx      1))
      (while (not (null rest))
        (let ((val (funcall fun (car rest))))
          (when (> val best-val)
            (setq best-val val)
            (setq best-idx idx)))
        (setq rest (cdr rest))
        (setq idx (+ 1 idx)))
      best-idx)))

(defun auto-cpp--choose-with-weight-function (xs)
  (let ((res-idx (auto-cpp--max-index 'auto-cpp--weight-function xs)))
    (nth res-idx xs)))

(defun auto-cpp--is-cpp-file (file)
  (let ((ext (file-name-extension file)))
    (--any (equal ext it) '("h" "hpp" "c" "cpp"))))

(defun auto-cpp--guess-if-cpp-proj (dir)
  (let* ((default-directory dir)
         (all-files (projectile-current-project-files))
         (all-files-cnt (length all-files))
         (cpp-files-cnt (-count 'auto-cpp--is-cpp-file all-files))
         (cpp-percents (/ (* 100 cpp-files-cnt) all-files-cnt)))
    (or (> cpp-files-cnt 100)
        (> cpp-percents  20))))

(defun auto-cpp--is-string-suffix (full-str suffix)
  (or (equal full-str suffix)
      (string-suffix-p (concat "/" suffix) full-str t)))

(defun auto-cpp--locate-file (file-name root-dir)
  (let* ((default-directory root-dir)
         (all-files  (projectile-current-project-files))
         (good-files (--filter (auto-cpp--is-string-suffix it file-name) all-files)))
    (when (not (null good-files))
      (expand-file-name (auto-cpp--choose-with-weight-function good-files)
                        root-dir))))

(defun auto-cpp--project-file (&optional dir)
  (let* ((default-directory dir)
         (proj (projectile-project-p)))
    (when  proj
      (let* ((proj-file-name   (expand-file-name ".cpp-project" proj))
             (proj-file-exists (file-exists-p proj-file-name)))
        (when (or proj-file-exists
                  (auto-cpp--guess-if-cpp-proj dir))
          proj-file-name)))))

(defun auto-cpp--project-root (dir)
  (let ((projfile (auto-cpp--project-file (or dir default-directory))))
    (when projfile
      (file-name-directory projfile))))

(defun auto-cpp--load-project (dir)
  (let ((proj-file-name (auto-cpp--project-file dir)))
    (when (not (file-exists-p proj-file-name))
      (with-temp-buffer (write-file proj-file-name)))
    (ede-cpp-root-project "some-cool-project"
                          :file proj-file-name
                          :locate-fcn 'auto-cpp--locate-file)))

(defun auto-cpp-register-ede-autoload ()
  (ede-add-project-autoload
   (ede-project-autoload "dynamic-cpp-root"
                         :name "dynamic cpp root"
                         :file 'ede/cpp-root
                         :proj-file 'auto-cpp--project-file
                         :proj-root 'auto-cpp--project-root
                         :load-type 'auto-cpp--load-project
                         :proj-root-dirmatch "*" ;; have no idea what is it
                         :class-sym 'ede-cpp-root-project
                         :new-p nil
                         :safe-p t)))

(defun auto-cpp--guess-if-include-dir (dir)
  (--any (string-match it dir) '("/inc/" "/include/")))

(defun auto-cpp--guess-project-includes ()
  (when (projectile-project-p)
    (let ((root (projectile-project-root))
          (dirs (projectile-current-project-dirs))
          (case-fold-search t))
      (-filter 'auto-cpp--guess-if-include-dir
               (--map (expand-file-name it root) dirs)))))

(defun auto-cpp-configure-flycheck ()
  (-when-let (dirs (auto-cpp--guess-project-includes))
    (make-variable-buffer-local 'flycheck-clang-include-path)
    (make-variable-buffer-local 'flycheck-gcc-include-path)
    (setq flycheck-clang-include-path (append flycheck-clang-include-path dirs))
    (setq flycheck-gcc-include-path   (append flycheck-gcc-include-path dirs))))

(provide 'auto-cpp-project)
