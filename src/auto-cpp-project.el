(require 'projectile)
(require 'dash)
(require 'ede)

;; TODO: create weigh function which considers length of path and
;;       amount of names like "inc", "include", "sdk", "lib" etc
(defun auto-cpp--best-match (dir file-name)
  (let* ((default-directory dir)
         (all-files  (projectile-current-project-files))
         (good-files (--filter (string-match file-name it) all-files)))
    (when (not (null good-files))
      (expand-file-name (car good-files) dir))))

(defun auto-cpp--is-cpp-file (file)
  (let ((ext (file-name-extension file)))
    (--any (equal ext it) '("h" "hpp" "c" "cpp"))))

(defun auto-cpp--guess-cpp-proj (dir)
  (let* ((default-directory dir)
         (all-files (projectile-current-project-files))
         (all-files-cnt (length all-files))
         (cpp-files-cnt (-count 'auto-cpp--is-cpp-file all-files))
         (cpp-percents (/ (* 100 cpp-files-cnt) all-files-cnt)))
    (or (> cpp-files-cnt 100)
        (> cpp-percents  20))))

(defun auto-cpp--locate (filename root-dir)
  (auto-cpp--best-match root-dir filename))

(defun auto-cpp--file-for-dir (&optional dir)
  (let* ((default-directory dir)
         (proj (projectile-project-p)))
    (when  proj
      (let* ((proj-file-name   (expand-file-name ".cpp-project" proj))
             (proj-file-exists (file-exists-p proj-file-name)))
        (when (or proj-file-exists
                  (auto-cpp--guess-cpp-proj dir))
          (when (not proj-file-exists)
            (with-temp-buffer (write-file proj-file-name)))
          proj-file-name)))))

(defun auto-cpp--project-root (dir)
  (let ((projfile (auto-cpp--file-for-dir (or dir default-directory))))
    (when projfile
      (file-name-directory projfile))))

(defun auto-cpp--load (dir)
  (ede-cpp-root-project "some-cool-project"
                        :file (auto-cpp--file-for-dir dir)
                        :locate-fcn 'auto-cpp--locate))

(ede-add-project-autoload
 (ede-project-autoload "dynamic-cpp-root"
                       :name "dynamic cpp root"
                       :file 'ede/cpp-root
                       :proj-file 'auto-cpp--file-for-dir
                       :proj-root 'auto-cpp--project-root
                       :load-type 'auto-cpp--load
                       :proj-root-dirmatch "*" ;; have no idea what is it
                       :class-sym 'ede-cpp-root-project
                       :new-p nil
                       :safe-p t))

(provide 'auto-cpp-project)
