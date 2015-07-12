;;; experimental --- javascript-like objects in emacs lisp -*- lexical-binding: t -

;;; Commentary:

(defun make-simple-interface (&rest export-list)
  (let ((interface (make-hash-table :test 'equal)))
    (dolist (x export-list)
      (puthash x (symbol-function x) interface))
    interface))

;; directly call interface function and pass interface as first argument
(defun ==> (interface name &rest args)
  (unless (and interface (hash-table-p interface))
    (error "expected hash table with functions"))
  (let ((f (gethash name interface)))
    (unless (and f (functionp f)) (error ""))
    (apply f interface args)))

;; call object function and pass blessed object as first argument
(defun => (blessed name &rest args)
  (unless (consp blessed)
    (error "expected blessed pair"))
  (let ((interface (cdr blessed)))
    (unless (and interface (hash-table-p interface))
      (error "expected hash table with functions"))
    (let ((f (gethash name interface)))
      (unless (and f (functionp f)) (error ""))
      (apply f blessed args))))

;; bless
(defun >> (obj interface)
  (cons obj interface))

;; unbless
(defun << (blessed)
  (car blessed))

;; get prototype
(defun <<I (blessed)
  (cdr blessed))

(defun compilation-command-factory ()
  (defun get-directory (I this)
    (nth 1 this))

  (defun get-command (I this)
    (nth 2 this))

  (defun get-already-executed (I this)
    (nth 0 this))

  (defun set-already-executed (I this value)
    (cons value (cdr this)))

  (defun new (I dir cmd already-executed)
    (list dir cmd already-executed))

  (make-simple-interface 'new
                         'get-directory
                         'get-command
                         'get-already-executed
                         'set-already-executed))

(defun blessed-compilation-command-factory ()
  (let ((interface))

    (defun get-directory (this)
      (nth 1 (<< this)))

    (defun get-command (this)
      (nth 2 (<< this)))

    (defun get-already-executed (this)
      (nth 0 (<< this)))

    (defun set-already-executed (this value)
      (>> (cons value (cdr (<< this)))
          (<<I this)))

    (defun new (class dir cmd already-executed)
      (>> (list already-executed dir cmd) class))

    (setq interface
          (make-simple-interface 'new
                                 'get-directory
                                 'get-command
                                 'get-already-executed
                                 'set-already-executed))))

(setq cmd-intf (compilation-command-factory))
(==> cmd-intf 'new "dir" "cmd" t)
(==> cmd-intf 'get-directory
              (==> cmd-intf 'new "dir" "cmd" t))

(setq cmd-intf-blessed (blessed-compilation-command-factory))
(setq test (==> cmd-intf-blessed 'new "dir" "cmd" t))

(=> test 'get-command)
(=> test 'get-directory)
(=> test 'get-already-executed)
(=> (=> test 'set-already-executed t)
    'get-directory)

(provide 'experimental)
;;; experimental ends here
