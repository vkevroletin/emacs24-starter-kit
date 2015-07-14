;;; experimental --- javascript-like objects in emacs lisp -*- lexical-binding: t -

;;; Commentary:

(defun make-simple-interface (&rest export-list)
  (let ((interface (make-hash-table :test 'equal)))
    (dolist (x export-list)
      (puthash x (symbol-function x) interface))
    interface))

;; call interface function and pass interface as first argument
(defun dispatch (method interface &rest args)
  (unless (and interface (hash-table-p interface))
    (error "expected hash table with functions"))
  (let ((f (gethash method interface)))
    (unless (and f (functionp f))
      (error "interface doesn't have requested method %s" method))
    (apply f interface args)))

(fset '<> 'dispatch)

;; unpack pair of interface and data and call function from interface
;; passing interface and data as first two parameters
(defun dispatch-obj (method pair &rest args)
  (unless (consp pair)
    (error "expected pair as first argument"))
  (let ((interface (car pair))
         (data      (cdr pair)))
    (unless (and interface (hash-table-p interface))
      (error "expected hash table with functions"))
    (let ((f (gethash method interface)))
      (unless (and f (functionp f))
        (error "interface doesn't have requested method %s" method))
      (apply f interface data args))))

(fset '<-> 'dispatch-obj)

;; bless
(defun bless (interface obj)
  (cons interface obj))

;; unbless
(defun <<D (blessed)
  (cdr blessed))

;; get prototype
(defun <<I (blessed)
  (car blessed))

(provide 'experimental)
;;; experimental ends here
