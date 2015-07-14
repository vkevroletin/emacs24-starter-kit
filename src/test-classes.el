;; -*- lexical-binding: t -

(defun si-test-plain-interface-module ()
  (defun function1 (intf)
    "magic1")

  (defun function2 (intf)
    "magic2")

  (defun function3 (intf)
    (<> 'function1 intf))

  (make-simple-interface 'function1
                         'function2
                         'function3))

(defun si-test-counter-module ()
  (defun add (intf data x)
    (setcar data (+ x (car data))))

  (defun get (intf data)
    (car data))

  (defun new (intf)
    (bless intf (list 0)))

  (make-simple-interface 'new
                         'add
                         'get))

(provide 'test-classes)
