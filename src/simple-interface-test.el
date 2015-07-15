;; -*- lexical-binding: t -

(require 'simple-interface)
(require 'simple-test)

(defun si-test-plain-interface-module ()
  "Creates simple interface with 3 functions."

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
  "Creates interface to create and manipulate object with state."

  (defun add (intf data x)
    (setcar data (+ x (car data))))

  (defun get (intf data)
    (car data))

  (defun new (intf)
    (bless intf (list 0)))

  (make-simple-interface 'new
                         'add
                         'get))

(simple-test-init)

(simple-test
 "plain interface"
 (lambda ()
   (let ((intf (si-test-plain-interface-module)))

     (expect "call function1"
             (lambda () (equal  "magic1"
                           (<> 'function1 intf))))

     (expect "call function2"
             (lambda () (equal "magic2"
                          (<> 'function2 intf))))

     (expect "call recursive function"
             (lambda () (equal "magic1"
                          (<> 'function3 intf))))

     (expect-error "call of wrong method cause error"
                   (lambda () (<> intf 'wrong-method))))))

(simple-test
 "objects with state"
 (lambda ()
   (let* ((cnt-intf (si-test-counter-module))
          (cnt1 (<> 'new cnt-intf))
          (cnt2 (<> 'new cnt-intf)))

     (expect "init values of 1st counter"
             (lambda () (equal 0 (<-> 'get cnt1))))

     (expect "init value of 2nd counter"
             (lambda () (equal 0 (<-> 'get cnt2))))

     (expect "add several times"
             (lambda ()
               (<-> 'add cnt1 10)
               (<-> 'add cnt1 10)
               t))

     (expect "new value of 1st counter"
             (lambda () (equal 20 (<-> 'get cnt1))))

     (expect "unchanged value of 2nd counter"
             (lambda () (equal 0 (<-> 'get cnt2)))))))

(simple-tests-run)

(provide 'simple-interface-test)
