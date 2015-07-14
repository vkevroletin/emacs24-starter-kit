(require 'experimental)
(require 'test-classes)

(ert-deftest simple-interface-test-1 ()
  ""
  (let ((intf))
    (setq intf (si-test-plain-interface-module))

    (message "call different function")


    (should (equal  "magic1"
                    (dispatch 'function1 intf)))
    (should (equal "magic2"
                   (dispatch 'function2 intf)))

    (message "call recursive function")
    (should (equal "magic1"
                   (dispatch 'function3 intf)))

    (should-error
     (dispatch intf 'wrong-method))))

(ert-deftest simple-interface-test-2 ()
  ""
  (let ((cnt-intf)
        (cnt1)
        (cnt2))
    (setq cnt-intf (si-test-counter-module))
    (setq cnt1 (dispatch 'new cnt-intf))
    (setq cnt2 (dispatch 'new cnt-intf))

    (message "check init values")

    (should (equal 0
                   (dispatch-obj 'get cnt1)))

    (should (equal 0
                   (dispatch-obj 'get cnt2)))

    (message "add several times")

    (dispatch-obj 'add cnt1 10)
    (dispatch-obj 'add cnt1 10)

    (should (equal 20
                   (dispatch-obj 'get cnt1)))
    (should (equal 0
                   (dispatch-obj 'get cnt2)))))

(provide 'tests)
