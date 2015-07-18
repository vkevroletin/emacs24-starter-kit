(require 'compile-per-project)
(require 'simple-test)


(simple-test-init)

(simple-test
 "check caching functions"
 (lambda ()
   (let ((compile-per-project/compile-cmd-map)
         (test-cmd (compile-per-project/compilation-command "test"
                                                            :directory "dir123"
                                                            :command "cmd123"
                                                            :already-executed nil)))
     (compile-per-project/init-compile-cmd-map)
     (compile-per-project/cache-executed-command "test" "proj1" test-cmd)
     (check "inserted command available"
            (lambda () (compile-per-project/get-from-cache "test" "proj1"))))))

;; too lazy... can't test

(simple-test-run)

(provide 'compile-per-project-test)
