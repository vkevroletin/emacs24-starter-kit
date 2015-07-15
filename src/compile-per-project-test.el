(require 'compile-per-project)
(require 'simple-test)


(simple-test-init)

(simple-test
 "test-init-map"
 (lambda ()
   (let ((my:compile-cmd-map))
     (my:init-compile-cmd-map)

     (check "1" (lambda ()
                  (hash-table-p my:compile-cmd-map)))

     (check "2" (lambda ()
                  (equal (hash-table-count my:compile-cmd-map) 0))))))

(simple-test
 "my:test-recompile"
 (lambda ()
   "Ensure that my:recompile configures default-directory global variable
and calls compilation-start with right argument."
   (let ((compilation-directory)
         (called nil))
     (with-new-functions
      '((compilation-start (lambda (command)
                             (check "dir"
                                    (lambda () (equal default-directory "/123/456")))
                             (check "cmd"
                                    (lambda () (equal command "mmmega command")))
                             (setq called t))))
      (lambda ()
        (my:recompile (compilation-command "test"
                                           :directory "/123/456"
                                           :command "mmmega command"))))
     (check "fake function was called"
             (lambda () called)))))

(simple-test
 "my:test-ask-and-recompile"
 (lambda ()
   "test-ask-and-recompile should not modify compilation command map"
   (let ((my:compile-cmd-map)
         (compile-called nil))
     (with-new-functions
      '((read-directory-name (lambda (x y) "/123/456"))
        (compilation-read-command (lambda (x) "mmmega command"))
        (my:recompile (lambda (x) (setq compile-called t)))
        (my:project-dir (lambda () "/project/dir")))
      (lambda ()
        (my:init-compile-cmd-map)
        (check "commands map is hash"
               (lambda () (compilation-command-p (my:ask-and-recompile nil))))

        (check "commands map is empty"
               (lambda () (equal (hash-table-count my:compile-cmd-map) 0))))))))

(simple-test-run)

(provide 'compile-per-project-test)
