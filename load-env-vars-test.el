(require 'ert)
(require 'load-env-vars)

(ert-deftest test-load-env-vars ()
  "Test loading env vars from file"
  (let (set-env-vars (list))
    (cl-letf (((symbol-function 'setenv)
               (lambda (key value)
                 (push (list key value) set-env-vars))))
      (load-env-vars "env.example")
      (should (equal
               (list
                (list "EXPORT_KEY" "VALUE")
                (list "KEY" "VALUE")
                (list "KEY_QUOTES" "VALUE")
                (list "KEY_DOUBLE_QUOTES" "VALUE")
                (list "KEY_WITH_COMMENT" "VALUE")
                )
               set-env-vars)))))
