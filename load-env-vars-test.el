(require 'ert)
(require 'load-env-vars)

(defvar test-load-env-vars-sample )

(ert-deftest test-load-env-vars-extract-env-vars ()
       "Tests extracting env-vars from string"
       (should (equal
                (load-env-vars-extract-env-vars "
export EXPORT_KEY=VALUE
KEY=VALUE
KEY_QUOTES='VALUE'
KEY_DOUBLE_QUOTES=\"VALUE\"
KEY_WITH_COMMENT=VALUE # Inline comments are ignored
")
                (list
                 (list "KEY_WITH_COMMENT" "VALUE")
                 (list "KEY_DOUBLE_QUOTES" "VALUE")
                 (list "KEY_QUOTES" "VALUE")
                 (list "KEY" "VALUE")
                 (list "EXPORT_KEY" "VALUE")
                 ))))

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
