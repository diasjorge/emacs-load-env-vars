(when (require 'undercover nil t)
  (undercover "load-env-vars.el"))

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
                (list "KEY_WITH_QUOTES_INSIDE_DOUBLE_QUOTES" "VALUE'")
                (list "KEY_WITH_QUOTED_QUOTES" "VALUE\\'")
                (list "KEY_WITH_DOUBLE_QUOTES_INSIDE_QUOTES" "VALUE\"")
                (list "KEY_WITH_QUOTED_DOUBLE_QUOTES" "VALUE\\\"")
                (list "KEY_WITH_COMMENT" "VALUE")
                (list "KEY_WITH_COLON" "VALUE")
                )
               set-env-vars)))))
