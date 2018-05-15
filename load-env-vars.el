;;; load-env-vars.el --- Load environment variables from files                     -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Jorge Dias

;; Author: Jorge Dias <jorge@mrdias.com>
;; URL: https://github.com/diasjorge/emacs-load-env-vars
;; Keywords: lisp
;; Version: 0.0.2
;; Package-Requires: ((emacs "24"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package allows you set environment variables loaded from a
;; file with bash style variable declarations.
;; Supported syntax:
;;
;; export KEY=VALUE
;; KEY=VALUE
;; KEY='VALUE'
;; KEY="VALUE"
;; # Comment lines are ignored
;; KEY=VALUE # Inline comments are ignored
;; KEY: VALUE

;;; Code:

(defvar load-env-vars-env-var-regexp
  (rx
   line-start
   (0+ space)
   (optional "export" (0+ space)) ;; optional export
   (group (1+ (in "_" alnum))) ;; key
   (or
    (and (0+ space) "=" (0+ space))
    (and ":" (1+ space))) ;; separator
   (or
    (and "'" (group (0+ (or "\\'" (not (any "'"))))) "'") ;; single quoted value
    (and ?\" (group (0+ (or "\\\"" (not (any "\""))))) ?\") ;; double quoted value
    (group (1+ (not (in "#" "\n" space)))) ;; unquoted value
    )
   (0+ space)
   (optional "#" (0+ any))
   )
  "Regexp to match env vars in file."
  )

(defvar load-env-vars-env-var-internal-regexp
  (rx
   "$"
   (optional "{")
   (group (1+ (in "_" alnum)))
   (optional "}"))
  "Regexp to match internal/embedded env vars in a string")

(defun load-env-vars-sub-internal-variables (str)
  "Substitutes embedded variables references in STR, for instance,
if we had previous defined an environment variable (with `setenv')
of 'FOO' to be 'hello', then a value for STR of either '$FOO/world'
or '${FOO}/world' would return a value of 'hello/world'.

Note: Referencing undefined variables will be substituted with an
empty string."
  (replace-regexp-in-string load-env-vars-env-var-internal-regexp
                            (lambda (p)
                              (or (getenv (match-string 1 p)) ""))
                            value t))

(defun load-env-vars-re-seq (regexp)
  "Get a list of all REGEXP matches in a buffer."
  (save-excursion
    (goto-char (point-min))
    (save-match-data
      (let (matches)
        (while (re-search-forward regexp nil t)
          (push (list (match-string-no-properties 1) (or (match-string-no-properties 2) (match-string-no-properties 3) (match-string-no-properties 4))) matches))
        matches))))

(defun load-env-vars-extract-env-vars ()
  "Extract environment variable name and value from STRING."
  (load-env-vars-re-seq load-env-vars-env-var-regexp))

(defun load-env-vars-set-env (env-vars)
  "Set envariable variables from key value lists from ENV-VARS."
  (dolist (element env-vars)
    (let* ((key (car element))
           (value (cadr element))
           (expval (load-env-vars-sub-internal-variables value)))
      (setenv key expval))))

;;;###autoload
(defun load-env-vars (file-path)
  "Load environment variables found in FILE-PATH."
  (interactive "fEnvironment variables file: ")
  (with-temp-buffer
    (insert-file-contents file-path)
    (let ((env-vars (load-env-vars-extract-env-vars)))
      (load-env-vars-set-env env-vars))))

(provide 'load-env-vars)
;;; load-env-vars.el ends here
