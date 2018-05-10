;;; load-env-vars.el --- Load environment variables from files                     -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Jorge Dias

;; Author: Jorge Dias <jorge@mrdias.com>
;; URL: https://github.com/diasjorge/emacs-load-env-vars
;; Keywords: lisp
;; Version: 0.0.1
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

;;; Code:

;; code goes here

(defvar load-env-vars-env-var-regexp
  "^\\(?:export[[:blank:]]\\)?\\([[:alpha:]_]+[[:alnum:]_]*\\)[=]['\"]?\\([^[:space:]'\"]*\\)['\"]?"
  "Regexp to match env vars in file.")

(defun load-env-vars-extract-env-vars (string)
  "Extract environment variable name and value from STRING."
  (save-match-data
    (let (matches)
      (dolist (element (split-string string "\n" t))
        (if (string-match load-env-vars-env-var-regexp element)
            (push (list (match-string 1 element) (match-string 2 element)) matches)))
      matches)))

(defun load-env-vars-get-string-from-file (file-path)
  "Return FILE-PATH's file content."
  (with-temp-buffer
    (insert-file-contents file-path)
    (buffer-string)))

(defun load-env-vars-set-env-vars (env-vars)
  "Set envariable variables from key value lists from ENV-VARS."
  (dolist (element env-vars)
    (let ((key (car element)) (value (cadr element)))
      (setenv key value))))

;;;###autoload
(defun load-env-vars (file-path)
  "Load environment variables found in FILE-PATH."
  (interactive "fEnvironment variables file: ")
  (let ((env-vars (load-env-vars-extract-env-vars (load-env-vars-get-string-from-file file-path))))
    (load-env-vars-set-env-vars env-vars)))

(provide 'load-env-vars)
;;; load-env-vars.el ends here
