;;; org-marginalia-namespace-test.el --- Namespace tests for org-marginalia -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests that the standalone org-marginalia package does not expose legacy
;; personal namespace symbols.

;;; Code:

(require 'ert)
(require 'subr-x)

(defun org-marginalia-namespace-test--root ()
  "Return the package root directory."
  (file-name-directory (or (locate-library "org-marginalia")
			   load-file-name
			   buffer-file-name)))

(defun org-marginalia-namespace-test--elisp-files ()
  "Return package Elisp files excluding tests."
  (directory-files (org-marginalia-namespace-test--root) t "\\.el\\'"))

(ert-deftest org-marginalia-namespace-has-no-hub-symbols ()
  "Package code does not contain legacy hub symbols.
The literal Org metadata property names `HUB_NOTE_*' are intentionally allowed."
  (dolist (file (org-marginalia-namespace-test--elisp-files))
    (let ((contents (with-temp-buffer
		      (insert-file-contents file)
		      (buffer-string))))
      (should-not (string-match-p "\\_<hub/" contents))
      (should-not (string-match-p "\\_<hub-" contents)))))

(provide 'org-marginalia-namespace-test)
;;; org-marginalia-namespace-test.el ends here
