;;; cp-org-marginalia-namespace-test.el --- Namespace tests for cp-org-marginalia -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests that the standalone cp-org-marginalia package does not expose legacy
;; personal namespace symbols.

;;; Code:

(require 'ert)
(require 'subr-x)

(defun cp-org-marginalia-namespace-test--root ()
  "Return the package root directory."
  (file-name-directory (or (locate-library "cp-org-marginalia")
			   load-file-name
			   buffer-file-name)))

(defun cp-org-marginalia-namespace-test--elisp-files ()
  "Return package Elisp files excluding tests."
  (directory-files (cp-org-marginalia-namespace-test--root) t "\\.el\\'"))

(ert-deftest cp-org-marginalia-namespace-has-no-hub-symbols ()
  "Package code does not contain legacy hub symbols.
The literal Org metadata property names `HUB_NOTE_*' are intentionally allowed."
  (dolist (file (cp-org-marginalia-namespace-test--elisp-files))
    (let ((contents (with-temp-buffer
		      (insert-file-contents file)
		      (buffer-string))))
      (should-not (string-match-p "\\_<hub/" contents))
      (should-not (string-match-p "\\_<hub-" contents)))))

(provide 'cp-org-marginalia-namespace-test)
;;; cp-org-marginalia-namespace-test.el ends here
