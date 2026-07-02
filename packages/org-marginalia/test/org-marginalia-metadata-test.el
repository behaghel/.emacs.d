;;; org-marginalia-metadata-test.el --- Metadata tests for org-marginalia -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for package header metadata that standalone package tooling expects.

;;; Code:

(require 'ert)
(require 'lisp-mnt)

(defconst org-marginalia-metadata-test--files
  '("org-marginalia.el" "org-marginalia-context-panel.el")
  "Package files that should expose standard metadata headers.")

(defun org-marginalia-metadata-test--root ()
  "Return the package root directory."
  (file-name-directory (or (locate-library "org-marginalia")
			   load-file-name
			   buffer-file-name)))

(defun org-marginalia-metadata-test--file (name)
  "Return absolute path for package file NAME."
  (expand-file-name name (org-marginalia-metadata-test--root)))

(ert-deftest org-marginalia-metadata-has-standard-package-headers ()
  "Package files include standard package metadata headers."
  (dolist (name org-marginalia-metadata-test--files)
    (let ((file (org-marginalia-metadata-test--file name)))
      (with-temp-buffer
	(insert-file-contents file)
	(should (lm-summary))
	(should (lm-header "Author"))
	(should (lm-header "Maintainer"))
	(should (lm-header "Version"))
	(should (lm-header "Package-Requires"))
	(should (lm-keywords))
	(should (lm-homepage))))))

(provide 'org-marginalia-metadata-test)
;;; org-marginalia-metadata-test.el ends here
