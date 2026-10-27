;;; cp-org-marginalia-metadata-test.el --- Metadata tests for cp-org-marginalia -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for package header metadata that standalone package tooling expects.

;;; Code:

(require 'ert)
(require 'lisp-mnt)

(defconst cp-org-marginalia-metadata-test--files
  '("cp-org-marginalia.el" "cp-org-marginalia-context-panel.el")
  "Package files that should expose standard metadata headers.")

(defun cp-org-marginalia-metadata-test--root ()
  "Return the package root directory."
  (file-name-directory (or (locate-library "cp-org-marginalia")
			   load-file-name
			   buffer-file-name)))

(defun cp-org-marginalia-metadata-test--file (name)
  "Return absolute path for package file NAME."
  (expand-file-name name (cp-org-marginalia-metadata-test--root)))

(ert-deftest cp-org-marginalia-metadata-has-standard-package-headers ()
  "Package files include standard package metadata headers."
  (dolist (name cp-org-marginalia-metadata-test--files)
    (let ((file (cp-org-marginalia-metadata-test--file name)))
      (with-temp-buffer
	(insert-file-contents file)
	(should (lm-summary))
	(should (lm-header "Author"))
	(should (lm-header "Maintainer"))
	(should (lm-header "Version"))
	(should (lm-header "Package-Requires"))
	(should (lm-keywords))
	(should (lm-homepage))))))

(provide 'cp-org-marginalia-metadata-test)
;;; cp-org-marginalia-metadata-test.el ends here
