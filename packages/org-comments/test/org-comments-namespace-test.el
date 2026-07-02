;;; org-comments-namespace-test.el --- Namespace tests for org-comments package -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests that the reusable package does not expose personal symbols.

;;; Code:

(require 'ert)
(require 'seq)

(ert-deftest org-comments-namespace-package-has-no-hub-symbols ()
  "Package source files should not contain personal namespace symbols."
  (let* ((root (file-name-directory (locate-library "org-comments")))
	 (files (directory-files root t "\\.el\\'"))
	 offenders)
    (dolist (file files)
      (with-temp-buffer
	(insert-file-contents file)
	(when (search-forward (concat "hub" "/") nil t)
	  (push (file-name-nondirectory file) offenders))))
    (should-not (nreverse offenders))))

(provide 'org-comments-namespace-test)
;;; org-comments-namespace-test.el ends here
