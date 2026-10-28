;;; package-docs-test.el --- Package reference generation regression -*- lexical-binding: t; -*-

;;; Commentary:
;; Verify the documented Confluence reference-generation command in batch Emacs,
;; with cached dependency source paths but without the personal initialization.

;;; Code:

(require 'ert)

(defconst hub/package-docs-test--root
  (file-name-as-directory
   (locate-dominating-file (or load-file-name buffer-file-name) "domains.yaml"))
  "Repository containing the package documentation generator.")

(ert-deftest hub/confluence-docs-load-declared-source-dependencies ()
  "Reference generation resolves the facade's cached dependencies in clean Emacs."
  (let ((output (make-temp-file "confluence-package-docs-" t))
	(default-directory hub/package-docs-test--root)
	(process-environment (copy-sequence process-environment)))
    (unwind-protect
	(progn
	  (setenv "CONTEXT_PANELS_PATH" (file-name-directory (locate-library "context-panels")))
	  (with-temp-buffer
	    (let ((status (process-file
			   (expand-file-name "scripts/elisp-package-docs" hub/package-docs-test--root)
			   nil (list t t) nil
			   "--load" "packages/org-confluence/org-confluence.el"
			   "--prefix" "org-confluence-" "--out" output)))
	      (ert-info ((buffer-string)) (should (zerop status)))))
	  (should (file-exists-p (expand-file-name "config-reference.org" output)))
	  (with-temp-buffer
	    (insert-file-contents (expand-file-name "commands.org" output))
	    (should (search-forward "=org-confluence-mode=" nil t))))
      (delete-directory output t))))

(provide 'package-docs-test)
;;; package-docs-test.el ends here
