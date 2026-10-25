;;; google-docs-dependency-test.el --- Google Docs source tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Load the real personal configuration in a subprocess with only package
;; registration, adapter loading and use-package activation stubbed.  No OAuth
;; credentials, package downloads or Google Docs mutations are needed.

;;; Code:

(require 'ert)
(require 'subr-x)

(defconst hub/gdocs-dependency-test--root
  (expand-file-name
   (locate-dominating-file (or load-file-name buffer-file-name) "domains.yaml"))
  "Repository root for Google Docs dependency tests.")

(defun hub/gdocs-dependency-test--load (override)
  "Load personal configuration with GDOCS_PATH set to OVERRIDE.
Return the subprocess status and output."
  (let ((process-environment (copy-sequence process-environment)))
    (setenv "GDOCS_PATH" override)
    (with-temp-buffer
      (let* ((form
	      `(progn
		 (require 'cl-lib)
		 (defvar org-google-docs-gdocs-repository nil)
		 (defun hub/org-google-docs-style-definitions () nil)
		 (defun straight-use-package (recipe)
		   (princ (format "RECIPE:%S\n" recipe)))
		 (defun org-google-docs-ensure-gdocs-loaded (&optional _noerror)
		   (princ (format "SOURCE:%S\n" org-google-docs-gdocs-repository)))
		 (defmacro use-package (&rest _args) nil)
		 (let ((original-require (symbol-function 'require)))
		   (cl-letf (((symbol-function 'require)
			      (lambda (feature &rest args)
				(if (memq feature '(hub-utils org/google-docs-styles
							      org-google-docs))
				    t
				  (apply original-require feature args)))))
		     (load ,(expand-file-name
			     "modules/interactive/org/google-docs.el"
			     hub/gdocs-dependency-test--root) nil t)))))
	     (status
	      (call-process (expand-file-name invocation-name invocation-directory)
			    nil t nil "--batch" "-Q" "--eval"
			    (prin1-to-string form))))
	(list status (buffer-string))))))

(ert-deftest hub/gdocs-default-source-is-the-pinned-semantic-fork ()
  "No local checkout is needed to select the semantic-capable gdocs source."
  (pcase-let ((`(,status ,output) (hub/gdocs-dependency-test--load nil)))
    (should (equal status 0))
    (should (string-match-p
	     (regexp-quote "RECIPE:(gdocs :type git :host github :repo \"behaghel/gdocs\" :branch \"org-image-seam\")")
	     output))
    (should (string-match-p "^SOURCE:nil$" output)))
  (let* ((versions
	  (with-temp-buffer
	    (insert-file-contents
	     (expand-file-name "straight/versions/default.el"
			       hub/gdocs-dependency-test--root))
	    (read (current-buffer))))
	 (pin (cdr (assoc "gdocs" versions))))
    (should (stringp pin))
    (should (string-match-p "\\`[[:xdigit:]]\\{40\\}\\'" pin))))

(ert-deftest hub/gdocs-local-checkout-is-an-explicit-override ()
  "A local source uses Straight's local recipe so its dependencies are available."
  (let ((directory (make-temp-file "hub-gdocs-development-" t)))
    (unwind-protect
	(progn
	  (with-temp-file (expand-file-name "gdocs.el" directory)
	    (insert "(provide 'gdocs)\n"))
	  (pcase-let ((`(,status ,output) (hub/gdocs-dependency-test--load directory)))
	    (should (equal status 0))
	    (should (string-match-p
		     (regexp-quote (format "SOURCE:%S" directory)) output))
	    (should (string-match-p
		     (regexp-quote
		      (format "RECIPE:(gdocs :type nil :local-repo %S)" directory))
		     output))
	    (should-not (string-match-p ":repo \"behaghel/gdocs\"" output))))
      (delete-directory directory t))))

(ert-deftest hub/gdocs-invalid-local-override-does-not-fall-back ()
  "An explicit directory without gdocs.el fails before package registration."
  (let ((directory (make-temp-file "hub-gdocs-invalid-" t)))
    (unwind-protect
	(pcase-let ((`(,status ,output) (hub/gdocs-dependency-test--load directory)))
	  (should (integerp status))
	  (should-not (zerop status))
	  (should (string-match-p "GDOCS_PATH" output))
	  (should-not (string-match-p "^RECIPE:(gdocs " output)))
      (delete-directory directory t))))

(ert-deftest hub/gdocs-style-fixtures-use-the-managed-build ()
  "Style tests load the managed converter without an implicit home checkout."
  (let* ((root (make-temp-file "hub-gdocs-style-source-" t))
	 (default-directory (file-name-as-directory root))
	 (process-environment (copy-sequence process-environment)))
    (unwind-protect
	(progn
	  (setenv "HOME" root)
	  (setenv "GDOCS_PATH" nil)
	  (setenv "STRAIGHT_BUILD_PATH" nil)
	  (make-directory (expand-file-name "modules/org" root) t)
	  (dolist (file '("modules/org/google-docs-themes.el"
			  "modules/org/google-docs-styles.el"))
	    (copy-file (expand-file-name file hub/gdocs-dependency-test--root)
		       (expand-file-name file root)))
	  (let ((converter (expand-file-name "straight/build/gdocs/gdocs-convert.el" root)))
	    (make-directory (file-name-directory converter) t)
	    (with-temp-file converter
	      (insert "(princ \"MANAGED-CONVERTER-LOADED\\n\")\n"
		      "(provide 'gdocs-convert)\n")))
	  (with-temp-buffer
	    (let ((status
		   (call-process
		    (expand-file-name invocation-name invocation-directory)
		    nil t nil "--batch" "-Q"
		    "-l" (expand-file-name
			  "test/authoring/documents/org-google-docs-styles-test.el"
			  hub/gdocs-dependency-test--root)
		    "--eval" "(unless (featurep 'gdocs-convert) (error \"Managed converter was not loaded\"))")))
	      (should (equal status 0))
	      (should (string-match-p "^MANAGED-CONVERTER-LOADED$" (buffer-string))))))
      (delete-directory root t))))

(provide 'google-docs-dependency-test)
;;; google-docs-dependency-test.el ends here
