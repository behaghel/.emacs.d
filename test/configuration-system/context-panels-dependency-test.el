;;; context-panels-dependency-test.el --- Context dependency regression tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Exercise package setup and the real test runner without network access or
;; relying on the developer's context-panels checkout.

;;; Code:

(require 'ert)

(defconst hub/context-dependency-test--root
  (expand-file-name
   (locate-dominating-file (or load-file-name buffer-file-name) "domains.yaml"))
  "Repository containing package setup and the ERT runner.")

(defun hub/context-dependency-test--write (root path contents)
  "Write CONTENTS to PATH beneath fixture ROOT."
  (let ((file (expand-file-name path root)))
    (make-directory (file-name-directory file) t)
    (with-temp-file file (insert contents))))

(defun hub/context-dependency-test--bootstrap (override)
  "Run package setup with OVERRIDE and a recording Straight fixture.
Return the exit status and output without installing packages."
  (let* ((root (make-temp-file "context-source-" t))
	 (process-environment (copy-sequence process-environment)))
    (unwind-protect
	(progn
	  (setenv "CONTEXT_PANELS_PATH" override)
	  (hub/context-dependency-test--write
	   root "straight/repos/straight.el/bootstrap.el"
	   (concat
	    "(defun straight-use-package (recipe &rest _args)\n"
	    "  (princ (format \"RECIPE:%S\\n\" recipe)))\n"
	    "(dolist (feature '(use-package use-package-ensure\n"
	    "                   use-package-delight use-package-diminish))\n"
	    "  (provide feature))\n"))
	  (with-temp-buffer
	    (let ((status
		   (call-process
		    (expand-file-name invocation-name invocation-directory)
		    nil t nil "--batch" "-Q"
		    "-L" (expand-file-name "core" hub/context-dependency-test--root)
		    "--eval" (format "(setq user-emacs-directory %S)" root)
		    "-l" (expand-file-name "core/core-packages.el"
					   hub/context-dependency-test--root)
		    "--eval" "(require 'context-panels nil t)")))
	      (list status (buffer-string)))))
      (delete-directory root t))))

(ert-deftest hub/context-dependency-default-source-is-declared-and-pinned ()
  "An unset or empty override selects a published, pinned dependency."
  (dolist (override '(nil ""))
    (pcase-let ((`(,status ,output)
		 (hub/context-dependency-test--bootstrap override)))
      (should (equal status 0))
      (should (string-match-p
	       (regexp-quote
		"RECIPE:(context-panels :type git :host github :repo \"behaghel/context-panels\")")
	       output))))
  (let* ((versions
	  (with-temp-buffer
	    (insert-file-contents
	     (expand-file-name "straight/versions/default.el"
			       hub/context-dependency-test--root))
	    (read (current-buffer))))
	 (pin (cdr (assoc "context-panels" versions))))
    (should (stringp pin))
    (should (string-match-p "\\`[[:xdigit:]]\\{40\\}\\'" pin))))

(ert-deftest hub/context-dependency-development-source-is-explicit ()
  "An explicit checkout loads local code without registering a remote recipe."
  (let ((directory (make-temp-file "context-development-" t)))
    (unwind-protect
	(progn
	  (hub/context-dependency-test--write
	   directory "context-panels.el"
	   "(princ \"LOCAL-CONTEXT-LOADED\\n\")\n(provide 'context-panels)\n")
	  (hub/context-dependency-test--write
	   directory "context-panels-org.el" "(provide 'context-panels-org)\n")
	  (pcase-let ((`(,status ,output)
		       (hub/context-dependency-test--bootstrap directory)))
	    (should (equal status 0))
	    (should (string-match-p "^LOCAL-CONTEXT-LOADED$" output))
	    (should-not (string-match-p "RECIPE:(context-panels " output))))
      (delete-directory directory t))))

(ert-deftest hub/context-dependency-invalid-development-source-fails ()
  "Invalid overrides fail clearly rather than falling back to remote code."
  (let ((directory (make-temp-file "context-invalid-" t)))
    (unwind-protect
	(pcase-let ((`(,status ,output)
		     (hub/context-dependency-test--bootstrap directory)))
	  (should (integerp status))
	  (should-not (zerop status))
	  (should (string-match-p "CONTEXT_PANELS_PATH" output)))
      (delete-directory directory t))))

(defun hub/context-dependency-test--runner (source)
  "Run the real ERT runner using fixture SOURCE, returning status and output.
SOURCE is `managed', `development', or `invalid'."
  (let* ((root (make-temp-file "context-runner-" t))
	 (default-directory (file-name-as-directory root))
	 (process-environment (copy-sequence process-environment))
	 (directory (expand-file-name
		     (if (eq source 'managed)
			 "straight/build/context-panels"
		       "development/context-panels")
		     root)))
    (unwind-protect
	(progn
	  (make-directory (expand-file-name "packages" root) t)
	  (setenv "HOME" root)
	  (setenv "STRAIGHT_BUILD_PATH" nil)
	  (setenv "CONTEXT_PANELS_PATH"
		  (unless (eq source 'managed) directory))
	  (unless (eq source 'invalid)
	    (hub/context-dependency-test--write
	     directory "context-panels.el"
	     (concat "(defvar hub/context-dependency-fixture-loaded t)\n"
		     "(provide 'context-panels)\n"))
	    (hub/context-dependency-test--write
	     directory "context-panels-org.el" "(provide 'context-panels-org)\n"))
	  (hub/context-dependency-test--write root "test/test-helpers.el" "")
	  (hub/context-dependency-test--write
	   root "test/example/context-test.el"
	   (concat "(require 'context-panels)\n"
		   "(ert-deftest hub/context-fixture ()\n"
		   "  (should (bound-and-true-p hub/context-dependency-fixture-loaded)))\n"))
	  (with-temp-buffer
	    (let ((status
		   (call-process
		    "bash" nil t nil
		    (expand-file-name "scripts/elisp-ert"
				      hub/context-dependency-test--root)
		    "test/example/context-test.el")))
	      (list status (buffer-string)))))
      (delete-directory root t))))

(ert-deftest hub/context-dependency-runner-uses-managed-build ()
  "Tests load the managed dependency without a developer checkout."
  (pcase-let ((`(,status ,output)
	       (hub/context-dependency-test--runner 'managed)))
    (should (equal status 0))
    (should (string-match-p "passed.*hub/context-fixture" output))))

(ert-deftest hub/context-dependency-runner-uses-explicit-checkout ()
  "Tests honor the same development override as package setup."
  (pcase-let ((`(,status ,output)
	       (hub/context-dependency-test--runner 'development)))
    (should (equal status 0))
    (should (string-match-p "passed.*hub/context-fixture" output))))

(ert-deftest hub/context-dependency-runner-rejects-invalid-override ()
  "Tests diagnose an invalid development override before starting Emacs."
  (pcase-let ((`(,status ,output)
	       (hub/context-dependency-test--runner 'invalid)))
    (should (integerp status))
    (should-not (zerop status))
    (should (string-match-p "CONTEXT_PANELS_PATH" output))))

(provide 'context-panels-dependency-test)
;;; context-panels-dependency-test.el ends here
