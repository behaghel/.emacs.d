;;; architecture-lint-test.el --- Architecture lint regression tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Exercise the real lint entrypoint in isolated tracked fixture repositories.

;;; Code:

(require 'ert)
(require 'seq)

(defconst hub/architecture-lint-test--root
  (locate-dominating-file (or load-file-name buffer-file-name) "domains.yaml")
  "Repository containing the architecture lint entrypoint.")

(defun hub/architecture-lint-test--run (files)
  "Run architecture lint against tracked FILES, an alist of paths and contents.
Return a list containing the process exit status and its diagnostic output."
  (let* ((root (make-temp-file "architecture-lint-" t))
	 (default-directory (file-name-as-directory root))
	 ;; Commit hooks export repository-local Git variables.  Never let
	 ;; fixture commands read or write the invoking repository's index.
	 (process-environment
	  (seq-remove (lambda (entry) (string-prefix-p "GIT_" entry))
		      process-environment)))
    (unwind-protect
	(progn
	  (dolist (entry (cons '("lisp/hub-utils.el" .
				 "(defun hub/shared-helper () nil)\n")
			       files))
	    (let ((path (expand-file-name (car entry) root)))
	      (make-directory (file-name-directory path) t)
	      (with-temp-file path (insert (cdr entry)))))
	  (unless (and (zerop (process-file "git" nil nil nil "init" "--quiet"))
		       (zerop (process-file "git" nil nil nil "add" ".")))
	    (error "Could not prepare tracked lint fixtures"))
	  (with-temp-buffer
	    (let ((status
		   (process-file
		    (expand-file-name invocation-name invocation-directory)
		    nil t nil "--batch" "-Q"
		    "--eval" (format "(setq user-emacs-directory %S)"
				     default-directory)
		    "-l" (expand-file-name "scripts/lint-architecture.el"
					   hub/architecture-lint-test--root))))
	      (list status (buffer-string)))))
      (delete-directory root t))))

(ert-deftest hub/architecture-lint-isolates-hook-index ()
  "Fixture repositories do not touch an index inherited from a commit hook."
  (let ((index (make-temp-file "architecture-lint-foreign-index-"))
	(process-environment (copy-sequence process-environment)))
    (unwind-protect
	(progn
	  (setenv "GIT_INDEX_FILE" index)
	  (should (zerop (car (hub/architecture-lint-test--run nil))))
	  (should (zerop (file-attribute-size (file-attributes index)))))
      (delete-file index))))

(ert-deftest hub/architecture-lint-allows-module-owned-hub-names ()
  "Defining and calling module-local names does not imply a shared dependency."
  (should
   (zerop (car (hub/architecture-lint-test--run
		'(("modules/example/core.el" .
		   "(defun hub/local-helper () nil)\n(hub/local-helper)\n")))))))

(ert-deftest hub/architecture-lint-rejects-missing-shared-helper-import ()
  "A shared helper reference needs a real import."
  (let ((result (hub/architecture-lint-test--run
		 '(("modules/example/core.el" . "(hub/shared-helper)\n")))))
    (should-not (zerop (car result)))
    (should (string-match-p "hub-utils" (cadr result)))))

(ert-deftest hub/architecture-lint-allows-declared-shared-helper-import ()
  "A real shared-helper import satisfies the dependency check."
  (should
   (zerop (car (hub/architecture-lint-test--run
		'(("modules/example/core.el" .
		   "(require 'hub-utils)\n(hub/shared-helper)\n")))))))

(ert-deftest hub/architecture-lint-ignores-commented-import ()
  "A commented import cannot hide a missing dependency."
  (should-not
   (zerop (car (hub/architecture-lint-test--run
		'(("modules/example/core.el" .
		   ";; (require 'hub-utils)\n(hub/shared-helper)\n")))))))

(ert-deftest hub/architecture-lint-ignores-prose-helper-mentions ()
  "Comments and strings do not create executable dependencies."
  (should
   (zerop (car (hub/architecture-lint-test--run
		'(("modules/example/core.el" .
		   ";; hub/shared-helper\n(defvar description \"hub/shared-helper\")\n")))))))

(ert-deftest hub/architecture-lint-allows-nested-domain-tests ()
  "Tests beneath domain directories are not mistaken for flat tests."
  (should
   (zerop (car (hub/architecture-lint-test--run
		'(("test/example/nested-test.el" . "(require 'ert)\n")))))))

(ert-deftest hub/architecture-lint-rejects-flat-tests ()
  "Tests directly beneath test/ remain forbidden."
  (let ((result (hub/architecture-lint-test--run
		 '(("test/flat-test.el" . "(require 'ert)\n")))))
    (should-not (zerop (car result)))
    (should (string-match-p "flat tests found" (cadr result)))))

(ert-deftest hub/architecture-lint-rejects-invalid-reader-syntax ()
  "Unreadable source fails rather than bypassing dependency checks."
  (should-not
   (zerop (car (hub/architecture-lint-test--run
		'(("modules/example/core.el" . "(unclosed\n")))))))

(provide 'architecture-lint-test)
;;; architecture-lint-test.el ends here
