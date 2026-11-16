;;; org-google-docs-disposable-preflight.el --- Isolated live-check plans -*- lexical-binding: t; -*-

;;; Commentary:
;; Batch-only driver.  All authentication and HTTP remain outside this process.
;; Supply existing synthetic source/build/artifact paths, never credentials.

;;; Code:

(unless noninteractive
  (user-error "Run disposable preflight in a separate batch Emacs"))
(let* ((sdk (or (getenv "GDOCS_PATH") (error "GDOCS_PATH is required")))
       (build (or (getenv "STRAIGHT_BUILD_PATH") (error "STRAIGHT_BUILD_PATH is required")))
       (artifacts (or (getenv "GDOCS_DISPOSABLE_ARTIFACTS") (error "Artifact directory is required")))
       (packages (expand-file-name "../.." (file-name-directory load-file-name)))
       (source (or (getenv "GDOCS_DISPOSABLE_SOURCE")
		   (expand-file-name "test/fixtures/disposable-list-conformance.org" sdk)))
       (remote (getenv "GDOCS_DISPOSABLE_REMOTE"))
       (action (or (getenv "GDOCS_DISPOSABLE_ACTION") (if remote "inspect" "create"))))
  (unless (member action '("create" "inspect" "remote-edit" "local-edit" "repeat-edit" "export"))
    (error "Unknown disposable preflight action"))
  (dolist (directory (list sdk build artifacts))
    (unless (file-directory-p directory)
      (error "Missing explicit disposable test directory")))
  (dolist (name '("org" "compat" "cond-let" "llama" "transient" "dash" "s" "plz"
		  "magit-section" "context-panels"))
    (add-to-list 'load-path (expand-file-name name build)))
  (dolist (name '("org-google-docs" "org-comments" "ox-sync"))
    (add-to-list 'load-path (expand-file-name name packages)))
  (add-to-list 'load-path sdk)
  (add-to-list 'load-path (expand-file-name "test" sdk))
  (require 'gdocs-disposable-plan)
  (require 'org-google-docs)
  (when (member action '("create" "inspect" "remote-edit"))
    (with-temp-buffer
      (insert-file-contents source)
      (org-mode)
      (let ((trace (org-google-docs-debug-pipeline))
	    (destination (make-temp-file (expand-file-name "local-pipeline-" artifacts) nil ".org")))
	(with-current-buffer trace
	  (write-region (point-min) (point-max) destination nil 'silent)))))
  (prin1
   (pcase action
     ("create" (gdocs-disposable-plan-create source artifacts))
     ("inspect" (gdocs-disposable-plan-inspect source remote artifacts))
     ("remote-edit" (gdocs-disposable-plan-remote-edit remote source artifacts))
     ("local-edit" (gdocs-disposable-plan-local-edit source artifacts))
     ("repeat-edit" (gdocs-disposable-plan-repeat-edit source artifacts))
     ("export" (gdocs-disposable-plan-export-fixture
		remote (or (getenv "GDOCS_DISPOSABLE_EXPORT") (error "Fixture destination is required"))))))
  (terpri))

;;; org-google-docs-disposable-preflight.el ends here
