;;; org-google-docs-disposable-preflight.el --- Isolated live-check plans -*- lexical-binding: t; -*-

;;; Commentary:
;; Batch-only driver.  All authentication and HTTP remain outside this process.
;; Supply existing synthetic source/build/artifact paths, never credentials.

;;; Code:

(defvar gdocs-style-definitions)

(unless noninteractive
  (user-error "Run disposable preflight in a separate batch Emacs"))
(let* ((gdocs-source (or (getenv "GDOCS_PATH") (error "GDOCS_PATH is required")))
       (build (or (getenv "STRAIGHT_BUILD_PATH") (error "STRAIGHT_BUILD_PATH is required")))
       (artifacts (or (getenv "GDOCS_DISPOSABLE_ARTIFACTS") (error "Artifact directory is required")))
       (packages (expand-file-name "../.." (file-name-directory load-file-name)))
       (source (or (getenv "GDOCS_DISPOSABLE_SOURCE")
		   (expand-file-name "test/fixtures/disposable-list-conformance.org" gdocs-source)))
       (remote (getenv "GDOCS_DISPOSABLE_REMOTE"))
       (action (or (getenv "GDOCS_DISPOSABLE_ACTION") (if remote "inspect" "create"))))
  (unless (member action '("create" "inspect" "remote-edit" "local-edit" "repeat-edit" "style-repair" "export"
			   "coverage-create" "coverage-native" "coverage-fill" "coverage-inspect" "coverage-verify"))
    (error "Unknown disposable preflight action"))
  (dolist (directory (list gdocs-source build artifacts))
    (unless (file-directory-p directory)
      (error "Missing explicit disposable test directory")))
  (dolist (name '("org" "compat" "cond-let" "llama" "transient" "dash" "s" "plz"
		  "magit-section" "context-panels"))
    (add-to-list 'load-path (expand-file-name name build)))
  (dolist (name '("org-google-docs" "org-comments" "ox-sync"))
    (add-to-list 'load-path (expand-file-name name packages)))
  (add-to-list 'load-path (expand-file-name "../modules" packages))
  (add-to-list 'load-path gdocs-source)
  (add-to-list 'load-path (expand-file-name "test" gdocs-source))
  (add-to-list 'load-path (file-name-directory load-file-name))
  (require 'gdocs-disposable-plan)
  (require 'org-google-docs)
  (require 'org/google-docs-styles)
  (let ((gdocs-style-definitions
	 (append (with-temp-buffer
		   (insert-file-contents source)
		   (org-mode)
		   (hub/org-google-docs-style-definitions
		    (when-let* ((theme (getenv "GDOCS_DISPOSABLE_THEME"))) (intern theme))))
		 gdocs-style-definitions)))
    (when (member action '("create" "inspect" "remote-edit" "style-repair"))
      (with-temp-buffer
	(insert-file-contents source)
	(org-mode)
	(let ((trace (org-google-docs-debug-pipeline))
	      (destination (make-temp-file (expand-file-name "local-pipeline-" artifacts) nil ".org")))
	  (with-current-buffer trace
	    (write-region (point-min) (point-max) destination nil 'silent)))))
    (prin1
     (pcase action
       ((pred (string-prefix-p "coverage-"))
	(require 'org-google-docs-disposable-coverage)
	(org-google-docs-disposable-coverage-plan
	 (string-remove-prefix "coverage-" action) source remote artifacts
	 (getenv "GDOCS_DISPOSABLE_REPLY") (getenv "GDOCS_DISPOSABLE_IMAGE_URI")))
       ("create" (gdocs-disposable-plan-create source artifacts))
       ("inspect" (gdocs-disposable-plan-inspect source remote artifacts))
       ("remote-edit" (gdocs-disposable-plan-remote-edit remote source artifacts))
       ("local-edit" (gdocs-disposable-plan-local-edit source artifacts))
       ("repeat-edit" (gdocs-disposable-plan-repeat-edit source artifacts))
       ("style-repair" (gdocs-disposable-plan-style-repair source remote artifacts))
       ("export" (gdocs-disposable-plan-export-fixture
		  remote (or (getenv "GDOCS_DISPOSABLE_EXPORT") (error "Fixture destination is required"))))))
    (terpri)))

;;; org-google-docs-disposable-preflight.el ends here
