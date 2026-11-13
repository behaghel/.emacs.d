;;; org-confluence-test-support.el --- Fail-closed offline fixtures -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests run in a dedicated batch process.  Keep default credentials and runtime
;; caches away from the user's files, and reject unmocked I/O even when production
;; best-effort enrichment catches ordinary errors.  Explicit transport/config
;; fixtures remain available to tests that exercise those boundaries.

;;; Code:

(unless noninteractive
  (user-error "Run Confluence tests in a separate batch Emacs"))

(defconst org-confluence-test--root
  (make-temp-file "org-confluence-test-state-" t)
  "Owned temporary home and runtime-state root for this batch process.")

(defconst org-confluence-test--missing-config
  (expand-file-name "unconfigured-cfl.yml" org-confluence-test--root)
  "Reserved config path that must never be read without a fixture.")

(defvar org-confluence-api-cfl-config-file)
(defvar org-confluence-people-store-directory)

(setenv "HOME" org-confluence-test--root)
(setq user-emacs-directory (file-name-as-directory org-confluence-test--root)
      org-confluence-api-cfl-config-file org-confluence-test--missing-config
      org-confluence-people-store-directory
      (expand-file-name "people" org-confluence-test--root))

;; Deliberately not a subtype of `error': enrichment must not swallow this guard.
(put 'org-confluence-test-unexpected-io 'error-conditions
     '(org-confluence-test-unexpected-io))
(put 'org-confluence-test-unexpected-io 'error-message
     "Unmocked Confluence test I/O")

(defun org-confluence-test--unexpected-io (kind)
  "Reject unmocked I/O of KIND without printing request or credential data."
  (message "Unmocked Confluence test I/O: %s" kind)
  (signal 'org-confluence-test-unexpected-io (list kind)))

(defun org-confluence-test--deny-network (&rest _arguments)
  "Reject an unmocked network request without exposing request arguments."
  (org-confluence-test--unexpected-io 'network))

(defun org-confluence-test--deny-process (&rest _arguments)
  "Reject an unmocked external process without exposing command arguments."
  (org-confluence-test--unexpected-io 'process))

(defun org-confluence-test--guard-config (original &optional file)
  "Call ORIGINAL with FILE only for an explicit configuration fixture."
  (when (equal (or file org-confluence-api-cfl-config-file)
	       org-confluence-test--missing-config)
    (org-confluence-test--unexpected-io 'credentials))
  (funcall original file))

(advice-add 'make-network-process :override #'org-confluence-test--deny-network)
(advice-add 'process-file :override #'org-confluence-test--deny-process)
(with-eval-after-load 'org-confluence-api
  (advice-add 'org-confluence-api--read-cfl-config :around
	      #'org-confluence-test--guard-config))

(defun org-confluence-test--cleanup ()
  "Remove this batch process's owned temporary state."
  (delete-directory org-confluence-test--root t))

(add-hook 'kill-emacs-hook #'org-confluence-test--cleanup)

(provide 'org-confluence-test-support)
;;; org-confluence-test-support.el ends here
