;;; org-confluence-demo.el --- Offline Confluence conversion walkthrough -*- lexical-binding: t; -*-

;;; Commentary:
;; Explicit demo activation in the disposable launcher only.  Real conversion
;; commands remain available while remote operations and credential reads fail
;; closed.  This file is not loaded by the reusable package.

;;; Code:

(require 'org-confluence)

(defvar org-confluence-demo--source-buffer nil
  "Source buffer explicitly selected for this standalone session.")

(defun org-confluence-demo--disabled (&rest _args)
  "Refuse remote operations in the offline demonstration."
  (user-error "Offline walkthrough: remote operations and credential access are disabled"))

(defun org-confluence-demo-preview ()
  "Export the demo source into the standard Storage XHTML preview."
  (interactive)
  (unless (buffer-live-p org-confluence-demo--source-buffer)
    (user-error "Start the standalone Confluence demo first"))
  (let ((buffer (with-current-buffer org-confluence-demo--source-buffer
		  (org-confluence-export-as-xhtml nil nil nil t nil))))
    (pop-to-buffer buffer)
    buffer))

(defun org-confluence-demo-roundtrip ()
  "Import the last Storage preview into a separate read-only Org buffer."
  (interactive)
  (unless (buffer-live-p org-confluence-demo--source-buffer)
    (user-error "Start the standalone Confluence demo first"))
  (let* ((storage (or (get-buffer "*Org Confluence Export*")
		      (org-confluence-demo-preview)))
	 (directory (buffer-local-value 'default-directory org-confluence-demo--source-buffer))
	 (text (with-current-buffer storage
		 (org-confluence-import-storage-to-org (buffer-string) directory)))
	 (buffer (get-buffer-create "*Org Confluence Round Trip*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
	(erase-buffer)
	(insert text)
	(org-mode)
	(goto-char (point-min)))
      (setq buffer-read-only t))
    (pop-to-buffer buffer)
    buffer))

(defun org-confluence-demo-check-cli ()
  "Report the missing-CLI diagnostic without executing a process or moving point."
  (interactive)
  (save-window-excursion
    (save-excursion
      (let ((org-confluence-api-cfl-command "org-confluence-standalone-missing-cfl"))
	(condition-case error
	    (org-confluence-process-run-output
	     (org-confluence-api--page-view-storage-command "offline-demo"))
	  (user-error (message "%s" (error-message-string error))))))))

(defun org-confluence-demo-start ()
  "Activate the offline walkthrough for the current disposable Org source."
  (interactive)
  (unless (and (derived-mode-p 'org-mode) buffer-file-name)
    (user-error "Visit the disposable Org source before starting the demo"))
  (setq org-confluence-demo--source-buffer (current-buffer))
  (setq org-export-use-babel nil)
  (setq org-confluence-api--rest-transport #'org-confluence-demo--disabled)
  (dolist (function '(org-confluence-api--read-cfl-config
		      org-confluence-process-run-process browse-url))
    (advice-add function :override #'org-confluence-demo--disabled))
  (org-confluence-mode 1)
  (message "Offline Confluence demo: preview and round-trip commands are ready"))

(provide 'org-confluence-demo)
;;; org-confluence-demo.el ends here
