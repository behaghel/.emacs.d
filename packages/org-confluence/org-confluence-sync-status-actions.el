;;; org-confluence-sync-status-actions.el --- Sync status actions -*- lexical-binding: t; -*-

;;; Commentary:
;; Commands that act from a Confluence sync status report buffer.

;;; Code:

(require 'button)
(require 'cl-lib)
(require 'magit-section nil 'noerror)
(require 'org)
(require 'org-comments)
(require 'org-comments-collaboration)
(require 'org-comments-sidecar)
(require 'transient nil 'noerror)

(require 'org-confluence-comments-diagnostics)
(require 'org-confluence-comments-import)
(require 'org-confluence-import)
(require 'org-confluence-inline-repair)
(require 'org-confluence-sync)
(require 'org-confluence-sync-status-collect)

(declare-function org-confluence-publish "org-confluence-publish" (&optional page-id subtreep visible-only body-only ext-plist))
(declare-function org-confluence-publish-force "org-confluence-publish" ())
(declare-function org-confluence-open-page "org-confluence-publish" ())
(declare-function org-confluence-sync-status-refresh "org-confluence-sync-status" ())
(defvar org-confluence-sync-status--source-buffer nil
  "Source Org buffer associated with the current Confluence sync status report.")
(defvar org-confluence-sync-status--last-source-buffer nil
  "Last source Org buffer associated with a Confluence sync status report.")

(defun org-confluence-sync-status--source-buffer-live-p (buffer)
  "Return non-nil when BUFFER is a live source buffer."
  (and (buffer-live-p buffer)
       (with-current-buffer buffer
	 (and (derived-mode-p 'org-mode)
	      buffer-file-name))))

(defun org-confluence-sync-status--linked-org-buffer-p (buffer)
  "Return non-nil when BUFFER looks like a Confluence-linked Org source."
  (and (org-confluence-sync-status--source-buffer-live-p buffer)
       (with-current-buffer buffer
	 (save-excursion
	   (goto-char (point-min))
	   (re-search-forward "^[ \\t]*#\\+CONFLUENCE_PAGE_ID:" nil t)))))

(defun org-confluence-sync-status--source-buffer-from-visible-report ()
  "Return a source buffer from any visible sync status report."
  (cl-loop for window in (window-list nil 'no-minibuf)
	   for buffer = (window-buffer window)
	   for source = (buffer-local-value
			 'org-confluence-sync-status--source-buffer buffer)
	   when (org-confluence-sync-status--source-buffer-live-p source)
	   return source))

(defun org-confluence-sync-status--resolve-source-buffer ()
  "Return the best available source buffer for a sync status action."
  (or (and (org-confluence-sync-status--source-buffer-live-p
	    org-confluence-sync-status--source-buffer)
	   org-confluence-sync-status--source-buffer)
      (org-confluence-sync-status--source-buffer-from-visible-report)
      (and (org-confluence-sync-status--linked-org-buffer-p (current-buffer))
	   (current-buffer))
      (and (org-confluence-sync-status--source-buffer-live-p
	    org-confluence-sync-status--last-source-buffer)
	   org-confluence-sync-status--last-source-buffer)))

(defun org-confluence-sync-status--with-source-buffer (function)
  "Run FUNCTION in this report's source buffer."
  (let ((source-buffer (org-confluence-sync-status--resolve-source-buffer)))
    (unless source-buffer
      (user-error "No source buffer for this sync status action"))
    (with-current-buffer source-buffer
      (funcall function))))

(defun org-confluence-sync-status-import-comments ()
  "Import Confluence comments for this sync status report's source buffer."
  (interactive)
  (org-confluence-sync-status--with-source-buffer
   (lambda ()
     (org-confluence-comments-import-footer)
     (org-confluence-comments-import-inline))))

(defun org-confluence-sync-status-reanchor-comments ()
  "Reanchor imported inline comments for this report's source buffer."
  (interactive)
  (org-confluence-sync-status--with-source-buffer
   (lambda ()
     (org-comments-anchor-imported-inline-comments))))

(defun org-confluence-sync-status-repair-anchors ()
  "Repair remote Confluence inline anchors for this report's source buffer."
  (interactive)
  (org-confluence-sync-status--with-source-buffer
   (lambda ()
     (org-confluence-inline-repair-comment-anchors))))

(defun org-confluence-sync-status-sync-page ()
  "Synchronize this report's source page content with Confluence."
  (interactive)
  (org-confluence-sync-status--with-source-buffer #'org-confluence-sync-page-current))

(defun org-confluence-sync-status-sync-current ()
  "Synchronize this report's source page content and comments with Confluence."
  (interactive)
  (org-confluence-sync-status--with-source-buffer #'org-confluence-sync-current))

(defun org-confluence-sync-status-pull-page ()
  "Pull this report's source page content from Confluence."
  (interactive)
  (org-confluence-sync-status--with-source-buffer #'org-confluence-pull))

(defun org-confluence-sync-status-pull-page-with-comments ()
  "Pull this report's source page content and comments from Confluence."
  (interactive)
  (org-confluence-sync-status--with-source-buffer
   (lambda ()
     (org-confluence-pull nil t))))

(defun org-confluence-sync-status-publish ()
  "Publish this report's source buffer to Confluence."
  (interactive)
  (org-confluence-sync-status--with-source-buffer #'org-confluence-publish))

(defun org-confluence-sync-status-force-publish ()
  "Force-publish this report's source buffer after confirmation."
  (interactive)
  (unless (yes-or-no-p "Force publish may orphan comments. Continue? ")
    (user-error "Canceled"))
  (org-confluence-sync-status--with-source-buffer #'org-confluence-publish-force))

(defun org-confluence-sync-status-open-page ()
  "Open this report's source Confluence page."
  (interactive)
  (org-confluence-sync-status--with-source-buffer #'org-confluence-open-page))

(defun org-confluence-sync-status-open-sidecar ()
  "Open this report's source sidecar comment file."
  (interactive)
  (org-confluence-sync-status--with-source-buffer #'org-comments-open-sidecar))

(defun org-confluence-sync-status-list-comments ()
  "List Confluence comments for this report's source buffer."
  (interactive)
  (org-confluence-sync-status--with-source-buffer #'org-confluence-comments-list))

(defun org-confluence-sync-status-resolve-people ()
  "Resolve Confluence people for this report's source directory."
  (interactive)
  (org-confluence-sync-status--with-source-buffer
   (lambda ()
     (org-confluence-people-resolve default-directory))))

(defun org-confluence-sync-status--section-issue ()
  "Return the sync issue represented by the Magit section at point."
  (when (and (featurep 'magit-section) (fboundp 'magit-current-section))
    (let ((section (magit-current-section))
	  issue)
      (while (and section (not issue))
	(when (eq (oref section type) 'sync-issue)
	  (setq issue (oref section value)))
	(setq section (oref section parent)))
      issue)))

(defun org-confluence-sync-status--line-button ()
  "Return a button on the current line, if any."
  (or (button-at (point))
      (save-excursion
	(beginning-of-line)
	(cl-loop while (< (point) (line-end-position))
		 for candidate = (button-at (point))
		 when candidate return candidate
		 do (goto-char (next-single-property-change
				(point) 'button nil (line-end-position)))))))

(defun org-confluence-sync-status-act-at-point ()
  "Act on the sync issue at point, Magit-section style."
  (interactive)
  (if-let* ((button (org-confluence-sync-status--line-button)))
      (push-button (button-start button))
    (if-let* ((issue (org-confluence-sync-status--section-issue))
	      (link (org-confluence-sync-status--string-value
		     (alist-get 'link issue))))
	(org-link-open-from-string link)
      (user-error "No issue action here; use ? for actions or move to an issue"))))

(defun org-confluence-sync-status-delete-local-at-point ()
  "Delete the local sidecar comment for the current deleted-remote issue."
  (interactive)
  (let* ((issue (or (org-confluence-sync-status--section-issue)
		    (user-error "No sync issue at point")))
	 (action (org-confluence-sync-status--string-value (alist-get 'action issue)))
	 (link (org-confluence-sync-status--string-value (alist-get 'link issue))))
    (unless (equal action "delete-local")
      (user-error "This issue does not recommend deleting a local comment"))
    (unless link
      (user-error "This issue has no linked local comment"))
    (pcase-let* ((`(,source-file . ,comment-id)
		  (org-comments-parse-link-path
		   (if (string-match "org-comment:\\([^]]+\\)" link)
		       (match-string 1 link)
		     link)))
		 (sidecar (org-comments-sidecar-path source-file)))
      (unless (yes-or-no-p (format "Delete local comment %s? " comment-id))
	(user-error "Canceled"))
      (org-comments-delete-entry sidecar comment-id)
      (message "Deleted local comment %s" comment-id)
      (org-confluence-sync-status-refresh))))

(defun org-confluence-sync-status-actions ()
  "Show point-sensitive Confluence sync status actions."
  (interactive)
  (if (and (featurep 'transient) (fboundp 'org-confluence-sync-status-dispatch))
      (call-interactively #'org-confluence-sync-status-dispatch)
    (user-error "Transient is unavailable; use direct report keys instead")))

(provide 'org-confluence-sync-status-actions)
;;; org-confluence-sync-status-actions.el ends here
