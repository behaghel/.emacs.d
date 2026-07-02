;;; org-confluence-sync-status-display.el --- Sync status display -*- lexical-binding: t; -*-

;;; Commentary:
;; Window placement and page-context restoration for Confluence sync status
;; reports.

;;; Code:

(defvar-local org-confluence-sync-status--restore-page-context nil
  "Non-nil when closing this sync report should restore page comments.")

(defvar org-confluence-sync-status--source-buffer nil
  "Source Org buffer associated with the current Confluence sync status report.")

(defvar org-confluence-sync-status-page-context-window-function nil
  "Function called with a source buffer to find a visible page-context window.")

(defvar org-confluence-sync-status-restore-page-context-function nil
  "Function called with source buffer and window to restore page context.")

(defun org-confluence-sync-status-close ()
  "Close the current sync status report window.
When this report replaced page comments, restore that bottom panel instead of
closing the window."
  (interactive)
  (let ((source org-confluence-sync-status--source-buffer)
	(restore org-confluence-sync-status--restore-page-context)
	(window (selected-window)))
    (cond
     ((and restore
	   (buffer-live-p source)
	   (functionp org-confluence-sync-status-restore-page-context-function)
	   (funcall org-confluence-sync-status-restore-page-context-function
		    source window)))
     ((and (window-live-p window)
	   (not (one-window-p t)))
      (delete-window window))
     (t
      (quit-window)))))

(defun org-confluence-sync-status--page-context-window (source-buffer)
  "Return visible page-context window for SOURCE-BUFFER, or nil."
  (when (and (buffer-live-p source-buffer)
	     (functionp org-confluence-sync-status-page-context-window-function))
    (funcall org-confluence-sync-status-page-context-window-function source-buffer)))

(defun org-confluence-sync-status--display-bottom (buffer source-buffer)
  "Display sync status BUFFER below SOURCE-BUFFER.
If a page-comments panel is already visible, reuse its window and return
`page-context'.  Otherwise create/reuse a bottom window and return nil."
  (let* ((source-window (or (get-buffer-window source-buffer t) (selected-window)))
	 (page-window (org-confluence-sync-status--page-context-window source-buffer))
	 (target-window (or page-window
			    (split-window source-window
					  (- (max 8 (floor (* (window-total-height source-window) 0.33))))
					  'below))))
    (set-window-buffer target-window buffer)
    (select-window target-window)
    (when page-window 'page-context)))

(defun org-confluence-sync-status-display-bottom (buffer source-buffer)
  "Display sync status BUFFER below SOURCE-BUFFER and remember restore state."
  (with-current-buffer buffer
    (setq-local org-confluence-sync-status--restore-page-context
		(eq (org-confluence-sync-status--display-bottom buffer source-buffer)
		    'page-context))))

(provide 'org-confluence-sync-status-display)
;;; org-confluence-sync-status-display.el ends here
