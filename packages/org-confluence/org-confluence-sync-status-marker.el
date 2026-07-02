;;; org-confluence-sync-status-marker.el --- Source buffer sync marker -*- lexical-binding: t; -*-

;;; Commentary:
;; Top-of-buffer overlay marker for Confluence sync status in linked Org buffers.

;;; Code:

(require 'org)
(require 'subr-x)

(require 'org-confluence-api)
(require 'org-confluence-sync-status)

(defcustom org-confluence-sync-status-show-source-marker t
  "Whether `org-confluence-mode' shows a source-buffer sync status marker."
  :type 'boolean
  :group 'org-confluence-api)

(defvar-local org-confluence-sync-status--source-marker-overlay nil
  "Overlay displaying the source buffer Confluence sync status marker.")

(defun org-confluence-sync-status-marker-map ()
  "Return keymap for the Confluence sync status marker."
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'org-confluence-sync-status-open-from-marker)
    (define-key map [mouse-1] #'org-confluence-sync-status-open-from-marker)
    map))

(defun org-confluence-sync-status--metadata-end-position ()
  "Return position after leading Org metadata keywords and blank lines."
  (save-excursion
    (goto-char (point-min))
    (while (and (not (eobp))
		(looking-at-p "^[ \t]*#\\+[^:\n]+:.*$"))
      (forward-line 1))
    (while (and (not (eobp))
		(looking-at-p "^[ \t]*$"))
      (forward-line 1))
    (point)))

(defun org-confluence-sync-status--confluence-page-id-present-p ()
  "Return non-nil when the current Org buffer has a Confluence page ID."
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search t))
      (re-search-forward "^[[:blank:]]*#\\+CONFLUENCE_PAGE_ID:[[:blank:]]*\\S-+" nil t))))

(defun org-confluence-sync-status--delete-source-marker ()
  "Delete the current source-buffer sync status marker overlay."
  (when (overlayp org-confluence-sync-status--source-marker-overlay)
    (delete-overlay org-confluence-sync-status--source-marker-overlay))
  (setq org-confluence-sync-status--source-marker-overlay nil))

(defun org-confluence-sync-status--remove-persisted-source-markers ()
  "Remove accidentally persisted sync status marker lines near metadata."
  (save-excursion
    (let ((inhibit-read-only t))
      (goto-char (org-confluence-sync-status--metadata-end-position))
      (while (looking-at-p "^[[:blank:]]*\\[Sync [^]\n]+\\][[:blank:]]*$")
	(delete-region (line-beginning-position) (min (point-max) (1+ (line-end-position))))))))

;;;###autoload
(defun org-confluence-sync-status-refresh-source-marker ()
  "Refresh the Confluence sync status marker in the current Org source buffer."
  (interactive)
  (org-confluence-sync-status--delete-source-marker)
  (org-confluence-sync-status--remove-persisted-source-markers)
  (when (and org-confluence-sync-status-show-source-marker
	     buffer-file-name
	     (derived-mode-p 'org-mode)
	     (org-confluence-sync-status--confluence-page-id-present-p))
    (let* ((label (org-confluence-sync-status-marker-string buffer-file-name))
	   (position (org-confluence-sync-status--metadata-end-position))
	   (overlay (make-overlay position position nil t nil)))
      (overlay-put overlay 'priority -1)
      (overlay-put overlay 'after-string
		   (concat (propertize label
				       'face 'link
				       'mouse-face 'highlight
				       'help-echo "Open Confluence sync status"
				       'keymap (org-confluence-sync-status-marker-map))
			   "\n"))
      (setq org-confluence-sync-status--source-marker-overlay overlay))))

(defun org-confluence-sync-status-refresh-source-marker-in-buffer (source-buffer)
  "Refresh Confluence sync status marker in SOURCE-BUFFER."
  (when (buffer-live-p source-buffer)
    (with-current-buffer source-buffer
      (org-confluence-sync-status-refresh-source-marker))))

(defun org-confluence-sync-status-source-marker-setup ()
  "Enable source marker hooks in the current buffer."
  (add-hook 'after-save-hook #'org-confluence-sync-status-refresh-source-marker nil t)
  (org-confluence-sync-status-refresh-source-marker))

(defun org-confluence-sync-status-source-marker-teardown ()
  "Disable source marker hooks and remove marker overlay in the current buffer."
  (remove-hook 'after-save-hook #'org-confluence-sync-status-refresh-source-marker t)
  (org-confluence-sync-status--delete-source-marker))

(provide 'org-confluence-sync-status-marker)
;;; org-confluence-sync-status-marker.el ends here
