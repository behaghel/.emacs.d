;;; org-confluence-sync-status.el --- Confluence sync status UI -*- lexical-binding: t; -*-

;;; Commentary:
;; Interactive sync status report orchestration for Confluence-backed Org buffers.

;;; Code:

(require 'magit-section nil 'noerror)
(require 'transient nil 'noerror)
(require 'org-confluence-sync-status-actions)
(require 'org-confluence-sync-status-cache)
(require 'org-confluence-sync-status-collect)
(require 'org-confluence-sync-status-display)
(require 'org-confluence-sync-status-render)

(declare-function magit-section-mode "magit-section")
(defvar-local org-confluence-sync-status--source-buffer nil
  "Source Org buffer associated with the current Confluence sync status report.")

(defvar org-confluence-sync-status--last-source-buffer nil
  "Last source Org buffer associated with a Confluence sync status report.")

(defvar org-confluence-sync-status-refresh-source-marker-function nil
  "Function called with a source buffer after sync status cache updates.")

(defun org-confluence-sync-status--refresh-source-marker (source-buffer)
  "Refresh the top sync marker in SOURCE-BUFFER when a callback is configured."
  (when (and (buffer-live-p source-buffer)
	     (functionp org-confluence-sync-status-refresh-source-marker-function))
    (funcall org-confluence-sync-status-refresh-source-marker-function source-buffer)))

(defun org-confluence-sync-status-jump-section (section-name)
  "Jump to sync status section named SECTION-NAME."
  (goto-char (point-min))
  (unless (re-search-forward (format "^[✓⚠×?] %s " (regexp-quote section-name)) nil t)
    (user-error "No %s section in this sync report" section-name))
  (beginning-of-line))

(defun org-confluence-sync-status-jump-page ()
  "Jump to the Page section."
  (interactive)
  (org-confluence-sync-status-jump-section "Page"))

(defun org-confluence-sync-status-jump-comments ()
  "Jump to the Comments section."
  (interactive)
  (org-confluence-sync-status-jump-section "Comments"))

(defun org-confluence-sync-status-jump-attachments ()
  "Jump to the Attachments section."
  (interactive)
  (org-confluence-sync-status-jump-section "Attachments"))

(defun org-confluence-sync-status-jump-people ()
  "Jump to the People section."
  (interactive)
  (org-confluence-sync-status-jump-section "People"))

(defvar org-confluence-sync-status-mode-map
  (let ((map (make-sparse-keymap)))
    (when (boundp 'magit-section-mode-map)
      (set-keymap-parent map magit-section-mode-map))
    (define-key map (kbd "g") #'org-confluence-sync-status-refresh)
    (define-key map (kbd "y") #'org-confluence-sync-status-sync-page)
    (define-key map (kbd "Y") #'org-confluence-sync-status-sync-current)
    (define-key map (kbd "f") #'org-confluence-sync-status-pull-page)
    (define-key map (kbd "F") #'org-confluence-sync-status-pull-page-with-comments)
    (define-key map (kbd "i") #'org-confluence-sync-status-import-comments)
    (define-key map (kbd "a") #'org-confluence-sync-status-reanchor-comments)
    (define-key map (kbd "A") #'org-confluence-sync-status-repair-anchors)
    (define-key map (kbd "u") #'org-confluence-sync-status-resolve-people)
    (define-key map (kbd "p") #'org-confluence-sync-status-publish)
    (define-key map (kbd "o") #'org-confluence-sync-status-open-page)
    (define-key map (kbd "C") #'org-confluence-sync-status-open-sidecar)
    (define-key map (kbd "L") #'org-confluence-sync-status-list-comments)
    (define-key map (kbd "!") #'org-confluence-sync-status-force-publish)
    (define-key map (kbd "?") #'org-confluence-sync-status-actions)
    (define-key map (kbd "x") #'org-confluence-sync-status-delete-local-at-point)
    (define-key map (kbd "RET") #'org-confluence-sync-status-act-at-point)
    (define-key map (kbd "q") #'org-confluence-sync-status-close)
    (define-key map (kbd "j p") #'org-confluence-sync-status-jump-page)
    (define-key map (kbd "j c") #'org-confluence-sync-status-jump-comments)
    (define-key map (kbd "j a") #'org-confluence-sync-status-jump-attachments)
    (define-key map (kbd "j u") #'org-confluence-sync-status-jump-people)
    map)
  "Keymap for Confluence sync status reports.")

(if (featurep 'magit-section)
    (define-derived-mode org-confluence-sync-status-mode magit-section-mode "Confluence-Sync"
      "Major mode for Confluence sync status reports."
      (setq-local truncate-lines nil
		  word-wrap t))
  (define-derived-mode org-confluence-sync-status-mode special-mode "Confluence-Sync"
    "Major mode for Confluence sync status reports."
    (setq-local truncate-lines nil
		word-wrap t)))

(when (featurep 'transient)
  (transient-define-prefix org-confluence-sync-status-dispatch ()
			   "Show Confluence sync status actions."
			   [["Content"
			     ("g" "refresh report" org-confluence-sync-status-refresh)
			     ("y" "sync page" org-confluence-sync-status-sync-page)
			     ("f" "fetch page" org-confluence-sync-status-pull-page)
			     ("p" "publish safely" org-confluence-sync-status-publish)
			     ("!" "force publish" org-confluence-sync-status-force-publish)
			     ("o" "open page" org-confluence-sync-status-open-page)]
			    ["Comments"
			     ("Y" "sync page + comments" org-confluence-sync-status-sync-current)
			     ("F" "fetch page + comments" org-confluence-sync-status-pull-page-with-comments)
			     ("i" "import comments" org-confluence-sync-status-import-comments)
			     ("C" "open sidecar" org-confluence-sync-status-open-sidecar)
			     ("L" "list remote comments" org-confluence-sync-status-list-comments)]
			    ["At point / tools"
			     ("RET" "open issue/comment" org-confluence-sync-status-act-at-point)
			     ("x" "delete local deleted-remote comment" org-confluence-sync-status-delete-local-at-point)
			     ("a" "reanchor local comments" org-confluence-sync-status-reanchor-comments)
			     ("A" "repair remote anchors" org-confluence-sync-status-repair-anchors)
			     ("u" "resolve people" org-confluence-sync-status-resolve-people)]]))

;;;###autoload
(defun org-confluence-sync-status-open-from-marker ()
  "Open sync status from the top marker, refreshing stale or unknown cache."
  (interactive)
  (let* ((cached (org-confluence-sync-status--cache-state buffer-file-name))
	 (state (alist-get 'state cached)))
    (org-confluence-sync-status (equal state "unknown"))))

;;;###autoload
(defun org-confluence-sync-status (&optional _refresh)
  "Refresh and open Confluence sync status report for the current Org buffer."
  (interactive "P")
  (let* ((source-buffer (current-buffer))
	 (status (let ((fresh (org-confluence-sync-status--collect source-buffer)))
		   (org-confluence-sync-status--write-cache
		    fresh (buffer-file-name source-buffer))
		   (org-confluence-sync-status--refresh-source-marker source-buffer)
		   fresh))
	 (buffer (get-buffer-create "*Org Confluence Sync Status*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
	(erase-buffer)
	(org-confluence-sync-status-mode)
	(setq-local org-confluence-sync-status--source-buffer source-buffer)
	(setq org-confluence-sync-status--last-source-buffer source-buffer)
	(org-confluence-sync-status-render status source-buffer)
	(goto-char (point-min))))
    (org-confluence-sync-status-display-bottom buffer source-buffer)
    buffer))

(defun org-confluence-sync-status-refresh ()
  "Refresh the current Confluence sync status report."
  (interactive)
  (unless (buffer-live-p org-confluence-sync-status--source-buffer)
    (user-error "No source buffer for this sync status report"))
  (with-current-buffer org-confluence-sync-status--source-buffer
    (org-confluence-sync-status t)))


(provide 'org-confluence-sync-status)
;;; org-confluence-sync-status.el ends here
