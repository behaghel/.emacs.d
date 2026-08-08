;;; org-copilot-diff.el --- Diff previews for Org Copilot -*- lexical-binding: t; -*-

;; Author: Hubert Behaghel
;; Maintainer: Hubert Behaghel
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (org "9.6"))
;; Keywords: outlines, tools, convenience
;; URL: https://github.com/behaghel/org-copilot

;;; Commentary:
;; Read-only diff previews for Org Copilot AI suggestions.

;;; Code:

(require 'diff-mode)
(require 'org)
(require 'context-panels)
(require 'org-copilot-model)
(require 'org-copilot-session)
(require 'org-suggestions nil 'noerror)

(defcustom org-copilot-diff-buffer-name "*Org Copilot Diff*"
  "Buffer name used for Org Copilot suggestion diff previews."
  :type 'string
  :group 'org-copilot)

(defvar-local org-copilot-diff-source-buffer nil
  "Source buffer associated with the current Org Copilot diff buffer.")

(defvar-local org-copilot-diff-comment nil
  "AI comment snapshot associated with the current Org Copilot diff buffer.")

(defvar-local org-copilot-diff-comment-id nil
  "AI comment id associated with the current Org Copilot diff buffer.")

(defvar org-copilot-diff-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map diff-mode-map)
    (define-key map (kbd "a") #'org-copilot-accept-at-point)
    (define-key map (kbd "q") #'org-copilot-close-diff)
    map)
  "Keymap used in Org Copilot diff buffers.")

(define-derived-mode org-copilot-diff-mode diff-mode "Org-Copilot-Diff"
  "Major mode for Org Copilot suggestion diff buffers.")

(defun org-copilot-diff--ensure-suggestion (comment)
  "Return durable suggestion linked to COMMENT or signal a user error."
  (or (org-copilot-comment-suggestion-text comment)
      (user-error "AI comment has no durable suggestion to diff")))

(defun org-copilot-diff--format-lines (prefix text)
  "Return TEXT formatted as diff lines with PREFIX."
  (mapconcat (lambda (line) (concat prefix line))
	     (split-string text "\n")
	     "\n"))

(defun org-copilot-diff--insert (source-buffer comment)
  "Insert diff preview for COMMENT from SOURCE-BUFFER."
  (let ((old-text (or (org-copilot-comment-target-text comment) ""))
	(new-text (org-copilot-diff--ensure-suggestion comment)))
    (insert (format "--- %s\n" (buffer-name source-buffer)))
    (insert (format "+++ %s suggestion %s\n"
		    (buffer-name source-buffer)
		    (org-copilot-comment-id comment)))
    (insert "@@ org-copilot suggestion @@\n")
    (insert (org-copilot-diff--format-lines "-" old-text) "\n")
    (insert (org-copilot-diff--format-lines "+" new-text) "\n")))

(defun org-copilot-diff-open (source-buffer comment)
  "Open a read-only diff preview for COMMENT from SOURCE-BUFFER.
Return the diff buffer."
  (org-copilot-diff--ensure-suggestion comment)
  (let ((buffer (get-buffer-create org-copilot-diff-buffer-name)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
	(erase-buffer)
	(org-copilot-diff-mode)
	(setq org-copilot-diff-source-buffer source-buffer)
	(setq context-panels-source-buffer source-buffer)
	(setq context-panels-view-id 'copilot-diff)
	(setq org-copilot-diff-comment comment)
	(setq org-copilot-diff-comment-id (org-copilot-comment-id comment))
	(org-copilot-diff--insert source-buffer comment)
	(goto-char (point-min))
	(setq buffer-read-only t)))
    buffer))

(defun org-copilot-diff--source-buffer ()
  "Return the source buffer for a diff command."
  (cond
   ((buffer-live-p org-copilot-diff-source-buffer)
    org-copilot-diff-source-buffer)
   ((derived-mode-p 'org-mode)
    (current-buffer))
   (t
    (context-panels-current-source-buffer))))

(defun org-copilot-latest-comment-for-item (item source-buffer)
  "Return latest model comment for ITEM from SOURCE-BUFFER.
Fall back to ITEM when it cannot be resolved by id."
  (let ((id (org-copilot-comment-id item)))
    (or (and id
	     (with-current-buffer source-buffer
	       (org-copilot-find-visible-comment id)))
	item)))

(defun org-copilot-comment-at-point ()
  "Return the latest Org Copilot AI comment at point, or signal a user error."
  (let* ((source-buffer (org-copilot-diff--source-buffer))
	 (item (or (and org-copilot-diff-comment-id
			(with-current-buffer source-buffer
			  (org-copilot-find-visible-comment org-copilot-diff-comment-id)))
		   org-copilot-diff-comment
		   (context-panels-item-at-point)
		   (with-current-buffer source-buffer
		     (and org-copilot-chat-focus-comment-id
			  (org-copilot-find-visible-comment
			   org-copilot-chat-focus-comment-id)))
		   (user-error "No AI comment at point"))))
    (org-copilot-latest-comment-for-item item source-buffer)))

(defun org-copilot-comment-valid-target-p (comment source-buffer)
  "Return non-nil when COMMENT still matches SOURCE-BUFFER text."
  (let ((start (plist-get comment :source-start))
	(end (plist-get comment :source-end))
	(target-text (plist-get comment :target-text)))
    (with-current-buffer source-buffer
      (if (eq (plist-get comment :type) 'insertion)
	  (and start end
	       (= start end)
	       (<= (point-min) start)
	       (<= start (point-max)))
	(and start end target-text
	     (<= (point-min) start)
	     (<= start end)
	     (<= end (point-max))
	     (equal (buffer-substring-no-properties start end) target-text))))))

(defun org-copilot--target-text-matches (target-text source-buffer)
  "Return exact source matches for TARGET-TEXT in SOURCE-BUFFER."
  (unless (string-empty-p (or target-text ""))
    (with-current-buffer source-buffer
      (save-excursion
	(goto-char (point-min))
	(let (matches)
	  (while (search-forward target-text nil t)
	    (push (cons (match-beginning 0) (match-end 0)) matches))
	  (nreverse matches))))))

(defun org-copilot-resolve-comment-target (comment source-buffer)
  "Return COMMENT with recovered source bounds when uniquely resolvable."
  (if (org-copilot-comment-valid-target-p comment source-buffer)
      comment
    (let ((matches (org-copilot--target-text-matches
		    (plist-get comment :target-text) source-buffer)))
      (if (= (length matches) 1)
	  (let* ((match (car matches))
		 (copy (copy-sequence comment)))
	    (plist-put copy :source-start (car match))
	    (plist-put copy :source-end (cdr match))
	    copy)
	comment))))

(defun org-copilot--update-comment-status (comment _source-buffer status)
  "Update durable COMMENT with lifecycle STATUS."
  (if (plist-get comment :sidecar-file)
      (org-copilot-set-durable-comment-status
       comment (if (eq status 'dismissed) "RESOLVED" "OPEN"))
    (user-error "Legacy in-memory Copilot comments are retired")))

(defun org-copilot-accept-comment (comment source-buffer)
  "Accept durable COMMENT's linked suggestion in SOURCE-BUFFER."
  (if-let* ((source-file (buffer-file-name source-buffer))
	    (linked-id (org-copilot-linked-suggestion-id comment source-file)))
      (progn
	(org-suggestions-accept-candidate-id source-buffer linked-id)
	(org-copilot-set-durable-comment-status comment "RESOLVED"))
    (user-error "Copilot suggestions must be durable org-suggestions candidates")))

(defun org-copilot-undo-accepted-comment (_comment _source-buffer)
  "Undo accepted durable suggestions.
Rollback now belongs to `org-suggestions' session-local undo support."
  (user-error "Use org-suggestions undo for durable accepted suggestions"))

(defun org-copilot-dismiss-comment (comment source-buffer)
  "Dismiss durable COMMENT from SOURCE-BUFFER's current Org Copilot session."
  (if-let* ((source-file (buffer-file-name source-buffer))
	    ((or (plist-get comment :sidecar-file)
		 (plist-get comment :suggestion-ids))))
      (progn
	(org-copilot-set-linked-suggestions-status source-file comment 'dismissed)
	(org-copilot-set-durable-comment-status comment "RESOLVED"))
    (user-error "Legacy in-memory Copilot comments are retired")))

(defun org-copilot-diff--refresh-panel-buffer (source-buffer)
  "Refresh the current panel buffer for SOURCE-BUFFER when applicable."
  (when (and (derived-mode-p 'org-copilot-panel-mode)
	     (eq context-panels-source-buffer source-buffer))
    (context-panels-render-side-panel source-buffer)))

;;;###autoload
(defun org-copilot-close-diff ()
  "Close the current Org Copilot diff buffer."
  (interactive)
  (let ((buffer (current-buffer))
	(window (selected-window)))
    (when (window-live-p window)
      (unless (one-window-p t)
	(delete-window window)))
    (when (buffer-live-p buffer)
      (kill-buffer buffer))))

(defun org-copilot-diff--close-current-buffer-after-accept (source-buffer)
  "Close the current diff buffer and return focus to SOURCE-BUFFER."
  (let ((buffer (current-buffer))
	(window (selected-window)))
    (when (buffer-live-p source-buffer)
      (pop-to-buffer source-buffer))
    (when (and (window-live-p window)
	       (not (one-window-p t)))
      (delete-window window))
    (when (buffer-live-p buffer)
      (kill-buffer buffer))))

(defun org-copilot--linked-suggestion-id-at-point (source-buffer)
  "Return the linked suggestion id for context item at point in SOURCE-BUFFER."
  (when (fboundp 'org-suggestions-find-candidate)
    (when-let* ((item (ignore-errors (context-panels-item-at-point)))
		(ids (plist-get item :suggestion-ids))
		(source-file (buffer-file-name source-buffer))
		(threads (org-suggestions-load-sidecar source-file)))
      (cl-loop for id in (reverse (split-string ids "[[:space:]]+" t))
	       for match = (org-suggestions-find-candidate threads id)
	       when (and match (eq (plist-get (cdr match) :status) 'active))
	       return id
	       finally return (car (last (split-string ids "[[:space:]]+" t)))))))

;;;###autoload
(defun org-copilot-accept-at-point ()
  "Accept the AI suggestion at point."
  (interactive)
  (let* ((source-buffer (org-copilot-diff--source-buffer))
	 (linked-suggestion-id
	  (org-copilot--linked-suggestion-id-at-point source-buffer))
	 (close-diff-buffer-p (and org-copilot-diff-comment
				   (derived-mode-p 'diff-mode))))
    (if linked-suggestion-id
	(org-suggestions-accept-candidate-id source-buffer linked-suggestion-id)
      (org-copilot-accept-comment
       (org-copilot-comment-at-point)
       source-buffer))
    (with-current-buffer source-buffer
      (when (fboundp 'org-copilot-refresh-overlays)
	(org-copilot-refresh-overlays)))
    (org-copilot-diff--refresh-panel-buffer source-buffer)
    (when close-diff-buffer-p
      (org-copilot-diff--close-current-buffer-after-accept source-buffer))))

;;;###autoload
(defun org-copilot-dismiss-at-point ()
  "Dismiss the AI comment at point."
  (interactive)
  (let ((source-buffer (org-copilot-diff--source-buffer)))
    (org-copilot-dismiss-comment
     (org-copilot-comment-at-point)
     source-buffer)
    (with-current-buffer source-buffer
      (when (fboundp 'org-copilot-refresh-overlays)
	(org-copilot-refresh-overlays)))
    (org-copilot-diff--refresh-panel-buffer source-buffer)))

(defun org-copilot--display-diff-buffer (source-buffer buffer)
  "Display diff BUFFER for SOURCE-BUFFER and select its window."
  (let ((window (display-buffer-in-side-window
		 buffer
		 '((side . bottom)
		   (slot . 0)
		   (window-height . 12)
		   (window-parameters
		    . ((no-other-window . t)
		       (no-delete-other-windows . t)))))))
    (when (window-live-p window)
      (when (fboundp 'context-panels-protect-window)
	(context-panels-protect-window window buffer source-buffer))
      (select-window window))))

;;;###autoload
(defun org-copilot-view-diff-at-point ()
  "Open a read-only diff preview for the durable AI suggestion at point."
  (interactive)
  (let* ((comment (org-copilot-comment-at-point))
	 (source-buffer (org-copilot-diff--source-buffer))
	 (buffer (org-copilot-diff-open source-buffer comment)))
    (org-copilot--display-diff-buffer source-buffer buffer)))

;;;###autoload
(defun org-copilot-visualize-at-point ()
  "Visualize the focused Copilot artifact at point.
Linked durable suggestions open a diff.  Plain comments focus their chat
context instead of signaling a suggestion-specific error."
  (interactive)
  (let* ((source-buffer (org-copilot-diff--source-buffer))
	 (comment (org-copilot-comment-at-point)))
    (if (with-current-buffer source-buffer
	  (org-copilot-comment-suggestion-text comment))
	(org-copilot--display-diff-buffer
	 source-buffer (org-copilot-diff-open source-buffer comment))
      (with-current-buffer source-buffer
	(org-copilot-chat--set-context
	 source-buffer (list :type 'comment
			     :comment-id (org-copilot-comment-id comment))))
      (when (fboundp 'org-copilot-chat)
	(org-copilot-chat))
      (message "Focused Copilot comment has no linked durable suggestion"))))

(provide 'org-copilot-diff)
;;; org-copilot-diff.el ends here
