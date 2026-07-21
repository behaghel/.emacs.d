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
(require 'org-context-panel)
(require 'org-copilot-model)
(require 'org-copilot-session)
(require 'org-copilot-suggestion)
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
  "Return COMMENT's suggestion or signal a user error."
  (or (plist-get comment :suggestion)
      (user-error "AI comment has no suggestion to diff")))

(defun org-copilot-diff--format-lines (prefix text)
  "Return TEXT formatted as diff lines with PREFIX."
  (mapconcat (lambda (line) (concat prefix line))
	     (split-string text "\n")
	     "\n"))

(defun org-copilot-diff--insert (source-buffer comment)
  "Insert diff preview for COMMENT from SOURCE-BUFFER."
  (let ((old-text (or (plist-get comment :target-text) ""))
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
	(setq org-context-panel-source-buffer source-buffer)
	(setq org-context-panel-view-id 'copilot-diff)
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
    (org-context-panel-current-source-buffer))))

(defun org-copilot-latest-comment-for-item (item source-buffer)
  "Return latest model comment for ITEM from SOURCE-BUFFER.
Fall back to ITEM when it cannot be resolved by id."
  (let ((id (org-copilot-comment-id item)))
    (or (and id
	     (with-current-buffer source-buffer
	       (org-copilot-find-comment id)))
	item)))

(defun org-copilot-comment-at-point ()
  "Return the latest Org Copilot AI comment at point, or signal a user error."
  (let* ((source-buffer (org-copilot-diff--source-buffer))
	 (item (or (and org-copilot-diff-comment-id
			(with-current-buffer source-buffer
			  (org-copilot-find-comment org-copilot-diff-comment-id)))
		   org-copilot-diff-comment
		   (org-context-panel-item-at-point)
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

(defun org-copilot--update-comment-status (comment source-buffer status)
  "Update COMMENT in SOURCE-BUFFER with lifecycle STATUS."
  (with-current-buffer source-buffer
    (org-copilot-update-comment
     (org-copilot-comment-with-status comment status))))

(defun org-copilot-accept-section-suggestion (comment source-buffer)
  "Accept section suggestion COMMENT in SOURCE-BUFFER.
Section suggestions replace the current section body, not a stale stored diff."
  (let* ((suggestion (org-copilot-diff--ensure-suggestion comment))
	 (section (or (org-copilot-suggestion-resolve-comment-section
		       source-buffer comment)
		      (progn
			(org-copilot--update-comment-status comment source-buffer 'stale)
			(user-error "AI section target is stale; reselect section"))))
	 (start (plist-get section :body-start))
	 (end (plist-get section :end))
	 (normalized (org-copilot-suggestion-normalize-section-body suggestion)))
    (with-current-buffer source-buffer
      (let ((original-text (buffer-substring-no-properties start end)))
	(save-excursion
	  (goto-char start)
	  (delete-region start end)
	  (insert normalized))
	(let* ((root-id (or (plist-get comment :thread-root-id)
			    (org-copilot-comment-id comment)))
	       (accepted (org-copilot-comment-with-status comment 'accepted))
	       (accepted (plist-put accepted :thread-root-id root-id))
	       (accepted (plist-put accepted :original-target-text original-text))
	       (accepted (plist-put accepted :original-source-start start))
	       (accepted (plist-put accepted :original-source-end end))
	       (accepted (plist-put accepted :accepted-text normalized))
	       (accepted (plist-put accepted :source-start start))
	       (accepted (plist-put accepted :source-end (+ start (length normalized))))
	       (accepted (plist-put accepted :target-text normalized))
	       (accepted (plist-put accepted :heading-line
				    (plist-get section :heading-line)))
	       (accepted (plist-put accepted :section-title
				    (plist-get section :section-title)))
	       (accepted (plist-put accepted :section-path
				    (plist-get section :section-path))))
	  (org-copilot-update-comment accepted)
	  (dolist (sibling (org-copilot-comments))
	    (when (and (equal root-id (or (plist-get sibling :thread-root-id)
					  (org-copilot-comment-id sibling)))
		       (not (equal (org-copilot-comment-id sibling)
				   (org-copilot-comment-id accepted)))
		       (eq (org-copilot-comment-status sibling) 'active))
	      (let ((copy (org-copilot-comment-with-status sibling 'dismissed)))
		(org-copilot-update-comment
		 (plist-put copy :superseded-by
			    (org-copilot-comment-id accepted)))))))))))

(defun org-copilot-accept-comment (comment source-buffer)
  "Accept COMMENT's suggestion in SOURCE-BUFFER.
If COMMENT no longer matches the source text, mark it stale and signal a user
error instead of modifying the source."
  (if (org-copilot-suggestion-section-comment-p comment)
      (org-copilot-accept-section-suggestion comment source-buffer)
    (let* ((comment (org-copilot-resolve-comment-target comment source-buffer))
	   (suggestion (org-copilot-diff--ensure-suggestion comment))
	   (start (plist-get comment :source-start))
	   (end (plist-get comment :source-end)))
      (unless (org-copilot-comment-valid-target-p comment source-buffer)
	(org-copilot--update-comment-status comment source-buffer 'stale)
	(user-error "AI comment target is stale; review again before accepting"))
      (with-current-buffer source-buffer
	(save-excursion
	  (goto-char start)
	  (delete-region start end)
	  (insert suggestion))
	(let* ((accepted (org-copilot-comment-with-status comment 'accepted))
	       (accepted (plist-put accepted :original-target-text
				    (or (plist-get comment :target-text) "")))
	       (accepted (plist-put accepted :original-source-start start))
	       (accepted (plist-put accepted :original-source-end end))
	       (accepted (plist-put accepted :accepted-text suggestion))
	       (accepted (plist-put accepted :source-end (+ start (length suggestion))))
	       (accepted (plist-put accepted :target-text suggestion)))
	  (org-copilot-update-comment accepted))))))

(defun org-copilot-undo-accepted-comment (comment source-buffer)
  "Undo accepted COMMENT in SOURCE-BUFFER."
  (unless (eq (org-copilot-comment-status comment) 'accepted)
    (user-error "AI comment is not accepted"))
  (let ((original-text (or (plist-get comment :original-target-text)
			   (user-error "AI comment has no rollback text")))
	(accepted-text (or (plist-get comment :accepted-text)
			   (plist-get comment :target-text)))
	(start (plist-get comment :source-start))
	(end (plist-get comment :source-end)))
    (with-current-buffer source-buffer
      (unless (and start end
		   (<= (point-min) start)
		   (<= start end)
		   (<= end (point-max))
		   (equal (buffer-substring-no-properties start end) accepted-text))
	(user-error "AI comment accepted text is stale; cannot undo safely"))
      (save-excursion
	(goto-char start)
	(delete-region start end)
	(insert original-text))
      (let* ((active (org-copilot-comment-with-status comment 'active))
	     (active (plist-put active :source-start
				(or (plist-get comment :original-source-start) start)))
	     (active (plist-put active :source-end
				(or (plist-get comment :original-source-end)
				    (+ start (length original-text)))))
	     (active (plist-put active :target-text original-text)))
	(org-copilot-update-comment active)))))

(defun org-copilot-dismiss-comment (comment source-buffer)
  "Dismiss COMMENT from SOURCE-BUFFER's current Org Copilot session."
  (with-current-buffer source-buffer
    (org-copilot-remove-comment comment)))

(defun org-copilot-diff--refresh-panel-buffer (source-buffer)
  "Refresh the current panel buffer for SOURCE-BUFFER when applicable."
  (when (and (derived-mode-p 'org-copilot-panel-mode)
	     (eq org-context-panel-source-buffer source-buffer))
    (org-context-panel-render-side-panel source-buffer)))

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
    (when-let* ((item (ignore-errors (org-context-panel-item-at-point)))
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

;;;###autoload
(defun org-copilot-view-diff-at-point ()
  "Open a read-only diff preview for the AI suggestion at point."
  (interactive)
  (let* ((comment (org-copilot-comment-at-point))
	 (source-buffer (org-copilot-diff--source-buffer))
	 (buffer (org-copilot-diff-open source-buffer comment))
	 (window (display-buffer-in-side-window
		  buffer
		  '((side . bottom)
		    (slot . 0)
		    (window-height . 12)
		    (window-parameters
		     . ((no-other-window . t)
			(no-delete-other-windows . t)))))))
    (when (window-live-p window)
      (when (fboundp 'org-context-panel-protect-window)
	(org-context-panel-protect-window window buffer source-buffer))
      (select-window window))))

(provide 'org-copilot-diff)
;;; org-copilot-diff.el ends here
