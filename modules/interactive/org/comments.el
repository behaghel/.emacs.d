;;; comments.el --- Org sidecar comment commands -*- lexical-binding: t; -*-

;;; Commentary:
;; Interactive commands for region-targeted Org comments stored in colocated
;; plain-Org sidecar files.

;;; Code:

(require 'cl-lib)
(require 'hub-org-comments)
(require 'org/context-panel)
(require 'org)

(defun hub/org-comment--region-bounds ()
  "Return active region bounds or signal a user error."
  (unless (use-region-p)
    (user-error "Select a region to comment on"))
  (cons (region-beginning) (region-end)))

(defun hub/org-comment--valid-comments ()
  "Return valid sidecar comments for the current Org buffer."
  (unless (derived-mode-p 'org-mode)
    (user-error "Org comments only work in Org buffers"))
  (or (hub/org-comment-collect (current-buffer))
      (user-error "No comments in this buffer")))

(defun hub/org-comment--target-start (comment)
  "Return COMMENT target start position."
  (or (plist-get comment :target-start)
      (plist-get comment :jump-pos)))

(defun hub/org-comment--stale-comments ()
  "Return stale sidecar comments for the current Org buffer."
  (cl-remove-if-not
   (lambda (comment)
     (eq (plist-get comment :anchor-state) 'stale))
   (or (hub/org-comment-collect (current-buffer) t)
       (user-error "No comments in this buffer"))))

(defun hub/org-comment--single-stale-comment ()
  "Return the only stale sidecar comment for the current Org buffer."
  (let ((comments (hub/org-comment--stale-comments)))
    (pcase (length comments)
      (0 (user-error "No stale comments in this buffer"))
      (1 (car comments))
      (_ (user-error "Multiple stale comments; repair from the sidecar for now")))))

(defun hub/org-comment--leave-visual-state ()
  "Deactivate visual selection before prompting or mutating sidecars."
  (deactivate-mark)
  (when (and (bound-and-true-p evil-mode)
	     (fboundp 'evil-normal-state))
    (evil-normal-state)))

(defun hub/org-comment--goto (comment)
  "Move point to COMMENT and open the context panel."
  (goto-char (hub/org-comment--target-start comment))
  (hub/org-comment-overlays-refresh)
  (hub/org-context-panel-open))

(defun hub/org-comment--active-at-point ()
  "Return the sidecar comment active at point, or signal a user error."
  (let ((point (point)))
    (or (cl-find-if
	 (lambda (comment)
	   (let ((start (plist-get comment :target-start))
		 (end (plist-get comment :target-end)))
	     (and start end (<= start point) (<= point end))))
	 (hub/org-comment--valid-comments))
	(user-error "Point is not inside a commented region"))))

(defun hub/org-comment--goto-sidecar-heading (comment)
  "Open COMMENT sidecar and move point to its heading."
  (let ((sidecar-file (plist-get comment :sidecar-file))
	(id (plist-get comment :id)))
    (unless (and sidecar-file id)
      (user-error "Comment record is missing sidecar metadata"))
    (find-file sidecar-file)
    (org-mode)
    (goto-char (point-min))
    (unless (cl-loop while (re-search-forward org-heading-regexp nil t)
		     do (goto-char (match-beginning 0))
		     when (equal id (org-entry-get nil "HUB_COMMENT_ID"))
		     return t
		     do (forward-line 1))
      (user-error "Comment %s not found in sidecar" id))))

(defun hub/org-comment--goto-sidecar-body (comment)
  "Open COMMENT sidecar and move point to its body."
  (hub/org-comment--goto-sidecar-heading comment)
  (forward-line 1)
  (when (looking-at-p "[[:space:]]*:PROPERTIES:[[:space:]]*$")
    (let ((subtree-end (save-excursion (org-end-of-subtree t t))))
      (when (re-search-forward "^[[:space:]]*:END:[[:space:]]*$" subtree-end t)
	(forward-line 1))))
  (while (looking-at-p "\n")
    (forward-char 1)))

(defun hub/org-comment--set-sidecar-status (comment status)
  "Set COMMENT sidecar heading TODO keyword to STATUS."
  (let ((sidecar-file (plist-get comment :sidecar-file))
	(id (plist-get comment :id)))
    (unless (and sidecar-file id)
      (user-error "Comment record is missing sidecar metadata"))
    (with-temp-buffer
      (insert-file-contents sidecar-file)
      (org-mode)
      (goto-char (point-min))
      (unless (cl-loop while (re-search-forward org-heading-regexp nil t)
		       do (goto-char (match-beginning 0))
		       when (equal id (org-entry-get nil "HUB_COMMENT_ID"))
		       return (progn (org-todo status) t)
		       do (forward-line 1))
	(user-error "Comment %s not found in sidecar" id))
      (write-region (point-min) (point-max) sidecar-file nil 'silent))))

(defun hub/org-comment-set-status (status)
  "Set the active comment at point to STATUS."
  (let ((comment (hub/org-comment--active-at-point)))
    (hub/org-comment--set-sidecar-status comment status)
    (hub/org-comment-overlays-refresh)
    (hub/org-context-panel-open)))

;;;###autoload
(defun hub/org-comment-next ()
  "Jump to the next sidecar comment in the current Org buffer."
  (interactive)
  (let* ((point (point))
	 (comments (hub/org-comment--valid-comments))
	 (next (or (cl-find-if
		    (lambda (comment)
		      (> (hub/org-comment--target-start comment) point))
		    comments)
		   (car comments))))
    (hub/org-comment--goto next)))

;;;###autoload
(defun hub/org-comment-previous ()
  "Jump to the previous sidecar comment in the current Org buffer."
  (interactive)
  (let* ((point (point))
	 (comments (reverse (hub/org-comment--valid-comments)))
	 (previous (or (cl-find-if
			(lambda (comment)
			  (< (hub/org-comment--target-start comment) point))
			comments)
		       (car comments))))
    (hub/org-comment--goto previous)))

;;;###autoload
(defun hub/org-comment-jump-to-sidecar ()
  "Jump to the active sidecar comment heading."
  (interactive)
  (hub/org-comment--goto-sidecar-heading (hub/org-comment--active-at-point)))

;;;###autoload
(defun hub/org-comment-reanchor (start end &optional comment)
  "Reanchor stale COMMENT to source region START to END."
  (interactive "r")
  (unless (derived-mode-p 'org-mode)
    (user-error "Org comments only work in Org buffers"))
  (hub/org-comment--leave-visual-state)
  (let* ((source-buffer (current-buffer))
	 (target-comment (or comment (hub/org-comment--single-stale-comment)))
	 (record (hub/org-comment-create-record
		  buffer-file-name start end "" (plist-get target-comment :id))))
    (hub/org-comment-update-anchor
     (plist-get target-comment :sidecar-file)
     (plist-get target-comment :id)
     record)
    (with-current-buffer source-buffer
      (goto-char start)
      (hub/org-comment-overlays-refresh)
      (hub/org-context-panel-open))))

;;;###autoload
(defun hub/org-comment-edit ()
  "Edit the active sidecar comment body narrowed to its subtree."
  (interactive)
  (let ((comment (hub/org-comment--active-at-point)))
    (hub/org-comment--goto-sidecar-heading comment)
    (org-narrow-to-subtree)
    (hub/org-comment--goto-sidecar-body comment)))

;;;###autoload
(defun hub/org-comment-mark-open ()
  "Mark the active sidecar comment at point as open."
  (interactive)
  (hub/org-comment-set-status "OPEN"))

;;;###autoload
(defun hub/org-comment-mark-todo ()
  "Mark the active sidecar comment at point as requiring action."
  (interactive)
  (hub/org-comment-set-status "TODO"))

;;;###autoload
(defun hub/org-comment-mark-resolved ()
  "Mark the active sidecar comment at point as resolved."
  (interactive)
  (hub/org-comment-set-status "RESOLVED"))

;;;###autoload
(defun hub/org-comment-cycle-status ()
  "Cycle the active sidecar comment status through OPEN, TODO, and RESOLVED."
  (interactive)
  (let* ((comment (hub/org-comment--active-at-point))
	 (status (plist-get comment :status))
	 (next (pcase status
		 ("OPEN" "TODO")
		 ("TODO" "RESOLVED")
		 (_ "OPEN"))))
    (hub/org-comment--set-sidecar-status comment next)
    (hub/org-comment-overlays-refresh)
    (hub/org-context-panel-open)))

;;;###autoload
(defun hub/org-comment-create (start end &optional body)
  "Create a sidecar comment for region START to END with BODY."
  (interactive "r")
  (unless (derived-mode-p 'org-mode)
    (user-error "Org comments only work in Org buffers"))
  (unless buffer-file-name
    (user-error "Current buffer is not visiting a file"))
  (hub/org-comment--leave-visual-state)
  (let* ((source-buffer (current-buffer))
	 (comment-body (or body (read-string "Comment: ")))
	 (record (hub/org-comment-create-record buffer-file-name start end comment-body)))
    (hub/org-comment-append-to-sidecar record)
    (with-current-buffer source-buffer
      (goto-char start)
      (hub/org-context-panel-open))))

(provide 'org/comments)
;;; comments.el ends here
