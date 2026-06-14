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

(defun hub/org-comment--goto (comment)
  "Move point to COMMENT and open the context panel."
  (goto-char (hub/org-comment--target-start comment))
  (hub/org-comment-overlays-refresh)
  (hub/org-context-panel-open))

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
(defun hub/org-comment-create (start end body)
  "Create a sidecar comment for region START to END with BODY."
  (interactive
   (let ((bounds (hub/org-comment--region-bounds)))
     (list (car bounds) (cdr bounds) (read-string "Comment: "))))
  (unless (derived-mode-p 'org-mode)
    (user-error "Org comments only work in Org buffers"))
  (unless buffer-file-name
    (user-error "Current buffer is not visiting a file"))
  (let* ((source-buffer (current-buffer))
	 (record (hub/org-comment-create-record buffer-file-name start end body)))
    (hub/org-comment-append-to-sidecar record)
    (deactivate-mark)
    (when (and (bound-and-true-p evil-mode)
	       (fboundp 'evil-normal-state))
      (evil-normal-state))
    (with-current-buffer source-buffer
      (goto-char start)
      (hub/org-context-panel-open))))

(provide 'org/comments)
;;; comments.el ends here
