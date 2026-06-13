;;; comments.el --- Org sidecar comment commands -*- lexical-binding: t; -*-

;;; Commentary:
;; Interactive commands for region-targeted Org comments stored in colocated
;; plain-Org sidecar files.

;;; Code:

(require 'hub-org-comments)
(require 'org/context-panel)
(require 'org)

(defun hub/org-comment--region-bounds ()
  "Return active region bounds or signal a user error."
  (unless (use-region-p)
    (user-error "Select a region to comment on"))
  (cons (region-beginning) (region-end)))

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
      (goto-char end)
      (hub/org-context-panel-open))))

(provide 'org/comments)
;;; comments.el ends here
