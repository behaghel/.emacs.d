;;; org-comments-backend-org.el --- Local Org sidecar backend -*- lexical-binding: t; -*-

;;; Commentary:
;; Default local backend for Org comments.  This backend delegates to the
;; existing sidecar implementation while the package API is being extracted.

;;; Code:

(require 'org-comments-backend)
(require 'org-comments-store)

(defun org-comments-backend-org-list (&optional source-buffer include-stale)
  "List local sidecar comments for SOURCE-BUFFER.
When INCLUDE-STALE is non-nil, include stale local sidecar comments."
  (org-comments-collect source-buffer include-stale))

(defun org-comments-backend-org-list-page (&optional source-buffer _include-stale)
  "List local page-level sidecar comments for SOURCE-BUFFER."
  (org-comments-collect-page source-buffer))

(defun org-comments-backend-org-create (record)
  "Create local sidecar comment from RECORD and return its sidecar file."
  (org-comments-append-to-sidecar record))

(defun org-comments-backend-org-delete (comment)
  "Delete local sidecar COMMENT.
COMMENT may be a comment plist with :sidecar-file and :id, or a cons cell of
sidecar file and comment id."
  (pcase-let ((`(,sidecar-file . ,comment-id)
	       (if (consp comment)
		   comment
		 (cons (plist-get comment :sidecar-file)
		       (plist-get comment :id)))))
    (unless (and sidecar-file comment-id)
      (user-error "Local Org backend delete needs sidecar file and comment id"))
    (org-comments-delete-entry sidecar-file comment-id)))

(org-comments-register-backend
 'org
 '(:name "Local Org sidecar"
	 :capabilities (:list-comments :list-page-comments :create-inline :delete)
	 :list org-comments-backend-org-list
	 :list-page org-comments-backend-org-list-page
	 :create org-comments-backend-org-create
	 :delete org-comments-backend-org-delete))

(provide 'org-comments-backend-org)
;;; org-comments-backend-org.el ends here
