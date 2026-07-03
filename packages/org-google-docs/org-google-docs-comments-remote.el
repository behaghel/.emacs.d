;;; org-google-docs-comments-remote.el --- Google Docs comment payload helpers -*- lexical-binding: t; -*-

;;; Commentary:
;; Helpers for normalizing Google Drive Comments API payloads into a stable
;; adapter-local plist shape.  These helpers are pure and network-free so later
;; import/sync code can be tested independently of Google APIs.

;;; Code:

(require 'subr-x)

(defun org-google-docs-comments-remote--alist-value (key alist)
  "Return KEY value from ALIST, trying symbol and string key spellings."
  (or (alist-get key alist)
      (alist-get (symbol-name key) alist nil nil #'equal)))

(defun org-google-docs-comments-remote-id (comment)
  "Return Google COMMENT id as a string, or nil."
  (when-let* ((id (org-google-docs-comments-remote--alist-value 'id comment)))
    (format "%s" id)))

(defun org-google-docs-comments-remote-body (comment)
  "Return Google COMMENT body text, or an empty string."
  (or (org-google-docs-comments-remote--alist-value 'content comment) ""))

(defun org-google-docs-comments-remote-quoted-text (comment)
  "Return Google COMMENT quoted target text, or nil."
  (let ((quoted (org-google-docs-comments-remote--alist-value 'quotedFileContent comment)))
    (when (listp quoted)
      (org-google-docs-comments-remote--alist-value 'value quoted))))

(defun org-google-docs-comments-remote-author-name (comment)
  "Return Google COMMENT author display name, or nil."
  (let ((author (org-google-docs-comments-remote--alist-value 'author comment)))
    (when (listp author)
      (or (org-google-docs-comments-remote--alist-value 'displayName author)
	  (org-google-docs-comments-remote--alist-value 'me author)))))

(defun org-google-docs-comments-remote-author-email (comment)
  "Return Google COMMENT author email address, or nil."
  (let ((author (org-google-docs-comments-remote--alist-value 'author comment)))
    (when (listp author)
      (org-google-docs-comments-remote--alist-value 'emailAddress author))))

(defun org-google-docs-comments-remote-created-at (comment)
  "Return Google COMMENT creation timestamp, or nil."
  (org-google-docs-comments-remote--alist-value 'createdTime comment))

(defun org-google-docs-comments-remote-updated-at (comment)
  "Return Google COMMENT modified timestamp, or nil."
  (or (org-google-docs-comments-remote--alist-value 'modifiedTime comment)
      (org-google-docs-comments-remote-created-at comment)))

(defun org-google-docs-comments-remote-resolved-p (comment)
  "Return non-nil when Google COMMENT is resolved."
  (eq (org-google-docs-comments-remote--alist-value 'resolved comment) t))

(defun org-google-docs-comments-remote-status (comment)
  "Return normalized status string for Google COMMENT."
  (if (org-google-docs-comments-remote-resolved-p comment)
      "resolved"
    "open"))

(defun org-google-docs-comments-remote-replies (comment)
  "Return Google COMMENT replies as a list."
  (let ((replies (org-google-docs-comments-remote--alist-value 'replies comment)))
    (cond
     ((vectorp replies) (append replies nil))
     ((listp replies) replies))))

(defun org-google-docs-comments-remote-normalize-reply (reply)
  "Normalize Google comment REPLY into an adapter plist."
  (list :backend 'google-docs
	:kind 'reply
	:remote-id (org-google-docs-comments-remote-id reply)
	:body (org-google-docs-comments-remote-body reply)
	:author-name (org-google-docs-comments-remote-author-name reply)
	:author-email (org-google-docs-comments-remote-author-email reply)
	:created-at (org-google-docs-comments-remote-created-at reply)
	:updated-at (org-google-docs-comments-remote-updated-at reply)
	:status (org-google-docs-comments-remote-status reply)))

(defun org-google-docs-comments-remote-normalize (comment)
  "Normalize Google Drive COMMENT payload into an adapter plist."
  (list :backend 'google-docs
	:kind 'comment
	:remote-id (org-google-docs-comments-remote-id comment)
	:body (org-google-docs-comments-remote-body comment)
	:target-text (org-google-docs-comments-remote-quoted-text comment)
	:author-name (org-google-docs-comments-remote-author-name comment)
	:author-email (org-google-docs-comments-remote-author-email comment)
	:created-at (org-google-docs-comments-remote-created-at comment)
	:updated-at (org-google-docs-comments-remote-updated-at comment)
	:status (org-google-docs-comments-remote-status comment)
	:replies (mapcar #'org-google-docs-comments-remote-normalize-reply
			 (org-google-docs-comments-remote-replies comment))))

(provide 'org-google-docs-comments-remote)
;;; org-google-docs-comments-remote.el ends here
