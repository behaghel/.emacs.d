;;; org-google-docs-comments-backend.el --- Org comments backend for Google Docs -*- lexical-binding: t; -*-

;;; Commentary:
;; Thin registration layer between org-google-docs and the generic org-comments
;; backend registry.  The initial backend is read-only: pull/sync import remote
;; Google Docs comments into sidecars and open-remote browses the linked Doc.

;;; Code:

(require 'browse-url)
(require 'cl-lib)
(require 'json)
(require 'org)
(require 'org-comments-backend)
(require 'org-comments-sidecar)
(require 'org-google-docs-comments)
(require 'org-google-docs-comments-import)
(require 'subr-x)
(require 'url-util)

(defun org-google-docs-comments-backend--source-file (record operation)
  "Return source file from RECORD for OPERATION or signal a user error."
  (or (plist-get record :source-file)
      (plist-get record :file)
      (user-error "Google Docs %s needs :source-file" operation)))

(defun org-google-docs-comments-backend-pull (record)
  "Pull Google Docs comments into RECORD's source Org sidecar.
RECORD is a plist containing `:source-file'.  Optional `:include-resolved'
controls whether resolved comments are imported."
  (let ((source-file (org-google-docs-comments-backend--source-file record "pull")))
    (with-current-buffer (find-file-noselect source-file)
      (org-mode)
      (org-google-docs-comments-import (plist-get record :include-resolved)))))

(defun org-google-docs-comments-backend-sync (record)
  "Synchronize Google Docs comments for RECORD without syncing document body."
  (org-google-docs-comments-backend-pull record))

(defun org-google-docs-comments-backend--source-document-id (comment)
  "Return Google Docs document id from COMMENT's source file, or nil."
  (when-let* ((source-file (plist-get comment :source-file)))
    (with-current-buffer (find-file-noselect source-file)
      (org-mode)
      (org-google-docs-comments-document-id))))

(defun org-google-docs-comments-backend--document-id (comment)
  "Return Google Docs document id for COMMENT or signal a user error."
  (or (plist-get comment :document-id)
      (plist-get comment :gdocs-document-id)
      (org-google-docs-comments-backend--source-document-id comment)
      (user-error "Google Docs comment has no document id")))

(defun org-google-docs-comments-backend--document-url (document-id &optional comment-id)
  "Return browser URL for DOCUMENT-ID and optional COMMENT-ID."
  (concat "https://docs.google.com/document/d/"
	  (url-hexify-string document-id)
	  "/edit"
	  (when (and comment-id (not (string-empty-p comment-id)))
	    (concat "?disco=" (url-hexify-string comment-id)))))

(defun org-google-docs-comments-backend-open-remote (comment)
  "Open remote Google Docs COMMENT in a browser and return its URL."
  (let* ((document-id (org-google-docs-comments-backend--document-id comment))
	 (comment-id (or (plist-get comment :remote-id)
			 (plist-get comment :comment-id)
			 (plist-get comment :remote-comment-id)))
	 (url (org-google-docs-comments-backend--document-url document-id comment-id)))
    (browse-url url)
    url))

(defun org-google-docs-comments-backend--comment-remote-id (comment)
  "Return Google Docs remote comment id from COMMENT or signal a user error."
  (or (plist-get comment :remote-id)
      (plist-get comment :comment-id)
      (plist-get comment :remote-comment-id)
      (user-error "Google Docs comment has no remote comment id")))

(defun org-google-docs-comments-backend--comment-account (comment)
  "Return Google Docs account from COMMENT or its source file, or nil."
  (or (plist-get comment :account)
      (plist-get comment :gdocs-account)
      (when-let* ((source-file (plist-get comment :source-file)))
	(with-current-buffer (find-file-noselect source-file)
	  (org-mode)
	  (org-google-docs-comments-account)))))

(defun org-google-docs-comments-backend--goto-sidecar-comment (comment)
  "Move to COMMENT in its sidecar buffer and return non-nil when found."
  (let ((comment-id (plist-get comment :id))
	(remote-id (plist-get comment :remote-id)))
    (goto-char (point-min))
    (cl-loop while (re-search-forward org-heading-regexp nil t)
	     do (goto-char (match-beginning 0))
	     when (or (and comment-id
			   (equal comment-id (org-entry-get nil "ORG_COMMENTS_ID")))
		      (and remote-id
			   (equal remote-id (org-entry-get nil "ORG_COMMENTS_REMOTE_ID"))))
	     return t
	     do (forward-line 1))))

(defun org-google-docs-comments-backend--mark-sidecar-resolved (comment)
  "Mark COMMENT's sidecar entry resolved when sidecar metadata is available."
  (when-let* ((sidecar-file (plist-get comment :sidecar-file)))
    (with-temp-buffer
      (insert-file-contents sidecar-file)
      (org-mode)
      (unless (org-google-docs-comments-backend--goto-sidecar-comment comment)
	(user-error "Cannot find Google Docs comment in sidecar"))
      (org-todo "RESOLVED")
      (org-entry-put nil "ORG_COMMENTS_REMOTE_RESOLUTION_STATUS" "resolved")
      (write-region (point-min) (point-max) sidecar-file nil 'silent))))

(defun org-google-docs-comments-backend--reply-payload (comment)
  "Return remote reply payload data for local sidecar reply COMMENT."
  (let ((document-id (org-google-docs-comments-backend--document-id comment))
	(sidecar-file (or (plist-get comment :sidecar-file)
			  (user-error "Google Docs reply needs :sidecar-file")))
	(comment-id (or (plist-get comment :id)
			(user-error "Google Docs reply needs :id"))))
    (with-temp-buffer
      (insert-file-contents sidecar-file)
      (org-mode)
      (unless (org-google-docs-comments-backend--goto-sidecar-comment
	       (list :id comment-id))
	(user-error "Cannot find Google Docs reply in sidecar"))
      (unless (equal (org-entry-get nil "ORG_COMMENTS_SYNC_KIND") "reply")
	(user-error "Google Docs remote reply push requires a sidecar reply"))
      (let ((parent-remote-id (org-entry-get nil "ORG_COMMENTS_REMOTE_PARENT_ID"))
	    (body (org-comments-entry-body (save-excursion (org-end-of-subtree t t)))))
	(unless (and parent-remote-id (not (string-empty-p parent-remote-id)))
	  (user-error "Google Docs reply has no remote parent comment id"))
	(list :document-id document-id
	      :parent-remote-id parent-remote-id
	      :body body
	      :sidecar-file sidecar-file
	      :id comment-id)))))

(defun org-google-docs-comments-backend--require-upstream-api ()
  "Require upstream gdocs API helpers or signal an actionable error."
  (unless (require 'gdocs-api nil 'noerror)
    (user-error "Missing upstream gdocs-api; install and configure benthamite/gdocs")))

(defun org-google-docs-comments-backend-set-status (comment status)
  "Set Google Docs COMMENT to STATUS.
The initial backend only supports resolving remote comments."
  (unless (equal status "RESOLVED")
    (user-error "Google Docs comments backend only supports RESOLVED status for now"))
  (org-google-docs-comments-backend--require-upstream-api)
  (let ((document-id (org-google-docs-comments-backend--document-id comment))
	(remote-id (org-google-docs-comments-backend--comment-remote-id comment))
	(account (org-google-docs-comments-backend--comment-account comment))
	result)
    (gdocs-api-resolve-comment
     document-id remote-id
     (lambda (response)
       (setq result response)
       (org-google-docs-comments-backend--mark-sidecar-resolved comment))
     account)
    result))

(defun org-google-docs-comments-backend--reply-url (document-id parent-id)
  "Return Drive API URL for replies under PARENT-ID on DOCUMENT-ID."
  (concat gdocs-api--drive-base-url "/"
	  (url-hexify-string document-id)
	  "/comments/"
	  (url-hexify-string parent-id)
	  "/replies?fields=id,content,author,createdTime,modifiedTime"))

(defun org-google-docs-comments-backend--create-reply
    (document-id parent-id body callback &optional account)
  "Create a Google Docs reply under PARENT-ID on DOCUMENT-ID.
BODY is the reply text.  CALLBACK receives the created reply payload.  ACCOUNT
is forwarded to upstream gdocs request helpers."
  (org-google-docs-comments-backend--require-upstream-api)
  (gdocs-api--request
   'post
   (org-google-docs-comments-backend--reply-url document-id parent-id)
   callback
   :account account
   :body (json-encode `((content . ,body)))))

(defun org-google-docs-comments-backend--record-reply-remote-id (payload response)
  "Record remote reply RESPONSE metadata in sidecar reply described by PAYLOAD."
  (let ((remote-id (alist-get 'id response))
	(sidecar-file (plist-get payload :sidecar-file))
	(comment-id (plist-get payload :id)))
    (unless remote-id
      (user-error "Google Docs reply response did not include id"))
    (with-temp-buffer
      (insert-file-contents sidecar-file)
      (org-mode)
      (unless (org-google-docs-comments-backend--goto-sidecar-comment
	       (list :id comment-id))
	(user-error "Cannot find Google Docs reply in sidecar"))
      (org-entry-put nil "ORG_COMMENTS_REMOTE_ID" remote-id)
      (org-entry-put nil "ORG_COMMENTS_REMOTE_PARENT_ID"
		     (plist-get payload :parent-remote-id))
      (when-let* ((created-at (alist-get 'createdTime response)))
	(org-entry-put nil "ORG_COMMENTS_REMOTE_CREATED_AT" created-at))
      (when-let* ((updated-at (alist-get 'modifiedTime response)))
	(org-entry-put nil "ORG_COMMENTS_REMOTE_UPDATED_AT" updated-at))
      (write-region (point-min) (point-max) sidecar-file nil 'silent))))

(defun org-google-docs-comments-backend-push (comment)
  "Push local Google Docs COMMENT changes to the remote service.
The initial push implementation supports sidecar replies only.  Creating new
anchored root comments is intentionally deferred."
  (let* ((payload (org-google-docs-comments-backend--reply-payload comment))
	 (account (or (plist-get comment :account)
		      (plist-get comment :gdocs-account)))
	 result)
    (org-google-docs-comments-backend--create-reply
     (plist-get payload :document-id)
     (plist-get payload :parent-remote-id)
     (plist-get payload :body)
     (lambda (response)
       (setq result response)
       (org-google-docs-comments-backend--record-reply-remote-id payload response))
     account)
    result))

(defun org-google-docs-comments-backend-detect (&optional source-buffer)
  "Return non-nil when SOURCE-BUFFER is linked to a Google Doc."
  (with-current-buffer (or source-buffer (current-buffer))
    (and (derived-mode-p 'org-mode)
	 (org-google-docs-comments-document-id))))

;;;###autoload
(defun org-google-docs-comments-backend-register ()
  "Register the Google Docs backend with `org-comments'."
  (org-comments-register-backend
   'google-docs
   (list :name "Google Docs"
	 :capabilities '(:sync :pull :open-remote :set-status :push)
	 :sync #'org-google-docs-comments-backend-sync
	 :pull #'org-google-docs-comments-backend-pull
	 :open-remote #'org-google-docs-comments-backend-open-remote
	 :set-status #'org-google-docs-comments-backend-set-status
	 :push #'org-google-docs-comments-backend-push))
  (org-comments-register-backend-detector
   'google-docs #'org-google-docs-comments-backend-detect))

(org-google-docs-comments-backend-register)

(provide 'org-google-docs-comments-backend)
;;; org-google-docs-comments-backend.el ends here
