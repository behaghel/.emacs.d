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
(require 'org-comments-core)
(require 'org-comments-sidecar)
(require 'org-comments-ui)
(require 'org-google-docs-comments)
(require 'org-google-docs-comments-import)
(require 'subr-x)
(require 'url-util)

(defvar gdocs-api--drive-base-url)

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
  (let ((source-file (org-google-docs-comments-backend--source-file record "sync")))
    (org-google-docs-comments-backend--push-pending-statuses source-file)
    (org-google-docs-comments-backend-pull record)))

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

(defun org-google-docs-comments-backend--pending-status-comment
    (source-file sidecar-file document-id account)
  "Return pending resolved comment at point for SOURCE-FILE.
SIDECAR-FILE, DOCUMENT-ID, and ACCOUNT are copied into the returned record."
  (let ((status (org-get-todo-state))
	(sync-kind (org-entry-get nil "ORG_COMMENTS_SYNC_KIND"))
	(remote-id (org-entry-get nil "ORG_COMMENTS_REMOTE_ID"))
	(remote-status (org-entry-get nil "ORG_COMMENTS_REMOTE_RESOLUTION_STATUS"))
	(status-dirty (org-entry-get nil "ORG_COMMENTS_LOCAL_STATUS_DIRTY")))
    (when (and (not (equal sync-kind "reply"))
	       (equal status "RESOLVED")
	       (equal status-dirty "status")
	       remote-id
	       (not (string-empty-p remote-id))
	       (not (equal remote-status "resolved")))
      (list :source-file source-file
	    :sidecar-file sidecar-file
	    :document-id document-id
	    :account account
	    :id (org-entry-get nil "ORG_COMMENTS_ID")
	    :remote-id remote-id
	    :status status
	    :remote-resolution-status remote-status
	    :body (org-comments-entry-body
		   (save-excursion (org-end-of-subtree t t)))))))

(defun org-google-docs-comments-backend--pending-status-comments (source-file)
  "Return Google Docs comments with local status pending push for SOURCE-FILE."
  (let ((sidecar-file (org-comments-sidecar-path source-file)))
    (when (file-exists-p sidecar-file)
      (with-current-buffer (find-file-noselect source-file)
	(org-mode)
	(let ((document-id (org-google-docs-comments-document-id))
	      (account (org-google-docs-comments-account)))
	  (with-temp-buffer
	    (insert-file-contents sidecar-file)
	    (org-mode)
	    (let (comments)
	      (goto-char (point-min))
	      (while (re-search-forward org-heading-regexp nil t)
		(goto-char (match-beginning 0))
		(when-let* ((comment
			     (org-google-docs-comments-backend--pending-status-comment
			      source-file sidecar-file document-id account)))
		  (push comment comments))
		(forward-line 1))
	      (nreverse comments))))))))

(defun org-google-docs-comments-backend--push-pending-statuses (source-file)
  "Push pending local status changes for Google Docs SOURCE-FILE comments."
  (let ((comments (org-google-docs-comments-backend--pending-status-comments source-file)))
    (dolist (comment comments)
      (org-google-docs-comments-backend-set-status
       (plist-put (copy-sequence comment) :suppress-report t) "RESOLVED"))
    (when comments
      (org-comments-sync-report-message
       (list :provider "Google Docs" :pushed-statuses (length comments))))
    (list :pushed-statuses (length comments))))

(defun org-google-docs-comments-backend--sidecar-entry-info (comment)
  "Return sidecar entry metadata for COMMENT, or nil when not found."
  (when-let* ((sidecar-file (plist-get comment :sidecar-file)))
    (with-temp-buffer
      (insert-file-contents sidecar-file)
      (org-mode)
      (when (org-comments-sidecar-goto-comment comment)
	(list :id (org-entry-get nil "ORG_COMMENTS_ID")
	      :status (org-get-todo-state)
	      :sync-kind (org-entry-get nil "ORG_COMMENTS_SYNC_KIND")
	      :remote-id (org-entry-get nil "ORG_COMMENTS_REMOTE_ID")
	      :remote-resolution-status
	      (org-entry-get nil "ORG_COMMENTS_REMOTE_RESOLUTION_STATUS")
	      :local-status-dirty
	      (org-entry-get nil "ORG_COMMENTS_LOCAL_STATUS_DIRTY")
	      :body (org-comments-entry-body
		     (save-excursion (org-end-of-subtree t t))))))))


(defun org-google-docs-comments-backend--mark-sidecar-resolved (comment)
  "Mark COMMENT's sidecar entry resolved when sidecar metadata is available."
  (when-let* ((sidecar-file (plist-get comment :sidecar-file)))
    (with-temp-buffer
      (insert-file-contents sidecar-file)
      (org-mode)
      (unless (org-comments-sidecar-goto-comment comment)
	(user-error "Cannot find Google Docs comment in sidecar"))
      (let ((inhibit-message t)
	    (org-log-done nil)
	    (org-inhibit-logging t))
	(org-todo "RESOLVED"))
      (org-entry-put nil "ORG_COMMENTS_REMOTE_RESOLUTION_STATUS" "resolved")
      (org-entry-delete nil "ORG_COMMENTS_LOCAL_STATUS_DIRTY")
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
      (unless (org-comments-sidecar-goto-comment (list :id comment-id))
	(user-error "Cannot find Google Docs reply in sidecar"))
      (unless (equal (org-entry-get nil "ORG_COMMENTS_SYNC_KIND") "reply")
	(user-error "Google Docs remote reply push requires a sidecar reply"))
      (let ((parent-remote-id (org-entry-get nil "ORG_COMMENTS_REMOTE_PARENT_ID"))
	    (remote-id (org-entry-get nil "ORG_COMMENTS_REMOTE_ID"))
	    (body (org-comments-entry-body (save-excursion (org-end-of-subtree t t)))))
	(unless (and parent-remote-id (not (string-empty-p parent-remote-id)))
	  (user-error "Google Docs reply has no remote parent comment id"))
	(list :document-id document-id
	      :parent-remote-id parent-remote-id
	      :remote-id remote-id
	      :body body
	      :sidecar-file sidecar-file
	      :id comment-id)))))

(defun org-google-docs-comments-backend--require-upstream-api ()
  "Require upstream gdocs API helpers or signal an actionable error."
  (unless (require 'gdocs-api nil 'noerror)
    (user-error "Missing upstream gdocs-api; install and configure benthamite/gdocs")))

(defun org-google-docs-comments-backend--sidecar-body (comment)
  "Return COMMENT body from its sidecar metadata, or nil."
  (when-let* ((sidecar-file (plist-get comment :sidecar-file)))
    (with-temp-buffer
      (insert-file-contents sidecar-file)
      (org-mode)
      (when (org-comments-sidecar-goto-comment comment)
	(org-comments-entry-body (save-excursion (org-end-of-subtree t t)))))))

(defun org-google-docs-comments-backend--comment-content (comment)
  "Return non-empty Google Docs COMMENT content for update requests."
  (let ((content (string-trim
		  (or (plist-get comment :body)
		      (org-google-docs-comments-backend--sidecar-body comment)
		      ""))))
    (unless (not (string-empty-p content))
      (user-error "Google Docs comment content is required to resolve remotely"))
    content))

(defun org-google-docs-comments-backend--resolve-url (document-id comment-id)
  "Return Drive API replies URL for resolving COMMENT-ID on DOCUMENT-ID."
  (concat gdocs-api--drive-base-url "/"
	  (url-hexify-string document-id)
	  "/comments/"
	  (url-hexify-string comment-id)
	  "/replies?fields=id,action,content"))

(defun org-google-docs-comments-backend--resolve-comment
    (document-id comment-id _content callback &optional account)
  "Resolve COMMENT-ID on DOCUMENT-ID by creating a Drive reply action.
Google Drive's `comments.resolved' field is read-only; comments are resolved by
posting a reply with action `resolve'.  CALLBACK receives the created reply
payload."
  (org-google-docs-comments-backend--require-upstream-api)
  (gdocs-api--request
   'post
   (org-google-docs-comments-backend--resolve-url document-id comment-id)
   callback
   :account account
   :body (json-encode '((action . "resolve")))))

(defun org-google-docs-comments-backend--refresh-source-ui (comment)
  "Refresh visible comments UI from COMMENT's source buffer when available."
  (if-let* ((source-file (plist-get comment :source-file))
	    (source-buffer (find-buffer-visiting source-file)))
      (with-current-buffer source-buffer
	(org-comments-ui-refresh))
    (org-comments-ui-refresh)))

(defun org-google-docs-comments-backend-set-status (comment status)
  "Set Google Docs COMMENT to STATUS.
The initial backend only supports resolving remote comments."
  (unless (equal status "RESOLVED")
    (user-error "Google Docs comments backend only supports RESOLVED status for now"))
  (org-google-docs-comments-backend--require-upstream-api)
  (let ((document-id (org-google-docs-comments-backend--document-id comment))
	(remote-id (org-google-docs-comments-backend--comment-remote-id comment))
	(content (org-google-docs-comments-backend--comment-content comment))
	(account (org-google-docs-comments-backend--comment-account comment))
	result)
    (org-google-docs-comments-backend--resolve-comment
     document-id remote-id content
     (lambda (response)
       (setq result response)
       (org-google-docs-comments-backend--mark-sidecar-resolved comment)
       (org-google-docs-comments-backend--refresh-source-ui comment)
       (unless (plist-get comment :suppress-report)
	 (org-comments-sync-report-message
	  (list :provider "Google Docs" :resolved 1))))
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
      (unless (org-comments-sidecar-goto-comment (list :id comment-id))
	(user-error "Cannot find Google Docs reply in sidecar"))
      (org-entry-put nil "ORG_COMMENTS_REMOTE_ID" remote-id)
      (org-entry-put nil "ORG_COMMENTS_REMOTE_PARENT_ID"
		     (plist-get payload :parent-remote-id))
      (org-entry-put nil "ORG_COMMENTS_REMOTE_STATE" "present")
      (when-let* ((created-at (alist-get 'createdTime response)))
	(org-entry-put nil "ORG_COMMENTS_REMOTE_CREATED_AT" created-at))
      (when-let* ((updated-at (alist-get 'modifiedTime response)))
	(org-entry-put nil "ORG_COMMENTS_REMOTE_UPDATED_AT" updated-at))
      (write-region (point-min) (point-max) sidecar-file nil 'silent))))

(defun org-google-docs-comments-backend--push-root-status (comment)
  "Push pending root COMMENT status changes, or return nil when not applicable."
  (let* ((info (org-google-docs-comments-backend--sidecar-entry-info comment))
	 (sync-kind (plist-get info :sync-kind))
	 (remote-id (or (plist-get comment :remote-id)
			(plist-get info :remote-id)))
	 (status (or (plist-get comment :status) (plist-get info :status)))
	 (status-dirty (or (plist-get comment :local-status-dirty)
			   (plist-get info :local-status-dirty)))
	 (remote-status (or (plist-get comment :remote-resolution-status)
			    (plist-get info :remote-resolution-status))))
    (unless (equal sync-kind "reply")
      (unless (and remote-id (not (string-empty-p remote-id)))
	(user-error "Google Docs root comment creation is not supported yet"))
      (cond
       ((and (equal status "RESOLVED")
	     (equal status-dirty "status")
	     (not (equal remote-status "resolved")))
	(org-google-docs-comments-backend-set-status
	 (append comment
		 (list :remote-id remote-id
		       :body (or (plist-get comment :body) (plist-get info :body))))
	 "RESOLVED"))
       ((and (equal status "RESOLVED")
	     (equal remote-status "resolved"))
	(org-comments-sync-report-message
	 (list :provider "Google Docs" :already-pushed 1))
	(list :already-pushed t :remote-id remote-id :status "RESOLVED"))
       (t
	(org-comments-sync-report-message
	 (list :provider "Google Docs" :no-op 1))
	(list :no-op t :remote-id remote-id))))))

(defun org-google-docs-comments-backend-push (comment)
  "Push local Google Docs COMMENT changes to the remote service.
This supports local replies and pending local resolution of imported root
comments.  Creating new anchored root comments is intentionally deferred."
  (or (org-google-docs-comments-backend--push-root-status comment)
      (let* ((payload (org-google-docs-comments-backend--reply-payload comment))
	     (remote-id (plist-get payload :remote-id))
	     (account (or (plist-get comment :account)
			  (plist-get comment :gdocs-account)))
	     result)
	(if (and remote-id (not (string-empty-p remote-id)))
	    (progn
	      (org-comments-sync-report-message
	       (list :provider "Google Docs" :already-pushed 1))
	      (list :already-pushed t :remote-id remote-id))
	  (org-google-docs-comments-backend--create-reply
	   (plist-get payload :document-id)
	   (plist-get payload :parent-remote-id)
	   (plist-get payload :body)
	   (lambda (response)
	     (setq result response)
	     (org-google-docs-comments-backend--record-reply-remote-id payload response)
	     (org-comments-sync-report-message
	      (list :provider "Google Docs" :pushed-replies 1)))
	   account)
	  result))))

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
