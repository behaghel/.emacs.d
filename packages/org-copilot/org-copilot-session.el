;;; org-copilot-session.el --- Ephemeral sessions for Org Copilot -*- lexical-binding: t; -*-

;; Author: Hubert Behaghel
;; Maintainer: Hubert Behaghel
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (org "9.6"))
;; Keywords: outlines, tools, convenience
;; URL: https://github.com/behaghel/org-copilot

;;; Commentary:
;; Buffer-local session state for Org Copilot AI comments.

;;; Code:

(require 'cl-lib)
(require 'org-copilot-model)
(require 'org-copilot-sidecar nil 'noerror)
(require 'org-comments-sidecar nil 'noerror)
(require 'org-suggestions nil 'noerror)

(defvar-local org-copilot--comments nil
  "Ephemeral AI comments for the current Org source buffer.")

(defvar-local org-copilot--chat-messages nil
  "Ephemeral chat messages for the current Org source buffer.")

(defvar-local org-copilot--chat-messages-restored-p nil
  "Non-nil when durable chat messages have been restored for this buffer.")

(defvar org-copilot--suppress-chat-persistence nil
  "Non-nil while restoring chat messages from durable storage.")

(defvar-local org-copilot-chat-focus-comment-id nil
  "AI comment id currently focused by the Org Copilot chat view.")

(defvar-local org-copilot-chat-context '(:type full-document)
  "Current Org Copilot chat context plist.
The context has a `:type' key.  Supported values are `full-document',
`comment', and `section'.  `org-copilot-chat-focus-comment-id' mirrors
comment contexts for compatibility with older actions and tests.")

(defun org-copilot-comments ()
  "Return ephemeral AI comments for the current buffer."
  (copy-sequence org-copilot--comments))

(defun org-copilot-add-comment (comment)
  "Add AI COMMENT to the current buffer session and return it.
COMMENT is normalized before insertion.  Adding another comment with the same
`:id' replaces the previous comment while preserving list order for other
comments."
  (let* ((normalized (org-copilot-normalize-comment comment))
	 (id (org-copilot-comment-id normalized)))
    (setq org-copilot--comments
	  (append (cl-remove id org-copilot--comments
			     :key #'org-copilot-comment-id
			     :test #'equal)
		  (list normalized)))
    normalized))

(defun org-copilot-set-comments (comments)
  "Replace current buffer session with normalized AI COMMENTS.
Return the normalized comments."
  (setq org-copilot--comments nil)
  (dolist (comment comments)
    (org-copilot-add-comment comment))
  (org-copilot-comments))

(defun org-copilot-find-comment (id)
  "Return AI comment ID from the current buffer session, or nil."
  (cl-find id org-copilot--comments
	   :key #'org-copilot-comment-id
	   :test #'equal))

(defun org-copilot-update-comment (comment)
  "Replace AI COMMENT in the current buffer session and return it."
  (let* ((normalized (org-copilot-normalize-comment comment))
	 (id (org-copilot-comment-id normalized)))
    (unless (org-copilot-find-comment id)
      (error "No Org Copilot comment with id: %S" id))
    (setq org-copilot--comments
	  (mapcar (lambda (entry)
		    (if (equal (org-copilot-comment-id entry) id)
			normalized
		      entry))
		  org-copilot--comments))
    normalized))

(defun org-copilot-remove-comment (comment-or-id)
  "Remove COMMENT-OR-ID from the current buffer session."
  (let ((id (if (listp comment-or-id)
		(org-copilot-comment-id comment-or-id)
	      comment-or-id)))
    (setq org-copilot--comments
	  (cl-remove id org-copilot--comments
		     :key #'org-copilot-comment-id
		     :test #'equal))))

(defun org-copilot-chat-messages ()
  "Return ephemeral chat messages for the current buffer."
  (copy-sequence org-copilot--chat-messages))

(defun org-copilot-set-chat-messages (messages)
  "Replace current buffer chat transcript with MESSAGES."
  (setq org-copilot--chat-messages messages))

(defun org-copilot-restore-chat-messages ()
  "Restore durable chat messages for the current buffer once."
  (when (and (not org-copilot--chat-messages-restored-p)
	     buffer-file-name
	     (fboundp 'org-copilot-sidecar-load-messages))
    (setq org-copilot--chat-messages-restored-p t)
    (when-let* ((messages (org-copilot-sidecar-load-messages buffer-file-name)))
      (let ((org-copilot--suppress-chat-persistence t))
	(org-copilot-set-chat-messages messages)))))

(defun org-copilot-add-chat-message (role content &optional comment-id context-id)
  "Add a chat message with ROLE and CONTENT to the current buffer session.
When COMMENT-ID is non-nil, associate the message with that AI comment.
When CONTEXT-ID is non-nil, associate the message with another chat context."
  (let ((message (list :role role
		       :content content
		       :comment-id comment-id
		       :context-id context-id
		       :created-at (format-time-string "%FT%T%z"))))
    (setq org-copilot--chat-messages
	  (append org-copilot--chat-messages (list message)))
    (when (and buffer-file-name
	       (not org-copilot--suppress-chat-persistence)
	       (memq role '(user assistant))
	       (fboundp 'org-copilot-sidecar-append-message))
      (org-copilot-sidecar-append-message
       buffer-file-name role content
       (list "ORG_COPILOT_CREATED_AT" (plist-get message :created-at)
	     "ORG_COPILOT_CONTEXT_ID" context-id
	     "ORG_COPILOT_COMMENT_ID" comment-id)))
    message))

(defun org-copilot-remove-pending-chat-message (&optional comment-id context-id)
  "Remove the last pending chat message for COMMENT-ID and CONTEXT-ID.
When COMMENT-ID and CONTEXT-ID are nil, remove the last pending message for
full-document chat."
  (let ((removed nil))
    (setq org-copilot--chat-messages
	  (nreverse
	   (cl-remove-if
	    (lambda (message)
	      (and (not removed)
		   (eq (plist-get message :role) 'pending)
		   (equal (plist-get message :comment-id) comment-id)
		   (equal (plist-get message :context-id) context-id)
		   (setq removed t)))
	    (reverse org-copilot--chat-messages))))))

(defun org-copilot--current-session-id ()
  "Return current Copilot session id."
  "default")

(defun org-copilot--archive-session-artifacts (source-file session-id)
  "Archive durable Copilot artifacts for SOURCE-FILE SESSION-ID."
  (when (and source-file (fboundp 'org-copilot-sidecar-archive-session))
    (org-copilot-sidecar-archive-session source-file session-id))
  (when (and source-file (fboundp 'org-comments-archive-provider-session-comments))
    (let ((comments-sidecar (org-comments-sidecar-path source-file)))
      (when (file-exists-p comments-sidecar)
	(org-comments-archive-provider-session-comments
	 comments-sidecar "org-copilot" session-id))))
  (when (and source-file
	     (fboundp 'org-suggestions-archive-provider-session-threads))
    (let ((suggestions-sidecar (org-suggestions-sidecar-path source-file)))
      (when (file-exists-p suggestions-sidecar)
	(org-suggestions-archive-provider-session-threads
	 suggestions-sidecar "org-copilot" session-id)))))

(defun org-copilot--delete-session-artifacts (source-file session-id)
  "Delete durable Copilot artifacts for SOURCE-FILE SESSION-ID."
  (when (and source-file (fboundp 'org-copilot-sidecar-delete-session))
    (org-copilot-sidecar-delete-session source-file session-id))
  (when (and source-file (fboundp 'org-comments-delete-provider-session-comments))
    (let ((comments-sidecar (org-comments-sidecar-path source-file)))
      (when (file-exists-p comments-sidecar)
	(org-comments-delete-provider-session-comments
	 comments-sidecar "org-copilot" session-id))))
  (when (and source-file
	     (fboundp 'org-suggestions-delete-provider-session-threads))
    (let ((suggestions-sidecar (org-suggestions-sidecar-path source-file)))
      (when (file-exists-p suggestions-sidecar)
	(org-suggestions-delete-provider-session-threads
	 suggestions-sidecar "org-copilot" session-id)))))

(defun org-copilot--clear-ephemeral-session ()
  "Clear ephemeral Org Copilot state for the current buffer."
  (setq org-copilot--comments nil)
  (setq org-copilot--chat-messages nil)
  (setq org-copilot--chat-messages-restored-p t)
  (setq org-copilot-chat-focus-comment-id nil)
  (setq org-copilot-chat-context '(:type full-document))
  (when (fboundp 'org-copilot-delete-overlays)
    (org-copilot-delete-overlays))
  (when (and (boundp 'org-copilot-panel-buffer-name)
	     (get-buffer org-copilot-panel-buffer-name)
	     (fboundp 'org-context-panel-refresh))
    (org-context-panel-refresh)))

(defun org-copilot-clear-session (&optional preserve-artifacts)
  "Clear current Copilot session.
By default, archive durable Copilot chat, comments, and suggestions for the
current session.  With PRESERVE-ARTIFACTS, only clear ephemeral UI state."
  (interactive "P")
  (let ((source-file buffer-file-name)
	(session-id (org-copilot--current-session-id)))
    (unless preserve-artifacts
      (org-copilot--archive-session-artifacts source-file session-id))
    (org-copilot--clear-ephemeral-session)))

(defun org-copilot-erase-session ()
  "Hard-delete durable artifacts for the current Copilot session."
  (interactive)
  (when (yes-or-no-p "Hard-delete current Org Copilot session artifacts? ")
    (org-copilot--delete-session-artifacts
     buffer-file-name (org-copilot--current-session-id))
    (org-copilot--clear-ephemeral-session)))

(provide 'org-copilot-session)
;;; org-copilot-session.el ends here
