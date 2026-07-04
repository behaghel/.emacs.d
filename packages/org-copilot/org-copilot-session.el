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

(defvar-local org-copilot--comments nil
  "Ephemeral AI comments for the current Org source buffer.")

(defvar-local org-copilot--chat-messages nil
  "Ephemeral chat messages for the current Org source buffer.")

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

(defun org-copilot-clear-session ()
  "Clear ephemeral Org Copilot session state for the current buffer."
  (interactive)
  (setq org-copilot--comments nil)
  (setq org-copilot--chat-messages nil)
  (setq org-copilot-chat-focus-comment-id nil)
  (setq org-copilot-chat-context '(:type full-document))
  (when (fboundp 'org-copilot-delete-overlays)
    (org-copilot-delete-overlays))
  (when (and (boundp 'org-copilot-panel-buffer-name)
	     (get-buffer org-copilot-panel-buffer-name)
	     (fboundp 'org-context-panel-refresh))
    (org-context-panel-refresh)))

(provide 'org-copilot-session)
;;; org-copilot-session.el ends here
