;;; org-confluence-comments-push.el --- Push Confluence comments -*- lexical-binding: t; -*-

;;; Commentary:
;; Push local Org sidecar comments to Confluence.  This module owns the
;; package-native `org-confluence-comments-push-current' entrypoint and its
;; lower-level sidecar validation, storage conversion, create, and update
;; helpers.

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'org-comments)
(require 'org-comments-anchors)
(require 'org-comments-sidecar)
(require 'org-confluence-api)
(require 'org-confluence-comments-context)
(require 'org-confluence-comments-import)
(require 'org-confluence-comments-remote)
(require 'org-confluence-people)
(require 'org-confluence-response)
(require 'ox-html)

(defun org-confluence-comments-push-sanitize-storage (html)
  "Return Confluence-friendly storage XHTML from Org-exported HTML."
  (let ((storage (string-trim html)))
    (setq storage (replace-regexp-in-string "<b>" "<strong>" storage t t))
    (setq storage (replace-regexp-in-string "</b>" "</strong>" storage t t))
    (setq storage (replace-regexp-in-string "<i>" "<em>" storage t t))
    (setq storage (replace-regexp-in-string "</i>" "</em>" storage t t))
    (setq storage (replace-regexp-in-string "<ul class=\"org-ul\">" "<ul>" storage t t))
    (setq storage (replace-regexp-in-string "<ol class=\"org-ol\">" "<ol>" storage t t))
    (setq storage (replace-regexp-in-string "\n+" " " storage t t))
    (setq storage (replace-regexp-in-string "<p>[[:space:]]+" "<p>" storage t t))
    (setq storage (replace-regexp-in-string "[[:space:]]+</p>" "</p>" storage t t))
    (setq storage (replace-regexp-in-string "</p>[[:space:]]+<" "</p><" storage t t))
    (setq storage (replace-regexp-in-string "</ul>[[:space:]]+<" "</ul><" storage t t))
    (setq storage (replace-regexp-in-string "</ol>[[:space:]]+<" "</ol><" storage t t))
    (setq storage (replace-regexp-in-string "<ul>[[:space:]]+" "<ul>" storage t t))
    (setq storage (replace-regexp-in-string "[[:space:]]+</ul>" "</ul>" storage t t))
    (setq storage (replace-regexp-in-string "<ol>[[:space:]]+" "<ol>" storage t t))
    (setq storage (replace-regexp-in-string "[[:space:]]+</ol>" "</ol>" storage t t))
    (setq storage (replace-regexp-in-string ">[[:space:]]+</li>" "></li>" storage t t))
    (setq storage (replace-regexp-in-string "</li>[[:space:]]+<li" "</li><li" storage t t))
    (string-trim storage)))

(defun org-confluence-comments-push-body-to-storage (body)
  "Return Confluence storage XHTML for sidecar Org comment BODY.
Supported authoring syntax includes paragraphs, basic inline markup, links, and
plain Org ordered/unordered lists."
  (if (string-empty-p (string-trim (or body "")))
      ""
    (org-confluence-comments-push-sanitize-storage
     (org-export-string-as
      (string-trim body) 'html t
      '(:with-toc nil :section-numbers nil :with-title nil)))))

(defun org-confluence-comments-push-current-body ()
  "Return body text for the current sidecar comment heading."
  (org-comments-entry-body (save-excursion (org-end-of-subtree t t))))

(defun org-confluence-comments-push-current-sync-kind ()
  "Return current sidecar comment sync kind for push."
  (let ((kind (org-entry-get nil "ORG_COMMENTS_SYNC_KIND"))
	(target (org-entry-get nil "ORG_COMMENTS_TARGET_TEXT")))
    (cond
     ((member kind '("footer" "inline" "reply")) kind)
     (target "inline")
     (t nil))))

(defun org-confluence-comments-push-root-info ()
  "Return plist for nearest root sidecar comment ancestor."
  (save-excursion
    (while (> (org-outline-level) 1)
      (org-up-heading-safe))
    (list :remote-id (org-entry-get nil "ORG_COMMENTS_REMOTE_ID")
	  :sync-kind (org-entry-get nil "ORG_COMMENTS_SYNC_KIND"))))

(defun org-confluence-comments-push-ensure-current-pushable ()
  "Validate the current sidecar heading for Confluence comment push.
Return the comment sync kind, one of `footer', `inline', or `reply'."
  (unless (and buffer-file-name
	       (string-suffix-p ".comments.org" buffer-file-name))
    (user-error "Current buffer is not a comments sidecar"))
  (unless (derived-mode-p 'org-mode)
    (org-mode))
  (when (org-before-first-heading-p)
    (user-error "Point is not on a sidecar comment"))
  (org-back-to-heading t)
  (unless (org-entry-get nil "ORG_COMMENTS_ID")
    (user-error "Point is not on a sidecar comment"))
  (when (and (> (org-outline-level) 1)
	     (not (equal "reply" (org-entry-get nil "ORG_COMMENTS_SYNC_KIND"))))
    (user-error "Child comments must be marked with ORG_COMMENTS_SYNC_KIND: reply"))
  (when (and (org-entry-get nil "ORG_COMMENTS_REMOTE_ID")
	     (not (org-entry-get nil "ORG_COMMENTS_LOCAL_UPDATED_AT")))
    (user-error "Comment is already linked to Confluence and has no local edits to update"))
  (when (equal "RESOLVED" (org-get-todo-state))
    (user-error "Refusing to push RESOLVED local comment; reopen it first with OPEN or TODO"))
  (when (string-empty-p (string-trim (org-confluence-comments-push-current-body)))
    (user-error "Cannot push an empty sidecar comment"))
  (let ((sync-kind (or (org-confluence-comments-push-current-sync-kind)
		       (user-error "Comment is not marked as a footer/page, inline, or reply comment"))))
    (when (equal sync-kind "reply")
      (let* ((root (org-confluence-comments-push-root-info))
	     (parent-id (plist-get root :remote-id))
	     (parent-kind (plist-get root :sync-kind))
	     (stored-parent (org-entry-get nil "ORG_COMMENTS_REMOTE_PARENT_ID")))
	(unless (member parent-kind '("footer" "inline"))
	  (user-error "Cannot determine Confluence endpoint for reply parent"))
	(unless parent-id
	  (user-error "Cannot push reply before parent comment has ORG_COMMENTS_REMOTE_ID"))
	(when (and stored-parent (not (equal stored-parent parent-id)))
	  (user-error "Reply parent ID %s does not match ancestor remote ID %s"
		      stored-parent parent-id))))
    sync-kind))

(defun org-confluence-comments-push-stamp-created (comment seen-at sync-kind &optional parent-id)
  "Stamp current sidecar heading with created remote COMMENT metadata at SEEN-AT.
SYNC-KIND is stored as `ORG_COMMENTS_SYNC_KIND'.  PARENT-ID, when non-nil, is
stored as `ORG_COMMENTS_REMOTE_PARENT_ID'."
  (let ((remote-id (org-confluence-comments-remote-id comment))
	(author-name (org-confluence-comments-remote-author-name comment))
	(author-id (org-confluence-comments-remote-author-id comment))
	(created-at (org-confluence-comments-remote-created-at comment))
	(resolution-status (org-confluence-comments-remote-resolution-status comment)))
    (unless remote-id
      (user-error "Confluence create response did not include a comment id"))
    (org-entry-put nil "ORG_COMMENTS_REMOTE_ID" remote-id)
    (org-entry-put nil "ORG_COMMENTS_SOURCE" "confluence")
    (org-entry-put nil "ORG_COMMENTS_SYNC_KIND" sync-kind)
    (when parent-id
      (org-entry-put nil "ORG_COMMENTS_REMOTE_PARENT_ID" parent-id))
    (org-entry-put nil "ORG_COMMENTS_REMOTE_LAST_SEEN_AT" seen-at)
    (when resolution-status
      (org-entry-put nil "ORG_COMMENTS_REMOTE_RESOLUTION_STATUS" resolution-status))
    (when (equal sync-kind "inline")
      (org-confluence-comments-push-stamp-inline-match-info)
      (org-confluence-comments-push-stamp-remote-inline-anchor-state comment))
    (org-confluence-comments-import-put-property-when-missing "ORG_COMMENTS_REMOTE_AUTHOR_ID" author-id)
    (org-confluence-comments-import-put-property-when-missing
     "ORG_COMMENTS_REMOTE_AUTHOR_DISPLAY_NAME" author-name)
    (org-confluence-comments-import-put-property-when-missing "ORG_COMMENTS_REMOTE_CREATED_AT" created-at)
    remote-id))

(defun org-confluence-comments-push-target-bounds ()
  "Return current sidecar target bounds as a cons cell, or nil."
  (when-let* ((value (org-entry-get nil "ORG_COMMENTS_TARGET"))
	      (parts (split-string value "[[:space:]]+" t)))
    (when (= 2 (length parts))
      (cons (string-to-number (car parts))
	    (string-to-number (cadr parts))))))

(defun org-confluence-comments-push-inline-match-info (source-file target-text target-bounds)
  "Return strict inline match info for SOURCE-FILE TARGET-TEXT and TARGET-BOUNDS.
Signal `user-error' when the sidecar anchor is missing, stale, or ambiguous in a
way that prevents computing Confluence's text selection match index."
  (unless (org-confluence-api--present-string-p target-text)
    (user-error "Inline comment is missing ORG_COMMENTS_TARGET_TEXT"))
  (unless target-bounds
    (user-error "Inline comment is missing ORG_COMMENTS_TARGET"))
  (unless (and source-file (file-readable-p source-file))
    (user-error "Cannot read source Org file for inline anchor preflight"))
  (with-current-buffer (find-file-noselect source-file)
    (let* ((matches (org-comments-anchor-matches-for-text (current-buffer) target-text))
	   (index (cl-position target-bounds matches :test #'equal)))
      (unless index
	(user-error "Inline comment anchor is stale; reanchor before pushing"))
      (list :count (length matches) :index index))))

(defun org-confluence-comments-push-inline-properties ()
  "Return strict `inlineCommentProperties' for the current sidecar comment."
  (let* ((target-text (org-entry-get nil "ORG_COMMENTS_TARGET_TEXT"))
	 (match-info (org-confluence-comments-push-inline-match-info
		      (org-confluence-comments-sidecar-source-file)
		      target-text
		      (org-confluence-comments-push-target-bounds))))
    `((textSelection . ,target-text)
      (textSelectionMatchCount . ,(plist-get match-info :count))
      (textSelectionMatchIndex . ,(plist-get match-info :index)))))

(defun org-confluence-comments-push-stamp-inline-match-info ()
  "Store local inline target match diagnostics at the current heading."
  (let ((match-info (org-confluence-comments-push-inline-match-info
		     (org-confluence-comments-sidecar-source-file)
		     (org-entry-get nil "ORG_COMMENTS_TARGET_TEXT")
		     (org-confluence-comments-push-target-bounds))))
    (org-entry-put nil "ORG_COMMENTS_TARGET_MATCH_COUNT"
		   (number-to-string (plist-get match-info :count)))
    (org-entry-put nil "ORG_COMMENTS_TARGET_MATCH_INDEX"
		   (number-to-string (plist-get match-info :index)))))

(defun org-confluence-comments-push-endpoint-for-kind (sync-kind)
  "Return Confluence REST endpoint for SYNC-KIND at point."
  (pcase sync-kind
    ("footer" "footer-comments")
    ("inline" "inline-comments")
    ("reply" (plist-get (org-confluence-comments-push-reply-parent-info) :endpoint))))

(defun org-confluence-comments-push-reply-parent-info ()
  "Return plist with reply parent remote id, sync kind, and endpoint."
  (let* ((root (org-confluence-comments-push-root-info))
	 (parent-kind (plist-get root :sync-kind))
	 (parent-id (plist-get root :remote-id)))
    (list :parent-id parent-id
	  :parent-kind parent-kind
	  :endpoint (pcase parent-kind
		      ("footer" "footer-comments")
		      ("inline" "inline-comments")))))

(defun org-confluence-comments-push-remote-inline-anchor-confirmed-p (comment)
  "Return non-nil when COMMENT includes Confluence inline anchor properties."
  (let ((properties (org-confluence-comments-remote-inline-target-object comment)))
    (and (listp properties)
	 (or (org-confluence-api--present-string-p
	      (alist-get 'inlineMarkerRef properties))
	     (org-confluence-api--present-string-p
	      (alist-get 'inline-marker-ref properties))
	     (org-confluence-api--present-string-p
	      (alist-get 'markerRef properties))
	     (org-confluence-api--present-string-p
	      (alist-get 'marker-ref properties))
	     (org-confluence-api--present-string-p
	      (alist-get 'inlineOriginalSelection properties))
	     (org-confluence-api--present-string-p
	      (alist-get 'inline-original-selection properties))
	     (org-confluence-api--present-string-p
	      (alist-get 'originalSelection properties))
	     (org-confluence-api--present-string-p
	      (alist-get 'original-selection properties))))))

(defun org-confluence-comments-push-stamp-remote-inline-anchor-state (comment)
  "Stamp remote inline anchor state from Confluence COMMENT response."
  (cond
   ((equal "dangling" (org-confluence-comments-remote-resolution-status comment))
    (org-entry-put nil "ORG_COMMENTS_REMOTE_ANCHOR_STATE" "dangling"))
   ((org-confluence-comments-push-remote-inline-anchor-confirmed-p comment)
    (org-entry-delete nil "ORG_COMMENTS_REMOTE_ANCHOR_STATE"))
   (t
    (org-entry-put nil "ORG_COMMENTS_REMOTE_ANCHOR_STATE" "unconfirmed"))))

(defun org-confluence-comments-push-response-comment (response)
  "Return a Confluence comment alist parsed from REST RESPONSE."
  (org-confluence-response-json-alist
   (org-confluence-response-body response)))

(defun org-confluence-comments-push-ensure-current-user-authored (remote-comment)
  "Signal unless REMOTE-COMMENT was authored by the authenticated user."
  (let ((author-id (org-confluence-comments-remote-author-id remote-comment))
	(account-id (org-confluence-people-current-user-account-id)))
    (unless (and author-id (equal author-id account-id))
      (user-error "Refusing to update Confluence comment authored by %s as %s"
		  (or author-id "<unknown>") account-id))))

(defun org-confluence-comments-push-stamp-updated (comment seen-at)
  "Stamp current sidecar heading after remote COMMENT update at SEEN-AT."
  (org-entry-delete nil "ORG_COMMENTS_LOCAL_UPDATED_AT")
  (org-entry-put nil "ORG_COMMENTS_REMOTE_LAST_SEEN_AT" seen-at)
  (when-let* ((updated-at (org-confluence-comments-remote-updated-at comment)))
    (org-entry-put nil "ORG_COMMENTS_REMOTE_UPDATED_AT" updated-at))
  (when-let* ((status (org-confluence-comments-remote-resolution-status comment)))
    (org-entry-put nil "ORG_COMMENTS_REMOTE_RESOLUTION_STATUS" status))
  (when (equal "inline" (org-entry-get nil "ORG_COMMENTS_SYNC_KIND"))
    (org-confluence-comments-push-stamp-remote-inline-anchor-state comment)))

(defun org-confluence-comments-push-update-current (sync-kind storage seen-at)
  "Update current remote-linked comment of SYNC-KIND with STORAGE at SEEN-AT."
  (let* ((remote-id (org-entry-get nil "ORG_COMMENTS_REMOTE_ID"))
	 (endpoint (org-confluence-comments-push-endpoint-for-kind sync-kind))
	 (current (org-confluence-comments-push-response-comment
		   (org-confluence-api--get-comment endpoint remote-id "storage")))
	 (version (org-confluence-comments-remote-version-number current)))
    (unless version
      (user-error "Confluence comment %s response did not include version.number" remote-id))
    (org-confluence-comments-push-ensure-current-user-authored current)
    (let* ((response (org-confluence-api--update-comment endpoint remote-id storage (1+ version)))
	   (comment (org-confluence-comments-push-response-comment response)))
      (org-confluence-comments-push-stamp-updated comment seen-at)
      remote-id)))

;;;###autoload
(defun org-confluence-comments-push-current (&optional page-id)
  "Push the current local sidecar comment to Confluence.
This implementation works from inside a `.comments.org' sidecar on a root local
footer or inline comment.  PAGE-ID, when non-nil, overrides the page ID read
from the sidecar source Org file."
  (interactive)
  (let* ((sync-kind (org-confluence-comments-push-ensure-current-pushable))
	 (sidecar-file buffer-file-name)
	 (heading (org-get-heading t t t t))
	 (body (org-confluence-comments-push-current-body))
	 (storage (org-confluence-comments-push-body-to-storage body))
	 (reply-info (when (equal sync-kind "reply")
		       (org-confluence-comments-push-reply-parent-info)))
	 (page-metadata (org-confluence-comments-source-page-metadata-from-sidecar))
	 (id (or page-id (plist-get page-metadata :page-id)))
	 (space (plist-get page-metadata :space))
	 (seen-at (org-confluence-comments-import-sync-timestamp))
	 (existing-remote-id (org-entry-get nil "ORG_COMMENTS_REMOTE_ID"))
	 response comment remote-id action)
    (if existing-remote-id
	(setq remote-id (org-confluence-comments-push-update-current sync-kind storage seen-at)
	      action :updated)
      (setq response (pcase sync-kind
		       ("footer" (org-confluence-api--create-footer-comment id storage))
		       ("inline" (org-confluence-api--create-inline-comment
				  id storage
				  (org-confluence-comments-push-inline-properties)))
		       ("reply" (org-confluence-api--create-comment-reply
				 (plist-get reply-info :endpoint)
				 (plist-get reply-info :parent-id)
				 storage)))
	    comment (org-confluence-comments-push-response-comment response)
	    action :created)
      (setq remote-id
	    (condition-case error
		(org-confluence-comments-push-stamp-created
		 comment seen-at sync-kind (plist-get reply-info :parent-id))
	      (error
	       (let ((created-id (org-confluence-comments-remote-id comment)))
		 (if created-id
		     (user-error "Created Confluence comment %s, but failed to update sidecar: %s"
				 created-id (error-message-string error))
		   (signal (car error) (cdr error))))))))
    (let ((url (org-confluence-api--comment-url id remote-id space)))
      (save-buffer)
      (org-comments-refresh-sidecar-headings sidecar-file)
      (org-comments-fold-sidecar-property-drawers)
      (kill-new url)
      (message "%s Confluence %s comment %s for %s; URL copied: %s"
	       (if (eq action :updated) "Updated" "Created")
	       sync-kind remote-id heading url)
      (list action 1 :remote-id remote-id :sync-kind sync-kind
	    :sidecar-file sidecar-file :url url))))



(provide 'org-confluence-comments-push)
;;; org-confluence-comments-push.el ends here
