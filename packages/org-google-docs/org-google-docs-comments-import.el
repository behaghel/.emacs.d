;;; org-google-docs-comments-import.el --- Import Google Docs comments -*- lexical-binding: t; -*-

;;; Commentary:
;; Import read-only Google Docs comments into the generic Org comments sidecar
;; format.  This first slice appends active remote comments and records enough
;; remote metadata for later update/preservation slices.

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'org-comments-anchors)
(require 'org-comments-core)
(require 'org-comments-sidecar)
(require 'org-google-docs-comments)
(require 'subr-x)

(defun org-google-docs-comments-import--status (comment)
  "Return sidecar TODO status for normalized COMMENT."
  (if (equal (plist-get comment :status) "resolved")
      "RESOLVED"
    "OPEN"))

(defun org-google-docs-comments-import--title (comment)
  "Return a sidecar heading title for normalized COMMENT."
  (if-let* ((author (plist-get comment :author-name)))
      (format "Google Docs comment from %s" author)
    "Google Docs comment"))

(defun org-google-docs-comments-import--property-line (key value)
  "Return Org property line for KEY and VALUE when VALUE is present."
  (when (and value (not (and (stringp value) (string-empty-p value))))
    (format ":%s: %s\n" key value)))

(defun org-google-docs-comments-import--entry (comment)
  "Return sidecar Org entry text for normalized Google COMMENT."
  (let ((remote-id (plist-get comment :remote-id))
	(status (org-google-docs-comments-import--status comment))
	(title (org-google-docs-comments-import--title comment)))
    (concat
     (format "* %s %s\n" status title)
     ":PROPERTIES:\n"
     (org-google-docs-comments-import--property-line
      "ORG_COMMENTS_ID" (format "google-docs:%s" remote-id))
     ":ORG_COMMENTS_BACKEND: google-docs\n"
     (org-google-docs-comments-import--property-line
      "ORG_COMMENTS_REMOTE_ID" remote-id)
     (org-google-docs-comments-import--property-line
      "ORG_COMMENTS_REMOTE_AUTHOR_DISPLAY_NAME" (plist-get comment :author-name))
     (org-google-docs-comments-import--property-line
      "ORG_COMMENTS_REMOTE_AUTHOR_EMAIL" (plist-get comment :author-email))
     (org-google-docs-comments-import--property-line
      "ORG_COMMENTS_REMOTE_CREATED_AT" (plist-get comment :created-at))
     (org-google-docs-comments-import--property-line
      "ORG_COMMENTS_REMOTE_UPDATED_AT" (plist-get comment :updated-at))
     (org-google-docs-comments-import--property-line
      "ORG_COMMENTS_REMOTE_RESOLUTION_STATUS" (plist-get comment :status))
     (org-google-docs-comments-import--property-line
      "ORG_COMMENTS_TARGET_TEXT" (plist-get comment :target-text))
     ":END:\n\n"
     (string-trim-right (or (plist-get comment :body) ""))
     "\n")))

(defun org-google-docs-comments-import--reply-title (reply)
  "Return a sidecar heading title for normalized Google REPLY."
  (if-let* ((author (plist-get reply :author-name)))
      (format "Reply from %s" author)
    "Reply"))

(defun org-google-docs-comments-import--reply-entry (reply parent-remote-id)
  "Return sidecar Org child entry text for normalized Google REPLY."
  (let ((remote-id (plist-get reply :remote-id)))
    (concat
     (format "** OPEN %s\n" (org-google-docs-comments-import--reply-title reply))
     ":PROPERTIES:\n"
     (org-google-docs-comments-import--property-line
      "ORG_COMMENTS_ID" (format "google-docs-reply:%s" remote-id))
     ":ORG_COMMENTS_BACKEND: google-docs\n"
     ":ORG_COMMENTS_SYNC_KIND: reply\n"
     (org-google-docs-comments-import--property-line
      "ORG_COMMENTS_REMOTE_ID" remote-id)
     (org-google-docs-comments-import--property-line
      "ORG_COMMENTS_REMOTE_PARENT_ID" parent-remote-id)
     ":ORG_COMMENTS_REMOTE_STATE: present\n"
     (org-google-docs-comments-import--property-line
      "ORG_COMMENTS_REMOTE_AUTHOR_DISPLAY_NAME" (plist-get reply :author-name))
     (org-google-docs-comments-import--property-line
      "ORG_COMMENTS_REMOTE_AUTHOR_EMAIL" (plist-get reply :author-email))
     (org-google-docs-comments-import--property-line
      "ORG_COMMENTS_REMOTE_CREATED_AT" (plist-get reply :created-at))
     (org-google-docs-comments-import--property-line
      "ORG_COMMENTS_REMOTE_UPDATED_AT" (plist-get reply :updated-at))
     ":END:\n\n"
     (string-trim-right (or (plist-get reply :body) ""))
     "\n")))

(defun org-google-docs-comments-import--apply-anchor (comment source-buffer)
  "Set sidecar anchor properties for COMMENT by matching SOURCE-BUFFER text."
  (when-let* ((target-text (plist-get comment :target-text)))
    (let* ((matches (org-comments-anchor-matches-for-text source-buffer target-text))
	   (count (length matches)))
      (pcase count
	(0
	 (org-entry-delete nil "ORG_COMMENTS_TARGET")
	 (org-entry-put nil "ORG_COMMENTS_ANCHOR_STATE" "missing")
	 (org-entry-put nil "ORG_COMMENTS_ANCHOR_MATCH_COUNT" "0"))
	(1
	 (pcase-let ((`(,start . ,end) (car matches)))
	   (org-entry-put nil "ORG_COMMENTS_TARGET" (format "%s %s" start end))
	   (org-entry-delete nil "ORG_COMMENTS_ANCHOR_STATE")
	   (org-entry-delete nil "ORG_COMMENTS_ANCHOR_MATCH_COUNT")))
	(_
	 (org-entry-delete nil "ORG_COMMENTS_TARGET")
	 (org-entry-put nil "ORG_COMMENTS_ANCHOR_STATE" "ambiguous")
	 (org-entry-put nil "ORG_COMMENTS_ANCHOR_MATCH_COUNT" (number-to-string count)))))))

(defun org-google-docs-comments-import--apply-remote-status (comment)
  "Apply COMMENT remote status to the current heading.
Return `:remote-resolved' when this changes the local TODO state to RESOLVED."
  (let ((status (plist-get comment :status))
	(local (or (org-get-todo-state) "OPEN"))
	(org-log-done nil)
	(org-inhibit-logging t))
    (when (and (equal status "resolved")
	       (not (equal local "RESOLVED")))
      (org-todo "RESOLVED")
      :remote-resolved)))

(defun org-google-docs-comments-import--update-at-heading (comment source-buffer)
  "Update sidecar heading at point from normalized Google COMMENT and SOURCE-BUFFER.
Return `:remote-resolved' when remote state changed the local TODO state, or
`:updated' otherwise."
  (let ((status-change (org-google-docs-comments-import--apply-remote-status comment)))
    (org-comments-sidecar-stamp-remote-metadata
     (list :backend "google-docs"
	   :remote-id (plist-get comment :remote-id)
	   :remote-state "present"
	   :remote-author-display-name (plist-get comment :author-name)
	   :remote-author-email (plist-get comment :author-email)
	   :remote-created-at (plist-get comment :created-at)
	   :remote-updated-at (plist-get comment :updated-at)
	   :remote-resolution-status (plist-get comment :status)))
    (when-let* ((status (plist-get comment :status)))
      (when (equal (org-comments-remote-status-value (org-get-todo-state)) status)
	(org-entry-delete nil "ORG_COMMENTS_LOCAL_STATUS_DIRTY")))
    (when-let* ((target-text (plist-get comment :target-text)))
      (org-entry-put nil "ORG_COMMENTS_TARGET_TEXT" target-text))
    (org-google-docs-comments-import--apply-anchor comment source-buffer)
    (org-comments-sidecar-replace-entry-body
     (plist-get comment :body) (save-excursion (org-end-of-subtree t t)))
    (or status-change :updated)))

(defun org-google-docs-comments-import--update-entry (sidecar-file remote-id comment source-buffer)
  "Update existing REMOTE-ID entry in SIDECAR-FILE from COMMENT and SOURCE-BUFFER.
Return non-nil when an entry was updated."
  (with-temp-buffer
    (insert-file-contents sidecar-file)
    (org-mode)
    (let ((updated nil))
      (goto-char (point-min))
      (while (and (not updated) (re-search-forward org-heading-regexp nil t))
	(goto-char (match-beginning 0))
	(when (equal remote-id (org-entry-get nil "ORG_COMMENTS_REMOTE_ID"))
	  (setq updated
		(org-google-docs-comments-import--update-at-heading
		 comment source-buffer)))
	(forward-line 1))
      (when updated
	(write-region (point-min) (point-max) sidecar-file nil 'silent))
      updated)))

(defun org-google-docs-comments-import--append-entry (sidecar-file source-file entry)
  "Append ENTRY to SIDECAR-FILE for SOURCE-FILE."
  (org-comments-sidecar-append-entry sidecar-file entry source-file))

(defun org-google-docs-comments-import--update-reply-at-heading (reply parent-remote-id)
  "Update sidecar reply heading at point from normalized Google REPLY."
  (org-comments-sidecar-stamp-remote-metadata
   (list :backend "google-docs"
	 :sync-kind "reply"
	 :remote-id (plist-get reply :remote-id)
	 :remote-parent-id parent-remote-id
	 :remote-state "present"
	 :remote-author-display-name (plist-get reply :author-name)
	 :remote-author-email (plist-get reply :author-email)
	 :remote-created-at (plist-get reply :created-at)
	 :remote-updated-at (plist-get reply :updated-at)))
  (org-comments-sidecar-replace-entry-body
   (plist-get reply :body) (save-excursion (org-end-of-subtree t t))))

(defun org-google-docs-comments-import--update-reply
    (sidecar-file parent-remote-id reply)
  "Update existing remote REPLY in SIDECAR-FILE.
Return non-nil when a reply was updated."
  (let ((remote-id (plist-get reply :remote-id)))
    (when (and remote-id (file-exists-p sidecar-file))
      (with-temp-buffer
	(insert-file-contents sidecar-file)
	(org-mode)
	(let ((updated nil))
	  (goto-char (point-min))
	  (while (and (not updated) (re-search-forward org-heading-regexp nil t))
	    (goto-char (match-beginning 0))
	    (when (and (equal remote-id (org-entry-get nil "ORG_COMMENTS_REMOTE_ID"))
		       (equal (org-entry-get nil "ORG_COMMENTS_SYNC_KIND") "reply"))
	      (org-google-docs-comments-import--update-reply-at-heading
	       reply parent-remote-id)
	      (setq updated t))
	    (forward-line 1))
	  (when updated
	    (write-region (point-min) (point-max) sidecar-file nil 'silent))
	  updated)))))

(defun org-google-docs-comments-import--append-reply
    (sidecar-file parent-remote-id reply)
  "Append normalized Google REPLY under PARENT-REMOTE-ID in SIDECAR-FILE.
Return non-nil when the reply was appended."
  (let ((remote-id (plist-get reply :remote-id)))
    (when (and remote-id
	       (not (org-comments-sidecar-has-remote-p
		     sidecar-file remote-id)))
      (unless (org-comments-sidecar-append-child-under-remote
	       sidecar-file parent-remote-id
	       (org-google-docs-comments-import--reply-entry reply parent-remote-id))
	(user-error "Cannot find Google Docs parent comment %s" parent-remote-id))
      t)))

(defun org-google-docs-comments-import--mark-missing-replies
    (sidecar-file parent-remote-id seen-reply-ids report)
  "Mark replies under PARENT-REMOTE-ID absent from SEEN-REPLY-IDS missing."
  (let ((missing-count
	 (org-comments-sidecar-reconcile-missing
	  sidecar-file seen-reply-ids
	  :sync-kind "reply"
	  :parent-remote-id parent-remote-id)))
    (plist-put report :remote-missing-replies
	       (+ (or (plist-get report :remote-missing-replies) 0)
		  (or missing-count 0)))))

(defun org-google-docs-comments-import--import-replies (sidecar-file comment report)
  "Import COMMENT replies into SIDECAR-FILE and update REPORT."
  (when-let* ((parent-remote-id (plist-get comment :remote-id)))
    (let (seen-reply-ids)
      (dolist (reply (plist-get comment :replies))
	(when-let* ((remote-id (plist-get reply :remote-id)))
	  (push remote-id seen-reply-ids))
	(cond
	 ((org-google-docs-comments-import--update-reply
	   sidecar-file parent-remote-id reply)
	  (plist-put report :updated-replies
		     (1+ (or (plist-get report :updated-replies) 0))))
	 ((org-google-docs-comments-import--append-reply
	   sidecar-file parent-remote-id reply)
	  (plist-put report :added-replies
		     (1+ (or (plist-get report :added-replies) 0))))))
      (org-google-docs-comments-import--mark-missing-replies
       sidecar-file parent-remote-id seen-reply-ids report))))

(defun org-google-docs-comments-import--mark-missing-comments
    (sidecar-file seen-remote-ids report)
  "Mark Google Docs comments absent from SEEN-REMOTE-IDS missing in SIDECAR-FILE.
Update REPORT with the number of newly missing root comments."
  (let ((missing-count
	 (org-comments-sidecar-reconcile-missing
	  sidecar-file seen-remote-ids
	  :backend "google-docs"
	  :sync-kind nil)))
    (plist-put report :remote-missing
	       (+ (or (plist-get report :remote-missing) 0)
		  (or missing-count 0)))))

(defun org-google-docs-comments-import--import-list
    (comments include-resolved source-file source-buffer)
  "Import COMMENTS for SOURCE-FILE and SOURCE-BUFFER, optionally INCLUDE-RESOLVED.
Return a provider-neutral import report plist."
  (let ((sidecar-file (org-comments-sidecar-path source-file))
	(report (list :provider "Google Docs"
		      :added 0
		      :added-replies 0
		      :updated-replies 0
		      :updated 0
		      :remote-resolved 0
		      :remote-missing 0
		      :remote-missing-replies 0
		      :skipped-resolved 0
		      :preserved-local t))
	seen-remote-ids)
    (org-comments-ensure-sidecar-header sidecar-file source-file)
    (dolist (comment comments)
      (let* ((remote-id (plist-get comment :remote-id))
	     (existing (and remote-id
			    (org-comments-sidecar-has-remote-p
			     sidecar-file remote-id))))
	(when remote-id
	  (push remote-id seen-remote-ids))
	(if (and (not include-resolved)
		 (equal (plist-get comment :status) "resolved")
		 (not existing))
	    (plist-put report :skipped-resolved
		       (1+ (or (plist-get report :skipped-resolved) 0)))
	  (let ((update-result (and existing
				    (org-google-docs-comments-import--update-entry
				     sidecar-file remote-id comment source-buffer))))
	    (if update-result
		(progn
		  (plist-put report :updated (1+ (or (plist-get report :updated) 0)))
		  (when (eq update-result :remote-resolved)
		    (plist-put report :remote-resolved
			       (1+ (or (plist-get report :remote-resolved) 0)))))
	      (org-google-docs-comments-import--append-entry
	       sidecar-file source-file
	       (org-google-docs-comments-import--entry comment))
	      (plist-put report :added (1+ (or (plist-get report :added) 0)))
	      (when remote-id
		(org-google-docs-comments-import--update-entry
		 sidecar-file remote-id comment source-buffer)))
	    (org-google-docs-comments-import--import-replies
	     sidecar-file comment report)))))
    (org-google-docs-comments-import--mark-missing-comments
     sidecar-file seen-remote-ids report)
    (plist-put report :sidecar-file sidecar-file)
    report))

;;;###autoload
(defun org-google-docs-comments-import (&optional include-resolved callback)
  "Import Google Docs comments into the current Org sidecar.
By default, only active comments are imported.  With INCLUDE-RESOLVED non-nil,
resolved comments are imported too.  CALLBACK, when non-nil, is called with the
sidecar file path after import."
  (interactive "P")
  (let ((source-file (or buffer-file-name
			 (user-error "Current buffer is not visiting a file")))
	(source-buffer (current-buffer))
	(interactive-p (called-interactively-p 'interactive)))
    (org-google-docs-comments-list
     (lambda (comments)
       (let* ((report (org-google-docs-comments-import--import-list
		       comments include-resolved source-file source-buffer))
	      (sidecar-file (plist-get report :sidecar-file)))
	 (when callback
	   (funcall callback sidecar-file))
	 (when interactive-p
	   (org-comments-import-report-message report))
	 sidecar-file)))))

(provide 'org-google-docs-comments-import)
;;; org-google-docs-comments-import.el ends here
