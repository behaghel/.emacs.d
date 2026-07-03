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

(defun org-google-docs-comments-import--body-region (subtree-end)
  "Return current heading body region before child headings and SUBTREE-END."
  (save-excursion
    (forward-line 1)
    (when (looking-at-p "[	]*:PROPERTIES:[		]*$")
      (unless (re-search-forward "^[	]*:END:[	]*$" subtree-end t)
	(user-error "Google Docs comment property drawer has no :END:"))
      (forward-line 1))
    (let ((start (point))
	  (end (save-excursion
		 (if (re-search-forward org-heading-regexp subtree-end t)
		     (match-beginning 0)
		   subtree-end))))
      (cons start end))))

(defun org-google-docs-comments-import--replace-body (comment subtree-end)
  "Replace current subtree machine-owned body with COMMENT body before SUBTREE-END."
  (let ((body (string-trim-right (or (plist-get comment :body) ""))))
    (pcase-let ((`(,start . ,end) (org-google-docs-comments-import--body-region subtree-end)))
      (delete-region start end)
      (goto-char start)
      (insert "\n" body "\n\n"))))

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
    (org-entry-put nil "ORG_COMMENTS_BACKEND" "google-docs")
    (org-entry-put nil "ORG_COMMENTS_REMOTE_ID" (plist-get comment :remote-id))
    (org-entry-put nil "ORG_COMMENTS_REMOTE_STATE" "present")
    (org-entry-delete nil "ORG_COMMENTS_REMOTE_MISSING_AT")
    (when-let* ((author (plist-get comment :author-name)))
      (org-entry-put nil "ORG_COMMENTS_REMOTE_AUTHOR_DISPLAY_NAME" author))
    (when-let* ((email (plist-get comment :author-email)))
      (org-entry-put nil "ORG_COMMENTS_REMOTE_AUTHOR_EMAIL" email))
    (when-let* ((created-at (plist-get comment :created-at)))
      (org-entry-put nil "ORG_COMMENTS_REMOTE_CREATED_AT" created-at))
    (when-let* ((updated-at (plist-get comment :updated-at)))
      (org-entry-put nil "ORG_COMMENTS_REMOTE_UPDATED_AT" updated-at))
    (when-let* ((status (plist-get comment :status)))
      (org-entry-put nil "ORG_COMMENTS_REMOTE_RESOLUTION_STATUS" status)
      (when (equal (org-comments-remote-status-value (org-get-todo-state)) status)
	(org-entry-delete nil "ORG_COMMENTS_LOCAL_STATUS_DIRTY")))
    (when-let* ((target-text (plist-get comment :target-text)))
      (org-entry-put nil "ORG_COMMENTS_TARGET_TEXT" target-text))
    (org-google-docs-comments-import--apply-anchor comment source-buffer)
    (org-google-docs-comments-import--replace-body
     comment (save-excursion (org-end-of-subtree t t)))
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
  (org-comments-ensure-sidecar-header sidecar-file source-file)
  (with-temp-buffer
    (insert-file-contents sidecar-file)
    (goto-char (point-max))
    (unless (bolp) (insert "\n"))
    (unless (save-excursion
	      (forward-line -1)
	      (looking-at-p "[[:space:]]*$"))
      (insert "\n"))
    (insert entry)
    (write-region (point-min) (point-max) sidecar-file nil 'silent)))

(defun org-google-docs-comments-import--sidecar-has-remote-p (sidecar-file remote-id)
  "Return non-nil when SIDECAR-FILE already contains REMOTE-ID."
  (when (and remote-id (file-exists-p sidecar-file))
    (with-temp-buffer
      (insert-file-contents sidecar-file)
      (org-mode)
      (goto-char (point-min))
      (cl-loop while (re-search-forward org-heading-regexp nil t)
	       do (goto-char (match-beginning 0))
	       when (equal remote-id (org-entry-get nil "ORG_COMMENTS_REMOTE_ID"))
	       return t
	       do (forward-line 1)))))

(defun org-google-docs-comments-import--update-reply-at-heading (reply parent-remote-id)
  "Update sidecar reply heading at point from normalized Google REPLY."
  (org-entry-put nil "ORG_COMMENTS_BACKEND" "google-docs")
  (org-entry-put nil "ORG_COMMENTS_SYNC_KIND" "reply")
  (org-entry-put nil "ORG_COMMENTS_REMOTE_ID" (plist-get reply :remote-id))
  (org-entry-put nil "ORG_COMMENTS_REMOTE_PARENT_ID" parent-remote-id)
  (org-entry-put nil "ORG_COMMENTS_REMOTE_STATE" "present")
  (org-entry-delete nil "ORG_COMMENTS_REMOTE_MISSING_AT")
  (when-let* ((author (plist-get reply :author-name)))
    (org-entry-put nil "ORG_COMMENTS_REMOTE_AUTHOR_DISPLAY_NAME" author))
  (when-let* ((email (plist-get reply :author-email)))
    (org-entry-put nil "ORG_COMMENTS_REMOTE_AUTHOR_EMAIL" email))
  (when-let* ((created-at (plist-get reply :created-at)))
    (org-entry-put nil "ORG_COMMENTS_REMOTE_CREATED_AT" created-at))
  (when-let* ((updated-at (plist-get reply :updated-at)))
    (org-entry-put nil "ORG_COMMENTS_REMOTE_UPDATED_AT" updated-at))
  (org-google-docs-comments-import--replace-body
   reply (save-excursion (org-end-of-subtree t t))))

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
	       (not (org-google-docs-comments-import--sidecar-has-remote-p
		     sidecar-file remote-id)))
      (with-temp-buffer
	(insert-file-contents sidecar-file)
	(org-mode)
	(goto-char (point-min))
	(unless (cl-loop while (re-search-forward org-heading-regexp nil t)
			 do (goto-char (match-beginning 0))
			 when (equal parent-remote-id
				     (org-entry-get nil "ORG_COMMENTS_REMOTE_ID"))
			 return t
			 do (forward-line 1))
	  (user-error "Cannot find Google Docs parent comment %s" parent-remote-id))
	(goto-char (save-excursion (org-end-of-subtree t t)))
	(unless (bolp) (insert "\n"))
	(unless (save-excursion
		  (forward-line -1)
		  (looking-at-p "[[:space:]]*$"))
	  (insert "\n"))
	(insert (org-google-docs-comments-import--reply-entry reply parent-remote-id))
	(write-region (point-min) (point-max) sidecar-file nil 'silent))
      t)))

(defun org-google-docs-comments-import--mark-missing-replies
    (sidecar-file parent-remote-id seen-reply-ids report)
  "Mark replies under PARENT-REMOTE-ID absent from SEEN-REPLY-IDS missing."
  (when (file-exists-p sidecar-file)
    (with-temp-buffer
      (insert-file-contents sidecar-file)
      (org-mode)
      (let ((changed nil)
	    (missing-count 0))
	(goto-char (point-min))
	(while (re-search-forward org-heading-regexp nil t)
	  (goto-char (match-beginning 0))
	  (let ((sync-kind (org-entry-get nil "ORG_COMMENTS_SYNC_KIND"))
		(parent-id (org-entry-get nil "ORG_COMMENTS_REMOTE_PARENT_ID"))
		(remote-id (org-entry-get nil "ORG_COMMENTS_REMOTE_ID"))
		(remote-state (org-entry-get nil "ORG_COMMENTS_REMOTE_STATE")))
	    (when (and (equal sync-kind "reply")
		       (equal parent-id parent-remote-id)
		       remote-id
		       (not (member remote-id seen-reply-ids))
		       (not (equal remote-state "missing")))
	      (org-entry-put nil "ORG_COMMENTS_REMOTE_STATE" "missing")
	      (org-entry-put nil "ORG_COMMENTS_REMOTE_MISSING_AT"
			     (org-comments-current-created-at))
	      (setq changed t)
	      (setq missing-count (1+ missing-count))))
	  (forward-line 1))
	(when changed
	  (write-region (point-min) (point-max) sidecar-file nil 'silent))
	(plist-put report :remote-missing-replies
		   (+ (or (plist-get report :remote-missing-replies) 0)
		      missing-count))))))

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
  (when (file-exists-p sidecar-file)
    (with-temp-buffer
      (insert-file-contents sidecar-file)
      (org-mode)
      (let ((changed nil)
	    (missing-count 0))
	(goto-char (point-min))
	(while (re-search-forward org-heading-regexp nil t)
	  (goto-char (match-beginning 0))
	  (let ((backend (org-entry-get nil "ORG_COMMENTS_BACKEND"))
		(sync-kind (org-entry-get nil "ORG_COMMENTS_SYNC_KIND"))
		(remote-id (org-entry-get nil "ORG_COMMENTS_REMOTE_ID"))
		(remote-state (org-entry-get nil "ORG_COMMENTS_REMOTE_STATE")))
	    (when (and (equal backend "google-docs")
		       (not (equal sync-kind "reply"))
		       remote-id
		       (not (member remote-id seen-remote-ids))
		       (not (equal remote-state "missing")))
	      (org-entry-put nil "ORG_COMMENTS_REMOTE_STATE" "missing")
	      (org-entry-put nil "ORG_COMMENTS_REMOTE_MISSING_AT"
			     (org-comments-current-created-at))
	      (setq changed t)
	      (setq missing-count (1+ missing-count))))
	  (forward-line 1))
	(when changed
	  (write-region (point-min) (point-max) sidecar-file nil 'silent))
	(plist-put report :remote-missing
		   (+ (or (plist-get report :remote-missing) 0)
		      missing-count))))))

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
			    (org-google-docs-comments-import--sidecar-has-remote-p
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
