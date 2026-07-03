;;; org-confluence-comments-import.el --- Import Confluence comments -*- lexical-binding: t; -*-

;;; Commentary:
;; Import Confluence footer and inline comments into Org sidecars.

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'org-comments-anchors)
(require 'org-comments-core)
(require 'org-comments-sidecar)
(require 'org-comments-store)
(require 'seq)
(require 'subr-x)

(require 'org-confluence-api)
(require 'org-confluence-comments-context)
(require 'org-confluence-inline-comments)
(require 'org-confluence-comments-remote)
(require 'org-confluence-people-store)
(require 'org-confluence-response)

(defcustom org-confluence-comments-import-resolve-people t
  "Whether comment import resolves Confluence people in bulk afterwards."
  :type 'boolean
  :group 'org-confluence-api)

(autoload 'org-confluence-people-resolve "org-confluence-people" nil t)

(defun org-confluence-comments-import-heading-title (record &optional directory reply)
  "Return Confluence-aware heading title for RECORD using DIRECTORY.
When REPLY is non-nil, return a reply heading title."
  (let ((org-comments-resolve-account-id-function
	 #'org-confluence-people-resolve-account-id))
    (if reply
	(org-comments-reply-heading-title record directory)
      (org-comments-heading-title record directory))))

(defun org-confluence-comments-import-property-line (key value)
  "Return an Org property line for KEY and VALUE when VALUE is present."
  (when (and (stringp value) (not (string-empty-p (string-trim value))))
    (format ":%s: %s\n" key (string-trim value))))

(defun org-confluence-comments-import-number-property-line (key value)
  "Return an Org property line for KEY and numeric VALUE when present."
  (when (numberp value)
    (format ":%s: %s\n" key value)))

(defun org-confluence-comments-import-people-cache-file (directory)
  "Return people cache file for DIRECTORY, preferring an existing local file."
  (let ((local (and directory (org-confluence-people-store-local-file directory))))
    (if (and local (file-exists-p local))
	local
      (org-confluence-people-store-global-file))))

(defun org-confluence-comments-import-cache-author (comment &optional directory)
  "Cache Confluence author identity from COMMENT when author data is present."
  (let ((author-id (org-confluence-comments-remote-author-id comment))
	(display-name (org-confluence-comments-remote-author-display-name comment)))
    (when author-id
      (org-confluence-people-store-cache-identity
       author-id display-name nil (org-confluence-comments-import-people-cache-file directory)))))

(defun org-confluence-comments-import-remote-parent-id (comment)
  "Return remote parent comment ID for COMMENT, or nil."
  (when-let* ((id (or (alist-get 'parentCommentId comment)
		      (alist-get 'parent-comment-id comment))))
    (format "%s" id)))

(defun org-confluence-comments-import-sidecar-has-remote-p (sidecar-file remote-id)
  "Return non-nil when SIDECAR-FILE already has Confluence REMOTE-ID."
  (org-comments-sidecar-has-remote-p sidecar-file remote-id))

(defun org-confluence-comments-import-put-property-when-missing (property value)
  "Set Org PROPERTY to VALUE at point only when missing."
  (when (stringp value)
    (org-comments-sidecar-put-property-when-missing property (string-trim value))))

(defun org-confluence-comments-import-stamp-remote-author-metadata (comment)
  "Stamp missing sidecar author metadata from remote COMMENT."
  (org-comments-sidecar-stamp-remote-metadata-when-missing
   (list :remote-author-id (org-confluence-comments-remote-author-id comment)
	 :remote-author-display-name (org-confluence-comments-remote-author-name comment)
	 :remote-created-at (org-confluence-comments-remote-created-at comment))))

(defun org-confluence-comments-import-backfill-footer-metadata (sidecar-file comment)
  "Backfill missing sidecar metadata for existing remote footer COMMENT."
  (when-let* ((remote-id (org-confluence-comments-remote-id comment)))
    (org-comments-sidecar-with-remote-heading
     sidecar-file remote-id
     (lambda ()
       (org-confluence-comments-import-stamp-remote-author-metadata comment)
       t)
     :source "confluence")))

(defun org-confluence-comments-import-sync-timestamp ()
  "Return timestamp used for Confluence comment sync metadata."
  (format-time-string "%Y-%m-%dT%H:%M:%S%z"))

(defun org-confluence-comments-import-common-properties (comment body-format sync-kind)
  "Return common sidecar property text for COMMENT, BODY-FORMAT, and SYNC-KIND."
  (let* ((remote-id (org-confluence-comments-remote-id comment))
	 (local-id (format "remote-confluence-%s" remote-id))
	 (author-name (org-confluence-comments-remote-author-name comment))
	 (author-id (org-confluence-comments-remote-author-id comment))
	 (resolution-status (org-confluence-comments-remote-resolution-status comment))
	 (created-at (org-confluence-comments-remote-created-at comment)))
    (concat
     (format ":ORG_COMMENTS_ID: %s\n" local-id)
     (format ":ORG_COMMENTS_REMOTE_ID: %s\n" remote-id)
     ":ORG_COMMENTS_SOURCE: confluence\n"
     (format ":ORG_COMMENTS_SYNC_KIND: %s\n" sync-kind)
     (unless (equal body-format "storage")
       (format ":ORG_COMMENTS_BODY_FORMAT: %s\n" body-format))
     (org-confluence-comments-import-property-line "ORG_COMMENTS_REMOTE_LAST_SEEN_AT"
						   (org-confluence-comments-import-sync-timestamp))
     (org-confluence-comments-import-property-line "ORG_COMMENTS_REMOTE_RESOLUTION_STATUS" resolution-status)
     (org-confluence-comments-import-property-line "ORG_COMMENTS_REMOTE_AUTHOR_ID" author-id)
     (org-confluence-comments-import-property-line "ORG_COMMENTS_REMOTE_AUTHOR_DISPLAY_NAME" author-name)
     (org-confluence-comments-import-property-line "ORG_COMMENTS_REMOTE_CREATED_AT" created-at))))

(defun org-confluence-comments-import-title-record (comment body-format sync-kind)
  "Return normalized title record for remote COMMENT.
BODY-FORMAT and SYNC-KIND describe imported comment metadata."
  (let ((author-name (org-confluence-comments-remote-author-name comment))
	(resolution-status (org-confluence-comments-remote-resolution-status comment)))
    (org-comments-normalize-record
     (list :backend 'confluence
	   :author author-name
	   :remote-id (org-confluence-comments-remote-id comment)
	   :remote-state 'present
	   :remote-author-id (org-confluence-comments-remote-author-id comment)
	   :remote-author-display-name author-name
	   :remote-author-name author-name
	   :remote-resolution-status resolution-status
	   :resolved (and resolution-status (equal resolution-status "resolved"))
	   :created-at (org-confluence-comments-remote-created-at comment)
	   :sync-kind sync-kind
	   :body-format body-format
	   :target-text (org-confluence-comments-remote-inline-target-text comment)
	   :body (org-confluence-comments-remote-body comment)))))

(defun org-confluence-comments-import-reply-entry (comment body-format parent-remote-id &optional directory)
  "Return sidecar Org child entry text for reply COMMENT."
  (let ((parent-id (or parent-remote-id
		       (org-confluence-comments-import-remote-parent-id comment))))
    (concat
     (format "** %s\n"
	     (org-confluence-comments-import-heading-title
	      (org-confluence-comments-import-title-record comment body-format "reply")
	      directory
	      t))
     ":PROPERTIES:\n"
     (org-confluence-comments-import-common-properties comment body-format "reply")
     (org-confluence-comments-import-property-line "ORG_COMMENTS_REMOTE_PARENT_ID" parent-id)
     ":END:\n\n"
     (string-trim-right (org-confluence-comments-remote-body comment))
     "\n")))

(defun org-confluence-comments-import-orphan-thread-entry (parent-remote-id &optional _directory)
  "Return placeholder sidecar root entry for missing PARENT-REMOTE-ID."
  (concat
   (format "* OPEN Remote conversation %s - Missing parent comment\n" parent-remote-id)
   ":PROPERTIES:\n"
   (format ":ORG_COMMENTS_ID: remote-confluence-%s\n" parent-remote-id)
   (format ":ORG_COMMENTS_REMOTE_ID: %s\n" parent-remote-id)
   ":ORG_COMMENTS_SOURCE: confluence\n"
   ":ORG_COMMENTS_SYNC_KIND: orphan-thread\n"
   ":ORG_COMMENTS_ANCHOR_STATE: missing-parent\n"
   ":END:\n\n"
   "Parent comment was not returned by Confluence import.\n"))

(defun org-confluence-comments-import-initial-local-status (comment)
  "Return initial local TODO status for remote COMMENT."
  (if (equal "resolved" (org-confluence-comments-remote-resolution-status comment))
      "RESOLVED"
    "OPEN"))

(defun org-confluence-comments-import-footer-entry (comment body-format &optional directory)
  "Return sidecar Org entry text for remote footer COMMENT with BODY-FORMAT."
  (concat
   (format "* %s %s\n" (org-confluence-comments-import-initial-local-status comment)
	   (org-confluence-comments-import-heading-title
	    (org-confluence-comments-import-title-record comment body-format "footer")
	    directory))
   ":PROPERTIES:\n"
   (org-confluence-comments-import-common-properties comment body-format "footer")
   ":END:\n\n"
   (string-trim-right (org-confluence-comments-remote-body comment))
   "\n"))

(defun org-confluence-comments-import-inline-entry (comment body-format &optional directory)
  "Return sidecar Org entry text for remote inline COMMENT with BODY-FORMAT."
  (let* ((target-text (org-confluence-comments-remote-inline-target-text comment)))
    (concat
     (format "* %s %s\n" (org-confluence-comments-import-initial-local-status comment)
	     (org-confluence-comments-import-heading-title
	      (org-confluence-comments-import-title-record comment body-format "inline")
	      directory))
     ":PROPERTIES:\n"
     (org-confluence-comments-import-common-properties comment body-format "inline")
     (org-confluence-comments-import-property-line "ORG_COMMENTS_TARGET_TEXT" target-text)
     (org-confluence-comments-import-number-property-line
      "ORG_COMMENTS_TARGET_MATCH_COUNT"
      (org-confluence-comments-remote-inline-match-count comment))
     (org-confluence-comments-import-number-property-line
      "ORG_COMMENTS_TARGET_MATCH_INDEX"
      (org-confluence-comments-remote-inline-match-index comment))
     (org-confluence-comments-import-number-property-line
      "ORG_COMMENTS_REMOTE_MARKER_OCCURRENCE_COUNT"
      (org-confluence-comments-remote-marker-occurrence-count comment))
     (org-confluence-comments-import-number-property-line
      "ORG_COMMENTS_REMOTE_MARKER_OCCURRENCE_INDEX"
      (org-confluence-comments-remote-marker-occurrence-index comment))
     ":END:\n\n"
     (string-trim-right (org-confluence-comments-remote-body comment))
     "\n")))

(defun org-confluence-comments-import-append-entry (sidecar-file source-file entry)
  "Append remote comment ENTRY to SIDECAR-FILE for SOURCE-FILE."
  (org-comments-sidecar-append-entry sidecar-file entry source-file))

(defun org-confluence-comments-import-append-footer (sidecar-file source-file comment body-format)
  "Append remote footer COMMENT to SIDECAR-FILE for SOURCE-FILE."
  (org-confluence-comments-import-append-entry
   sidecar-file source-file
   (org-confluence-comments-import-footer-entry
    comment body-format (file-name-directory sidecar-file))))

(defun org-confluence-comments-import-append-inline (sidecar-file source-file comment body-format)
  "Append remote inline COMMENT to SIDECAR-FILE for SOURCE-FILE."
  (org-confluence-comments-import-append-entry
   sidecar-file source-file
   (org-confluence-comments-import-inline-entry
    comment body-format (file-name-directory sidecar-file))))

(defun org-confluence-comments-import-append-reply
    (sidecar-file source-file comment body-format parent-remote-id)
  "Append remote reply COMMENT under PARENT-REMOTE-ID in SIDECAR-FILE."
  (org-comments-ensure-sidecar-header sidecar-file source-file)
  (let ((entry (org-confluence-comments-import-reply-entry
		comment body-format parent-remote-id
		(file-name-directory sidecar-file))))
    (unless (org-comments-sidecar-append-child-under-remote
	     sidecar-file parent-remote-id entry :source "confluence")
      (org-confluence-comments-import-append-entry
       sidecar-file source-file
       (org-confluence-comments-import-orphan-thread-entry
	parent-remote-id (file-name-directory sidecar-file)))
      (org-comments-sidecar-append-entry sidecar-file entry))))

(defun org-confluence-comments-import-maybe-resolve-people (sidecar-file directory)
  "Resolve people for DIRECTORY and refresh SIDECAR-FILE headings when enabled."
  (when org-confluence-comments-import-resolve-people
    (condition-case error
	(progn
	  (org-confluence-people-resolve directory)
	  (when (file-exists-p sidecar-file)
	    (org-comments-refresh-sidecar-headings sidecar-file)))
      (error (message "Confluence people resolution skipped: %s" (error-message-string error))))))

(defun org-confluence-comments-import-set-remote-present-properties (comment)
  "Set present-state properties for remote COMMENT at current Org heading."
  (org-comments-sidecar-mark-present-at-heading
   #'org-confluence-comments-import-sync-timestamp)
  (if-let* ((status (org-confluence-comments-remote-resolution-status comment)))
      (org-entry-put nil "ORG_COMMENTS_REMOTE_RESOLUTION_STATUS" status)
    (org-entry-delete nil "ORG_COMMENTS_REMOTE_RESOLUTION_STATUS")))

(defun org-confluence-comments-import-apply-remote-status (comment report)
  "Apply COMMENT remote resolution status to current root heading.
Update REPORT with any local status changes."
  (let ((status (org-confluence-comments-remote-resolution-status comment))
	(local (or (org-get-todo-state) "OPEN"))
	(org-log-done nil)
	(org-inhibit-logging t))
    (pcase status
      ("resolved"
       (unless (equal local "RESOLVED")
	 (plist-put report :remote-resolved
		    (1+ (or (plist-get report :remote-resolved) 0)))
	 (when (equal local "TODO")
	   (plist-put report :todo-resolved
		      (cons (or (org-entry-get nil "ORG_COMMENTS_REMOTE_ID") "<unknown>")
			    (plist-get report :todo-resolved))))
	 (org-todo "RESOLVED")))
      ("open"
       (when (equal local "RESOLVED")
	 (plist-put report :remote-reopened
		    (1+ (or (plist-get report :remote-reopened) 0)))
	 (org-todo "OPEN"))))))

(defun org-confluence-comments-import-backfill-remote-metadata (sidecar-file comment &optional report)
  "Backfill/update sidecar metadata for existing remote COMMENT.
+REPORT is updated for TODO-impacting status changes when non-nil."
  (when-let* ((remote-id (org-confluence-comments-remote-id comment)))
    (org-comments-sidecar-with-remote-heading
     sidecar-file remote-id
     (lambda ()
       (org-confluence-comments-import-stamp-remote-author-metadata comment)
       (when (equal "inline" (org-entry-get nil "ORG_COMMENTS_SYNC_KIND"))
	 (org-confluence-comments-import-put-property-when-missing
	  "ORG_COMMENTS_TARGET_TEXT"
	  (org-confluence-comments-remote-inline-target-text comment))
	 (when-let* ((count (org-confluence-comments-remote-inline-match-count comment)))
	   (org-confluence-comments-import-put-property-when-missing
	    "ORG_COMMENTS_TARGET_MATCH_COUNT" (number-to-string count)))
	 (when-let* ((index (org-confluence-comments-remote-inline-match-index comment)))
	   (org-confluence-comments-import-put-property-when-missing
	    "ORG_COMMENTS_TARGET_MATCH_INDEX" (number-to-string index)))
	 (when-let* ((count (org-confluence-comments-remote-marker-occurrence-count comment)))
	   (org-confluence-comments-import-put-property-when-missing
	    "ORG_COMMENTS_REMOTE_MARKER_OCCURRENCE_COUNT" (number-to-string count)))
	 (when-let* ((index (org-confluence-comments-remote-marker-occurrence-index comment)))
	   (org-confluence-comments-import-put-property-when-missing
	    "ORG_COMMENTS_REMOTE_MARKER_OCCURRENCE_INDEX" (number-to-string index))))
       (org-confluence-comments-import-set-remote-present-properties comment)
       (unless (equal "reply" (org-entry-get nil "ORG_COMMENTS_SYNC_KIND"))
	 (org-confluence-comments-import-apply-remote-status comment report))
       t))))

(defun org-confluence-comments-import-remote-replies
    (sidecar-file source-file parent-comment endpoint body-format report)
  "Import remote replies under PARENT-COMMENT from ENDPOINT.
Return plist with `:imported', `:seen-ids', `:parent-id', and `:complete'."
  (let ((parent-id (org-confluence-comments-remote-id parent-comment))
	(imported 0)
	(seen-ids nil)
	(complete nil))
    (when parent-id
      (let ((replies (condition-case error
			 (prog1 (org-confluence-response-comment-results
				 (org-confluence-api--list-comment-children parent-id endpoint body-format))
			   (setq complete t))
		       (error
			(message "Confluence reply import skipped for %s: %s"
				 parent-id (error-message-string error))
			nil))))
	(dolist (reply replies)
	  (org-confluence-comments-import-cache-author reply (file-name-directory source-file))
	  (when-let* ((remote-id (org-confluence-comments-remote-id reply)))
	    (push remote-id seen-ids)
	    (if (org-confluence-comments-import-sidecar-has-remote-p sidecar-file remote-id)
		(org-confluence-comments-import-backfill-remote-metadata sidecar-file reply report)
	      (org-confluence-comments-import-append-reply
	       sidecar-file source-file reply body-format parent-id)
	      (plist-put report :imported-ids
			 (cons remote-id (plist-get report :imported-ids)))
	      (plist-put report :imported-reply-ids
			 (cons remote-id (plist-get report :imported-reply-ids)))
	      (setq imported (1+ imported)))))))
    (list :imported imported :seen-ids seen-ids :parent-id parent-id :complete complete)))

(defun org-confluence-comments-import-endpoint-sync-kind (endpoint)
  "Return sidecar sync kind for Confluence ENDPOINT."
  (pcase endpoint
    ("footer-comments" "footer")
    ("inline-comments" "inline")))

(defun org-confluence-comments-import-reconcile-missing-roots
    (sidecar-file sync-kind seen-ids report)
  "Mark remote-linked root comments of SYNC-KIND absent from SEEN-IDS missing."
  (let ((missing-count
	 (org-comments-sidecar-reconcile-missing
	  sidecar-file seen-ids
	  :sync-kind sync-kind
	  :timestamp #'org-confluence-comments-import-sync-timestamp
	  :on-missing
	  (lambda (remote-id _newly-missing)
	    (when (equal (org-get-todo-state) "TODO")
	      (plist-put report :todo-missing
			 (cons remote-id (plist-get report :todo-missing))))))))
    (plist-put report :marked-missing
	       (+ (or (plist-get report :marked-missing) 0)
		  (or missing-count 0)))))

(defun org-confluence-comments-import-reconcile-missing-replies
    (sidecar-file parent-id seen-ids report)
  "Mark remote-linked replies under PARENT-ID absent from SEEN-IDS missing."
  (let ((missing-count
	 (and parent-id
	      (org-comments-sidecar-reconcile-missing
	       sidecar-file seen-ids
	       :sync-kind "reply"
	       :parent-remote-id parent-id
	       :timestamp #'org-confluence-comments-import-sync-timestamp))))
    (plist-put report :marked-missing
	       (+ (or (plist-get report :marked-missing) 0)
		  (or missing-count 0)))))

(defun org-confluence-comments-import-record-present-again (sidecar-file remote-id report)
  "Record REMOTE-ID present-again in REPORT when SIDECAR-FILE says missing."
  (when (org-comments-sidecar-remote-missing-p sidecar-file remote-id)
    (plist-put report :present-again (1+ (or (plist-get report :present-again) 0)))))

(defun org-confluence-comments-import-generic-report (report)
  "Return provider-neutral import report derived from Confluence REPORT."
  (let* ((root-ids (plist-get report :imported-root-ids))
	 (reply-ids (plist-get report :imported-reply-ids))
	 (root-count (if root-ids
			 (length root-ids)
		       (max 0 (- (or (plist-get report :imported) 0)
				 (length reply-ids))))))
    (list :provider "Confluence"
	  :added root-count
	  :added-replies (length reply-ids)
	  :updated 0
	  :remote-resolved (or (plist-get report :remote-resolved) 0)
	  :remote-reopened (or (plist-get report :remote-reopened) 0)
	  :remote-missing (or (plist-get report :marked-missing) 0)
	  :remote-present (or (plist-get report :present-again) 0)
	  :preserved-local t)))

(defun org-confluence-comments-import-report-details (report)
  "Open detailed Confluence REPORT buffer for TODO-impacting changes."
  (when (or (plist-get report :todo-resolved)
	    (plist-get report :todo-missing))
    (with-current-buffer (get-buffer-create "*Confluence Comment Sync Report*")
      (let ((inhibit-read-only t))
	(erase-buffer)
	(insert "Confluence comment sync report\n\n")
	(when-let* ((items (plist-get report :todo-resolved)))
	  (insert "* TODO closed by remote resolution\n")
	  (dolist (id (reverse items)) (insert "- " id "\n")))
	(when-let* ((items (plist-get report :todo-missing)))
	  (insert "* TODO remote-missing\n")
	  (dolist (id (reverse items)) (insert "- " id "\n")))
	(special-mode)))))

(defun org-confluence-comments-import-report-message (report _fallback)
  "Show provider-neutral import summary for Confluence REPORT."
  (org-comments-import-report-message
   (org-confluence-comments-import-generic-report report))
  (org-confluence-comments-import-report-details report)
  report)

(defun org-confluence-comments-import-remote-comments
    (page-id endpoint body-format append-function &optional resolve-after report)
  "Import PAGE-ID comments from ENDPOINT using APPEND-FUNCTION.
BODY-FORMAT defaults to storage.  Return import report plist."
  (unless buffer-file-name
    (user-error "Current buffer is not visiting a file"))
  (let* ((source-file buffer-file-name)
	 (source-directory (file-name-directory source-file))
	 (sidecar-file (org-comments-sidecar-path source-file))
	 (id (org-confluence-comments-page-id-or-read page-id))
	 (format-name (or body-format "storage"))
	 (response (org-confluence-api--list-page-comments id endpoint format-name))
	 (comments (org-confluence-response-comment-results response))
	 (sync-kind (org-confluence-comments-import-endpoint-sync-kind endpoint))
	 (report (or report (list :imported 0)))
	 page-storage
	 seen-ids)
    (when (equal sync-kind "inline")
      (setq page-storage
	    (condition-case nil
		(org-confluence-response-page-body-storage-value
		 (org-confluence-api--get-page id "storage"))
	      (user-error nil)))
      (when page-storage
	(setq comments
	      (mapcar (lambda (comment)
			(org-confluence-inline-comments-annotate-inline-comment-with-marker-occurrence
			 page-storage comment))
		      comments))))
    (dolist (comment comments)
      (org-confluence-comments-import-cache-author comment source-directory)
      (when-let* ((remote-id (org-confluence-comments-remote-id comment)))
	(push remote-id seen-ids)
	(org-confluence-comments-import-record-present-again sidecar-file remote-id report)
	(if (org-confluence-comments-import-sidecar-has-remote-p sidecar-file remote-id)
	    (org-confluence-comments-import-backfill-remote-metadata sidecar-file comment report)
	  (funcall append-function sidecar-file source-file comment format-name)
	  (plist-put report :imported (1+ (or (plist-get report :imported) 0)))
	  (plist-put report :imported-ids
		     (cons remote-id (plist-get report :imported-ids)))
	  (plist-put report :imported-root-ids
		     (cons remote-id (plist-get report :imported-root-ids))))
	(let ((reply-summary (org-confluence-comments-import-remote-replies
			      sidecar-file source-file comment endpoint format-name report)))
	  (plist-put report :imported (+ (or (plist-get report :imported) 0)
					 (plist-get reply-summary :imported)))
	  (when (plist-get reply-summary :complete)
	    (org-confluence-comments-import-reconcile-missing-replies
	     sidecar-file remote-id (plist-get reply-summary :seen-ids) report)))))
    (org-confluence-comments-import-reconcile-missing-roots sidecar-file sync-kind seen-ids report)
    (when (file-exists-p sidecar-file)
      (org-comments-anchor-imported-inline-comments (current-buffer) sidecar-file)
      (org-comments-refresh-sidecar-headings sidecar-file))
    (when resolve-after
      (org-confluence-comments-import-maybe-resolve-people sidecar-file source-directory))
    report))

;;;###autoload
(defun org-confluence-comments-import-footer (&optional page-id body-format)
  "Import remote footer comments for PAGE-ID into the current Org sidecar.
This function is append-only and idempotent: existing remote Confluence comments
are detected by remote ID and are not overwritten.  BODY-FORMAT defaults to
storage, and the raw remote body is preserved verbatim."
  (interactive)
  (let ((report (org-confluence-comments-import-remote-comments
		 page-id "footer-comments" body-format
		 #'org-confluence-comments-import-append-footer t)))
    (org-confluence-comments-import-report-message
     report "Imported 0 Confluence footer comments")
    (or (plist-get report :imported) 0)))

;;;###autoload
(defun org-confluence-comments-import-inline (&optional page-id body-format)
  "Import remote inline comments for PAGE-ID into the current Org sidecar.
Imported inline comments are left unanchored until manually reanchored."
  (interactive)
  (let ((report (org-confluence-comments-import-remote-comments
		 page-id "inline-comments" body-format
		 #'org-confluence-comments-import-append-inline t)))
    (org-confluence-comments-import-report-message
     report "Imported 0 Confluence inline comments")
    (or (plist-get report :imported) 0)))

;;;###autoload
(defun org-confluence-comments-import (&optional page-id body-format)
  "Import remote footer and inline comments for PAGE-ID into the sidecar.
BODY-FORMAT is passed through to the Confluence comments API and defaults to
storage in the lower-level import commands."
  (interactive)
  (let* ((id (org-confluence-comments-page-id-or-read page-id))
	 (footer-count (org-confluence-comments-import-footer id body-format))
	 (inline-count (org-confluence-comments-import-inline id body-format)))
    (message "Imported %s footer and %s inline Confluence comments"
	     footer-count inline-count)
    (+ footer-count inline-count)))




(provide 'org-confluence-comments-import)
;;; org-confluence-comments-import.el ends here
