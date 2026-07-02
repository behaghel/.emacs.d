;;; org-confluence-sync-status-collect.el --- Confluence sync issue collection -*- lexical-binding: t; -*-

;;; Commentary:
;; Issue construction and fresh sync status collection for Confluence-backed Org
;; buffers.  Rendering lives in `org-confluence-sync-status-render'; actions live
;; in `org-confluence-sync-status'.

;;; Code:

(require 'org)
(require 'org-comments)
(require 'org-comments-sidecar)
(require 'seq)
(require 'subr-x)

(require 'org-confluence-api)
(require 'org-confluence-export)
(require 'org-confluence-people)
(require 'org-confluence-response)
(require 'org-confluence-sync-status-cache)

(defun org-confluence-sync-status--string-value (value)
  "Return VALUE when it is a string, otherwise nil.
This treats JSON null values parsed as `:null' as absent."
  (and (stringp value) value))

(defun org-confluence-sync-status--issue (severity category message &optional action link)
  "Return a Confluence sync issue.
SEVERITY, CATEGORY, and MESSAGE describe the issue.  ACTION names the suggested
correction and LINK is an optional `org-comment:' link for the relevant sidecar
comment."
  (append `((severity . ,severity)
	    (category . ,category)
	    (message . ,message))
	  (when action `((action . ,action)))
	  (when link `((link . ,link)))))

(defun org-confluence-sync-status--issue-severity (issue)
  "Return ISSUE severity string."
  (alist-get 'severity issue))

(defun org-confluence-sync-status--overall-state (issues)
  "Return overall sync state for ISSUES."
  (cond
   ((seq-some (lambda (issue)
		(equal "blocking" (org-confluence-sync-status--issue-severity issue)))
	      issues)
    "blocking")
   (issues "warning")
   (t "clean")))

(defun org-confluence-sync-status--plural (count singular)
  "Return COUNT plus SINGULAR pluralized for human status summaries."
  (format "%s %s%s" count singular (if (= count 1) "" "s")))

(defun org-confluence-sync-status--summary (state issues &optional fallback)
  "Return human summary for STATE and ISSUES using FALLBACK when needed."
  (if (member state '("unknown" unknown))
      (or fallback "unknown")
    (if (null issues)
	""
      (let ((counts nil))
	(dolist (issue issues)
	  (let ((category (alist-get 'category issue)))
	    (setf (alist-get category counts nil nil #'equal)
		  (1+ (or (alist-get category counts nil nil #'equal) 0)))))
	(string-join
	 (mapcar (lambda (entry)
		   (org-confluence-sync-status--plural
		    (cdr entry) (format "%s issue" (car entry))))
		 (nreverse counts))
	 ", ")))))

(defun org-confluence-sync-status--shorten (text &optional length)
  "Return TEXT shortened to LENGTH for sync status display."
  (let ((limit (or length 72)))
    (if (and text (> (length text) limit))
	(concat (substring text 0 (max 0 (- limit 1))) "…")
      text)))

(defun org-confluence-sync-status--comment-summary ()
  "Return a compact summary of the sidecar comment at point."
  (org-confluence-sync-status--shorten
   (string-trim (or (org-get-heading t t t t) "untitled comment")) 96))

(defun org-confluence-sync-status--comment-label (sync-kind remote-id target-text)
  "Return a human label for comment SYNC-KIND, REMOTE-ID, and TARGET-TEXT."
  (string-join
   (delq nil
	 (list (if (string-empty-p (or sync-kind "")) "comment" sync-kind)
	       (and remote-id (format "#%s" remote-id))
	       (and target-text
		    (not (string-empty-p target-text))
		    (format "“%s”" (org-confluence-sync-status--shorten target-text 40)))))
   " "))

(defun org-confluence-sync-status--comment-details (summary &rest details)
  "Return sync issue text from SUMMARY plus non-empty DETAILS."
  (string-join (delq nil (cons summary details)) "; "))

(defun org-confluence-sync-status--comment-link (source-file comment-id heading)
  "Return an `org-comment:' link for COMMENT-ID in SOURCE-FILE."
  (when (and source-file comment-id)
    (org-comments-make-link
     source-file comment-id (org-confluence-sync-status--shorten heading 72))))

(defun org-confluence-sync-status--remote-comment-id-set (page-id)
  "Return a hash table of remote comment IDs currently present on PAGE-ID."
  (let ((ids (make-hash-table :test #'equal)))
    (dolist (endpoint '("footer-comments" "inline-comments"))
      (dolist (comment (org-confluence-response-comment-results
			(org-confluence-api--list-page-comments page-id endpoint "storage")))
	(when-let* ((id (org-confluence-sync-status--string-value
			 (alist-get 'id comment))))
	  (puthash id endpoint ids))))
    ids))

(defun org-confluence-sync-status--sidecar-issues (source-buffer remote-comment-ids)
  "Return sync issues found in SOURCE-BUFFER's sidecar comments.
REMOTE-COMMENT-IDS is a hash table of comment IDs currently returned by
Confluence, used to distinguish remotely deleted comments from stale sidecar
metadata."
  (let* ((source-file (buffer-file-name source-buffer))
	 (sidecar (and source-file (org-comments-sidecar-path source-file)))
	 (people (make-hash-table :test #'equal))
	 issues)
    (when (and sidecar (file-readable-p sidecar))
      (with-temp-buffer
	(insert-file-contents sidecar)
	(org-mode)
	(goto-char (point-min))
	(while (re-search-forward org-heading-regexp nil t)
	  (goto-char (match-beginning 0))
	  (let* ((state (org-entry-get nil "ORG_COMMENTS_ANCHOR_STATE"))
		 (sync-kind (org-entry-get nil "ORG_COMMENTS_SYNC_KIND"))
		 (remote-id (org-entry-get nil "ORG_COMMENTS_REMOTE_ID"))
		 (remote-state (org-entry-get nil "ORG_COMMENTS_REMOTE_STATE"))
		 (author-id (org-entry-get nil "ORG_COMMENTS_REMOTE_AUTHOR_ID"))
		 (display-name (org-entry-get nil "ORG_COMMENTS_REMOTE_AUTHOR_DISPLAY_NAME"))
		 (target-text (org-entry-get nil "ORG_COMMENTS_TARGET_TEXT"))
		 (target-lines (org-entry-get nil "ORG_COMMENTS_TARGET_LINES"))
		 (last-seen (org-entry-get nil "ORG_COMMENTS_REMOTE_LAST_SEEN_AT"))
		 (missing-at (org-entry-get nil "ORG_COMMENTS_REMOTE_MISSING_AT"))
		 (created-at (or (org-entry-get nil "ORG_COMMENTS_REMOTE_CREATED_AT")
				 (org-entry-get nil "ORG_COMMENTS_CREATED_AT")))
		 (heading (org-confluence-sync-status--comment-summary))
		 (comment-id (org-entry-get nil "ORG_COMMENTS_ID"))
		 (comment-link (org-confluence-sync-status--comment-link source-file comment-id heading))
		 (label (org-confluence-sync-status--comment-label sync-kind remote-id target-text)))
	    (when (member state '("ambiguous" "stale" "missing"))
	      (push (org-confluence-sync-status--issue
		     "warning" "comment"
		     (org-confluence-sync-status--comment-details
		      (format "Local anchor for %s is %s" label state)
		      (and target-lines (format "source lines %s" target-lines))
		      (format "sidecar: %s" heading))
		     "reanchor" comment-link)
		    issues))
	    (when (and sync-kind (not remote-id) (not (equal "reply" sync-kind)))
	      (push (org-confluence-sync-status--issue
		     "warning" "comment"
		     (org-confluence-sync-status--comment-details
		      (format "Local %s draft is unpublished" sync-kind)
		      (and target-text
			   (format "target “%s”"
				   (org-confluence-sync-status--shorten target-text 40)))
		      (format "sidecar: %s" heading))
		     "open-sidecar" comment-link)
		    issues))
	    (when (member remote-state '("missing" "dangling"))
	      (if (and remote-id remote-comment-ids (gethash remote-id remote-comment-ids))
		  (push (org-confluence-sync-status--issue
			 "warning" "comment"
			 (org-confluence-sync-status--comment-details
			  (format "Local metadata is stale: remote %s is present in Confluence, but the sidecar still says %s"
				  label remote-state)
			  "use i to import comments and clear the stale missing marker"
			  (format "sidecar: %s" heading))
			 "import" comment-link)
			issues)
		(push (org-confluence-sync-status--issue
		       "warning" "comment"
		       (org-confluence-sync-status--comment-details
			(format "Remote %s appears deleted or unavailable in Confluence" label)
			"if this deletion is intentional, open the comment and delete the local copy with x"
			"if this looks wrong, use i to import comments before deleting anything"
			(and missing-at (format "missing since %s" missing-at))
			(and last-seen (format "last seen %s" last-seen))
			(and created-at (format "created %s" created-at))
			(format "sidecar: %s" heading))
		       "delete-local" comment-link)
		      issues)))
	    (when (and author-id display-name (equal author-id display-name))
	      (let ((resolved-name (org-confluence-people-resolve-account-id
				    author-id (file-name-directory source-file))))
		(when (or (not resolved-name) (equal resolved-name author-id))
		  (let* ((entry (gethash author-id people))
			 (count (1+ (or (plist-get entry :count) 0)))
			 (examples (plist-get entry :examples)))
		    (puthash author-id
			     (list :display-name display-name
				   :count count
				   :examples (if (< (length examples) 3)
						 (append examples (list (or remote-id heading)))
					       examples))
			     people))))))
	  (forward-line 1))
	(maphash
	 (lambda (account-id data)
	   (let ((count (plist-get data :count))
		 (examples (plist-get data :examples)))
	     (push (org-confluence-sync-status--issue
		    "warning" "people"
		    (format "Unresolved Confluence person %s appears in %s; examples: %s"
			    account-id
			    (org-confluence-sync-status--plural count "comment")
			    (string-join examples ", "))
		    "resolve-people")
		   issues)))
	 people)))
    (nreverse issues)))

(defun org-confluence-sync-status--attachment-issues ()
  "Return sync issues for image attachments in the current Org buffer."
  (condition-case error
      (let (issues)
	(dolist (asset (org-confluence-image-assets))
	  (when (plist-get asset :missing-source)
	    (push (org-confluence-sync-status--issue
		   "warning" "attachment"
		   (format "Remote attachment %s is reused without a local file"
			   (plist-get asset :filename))
		   "download-attachment-todo")
		  issues)))
	(nreverse issues))
    (user-error
     (list (org-confluence-sync-status--issue
	    "blocking" "attachment" (error-message-string error) "fix-local-image-path")))))

(defun org-confluence-sync-status--remote-page-issues (page-id)
  "Return remote page sync issues for PAGE-ID."
  (condition-case error
      (progn
	(org-confluence-api--get-page page-id "storage")
	nil)
    (user-error
     (list (org-confluence-sync-status--issue
	    "blocking" "page" (error-message-string error) "refresh")))))

(defun org-confluence-sync-status--collect (&optional source-buffer)
  "Collect fresh Confluence sync status for SOURCE-BUFFER."
  (let* ((buffer (or source-buffer (current-buffer)))
	 (source-file (buffer-file-name buffer))
	 (page-id (with-current-buffer buffer
		    (org-confluence-api--page-id-from-buffer)))
	 remote-comment-ids
	 issues)
    (unless source-file
      (user-error "Current buffer is not visiting a file"))
    (if (not page-id)
	(push (org-confluence-sync-status--issue
	       "blocking" "page" "Missing #+CONFLUENCE_PAGE_ID" "open-page")
	      issues)
      (setq issues (append issues (org-confluence-sync-status--remote-page-issues page-id)))
      (condition-case error
	  (setq remote-comment-ids (org-confluence-sync-status--remote-comment-id-set page-id))
	(user-error
	 (push (org-confluence-sync-status--issue
		"warning" "comment"
		(format "Could not verify live Confluence comments: %s"
			(error-message-string error))
		"import")
	       issues))))
    (with-current-buffer buffer
      (setq issues (append issues (org-confluence-sync-status--attachment-issues))))
    (setq issues (append issues (org-confluence-sync-status--sidecar-issues
				 buffer remote-comment-ids)))
    (let* ((state (org-confluence-sync-status--overall-state issues))
	   (summary (org-confluence-sync-status--summary state issues)))
      `((checkedAt . ,(org-confluence-sync-status--timestamp))
	(sourceFile . ,source-file)
	(pageId . ,page-id)
	(state . ,state)
	(summary . ,summary)
	(issues . ,(vconcat issues))))))

(provide 'org-confluence-sync-status-collect)
;;; org-confluence-sync-status-collect.el ends here
