;;; org-confluence-commands.el --- Org Confluence publish commands -*- lexical-binding: t; -*-

;;; Commentary:
;; User-facing commands for publishing Org buffers to Confluence via cfl.

;;; Code:

(require 'hub-confluence-people)
(require 'hub-org-comments)
(require 'json)
(require 'org)
(require 'seq)
(require 'subr-x)
(require 'xml)

(require 'org-confluence-api)
(require 'org-confluence-export)

(defcustom hub/confluence-comment-import-resolve-people t
  "Whether comment import resolves Confluence people in bulk afterwards."
  :type 'boolean
  :group 'hub/confluence-api)

(defun hub/confluence-commands--write-temp-xhtml (xhtml)
  "Write XHTML to a temporary .xhtml file and return its path."
  (let ((file (make-temp-file "org-confluence-" nil ".xhtml")))
    (with-temp-file file
      (insert xhtml))
    file))

(defun hub/confluence-commands--run-process (command output-buffer)
  "Run shell COMMAND into OUTPUT-BUFFER and return its exit code."
  (with-current-buffer (get-buffer-create output-buffer)
    (erase-buffer)
    (process-file shell-file-name nil output-buffer nil shell-command-switch command)))

(defun hub/confluence-commands--command-output (output-buffer)
  "Return trimmed contents of OUTPUT-BUFFER."
  (when-let* ((buffer (get-buffer output-buffer)))
    (with-current-buffer buffer
      (string-trim (buffer-string)))))

(defun hub/confluence-commands--run (command)
  "Run shell COMMAND and signal `user-error' on failure."
  (unless (hub/confluence-api--cfl-available-p)
    (user-error "Cannot find `%s' on Emacs PATH. Current PATH: %s"
		hub/confluence-api-cfl-command
		(getenv "PATH")))
  (let* ((output-buffer "*hub-confluence-cfl*")
	 (default-directory user-emacs-directory)
	 (exit-code (hub/confluence-commands--run-process command output-buffer)))
    (unless (zerop exit-code)
      (let ((output (hub/confluence-commands--command-output output-buffer)))
	(user-error "Confluence publish failed with exit code %s.\nCommand: %s\nOutput: %s"
		    exit-code command (if (string-empty-p (or output "")) "<empty>" output))))
    exit-code))

(defun hub/confluence-commands--run-output (command)
  "Run shell COMMAND and return its trimmed output."
  (unless (hub/confluence-api--cfl-available-p)
    (user-error "Cannot find `%s' on Emacs PATH. Current PATH: %s"
		hub/confluence-api-cfl-command
		(getenv "PATH")))
  (let* ((output-buffer "*hub-confluence-cfl*")
	 (default-directory user-emacs-directory)
	 (exit-code (hub/confluence-commands--run-process command output-buffer))
	 (output (hub/confluence-commands--command-output output-buffer)))
    (unless (zerop exit-code)
      (user-error "Confluence command failed with exit code %s.\nCommand: %s\nOutput: %s"
		  exit-code command (if (string-empty-p (or output "")) "<empty>" output)))
    output))

(defun hub/confluence-commands--response-body (response)
  "Return JSON body string from REST RESPONSE."
  (cond
   ((stringp response) response)
   ((plist-member response :body) (plist-get response :body))
   (t (user-error "Confluence REST response did not include a body"))))

(defun hub/confluence-commands--json-alist (json)
  "Parse JSON string JSON into alists and lists."
  (json-parse-string json :object-type 'alist :array-type 'list :null-object nil :false-object nil))

(defun hub/confluence-commands--comment-results (response)
  "Return comment results from REST RESPONSE."
  (let* ((json (hub/confluence-commands--json-alist
		(hub/confluence-commands--response-body response)))
	 (results (or (alist-get 'results json)
		      (alist-get 'values json))))
    (unless (listp results)
      (user-error "Confluence comment response did not include results"))
    results))

(defun hub/confluence-commands--user-results (response)
  "Return user results from REST RESPONSE."
  (let* ((json (hub/confluence-commands--json-alist
		(hub/confluence-commands--response-body response)))
	 (results (alist-get 'results json)))
    (unless (listp results)
      (user-error "Confluence user response did not include results"))
    results))

(defun hub/confluence-commands--comment-body-storage (comment)
  "Return COMMENT storage body alist, or nil."
  (alist-get 'storage (alist-get 'body comment)))

(defun hub/confluence-commands--raw-resolution-fields (comment)
  "Return raw resolution fields found in COMMENT."
  (let ((properties (alist-get 'properties comment))
	fields)
    (dolist (key '(resolutionStatus resolution-status resolved))
      (when-let* ((value (alist-get key comment)))
	(push (format "%s=%S" key value) fields)))
    (when (listp properties)
      (dolist (key '(resolutionStatus resolution-status resolved))
	(when-let* ((value (alist-get key properties)))
	  (push (format "properties.%s=%S" key value) fields))))
    (nreverse fields)))

(defun hub/confluence-commands--raw-status-fields (comment)
  "Return raw non-resolution status fields found in COMMENT."
  (let ((properties (alist-get 'properties comment))
	fields)
    (when-let* ((value (alist-get 'status comment)))
      (push (format "status=%S" value) fields))
    (when (listp properties)
      (when-let* ((value (alist-get 'status properties)))
	(push (format "properties.status=%S" value) fields)))
    (nreverse fields)))

(defun hub/confluence-commands--insert-comment-diagnostic (comment)
  "Insert one remote COMMENT diagnostic entry at point."
  (let* ((id (or (alist-get 'id comment) "<missing id>"))
	 (storage (hub/confluence-commands--comment-body-storage comment))
	 (representation (or (alist-get 'representation storage) "unknown"))
	 (resolution (or (hub/confluence-commands--remote-comment-resolution-status comment)
			 "unknown"))
	 (raw-resolution-fields (hub/confluence-commands--raw-resolution-fields comment))
	 (raw-status-fields (hub/confluence-commands--raw-status-fields comment))
	 (value (or (alist-get 'value storage) "")))
    (insert (format "- id: %s\n" id))
    (insert (format "  resolution: %s\n" resolution))
    (when raw-resolution-fields
      (insert (format "  resolution-fields: %s\n"
		      (string-join raw-resolution-fields ", "))))
    (when raw-status-fields
      (insert (format "  status-fields: %s\n"
		      (string-join raw-status-fields ", "))))
    (insert (format "  representation: %s\n" representation))
    (unless (string-empty-p value)
      (insert "  body:\n")
      (dolist (line (split-string value "\n"))
	(insert (format "    %s\n" line))))))

(defun hub/confluence-commands--insert-comment-section (kind body-format response)
  "Insert diagnostic section for comment KIND and REST RESPONSE."
  (let ((comments (hub/confluence-commands--comment-results response)))
    (insert (format "* %s\n" kind))
    (insert (format "body-format: %s\n" body-format))
    (insert (format "count: %s\n\n" (length comments)))
    (if comments
	(dolist (comment comments)
	  (hub/confluence-commands--insert-comment-diagnostic comment))
      (insert "No comments.\n"))))

(defun hub/confluence-commands--remote-comment-id (comment)
  "Return remote Confluence COMMENT id as a string, or nil."
  (when-let* ((id (alist-get 'id comment)))
    (format "%s" id)))

(defun hub/confluence-commands--remote-comment-body (comment)
  "Return raw storage body for remote Confluence COMMENT."
  (or (alist-get 'value (hub/confluence-commands--comment-body-storage comment)) ""))

(defun hub/confluence-commands--remote-comment-author-id (comment)
  "Return remote author ID for Confluence COMMENT, or nil."
  (or (alist-get 'authorId comment)
      (alist-get 'author-id comment)
      (alist-get 'authorId (alist-get 'version comment))
      (alist-get 'author-id (alist-get 'version comment))
      (alist-get 'accountId (alist-get 'author comment))))

(defun hub/confluence-commands--remote-comment-author-display-name (comment)
  "Return display author name for Confluence COMMENT, or nil."
  (or (alist-get 'displayName (alist-get 'author comment))
      (alist-get 'publicName (alist-get 'author comment))))

(defun hub/confluence-commands--remote-comment-author-name (comment)
  "Return best author label for Confluence COMMENT, or nil."
  (or (hub/confluence-commands--remote-comment-author-display-name comment)
      (hub/confluence-commands--remote-comment-author-id comment)))

(defun hub/confluence-commands--remote-comment-created-at (comment)
  "Return remote created timestamp for Confluence COMMENT, or nil."
  (or (alist-get 'createdAt comment)
      (alist-get 'created-at comment)
      (alist-get 'createdAt (alist-get 'version comment))
      (alist-get 'created-at (alist-get 'version comment))))

(defun hub/confluence-commands--remote-comment-resolution-status (comment)
  "Return normalized remote resolution status for Confluence COMMENT, or nil."
  (let* ((properties (alist-get 'properties comment))
	 (raw (or (alist-get 'resolutionStatus comment)
		  (alist-get 'resolution-status comment)
		  (alist-get 'resolved comment)
		  (and (listp properties)
		       (or (alist-get 'resolutionStatus properties)
			   (alist-get 'resolution-status properties)
			   (alist-get 'resolved properties))))))
    (cond
     ((eq raw t) "resolved")
     ((null raw) nil)
     ((stringp raw)
      (let ((value (downcase (string-trim raw))))
	(cond
	 ((member value '("resolved" "closed" "done" "true")) "resolved")
	 ((member value '("open" "unresolved" "reopened" "active" "false")) "open"))))
     (t nil))))

(defun hub/confluence-commands--remote-inline-target-object (comment)
  "Return best available raw inline target object from COMMENT, or nil."
  (or (alist-get 'inlineCommentProperties comment)
      (alist-get 'inline-comment-properties comment)
      (alist-get 'target comment)
      (alist-get 'properties comment)))

(defun hub/confluence-commands--remote-inline-target-text (comment)
  "Return best available selected target text from inline COMMENT, or nil."
  (let ((target (hub/confluence-commands--remote-inline-target-object comment)))
    (or (alist-get 'originalSelection comment)
	(alist-get 'original-selection comment)
	(alist-get 'selectedText comment)
	(alist-get 'selected-text comment)
	(alist-get 'text comment)
	(and (listp target)
	     (or (alist-get 'originalSelection target)
		 (alist-get 'original-selection target)
		 (alist-get 'selectedText target)
		 (alist-get 'selected-text target)
		 (alist-get 'text target))))))

(defun hub/confluence-commands--property-line (key value)
  "Return an Org property line for KEY and VALUE when VALUE is present."
  (when (and (stringp value) (not (string-empty-p (string-trim value))))
    (format ":%s: %s\n" key (string-trim value))))

(defun hub/confluence-commands--people-cache-file (directory)
  "Return people cache file for DIRECTORY, preferring an existing local file."
  (let ((local (and directory (hub/confluence-people-local-file directory))))
    (if (and local (file-exists-p local))
	local
      (hub/confluence-people-global-file))))

(defun hub/confluence-commands--cache-comment-author (comment &optional directory)
  "Cache Confluence author identity from COMMENT when author data is present."
  (let ((author-id (hub/confluence-commands--remote-comment-author-id comment))
	(display-name (hub/confluence-commands--remote-comment-author-display-name comment)))
    (when author-id
      (hub/confluence-people-cache-identity
       author-id display-name nil (hub/confluence-commands--people-cache-file directory)))))

(defun hub/confluence-commands--remote-comment-parent-id (comment)
  "Return remote parent comment ID for COMMENT, or nil."
  (when-let* ((id (or (alist-get 'parentCommentId comment)
		      (alist-get 'parent-comment-id comment))))
    (format "%s" id)))

(defun hub/confluence-commands--sidecar-has-remote-comment-p (sidecar-file remote-id)
  "Return non-nil when SIDECAR-FILE already has Confluence REMOTE-ID."
  (when (file-exists-p sidecar-file)
    (with-temp-buffer
      (insert-file-contents sidecar-file)
      (org-mode)
      (goto-char (point-min))
      (cl-loop while (re-search-forward org-heading-regexp nil t)
	       do (goto-char (match-beginning 0))
	       when (equal remote-id (org-entry-get nil "HUB_COMMENT_REMOTE_ID"))
	       return t
	       do (forward-line 1)))))

(defun hub/confluence-commands--put-property-when-missing (property value)
  "Set Org PROPERTY to VALUE at point only when missing."
  (when (and (stringp value)
	     (not (string-empty-p (string-trim value)))
	     (not (org-entry-get nil property)))
    (org-entry-put nil property (string-trim value))))

(defun hub/confluence-commands--backfill-remote-footer-metadata (sidecar-file comment)
  "Backfill missing sidecar metadata for existing remote footer COMMENT."
  (let ((remote-id (hub/confluence-commands--remote-comment-id comment)))
    (when (and remote-id (file-exists-p sidecar-file))
      (with-temp-buffer
	(insert-file-contents sidecar-file)
	(org-mode)
	(goto-char (point-min))
	(when (cl-loop while (re-search-forward org-heading-regexp nil t)
		       do (goto-char (match-beginning 0))
		       when (and (equal "confluence" (org-entry-get nil "HUB_COMMENT_SOURCE"))
				 (equal remote-id (org-entry-get nil "HUB_COMMENT_REMOTE_ID")))
		       return t
		       do (forward-line 1))
	  (let ((author-name (hub/confluence-commands--remote-comment-author-name comment))
		(author-id (hub/confluence-commands--remote-comment-author-id comment))
		(created-at (hub/confluence-commands--remote-comment-created-at comment)))
	    (hub/confluence-commands--put-property-when-missing "HUB_COMMENT_REMOTE_AUTHOR_ID" author-id)
	    (hub/confluence-commands--put-property-when-missing
	     "HUB_COMMENT_REMOTE_AUTHOR_DISPLAY_NAME" author-name)
	    (hub/confluence-commands--put-property-when-missing
	     "HUB_COMMENT_REMOTE_CREATED_AT" created-at)
	    (write-region (point-min) (point-max) sidecar-file nil 'silent)))))))

(defun hub/confluence-commands--sync-timestamp ()
  "Return timestamp used for Confluence comment sync metadata."
  (format-time-string "%Y-%m-%dT%H:%M:%S%z"))

(defun hub/confluence-commands--remote-comment-common-properties (comment body-format sync-kind)
  "Return common sidecar property text for COMMENT, BODY-FORMAT, and SYNC-KIND."
  (let* ((remote-id (hub/confluence-commands--remote-comment-id comment))
	 (local-id (format "remote-confluence-%s" remote-id))
	 (author-name (hub/confluence-commands--remote-comment-author-name comment))
	 (author-id (hub/confluence-commands--remote-comment-author-id comment))
	 (resolution-status (hub/confluence-commands--remote-comment-resolution-status comment))
	 (created-at (hub/confluence-commands--remote-comment-created-at comment)))
    (concat
     (format ":HUB_COMMENT_ID: %s\n" local-id)
     (format ":HUB_COMMENT_REMOTE_ID: %s\n" remote-id)
     ":HUB_COMMENT_SOURCE: confluence\n"
     (format ":HUB_COMMENT_SYNC_KIND: %s\n" sync-kind)
     (unless (equal body-format "storage")
       (format ":HUB_COMMENT_BODY_FORMAT: %s\n" body-format))
     (hub/confluence-commands--property-line "HUB_COMMENT_REMOTE_LAST_SEEN_AT"
					     (hub/confluence-commands--sync-timestamp))
     (hub/confluence-commands--property-line "HUB_COMMENT_REMOTE_RESOLUTION_STATUS" resolution-status)
     (hub/confluence-commands--property-line "HUB_COMMENT_REMOTE_AUTHOR_ID" author-id)
     (hub/confluence-commands--property-line "HUB_COMMENT_REMOTE_AUTHOR_DISPLAY_NAME" author-name)
     (hub/confluence-commands--property-line "HUB_COMMENT_REMOTE_CREATED_AT" created-at))))

(defun hub/confluence-commands--remote-title-record (comment body-format sync-kind)
  "Return title record for remote COMMENT with BODY-FORMAT and SYNC-KIND."
  (list :author (hub/confluence-commands--remote-comment-author-name comment)
	:remote-author-id (hub/confluence-commands--remote-comment-author-id comment)
	:remote-author-display-name (hub/confluence-commands--remote-comment-author-name comment)
	:created-at (hub/confluence-commands--remote-comment-created-at comment)
	:sync-kind sync-kind
	:body-format body-format
	:target-text (hub/confluence-commands--remote-inline-target-text comment)
	:body (hub/confluence-commands--remote-comment-body comment)))

(defun hub/confluence-commands--remote-reply-entry (comment body-format parent-remote-id &optional directory)
  "Return sidecar Org child entry text for reply COMMENT."
  (let ((parent-id (or parent-remote-id
		       (hub/confluence-commands--remote-comment-parent-id comment))))
    (concat
     (format "** %s\n"
	     (hub/org-comment-reply-heading-title
	      (hub/confluence-commands--remote-title-record comment body-format "reply")
	      directory))
     ":PROPERTIES:\n"
     (hub/confluence-commands--remote-comment-common-properties comment body-format "reply")
     (hub/confluence-commands--property-line "HUB_COMMENT_REMOTE_PARENT_ID" parent-id)
     ":END:\n\n"
     (string-trim-right (hub/confluence-commands--remote-comment-body comment))
     "\n")))

(defun hub/confluence-commands--remote-orphan-thread-entry (parent-remote-id &optional _directory)
  "Return placeholder sidecar root entry for missing PARENT-REMOTE-ID."
  (concat
   (format "* OPEN Remote conversation %s — Missing parent comment\n" parent-remote-id)
   ":PROPERTIES:\n"
   (format ":HUB_COMMENT_ID: remote-confluence-%s\n" parent-remote-id)
   (format ":HUB_COMMENT_REMOTE_ID: %s\n" parent-remote-id)
   ":HUB_COMMENT_SOURCE: confluence\n"
   ":HUB_COMMENT_SYNC_KIND: orphan-thread\n"
   ":HUB_COMMENT_ANCHOR_STATE: missing-parent\n"
   ":END:\n\n"
   "Parent comment was not returned by Confluence import.\n"))

(defun hub/confluence-commands--initial-local-status (comment)
  "Return initial local TODO status for remote COMMENT."
  (if (equal "resolved" (hub/confluence-commands--remote-comment-resolution-status comment))
      "RESOLVED"
    "OPEN"))

(defun hub/confluence-commands--remote-footer-entry (comment body-format &optional directory)
  "Return sidecar Org entry text for remote footer COMMENT with BODY-FORMAT."
  (let ((remote-id (hub/confluence-commands--remote-comment-id comment)))
    (concat
     (format "* %s %s\n" (hub/confluence-commands--initial-local-status comment)
	     (hub/org-comment-heading-title
	      (hub/confluence-commands--remote-title-record comment body-format "footer")
	      directory))
     ":PROPERTIES:\n"
     (hub/confluence-commands--remote-comment-common-properties comment body-format "footer")
     ":END:\n\n"
     (string-trim-right (hub/confluence-commands--remote-comment-body comment))
     "\n")))

(defun hub/confluence-commands--remote-inline-entry (comment body-format &optional directory)
  "Return sidecar Org entry text for remote inline COMMENT with BODY-FORMAT."
  (let* ((remote-id (hub/confluence-commands--remote-comment-id comment))
	 (target-text (hub/confluence-commands--remote-inline-target-text comment)))
    (concat
     (format "* %s %s\n" (hub/confluence-commands--initial-local-status comment)
	     (hub/org-comment-heading-title
	      (hub/confluence-commands--remote-title-record comment body-format "inline")
	      directory))
     ":PROPERTIES:\n"
     (hub/confluence-commands--remote-comment-common-properties comment body-format "inline")
     (hub/confluence-commands--property-line "HUB_COMMENT_TARGET_TEXT" target-text)
     ":END:\n\n"
     (string-trim-right (hub/confluence-commands--remote-comment-body comment))
     "\n")))

(defun hub/confluence-commands--append-remote-comment-entry (sidecar-file source-file entry)
  "Append remote comment ENTRY to SIDECAR-FILE for SOURCE-FILE."
  (hub/org-comment--ensure-sidecar-header sidecar-file source-file)
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

(defun hub/confluence-commands--append-remote-footer-comment (sidecar-file source-file comment body-format)
  "Append remote footer COMMENT to SIDECAR-FILE for SOURCE-FILE."
  (hub/confluence-commands--append-remote-comment-entry
   sidecar-file source-file
   (hub/confluence-commands--remote-footer-entry
    comment body-format (file-name-directory sidecar-file))))

(defun hub/confluence-commands--append-remote-inline-comment (sidecar-file source-file comment body-format)
  "Append remote inline COMMENT to SIDECAR-FILE for SOURCE-FILE."
  (hub/confluence-commands--append-remote-comment-entry
   sidecar-file source-file
   (hub/confluence-commands--remote-inline-entry
    comment body-format (file-name-directory sidecar-file))))

(defun hub/confluence-commands--append-remote-reply-comment
    (sidecar-file source-file comment body-format parent-remote-id)
  "Append remote reply COMMENT under PARENT-REMOTE-ID in SIDECAR-FILE."
  (hub/org-comment--ensure-sidecar-header sidecar-file source-file)
  (with-temp-buffer
    (insert-file-contents sidecar-file)
    (org-mode)
    (goto-char (point-min))
    (unless (cl-loop while (re-search-forward org-heading-regexp nil t)
		     do (goto-char (match-beginning 0))
		     when (and (equal "confluence" (org-entry-get nil "HUB_COMMENT_SOURCE"))
			       (equal parent-remote-id (org-entry-get nil "HUB_COMMENT_REMOTE_ID")))
		     return (let ((end (save-excursion (org-end-of-subtree t t))))
			      (goto-char end)
			      (unless (bolp) (insert "\n"))
			      (unless (save-excursion
					(forward-line -1)
					(looking-at-p "[[:space:]]*$"))
				(insert "\n"))
			      (insert (hub/confluence-commands--remote-reply-entry
				       comment body-format parent-remote-id
				       (file-name-directory sidecar-file)))
			      t)
		     do (forward-line 1))
      (hub/confluence-commands--append-remote-comment-entry
       sidecar-file source-file
       (hub/confluence-commands--remote-orphan-thread-entry
	parent-remote-id (file-name-directory sidecar-file)))
      (goto-char (point-min))
      (re-search-forward org-heading-regexp nil t)
      (goto-char (point-max))
      (insert (hub/confluence-commands--remote-reply-entry
	       comment body-format parent-remote-id
	       (file-name-directory sidecar-file))))
    (write-region (point-min) (point-max) sidecar-file nil 'silent)))

(defun hub/confluence-commands--title-from-buffer ()
  "Return the Org #+TITLE value from the current buffer, or nil."
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search t))
      (when (re-search-forward "^[	]*#\\+TITLE:[	]*\\(.*?\\)[	]*$" nil t)
	(let ((title (string-trim (match-string-no-properties 1))))
	  (unless (string-empty-p title) title))))))

(defun hub/confluence-commands--asset-filename-map (assets)
  "Return export filename mapping for image ASSETS."
  (mapcar (lambda (asset)
	    (cons (plist-get asset :source-link)
		  (plist-get asset :filename)))
	  assets))

(defun hub/confluence-commands--duplicate-attachment-error-p (error)
  "Return non-nil when ERROR is cfl's duplicate attachment failure."
  (string-match-p "Cannot add a new attachment with same file name as an existing attachment"
		  (error-message-string error)))

(defun hub/confluence-import--node-tag (node)
  "Return NODE tag, or nil for text nodes."
  (when (consp node) (car node)))

(defun hub/confluence-import--node-attributes (node)
  "Return NODE attributes."
  (when (consp node) (cadr node)))

(defun hub/confluence-import--node-children (node)
  "Return NODE children."
  (when (consp node) (cddr node)))

(defun hub/confluence-import--attribute (node attribute)
  "Return NODE ATTRIBUTE value."
  (cdr (assq attribute (hub/confluence-import--node-attributes node))))

(defun hub/confluence-import--macro-name (node)
  "Return Confluence macro name for NODE."
  (hub/confluence-import--attribute node 'ac:name))

(defun hub/confluence-import--macro-parameter (node name)
  "Return Confluence macro NODE parameter NAME, or nil."
  (seq-some (lambda (child)
	      (when (and (eq (hub/confluence-import--node-tag child) 'ac:parameter)
			 (string= (or (hub/confluence-import--attribute child 'ac:name) "") name))
		(hub/confluence-import--inline child)))
	    (hub/confluence-import--node-children node)))

(defun hub/confluence-import--status (node)
  "Convert Confluence status macro NODE to an Org status link."
  (let ((title (or (hub/confluence-import--macro-parameter node "title") ""))
	(colour (or (hub/confluence-import--macro-parameter node "colour") "Grey")))
    (format "[[confluence-status:%s][%s]]"
	    (string-trim colour)
	    (string-trim title))))

(defun hub/confluence-import--emoticon-unicode-fallback (node)
  "Return known Unicode fallback for Confluence emoticon NODE."
  (let ((id (hub/confluence-import--attribute node 'ac:emoji-id))
	(name (hub/confluence-import--attribute node 'ac:name))
	(shortname (hub/confluence-import--attribute node 'ac:emoji-shortname)))
    (cond
     ((or (string= (or id "") "atlassian-info")
	  (string= (or name "") "information")
	  (string= (or shortname "") ":info:"))
      "ℹ️")
     ((string= (or shortname "") ":document:")
      "📄"))))

(defun hub/confluence-import--emoticon (node)
  "Convert Confluence emoticon NODE to readable Org text."
  (or (hub/confluence-import--emoticon-unicode-fallback node)
      (hub/confluence-import--attribute node 'ac:emoji-fallback)
      (hub/confluence-import--attribute node 'ac:emoji-shortname)
      (hub/confluence-import--attribute node 'ac:name)
      ""))

(defun hub/confluence-import--callout-macro-p (node)
  "Return non-nil when NODE is a panel-like Confluence macro."
  (member (hub/confluence-import--macro-name node)
	  '("info" "note" "warning" "tip" "panel")))

(defun hub/confluence-import--rich-text-body (node)
  "Return Org text converted from NODE's rich text body."
  (hub/confluence-import--join-blocks
   (mapcar (lambda (child)
	     (when (eq (hub/confluence-import--node-tag child) 'ac:rich-text-body)
	       (hub/confluence-import--block child)))
	   (hub/confluence-import--node-children node))))

(defun hub/confluence-import--callout (node)
  "Convert Confluence panel-like macro NODE to semantic Org callout."
  (let* ((type (hub/confluence-import--macro-name node))
	 (title (hub/confluence-import--macro-parameter node "title"))
	 (attributes (format "#+ATTR_CALLOUT: :type %s" type))
	 (body (hub/confluence-import--rich-text-body node)))
    (when (and title (not (string-empty-p (string-trim title))))
      (setq attributes (format "%s :title %S" attributes (string-trim title))))
    (format "%s\n#+begin_callout\n%s\n#+end_callout" attributes body)))

(defun hub/confluence-import--clean-inline-spacing (text)
  "Clean Org inline TEXT spacing introduced for markup boundaries."
  (let ((cleaned (replace-regexp-in-string "[[:space:]]+" " " text)))
    (setq cleaned (replace-regexp-in-string "[[:space:]]+\\([.,;:!?)]\\)" "\\1" cleaned))
    (string-trim cleaned)))

(defun hub/confluence-import--markup (marker contents)
  "Return Org inline markup using MARKER around CONTENTS."
  (format " %s%s%s " marker (string-trim contents) marker))

(defun hub/confluence-import--inline (node)
  "Convert XHTML NODE to inline Org text."
  (cond
   ((stringp node) node)
   ((not (consp node)) "")
   (t
    (let ((contents (mapconcat #'hub/confluence-import--inline
			       (hub/confluence-import--node-children node) "")))
      (pcase (hub/confluence-import--node-tag node)
	('ac:emoticon (hub/confluence-import--emoticon node))
	('ac:structured-macro
	 (if (string= (or (hub/confluence-import--macro-name node) "") "status")
	     (hub/confluence-import--status node)
	   (hub/confluence-import--clean-inline-spacing contents)))
	((or 'strong 'b) (hub/confluence-import--markup "*" contents))
	((or 'em 'i) (hub/confluence-import--markup "/" contents))
	('u (hub/confluence-import--markup "_" contents))
	('strike (hub/confluence-import--markup "+" contents))
	('code (hub/confluence-import--markup "~" contents))
	('a (let ((href (hub/confluence-import--attribute node 'href)))
	      (if href (format "[[%s][%s]]" href (hub/confluence-import--clean-inline-spacing contents))
		(hub/confluence-import--clean-inline-spacing contents))))
	(_ (hub/confluence-import--clean-inline-spacing contents)))))))

(defun hub/confluence-import--join-blocks (blocks)
  "Join non-empty Org BLOCKS with newlines."
  (string-join (seq-filter (lambda (block)
			     (not (string-empty-p (string-trim (or block "")))))
			   blocks)
	       "\n"))

(defun hub/confluence-import--list-child-p (node)
  "Return non-nil when NODE is a list element."
  (memq (hub/confluence-import--node-tag node) '(ul ol)))

(defun hub/confluence-import--list-item-text (item)
  "Return ITEM direct inline text, excluding nested lists."
  (string-trim
   (mapconcat #'hub/confluence-import--inline
	      (seq-remove #'hub/confluence-import--list-child-p
			  (hub/confluence-import--node-children item))
	      "")))

(defun hub/confluence-import--indent-lines (text spaces)
  "Indent every line in TEXT by SPACES spaces."
  (let ((prefix (make-string spaces ?\s)))
    (string-join
     (mapcar (lambda (line) (concat prefix line))
	     (split-string text "\n"))
     "\n")))

(defun hub/confluence-import--list (list-node &optional level)
  "Convert LIST-NODE to an Org list at nesting LEVEL."
  (let ((index 0)
	(level (or level 0))
	(ordered (eq (hub/confluence-import--node-tag list-node) 'ol)))
    (hub/confluence-import--join-blocks
     (mapcar
      (lambda (child)
	(when (eq (hub/confluence-import--node-tag child) 'li)
	  (when ordered (setq index (1+ index)))
	  (let* ((marker (if ordered (format "%d." index) "-"))
		 (line (format "%s%s %s" (make-string (* 2 level) ?\s)
			       marker
			       (hub/confluence-import--list-item-text child)))
		 (nested (hub/confluence-import--join-blocks
			  (mapcar (lambda (nested-list)
				    (when (hub/confluence-import--list-child-p nested-list)
				      (hub/confluence-import--list nested-list (1+ level))))
				  (hub/confluence-import--node-children child)))))
	    (hub/confluence-import--join-blocks (list line nested)))))
      (hub/confluence-import--node-children list-node)))))

(defun hub/confluence-import--children-by-tag (node tag)
  "Return direct children of NODE with TAG."
  (seq-filter (lambda (child) (eq (hub/confluence-import--node-tag child) tag))
	      (hub/confluence-import--node-children node)))

(defun hub/confluence-import--table-rows (table)
  "Return Confluence storage TABLE rows."
  (let ((direct-rows (hub/confluence-import--children-by-tag table 'tr)))
    (if direct-rows
	direct-rows
      (apply #'append
	     (mapcar (lambda (section)
		       (hub/confluence-import--children-by-tag section 'tr))
		     (seq-filter (lambda (child)
				   (memq (hub/confluence-import--node-tag child) '(thead tbody)))
				 (hub/confluence-import--node-children table)))))))

(defun hub/confluence-import--table-row-cells (row)
  "Return ROW cells as Org-safe inline text."
  (mapcar (lambda (cell)
	    (string-trim
	     (replace-regexp-in-string "|" "\\\\vert{}"
				       (hub/confluence-import--inline cell)
				       t t)))
	  (seq-filter (lambda (child)
			(memq (hub/confluence-import--node-tag child) '(th td)))
		      (hub/confluence-import--node-children row))))

(defun hub/confluence-import--org-table-line (cells)
  "Return an Org table row for CELLS."
  (format "| %s |" (string-join cells " | ")))

(defun hub/confluence-import--org-table-separator (cells)
  "Return an Org table separator sized for CELLS."
  (format "|%s|"
	  (string-join
	   (mapcar (lambda (cell)
		     (make-string (+ 2 (length cell)) ?-))
		   cells)
	   "+")))

(defun hub/confluence-import--table (table)
  "Convert Confluence storage TABLE to an Org table."
  (let ((rows (mapcar #'hub/confluence-import--table-row-cells
		      (hub/confluence-import--table-rows table))))
    (hub/confluence-import--join-blocks
     (append (list (hub/confluence-import--org-table-line (car rows))
		   (hub/confluence-import--org-table-separator (car rows)))
	     (mapcar #'hub/confluence-import--org-table-line (cdr rows))))))

(defun hub/confluence-import--block (node)
  "Convert XHTML NODE to Org block text."
  (pcase (hub/confluence-import--node-tag node)
    ((or 'html 'body 'ac:rich-text-body)
     (hub/confluence-import--join-blocks
      (mapcar #'hub/confluence-import--block
	      (hub/confluence-import--node-children node))))
    ('ac:structured-macro
     (cond
      ((string= (or (hub/confluence-import--macro-name node) "") "status")
       (hub/confluence-import--status node))
      ((hub/confluence-import--callout-macro-p node)
       (hub/confluence-import--callout node))
      (t (hub/confluence-import--rich-text-body node))))
    ('h1 (format "* %s" (hub/confluence-import--inline node)))
    ('h2 (format "** %s" (hub/confluence-import--inline node)))
    ('h3 (format "*** %s" (hub/confluence-import--inline node)))
    ('h4 (format "**** %s" (hub/confluence-import--inline node)))
    ('p (hub/confluence-import--inline node))
    ((or 'ul 'ol) (hub/confluence-import--list node))
    ('table (hub/confluence-import--table node))
    (_ (hub/confluence-import--inline node))))

(defun hub/confluence-import-storage-to-org (xhtml)
  "Convert Confluence storage XHTML string XHTML to Org text."
  (with-temp-buffer
    (insert xhtml)
    (let ((tree (libxml-parse-html-region (point-min) (point-max))))
      (string-trim (hub/confluence-import--block tree)))))

(defun hub/confluence-commands--upload-asset (page-id asset upload-directory)
  "Upload one image ASSET to Confluence PAGE-ID from UPLOAD-DIRECTORY."
  (let ((upload-path (expand-file-name (plist-get asset :filename) upload-directory)))
    (copy-file (plist-get asset :source-path) upload-path t)
    (condition-case err
	(hub/confluence-commands--run
	 (hub/confluence-api--attachment-upload-command page-id upload-path))
      (user-error
       (if (hub/confluence-commands--duplicate-attachment-error-p err)
	   (message "Confluence attachment already exists, reusing %s"
		    (plist-get asset :filename))
	 (signal (car err) (cdr err)))))))

(defun hub/confluence-commands--unique-upload-assets (assets)
  "Return ASSETS deduplicated by generated attachment filename."
  (let ((seen nil)
	(unique nil))
    (dolist (asset assets (nreverse unique))
      (let ((filename (plist-get asset :filename)))
	(unless (member filename seen)
	  (push filename seen)
	  (push asset unique))))))

(defun hub/confluence-commands--upload-assets (page-id assets)
  "Upload image ASSETS to Confluence PAGE-ID."
  (when assets
    (let ((upload-directory (make-temp-file "org-confluence-assets-" t)))
      (unwind-protect
	  (dolist (asset (hub/confluence-commands--unique-upload-assets assets))
	    (hub/confluence-commands--upload-asset page-id asset upload-directory))
	(delete-directory upload-directory t)))))

;;;###autoload
(defun hub/confluence-publish (&optional async subtreep visible-only body-only ext-plist)
  "Publish the current Org buffer or subtree to an existing Confluence page.

The document must contain #+CONFLUENCE_PAGE_ID.  When SUBTREEP is non-nil, a
CONFLUENCE_PAGE_ID Org property on the current subtree takes precedence.  The
Org document is exported to Confluence Storage Format XHTML and passed to `cfl
page edit --file --storage'.  ASYNC, SUBTREEP, VISIBLE-ONLY, BODY-ONLY, and
EXT-PLIST follow Org export conventions."
  (interactive)
  (ignore async)
  (let* ((page-id (hub/confluence-api--page-id-from-buffer subtreep))
	 (assets (org-confluence-image-assets subtreep))
	 (xhtml-file nil))
    (unwind-protect
	(progn
	  (setq xhtml-file
		(hub/confluence-commands--write-temp-xhtml
		 (org-confluence-export nil subtreep visible-only body-only
					(append (list :confluence-image-filenames
						      (hub/confluence-commands--asset-filename-map assets))
						ext-plist))))
	  (hub/confluence-commands--upload-assets page-id assets)
	  (hub/confluence-commands--run
	   (hub/confluence-api--page-update-command page-id xhtml-file))
	  (message "Published Org buffer to Confluence page %s" page-id)
	  page-id)
      (when (and xhtml-file (file-exists-p xhtml-file))
	(delete-file xhtml-file)))))

;;;###autoload
(defun hub/confluence-publish-from-export-dispatch
    (&optional async subtreep visible-only body-only ext-plist)
  "Publish or create through `org-export-dispatch' using Org export options."
  (interactive)
  (hub/confluence-publish-dwim nil nil async subtreep visible-only body-only ext-plist))

;;;###autoload
(defun hub/confluence-open-page (&optional page-id space)
  "Open Confluence PAGE-ID in browser using optional SPACE."
  (interactive)
  (let* ((id (or page-id (hub/confluence-api--page-id-from-buffer)))
	 (page-space (or space (hub/confluence-api--space-from-buffer)))
	 (url (hub/confluence-api--page-url id page-space)))
    (browse-url url)
    url))

;;;###autoload
(defun hub/confluence-publish-and-open-from-export-dispatch
    (&optional async subtreep visible-only body-only ext-plist)
  "Publish or create through `org-export-dispatch', then open the page."
  (interactive)
  (let* ((page-id (hub/confluence-publish-dwim nil nil async subtreep visible-only body-only ext-plist))
	 (space (hub/confluence-api--space-from-buffer)))
    (hub/confluence-open-page page-id space)))

(defun hub/confluence-commands--created-page-id (output)
  "Return Confluence page ID parsed from cfl create OUTPUT, or nil."
  (cond
   ((string-match "\"id\"[[:space:]]*:[[:space:]]*\"?\\([0-9]+\\)\"?" output)
    (match-string 1 output))
   ((string-match "\\b\\(?:Page[[:space:]]+\\)?ID[[:space:]]*[:=][[:space:]]*\\([0-9]+\\)" output)
    (match-string 1 output))
   ((string-match "/pages/\\([0-9]+\\)" output)
    (match-string 1 output))))

(defun hub/confluence-commands--metadata-keyword-present-p (keyword)
  "Return non-nil when Org metadata KEYWORD is present in the current buffer."
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search t)
	  (regexp (format "^[ \t]*#\\+%s:" (regexp-quote keyword))))
      (re-search-forward regexp nil t))))

(defun hub/confluence-commands--insert-metadata-after-keywords (metadata)
  "Insert missing Confluence METADATA after leading Org keyword lines.

METADATA is an alist of Org keyword names to values."
  (let ((missing (seq-filter (lambda (entry)
			       (not (hub/confluence-commands--metadata-keyword-present-p (car entry))))
			     metadata)))
    (when missing
      (save-excursion
	(goto-char (point-min))
	(while (looking-at "^[ \t]*#\\+[^:\n]+:.*$")
	  (forward-line 1))
	(dolist (entry missing)
	  (insert (format "#+%s: %s\n" (car entry) (cdr entry))))))))

(defun hub/confluence-publish--record-created-page (page-id space)
  "Record created PAGE-ID and SPACE metadata in the current Org buffer."
  (hub/confluence-commands--insert-metadata-after-keywords
   `(("CONFLUENCE_PAGE_ID" . ,page-id)
     ("CONFLUENCE_SPACE" . ,space))))

;;;###autoload
(defun hub/confluence-pull (&optional page-id)
  "Fetch Confluence PAGE-ID and open a new Org buffer with imported content.

When PAGE-ID is nil, default to #+CONFLUENCE_PAGE_ID in the current Org buffer
and prompt if no page ID is available.  The page is fetched as raw Confluence
storage XHTML using cfl and converted to a conservative Org representation."
  (interactive)
  (let* ((id (or page-id
		 (hub/confluence-api--page-id-from-buffer)
		 (read-string "Confluence page ID: ")))
	 (xhtml (hub/confluence-commands--run-output
		 (hub/confluence-api--page-view-storage-command id)))
	 (org (hub/confluence-import-storage-to-org xhtml))
	 (buffer (generate-new-buffer (format "*Confluence %s*" id))))
    (with-current-buffer buffer
      (org-mode)
      (insert (format "#+CONFLUENCE_PAGE_ID: %s\n\n%s\n" id org))
      (goto-char (point-min)))
    (pop-to-buffer buffer)))

;;;###autoload
(defun hub/confluence-comment-list (&optional page-id body-format)
  "Fetch Confluence comments for PAGE-ID and render a diagnostic buffer.
This command is intentionally read-only: it does not write sidecar files or
modify the source Org buffer.  BODY-FORMAT defaults to storage."
  (interactive)
  (let* ((id (or page-id
		 (hub/confluence-api--page-id-from-buffer)
		 (read-string "Confluence page ID: ")))
	 (format-name (or body-format "storage"))
	 (footer-response (hub/confluence-api--list-page-comments
			   id "footer-comments" format-name))
	 (inline-response (hub/confluence-api--list-page-comments
			   id "inline-comments" format-name))
	 (buffer (generate-new-buffer (format "*Confluence Comments %s*" id))))
    (with-current-buffer buffer
      (special-mode)
      (let ((inhibit-read-only t))
	(erase-buffer)
	(insert (format "Confluence comments for page %s\n\n" id))
	(hub/confluence-commands--insert-comment-section
	 "footer-comments" format-name footer-response)
	(insert "\n")
	(hub/confluence-commands--insert-comment-section
	 "inline-comments" format-name inline-response)
	(goto-char (point-min))))
    (pop-to-buffer buffer)))

(defun hub/confluence-commands--page-id-or-read (page-id)
  "Return PAGE-ID, current buffer page ID, or prompt for one."
  (or page-id
      (hub/confluence-api--page-id-from-buffer)
      (read-string "Confluence page ID: ")))

(defun hub/confluence-commands--maybe-resolve-people-and-refresh (sidecar-file directory)
  "Resolve people for DIRECTORY and refresh SIDECAR-FILE headings when enabled."
  (when hub/confluence-comment-import-resolve-people
    (condition-case error
	(progn
	  (hub/confluence-people-resolve directory)
	  (when (file-exists-p sidecar-file)
	    (hub/org-comment-refresh-sidecar-headings sidecar-file)))
      (error (message "Confluence people resolution skipped: %s" (error-message-string error))))))

(defun hub/confluence-commands--set-remote-present-properties (comment)
  "Set present-state properties for remote COMMENT at current Org heading."
  (org-entry-delete nil "HUB_COMMENT_REMOTE_STATE")
  (org-entry-put nil "HUB_COMMENT_REMOTE_LAST_SEEN_AT"
		 (hub/confluence-commands--sync-timestamp))
  (org-entry-delete nil "HUB_COMMENT_REMOTE_MISSING_AT")
  (if-let* ((status (hub/confluence-commands--remote-comment-resolution-status comment)))
      (org-entry-put nil "HUB_COMMENT_REMOTE_RESOLUTION_STATUS" status)
    (org-entry-delete nil "HUB_COMMENT_REMOTE_RESOLUTION_STATUS")))

(defun hub/confluence-commands--apply-remote-status-at-heading (comment report)
  "Apply COMMENT remote resolution status to current root heading and update REPORT."
  (let ((status (hub/confluence-commands--remote-comment-resolution-status comment))
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
		      (cons (or (org-entry-get nil "HUB_COMMENT_REMOTE_ID") "<unknown>")
			    (plist-get report :todo-resolved))))
	 (org-todo "RESOLVED")))
      ("open"
       (when (equal local "RESOLVED")
	 (plist-put report :remote-reopened
		    (1+ (or (plist-get report :remote-reopened) 0)))
	 (org-todo "OPEN"))))))

(defun hub/confluence-commands--backfill-remote-metadata (sidecar-file comment &optional report)
  "Backfill/update sidecar metadata for existing remote COMMENT.
+REPORT is updated for TODO-impacting status changes when non-nil."
  (let ((remote-id (hub/confluence-commands--remote-comment-id comment)))
    (when (and remote-id (file-exists-p sidecar-file))
      (with-temp-buffer
	(insert-file-contents sidecar-file)
	(org-mode)
	(goto-char (point-min))
	(when (cl-loop while (re-search-forward org-heading-regexp nil t)
		       do (goto-char (match-beginning 0))
		       when (equal remote-id (org-entry-get nil "HUB_COMMENT_REMOTE_ID"))
		       return t
		       do (forward-line 1))
	  (let ((author-name (hub/confluence-commands--remote-comment-author-name comment))
		(author-id (hub/confluence-commands--remote-comment-author-id comment))
		(created-at (hub/confluence-commands--remote-comment-created-at comment)))
	    (hub/confluence-commands--put-property-when-missing "HUB_COMMENT_REMOTE_AUTHOR_ID" author-id)
	    (hub/confluence-commands--put-property-when-missing
	     "HUB_COMMENT_REMOTE_AUTHOR_DISPLAY_NAME" author-name)
	    (hub/confluence-commands--put-property-when-missing
	     "HUB_COMMENT_REMOTE_CREATED_AT" created-at)
	    (hub/confluence-commands--set-remote-present-properties comment)
	    (unless (equal "reply" (org-entry-get nil "HUB_COMMENT_SYNC_KIND"))
	      (hub/confluence-commands--apply-remote-status-at-heading comment report))
	    (write-region (point-min) (point-max) sidecar-file nil 'silent)))))))

(defun hub/confluence-commands--import-remote-replies
    (sidecar-file source-file parent-comment endpoint body-format report)
  "Import remote replies under PARENT-COMMENT from ENDPOINT.
Return plist with `:imported', `:seen-ids', `:parent-id', and `:complete'."
  (let ((parent-id (hub/confluence-commands--remote-comment-id parent-comment))
	(imported 0)
	(seen-ids nil)
	(complete nil))
    (when parent-id
      (let ((replies (condition-case error
			 (prog1 (hub/confluence-commands--comment-results
				 (hub/confluence-api--list-comment-children parent-id endpoint body-format))
			   (setq complete t))
		       (error
			(message "Confluence reply import skipped for %s: %s"
				 parent-id (error-message-string error))
			nil))))
	(dolist (reply replies)
	  (hub/confluence-commands--cache-comment-author reply (file-name-directory source-file))
	  (when-let* ((remote-id (hub/confluence-commands--remote-comment-id reply)))
	    (push remote-id seen-ids)
	    (if (hub/confluence-commands--sidecar-has-remote-comment-p sidecar-file remote-id)
		(hub/confluence-commands--backfill-remote-metadata sidecar-file reply report)
	      (hub/confluence-commands--append-remote-reply-comment
	       sidecar-file source-file reply body-format parent-id)
	      (setq imported (1+ imported)))))))
    (list :imported imported :seen-ids seen-ids :parent-id parent-id :complete complete)))

(defun hub/confluence-commands--endpoint-sync-kind (endpoint)
  "Return sidecar sync kind for Confluence ENDPOINT."
  (pcase endpoint
    ("footer-comments" "footer")
    ("inline-comments" "inline")))

(defun hub/confluence-commands--mark-missing-at-heading ()
  "Mark current remote-linked Org heading as remote missing."
  (org-entry-put nil "HUB_COMMENT_REMOTE_STATE" "missing")
  (unless (org-entry-get nil "HUB_COMMENT_REMOTE_MISSING_AT")
    (org-entry-put nil "HUB_COMMENT_REMOTE_MISSING_AT"
		   (hub/confluence-commands--sync-timestamp))))

(defun hub/confluence-commands--reconcile-missing-roots
    (sidecar-file sync-kind seen-ids report)
  "Mark remote-linked root comments of SYNC-KIND absent from SEEN-IDS missing."
  (when (file-exists-p sidecar-file)
    (with-temp-buffer
      (insert-file-contents sidecar-file)
      (org-mode)
      (let ((changed nil))
	(goto-char (point-min))
	(while (re-search-forward org-heading-regexp nil t)
	  (goto-char (match-beginning 0))
	  (let ((remote-id (org-entry-get nil "HUB_COMMENT_REMOTE_ID"))
		(kind (org-entry-get nil "HUB_COMMENT_SYNC_KIND")))
	    (when (and remote-id
		       (equal kind sync-kind)
		       (not (member remote-id seen-ids)))
	      (when (equal (org-get-todo-state) "TODO")
		(plist-put report :todo-missing
			   (cons remote-id (plist-get report :todo-missing))))
	      (unless (equal (org-entry-get nil "HUB_COMMENT_REMOTE_STATE") "missing")
		(plist-put report :marked-missing (1+ (or (plist-get report :marked-missing) 0))))
	      (hub/confluence-commands--mark-missing-at-heading)
	      (setq changed t)))
	  (forward-line 1))
	(when changed
	  (write-region (point-min) (point-max) sidecar-file nil 'silent))))))

(defun hub/confluence-commands--reconcile-missing-replies
    (sidecar-file parent-id seen-ids report)
  "Mark remote-linked replies under PARENT-ID absent from SEEN-IDS missing."
  (when (and parent-id (file-exists-p sidecar-file))
    (with-temp-buffer
      (insert-file-contents sidecar-file)
      (org-mode)
      (let ((changed nil))
	(goto-char (point-min))
	(while (re-search-forward org-heading-regexp nil t)
	  (goto-char (match-beginning 0))
	  (let ((remote-id (org-entry-get nil "HUB_COMMENT_REMOTE_ID"))
		(parent (org-entry-get nil "HUB_COMMENT_REMOTE_PARENT_ID"))
		(kind (org-entry-get nil "HUB_COMMENT_SYNC_KIND")))
	    (when (and remote-id
		       (equal kind "reply")
		       (equal parent parent-id)
		       (not (member remote-id seen-ids)))
	      (unless (equal (org-entry-get nil "HUB_COMMENT_REMOTE_STATE") "missing")
		(plist-put report :marked-missing (1+ (or (plist-get report :marked-missing) 0))))
	      (hub/confluence-commands--mark-missing-at-heading)
	      (setq changed t)))
	  (forward-line 1))
	(when changed
	  (write-region (point-min) (point-max) sidecar-file nil 'silent))))))

(defun hub/confluence-commands--record-present-again (sidecar-file remote-id report)
  "Record REMOTE-ID present-again in REPORT when SIDECAR-FILE says missing."
  (when (and remote-id (file-exists-p sidecar-file))
    (with-temp-buffer
      (insert-file-contents sidecar-file)
      (org-mode)
      (goto-char (point-min))
      (when (cl-loop while (re-search-forward org-heading-regexp nil t)
		     do (goto-char (match-beginning 0))
		     when (equal remote-id (org-entry-get nil "HUB_COMMENT_REMOTE_ID"))
		     return (equal (org-entry-get nil "HUB_COMMENT_REMOTE_STATE") "missing")
		     do (forward-line 1))
	(plist-put report :present-again (1+ (or (plist-get report :present-again) 0)))))))

(defun hub/confluence-commands--import-report-message (report fallback)
  "Show concise import REPORT with FALLBACK message when it is empty."
  (let ((lines (delq nil
		     (list
		      (when-let* ((count (plist-get report :imported)))
			(when (> count 0) (format "Imported new: %s" count)))
		      (when-let* ((count (plist-get report :marked-missing)))
			(when (> count 0) (format "Remote missing: %s" count)))
		      (when-let* ((count (plist-get report :present-again)))
			(when (> count 0) (format "Remote present again: %s" count)))
		      (when-let* ((count (plist-get report :remote-resolved)))
			(when (> count 0) (format "Remote resolved locally: %s" count)))
		      (when-let* ((count (plist-get report :remote-reopened)))
			(when (> count 0) (format "Remote reopened locally: %s" count)))
		      (when-let* ((items (plist-get report :todo-resolved)))
			(when items (format "Local TODO closed by remote resolution: %s" (length items))))
		      (when-let* ((items (plist-get report :todo-missing)))
			(when items (format "Local TODO now remote-missing: %s" (length items))))))))
    (message "%s" (if lines (string-join lines "; ") fallback))
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
	  (special-mode))))
    report))

(defun hub/confluence-commands--import-remote-comments
    (page-id endpoint body-format append-function &optional resolve-after report)
  "Import PAGE-ID comments from ENDPOINT using APPEND-FUNCTION.
BODY-FORMAT defaults to storage.  Return import report plist."
  (unless buffer-file-name
    (user-error "Current buffer is not visiting a file"))
  (let* ((source-file buffer-file-name)
	 (source-directory (file-name-directory source-file))
	 (sidecar-file (hub/org-comment-sidecar-path source-file))
	 (id (hub/confluence-commands--page-id-or-read page-id))
	 (format-name (or body-format "storage"))
	 (response (hub/confluence-api--list-page-comments id endpoint format-name))
	 (comments (hub/confluence-commands--comment-results response))
	 (sync-kind (hub/confluence-commands--endpoint-sync-kind endpoint))
	 (report (or report (list :imported 0)))
	 seen-ids)
    (dolist (comment comments)
      (hub/confluence-commands--cache-comment-author comment source-directory)
      (when-let* ((remote-id (hub/confluence-commands--remote-comment-id comment)))
	(push remote-id seen-ids)
	(hub/confluence-commands--record-present-again sidecar-file remote-id report)
	(if (hub/confluence-commands--sidecar-has-remote-comment-p sidecar-file remote-id)
	    (hub/confluence-commands--backfill-remote-metadata sidecar-file comment report)
	  (funcall append-function sidecar-file source-file comment format-name)
	  (plist-put report :imported (1+ (or (plist-get report :imported) 0))))
	(let ((reply-summary (hub/confluence-commands--import-remote-replies
			      sidecar-file source-file comment endpoint format-name report)))
	  (plist-put report :imported (+ (or (plist-get report :imported) 0)
					 (plist-get reply-summary :imported)))
	  (when (plist-get reply-summary :complete)
	    (hub/confluence-commands--reconcile-missing-replies
	     sidecar-file remote-id (plist-get reply-summary :seen-ids) report)))))
    (hub/confluence-commands--reconcile-missing-roots sidecar-file sync-kind seen-ids report)
    (when (file-exists-p sidecar-file)
      (hub/org-comment--anchor-imported-inline-comments (current-buffer) sidecar-file)
      (hub/org-comment-refresh-sidecar-headings sidecar-file))
    (when resolve-after
      (hub/confluence-commands--maybe-resolve-people-and-refresh sidecar-file source-directory))
    report))

;;;###autoload
(defun hub/confluence-comment-import-footer (&optional page-id body-format)
  "Import remote footer comments for PAGE-ID into the current Org sidecar.
This command is append-only and idempotent: existing remote Confluence comments
are detected by remote ID and are not overwritten.  BODY-FORMAT defaults to
storage, and the raw remote body is preserved verbatim."
  (interactive)
  (let ((report (hub/confluence-commands--import-remote-comments
		 page-id "footer-comments" body-format
		 #'hub/confluence-commands--append-remote-footer-comment t)))
    (hub/confluence-commands--import-report-message
     report "Imported 0 Confluence footer comments")
    (or (plist-get report :imported) 0)))

;;;###autoload
(defun hub/confluence-comment-import-inline (&optional page-id body-format)
  "Import remote inline comments for PAGE-ID into the current Org sidecar.
Imported inline comments are left unanchored until manually reanchored."
  (interactive)
  (let ((report (hub/confluence-commands--import-remote-comments
		 page-id "inline-comments" body-format
		 #'hub/confluence-commands--append-remote-inline-comment t)))
    (hub/confluence-commands--import-report-message
     report "Imported 0 Confluence inline comments")
    (or (plist-get report :imported) 0)))

;;;###autoload
(defun hub/confluence-comment-import (&optional page-id body-format)
  "Import remote footer and inline comments for PAGE-ID into the sidecar."
  (interactive)
  (let* ((id (hub/confluence-commands--page-id-or-read page-id))
	 (footer-count (hub/confluence-comment-import-footer id body-format))
	 (inline-count (hub/confluence-comment-import-inline id body-format)))
    (message "Imported %s footer and %s inline Confluence comments"
	     footer-count inline-count)
    (+ footer-count inline-count)))

(defun hub/confluence-commands--source-directory ()
  "Return current buffer file directory for people lookup, or nil."
  (when buffer-file-name
    (file-name-directory buffer-file-name)))

(defun hub/confluence-commands--user-account-id (user)
  "Return Confluence account ID from USER alist."
  (alist-get 'accountId user))

(defun hub/confluence-commands--user-display-name (user)
  "Return display name from Confluence USER alist."
  (or (alist-get 'displayName user)
      (alist-get 'publicName user)))

(defun hub/confluence-commands--resolve-people-in-files (files users)
  "Update people FILES from Confluence USERS and return resolved count."
  (let ((resolved 0))
    (dolist (user users resolved)
      (when-let* ((account-id (hub/confluence-commands--user-account-id user)))
	(dolist (file files)
	  (when (hub/confluence-people-update-identity
		 file account-id
		 (hub/confluence-commands--user-display-name user)
		 (alist-get 'email user)
		 (alist-get 'accountStatus user)
		 (alist-get 'timeZone user))
	    (setq resolved (1+ resolved))))))))

;;;###autoload
(defun hub/confluence-people-resolve (&optional directory)
  "Resolve unresolved Confluence people in local/global people files.
DIRECTORY controls the local people file context.  Interactively, use the
current Org file's directory when available and the global people file otherwise."
  (interactive)
  (let* ((context-directory (or directory (hub/confluence-commands--source-directory)))
	 (files (hub/confluence-people-context-files context-directory))
	 (account-ids (hub/confluence-people-unresolved-account-ids files)))
    (if (not account-ids)
	(progn
	  (message "No unresolved Confluence people")
	  0)
      (let* ((users (hub/confluence-commands--user-results
		     (hub/confluence-api--users-bulk account-ids)))
	     (resolved (hub/confluence-commands--resolve-people-in-files files users))
	     (unresolved (- (length account-ids)
			    (length (delete-dups (mapcar #'hub/confluence-commands--user-account-id users))))))
	(message "Resolved %s Confluence people (%s unresolved)" resolved unresolved)
	resolved))))

;;;###autoload
(defun hub/confluence-publish-dwim
    (&optional title parent-id async subtreep visible-only body-only ext-plist)
  "Publish current Org buffer to Confluence, updating or creating as needed.

When #+CONFLUENCE_PAGE_ID is present, update that page.  Otherwise use
#+CONFLUENCE_SPACE or `hub/confluence-api-default-space' and create a new page
using TITLE, prompting interactively when needed.  Optional PARENT-ID is passed
to cfl as the parent page.  When the new page ID can be parsed from cfl output,
record Confluence metadata, upload image assets, and re-save the page content.
ASYNC, SUBTREEP, VISIBLE-ONLY, BODY-ONLY, and EXT-PLIST follow Org export
conventions."
  (interactive)
  (let ((page-id (hub/confluence-api--page-id-from-buffer subtreep)))
    (if page-id
	(hub/confluence-publish async subtreep visible-only body-only ext-plist)
      (let ((assets (org-confluence-image-assets subtreep)))
	(let* ((space (hub/confluence-api--space-from-buffer))
	       (page-title (or title
			       (hub/confluence-commands--title-from-buffer)
			       (read-string "Confluence page title: ")))
	       (xhtml-file nil))
	  (unwind-protect
	      (let ((command nil)
		    (output nil)
		    (created-page-id nil))
		(setq xhtml-file
		      (hub/confluence-commands--write-temp-xhtml
		       (org-confluence-export async subtreep visible-only body-only
					      (append (list :confluence-image-filenames
							    (hub/confluence-commands--asset-filename-map assets))
						      ext-plist))))
		(setq command (hub/confluence-api--page-create-command space page-title xhtml-file parent-id))
		(setq output (hub/confluence-commands--run-output command))
		(setq created-page-id (hub/confluence-commands--created-page-id output))
		(when created-page-id
		  (hub/confluence-publish--record-created-page created-page-id space)
		  (hub/confluence-commands--upload-assets created-page-id assets)
		  (hub/confluence-commands--run
		   (hub/confluence-api--page-update-command created-page-id xhtml-file)))
		(message "Created Confluence page %s in space %s%s"
			 page-title space
			 (if created-page-id (format " (ID %s)" created-page-id) ""))
		created-page-id)
	    (when (and xhtml-file (file-exists-p xhtml-file))
	      (delete-file xhtml-file))))))))

(provide 'org-confluence-commands)
(provide 'org/export-confluence-commands)
;;; org-confluence-commands.el ends here
