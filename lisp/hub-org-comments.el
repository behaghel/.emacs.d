;;; hub-org-comments.el --- Org sidecar comment storage -*- lexical-binding: t; -*-

;;; Commentary:
;; Plain Org sidecar storage for region-targeted comments.  Source Org buffers
;; remain clean; comments are stored next to them as `article.comments.org'.

;;; Code:

(require 'cl-lib)
(require 'dom)
(require 'hub-confluence-people nil 'noerror)
(require 'org)
(require 'ol)
(require 'seq)
(require 'subr-x)

(defgroup hub/org-comments nil
  "Region-targeted Org sidecar comments."
  :group 'org)

(defcustom hub/org-comment-heading-preview-length 60
  "Maximum normalized target text length used in sidecar headings."
  :type 'natnum
  :group 'hub/org-comments)

(defcustom hub/org-comment-heading-target-preview-length 48
  "Maximum normalized target text length used in comment headings."
  :type 'natnum
  :group 'hub/org-comments)

(defcustom hub/org-comment-heading-body-preview-length 60
  "Maximum normalized body text length used in comment headings."
  :type 'natnum
  :group 'hub/org-comments)

(defcustom hub/org-comment-author nil
  "Author name used for newly created local comments.
When nil, comments fall back to Org metadata and then Emacs user identity."
  :type '(choice (const :tag "Infer author" nil) string)
  :group 'hub/org-comments)

(defun hub/org-comment-sidecar-path (&optional source-file)
  "Return sidecar comments path for SOURCE-FILE or current buffer file.
For example, article.org maps to article.comments.org."
  (let ((file (or source-file buffer-file-name)))
    (unless file
      (user-error "Current buffer is not visiting a file"))
    (concat (file-name-sans-extension file) ".comments.org")))

(defun hub/org-comment-normalize-target-text (text)
  "Return TEXT trimmed with whitespace runs collapsed to one space."
  (string-trim (replace-regexp-in-string "[[:space:]]+" " " text)))

(defun hub/org-comment-target-hash (target-text)
  "Return sha256 hash string for normalized TARGET-TEXT."
  (concat "sha256:" (secure-hash 'sha256 (hub/org-comment-normalize-target-text target-text))))

(defun hub/org-comment--valid-anchor-p (source-buffer properties)
  "Return non-nil when PROPERTIES still identify SOURCE-BUFFER text."
  (let* ((target-text (alist-get "HUB_COMMENT_TARGET_TEXT" properties nil nil #'equal))
	 (target-bounds (hub/org-comment--target-bounds properties))
	 (start (car target-bounds))
	 (end (cdr target-bounds)))
    (and target-text start end
	 (with-current-buffer source-buffer
	   (and (<= (point-min) start)
		(<= start end)
		(<= end (point-max))
		(equal (hub/org-comment-normalize-target-text
			(buffer-substring-no-properties start end))
		       (hub/org-comment-normalize-target-text target-text)))))))

(defun hub/org-comment--random-hex ()
  "Return a short random hexadecimal suffix for local comment IDs."
  (format "%06x" (random #x1000000)))

(defun hub/org-comment-generate-id ()
  "Return a stable local comment ID."
  (format "local-%s-%s" (format-time-string "%Y%m%dT%H%M%S") (hub/org-comment--random-hex)))

(defun hub/org-comment--line-column-at (position)
  "Return cons cell of one-based line and zero-based column at POSITION."
  (save-excursion
    (goto-char position)
    (cons (line-number-at-pos position t) (current-column))))

(defun hub/org-comment--present-string (value)
  "Return trimmed VALUE when it is a non-empty string, or nil."
  (when (stringp value)
    (let ((trimmed (string-trim value)))
      (unless (string-empty-p trimmed)
	trimmed))))

(defun hub/org-comment--keyword-from-buffer (keyword)
  "Return Org KEYWORD value from the current buffer, or nil."
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search t)
	  (regexp (format "^[	]*#\\+%s:[	]*\\(.*?\\)[	]*$"
			  (regexp-quote keyword))))
      (when (re-search-forward regexp nil t)
	(hub/org-comment--present-string (match-string-no-properties 1))))))

(defun hub/org-comment-current-author ()
  "Return the best available author name for a new local comment."
  (or (hub/org-comment--present-string hub/org-comment-author)
      (hub/org-comment--keyword-from-buffer "AUTHOR")
      (hub/org-comment--keyword-from-buffer "EMAIL")
      (hub/org-comment--present-string user-full-name)
      (hub/org-comment--present-string user-mail-address)
      (hub/org-comment--present-string user-login-name)
      "unknown"))

(defun hub/org-comment-current-created-at ()
  "Return an ISO-like timestamp for a new local comment."
  (format-time-string "%Y-%m-%dT%H:%M:%S%z"))

(defun hub/org-comment--truncate (text length)
  "Return TEXT truncated to LENGTH with ellipsis when needed."
  (if (> (length text) length)
      (concat (substring text 0 length) "…")
    text))

(defun hub/org-comment--heading-preview (target-text)
  "Return sidecar heading preview for normalized TARGET-TEXT."
  (hub/org-comment--truncate
   (hub/org-comment-normalize-target-text target-text)
   hub/org-comment-heading-preview-length))

(defun hub/org-comment--html-fragment-text (text)
  "Return TEXT as decoded plain text using the HTML parser when possible."
  (condition-case nil
      (with-temp-buffer
	(insert (or text ""))
	(let* ((document (libxml-parse-html-region (point-min) (point-max)))
	       (body (car (dom-by-tag document 'body))))
	  (or (and body (dom-texts body)) text)))
    (error text)))

(defun hub/org-comment--preview-plain-text (text)
  "Return TEXT stripped of markup and decoded for heading previews."
  (if (string-match-p "\\(&[#[:alnum:]]+;\\|<[[:alpha:]!/][^>]*>\\)" (or text ""))
      (hub/org-comment--html-fragment-text text)
    (or text "")))

(defun hub/org-comment--preview-text (text length)
  "Return TEXT normalized, decoded for heading use, and truncated to LENGTH."
  (let ((preview (hub/org-comment-normalize-target-text
		  (hub/org-comment--preview-plain-text (or text "")))))
    (unless (string-empty-p preview)
      (hub/org-comment--truncate preview length))))

(defun hub/org-comment--heading-author (record &optional directory)
  "Return display author for comment RECORD using optional DIRECTORY people lookup."
  (let ((author (plist-get record :author))
	(remote-author-display-name (plist-get record :remote-author-display-name))
	(remote-author-id (plist-get record :remote-author-id)))
    (or (when (and (fboundp 'hub/confluence-people-resolve-account-id)
		   remote-author-id)
	  (hub/confluence-people-resolve-account-id remote-author-id directory))
	remote-author-display-name
	author
	remote-author-id)))

(defun hub/org-comment-heading-title (record &optional directory)
  "Return human-readable sidecar heading title for comment RECORD.
DIRECTORY is used for resolving Confluence account IDs through people files."
  (let* ((page-comment (equal (plist-get record :sync-kind) "footer"))
	 (reply-count (plist-get record :reply-count))
	 (author (hub/org-comment--heading-author record directory))
	 (target (hub/org-comment--preview-text
		  (plist-get record :target-text)
		  hub/org-comment-heading-target-preview-length))
	 (body (hub/org-comment--preview-text
		(plist-get record :body)
		hub/org-comment-heading-body-preview-length))
	 (prefix-parts (delq nil (list (when page-comment "Page") author)))
	 (prefix (string-join prefix-parts " · "))
	 (subject (if target (format "“%s”" target) body))
	 (main (string-join (delq nil (list (unless (string-empty-p prefix) prefix)
					    subject))
			    " · ")))
    (setq main (if (and target body)
		   (format "%s — %s" main body)
		 main))
    (if (and reply-count (> reply-count 0))
	(format "[%s %s] %s" reply-count (if (= reply-count 1) "reply" "replies") main)
      main)))

(defun hub/org-comment-reply-heading-title (record &optional directory)
  "Return human-readable sidecar reply heading title for comment RECORD."
  (let* ((author (hub/org-comment--heading-author record directory))
	 (created-at (hub/org-comment--preview-text (plist-get record :created-at) 16))
	 (body (hub/org-comment--preview-text
		(plist-get record :body)
		hub/org-comment-heading-body-preview-length)))
    (string-join (delq nil (list "Reply" author created-at (when body (concat "— " body))))
		 " · ")))

(defun hub/org-comment-create-record (source-file start end body &optional id author created-at)
  "Return a comment plist for SOURCE-FILE region START to END with BODY.
ID defaults to a new local ID.  AUTHOR and CREATED-AT default to local metadata."
  (let* ((target-raw (buffer-substring-no-properties start end))
	 (target-text (hub/org-comment-normalize-target-text target-raw))
	 (start-line-column (hub/org-comment--line-column-at start))
	 (end-line-column (hub/org-comment--line-column-at end)))
    (list :id (or id (hub/org-comment-generate-id))
	  :status "OPEN"
	  :source-file source-file
	  :author (or author (hub/org-comment-current-author))
	  :created-at (or created-at (hub/org-comment-current-created-at))
	  :target-text target-text
	  :target-hash (hub/org-comment-target-hash target-text)
	  :target-start start
	  :target-end end
	  :target-start-line (car start-line-column)
	  :target-start-column (cdr start-line-column)
	  :target-end-line (car end-line-column)
	  :target-end-column (cdr end-line-column)
	  :sync-kind "inline"
	  :body body)))

(defun hub/org-comment--relative-source-file (source-file sidecar-file)
  "Return SOURCE-FILE relative to SIDECAR-FILE directory."
  (file-relative-name source-file (file-name-directory sidecar-file)))

(defun hub/org-comment-source-file-from-sidecar (&optional sidecar-file)
  "Return source Org file referenced by SIDECAR-FILE or current sidecar."
  (let* ((file (or sidecar-file buffer-file-name))
	 (directory (and file (file-name-directory file))))
    (unless file
      (user-error "No comments sidecar file"))
    (with-temp-buffer
      (insert-file-contents file)
      (let ((source (hub/org-comment--keyword-from-buffer "SOURCE")))
	(unless source
	  (user-error "Comments sidecar has no #+source keyword"))
	(expand-file-name source directory)))))

(defun hub/org-comment--ensure-sidecar-header (sidecar-file source-file)
  "Create SIDECAR-FILE with a minimal header for SOURCE-FILE when absent."
  (unless (file-exists-p sidecar-file)
    (make-directory (file-name-directory sidecar-file) t)
    (with-temp-file sidecar-file
      (insert (format "#+title: Comments for %s\n" (file-name-nondirectory source-file)))
      (insert (format "#+source: %s\n"
		      (hub/org-comment--relative-source-file source-file sidecar-file)))
      (insert "#+todo: OPEN TODO | RESOLVED\n\n"))))

(defun hub/org-comment--property-line (key value)
  "Return an Org property line for KEY and VALUE."
  (format ":%s: %s\n" key value))

(defun hub/org-comment-format-entry (record sidecar-file)
  "Return Org text for comment RECORD."
  (let ((title (or (plist-get record :title)
		   (hub/org-comment-heading-title
		    record (file-name-directory sidecar-file)))))
    (concat
     (format "* %s %s\n" (or (plist-get record :status) "OPEN") title)
     ":PROPERTIES:\n"
     (hub/org-comment--property-line "HUB_COMMENT_ID" (plist-get record :id))
     (when-let* ((author (plist-get record :author)))
       (hub/org-comment--property-line "HUB_COMMENT_AUTHOR" author))
     (when-let* ((created-at (plist-get record :created-at)))
       (hub/org-comment--property-line "HUB_COMMENT_CREATED_AT" created-at))
     (when-let* ((sync-kind (plist-get record :sync-kind)))
       (hub/org-comment--property-line "HUB_COMMENT_SYNC_KIND" sync-kind))
     (hub/org-comment--property-line
      "HUB_COMMENT_TARGET"
      (format "%s %s" (plist-get record :target-start) (plist-get record :target-end)))
     (hub/org-comment--property-line
      "HUB_COMMENT_TARGET_LINES"
      (format "%s:%s %s:%s"
	      (plist-get record :target-start-line)
	      (plist-get record :target-start-column)
	      (plist-get record :target-end-line)
	      (plist-get record :target-end-column)))
     (hub/org-comment--property-line "HUB_COMMENT_TARGET_TEXT" (plist-get record :target-text))
     ":END:\n\n"
     (string-trim-right (or (plist-get record :body) ""))
     "\n")))

(defun hub/org-comment-fold-sidecar-property-drawers (&optional buffer)
  "Fold property drawers in comments sidecar BUFFER or current buffer."
  (with-current-buffer (or buffer (current-buffer))
    (when (and buffer-file-name
	       (string-suffix-p ".comments.org" buffer-file-name))
      (org-mode)
      (save-excursion
	(save-restriction
	  (widen)
	  (goto-char (point-min))
	  (while (re-search-forward "^[[:space:]]*:PROPERTIES:[[:space:]]*$" nil t)
	    (when (fboundp 'org-fold-hide-drawer-toggle)
	      (org-fold-hide-drawer-toggle t))))))))

(defun hub/org-comment-refold-sidecar-file (sidecar-file)
  "Refold property drawers in any live buffer visiting SIDECAR-FILE."
  (when-let* ((buffer (find-buffer-visiting sidecar-file)))
    (with-current-buffer buffer
      (let ((point (point))
	    (window-starts (mapcar (lambda (window)
				     (cons window (window-start window)))
				   (get-buffer-window-list buffer nil t))))
	(revert-buffer :ignore-auto :noconfirm)
	(hub/org-comment-fold-sidecar-property-drawers buffer)
	(goto-char (min point (point-max)))
	(dolist (entry window-starts)
	  (when (window-live-p (car entry))
	    (set-window-start (car entry) (cdr entry) t)))))))

(defun hub/org-comment-append-to-sidecar (record &optional sidecar-file)
  "Append comment RECORD to SIDECAR-FILE and return SIDECAR-FILE.
SIDECAR-FILE defaults to the sidecar path for RECORD's source file."
  (let* ((source-file (plist-get record :source-file))
	 (target-file (or sidecar-file (hub/org-comment-sidecar-path source-file))))
    (hub/org-comment--ensure-sidecar-header target-file source-file)
    (with-temp-buffer
      (insert-file-contents target-file)
      (goto-char (point-max))
      (unless (bolp) (insert "\n"))
      (unless (save-excursion
		(forward-line -1)
		(looking-at-p "[[:space:]]*$"))
	(insert "\n"))
      (insert (hub/org-comment-format-entry record target-file))
      (write-region (point-min) (point-max) target-file nil 'silent))
    target-file))

(defun hub/org-comment-update-anchor (sidecar-file comment-id record)
  "Update COMMENT-ID anchor metadata in SIDECAR-FILE from RECORD."
  (with-temp-buffer
    (insert-file-contents sidecar-file)
    (org-mode)
    (goto-char (point-min))
    (unless (cl-loop while (re-search-forward org-heading-regexp nil t)
		     do (goto-char (match-beginning 0))
		     when (equal comment-id (org-entry-get nil "HUB_COMMENT_ID"))
		     return (progn
			      (org-entry-put nil "HUB_COMMENT_TARGET"
					     (format "%s %s"
						     (plist-get record :target-start)
						     (plist-get record :target-end)))
			      (org-entry-put nil "HUB_COMMENT_TARGET_LINES"
					     (format "%s:%s %s:%s"
						     (plist-get record :target-start-line)
						     (plist-get record :target-start-column)
						     (plist-get record :target-end-line)
						     (plist-get record :target-end-column)))
			      (org-entry-put nil "HUB_COMMENT_TARGET_TEXT"
					     (plist-get record :target-text))
			      (org-entry-delete nil "HUB_COMMENT_TARGET_HASH")
			      t)
		     do (forward-line 1))
      (user-error "Comment %s not found in sidecar" comment-id))
    (write-region (point-min) (point-max) sidecar-file nil 'silent)))

(defun hub/org-comment-delete-entry (sidecar-file comment-id)
  "Delete COMMENT-ID subtree from SIDECAR-FILE."
  (with-temp-buffer
    (insert-file-contents sidecar-file)
    (org-mode)
    (goto-char (point-min))
    (unless (cl-loop while (re-search-forward org-heading-regexp nil t)
		     do (goto-char (match-beginning 0))
		     when (equal comment-id (org-entry-get nil "HUB_COMMENT_ID"))
		     return (let ((start (point))
				  (end (save-excursion (org-end-of-subtree t t))))
			      (delete-region start end)
			      t)
		     do (forward-line 1))
      (user-error "Comment %s not found in sidecar" comment-id))
    (write-region (point-min) (point-max) sidecar-file nil 'silent)))

(defun hub/org-comment--reply-heading-p ()
  "Return non-nil when point is at a reply heading."
  (equal "reply" (org-entry-get nil "HUB_COMMENT_SYNC_KIND")))

(defun hub/org-comment--reply-count-at-heading ()
  "Return direct reply child count for heading at point."
  (let ((level (org-outline-level))
	(end (save-excursion (org-end-of-subtree t t)))
	(count 0))
    (save-excursion
      (forward-line 1)
      (while (re-search-forward org-heading-regexp end t)
	(goto-char (match-beginning 0))
	(cond
	 ((<= (org-outline-level) level)
	  (goto-char end))
	 ((and (= (org-outline-level) (1+ level))
	       (hub/org-comment--reply-heading-p))
	  (setq count (1+ count))))
	(forward-line 1)))
    count))

(defun hub/org-comment--title-record-from-heading (sidecar-file)
  "Return a plist record for refreshing the current heading in SIDECAR-FILE."
  (let* ((properties (hub/org-comment--parse-properties-at-heading))
	 (entry-end (save-excursion (org-end-of-subtree t t)))
	 (body (hub/org-comment--entry-body entry-end))
	 (record (append (hub/org-comment--base-record
			  properties sidecar-file
			  (alist-get "HUB_COMMENT_TARGET_TEXT" properties nil nil #'equal)
			  nil nil body)
			 (list :body-format (alist-get "HUB_COMMENT_BODY_FORMAT" properties nil nil #'equal)))))
    (if (hub/org-comment--reply-heading-p)
	record
      (append record (list :reply-count (hub/org-comment--reply-count-at-heading))))))

(defconst hub/org-comment-obsolete-metadata-properties
  '("HUB_COMMENT_TARGET_HASH"
    "HUB_COMMENT_PARENT_ID"
    "HUB_COMMENT_REMOTE_TARGET_JSON")
  "Sidecar properties removed by metadata compaction.")

(defun hub/org-comment--compact-heading-metadata ()
  "Compact metadata properties at the current sidecar heading."
  (let ((changed 0)
	(remote-linked (org-entry-get nil "HUB_COMMENT_REMOTE_ID")))
    (dolist (property hub/org-comment-obsolete-metadata-properties)
      (when (org-entry-get nil property)
	(org-entry-delete nil property)
	(setq changed (1+ changed))))
    (when (equal "storage" (org-entry-get nil "HUB_COMMENT_BODY_FORMAT"))
      (org-entry-delete nil "HUB_COMMENT_BODY_FORMAT")
      (setq changed (1+ changed)))
    (when (equal "present" (org-entry-get nil "HUB_COMMENT_REMOTE_STATE"))
      (org-entry-delete nil "HUB_COMMENT_REMOTE_STATE")
      (setq changed (1+ changed)))
    (when remote-linked
      (dolist (property '("HUB_COMMENT_AUTHOR" "HUB_COMMENT_CREATED_AT"))
	(when (org-entry-get nil property)
	  (org-entry-delete nil property)
	  (setq changed (1+ changed)))))
    changed))

;;;###autoload
(defun hub/org-comment-compact-sidecar-metadata (&optional sidecar-file)
  "Remove obsolete or derivable comment metadata from SIDECAR-FILE.
When called interactively, use the current comments sidecar or the current Org
file's sidecar.  Return the number of removed properties."
  (interactive)
  (let ((sidecar-file (or sidecar-file
			  (if (and buffer-file-name
				   (string-suffix-p ".comments.org" buffer-file-name))
			      buffer-file-name
			    (hub/org-comment-sidecar-path)))))
    (unless (file-exists-p sidecar-file)
      (user-error "No sidecar comments file: %s" sidecar-file))
    (let ((removed 0))
      (with-temp-buffer
	(insert-file-contents sidecar-file)
	(org-mode)
	(goto-char (point-min))
	(while (re-search-forward org-heading-regexp nil t)
	  (goto-char (match-beginning 0))
	  (when (org-entry-get nil "HUB_COMMENT_ID")
	    (setq removed (+ removed (hub/org-comment--compact-heading-metadata))))
	  (forward-line 1))
	(write-region (point-min) (point-max) sidecar-file nil 'silent))
      (hub/org-comment-refold-sidecar-file sidecar-file)
      (when (called-interactively-p 'interactive)
	(message "Removed %s compactable sidecar propert%s"
		 removed (if (= removed 1) "y" "ies")))
      removed)))

(defun hub/org-comment-refresh-sidecar-headings (&optional sidecar-file)
  "Refresh all comment headings in SIDECAR-FILE from metadata and body previews.
When called interactively, use the current sidecar file or the sidecar for the
current Org source file."
  (interactive)
  (let ((sidecar-file (or sidecar-file
			  (if (and buffer-file-name
				   (string-suffix-p ".comments.org" buffer-file-name))
			      buffer-file-name
			    (hub/org-comment-sidecar-path)))))
    (unless (file-exists-p sidecar-file)
      (user-error "No sidecar comments file: %s" sidecar-file))
    (with-temp-buffer
      (insert-file-contents sidecar-file)
      (org-mode)
      (let ((directory (file-name-directory sidecar-file))
	    (count 0))
	(goto-char (point-min))
	(while (re-search-forward org-heading-regexp nil t)
	  (goto-char (match-beginning 0))
	  (when (org-entry-get nil "HUB_COMMENT_ID")
	    (let ((record (hub/org-comment--title-record-from-heading sidecar-file)))
	      (org-edit-headline
	       (if (hub/org-comment--reply-heading-p)
		   (hub/org-comment-reply-heading-title record directory)
		 (hub/org-comment-heading-title record directory))))
	    (setq count (1+ count)))
	  (forward-line 1))
	(write-region (point-min) (point-max) sidecar-file nil 'silent)
	(hub/org-comment-refold-sidecar-file sidecar-file)
	(when (called-interactively-p 'interactive)
	  (message "Refreshed %s sidecar comment heading%s"
		   count (if (= count 1) "" "s")))
	count))))

(defun hub/org-comment--parse-properties-at-heading ()
  "Return an alist of Org properties at point without inherited values."
  (org-entry-properties nil nil))

(defun hub/org-comment--entry-body (end)
  "Return current heading body text ending before child headings and END."
  (save-excursion
    (let* ((level (org-outline-level))
	   (body-start (progn
			 (forward-line 1)
			 (when (looking-at-p "[[:space:]]*:PROPERTIES:[[:space:]]*$")
			   (when (re-search-forward "^[[:space:]]*:END:[[:space:]]*$" end t)
			     (forward-line 1)))
			 (point)))
	   (body-end (save-excursion
		       (if (re-search-forward org-heading-regexp end t)
			   (let ((heading-start (match-beginning 0)))
			     (goto-char heading-start)
			     (if (> (org-outline-level) level) heading-start end))
			 end))))
      (string-trim (buffer-substring-no-properties body-start body-end)))))

(defun hub/org-comment--number-property (properties key)
  "Return numeric property KEY from PROPERTIES, or nil."
  (when-let* ((value (alist-get key properties nil nil #'equal)))
    (string-to-number value)))

(defun hub/org-comment--target-bounds (properties)
  "Return target bounds cons from compact PROPERTIES."
  (when-let* ((value (alist-get "HUB_COMMENT_TARGET" properties nil nil #'equal))
	      (parts (split-string value "[[:space:]]+" t)))
    (when (= 2 (length parts))
      (cons (string-to-number (car parts))
	    (string-to-number (cadr parts))))))

(defun hub/org-comment--heading-status (properties)
  "Return comment workflow status from heading PROPERTIES."
  (or (alist-get "TODO" properties nil nil #'equal) "OPEN"))

(defun hub/org-comment--base-record (properties sidecar-file target-text start end body)
  "Return common plist record from PROPERTIES and comment fields."
  (list :type 'comment
	:id (alist-get "HUB_COMMENT_ID" properties nil nil #'equal)
	:kind 'comment
	:status (hub/org-comment--heading-status properties)
	:sidecar-file sidecar-file
	:author (alist-get "HUB_COMMENT_AUTHOR" properties nil nil #'equal)
	:created-at (or (alist-get "HUB_COMMENT_REMOTE_CREATED_AT" properties nil nil #'equal)
			(alist-get "HUB_COMMENT_CREATED_AT" properties nil nil #'equal))
	:local-updated-at (alist-get "HUB_COMMENT_LOCAL_UPDATED_AT" properties nil nil #'equal)
	:remote-author-id (alist-get "HUB_COMMENT_REMOTE_AUTHOR_ID" properties nil nil #'equal)
	:remote-author-display-name (alist-get "HUB_COMMENT_REMOTE_AUTHOR_DISPLAY_NAME" properties nil nil #'equal)
	:source (alist-get "HUB_COMMENT_SOURCE" properties nil nil #'equal)
	:remote-id (alist-get "HUB_COMMENT_REMOTE_ID" properties nil nil #'equal)
	:remote-state (alist-get "HUB_COMMENT_REMOTE_STATE" properties nil nil #'equal)
	:remote-anchor-state (alist-get "HUB_COMMENT_REMOTE_ANCHOR_STATE" properties nil nil #'equal)
	:remote-missing-at (alist-get "HUB_COMMENT_REMOTE_MISSING_AT" properties nil nil #'equal)
	:remote-last-seen-at (alist-get "HUB_COMMENT_REMOTE_LAST_SEEN_AT" properties nil nil #'equal)
	:remote-resolution-status (alist-get "HUB_COMMENT_REMOTE_RESOLUTION_STATUS" properties nil nil #'equal)
	:sync-kind (alist-get "HUB_COMMENT_SYNC_KIND" properties nil nil #'equal)
	:body-format (or (alist-get "HUB_COMMENT_BODY_FORMAT" properties nil nil #'equal)
			 "storage")
	:target-text target-text
	:target-start start
	:target-end end
	:body body
	:height (max 3 (+ 3 (length (split-string body "\n" t))))))

(defun hub/org-comment--reply-records-at-heading (sidecar-file)
  "Return direct reply child records under heading at point in SIDECAR-FILE."
  (let ((level (org-outline-level))
	(end (save-excursion (org-end-of-subtree t t)))
	replies)
    (save-excursion
      (forward-line 1)
      (while (re-search-forward org-heading-regexp end t)
	(goto-char (match-beginning 0))
	(cond
	 ((<= (org-outline-level) level)
	  (goto-char end))
	 ((and (= (org-outline-level) (1+ level))
	       (hub/org-comment--reply-heading-p))
	  (let* ((properties (hub/org-comment--parse-properties-at-heading))
		 (entry-end (save-excursion (org-end-of-subtree t t)))
		 (body (hub/org-comment--entry-body entry-end)))
	    (push (hub/org-comment--base-record properties sidecar-file nil nil nil body)
		  replies))))
	(forward-line 1)))
    (nreverse replies)))

(defun hub/org-comment--page-record-from-heading (sidecar-file)
  "Return a page-level comment record at point in SIDECAR-FILE, or nil."
  (let* ((properties (hub/org-comment--parse-properties-at-heading))
	 (id (alist-get "HUB_COMMENT_ID" properties nil nil #'equal))
	 (sync-kind (alist-get "HUB_COMMENT_SYNC_KIND" properties nil nil #'equal))
	 (entry-end (save-excursion (org-end-of-subtree t t)))
	 (body (hub/org-comment--entry-body entry-end)))
    (when (and id (equal sync-kind "footer"))
      (append (hub/org-comment--base-record properties sidecar-file nil nil nil body)
	      (list :page-comment t
		    :anchor-state 'page
		    :anchor-line 1
		    :jump-pos nil
		    :replies (hub/org-comment--reply-records-at-heading sidecar-file))))))

(defun hub/org-comment--record-from-heading (sidecar-file source-buffer &optional include-stale)
  "Return a comment record at heading in SIDECAR-FILE for SOURCE-BUFFER.
When INCLUDE-STALE is non-nil, return unanchored stale records for comments
whose stored target no longer validates against the source buffer."
  (let* ((properties (hub/org-comment--parse-properties-at-heading))
	 (id (alist-get "HUB_COMMENT_ID" properties nil nil #'equal))
	 (target-text (alist-get "HUB_COMMENT_TARGET_TEXT" properties nil nil #'equal))
	 (target-bounds (hub/org-comment--target-bounds properties))
	 (start (car target-bounds))
	 (end (cdr target-bounds))
	 (entry-end (save-excursion (org-end-of-subtree t t)))
	 (body (hub/org-comment--entry-body entry-end))
	 (sync-kind (alist-get "HUB_COMMENT_SYNC_KIND" properties nil nil #'equal)))
    (when (and id
	       (not (equal sync-kind "reply"))
	       (or target-text (equal sync-kind "inline")))
      (let ((record (append (hub/org-comment--base-record
			     properties sidecar-file target-text start end body)
			    (list :replies (hub/org-comment--reply-records-at-heading sidecar-file)))))
	(if (hub/org-comment--valid-anchor-p source-buffer properties)
	    (with-current-buffer source-buffer
	      (append record
		      (list :anchor-line (line-number-at-pos start t)
			    :anchor-pos start
			    :jump-pos start)))
	  (when include-stale
	    (append record
		    (list :anchor-state 'stale
			  :stale t
			  :anchor-line most-positive-fixnum))))))))

(defun hub/org-comment-goto-id (comment-id)
  "Move point to sidecar heading with COMMENT-ID and return non-nil when found."
  (goto-char (point-min))
  (cl-loop while (re-search-forward org-heading-regexp nil t)
	   do (goto-char (match-beginning 0))
	   when (equal comment-id (org-entry-get nil "HUB_COMMENT_ID"))
	   return t
	   do (forward-line 1)))

(defun hub/org-comment-local-id-for-remote-id (sidecar-file remote-id)
  "Return local comment id in SIDECAR-FILE for Confluence REMOTE-ID."
  (when (and remote-id (file-exists-p sidecar-file))
    (with-temp-buffer
      (insert-file-contents sidecar-file)
      (org-mode)
      (goto-char (point-min))
      (cl-loop while (re-search-forward org-heading-regexp nil t)
	       do (goto-char (match-beginning 0))
	       when (equal remote-id (org-entry-get nil "HUB_COMMENT_REMOTE_ID"))
	       return (org-entry-get nil "HUB_COMMENT_ID")
	       do (forward-line 1)))))

(defun hub/org-comment--collect-from-sidecar (sidecar-file collector)
  "Collect comments from SIDECAR-FILE using heading COLLECTOR."
  (with-temp-buffer
    (insert-file-contents sidecar-file)
    (org-mode)
    (let (comments)
      (goto-char (point-min))
      (while (re-search-forward org-heading-regexp nil t)
	(goto-char (match-beginning 0))
	(when-let* ((record (funcall collector)))
	  (push record comments))
	(forward-line 1))
      (nreverse comments))))

(defun hub/org-comment-collect (&optional source-buffer include-stale)
  "Collect valid sidecar comments for SOURCE-BUFFER or current buffer.
When INCLUDE-STALE is non-nil, include comments whose stored target no longer
matches the source buffer as unanchored stale records."
  (let* ((buffer (or source-buffer (current-buffer)))
	 (source-file (buffer-file-name buffer))
	 (sidecar-file (and source-file (hub/org-comment-sidecar-path source-file))))
    (when (and sidecar-file (file-exists-p sidecar-file))
      (hub/org-comment--collect-from-sidecar
       sidecar-file
       (lambda ()
	 (hub/org-comment--record-from-heading sidecar-file buffer include-stale))))))

(defun hub/org-comment-collect-page (&optional source-buffer)
  "Collect page-level sidecar comments for SOURCE-BUFFER or current buffer."
  (let* ((buffer (or source-buffer (current-buffer)))
	 (source-file (buffer-file-name buffer))
	 (sidecar-file (and source-file (hub/org-comment-sidecar-path source-file))))
    (when (and sidecar-file (file-exists-p sidecar-file))
      (hub/org-comment--collect-from-sidecar
       sidecar-file
       (lambda ()
	 (hub/org-comment--page-record-from-heading sidecar-file))))))

(defun hub/org-comment--inline-anchor-target-text (properties)
  "Return matching target text from inline comment PROPERTIES."
  (alist-get "HUB_COMMENT_TARGET_TEXT" properties nil nil #'equal))

(defun hub/org-comment--normalized-buffer-with-map ()
  "Return normalized buffer text with source position map for current buffer.
The result is a cons cell (TEXT . MAP).  MAP entries are cons cells of original
buffer start and end positions for each character in TEXT."
  (let ((position (point-min))
	(text nil)
	(map nil)
	(pending-space nil))
    (while (< position (point-max))
      (let ((char (char-after position)))
	(if (and char (string-match-p "[[:space:]]" (char-to-string char)))
	    (let ((start position))
	      (while (and (< position (point-max))
			  (string-match-p "[[:space:]]" (char-to-string (char-after position))))
		(setq position (1+ position)))
	      (setq pending-space (cons start position)))
	  (when pending-space
	    (unless (null text)
	      (push ?\s text)
	      (push pending-space map))
	    (setq pending-space nil))
	  (push char text)
	  (push (cons position (1+ position)) map)
	  (setq position (1+ position)))))
    (cons (apply #'string (nreverse text)) (nreverse map))))

(defun hub/org-comment--source-match-from-normalized (map start end)
  "Return source buffer cons from normalized MAP START END positions."
  (cons (car (nth start map))
	(cdr (nth (1- end) map))))

(defun hub/org-comment--anchor-matches-for-text (source-buffer target-text)
  "Return source-buffer matches for normalized TARGET-TEXT."
  (let ((target (hub/org-comment-normalize-target-text (or target-text ""))))
    (unless (string-empty-p target)
      (with-current-buffer source-buffer
	(let* ((normalized (hub/org-comment--normalized-buffer-with-map))
	       (source (car normalized))
	       (map (cdr normalized))
	       (start 0)
	       matches)
	  (while (string-match (regexp-quote target) source start)
	    (let* ((match-start (match-beginning 0))
		   (match-end (match-end 0)))
	      (push (hub/org-comment--source-match-from-normalized map match-start match-end) matches)
	      (setq start (max (1+ match-start) match-end))))
	  (nreverse matches))))))

(defun hub/org-comment--similarity (left right)
  "Return approximate similarity ratio between LEFT and RIGHT."
  (let* ((a (hub/org-comment-normalize-target-text (or left "")))
	 (b (hub/org-comment-normalize-target-text (or right "")))
	 (max-length (max (length a) (length b))))
    (if (zerop max-length)
	0.0
      (max 0.0 (- 1.0 (/ (float (string-distance a b)) max-length))))))

(defun hub/org-comment--target-seeds (target)
  "Return useful word seed plists for normalized TARGET."
  (let* ((text (hub/org-comment-normalize-target-text (or target "")))
	 (words nil)
	 (index 0))
    (while (string-match "[[:alnum:]][[:alnum:]'’_-]*" text index)
      (let ((word (match-string 0 text)))
	(when (>= (length word) 3)
	  (push (list :word word :index (match-beginning 0)) words)))
      (setq index (match-end 0)))
    (setq words (nreverse words))
    (delete-dups
     (append (delq nil (list (car words)
			     (car (last words))
			     (cadr words)
			     (cadr (reverse words))))
	     (sort (copy-sequence words)
		   (lambda (left right)
		     (> (length (plist-get left :word))
			(length (plist-get right :word)))))))))

(defun hub/org-comment--target-window-eligible-p (window)
  "Return non-nil when shrink WINDOW is safe enough for exact anchoring."
  (let ((text (hub/org-comment-normalize-target-text (or window "")))
	(count 0)
	(index 0))
    (while (string-match "[[:alnum:]][[:alnum:]'’_-]*" text index)
      (when (>= (length (match-string 0 text)) 3)
	(setq count (1+ count)))
      (setq index (match-end 0)))
    (or (>= count 3) (>= (length text) 12))))

(defun hub/org-comment--camel-boundary-spaced (text)
  "Return TEXT with likely lowercase-to-uppercase word boundaries spaced."
  (let ((case-fold-search nil))
    (replace-regexp-in-string "\\([[:lower:]]\\)\\([[:upper:]]\\)" "\\1 \\2" text)))

(defun hub/org-comment--shrink-windows (target-text)
  "Return contiguous shrink windows for TARGET-TEXT, longest first."
  (let* ((target (hub/org-comment-normalize-target-text
		  (hub/org-comment--camel-boundary-spaced (or target-text ""))))
	 (words (split-string target "[[:space:]]+" t))
	 windows)
    (cl-loop for size from (length words) downto 1
	     do (cl-loop for start from 0 to (- (length words) size)
			 for window = (string-join (seq-subseq words start (+ start size)) " ")
			 when (hub/org-comment--target-window-eligible-p window)
			 do (push window windows)))
    (nreverse (delete-dups windows))))

(defun hub/org-comment--shrink-anchor-candidates (source-buffer target-text)
  "Return exact shrink candidates for TARGET-TEXT in SOURCE-BUFFER."
  (let (candidates best-ambiguous-count unique)
    (catch 'found
      (dolist (window (hub/org-comment--shrink-windows target-text))
	(let* ((matches (hub/org-comment--anchor-matches-for-text source-buffer window))
	       (count (length matches)))
	  (cond
	   ((= count 1)
	    (setq unique (list :kind 'exact
			       :start (caar matches)
			       :end (cdar matches)
			       :target-text window))
	    (throw 'found nil))
	   ((> count 1)
	    (setq best-ambiguous-count (if best-ambiguous-count
					   (min best-ambiguous-count count)
					 count))
	    (dolist (match matches)
	      (push (list :kind 'exact
			  :match-count count
			  :window-length (length window)
			  :start (car match)
			  :end (cdr match)
			  :target-text window)
		    candidates)))))))
    (list :unique unique
	  :candidates
	  (seq-take
	   (sort (cl-remove-duplicates
		  candidates
		  :test (lambda (left right)
			  (and (= (plist-get left :start) (plist-get right :start))
			       (= (plist-get left :end) (plist-get right :end))
			       (equal (plist-get left :target-text)
				      (plist-get right :target-text)))))
		 (lambda (left right)
		   (or (> (plist-get left :window-length) (plist-get right :window-length))
		       (and (= (plist-get left :window-length) (plist-get right :window-length))
			    (< (plist-get left :match-count) (plist-get right :match-count)))
		       (and (= (plist-get left :window-length) (plist-get right :window-length))
			    (= (plist-get left :match-count) (plist-get right :match-count))
			    (< (plist-get left :start) (plist-get right :start))))))
	   5)
	  :best-count best-ambiguous-count)))

(defun hub/org-comment--fuzzy-anchor-candidates (source-buffer target-text)
  "Return fuzzy candidate plists for TARGET-TEXT in SOURCE-BUFFER."
  (let ((target (hub/org-comment-normalize-target-text (or target-text ""))))
    (unless (string-empty-p target)
      (with-current-buffer source-buffer
	(let* ((normalized (hub/org-comment--normalized-buffer-with-map))
	       (source (car normalized))
	       (map (cdr normalized))
	       (target-length (length target))
	       (margin (max 12 (/ target-length 5)))
	       candidates)
	  (dolist (seed (hub/org-comment--target-seeds target))
	    (let ((start 0)
		  (word (regexp-quote (plist-get seed :word)))
		  (seed-index (plist-get seed :index)))
	      (while (string-match word source start)
		(let* ((match-start (match-beginning 0))
		       (window-start (max 0 (- match-start seed-index margin)))
		       (window-end (min (length source)
					(+ window-start target-length (* 2 margin))))
		       (window (substring source window-start window-end))
		       (score (hub/org-comment--similarity target window)))
		  (when (>= score 0.72)
		    (let ((source-match (hub/org-comment--source-match-from-normalized
					 map window-start window-end)))
		      (push (list :kind 'fuzzy
				  :score score
				  :start (car source-match)
				  :end (cdr source-match)
				  :text window)
			    candidates)))
		  (setq start (max (1+ match-start) (match-end 0)))))))
	  (setq candidates
		(sort (cl-remove-duplicates
		       candidates
		       :test (lambda (left right)
			       (and (= (plist-get left :start) (plist-get right :start))
				    (= (plist-get left :end) (plist-get right :end)))))
		      (lambda (left right)
			(> (plist-get left :score) (plist-get right :score)))))
	  (let ((strong (cl-remove-if-not (lambda (candidate)
					    (>= (plist-get candidate :score) 0.9))
					  candidates)))
	    (if strong
		(seq-take strong 3)
	      (seq-take candidates 3))))))))

(defun hub/org-comment--put-anchor-properties (source-buffer start end target-text)
  "Put anchor properties for SOURCE-BUFFER START END and TARGET-TEXT at point."
  (let ((start-line-column nil)
	(end-line-column nil)
	(normalized (hub/org-comment-normalize-target-text target-text)))
    (with-current-buffer source-buffer
      (setq start-line-column (hub/org-comment--line-column-at start))
      (setq end-line-column (hub/org-comment--line-column-at end)))
    (org-entry-put nil "HUB_COMMENT_TARGET" (format "%s %s" start end))
    (org-entry-put nil "HUB_COMMENT_TARGET_LINES"
		   (format "%s:%s %s:%s"
			   (car start-line-column) (cdr start-line-column)
			   (car end-line-column) (cdr end-line-column)))
    (org-entry-put nil "HUB_COMMENT_TARGET_TEXT" normalized)
    (org-entry-delete nil "HUB_COMMENT_TARGET_HASH")
    (org-entry-delete nil "HUB_COMMENT_ANCHOR_STATE")
    (org-entry-delete nil "HUB_COMMENT_ANCHOR_MATCH_COUNT")))

(defun hub/org-comment--source-context-label (source-buffer candidate &optional exact)
  "Return completion label for CANDIDATE in SOURCE-BUFFER.
When EXACT is non-nil, use an exact-match prefix instead of a fuzzy score."
  (with-current-buffer source-buffer
    (let* ((start (plist-get candidate :start))
	   (end (plist-get candidate :end))
	   (line (line-number-at-pos start t))
	   (column (save-excursion (goto-char start) (current-column)))
	   (before-start (max (point-min) (- start 36)))
	   (after-end (min (point-max) (+ end 36)))
	   (before (hub/org-comment-normalize-target-text
		    (buffer-substring-no-properties before-start start)))
	   (match (hub/org-comment-normalize-target-text
		   (buffer-substring-no-properties start end)))
	   (after (hub/org-comment-normalize-target-text
		   (buffer-substring-no-properties end after-end)))
	   (prefix (if exact
		       "exact"
		     (format "%.2f" (plist-get candidate :score)))))
      (format "%s · L%s:C%s · …%s [%s] %s…"
	      prefix line column before match after))))

(defun hub/org-comment--prompt-preview (text length)
  "Return a short decoded prompt preview for TEXT up to LENGTH."
  (or (hub/org-comment--preview-text text length) ""))

(defun hub/org-comment--triage-prompt (properties target-text body)
  "Return prompt for anchoring PROPERTIES with TARGET-TEXT and BODY."
  (let ((author (or (alist-get "HUB_COMMENT_AUTHOR" properties nil nil #'equal)
		    (alist-get "HUB_COMMENT_REMOTE_AUTHOR_ID" properties nil nil #'equal))))
    (format "Anchor “%s”%s: "
	    (hub/org-comment--prompt-preview target-text 36)
	    (let ((body-preview (hub/org-comment--prompt-preview body 48)))
	      (if (string-empty-p body-preview)
		  ""
		(format " — %s%s" (if author (concat author ": ") "") body-preview))))))

(defun hub/org-comment--candidate-choice (source-buffer properties target-text body exact-matches)
  "Read an anchor candidate for PROPERTIES from exact and fuzzy matches."
  (let* ((exact-candidates
	  (mapcar (lambda (match)
		    (list :kind 'exact :start (car match) :end (cdr match)
			  :target-text target-text))
		  exact-matches))
	 (fuzzy-candidates (hub/org-comment--fuzzy-anchor-candidates source-buffer target-text))
	 (shrink (unless fuzzy-candidates
		   (hub/org-comment--shrink-anchor-candidates source-buffer target-text)))
	 (shrink-candidates (plist-get shrink :candidates))
	 (candidates (append exact-candidates fuzzy-candidates shrink-candidates))
	 (skip-label "SKIP · leave this comment unanchored")
	 (entries (list (cons skip-label nil))))
    (dolist (candidate candidates)
      (push (cons (hub/org-comment--source-context-label
		   source-buffer candidate (eq (plist-get candidate :kind) 'exact))
		  candidate)
	    entries))
    (setq entries (nreverse entries))
    (alist-get
     (completing-read (hub/org-comment--triage-prompt properties target-text body)
		      (mapcar #'car entries) nil t nil nil skip-label)
     entries nil nil #'equal)))

(defun hub/org-comment--triage-imported-inline-comments (source-buffer sidecar-file)
  "Interactively triage unresolved imported inline comments in SIDECAR-FILE."
  (let ((selected 0)
	(skipped 0)
	(last-start nil)
	(last-end nil))
    (with-temp-buffer
      (insert-file-contents sidecar-file)
      (org-mode)
      (goto-char (point-min))
      (while (re-search-forward org-heading-regexp nil t)
	(goto-char (match-beginning 0))
	(let* ((properties (hub/org-comment--parse-properties-at-heading))
	       (source (alist-get "HUB_COMMENT_SOURCE" properties nil nil #'equal))
	       (sync-kind (alist-get "HUB_COMMENT_SYNC_KIND" properties nil nil #'equal)))
	  (when (and (equal source "confluence")
		     (equal sync-kind "inline")
		     (not (hub/org-comment--valid-anchor-p source-buffer properties)))
	    (let* ((entry-end (save-excursion (org-end-of-subtree t t)))
		   (body (hub/org-comment--entry-body entry-end))
		   (target-text (hub/org-comment--inline-anchor-target-text properties))
		   (exact-matches (hub/org-comment--anchor-matches-for-text source-buffer target-text))
		   (choice (hub/org-comment--candidate-choice
			    source-buffer properties target-text body
			    (when (> (length exact-matches) 1) exact-matches))))
	      (if choice
		  (progn
		    (hub/org-comment--put-anchor-properties
		     source-buffer (plist-get choice :start) (plist-get choice :end)
		     (or (plist-get choice :target-text) target-text))
		    (setq selected (1+ selected)
			  last-start (plist-get choice :start)
			  last-end (plist-get choice :end)))
		(setq skipped (1+ skipped))))))
	(forward-line 1))
      (write-region (point-min) (point-max) sidecar-file nil 'silent))
    (when last-start
      (with-current-buffer source-buffer
	(goto-char last-start)
	(pulse-momentary-highlight-region last-start last-end)))
    (list :selected selected :skipped skipped)))

(defun hub/org-comment--anchor-imported-inline-comments (&optional source-buffer sidecar-file)
  "Anchor imported Confluence inline comments for SOURCE-BUFFER in SIDECAR-FILE.
Return a plist summary with `:anchored', `:missing', and `:ambiguous' counts."
  (let* ((buffer (or source-buffer (current-buffer)))
	 (source-file (buffer-file-name buffer))
	 (target-sidecar (or sidecar-file (and source-file (hub/org-comment-sidecar-path source-file))))
	 (anchored 0)
	 (missing 0)
	 (ambiguous 0))
    (unless (and target-sidecar (file-exists-p target-sidecar))
      (user-error "No sidecar comments file: %s" target-sidecar))
    (with-temp-buffer
      (insert-file-contents target-sidecar)
      (org-mode)
      (goto-char (point-min))
      (while (re-search-forward org-heading-regexp nil t)
	(goto-char (match-beginning 0))
	(let* ((properties (hub/org-comment--parse-properties-at-heading))
	       (source (alist-get "HUB_COMMENT_SOURCE" properties nil nil #'equal))
	       (sync-kind (alist-get "HUB_COMMENT_SYNC_KIND" properties nil nil #'equal)))
	  (when (and (equal source "confluence")
		     (equal sync-kind "inline")
		     (not (hub/org-comment--valid-anchor-p buffer properties)))
	    (let* ((target-text (hub/org-comment--inline-anchor-target-text properties))
		   (matches (hub/org-comment--anchor-matches-for-text buffer target-text))
		   (count (length matches)))
	      (pcase count
		(0
		 (let* ((fuzzy-candidates (hub/org-comment--fuzzy-anchor-candidates buffer target-text))
			(shrink (unless fuzzy-candidates
				  (hub/org-comment--shrink-anchor-candidates buffer target-text)))
			(unique (plist-get shrink :unique))
			(best-count (plist-get shrink :best-count)))
		   (cond
		    (unique
		     (hub/org-comment--put-anchor-properties
		      buffer (plist-get unique :start) (plist-get unique :end)
		      (plist-get unique :target-text))
		     (setq anchored (1+ anchored)))
		    (best-count
		     (org-entry-put nil "HUB_COMMENT_ANCHOR_STATE" "ambiguous")
		     (org-entry-put nil "HUB_COMMENT_ANCHOR_MATCH_COUNT" (number-to-string best-count))
		     (setq ambiguous (1+ ambiguous)))
		    (t
		     (org-entry-put nil "HUB_COMMENT_ANCHOR_STATE" "missing")
		     (org-entry-put nil "HUB_COMMENT_ANCHOR_MATCH_COUNT" "0")
		     (setq missing (1+ missing))))))
		(1
		 (hub/org-comment--put-anchor-properties
		  buffer (caar matches) (cdar matches) target-text)
		 (setq anchored (1+ anchored)))
		(_
		 (org-entry-put nil "HUB_COMMENT_ANCHOR_STATE" "ambiguous")
		 (org-entry-put nil "HUB_COMMENT_ANCHOR_MATCH_COUNT" (number-to-string count))
		 (setq ambiguous (1+ ambiguous)))))))
	(forward-line 1))
      (write-region (point-min) (point-max) target-sidecar nil 'silent))
    (list :anchored anchored :missing missing :ambiguous ambiguous)))

(defun hub/org-comment--project-root (&optional file)
  "Return project-like root for FILE, or nil."
  (let ((directory (file-name-directory (or file default-directory))))
    (or (locate-dominating-file directory "domains.yaml")
	(locate-dominating-file directory ".git"))))

(defun hub/org-comment-link-source-path (source-file)
  "Return portable source path for SOURCE-FILE in `org-comment:' links."
  (let ((root (hub/org-comment--project-root source-file)))
    (if root
	(file-relative-name source-file root)
      (abbreviate-file-name source-file))))

(defun hub/org-comment--link-description (description fallback)
  "Return DESCRIPTION sanitized for use as an Org link label.
FALLBACK is used when DESCRIPTION is nil or empty."
  (let ((label (or (hub/org-comment--present-string description) fallback)))
    (replace-regexp-in-string "[][]" "" label)))

(defun hub/org-comment-make-link (source-file comment-id &optional description)
  "Return an Org link to COMMENT-ID belonging to SOURCE-FILE."
  (format "[[org-comment:%s::%s][%s]]"
	  (hub/org-comment-link-source-path source-file)
	  comment-id
	  (hub/org-comment--link-description description comment-id)))

(defun hub/org-comment--resolve-source-file (path)
  "Resolve org-comment source PATH to an absolute file name."
  (cond
   ((file-name-absolute-p path) (expand-file-name path))
   ((let* ((root (hub/org-comment--project-root default-directory))
	   (candidate (and root (expand-file-name path root))))
      (and candidate (file-exists-p candidate) candidate)))
   (t (expand-file-name path default-directory))))

(defun hub/org-comment--parse-link-path (path)
  "Parse org-comment PATH into source file and comment id."
  (if (string-match "\\`\\(.+\\)::\\(.+\\)\\'" path)
      (cons (hub/org-comment--resolve-source-file (match-string 1 path))
	    (match-string 2 path))
    (cons (or buffer-file-name
	      (when (and (string-suffix-p ".comments.org" (or buffer-file-name "")))
		(hub/org-comment-source-file-from-sidecar buffer-file-name)))
	  path)))

(defun hub/org-comment-open-link (path &optional _arg)
  "Open org-comment link PATH."
  (pcase-let* ((`(,source-file . ,comment-id) (hub/org-comment--parse-link-path path))
	       (sidecar-file (and source-file (hub/org-comment-sidecar-path source-file))))
    (unless (and source-file (file-exists-p source-file))
      (user-error "Cannot resolve org-comment source file: %s" (or source-file "<none>")))
    (unless (and sidecar-file (file-exists-p sidecar-file))
      (user-error "Cannot find comments sidecar for %s" source-file))
    (let ((source-buffer (find-file-noselect source-file)))
      (with-current-buffer source-buffer
	(org-mode))
      (with-current-buffer (find-file-noselect sidecar-file)
	(org-mode)
	(unless (hub/org-comment-goto-id comment-id)
	  (user-error "Cannot find org comment %s" comment-id))
	(let* ((sync-kind (org-entry-get nil "HUB_COMMENT_SYNC_KIND"))
	       (reply-parent
		(when (equal sync-kind "reply")
		  (save-excursion
		    (when (org-up-heading-safe)
		      (list :id (org-entry-get nil "HUB_COMMENT_ID")
			    :sync-kind (org-entry-get nil "HUB_COMMENT_SYNC_KIND"))))))
	       (lookup-id (or (plist-get reply-parent :id) comment-id))
	       (inline-record (when (equal (or (plist-get reply-parent :sync-kind) sync-kind) "inline")
				(seq-find (lambda (record)
					    (equal lookup-id (plist-get record :id)))
					  (with-current-buffer source-buffer
					    (hub/org-comment-collect source-buffer nil))))))
	  (cond
	   ((and (equal (or (plist-get reply-parent :sync-kind) sync-kind) "footer")
		 (fboundp 'hub/org-page-context-open-comment))
	    (pop-to-buffer source-buffer)
	    (hub/org-page-context-open-comment comment-id))
	   ((and inline-record (fboundp 'hub/org-context-panel-open-comment))
	    (pop-to-buffer source-buffer)
	    (hub/org-context-panel-open-comment
	     comment-id (plist-get inline-record :jump-pos)))
	   (t
	    (pop-to-buffer (current-buffer)))))))))

(defun hub/org-comment-store-link ()
  "Store an `org-comment:' link for the sidecar comment at point."
  (when (and buffer-file-name
	     (string-suffix-p ".comments.org" buffer-file-name)
	     (derived-mode-p 'org-mode)
	     (not (org-before-first-heading-p)))
    (org-back-to-heading t)
    (when-let* ((comment-id (org-entry-get nil "HUB_COMMENT_ID")))
      (let* ((source-file (hub/org-comment-source-file-from-sidecar buffer-file-name))
	     (heading (org-get-heading t t t t))
	     (link (hub/org-comment-make-link source-file comment-id heading)))
	(org-link-store-props :type "org-comment"
			      :link link
			      :description heading)
	link))))

(org-link-set-parameters "org-comment"
			 :follow #'hub/org-comment-open-link
			 :store #'hub/org-comment-store-link)

(provide 'hub-org-comments)
;;; hub-org-comments.el ends here
