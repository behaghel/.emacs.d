;;; org-comments-sidecar.el --- Sidecar files for Org comments -*- lexical-binding: t; -*-

;;; Commentary:
;; Sidecar path, header, formatting, and append helpers for the initial Org
;; comments extraction.  Names remain in the legacy `org-comments-*'
;; namespace until the public API migration slice.

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'org-comments-collaboration)
(require 'org-comments-core)
(require 'org-comments-model)
(require 'subr-x)

(defun org-comments-sidecar-path (&optional source-file)
  "Return sidecar comments path for SOURCE-FILE or current buffer file.
For example, article.org maps to article.comments.org."
  (let ((file (or source-file buffer-file-name)))
    (unless file
      (user-error "Current buffer is not visiting a file"))
    (concat (file-name-sans-extension file) ".comments.org")))

(defun org-comments--relative-source-file (source-file sidecar-file)
  "Return SOURCE-FILE relative to SIDECAR-FILE directory."
  (file-relative-name source-file (file-name-directory sidecar-file)))

(defun org-comments-source-file-from-sidecar (&optional sidecar-file)
  "Return source Org file referenced by SIDECAR-FILE or current sidecar."
  (let* ((file (or sidecar-file buffer-file-name))
	 (directory (and file (file-name-directory file))))
    (unless file
      (user-error "No comments sidecar file"))
    (with-temp-buffer
      (insert-file-contents file)
      (let ((source (org-comments--keyword-from-buffer "SOURCE")))
	(unless source
	  (user-error "Comments sidecar has no #+source keyword"))
	(expand-file-name source directory)))))

(defun org-comments--ensure-sidecar-header (sidecar-file source-file)
  "Create SIDECAR-FILE with a minimal header for SOURCE-FILE when absent."
  (unless (file-exists-p sidecar-file)
    (make-directory (file-name-directory sidecar-file) t)
    (with-temp-file sidecar-file
      (insert (format "#+title: Comments for %s\n" (file-name-nondirectory source-file)))
      (insert (format "#+source: %s\n"
		      (org-comments--relative-source-file source-file sidecar-file)))
      (insert "#+todo: OPEN TODO | RESOLVED\n\n"))))

(defun org-comments-ensure-sidecar-header (sidecar-file source-file)
  "Create SIDECAR-FILE with a minimal header for SOURCE-FILE when absent."
  (org-comments--ensure-sidecar-header sidecar-file source-file))

(defun org-comments--property-line (key value)
  "Return an Org property line for KEY and VALUE."
  (format ":%s: %s\n" key value))

(defun org-comments-format-entry (record sidecar-file)
  "Return Org text for comment RECORD."
  (let ((title (or (plist-get record :title)
		   (org-comments-heading-title
		    record (file-name-directory sidecar-file)))))
    (concat
     (format "* %s %s\n" (or (plist-get record :status) "OPEN") title)
     ":PROPERTIES:\n"
     (org-comments--property-line "ORG_COMMENTS_ID" (plist-get record :id))
     (when-let* ((author (plist-get record :author)))
       (org-comments--property-line "ORG_COMMENTS_AUTHOR" author))
     (when-let* ((created-at (plist-get record :created-at)))
       (org-comments--property-line "ORG_COMMENTS_CREATED_AT" created-at))
     (when-let* ((sync-kind (plist-get record :sync-kind)))
       (org-comments--property-line "ORG_COMMENTS_SYNC_KIND" sync-kind))
     (org-comments--property-line
      "ORG_COMMENTS_TARGET"
      (format "%s %s" (plist-get record :target-start) (plist-get record :target-end)))
     (org-comments--property-line
      "ORG_COMMENTS_TARGET_LINES"
      (format "%s:%s %s:%s"
	      (plist-get record :target-start-line)
	      (plist-get record :target-start-column)
	      (plist-get record :target-end-line)
	      (plist-get record :target-end-column)))
     (org-comments--property-line "ORG_COMMENTS_TARGET_TEXT" (plist-get record :target-text))
     ":END:\n\n"
     (string-trim-right (or (plist-get record :body) ""))
     "\n")))

(defun org-comments-remote-status-value (status)
  "Return remote resolution value corresponding to local TODO STATUS."
  (when (equal status "RESOLVED")
    "resolved"))

(defun org-comments-entry-mark-status-dirty (status)
  "Update current heading dirty metadata after setting local TODO STATUS.
Remote-backed comments get `ORG_COMMENTS_LOCAL_STATUS_DIRTY' when STATUS differs
from known remote state.  Matching or local-only comments clear that marker."
  (let ((remote-id (org-entry-get nil "ORG_COMMENTS_REMOTE_ID"))
	(remote-status (org-entry-get nil "ORG_COMMENTS_REMOTE_RESOLUTION_STATUS"))
	(target-remote-status (org-comments-remote-status-value status)))
    (if (and remote-id
	     target-remote-status
	     (not (equal remote-status target-remote-status)))
	(progn
	  (org-entry-put nil "ORG_COMMENTS_LOCAL_STATUS_DIRTY" "status")
	  (org-entry-put nil "ORG_COMMENTS_LOCAL_UPDATED_AT"
			 (org-comments-current-created-at)))
      (org-entry-delete nil "ORG_COMMENTS_LOCAL_STATUS_DIRTY"))))

(defun org-comments-set-entry-status (status)
  "Set current sidecar entry TODO state to STATUS and update dirty metadata."
  (org-todo status)
  (org-comments-entry-mark-status-dirty status))

(defun org-comments-append-to-sidecar (record &optional sidecar-file)
  "Append comment RECORD to SIDECAR-FILE and return SIDECAR-FILE.
SIDECAR-FILE defaults to the sidecar path for RECORD's source file."
  (let* ((source-file (plist-get record :source-file))
	 (target-file (or sidecar-file (org-comments-sidecar-path source-file))))
    (org-comments--ensure-sidecar-header target-file source-file)
    (with-temp-buffer
      (insert-file-contents target-file)
      (goto-char (point-max))
      (unless (bolp) (insert "\n"))
      (unless (save-excursion
		(forward-line -1)
		(looking-at-p "[[:space:]]*$"))
	(insert "\n"))
      (insert (org-comments-format-entry record target-file))
      (write-region (point-min) (point-max) target-file nil 'silent))
    target-file))


(defun org-comments-fold-sidecar-property-drawers (&optional buffer)
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

(defun org-comments-refold-sidecar-file (sidecar-file)
  "Refold property drawers in any live buffer visiting SIDECAR-FILE."
  (when-let* ((buffer (find-buffer-visiting sidecar-file)))
    (with-current-buffer buffer
      (let ((point (point))
	    (window-starts (mapcar (lambda (window)
				     (cons window (window-start window)))
				   (get-buffer-window-list buffer nil t))))
	(revert-buffer :ignore-auto :noconfirm)
	(org-comments-fold-sidecar-property-drawers buffer)
	(goto-char (min point (point-max)))
	(dolist (entry window-starts)
	  (when (window-live-p (car entry))
	    (set-window-start (car entry) (cdr entry) t)))))))

(defun org-comments-update-anchor (sidecar-file comment-id record)
  "Update COMMENT-ID anchor metadata in SIDECAR-FILE from RECORD."
  (with-temp-buffer
    (insert-file-contents sidecar-file)
    (org-mode)
    (goto-char (point-min))
    (unless (cl-loop while (re-search-forward org-heading-regexp nil t)
		     do (goto-char (match-beginning 0))
		     when (equal comment-id (org-entry-get nil "ORG_COMMENTS_ID"))
		     return (progn
			      (org-entry-put nil "ORG_COMMENTS_TARGET"
					     (format "%s %s"
						     (plist-get record :target-start)
						     (plist-get record :target-end)))
			      (org-entry-put nil "ORG_COMMENTS_TARGET_LINES"
					     (format "%s:%s %s:%s"
						     (plist-get record :target-start-line)
						     (plist-get record :target-start-column)
						     (plist-get record :target-end-line)
						     (plist-get record :target-end-column)))
			      (org-entry-put nil "ORG_COMMENTS_TARGET_TEXT"
					     (plist-get record :target-text))
			      (org-entry-delete nil "ORG_COMMENTS_TARGET_HASH")
			      t)
		     do (forward-line 1))
      (user-error "Comment %s not found in sidecar" comment-id))
    (write-region (point-min) (point-max) sidecar-file nil 'silent)))

(defun org-comments-delete-entry (sidecar-file comment-id)
  "Delete COMMENT-ID subtree from SIDECAR-FILE."
  (with-temp-buffer
    (insert-file-contents sidecar-file)
    (org-mode)
    (goto-char (point-min))
    (unless (cl-loop while (re-search-forward org-heading-regexp nil t)
		     do (goto-char (match-beginning 0))
		     when (equal comment-id (org-entry-get nil "ORG_COMMENTS_ID"))
		     return (let ((start (point))
				  (end (save-excursion (org-end-of-subtree t t))))
			      (delete-region start end)
			      t)
		     do (forward-line 1))
      (user-error "Comment %s not found in sidecar" comment-id))
    (write-region (point-min) (point-max) sidecar-file nil 'silent)))

(defun org-comments-sidecar--filter-match-p (filters)
  "Return non-nil when current heading matches sidecar FILTERS."
  (and (or (not (plist-member filters :backend))
	   (equal (plist-get filters :backend)
		  (org-entry-get nil "ORG_COMMENTS_BACKEND")))
       (or (not (plist-member filters :source))
	   (equal (plist-get filters :source)
		  (org-entry-get nil "ORG_COMMENTS_SOURCE")))
       (or (not (plist-member filters :sync-kind))
	   (equal (plist-get filters :sync-kind)
		  (org-entry-get nil "ORG_COMMENTS_SYNC_KIND")))
       (or (not (plist-member filters :parent-remote-id))
	   (equal (plist-get filters :parent-remote-id)
		  (org-entry-get nil "ORG_COMMENTS_REMOTE_PARENT_ID")))))

(defun org-comments-sidecar-has-remote-p (sidecar-file remote-id &rest filters)
  "Return non-nil when SIDECAR-FILE contains REMOTE-ID matching FILTERS."
  (when (and remote-id (file-exists-p sidecar-file))
    (with-temp-buffer
      (insert-file-contents sidecar-file)
      (org-mode)
      (goto-char (point-min))
      (cl-loop while (re-search-forward org-heading-regexp nil t)
	       do (goto-char (match-beginning 0))
	       when (and (equal remote-id (org-entry-get nil "ORG_COMMENTS_REMOTE_ID"))
			 (org-comments-sidecar--filter-match-p filters))
	       return t
	       do (forward-line 1)))))

(defun org-comments-sidecar-remote-missing-p (sidecar-file remote-id &rest filters)
  "Return non-nil when REMOTE-ID in SIDECAR-FILE is marked missing.
FILTERS narrow the candidate heading by backend, source, sync kind, or parent."
  (when (and remote-id (file-exists-p sidecar-file))
    (with-temp-buffer
      (insert-file-contents sidecar-file)
      (org-mode)
      (goto-char (point-min))
      (cl-loop while (re-search-forward org-heading-regexp nil t)
	       do (goto-char (match-beginning 0))
	       when (and (equal remote-id (org-entry-get nil "ORG_COMMENTS_REMOTE_ID"))
			 (org-comments-sidecar--filter-match-p filters))
	       return (equal (org-entry-get nil "ORG_COMMENTS_REMOTE_STATE") "missing")
	       do (forward-line 1)))))

(defun org-comments-sidecar--timestamp-value (timestamp)
  "Return sidecar reconciliation timestamp for TIMESTAMP."
  (cond
   ((functionp timestamp) (funcall timestamp))
   ((stringp timestamp) timestamp)
   (t (org-comments-current-created-at))))

(defun org-comments-sidecar-mark-missing-at-heading (&optional timestamp)
  "Mark current remote-linked heading missing with optional TIMESTAMP."
  (org-entry-put nil "ORG_COMMENTS_REMOTE_STATE" "missing")
  (unless (org-entry-get nil "ORG_COMMENTS_REMOTE_MISSING_AT")
    (org-entry-put nil "ORG_COMMENTS_REMOTE_MISSING_AT"
		   (org-comments-sidecar--timestamp-value timestamp))))

(defun org-comments-sidecar-mark-present-at-heading (&optional last-seen-at)
  "Mark current remote-linked heading present and clear missing metadata.
When LAST-SEEN-AT is non-nil, record it as `ORG_COMMENTS_REMOTE_LAST_SEEN_AT'."
  (org-entry-delete nil "ORG_COMMENTS_REMOTE_STATE")
  (org-entry-delete nil "ORG_COMMENTS_REMOTE_MISSING_AT")
  (when last-seen-at
    (org-entry-put nil "ORG_COMMENTS_REMOTE_LAST_SEEN_AT"
		   (org-comments-sidecar--timestamp-value last-seen-at))))

(defun org-comments-sidecar-reconcile-missing (sidecar-file seen-ids &rest options)
  "Mark remote-linked sidecar entries absent from SEEN-IDS as missing.
OPTIONS may include `:backend', `:source', `:sync-kind', `:parent-remote-id',
`:timestamp', and `:on-missing'.  `:on-missing', when non-nil, is called at
matching missing headings with REMOTE-ID and whether the heading is newly
missing.  Return the number of newly missing headings."
  (when (file-exists-p sidecar-file)
    (with-temp-buffer
      (insert-file-contents sidecar-file)
      (org-mode)
      (let ((changed nil)
	    (missing-count 0)
	    (on-missing (plist-get options :on-missing)))
	(goto-char (point-min))
	(while (re-search-forward org-heading-regexp nil t)
	  (goto-char (match-beginning 0))
	  (let ((remote-id (org-entry-get nil "ORG_COMMENTS_REMOTE_ID")))
	    (when (and remote-id
		       (org-comments-sidecar--filter-match-p options)
		       (not (member remote-id seen-ids)))
	      (let ((newly-missing
		     (not (equal (org-entry-get nil "ORG_COMMENTS_REMOTE_STATE")
				 "missing"))))
		(when on-missing
		  (funcall on-missing remote-id newly-missing))
		(when newly-missing
		  (setq missing-count (1+ missing-count)))
		(org-comments-sidecar-mark-missing-at-heading
		 (plist-get options :timestamp))
		(setq changed t))))
	  (forward-line 1))
	(when changed
	  (write-region (point-min) (point-max) sidecar-file nil 'silent))
	missing-count))))

(defun org-comments--parse-properties-at-heading ()
  "Return an alist of Org properties at point without inherited values."
  (org-entry-properties nil nil))

(defun org-comments--entry-body (end)
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

(defun org-comments-entry-body (end)
  "Return current heading body text ending before child headings and END."
  (org-comments--entry-body end))

(defun org-comments--number-property (properties key)
  "Return numeric property KEY from PROPERTIES, or nil."
  (when-let* ((value (alist-get key properties nil nil #'equal)))
    (string-to-number value)))

(defun org-comments--target-bounds (properties)
  "Return target bounds cons from compact PROPERTIES."
  (when-let* ((value (alist-get "ORG_COMMENTS_TARGET" properties nil nil #'equal))
	      (parts (split-string value "[[:space:]]+" t)))
    (when (= 2 (length parts))
      (cons (string-to-number (car parts))
	    (string-to-number (cadr parts))))))

(defun org-comments--heading-status (properties)
  "Return comment workflow status from heading PROPERTIES."
  (or (alist-get "TODO" properties nil nil #'equal) "OPEN"))


(defun org-comments--reply-heading-p ()
  "Return non-nil when point is at a reply heading."
  (equal "reply" (org-entry-get nil "ORG_COMMENTS_SYNC_KIND")))

(defun org-comments--reply-count-at-heading ()
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
	       (org-comments--reply-heading-p))
	  (setq count (1+ count))))
	(forward-line 1)))
    count))

(defun org-comments--sidecar-local-state (remote-id local-updated-at)
  "Return normalized local-state flags for REMOTE-ID and LOCAL-UPDATED-AT."
  (delq nil
	(list (unless remote-id :local-only)
	      (when local-updated-at :edited))))

(defun org-comments--normalize-sidecar-record (record)
  "Return sidecar RECORD with collaboration fields normalized."
  (let ((normalized (copy-sequence record)))
    (when (and (not (plist-get normalized :backend))
	       (equal (plist-get normalized :source) "confluence"))
      (setq normalized (plist-put normalized :backend 'confluence)))
    (when (and (plist-get normalized :remote-id)
	       (not (plist-get normalized :remote-state)))
      (setq normalized (plist-put normalized :remote-state 'present)))
    (when (equal (plist-get normalized :remote-resolution-status) "resolved")
      (setq normalized (plist-put normalized :resolved t)))
    (setq normalized
	  (plist-put normalized :local-state
		     (org-comments--sidecar-local-state
		      (plist-get normalized :remote-id)
		      (plist-get normalized :local-updated-at))))
    (org-comments-normalize-record normalized)))

(defun org-comments--base-record (properties sidecar-file target-text start end body)
  "Return common normalized plist record from PROPERTIES and comment fields."
  (let ((remote-author-name
	 (alist-get "ORG_COMMENTS_REMOTE_AUTHOR_DISPLAY_NAME" properties nil nil #'equal)))
    (org-comments--normalize-sidecar-record
     (list :type 'comment
	   :id (alist-get "ORG_COMMENTS_ID" properties nil nil #'equal)
	   :kind 'comment
	   :status (org-comments--heading-status properties)
	   :sidecar-file sidecar-file
	   :author (alist-get "ORG_COMMENTS_AUTHOR" properties nil nil #'equal)
	   :created-at (or (alist-get "ORG_COMMENTS_REMOTE_CREATED_AT" properties nil nil #'equal)
			   (alist-get "ORG_COMMENTS_CREATED_AT" properties nil nil #'equal))
	   :local-updated-at (alist-get "ORG_COMMENTS_LOCAL_UPDATED_AT" properties nil nil #'equal)
	   :remote-author-id (alist-get "ORG_COMMENTS_REMOTE_AUTHOR_ID" properties nil nil #'equal)
	   :remote-author-display-name remote-author-name
	   :remote-author-name remote-author-name
	   :source (alist-get "ORG_COMMENTS_SOURCE" properties nil nil #'equal)
	   :remote-id (alist-get "ORG_COMMENTS_REMOTE_ID" properties nil nil #'equal)
	   :remote-state (alist-get "ORG_COMMENTS_REMOTE_STATE" properties nil nil #'equal)
	   :remote-anchor-state (alist-get "ORG_COMMENTS_REMOTE_ANCHOR_STATE" properties nil nil #'equal)
	   :remote-missing-at (alist-get "ORG_COMMENTS_REMOTE_MISSING_AT" properties nil nil #'equal)
	   :remote-last-seen-at (alist-get "ORG_COMMENTS_REMOTE_LAST_SEEN_AT" properties nil nil #'equal)
	   :remote-resolution-status (alist-get "ORG_COMMENTS_REMOTE_RESOLUTION_STATUS" properties nil nil #'equal)
	   :local-status-dirty (alist-get "ORG_COMMENTS_LOCAL_STATUS_DIRTY" properties nil nil #'equal)
	   :sync-kind (alist-get "ORG_COMMENTS_SYNC_KIND" properties nil nil #'equal)
	   :body-format (or (alist-get "ORG_COMMENTS_BODY_FORMAT" properties nil nil #'equal)
			    "storage")
	   :target-text target-text
	   :target-start start
	   :target-end end
	   :body body
	   :height (max 3 (+ 3 (length (split-string body "\n" t))))))))

(defun org-comments--title-record-from-heading (sidecar-file)
  "Return a plist record for refreshing the current heading in SIDECAR-FILE."
  (let* ((properties (org-comments--parse-properties-at-heading))
	 (entry-end (save-excursion (org-end-of-subtree t t)))
	 (body (org-comments--entry-body entry-end))
	 (record (append (org-comments--base-record
			  properties sidecar-file
			  (alist-get "ORG_COMMENTS_TARGET_TEXT" properties nil nil #'equal)
			  nil nil body)
			 (list :body-format (alist-get "ORG_COMMENTS_BODY_FORMAT" properties nil nil #'equal)))))
    (if (org-comments--reply-heading-p)
	record
      (append record (list :reply-count (org-comments--reply-count-at-heading))))))

(defconst org-comments-obsolete-metadata-properties
  '("ORG_COMMENTS_TARGET_HASH"
    "ORG_COMMENTS_PARENT_ID"
    "ORG_COMMENTS_REMOTE_TARGET_JSON")
  "Sidecar properties removed by metadata compaction.")

(defun org-comments--compact-heading-metadata ()
  "Compact metadata properties at the current sidecar heading."
  (let ((changed 0)
	(remote-linked (org-entry-get nil "ORG_COMMENTS_REMOTE_ID")))
    (dolist (property org-comments-obsolete-metadata-properties)
      (when (org-entry-get nil property)
	(org-entry-delete nil property)
	(setq changed (1+ changed))))
    (when (equal "storage" (org-entry-get nil "ORG_COMMENTS_BODY_FORMAT"))
      (org-entry-delete nil "ORG_COMMENTS_BODY_FORMAT")
      (setq changed (1+ changed)))
    (when (equal "present" (org-entry-get nil "ORG_COMMENTS_REMOTE_STATE"))
      (org-entry-delete nil "ORG_COMMENTS_REMOTE_STATE")
      (setq changed (1+ changed)))
    (when remote-linked
      (dolist (property '("ORG_COMMENTS_AUTHOR" "ORG_COMMENTS_CREATED_AT"))
	(when (org-entry-get nil property)
	  (org-entry-delete nil property)
	  (setq changed (1+ changed)))))
    changed))

(defun org-comments-compact-sidecar-metadata (&optional sidecar-file)
  "Remove obsolete or derivable comment metadata from SIDECAR-FILE.
When called interactively, use the current comments sidecar or the current Org
file's sidecar.  Return the number of removed properties."
  (interactive)
  (let ((sidecar-file (or sidecar-file
			  (if (and buffer-file-name
				   (string-suffix-p ".comments.org" buffer-file-name))
			      buffer-file-name
			    (org-comments-sidecar-path)))))
    (unless (file-exists-p sidecar-file)
      (user-error "No sidecar comments file: %s" sidecar-file))
    (let ((removed 0))
      (with-temp-buffer
	(insert-file-contents sidecar-file)
	(org-mode)
	(goto-char (point-min))
	(while (re-search-forward org-heading-regexp nil t)
	  (goto-char (match-beginning 0))
	  (when (org-entry-get nil "ORG_COMMENTS_ID")
	    (setq removed (+ removed (org-comments--compact-heading-metadata))))
	  (forward-line 1))
	(write-region (point-min) (point-max) sidecar-file nil 'silent))
      (org-comments-refold-sidecar-file sidecar-file)
      (when (called-interactively-p 'interactive)
	(message "Removed %s compactable sidecar propert%s"
		 removed (if (= removed 1) "y" "ies")))
      removed)))

(defun org-comments-refresh-sidecar-headings (&optional sidecar-file)
  "Refresh all comment headings in SIDECAR-FILE from metadata and body previews.
When called interactively, use the current sidecar file or the sidecar for the
current Org source file."
  (interactive)
  (let ((sidecar-file (or sidecar-file
			  (if (and buffer-file-name
				   (string-suffix-p ".comments.org" buffer-file-name))
			      buffer-file-name
			    (org-comments-sidecar-path)))))
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
	  (when (org-entry-get nil "ORG_COMMENTS_ID")
	    (let ((record (org-comments--title-record-from-heading sidecar-file)))
	      (org-edit-headline
	       (if (org-comments--reply-heading-p)
		   (org-comments-reply-heading-title record directory)
		 (org-comments-heading-title record directory))))
	    (setq count (1+ count)))
	  (forward-line 1))
	(write-region (point-min) (point-max) sidecar-file nil 'silent)
	(org-comments-refold-sidecar-file sidecar-file)
	(when (called-interactively-p 'interactive)
	  (message "Refreshed %s sidecar comment heading%s"
		   count (if (= count 1) "" "s")))
	count))))

(provide 'org-comments-sidecar)
;;; org-comments-sidecar.el ends here
