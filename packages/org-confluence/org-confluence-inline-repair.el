;;; org-confluence-inline-repair.el --- Repair Confluence inline comments -*- lexical-binding: t; -*-

;;; Commentary:
;; Repair helpers for missing Confluence inline comment marker anchors.

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'org-comments-sidecar)
(require 'seq)
(require 'subr-x)
(require 'xml)

(require 'org-confluence-api)
(require 'org-confluence-comments-context)
(require 'org-confluence-comments-remote)
(require 'org-confluence-inline-comments)
(require 'org-confluence-process)
(require 'org-confluence-response)

(defun org-confluence-inline-repair-run (command)
  "Run shell COMMAND and signal `user-error' on failure."
  (org-confluence-process-run command))

(defun org-confluence-inline-repair-source-occurrence-index (source-file target-text target-start)
  "Return occurrence index of TARGET-TEXT before TARGET-START in SOURCE-FILE."
  (when (and source-file
	     (file-readable-p source-file)
	     (org-confluence-api--present-string-p target-text)
	     (integerp target-start))
    (with-temp-buffer
      (insert-file-contents source-file)
      (let ((count 0)
	    (start 0)
	    found)
	(while (and (not found)
		    (string-match (regexp-quote target-text) (buffer-string) start))
	  (if (= (1+ (match-beginning 0)) target-start)
	      (setq found count)
	    (setq count (1+ count)
		  start (1+ (match-beginning 0)))))
	found))))

(defun org-confluence-inline-repair-sidecar-target-info (remote-id source-file)
  "Return sidecar target info for REMOTE-ID in SOURCE-FILE, or nil."
  (when-let* ((sidecar-file (and source-file (org-comments-sidecar-path source-file))))
    (when (and remote-id (file-readable-p sidecar-file))
      (with-temp-buffer
	(insert-file-contents sidecar-file)
	(org-mode)
	(goto-char (point-min))
	(cl-loop while (re-search-forward org-heading-regexp nil t)
		 do (goto-char (match-beginning 0))
		 when (equal remote-id (org-entry-get nil "ORG_COMMENTS_REMOTE_ID"))
		 return (let* ((target-text (org-entry-get nil "ORG_COMMENTS_TARGET_TEXT"))
			       (target (org-entry-get nil "ORG_COMMENTS_TARGET"))
			       (target-start (and target
						  (string-match "\\`[[:space:]]*\\([0-9]+\\)" target)
						  (string-to-number (match-string 1 target)))))
			  (list :target-text target-text
				:target-start target-start
				:occurrence-index
				(org-confluence-inline-repair-source-occurrence-index
				 source-file target-text target-start)))
		 do (forward-line 1))))))

(defun org-confluence-inline-repair-comment-marker-present-p (storage comment)
  "Return non-nil when COMMENT marker ref is present in STORAGE."
  (when-let* ((ref (org-confluence-comments-remote-inline-marker-ref comment)))
    (string-match-p (regexp-quote ref) storage)))

(defun org-confluence-inline-repair-candidate-for-comment (storage comment &optional source-file)
  "Return repair candidate plist for COMMENT in STORAGE using SOURCE-FILE hints."
  (let* ((remote-id (org-confluence-comments-remote-id comment))
	 (ref (org-confluence-comments-remote-inline-marker-ref comment))
	 (selection (org-confluence-comments-remote-inline-target-text comment))
	 (candidates (and selection
			  (org-confluence-inline-comments-inline-comment-candidates storage selection)))
	 (sidecar-info (org-confluence-inline-repair-sidecar-target-info remote-id source-file))
	 (occurrence-index (plist-get sidecar-info :occurrence-index))
	 chosen reason)
    (cond
     ((not (org-confluence-api--present-string-p ref))
      (setq reason "missing marker ref"))
     ((not (org-confluence-api--present-string-p selection))
      (setq reason "missing original selection"))
     ((org-confluence-inline-repair-comment-marker-present-p storage comment)
      (setq reason "marker already present"))
     ((null candidates)
      (setq reason "selection not found"))
     ((= (length candidates) 1)
      (setq chosen (car candidates)))
     ((and (integerp occurrence-index)
	   (< occurrence-index (length candidates)))
      (setq chosen (nth occurrence-index candidates)))
     (t
      (setq reason (format "ambiguous selection (%s candidates)" (length candidates)))))
    (append (list :comment-id remote-id
		  :ref ref
		  :selection selection
		  :candidate-count (length candidates)
		  :reason reason)
	    chosen)))

(defun org-confluence-inline-repair-apply-candidates (storage candidates)
  "Return STORAGE with repair CANDIDATES inserted as inline comment markers."
  (dolist (candidate (sort (seq-copy candidates)
			   (lambda (a b) (> (plist-get a :start) (plist-get b :start)))))
    (let* ((start (plist-get candidate :start))
	   (end (plist-get candidate :end))
	   (ref (plist-get candidate :ref))
	   (selection (substring storage start end)))
      (setq storage
	    (concat (substring storage 0 start)
		    (format "<ac:inline-comment-marker ac:ref=\"%s\">%s</ac:inline-comment-marker>"
			    (xml-escape-string ref)
			    selection)
		    (substring storage end)))))
  storage)

(defun org-confluence-inline-repair-report-buffer (page-id repaired skipped backup-file repair-file)
  "Show repair report for PAGE-ID with REPAIRED and SKIPPED records."
  (let ((buffer (get-buffer-create "*Confluence Inline Comment Repair*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
	(erase-buffer)
	(insert (format "Confluence inline comment anchor repair for page %s\n\n" page-id))
	(insert (format "Backup: %s\nRepair: %s\n\n" backup-file repair-file))
	(insert (format "* Repairable (%s)\n" (length repaired)))
	(if repaired
	    (dolist (record repaired)
	      (insert (format "- comment %s ref %s selection %S candidates %s\n"
			      (plist-get record :comment-id)
			      (plist-get record :ref)
			      (plist-get record :selection)
			      (plist-get record :candidate-count))))
	  (insert "None.\n"))
	(insert (format "\n* Skipped (%s)\n" (length skipped)))
	(if skipped
	    (dolist (record skipped)
	      (insert (format "- comment %s: %s; selection %S; candidates %s\n"
			      (plist-get record :comment-id)
			      (plist-get record :reason)
			      (plist-get record :selection)
			      (plist-get record :candidate-count))))
	  (insert "None.\n"))
	(goto-char (point-min))
	(org-mode)
	(setq buffer-read-only t)))
    (display-buffer buffer)
    buffer))

;;;###autoload
(defun org-confluence-inline-repair-comment-anchors (&optional page-id apply)
  "Repair missing Confluence inline comment markers for PAGE-ID.

The command fetches current page storage and inline comment metadata, reinserts
missing `ac:inline-comment-marker' tags when a safe target can be identified,
writes before/after XHTML files under `temporary-file-directory', and applies
the repaired storage when APPLY is non-nil or the user confirms interactively."
  (interactive (list nil current-prefix-arg))
  (let* ((id (org-confluence-comments-page-id-or-read page-id))
	 (source-file buffer-file-name)
	 (page-response (org-confluence-api--get-page id "storage"))
	 (storage (org-confluence-response-page-body-storage-value page-response))
	 (comments-response (org-confluence-api--list-page-comments id "inline-comments" "storage"))
	 (comments (org-confluence-response-comment-results comments-response))
	 repaired skipped)
    (dolist (comment comments)
      (let ((candidate (org-confluence-inline-repair-candidate-for-comment storage comment source-file)))
	(if (and (plist-member candidate :start)
		 (not (plist-get candidate :reason)))
	    (push candidate repaired)
	  (push candidate skipped))))
    (setq repaired (nreverse repaired)
	  skipped (nreverse skipped))
    (let* ((backup-file (make-temp-file "confluence-before-anchor-repair-" nil ".xhtml"))
	   (repair-file (make-temp-file "confluence-after-anchor-repair-" nil ".xhtml"))
	   (repaired-storage (org-confluence-inline-repair-apply-candidates storage repaired)))
      (with-temp-file backup-file (insert storage))
      (with-temp-file repair-file (insert repaired-storage))
      (org-confluence-inline-repair-report-buffer id repaired skipped backup-file repair-file)
      (when (and repaired
		 (or apply
		     (yes-or-no-p (format "Apply %s inline comment anchor repair(s) to page %s? "
					  (length repaired) id))))
	(org-confluence-inline-repair-run
	 (org-confluence-api--page-update-command id repair-file))
	(message "Repaired %s Confluence inline comment anchor(s) on page %s"
		 (length repaired) id))
      (list :repaired repaired
	    :skipped skipped
	    :backup-file backup-file
	    :repair-file repair-file))))

(provide 'org-confluence-inline-repair)
;;; org-confluence-inline-repair.el ends here
