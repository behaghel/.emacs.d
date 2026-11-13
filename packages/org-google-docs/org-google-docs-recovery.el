;;; org-google-docs-recovery.el --- Explicit remote-authoritative recovery -*- lexical-binding: t; -*-

;;; Commentary:
;; Fetch a linked document without merging local edits.  Confirmation and a full
;; local backup precede replacement; local file metadata remains locally owned.

;;; Code:

(require 'org-google-docs)
(require 'org-element)

(declare-function gdocs-api-get-file-metadata "gdocs-api"
		  (file-id callback &optional account))
(declare-function gdocs-api-get-document "gdocs-api"
		  (document-id callback &optional account on-error))
(declare-function gdocs-convert-docs-json-to-ir "gdocs-convert" (json))
(declare-function gdocs-convert-ir-to-org "gdocs-convert" (ir))
(declare-function gdocs-sync--make-link-context "gdocs-sync" ())
(declare-function gdocs-sync--write-pull-backup "gdocs-sync" ())
(declare-function gdocs-sync--install-content "gdocs-sync" (org-string &optional revision-id))

(defvar gdocs-auto-push-on-save)
(defvar gdocs-auto-pull-on-open)
(defvar gdocs-convert--document-id)
(defvar gdocs-convert--link-context)

(defun org-google-docs-recovery--split-preamble (text)
  "Return TEXT as a cons of file metadata and body, without converting its body."
  (with-temp-buffer
    (insert text)
    (delay-mode-hooks (org-mode))
    (let* ((elements (org-element-contents (org-element-parse-buffer)))
	   (elements (if (eq (org-element-type (car elements)) 'section)
			 (append (org-element-contents (car elements))
				 (cdr elements))
		       elements))
	   (body (seq-find
		  (lambda (element)
		    (not (memq (org-element-type element)
			       '(keyword property-drawer comment comment-block))))
		  elements))
	   (start (if body (org-element-property :begin body) (point-max))))
      (cons (buffer-substring-no-properties (point-min) start)
	    (buffer-substring-no-properties start (point-max))))))

(defun org-google-docs-recovery--validate-content (content inline-objects &optional context)
  "Reject CONTENT the importer cannot preserve using INLINE-OBJECTS.
CONTEXT distinguishes plain table cells and footnotes from the main body."
  (when (and (eq context 'cell) (> (length content) 1))
    (user-error "Cannot replace local body: multi-paragraph table cells are unsupported"))
  (seq-doseq (element content)
    (cond
     ((assq 'paragraph element)
      (let* ((paragraph (alist-get 'paragraph element))
	     (runs (alist-get 'elements paragraph)))
	(when (and context
		   (or (alist-get 'bullet paragraph)
		       (not (member (alist-get 'namedStyleType
					       (alist-get 'paragraphStyle paragraph))
				    '(nil "NORMAL_TEXT")))))
	  (user-error "Cannot replace local body: structured %s paragraphs are unsupported" context))
	(when (alist-get 'positionedObjectIds paragraph)
	  (user-error "Cannot replace local body: positioned objects are unsupported"))
	(seq-doseq (run runs)
	  (cond
	   ((or (assq 'textRun run) (assq 'footnoteReference run)
		(assq 'dateElement run)))
	   ((assq 'inlineObjectElement run)
	    (let* ((id (alist-get 'inlineObjectId (alist-get 'inlineObjectElement run)))
		   (object (and id (alist-get (intern id) inline-objects)))
		   (embedded (alist-get 'embeddedObject
					(alist-get 'inlineObjectProperties object))))
	      (unless (and (not context) (assq 'imageProperties embedded)
			   (seq-every-p
			    (lambda (other)
			      (or (eq other run)
				  (and (assq 'textRun other)
				       (string-blank-p
					(or (alist-get 'content (alist-get 'textRun other)) "")))))
			    runs))
		(user-error "Cannot replace local body: only standalone inline images are supported"))))
	   (t (user-error "Cannot replace local body: unsupported paragraph element %S"
			  (mapcar #'car run)))))))
     ((assq 'table element)
      (when context
	(user-error "Cannot replace local body: tables inside %s are unsupported" context))
      (seq-doseq (row (alist-get 'tableRows (alist-get 'table element)))
	(seq-doseq (cell (alist-get 'tableCells row))
	  (org-google-docs-recovery--validate-content
	   (alist-get 'content cell) inline-objects 'cell))))
     ((and (not context) (assq 'sectionBreak element)
	   (zerop (or (alist-get 'startIndex element) 0))))
     (t (user-error "Cannot replace local body: unsupported structural element %S"
		    (mapcar #'car element))))))

(defun org-google-docs-recovery--validate-json (json)
  "Validate recognized body and footnote content in remote JSON before rendering."
  (unless (assq 'content (alist-get 'body json))
    (user-error "Cannot replace local body: remote response has no document body"))
  (let ((objects (alist-get 'inlineObjects json)))
    (org-google-docs-recovery--validate-content
     (alist-get 'content (alist-get 'body json)) objects)
    (dolist (footnote (alist-get 'footnotes json))
      (org-google-docs-recovery--validate-content
       (alist-get 'content (cdr footnote)) objects 'footnote))))

(defun org-google-docs-recovery--check-target (file document-id account)
  "Check the current buffer still owns FILE, DOCUMENT-ID, and ACCOUNT."
  (unless (and (equal buffer-file-name file)
	       (equal (org-google-docs--current-document-id) document-id)
	       (equal (org-google-docs--current-gdocs-account) account))
    (user-error "Google Docs replacement cancelled: source link changed"))
  (barf-if-buffer-read-only))

(defun org-google-docs-recovery--apply (json revision source file document-id account)
  "Validate JSON, confirm, and replace SOURCE using REVISION.
FILE, DOCUMENT-ID, and ACCOUNT identify the original linked document."
  (when (buffer-live-p source)
    (with-current-buffer source
      (save-restriction
	(widen)
	(org-google-docs-recovery--check-target file document-id account)
	(org-google-docs-recovery--validate-json json)
	(let* ((gdocs-convert--document-id document-id)
	       (gdocs-convert--link-context (gdocs-sync--make-link-context))
	       (remote (gdocs-convert-ir-to-org (gdocs-convert-docs-json-to-ir json)))
	       (body (cdr (org-google-docs-recovery--split-preamble remote))))
	  (if (not (yes-or-no-p
		    (format "Replace local body of %s from Google Docs (backup first)? "
			    (file-name-nondirectory file))))
	      (message "Google Docs replacement cancelled; local content unchanged")
	    (let* ((preamble (car (org-google-docs-recovery--split-preamble (buffer-string))))
		   (backup (gdocs-sync--write-pull-backup))
		   (gdocs-auto-push-on-save nil)
		   (gdocs-auto-pull-on-open nil)
		   (before-save-hook nil)
		   (after-save-hook nil))
	      (gdocs-sync--install-content (concat preamble body) revision)
	      (message "Replaced local body from Google Docs. Backup: %s" backup))))))))

;;;###autoload
(defun org-google-docs-pull-replace-local ()
  "Replace the local body from Google Docs after confirmation and a full backup.
Do not merge local edits or write to Google.  Preserve file-level metadata.
Editing during fetch is allowed; the backup includes the current buffer,
including unsaved edits, when replacement is confirmed."
  (interactive)
  (unless (and (derived-mode-p 'org-mode) buffer-file-name)
    (user-error "Visit a linked Org file before replacing its body"))
  (barf-if-buffer-read-only)
  (org-google-docs--require-upstream-library 'gdocs-sync)
  (let ((source (current-buffer))
	(file buffer-file-name)
	(document-id (org-google-docs--current-document-id))
	(account (org-google-docs--current-gdocs-account)))
    (unless document-id (user-error "Buffer is not linked to a Google Doc"))
    (org-google-docs-images-deactivate-session)
    (org-google-docs-footnotes--deactivate-session)
    (message "Fetching Google Docs version for local replacement")
    (gdocs-api-get-file-metadata
     document-id
     (lambda (metadata)
       (when (buffer-live-p source)
	 (gdocs-api-get-document
	  document-id
	  (lambda (json)
	    (org-google-docs-recovery--apply
	     json (alist-get 'headRevisionId metadata)
	     source file document-id account))
	  account
	  (lambda (_error)
	    (message "Google Docs replacement fetch failed; local content unchanged")))))
     account)))

(provide 'org-google-docs-recovery)
;;; org-google-docs-recovery.el ends here
