;;; typographic-semantics.el --- Org typographic semantic inventory -*- lexical-binding: t; -*-

;;; Commentary:
;; Buffer/file audit helpers for the backend-neutral typographic semantics
;; contract documented in modules/org/typographic-semantics.md.

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'org-element)
(require 'subr-x)

(defconst hub/org-typographic-semantics--image-extensions
  '("avif" "bmp" "gif" "jpeg" "jpg" "pdf" "png" "svg" "tif" "tiff" "webp")
  "File extensions treated as embedded image candidates.")

(defun hub/org-typographic-semantics--inc (table key)
  "Increment KEY in hash TABLE."
  (puthash key (1+ (gethash key table 0)) table))

(defun hub/org-typographic-semantics--alist (table)
  "Return hash TABLE as a key-sorted alist."
  (let (result)
    (maphash (lambda (key value)
	       (push (cons key value) result))
	     table)
    (sort result (lambda (left right)
		   (string< (symbol-name (car left))
			    (symbol-name (car right)))))))

(defun hub/org-typographic-semantics--keyword-counts ()
  "Return document metadata keyword counts for the current Org buffer."
  (let ((counts (make-hash-table :test #'eq)))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^[[:space:]]*#\\+\\([A-Z_]+\\):" nil t)
	(pcase (upcase (match-string-no-properties 1))
	  ("TITLE" (hub/org-typographic-semantics--inc counts 'title))
	  ("SUBTITLE" (hub/org-typographic-semantics--inc counts 'subtitle))
	  ("AUTHOR" (hub/org-typographic-semantics--inc counts 'author))
	  ("DATE" (hub/org-typographic-semantics--inc counts 'date))
	  ("LANGUAGE" (hub/org-typographic-semantics--inc counts 'locale))
	  ("EXPORT_EYEBROW" (hub/org-typographic-semantics--inc counts 'eyebrow))
	  ("EXPORT_FOOTER_NOTE" (hub/org-typographic-semantics--inc counts 'footer-note)))))
    counts))

(defun hub/org-typographic-semantics--image-link-p (link)
  "Return non-nil when LINK is a file link to a known image extension."
  (and (equal (org-element-property :type link) "file")
       (member (downcase (or (file-name-extension
			      (org-element-property :path link))
			     ""))
	       hub/org-typographic-semantics--image-extensions)))

(defun hub/org-typographic-semantics--described-link-p (link)
  "Return non-nil when LINK has an Org description."
  (org-element-contents link))

(defun hub/org-typographic-semantics--captioned-link-p (link)
  "Return non-nil when LINK or its paragraph has an affiliated caption."
  (or (org-element-property :caption link)
      (when-let* ((parent (org-element-parent link)))
	(org-element-property :caption parent))))

(defun hub/org-typographic-semantics--note-kind-from-definition (definition)
  "Return note kind string from footnote DEFINITION metadata, or nil."
  (when-let* ((begin (org-element-property :begin definition))
	      (end (org-element-property :end definition)))
    (save-excursion
      (save-restriction
	(narrow-to-region begin end)
	(goto-char (point-min))
	(when (re-search-forward "^[[:space:]]*:HUB_NOTE_KIND:[[:space:]]*\\([^[:space:]\n]+\\)" nil t)
	  (downcase (match-string-no-properties 1)))))))

(defun hub/org-typographic-semantics--audit-tree (tree)
  "Return typographic semantic counts for Org parse TREE."
  (let ((metadata (hub/org-typographic-semantics--keyword-counts))
	(inline (make-hash-table :test #'eq))
	(notes (make-hash-table :test #'eq))
	(blocks (make-hash-table :test #'eq))
	(structure (make-hash-table :test #'eq))
	(media (make-hash-table :test #'eq)))
    (org-element-map tree
		     '(bold italic underline strike-through code verbatim footnote-reference
			    timestamp link footnote-definition src-block example-block quote-block
			    special-block headline paragraph table plain-list item)
		     (lambda (element)
		       (pcase (org-element-type element)
			 ('bold (hub/org-typographic-semantics--inc inline 'bold))
			 ('italic (hub/org-typographic-semantics--inc inline 'italic))
			 ('underline (hub/org-typographic-semantics--inc inline 'underline))
			 ('strike-through (hub/org-typographic-semantics--inc inline 'strike-through))
			 ('code (hub/org-typographic-semantics--inc inline 'code))
			 ('verbatim (hub/org-typographic-semantics--inc inline 'verbatim))
			 ('footnote-reference
			  (hub/org-typographic-semantics--inc inline 'footnote-references))
			 ('timestamp
			  (pcase (org-element-property :type element)
			    (`active (hub/org-typographic-semantics--inc inline 'active-dates))
			    (`inactive (hub/org-typographic-semantics--inc inline 'inactive-dates))))
			 ('link
			  (cond
			   ((hub/org-typographic-semantics--image-link-p element)
			    (if (hub/org-typographic-semantics--described-link-p element)
				(hub/org-typographic-semantics--inc media 'described-image-links)
			      (hub/org-typographic-semantics--inc media 'standalone-images)
			      (when (hub/org-typographic-semantics--captioned-link-p element)
				(hub/org-typographic-semantics--inc media 'captioned-figures))))
			   ((or (equal (org-element-property :type element) "person")
				(and (equal (org-element-property :type element) "fuzzy")
				     (string-prefix-p "person:" (or (org-element-property :path element) ""))))
			    (hub/org-typographic-semantics--inc inline 'person-links))
			   ((or (equal (org-element-property :type element) "status")
				(and (equal (org-element-property :type element) "fuzzy")
				     (string-prefix-p "status:" (or (org-element-property :path element) ""))))
			    (hub/org-typographic-semantics--inc inline 'status-links))
			   ((member (org-element-property :type element) '("http" "https"))
			    (hub/org-typographic-semantics--inc inline 'external-links))))
			 ('footnote-definition
			  (hub/org-typographic-semantics--inc notes 'definitions)
			  (pcase (hub/org-typographic-semantics--note-kind-from-definition element)
			    ("footnote" (hub/org-typographic-semantics--inc notes 'footnote))
			    ("marginalia" (hub/org-typographic-semantics--inc notes 'marginalia))
			    (_ (hub/org-typographic-semantics--inc notes 'ordinary))))
			 ('src-block (hub/org-typographic-semantics--inc blocks 'source))
			 ('example-block (hub/org-typographic-semantics--inc blocks 'example))
			 ('quote-block (hub/org-typographic-semantics--inc blocks 'quote))
			 ('special-block
			  (pcase (downcase (or (org-element-property :type element) ""))
			    ("callout" (hub/org-typographic-semantics--inc blocks 'callout))
			    ("standfirst" (hub/org-typographic-semantics--inc blocks 'standfirst))
			    ("metric" (hub/org-typographic-semantics--inc blocks 'metric))
			    ("graph" (hub/org-typographic-semantics--inc blocks 'graph))))
			 ('headline (hub/org-typographic-semantics--inc structure 'headings))
			 ('paragraph (hub/org-typographic-semantics--inc structure 'paragraphs))
			 ('table (hub/org-typographic-semantics--inc structure 'tables))
			 ('plain-list
			  (pcase (org-element-property :type element)
			    (`ordered (hub/org-typographic-semantics--inc structure 'ordered-lists))
			    (`descriptive (hub/org-typographic-semantics--inc structure 'definition-lists))
			    (_ (hub/org-typographic-semantics--inc structure 'unordered-lists))))
			 ('item
			  (when (org-element-property :checkbox element)
			    (hub/org-typographic-semantics--inc structure 'checkboxes))))))
    (list :metadata (hub/org-typographic-semantics--alist metadata)
	  :inline (hub/org-typographic-semantics--alist inline)
	  :notes (hub/org-typographic-semantics--alist notes)
	  :blocks (hub/org-typographic-semantics--alist blocks)
	  :structure (hub/org-typographic-semantics--alist structure)
	  :media (hub/org-typographic-semantics--alist media)
	  :diagnostics nil)))

(defun hub/org-typographic-semantics-audit-buffer ()
  "Return a typographic semantic inventory for the current Org buffer."
  (unless (derived-mode-p 'org-mode)
    (org-mode))
  (hub/org-typographic-semantics--audit-tree (org-element-parse-buffer)))

(defun hub/org-typographic-semantics-audit-file (file)
  "Return a typographic semantic inventory for Org FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (org-mode)
    (hub/org-typographic-semantics-audit-buffer)))

(provide 'org/typographic-semantics)
;;; typographic-semantics.el ends here
