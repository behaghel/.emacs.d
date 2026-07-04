;;; org-google-docs-semantics.el --- Google Docs semantic support classification -*- lexical-binding: t; -*-

;;; Commentary:
;; Classify a typographic semantics audit against current Google Docs adapter
;; capabilities.  This file deliberately consumes a plain audit plist so the
;; package does not depend on local hub-prefixed authoring modules.

;;; Code:

(require 'seq)
(require 'subr-x)

(defconst org-google-docs-semantics--supported
  '((inline-emphasis
     :categories ((:inline bold italic underline strike-through code verbatim))
     :reason "Google Docs supports basic rich text and literal inline text."
     :target "Preserve inline rich-text and literal text semantics.")
    (external-links
     :categories ((:inline external-links))
     :reason "Google Docs supports links with labels."
     :target "Preserve link target and display label.")
    (lists
     :categories ((:structure ordered-lists unordered-lists definition-lists checkboxes))
     :reason "Google Docs supports ordered and unordered lists; task/definition details may be renderer-specific."
     :target "Preserve list structure and readable item text.")
    (tables
     :categories ((:structure tables))
     :reason "Google Docs supports tables."
     :target "Preserve grid structure and cell text."))
  "Semantic groups currently treated as supported by Google Docs.")

(defconst org-google-docs-semantics--degraded
  '((source-blocks
     :categories ((:blocks source))
     :reason "Upstream body sync can preserve code text, but language identity and block semantics are not yet native."
     :target "Preserve source text and language identity; syntax highlighting is styling-deferred.")
    (dates
     :categories ((:inline active-dates inactive-dates))
     :reason "Dates currently survive as text, but provider-native date semantics are not mapped yet."
     :target "Pull remote dates as inactive Org timestamps by default to avoid org-agenda pollution."))
  "Semantic groups currently treated as degraded by Google Docs.")

(defconst org-google-docs-semantics--unsupported
  '((footnotes
     :categories ((:inline footnote-references))
     :reason "Org footnotes currently degrade to literal text instead of native Google Docs footnotes."
     :target "Create native Google Docs footnotes and preserve Org references/definitions on pull.")
    (standalone-images
     :categories ((:media standalone-images))
     :reason "Standalone image links currently degrade to literal file paths."
     :target "Insert local standalone images as inline Google Docs images before remote mutation.")
    (marginalia
     :categories ((:notes marginalia))
     :reason "Google Docs has no implemented mapping for repository marginalia semantics yet."
     :target "Classify marginalia explicitly; choose native/degraded behavior in a later semantic design."))
  "Semantic groups currently treated as unsupported by Google Docs.")

(defconst org-google-docs-semantics--deferred
  '((quote-blocks
     :categories ((:blocks quote))
     :reason "Quote block boundaries can be represented locally, but visible Google Docs quote styling is a styling concern."
     :target "Preserve quote boundaries on round trip; defer visible quote decoration unless a reliable semantic exists.")
    (callouts
     :categories ((:blocks callout))
     :reason "Callout panel chrome is styling/provider-specific."
     :target "Preserve admonition text and classify type/title before adding visual panel behavior.")
    (person-links
     :categories ((:inline person-links))
     :reason "Native Google Docs people smart chips/mentions need a separate provider-specific design."
     :target "Preserve person identity and display text first; smart chips are a later enhancement.")
    (document-chrome
     :categories ((:blocks standfirst metric graph) (:metadata eyebrow footer-note))
     :reason "Document chrome/layout blocks are renderer-specific styling/layout semantics."
     :target "Preserve readable content and avoid silent loss; exact rendering is a later styling/layout epic."))
  "Semantic groups currently deferred for Google Docs.")

(defun org-google-docs-semantics--category-count (audit category name)
  "Return count for NAME in AUDIT CATEGORY."
  (or (alist-get name (plist-get audit category)) 0))

(defun org-google-docs-semantics--group-count (audit categories)
  "Return total count in AUDIT across CATEGORIES.
CATEGORIES is a list of entries shaped as (CATEGORY NAME...)."
  (apply #'+
	 (mapcar (lambda (entry)
		   (let ((category (car entry))
			 (names (cdr entry)))
		     (apply #'+
			    (mapcar (lambda (name)
				      (org-google-docs-semantics--category-count
				       audit category name))
				    names))))
		 categories)))

(defun org-google-docs-semantics--entry (audit spec)
  "Return one support classification entry from AUDIT using SPEC."
  (let* ((name (car spec))
	 (plist (cdr spec))
	 (categories (plist-get plist :categories))
	 (count (org-google-docs-semantics--group-count audit categories)))
    (when (> count 0)
      (list :name name
	    :count count
	    :reason (plist-get plist :reason)
	    :target (plist-get plist :target)))))

(defun org-google-docs-semantics--entries (audit specs)
  "Return non-empty classification entries for AUDIT using SPECS."
  (delq nil (mapcar (lambda (spec)
		      (org-google-docs-semantics--entry audit spec))
		    specs)))

(defun org-google-docs-semantics-classify-audit (audit)
  "Classify typographic AUDIT against current Google Docs support.
AUDIT is a plist shaped like `hub/org-typographic-semantics-audit-buffer'
returns, but this function intentionally depends only on that plain data shape."
  (list :provider 'google-docs
	:supported (org-google-docs-semantics--entries
		    audit org-google-docs-semantics--supported)
	:degraded (org-google-docs-semantics--entries
		   audit org-google-docs-semantics--degraded)
	:unsupported (org-google-docs-semantics--entries
		      audit org-google-docs-semantics--unsupported)
	:deferred (org-google-docs-semantics--entries
		   audit org-google-docs-semantics--deferred)))

(defun org-google-docs-semantics--human-name (name)
  "Return human-readable text for semantic NAME."
  (replace-regexp-in-string "-" " " (symbol-name name)))

(defun org-google-docs-semantics--format-entry (entry)
  "Return one report line for classification ENTRY."
  (format "  - %s (%d): %s"
	  (org-google-docs-semantics--human-name (plist-get entry :name))
	  (plist-get entry :count)
	  (plist-get entry :reason)))

(defun org-google-docs-semantics--format-section (title entries)
  "Return report section TITLE for ENTRIES."
  (concat title ":\n"
	  (if entries
	      (string-join (mapcar #'org-google-docs-semantics--format-entry entries) "\n")
	    "  - none")))

(defun org-google-docs-semantics-format-report (classification)
  "Return a readable Google Docs semantic support report for CLASSIFICATION."
  (string-join
   (list "Google Docs semantic support"
	 (org-google-docs-semantics--format-section
	  "Supported" (plist-get classification :supported))
	 (org-google-docs-semantics--format-section
	  "Degraded" (plist-get classification :degraded))
	 (org-google-docs-semantics--format-section
	  "Unsupported" (plist-get classification :unsupported))
	 (org-google-docs-semantics--format-section
	  "Deferred" (plist-get classification :deferred)))
   "\n\n"))

(provide 'org-google-docs-semantics)
;;; org-google-docs-semantics.el ends here
