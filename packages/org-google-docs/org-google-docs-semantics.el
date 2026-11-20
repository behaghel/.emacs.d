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
    (footnotes
     :categories ((:inline footnote-references))
     :reason "Single-paragraph named definitions use native Google Docs footnotes with rich emphasis and links; unsupported forms fail preflight."
     :target "Preserve references, labels, emphasis and link targets in both directions; update existing native segments for footnote-only edits.")
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
     :target "Preserve grid structure and cell text.")
    (section-breaks
     :categories ((:structure section-breaks))
     :reason "Versioned separator paragraphs render a visible themed line; native horizontal-rule objects are imported explicitly."
     :target "Preserve section-break identity independently of presentation and retain surrounding prose without inherited borders.")
    (center-blocks
     :categories ((:blocks center))
     :reason "Versioned centered paragraph groups preserve block identity and rich prose."
     :target "Preserve centered paragraphs, emphasis, links and distinct blocks, including empty blocks; reject unsupported contexts before mutation.")
    (verse-blocks
     :categories ((:blocks verse))
     :reason "Versioned native verse groups preserve rich hard lines and literal spacing."
     :target "Preserve verse boundaries, line breaks, stanza gaps, indentation and authored rich text; reject unsupported content before mutation.")
    (source-blocks
     :categories ((:blocks source))
     :reason "Google Docs sync preserves source block text and language identity through semantic markers; native code block building blocks are styling-deferred until a reliable API seam exists."
     :target "Round-trip Org source block boundaries, language identity, and code text; defer syntax highlighting and native code-block visuals.")
    (example-blocks :categories ((:blocks example))
		    :reason "Marked example blocks preserve literal text and hard lines."
		    :target "Preserve preformatted example boundaries and text.")
    (standalone-images :categories ((:media standalone-images))
		       :reason "Readable standalone local images are uploaded and inserted as inline images."
		       :target "Preserve image identity and dimensions; reject missing or unsupported assets before upload.")
    (captioned-figures :categories ((:media captioned-figures))
		       :reason "Semantic caption markers associate captions with their images."
		       :target "Preserve image/caption association without guessing from appearance.")
    (quote-blocks :categories ((:blocks quote))
		  :reason "Marked quote groups preserve boundaries and authored emphasis independently of quote presentation."
		  :target "Preserve quoted paragraphs and rich inline content; attribution is a separate unsupported construct.")
    (callouts :categories ((:blocks callout))
	      :reason "Marked callouts preserve type, title, prose and supported flat lists."
	      :target "Preserve callout content and metadata; unsupported nesting fails preflight.")
    (status-links :categories ((:inline status-links))
		  :reason "Static semantic status labels preserve value, display text, colour and source protocol."
		  :target "Preserve status identity without implying native dropdown definitions.")
    (document-structure :categories ((:metadata title) (:structure headings paragraphs))
			:reason "Titles, heading hierarchy and paragraph flow have explicit conversion."
			:target "Preserve title, hierarchy and prose boundaries.")
    (note-definitions :categories ((:notes definitions))
		      :reason "Supported named definitions become native footnote segments."
		      :target "Preserve rich single-paragraph definitions; unsupported forms block before uploads."))
  "Semantic groups currently treated as supported by Google Docs.")

(defconst org-google-docs-semantics--degraded
  '((dates
     :categories ((:inline active-dates inactive-dates))
     :reason "Native UTC day-date chips preserve supported values and source activity provenance; richer source timestamps retain their complete literal value."
     :target "Pull unmarked remote dates as inactive Org timestamps; retain explicitly authored active/inactive intent.")
    (person-links :categories ((:inline person-links))
		  :reason "Person identity and display text use explicit links, not native people smart chips."
		  :target "Retain the person URI and label; native people-chip mapping is not claimed.")
    (described-image-links :categories ((:media described-image-links))
			   :reason "Described file links remain hyperlinks rather than embedded image uploads."
			   :target "Preserve target and label; local file targets are not portable embedded media.")
    (note-placement :categories ((:notes ordinary footnote))
		    :reason "Google Docs uses native bottom footnotes, not renderer-specific sidenote placement."
		    :target "Preserve the note body without promising a margin/bottom placement distinction."))
  "Semantic groups currently treated as degraded by Google Docs.")

(defconst org-google-docs-semantics--unsupported
  '((marginalia
     :categories ((:notes marginalia))
     :reason "Google Docs has no implemented mapping for repository marginalia semantics yet."
     :target "Reject marginalia before uploads rather than convert it to ordinary footnotes.")
    (quote-attributions :categories ((:blocks quote-attributions))
			:reason "Quote attribution metadata has no implemented reversible mapping."
			:target "Stop before upload; put attribution in explicit body prose or omit it deliberately.")
    (document-metadata :categories ((:metadata subtitle author date locale eyebrow footer-note))
		       :reason "These document metadata fields have no implemented Google Docs mapping."
		       :target "Stop before upload; put required values in body prose or remove the unsupported metadata.")
    (document-chrome :categories ((:blocks standfirst metric graph))
		     :reason "These semantic blocks have no implemented Google Docs representation."
		     :target "Stop before upload instead of publishing literal Org delimiters as a semantic substitute."))
  "Semantic groups currently treated as unsupported by Google Docs.")

(defconst org-google-docs-semantics--deferred nil
  "Source constructs exclusively deferred rather than supported or rejected.
Native visual enhancements do not change a source construct's disposition.")

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

(defun org-google-docs-semantics--unclassified (audit)
  "Return visible unsupported entries for undeclared counters in AUDIT."
  (let (covered entries)
    (dolist (spec (append org-google-docs-semantics--supported org-google-docs-semantics--degraded
			  org-google-docs-semantics--unsupported org-google-docs-semantics--deferred))
      (dolist (group (plist-get (cdr spec) :categories))
	(dolist (name (cdr group)) (push (cons (car group) name) covered))))
    (while audit
      (let ((category (pop audit)) (counts (pop audit)))
	(dolist (counter counts)
	  (when (and (consp counter) (numberp (cdr counter)) (> (cdr counter) 0)
		     (not (member (cons category (car counter)) covered)))
	    (push (list :name (intern (format "unclassified-%s-%s" category (car counter)))
			:category category :counter (car counter) :count (cdr counter)
			:reason "No declared Google Docs mapping for this audit counter."
			:target "Treat as unsupported until its semantic disposition is established.") entries)))))
    (nreverse entries)))

(defun org-google-docs-semantics-classify-audit (audit)
  "Classify typographic AUDIT against current Google Docs support.
AUDIT is a plist shaped like `hub/org-typographic-semantics-audit-buffer'
returns, but this function intentionally depends only on that plain data shape."
  (let ((unclassified (org-google-docs-semantics--unclassified audit)))
    (list :provider 'google-docs
	  :supported (org-google-docs-semantics--entries
		      audit org-google-docs-semantics--supported)
	  :degraded (org-google-docs-semantics--entries
		     audit org-google-docs-semantics--degraded)
	  :unsupported (append (org-google-docs-semantics--entries
				audit org-google-docs-semantics--unsupported) unclassified)
	  :unclassified unclassified
	  :deferred (org-google-docs-semantics--entries
		     audit org-google-docs-semantics--deferred))))

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
