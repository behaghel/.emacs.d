;;; org-google-docs-footnotes.el --- Google Docs footnote planning -*- lexical-binding: t; -*-

;;; Commentary:
;; Extract a native Google Docs footnote push plan from Org before any remote
;; mutation.  This module deliberately stops at semantic planning; applying the
;; plan to upstream gdocs/Docs API requests is a later slice.

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'org-element)
(require 'seq)
(require 'subr-x)

(defgroup org-google-docs-footnotes nil
  "Google Docs footnote semantic planning."
  :group 'org-google-docs)

(defcustom org-google-docs-footnotes-section-names '("Footnotes" "Notes de bas de page")
  "Conventional section names that may contain Google Docs footnotes.
Google Docs native footnote push v1 only supports named footnote definitions in
one of these definitions-only sections."
  :type '(repeat string)
  :group 'org-google-docs-footnotes)

(defconst org-google-docs-footnotes--rich-elements
  '(bold italic underline strike-through code verbatim link timestamp)
  "Org elements treated as rich footnote body formatting for v1 diagnostics.")

(defun org-google-docs-footnotes--diagnostic (code message &rest properties)
  "Return a diagnostic plist for CODE and MESSAGE with PROPERTIES."
  (append (list :code code :message message) properties))

(defun org-google-docs-footnotes--definition-label (definition)
  "Return footnote DEFINITION label."
  (org-element-property :label definition))

(defun org-google-docs-footnotes--reference-label (reference)
  "Return footnote REFERENCE label."
  (org-element-property :label reference))

(defun org-google-docs-footnotes--anonymous-reference-p (reference)
  "Return non-nil when REFERENCE is an anonymous inline Org footnote."
  (not (org-google-docs-footnotes--reference-label reference)))

(defun org-google-docs-footnotes--note-kind (definition)
  "Return HUB_NOTE_KIND for footnote DEFINITION, or nil."
  (when-let* ((begin (org-element-property :begin definition))
	      (end (org-element-property :end definition)))
    (save-excursion
      (save-restriction
	(narrow-to-region begin end)
	(goto-char (point-min))
	(when (re-search-forward "^[[:space:]]*:HUB_NOTE_KIND:[[:space:]]*\\([^[:space:]\n]+\\)" nil t)
	  (downcase (match-string-no-properties 1)))))))

(defun org-google-docs-footnotes--footnotes-heading-p (headline)
  "Return non-nil when HEADLINE is a conventional footnotes section."
  (member (org-element-property :raw-value headline)
	  org-google-docs-footnotes-section-names))

(defun org-google-docs-footnotes--section-info (headline)
  "Return section info plist for footnotes HEADLINE."
  (list :heading (org-element-property :raw-value headline)
	:begin (org-element-property :begin headline)
	:end (org-element-property :end headline)
	:contents-begin (org-element-property :contents-begin headline)
	:contents-end (org-element-property :contents-end headline)))

(defun org-google-docs-footnotes--inside-section-p (element section)
  "Return non-nil when ELEMENT is inside SECTION."
  (let ((begin (org-element-property :begin element))
	(end (org-element-property :end element))
	(section-begin (plist-get section :begin))
	(section-end (plist-get section :end)))
    (and begin end section-begin section-end
	 (>= begin section-begin)
	 (<= end section-end))))

(defun org-google-docs-footnotes--definitions-by-label (definitions)
  "Return hash table mapping footnote DEFINITIONS by label."
  (let ((table (make-hash-table :test #'equal)))
    (dolist (definition definitions table)
      (puthash (org-google-docs-footnotes--definition-label definition)
	       definition
	       table))))

(defun org-google-docs-footnotes--plain-text (data)
  "Return DATA rendered as plain footnote text."
  (cond
   ((null data) "")
   ((stringp data) data)
   ((and (listp data) (keywordp (car data))) "")
   ((and (listp data) (symbolp (car data)))
    (pcase (org-element-type data)
      ('plain-text data)
      ((or 'property-drawer 'drawer) "")
      ((or 'code 'verbatim) (or (org-element-property :value data) ""))
      ('line-break "\n")
      ('link
       (if (org-element-contents data)
	   (org-google-docs-footnotes--plain-text (org-element-contents data))
	 (or (org-element-property :raw-link data) "")))
      (_ (org-google-docs-footnotes--plain-text (org-element-contents data)))))
   ((listp data)
    (mapconcat #'org-google-docs-footnotes--plain-text data ""))
   (t "")))

(defun org-google-docs-footnotes--definition-body (definition)
  "Return plain text body for footnote DEFINITION."
  (let* ((label (regexp-quote (org-google-docs-footnotes--definition-label definition)))
	 (text (buffer-substring-no-properties
		(org-element-property :begin definition)
		(org-element-property :end definition))))
    (setq text (replace-regexp-in-string
		(format "\\`[[:space:]]*\\[fn:%s\\][[:space:]]*" label)
		"" text))
    (setq text (replace-regexp-in-string
		"^[[:space:]]*:PROPERTIES:[[:space:]]*\n\\(?:.*\n\\)*?[[:space:]]*:END:[[:space:]]*\n?"
		"" text))
    (setq text (replace-regexp-in-string
		"\\[\\[[^][]+\\]\\[\\([^][]+\\)\\]\\]"
		"\\1" text))
    (setq text (replace-regexp-in-string
		"\\([/*_+=~]\\)\\([^[:space:]][^\n]*?[^[:space:]]\\|[^[:space:]]\\)\\1"
		"\\2" text))
    (string-trim text)))

(defun org-google-docs-footnotes--rich-body-p (definition)
  "Return non-nil when DEFINITION contains rich inline formatting."
  (org-element-map definition org-google-docs-footnotes--rich-elements
		   (lambda (_element) t)
		   nil t))

(defun org-google-docs-footnotes--reference-counts (references)
  "Return hash table of footnote REFERENCES counts by label."
  (let ((table (make-hash-table :test #'equal)))
    (dolist (reference references table)
      (when-let* ((label (org-google-docs-footnotes--reference-label reference)))
	(puthash label (1+ (gethash label table 0)) table)))))

(defun org-google-docs-footnotes--section-mixed-content-p (section definitions)
  "Return non-nil when SECTION has authored content outside DEFINITIONS."
  (when-let* ((begin (plist-get section :contents-begin))
	      (end (plist-get section :contents-end)))
    (let ((text (buffer-substring-no-properties begin end))
	  (ranges (sort
		   (delq nil
			 (mapcar (lambda (definition)
				   (when (org-google-docs-footnotes--inside-section-p
					  definition section)
				     (cons (org-element-property :begin definition)
					   (org-element-property :end definition))))
				 definitions))
		   (lambda (left right) (> (car left) (car right))))))
      (dolist (range ranges)
	(let ((relative-begin (- (car range) begin))
	      (relative-end (- (cdr range) begin)))
	  (setq text (concat (substring text 0 relative-begin)
			     (substring text relative-end)))))
      (not (string-empty-p (string-trim text))))))

(defun org-google-docs-footnotes--collect-degradations (references definitions-by-label)
  "Return degradation diagnostics for REFERENCES using DEFINITIONS-BY-LABEL."
  (let ((counts (org-google-docs-footnotes--reference-counts references))
	degradations)
    (maphash (lambda (label count)
	       (when (> count 1)
		 (push (org-google-docs-footnotes--diagnostic
			:repeated-reference
			"Repeated Org footnote references become separate Google Docs footnotes."
			:label label :count count)
		       degradations))
	       (when-let* ((definition (gethash label definitions-by-label))
			   ((org-google-docs-footnotes--rich-body-p definition)))
		 (push (org-google-docs-footnotes--diagnostic
			:rich-inline-formatting
			"Rich inline formatting in footnote bodies is plain text in Google Docs footnote v1."
			:label label)
		       degradations)))
	     counts)
    (nreverse degradations)))

(defun org-google-docs-footnotes--collect-diagnostics
    (references definitions definitions-by-label section)
  "Return blocking diagnostics for REFERENCES and DEFINITIONS in SECTION."
  (let (diagnostics)
    (dolist (reference references)
      (cond
       ((org-google-docs-footnotes--anonymous-reference-p reference)
	(push (org-google-docs-footnotes--diagnostic
	       :anonymous-footnote
	       "Anonymous inline Org footnotes are not supported by Google Docs footnote v1.")
	      diagnostics))
       ((not (gethash (org-google-docs-footnotes--reference-label reference)
		      definitions-by-label))
	(push (org-google-docs-footnotes--diagnostic
	       :missing-definition
	       "Footnote reference has no matching definition."
	       :label (org-google-docs-footnotes--reference-label reference))
	      diagnostics))))
    (dolist (definition definitions)
      (let ((label (org-google-docs-footnotes--definition-label definition))
	    (kind (org-google-docs-footnotes--note-kind definition)))
	(cond
	 ((equal kind "marginalia")
	  (push (org-google-docs-footnotes--diagnostic
		 :unsupported-marginalia
		 "HUB_NOTE_KIND marginalia cannot be mapped to native Google Docs footnotes in v1."
		 :label label)
		diagnostics))
	 ((not (and section
		    (org-google-docs-footnotes--inside-section-p definition section)))
	  (push (org-google-docs-footnotes--diagnostic
		 :definition-outside-footnotes-section
		 "Google Docs footnote v1 requires definitions in a conventional footnotes section."
		 :label label)
		diagnostics)))))
    (when (and section
	       (org-google-docs-footnotes--section-mixed-content-p section definitions))
      (push (org-google-docs-footnotes--diagnostic
	     :mixed-footnotes-section-content
	     "The conventional footnotes section contains authored content outside footnote definitions.")
	    diagnostics))
    (nreverse diagnostics)))

(defun org-google-docs-footnotes--reference-entry (ordinal reference definitions-by-label)
  "Return native footnote plan entry for REFERENCE at ORDINAL."
  (let* ((label (org-google-docs-footnotes--reference-label reference))
	 (definition (and label (gethash label definitions-by-label))))
    (list :label label
	  :ordinal ordinal
	  :body (and definition
		     (org-google-docs-footnotes--definition-body definition))
	  :kind (and definition (org-google-docs-footnotes--note-kind definition))
	  :begin (org-element-property :begin reference)
	  :end (org-element-property :end reference))))

(defun org-google-docs-footnotes--reference-doc-index (reference)
  "Return Google Docs document index for planned footnote REFERENCE."
  (or (plist-get reference :doc-index)
      (user-error "Missing Google Docs footnote document index for label `%s'"
		  (plist-get reference :label))))

(defun org-google-docs-footnotes-create-requests (references)
  "Return native createFootnote requests for planned REFERENCES.
Each reference must contain `:doc-index', supplied by the future upstream
conversion seam that knows exact Google Docs UTF-16 insertion indices."
  (mapcar (lambda (reference)
	    (let ((index (org-google-docs-footnotes--reference-doc-index reference)))
	      `((createFootnote . ((location . ((index . ,index))))))))
	  references))

(defun org-google-docs-footnotes--response-replies (response)
  "Return batchUpdate RESPONSE replies as a list."
  (let ((replies (alist-get 'replies response)))
    (cond
     ((vectorp replies) (append replies nil))
     ((listp replies) replies)
     (t nil))))

(defun org-google-docs-footnotes--reply-footnote-id (reply)
  "Return created footnote ID from createFootnote REPLY."
  (alist-get 'footnoteId (alist-get 'createFootnote reply)))

(defun org-google-docs-footnotes-body-insert-requests (references response)
  "Return insertText requests for REFERENCES using createFootnote RESPONSE.
Google Docs returns the created footnote segment IDs from the first batchUpdate;
these requests are intended for a second batchUpdate."
  (let ((replies (org-google-docs-footnotes--response-replies response)))
    (unless (= (length references) (length replies))
      (user-error "Expected %d createFootnote replies, got %d"
		  (length references) (length replies)))
    (cl-mapcar (lambda (reference reply)
		 (let ((footnote-id (org-google-docs-footnotes--reply-footnote-id reply)))
		   (unless footnote-id
		     (user-error "Missing createFootnote footnoteId for label `%s'"
				 (plist-get reference :label)))
		   `((insertText . ((text . ,(or (plist-get reference :body) ""))
				    (location . ((segmentId . ,footnote-id)
						 (index . 1))))))))
	       references replies)))

;;;###autoload
(defun org-google-docs-footnotes-plan-buffer ()
  "Return a native Google Docs footnote push plan for the current Org buffer.
The returned plist has `:ready-p' non-nil only when blocking diagnostics are
absent.  This function does not mutate the buffer or any remote document."
  (unless (derived-mode-p 'org-mode)
    (org-mode))
  (let* ((tree (org-element-parse-buffer))
	 (references (org-element-map tree 'footnote-reference #'identity))
	 (definitions (org-element-map tree 'footnote-definition #'identity))
	 (headlines (org-element-map tree 'headline #'identity))
	 (section-headline (seq-find #'org-google-docs-footnotes--footnotes-heading-p
				     headlines))
	 (section (and section-headline
		       (org-google-docs-footnotes--section-info section-headline)))
	 (definitions-by-label (org-google-docs-footnotes--definitions-by-label definitions))
	 (diagnostics (org-google-docs-footnotes--collect-diagnostics
		       references definitions definitions-by-label section))
	 (degradations (org-google-docs-footnotes--collect-degradations
			references definitions-by-label)))
    (list :ready-p (null diagnostics)
	  :references (cl-loop for reference in references
			       for ordinal from 1
			       unless (org-google-docs-footnotes--anonymous-reference-p reference)
			       collect (org-google-docs-footnotes--reference-entry
					ordinal reference definitions-by-label))
	  :definitions-section section
	  :diagnostics diagnostics
	  :degradations degradations)))

(provide 'org-google-docs-footnotes)
;;; org-google-docs-footnotes.el ends here
