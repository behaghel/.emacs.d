;;; org-google-docs-preflight.el --- Fail-closed source profile -*- lexical-binding: t; -*-

;;; Commentary:
;; Reject known source semantics with no reversible Google Docs mapping before
;; assets or documents are created.  This module reads Org; it performs no I/O.

;;; Code:

(require 'org-element)
(require 'subr-x)

(defun org-google-docs-preflight-buffer ()
  "Return blocking diagnostics for unsupported semantics in the current buffer.
Inspect the complete source even when the author has narrowed the buffer.
Document metadata is not silently omitted; unsupported block semantics are not
published as literal Org delimiters.  Footnote and asset checks remain owned by
their existing preflight modules."
  (save-restriction
    (widen)
    (let ((tree (org-element-parse-buffer)) diagnostics)
      (org-element-map tree 'keyword
		       (lambda (keyword)
			 (let ((key (upcase (org-element-property :key keyword))))
			   (when (member key '("SUBTITLE" "AUTHOR" "DATE" "LANGUAGE" "EXPORT_EYEBROW" "EXPORT_FOOTER_NOTE"))
			     (push (format "Unsupported #+%s metadata; move required content into body prose or remove the keyword" key)
				   diagnostics)))))
      (org-element-map tree 'quote-block
		       (lambda (block)
			 (when (org-element-property :attr_quote block)
			   (push "Unsupported quote attribution; move attribution into explicit body prose or remove #+ATTR_QUOTE" diagnostics))))
      (org-element-map tree 'special-block
		       (lambda (block)
			 (let ((kind (upcase (or (org-element-property :type block) ""))))
			   (unless (equal kind "CALLOUT")
			     (push (format "Unsupported %s block; use supported body constructs rather than literal Org delimiters" kind)
				   diagnostics)))))
      (nreverse diagnostics))))

(provide 'org-google-docs-preflight)
;;; org-google-docs-preflight.el ends here
