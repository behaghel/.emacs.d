;;; google-docs-styles.el --- Personal Google Docs publishing styles -*- lexical-binding: t; -*-

;;; Commentary:
;; Authoring-owned logical style definitions for Google Docs publishing.  The
;; upstream gdocs package provides neutral style primitives and defaults; this
;; module provides this configuration's visual policy.

;;; Code:

(defgroup hub/org-google-docs-styles nil
  "Personal Google Docs publishing style policy."
  :group 'org)

(defcustom hub/org-google-docs-caption-color "#666666"
  "Foreground color used for Google Docs image captions."
  :type 'string
  :group 'hub/org-google-docs-styles)

(defcustom hub/org-google-docs-code-font-family "Roboto Mono"
  "Font family used for Google Docs code text.
This is an authoring policy layered over neutral gdocs logical styles."
  :type 'string
  :group 'hub/org-google-docs-styles)

(defcustom hub/org-google-docs-code-block-background-color "#f5f5f5"
  "Background color used for Google Docs source block paragraphs."
  :type 'string
  :group 'hub/org-google-docs-styles)

(defcustom hub/org-google-docs-quote-block-background-color "#f5f5f5"
  "Background color used for Google Docs quote block paragraphs."
  :type 'string
  :group 'hub/org-google-docs-styles)

(defun hub/org-google-docs-style-definitions ()
  "Return personal logical style definitions for Google Docs publishing."
  `((gdocs-code
     :text (:font-family ,hub/org-google-docs-code-font-family))
    (gdocs-source-block
     :parent normal
     :paragraph (:spacing-mode never-collapse
			       :space-above 0
			       :space-below 0
			       :background-color ,hub/org-google-docs-code-block-background-color))
    (gdocs-source-block-first
     :parent normal
     :paragraph (:spacing-mode never-collapse
			       :space-above 6
			       :space-below 0
			       :background-color ,hub/org-google-docs-code-block-background-color))
    (gdocs-source-block-line
     :parent normal
     :paragraph (:spacing-mode never-collapse
			       :space-above 0
			       :space-below 0
			       :background-color ,hub/org-google-docs-code-block-background-color))
    (gdocs-source-block-last
     :parent normal
     :paragraph (:spacing-mode never-collapse
			       :space-above 0
			       :space-below 6
			       :background-color ,hub/org-google-docs-code-block-background-color))
    (gdocs-source-block-single
     :parent normal
     :paragraph (:spacing-mode never-collapse
			       :space-above 6
			       :space-below 6
			       :background-color ,hub/org-google-docs-code-block-background-color))
    (gdocs-quote-block
     :parent normal
     :paragraph (:spacing-mode never-collapse
			       :space-above 0
			       :space-below 0
			       :indent-start 36
			       :indent-first-line 0
			       :background-color ,hub/org-google-docs-quote-block-background-color))
    (gdocs-quote-block-first
     :parent normal
     :paragraph (:spacing-mode never-collapse
			       :space-above 6
			       :space-below 0
			       :indent-start 36
			       :indent-first-line 0
			       :background-color ,hub/org-google-docs-quote-block-background-color))
    (gdocs-quote-block-line
     :parent normal
     :paragraph (:spacing-mode never-collapse
			       :space-above 0
			       :space-below 0
			       :indent-start 36
			       :indent-first-line 0
			       :background-color ,hub/org-google-docs-quote-block-background-color))
    (gdocs-quote-block-last
     :parent normal
     :paragraph (:spacing-mode never-collapse
			       :space-above 0
			       :space-below 6
			       :indent-start 36
			       :indent-first-line 0
			       :background-color ,hub/org-google-docs-quote-block-background-color))
    (gdocs-quote-block-single
     :parent normal
     :paragraph (:spacing-mode never-collapse
			       :space-above 6
			       :space-below 6
			       :indent-start 36
			       :indent-first-line 0
			       :background-color ,hub/org-google-docs-quote-block-background-color))
    (gdocs-image-caption
     :parent normal
     :paragraph (:alignment center)
     :text (:italic t
		    :font-scale 0.85
		    :foreground-color ,hub/org-google-docs-caption-color))))

(provide 'org/google-docs-styles)
;;; google-docs-styles.el ends here
