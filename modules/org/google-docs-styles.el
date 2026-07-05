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

(defun hub/org-google-docs-style-definitions ()
  "Return personal logical style definitions for Google Docs publishing."
  `((gdocs-code
     :text (:font-family ,hub/org-google-docs-code-font-family))
    (gdocs-source-block
     :parent normal
     :paragraph (:spacing-mode never-collapse
			       :space-above 0
			       :space-below 0))
    (gdocs-image-caption
     :parent normal
     :paragraph (:alignment center)
     :text (:italic t
		    :font-scale 0.85
		    :foreground-color ,hub/org-google-docs-caption-color))))

(provide 'org/google-docs-styles)
;;; google-docs-styles.el ends here
