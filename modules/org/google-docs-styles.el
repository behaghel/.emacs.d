;;; google-docs-styles.el --- Personal Google Docs publishing styles -*- lexical-binding: t; -*-

;;; Commentary:
;; Authoring-owned logical style definitions for Google Docs publishing.  The
;; upstream gdocs package provides neutral style primitives and defaults; this
;; module bridges the selected local Google Docs theme to concrete
;; `gdocs-style-definitions'.

;;; Code:

(require 'org/google-docs-themes)

(defgroup hub/org-google-docs-styles nil
  "Personal Google Docs publishing style policy."
  :group 'org)

(defun hub/org-google-docs--theme-role (theme role)
  "Return ROLE from resolved Google Docs THEME."
  (hub/org-google-docs-theme-role theme role))

(defun hub/org-google-docs--theme-color (theme role)
  "Return color ROLE from resolved Google Docs THEME."
  (hub/org-google-docs-theme-color theme role))

(defun hub/org-google-docs--source-block-style-definitions (theme)
  "Return source block style definitions for THEME."
  (let ((background (hub/org-google-docs--theme-color theme 'code-surface)))
    `((gdocs-source-block
       :parent normal
       :paragraph (:spacing-mode never-collapse
				 :space-above 0
				 :space-below 0
				 :background-color ,background))
      (gdocs-source-block-first
       :parent normal
       :paragraph (:spacing-mode never-collapse
				 :space-above 6
				 :space-below 0
				 :background-color ,background))
      (gdocs-source-block-line
       :parent normal
       :paragraph (:spacing-mode never-collapse
				 :space-above 0
				 :space-below 0
				 :background-color ,background))
      (gdocs-source-block-last
       :parent normal
       :paragraph (:spacing-mode never-collapse
				 :space-above 0
				 :space-below 6
				 :background-color ,background))
      (gdocs-source-block-single
       :parent normal
       :paragraph (:spacing-mode never-collapse
				 :space-above 6
				 :space-below 6
				 :background-color ,background)))))

(defun hub/org-google-docs--quote-block-paragraph (theme space-above space-below)
  "Return quote block paragraph style for THEME and spacing values."
  `(:spacing-mode never-collapse
		  :space-above ,space-above
		  :space-below ,space-below
		  :indent-start ,(hub/org-google-docs--theme-role
				  theme 'quote-indent-start)
		  :indent-first-line 0
		  :border-padding ,(hub/org-google-docs--theme-role
				    theme 'quote-border-padding)
		  :background-color ,(hub/org-google-docs--theme-color
				      theme 'quote-surface)))

(defun hub/org-google-docs--quote-block-style-definitions (theme)
  "Return quote block style definitions for THEME."
  `((gdocs-quote-block
     :parent normal
     :paragraph ,(hub/org-google-docs--quote-block-paragraph theme 0 0)
     :text (:italic t))
    (gdocs-quote-block-first
     :parent normal
     :paragraph ,(hub/org-google-docs--quote-block-paragraph theme 6 0)
     :text (:italic t))
    (gdocs-quote-block-line
     :parent normal
     :paragraph ,(hub/org-google-docs--quote-block-paragraph theme 0 0)
     :text (:italic t))
    (gdocs-quote-block-last
     :parent normal
     :paragraph ,(hub/org-google-docs--quote-block-paragraph theme 0 6)
     :text (:italic t))
    (gdocs-quote-block-single
     :parent normal
     :paragraph ,(hub/org-google-docs--quote-block-paragraph theme 6 6)
     :text (:italic t))))

(defun hub/org-google-docs--callout-paragraph (theme space-above space-below)
  "Return callout paragraph style for THEME and spacing values."
  `(:spacing-mode never-collapse
		  :space-above ,space-above
		  :space-below ,space-below
		  :indent-start 0
		  :indent-first-line 0
		  :border-padding ,(hub/org-google-docs--theme-role
				    theme 'callout-border-padding)
		  :background-color ,(hub/org-google-docs--theme-color
				      theme 'callout-surface)))

(defun hub/org-google-docs--callout-style-definitions (theme)
  "Return Google Docs logical style definitions for callouts under THEME."
  (apply #'append
	 (mapcar
	  (lambda (type)
	    (let ((base (format "gdocs-callout-%s" type)))
	      `((,(intern (format "%s-label" base))
		 :parent normal
		 :paragraph ,(hub/org-google-docs--callout-paragraph
			      theme 10 0)
		 :text (:bold t))
		(,(intern (format "%s-first" base))
		 :parent normal
		 :paragraph ,(hub/org-google-docs--callout-paragraph theme 0 0))
		(,(intern (format "%s-line" base))
		 :parent normal
		 :paragraph ,(hub/org-google-docs--callout-paragraph theme 0 0))
		(,(intern (format "%s-last" base))
		 :parent normal
		 :paragraph ,(hub/org-google-docs--callout-paragraph theme 0 10))
		(,(intern (format "%s-single" base))
		 :parent normal
		 :paragraph ,(hub/org-google-docs--callout-paragraph
			      theme 0 10)))))
	  '("info" "note" "warning" "tip" "important"))))

(defun hub/org-google-docs-style-definitions (&optional theme-id)
  "Return logical style definitions for Google Docs publishing.
When THEME-ID is nil, use the current buffer's `#+GDOCS_THEME:' override or
`hub/org-google-docs-active-theme'."
  (let ((theme (hub/org-google-docs-current-theme theme-id)))
    (append
     `((gdocs-code
	:text (:font-family ,(hub/org-google-docs--theme-role theme 'code-font))))
     (hub/org-google-docs--source-block-style-definitions theme)
     (hub/org-google-docs--quote-block-style-definitions theme)
     (hub/org-google-docs--callout-style-definitions theme)
     `((gdocs-image-caption
	:parent normal
	:paragraph (:alignment center)
	:text (:italic t
		       :font-scale 0.85
		       :foreground-color ,(hub/org-google-docs--theme-color
					   theme 'caption-text)))))))

(provide 'org/google-docs-styles)
;;; google-docs-styles.el ends here
