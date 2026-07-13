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

(defun hub/org-google-docs--base-text-style-definitions (theme)
  "Return body, title, and heading style definitions for THEME."
  (let ((body-font (hub/org-google-docs--theme-role theme 'body-font))
	(heading-font (hub/org-google-docs--theme-role theme 'heading-font))
	(body-text (hub/org-google-docs--theme-color theme 'body-text))
	(heading-brand (hub/org-google-docs--theme-color theme 'heading-brand))
	(heading-text (hub/org-google-docs--theme-color theme 'heading-text))
	(heading-muted (hub/org-google-docs--theme-color theme 'heading-muted)))
    `((gdocs-body
       :parent normal
       :text (:font-family ,body-font
			   :foreground-color ,body-text))
      (gdocs-title
       :parent title
       :text (:font-family ,heading-font
			   :font-weight 800
			   :foreground-color ,heading-brand))
      (gdocs-heading-1
       :parent heading-1
       :text (:font-family ,heading-font
			   :font-weight 800
			   :foreground-color ,heading-brand))
      (gdocs-heading-2
       :parent heading-2
       :text (:font-family ,heading-font
			   :font-weight 800
			   :foreground-color ,heading-brand))
      (gdocs-heading-3
       :parent heading-3
       :text (:font-family ,heading-font
			   :bold t
			   :foreground-color ,heading-text))
      (gdocs-heading-4
       :parent heading-4
       :text (:font-family ,heading-font
			   :bold t
			   :foreground-color ,heading-muted))
      (gdocs-heading-5
       :parent heading-5
       :text (:font-family ,heading-font
			   :bold t
			   :foreground-color ,heading-muted))
      (gdocs-heading-6
       :parent heading-6
       :text (:font-family ,heading-font
			   :bold t
			   :foreground-color ,heading-muted)))))

(defun hub/org-google-docs--source-block-style-definitions (theme)
  "Return source block style definitions for THEME."
  (let ((background (hub/org-google-docs--theme-color theme 'code-surface))
	(border (hub/org-google-docs--theme-color theme 'code-border)))
    `((gdocs-source-block
       :parent normal
       :paragraph (:spacing-mode never-collapse
				 :space-above 0
				 :space-below 0
				 :border-padding 6
				 :border-color ,border
				 :background-color ,background))
      (gdocs-source-block-first
       :parent normal
       :paragraph (:spacing-mode never-collapse
				 :space-above 6
				 :space-below 0
				 :border-padding 6
				 :border-color ,border
				 :background-color ,background))
      (gdocs-source-block-line
       :parent normal
       :paragraph (:spacing-mode never-collapse
				 :space-above 0
				 :space-below 0
				 :border-padding 6
				 :border-color ,border
				 :background-color ,background))
      (gdocs-source-block-last
       :parent normal
       :paragraph (:spacing-mode never-collapse
				 :space-above 0
				 :space-below 6
				 :border-padding 6
				 :border-color ,border
				 :background-color ,background))
      (gdocs-source-block-single
       :parent normal
       :paragraph (:spacing-mode never-collapse
				 :space-above 6
				 :space-below 6
				 :border-padding 6
				 :border-color ,border
				 :background-color ,background)))))

(defun hub/org-google-docs--quote-block-borders (theme)
  "Return quote border style for THEME."
  (when (hub/org-google-docs--theme-role theme 'quote-left-border-only)
    (let ((surface (hub/org-google-docs--theme-color theme 'quote-surface))
	  (accent (hub/org-google-docs--theme-color theme 'quote-border))
	  (padding (hub/org-google-docs--theme-role theme 'quote-border-padding))
	  (accent-width (hub/org-google-docs--theme-role theme 'quote-border-width))
	  (muted-width (hub/org-google-docs--theme-role
			theme 'quote-muted-border-width)))
      `((left :color ,accent :width ,accent-width :padding ,padding)
	(top :color ,surface :width ,muted-width :padding ,padding)
	(bottom :color ,surface :width ,muted-width :padding ,padding)
	(right :color ,surface :width ,muted-width :padding ,padding)))))

(defun hub/org-google-docs--quote-block-paragraph
    (theme space-above space-below keep-with-next)
  "Return quote block paragraph style for THEME and spacing values."
  (append
   `(:spacing-mode never-collapse
		   :space-above ,space-above
		   :space-below ,space-below
		   :keep-lines-together t
		   :keep-with-next ,keep-with-next
		   :indent-start ,(hub/org-google-docs--theme-role
				   theme 'quote-indent-start)
		   :indent-first-line ,(hub/org-google-docs--theme-role
					theme 'quote-indent-first-line)
		   :border-padding ,(hub/org-google-docs--theme-role
				     theme 'quote-border-padding)
		   :border-color ,(hub/org-google-docs--theme-color
				   theme 'quote-border)
		   :background-color ,(hub/org-google-docs--theme-color
				       theme 'quote-surface))
   (when-let* ((borders (hub/org-google-docs--quote-block-borders theme)))
     (list :borders borders))))

(defun hub/org-google-docs--quote-block-text (theme)
  "Return quote text style for THEME."
  `(:font-family ,(hub/org-google-docs--theme-role theme 'body-font)
		 :foreground-color ,(hub/org-google-docs--theme-color theme 'body-text)
		 :italic t))

(defun hub/org-google-docs--quote-block-style-definitions (theme)
  "Return quote block style definitions for THEME."
  `((gdocs-quote-block
     :parent normal
     :paragraph ,(hub/org-google-docs--quote-block-paragraph theme 0 0 nil)
     :text ,(hub/org-google-docs--quote-block-text theme))
    (gdocs-quote-block-first
     :parent normal
     :paragraph ,(hub/org-google-docs--quote-block-paragraph theme 6 0 t)
     :text ,(hub/org-google-docs--quote-block-text theme))
    (gdocs-quote-block-line
     :parent normal
     :paragraph ,(hub/org-google-docs--quote-block-paragraph theme 0 0 t)
     :text ,(hub/org-google-docs--quote-block-text theme))
    (gdocs-quote-block-last
     :parent normal
     :paragraph ,(hub/org-google-docs--quote-block-paragraph theme 0 6 nil)
     :text ,(hub/org-google-docs--quote-block-text theme))
    (gdocs-quote-block-single
     :parent normal
     :paragraph ,(hub/org-google-docs--quote-block-paragraph theme 6 6 nil)
     :text ,(hub/org-google-docs--quote-block-text theme))))

(defun hub/org-google-docs--callout-color (theme type suffix fallback)
  "Return THEME callout color for TYPE and SUFFIX, falling back to FALLBACK."
  (let ((role (intern (format "callout-%s-%s" type suffix))))
    (or (hub/org-google-docs-theme-color theme role)
	(hub/org-google-docs-theme-color theme fallback))))

(defun hub/org-google-docs--callout-paragraph
    (theme type space-above space-below keep-with-next)
  "Return callout paragraph style for THEME, TYPE, and spacing values."
  `(:spacing-mode never-collapse
		  :space-above ,space-above
		  :space-below ,space-below
		  :keep-lines-together t
		  :keep-with-next ,keep-with-next
		  :indent-start 0
		  :indent-first-line 0
		  :border-padding ,(hub/org-google-docs--theme-role
				    theme 'callout-border-padding)
		  :border-color ,(hub/org-google-docs--callout-color
				  theme type "border" 'callout-border)
		  :background-color ,(hub/org-google-docs--callout-color
				      theme type "surface" 'callout-surface)))

(defun hub/org-google-docs--callout-body-text (theme)
  "Return callout body text style for THEME."
  `(:font-family ,(hub/org-google-docs--theme-role theme 'body-font)
		 :foreground-color ,(hub/org-google-docs--theme-color theme 'body-text)))

(defun hub/org-google-docs--callout-label-text (theme type)
  "Return callout label text style for THEME and TYPE."
  `(:font-family ,(hub/org-google-docs--theme-role theme 'body-font)
		 :bold t
		 :foreground-color ,(hub/org-google-docs--callout-color
				     theme type "label" 'callout-label)))

(defun hub/org-google-docs--callout-style-definitions (theme)
  "Return Google Docs logical style definitions for callouts under THEME."
  (apply #'append
	 (mapcar
	  (lambda (type)
	    (let ((base (format "gdocs-callout-%s" type)))
	      `((,(intern (format "%s-label" base))
		 :parent normal
		 :paragraph ,(hub/org-google-docs--callout-paragraph
			      theme type 10 4 t)
		 :text ,(hub/org-google-docs--callout-label-text theme type))
		(,(intern (format "%s-first" base))
		 :parent normal
		 :paragraph ,(hub/org-google-docs--callout-paragraph
			      theme type 0 0 t)
		 :text ,(hub/org-google-docs--callout-body-text theme))
		(,(intern (format "%s-line" base))
		 :parent normal
		 :paragraph ,(hub/org-google-docs--callout-paragraph
			      theme type 0 0 t)
		 :text ,(hub/org-google-docs--callout-body-text theme))
		(,(intern (format "%s-last" base))
		 :parent normal
		 :paragraph ,(hub/org-google-docs--callout-paragraph
			      theme type 0 10 nil)
		 :text ,(hub/org-google-docs--callout-body-text theme))
		(,(intern (format "%s-single" base))
		 :parent normal
		 :paragraph ,(hub/org-google-docs--callout-paragraph
			      theme type 0 10 nil)
		 :text ,(hub/org-google-docs--callout-body-text theme)))))
	  '("info" "note" "warning" "tip" "important"))))

(defun hub/org-google-docs-style-definitions (&optional theme-id)
  "Return logical style definitions for Google Docs publishing.
When THEME-ID is nil, use the current buffer's `#+GDOCS_THEME:' override or
`hub/org-google-docs-active-theme'."
  (let ((theme (hub/org-google-docs-current-theme theme-id)))
    (append
     (hub/org-google-docs--base-text-style-definitions theme)
     `((gdocs-code
	:text (:font-family ,(hub/org-google-docs--theme-role theme 'code-font)
			    :foreground-color ,(hub/org-google-docs--theme-color
						theme 'body-text))))
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
