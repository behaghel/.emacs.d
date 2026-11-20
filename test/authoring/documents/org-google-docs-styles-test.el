;;; org-google-docs-styles-test.el --- Google Docs style tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for personal Google Docs logical style policy.

;;; Code:

(require 'ert)
(require 'seq)

(defconst hub/org-google-docs-styles-test--build-directory
  (expand-file-name (or (getenv "STRAIGHT_BUILD_PATH") "straight/build")
		    default-directory)
  "Directory containing managed package builds for style request tests.")

(defconst hub/org-google-docs-styles-test--gdocs-directory
  (let ((override (getenv "GDOCS_PATH")))
    (if (and override (not (equal override "")))
	(expand-file-name override)
      (expand-file-name "gdocs" hub/org-google-docs-styles-test--build-directory)))
  "Managed gdocs build or explicit development checkout used by style tests.")

(dolist (directory
	 (list hub/org-google-docs-styles-test--gdocs-directory
	       (expand-file-name "dash" hub/org-google-docs-styles-test--build-directory)
	       (expand-file-name "s" hub/org-google-docs-styles-test--build-directory)))
  (when (file-directory-p directory)
    (add-to-list 'load-path directory)))

(load-file "modules/org/google-docs-themes.el")
(load-file "modules/org/google-docs-styles.el")
(condition-case nil
    (require 'gdocs-convert nil 'noerror)
  (error nil))

(defun hub/org-google-docs-styles-test--require-gdocs-convert ()
  "Skip unless `gdocs-convert' is available for request generation tests."
  (unless (featurep 'gdocs-convert)
    (ert-skip "gdocs-convert is unavailable"))
  (unless (eq (plist-get (car (gdocs-convert-org-string-to-ir
			       "#+BEGIN_CALLOUT\nBody.\n#+END_CALLOUT"))
			 :type)
	      'callout-block)
    (ert-skip "gdocs-convert lacks native callout-block support")))

(defun hub/org-google-docs-styles-test--requests (org)
  "Return Google Docs requests generated from ORG with personal styles applied."
  (hub/org-google-docs-styles-test--require-gdocs-convert)
  (let ((gdocs-style-definitions (hub/org-google-docs-style-definitions)))
    (gdocs-convert-ir-to-docs-requests
     (gdocs-convert-org-string-to-ir org))))

(defun hub/org-google-docs-styles-test--insert-text-p (request text)
  "Return non-nil when REQUEST inserts TEXT."
  (equal (alist-get 'text (alist-get 'insertText request)) text))

(ert-deftest hub/org-google-docs-theme-resolves-legacy-neutral-roles ()
  "Legacy neutral theme exposes semantic roles used by style generation."
  (let ((theme (hub/org-google-docs-theme-resolve 'legacy-neutral)))
    (should (equal (hub/org-google-docs-theme-color theme 'quote-surface)
		   "#f5f5f5"))
    (should (equal (hub/org-google-docs-theme-role theme 'code-font)
		   "Roboto Mono"))))

(ert-deftest hub/org-google-docs-theme-resolves-veriff-working-roles ()
  "Veriff working theme inherits brand palette and semantic roles."
  (let ((theme (hub/org-google-docs-theme-resolve 'veriff-working)))
    (should (equal (hub/org-google-docs-theme-color theme 'body-text)
		   "#03140F"))
    (should (equal (hub/org-google-docs-theme-color theme 'heading-brand)
		   "#0C3035"))
    (should (equal (hub/org-google-docs-theme-color theme 'quote-border)
		   "#9DF5EA"))
    (should (equal (hub/org-google-docs-theme-color theme 'callout-warning-label)
		   "#42081D"))))

(ert-deftest hub/org-google-docs-buffer-theme-rejects-duplicates ()
  "Duplicate GDOCS_THEME keywords fail before style generation."
  (with-temp-buffer
    (insert "#+GDOCS_THEME: legacy-neutral\n#+GDOCS_THEME: legacy-neutral\n")
    (should-error (hub/org-google-docs-buffer-theme-id) :type 'user-error)))

(ert-deftest hub/org-google-docs-buffer-theme-rejects-unknown-theme ()
  "Unknown GDOCS_THEME values fail before style generation."
  (with-temp-buffer
    (insert "#+GDOCS_THEME: missing-theme\n")
    (should-error (hub/org-google-docs-buffer-theme-id) :type 'user-error)))

(ert-deftest hub/org-google-docs-legacy-quote-styles-preserve-visual-treatment ()
  "Legacy quote logical styles keep the pre-theme visual treatment."
  (dolist (style '(gdocs-quote-block
		   gdocs-quote-block-first
		   gdocs-quote-block-line
		   gdocs-quote-block-last
		   gdocs-quote-block-single))
    (let* ((definition (alist-get style (hub/org-google-docs-style-definitions
					 'legacy-neutral)))
	   (paragraph (plist-get definition :paragraph))
	   (text (plist-get definition :text)))
      (should (equal (plist-get paragraph :background-color) "#f5f5f5"))
      (should (= (plist-get paragraph :indent-start) 18))
      (should (= (plist-get paragraph :border-padding) 6))
      (should (eq (plist-get text :italic) t)))))

(ert-deftest hub/org-google-docs-veriff-quote-styles-use-brand-treatment ()
  "Veriff quote styles use neutral surface and teal border accents."
  (let* ((definition (alist-get 'gdocs-quote-block-single
				(hub/org-google-docs-style-definitions
				 'veriff-working)))
	 (paragraph (plist-get definition :paragraph))
	 (text (plist-get definition :text)))
    (should (equal (plist-get paragraph :background-color) "#F6FDFC"))
    (should (equal (plist-get paragraph :border-color) "#9DF5EA"))
    (should (= (plist-get paragraph :indent-start) 23.04))
    (should (= (plist-get paragraph :indent-first-line) 23.04))
    (should (= (plist-get (cdr (cadr (plist-get paragraph :borders))) :width) 1))
    (should (= (plist-get (cdr (car (plist-get paragraph :borders))) :width) 4))
    (should (equal (plist-get text :font-family) "Inter"))
    (should (eq (plist-get text :italic) t))))

(ert-deftest hub/org-google-docs-veriff-callout-styles-use-semantic-colors ()
  "Veriff callout styles use semantic surfaces, borders, and label colors."
  (let* ((definitions (hub/org-google-docs-style-definitions 'veriff-working))
	 (warning-label (alist-get 'gdocs-callout-warning-label definitions))
	 (warning-body (alist-get 'gdocs-callout-warning-single definitions))
	 (tip-label (alist-get 'gdocs-callout-tip-label definitions))
	 (important-label (alist-get 'gdocs-callout-important-label definitions)))
    (should (= (plist-get (plist-get warning-label :paragraph) :space-below) 0))
    (should (= (plist-get (cdr (assq 'top (plist-get (plist-get warning-body :paragraph) :borders))) :padding) 4))
    (should (equal (plist-get (plist-get warning-label :paragraph)
			      :background-color)
		   "#FBEDE8"))
    (should (equal (plist-get (plist-get warning-label :paragraph)
			      :border-color)
		   "#FBEDE8"))
    (should (equal (plist-get (plist-get warning-label :text)
			      :foreground-color)
		   "#42081D"))
    (should (equal (plist-get (plist-get warning-body :text)
			      :foreground-color)
		   "#03140F"))
    (should (equal (plist-get (plist-get warning-body :text) :font-family)
		   "Inter"))
    (should (equal (plist-get (plist-get tip-label :paragraph)
			      :background-color)
		   "#FAFEEA"))
    (should (equal (plist-get (plist-get important-label :paragraph)
			      :border-color)
		   "#FF550F"))))

(ert-deftest hub/org-google-docs-body-and-heading-requests-use-veriff-theme ()
  "Body and heading requests use Inter and Veriff heading colors."
  (let ((requests (hub/org-google-docs-styles-test--requests
		   "#+TITLE: Branded doc\n\n* Heading one\n\nBody text.")))
    (should (seq-some
	     (lambda (request)
	       (let* ((update (alist-get 'updateTextStyle request))
		      (style (alist-get 'textStyle update))
		      (font (alist-get 'weightedFontFamily style)))
		 (and (equal (alist-get 'fontFamily font) "Inter")
		      (= (or (alist-get 'weight font) 0) 800)
		      (alist-get 'foregroundColor style))))
	     requests))
    (should (seq-some
	     (lambda (request)
	       (let* ((update (alist-get 'updateTextStyle request))
		      (style (alist-get 'textStyle update))
		      (font (alist-get 'weightedFontFamily style)))
		 (and (equal (alist-get 'fontFamily font) "Inter")
		      (not (alist-get 'weight font))
		      (alist-get 'foregroundColor style))))
	     requests))))

(ert-deftest hub/org-google-docs-titled-callout-request-includes-title ()
  "Titled callouts render generated label text with title in Docs requests."
  (let ((requests (hub/org-google-docs-styles-test--requests
		   "#+ATTR_CALLOUT: :type tip :title \"Composition\"\n#+BEGIN_CALLOUT\nBody.\n#+END_CALLOUT")))
    (should (seq-some
	     (lambda (request)
	       (hub/org-google-docs-styles-test--insert-text-p
		request "💡 Tip — Composition\n"))
	     requests))))

(ert-deftest hub/org-google-docs-quote-requests-keep-visual-treatment ()
  "Quote requests include background, indentation, padding, and italic text."
  (let ((requests (hub/org-google-docs-styles-test--requests
		   "#+BEGIN_QUOTE\nQuoted.\n#+END_QUOTE")))
    (should (seq-some
	     (lambda (request)
	       (let* ((update (alist-get 'updateParagraphStyle request))
		      (style (alist-get 'paragraphStyle update))
		      (indent (alist-get 'indentStart style)))
		 (and (alist-get 'shading style)
		      (= (alist-get 'magnitude indent) 23.04)
		      (= (alist-get 'magnitude
				    (alist-get 'indentFirstLine style))
			 23.04)
		      (= (alist-get 'magnitude
				    (alist-get 'width (alist-get 'borderLeft style)))
			 4)
		      (= (alist-get 'magnitude
				    (alist-get 'width (alist-get 'borderTop style)))
			 1))))
	     requests))
    (should (seq-some
	     (lambda (request)
	       (let* ((update (alist-get 'updateTextStyle request))
		      (style (alist-get 'textStyle update)))
		 (and (equal (alist-get 'fontFamily
					(alist-get 'weightedFontFamily style))
			     "Inter")
		      (eq (alist-get 'italic style) t))))
	     requests))))

(ert-deftest hub/org-google-docs-flow-gaps-and-compact-list-presentation ()
  "Flow paragraphs and separate lists have gaps without expanding list contents."
  (let ((definitions (hub/org-google-docs-style-definitions 'veriff-working)))
    (should (= (plist-get (plist-get (alist-get 'gdocs-list-first definitions) :paragraph) :space-above) 6))
    (should (= (plist-get (plist-get (alist-get 'gdocs-list-item definitions) :paragraph) :space-above) 0))
    (dolist (role '(line last))
      (let ((quote (plist-get (alist-get (intern (format "gdocs-quote-block-%s" role)) definitions) :paragraph))
	    (callout (plist-get (alist-get (intern (format "gdocs-callout-note-%s" role)) definitions) :paragraph)))
	(should (= (plist-get quote :space-above) 6))
	;; Internal callout gaps belong inside the shaded rectangle, not in
	;; outside paragraph spacing that can split the background surface.
	(should (= (plist-get callout :space-above) 0))
	(should (= (plist-get (cdr (assq 'top (plist-get callout :borders))) :padding) 6))))
    (dolist (role '(first line last single))
      (let ((style (alist-get (intern (format "gdocs-callout-note-%s-compact" role)) definitions)))
	(should (= (plist-get (plist-get style :paragraph) :space-above) 0))
	(should (= (plist-get (cdr (assq 'top (plist-get (plist-get style :paragraph) :borders))) :padding)
		   (if (memq role '(first single)) 4 0)))))))

(ert-deftest hub/org-google-docs-callout-full-width-surface-and-outer-spacing ()
  "Every callout paragraph has a full-width surface and bounded outer margins."
  (let ((definitions (hub/org-google-docs-style-definitions 'veriff-working)))
    (dolist (role '(label first line last single first-compact line-compact last-compact single-compact))
      (let ((paragraph (plist-get (alist-get (intern (format "gdocs-callout-note-%s" role)) definitions) :paragraph)))
	(should (stringp (plist-get paragraph :background-color)))
	(let* ((borders (plist-get paragraph :borders))
	       (left (or (plist-get (cdr (assq 'left borders)) :padding) (plist-get paragraph :border-padding)))
	       (right (or (plist-get (cdr (assq 'right borders)) :padding) (plist-get paragraph :border-padding))))
	  ;; Keep the padded surface inside, not beyond, printing margins.
	  (should (= (- (min (plist-get paragraph :indent-start)
			     (plist-get paragraph :indent-first-line)) left) 0))
	  (should (= (- (plist-get paragraph :indent-end) right) 0)))))
    (should (= (plist-get (plist-get (alist-get 'gdocs-callout-note-label definitions) :paragraph) :space-above) 10))
    (dolist (role '(last single last-compact single-compact))
      (should (= (plist-get (plist-get (alist-get (intern (format "gdocs-callout-note-%s" role)) definitions) :paragraph) :space-below) 10)))))

(ert-deftest hub/org-google-docs-callout-prose-has-a-small-left-inset ()
  "Callout labels and prose retain the configured eight-point inner inset."
  (let ((definitions (hub/org-google-docs-style-definitions 'veriff-working)))
    (dolist (role '(label first line last single first-compact line-compact last-compact single-compact))
      (let* ((paragraph (plist-get (alist-get (intern (format "gdocs-callout-note-%s" role)) definitions) :paragraph))
	     (left (plist-get (cdr (assq 'left (plist-get paragraph :borders))) :padding)))
	(should (= (plist-get paragraph :indent-start) 8))
	(should (= (plist-get paragraph :indent-first-line) 8))
	(should (= left 8))))))

(ert-deftest hub/org-google-docs-verse-lines-use-compact-body-typography ()
  "Verse roles inherit body typography without paragraph gaps between lines."
  (let* ((definitions (hub/org-google-docs-style-definitions 'veriff-working))
	 (body (plist-get (alist-get 'gdocs-body definitions) :text)))
    (dolist (role '(first line last single))
      (let* ((style (alist-get (intern (format "gdocs-verse-%s" role)) definitions))
	     (paragraph (plist-get style :paragraph)))
	(should (equal (plist-get style :text) body))
	(should (= (plist-get paragraph :space-above) (if (memq role '(first single)) 6 0)))
	(should (= (plist-get paragraph :space-below) 0))))))

(ert-deftest hub/org-google-docs-verse-outer-gap-requests-preserve-compact-lines ()
  "Multi-line and single-line verses receive an outer gap, not stanza inflation."
  (let ((requests (hub/org-google-docs-styles-test--requests
		   "Before.\n\n#+BEGIN_VERSE\none\nsecond\n#+END_VERSE\n\n#+BEGIN_VERSE\nsingle\n#+END_VERSE\n")))
    (dolist (entry '(("one\n" . 6) ("second\n" . 0) ("single\n" . 6)))
      (let* ((insert (alist-get 'insertText (seq-find (lambda (request)
							(equal (alist-get 'text (alist-get 'insertText request)) (car entry))) requests)))
	     (index (alist-get 'index (alist-get 'location insert))))
	(should index)
	(should (seq-some (lambda (request)
			    (let* ((update (alist-get 'updateParagraphStyle request))
				   (above (alist-get 'spaceAbove (alist-get 'paragraphStyle update))))
			      (and above (equal (alist-get 'startIndex (alist-get 'range update)) index)
				   (= (alist-get 'magnitude above) (cdr entry))))) requests))))))

(ert-deftest hub/org-google-docs-centered-prose-retains-body-and-paragraph-gaps ()
  "Centered prose keeps body typography, outer separation and paragraph spacing."
  (let* ((definitions (hub/org-google-docs-style-definitions 'veriff-working))
	 (body (plist-get (alist-get 'gdocs-body definitions) :text)))
    (dolist (role '(first line last single))
      (let ((style (alist-get (intern (format "gdocs-center-%s" role)) definitions)))
	(should (equal (plist-get style :text) body))
	(should (= (plist-get (plist-get style :paragraph) :space-above) 6))
	(should (= (plist-get (plist-get style :paragraph) :space-below) 0))
	(should (eq (plist-get (plist-get style :paragraph) :alignment) 'center))))))

(provide 'org-google-docs-styles-test)
;;; org-google-docs-styles-test.el ends here
