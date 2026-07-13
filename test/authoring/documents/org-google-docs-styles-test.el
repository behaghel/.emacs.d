;;; org-google-docs-styles-test.el --- Google Docs style tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for personal Google Docs logical style policy.

;;; Code:

(require 'ert)
(require 'seq)

(defconst hub/org-google-docs-styles-test--gdocs-directory
  (let ((local "/Users/hubertbehaghel/ws/gdocs"))
    (if (file-directory-p local)
	local
      (expand-file-name "straight/repos/gdocs" default-directory)))
  "Directory containing the gdocs checkout used by style request tests.")

(when (file-directory-p hub/org-google-docs-styles-test--gdocs-directory)
  (add-to-list 'load-path hub/org-google-docs-styles-test--gdocs-directory))

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
    (should (eq (plist-get paragraph :keep-lines-together) t))
    (should-not (plist-get paragraph :keep-with-next))
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
    (should (= (plist-get (plist-get warning-label :paragraph) :space-below)
	       4))
    (should (eq (plist-get (plist-get warning-label :paragraph)
			   :keep-lines-together)
		t))
    (should (eq (plist-get (plist-get warning-label :paragraph)
			   :keep-with-next)
		t))
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
			 1)
		      (eq (alist-get 'keepLinesTogether style) t))))
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

(provide 'org-google-docs-styles-test)
;;; org-google-docs-styles-test.el ends here
