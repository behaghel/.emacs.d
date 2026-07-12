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

(ert-deftest hub/org-google-docs-quote-styles-keep-visual-quote-treatment ()
  "Quote logical styles keep background, indentation, padding, and italic text."
  (dolist (style '(gdocs-quote-block
		   gdocs-quote-block-first
		   gdocs-quote-block-line
		   gdocs-quote-block-last
		   gdocs-quote-block-single))
    (let* ((definition (alist-get style (hub/org-google-docs-style-definitions)))
	   (paragraph (plist-get definition :paragraph))
	   (text (plist-get definition :text)))
      (should (equal (plist-get paragraph :background-color) "#f5f5f5"))
      (should (= (plist-get paragraph :indent-start) 18))
      (should (= (plist-get paragraph :border-padding) 6))
      (should (eq (plist-get text :italic) t)))))

(ert-deftest hub/org-google-docs-titled-callout-request-includes-title ()
  "Titled callouts render generated label text with title in Docs requests."
  (let ((requests (hub/org-google-docs-styles-test--requests
		   "#+ATTR_CALLOUT: :type tip :title \"Composition\"\n#+BEGIN_CALLOUT\nBody.\n#+END_CALLOUT")))
    (should (seq-some
	     (lambda (request)
	       (hub/org-google-docs-styles-test--insert-text-p
		request "Tip — Composition\n"))
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
		      (= (alist-get 'magnitude indent) 18)
		      (alist-get 'borderTop style))))
	     requests))
    (should (seq-some
	     (lambda (request)
	       (let* ((update (alist-get 'updateTextStyle request))
		      (style (alist-get 'textStyle update)))
		 (eq (alist-get 'italic style) t)))
	     requests))))

(provide 'org-google-docs-styles-test)
;;; org-google-docs-styles-test.el ends here
