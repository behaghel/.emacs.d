;;; org-google-docs-styles-test.el --- Google Docs style tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for personal Google Docs logical style policy.

;;; Code:

(require 'ert)

(load-file "modules/org/google-docs-styles.el")

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
      (should (equal (plist-get paragraph :background-color)
		     hub/org-google-docs-quote-block-background-color))
      (should (= (plist-get paragraph :indent-start) 18))
      (should (= (plist-get paragraph :border-padding) 6))
      (should (eq (plist-get text :italic) t)))))

(provide 'org-google-docs-styles-test)
;;; org-google-docs-styles-test.el ends here
