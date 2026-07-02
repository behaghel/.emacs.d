;;; org-comments-model-test.el --- Model tests for org-comments package -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for comment record and heading helpers extracted from the legacy implementation.

;;; Code:

(require 'ert)
(require 'org-comments-model)

(ert-deftest org-comments-model-heading-title-decodes-html-entities ()
  "Heading titles use readable body previews for HTML-ish comment text."
  (let ((record '(:id "local-1"
		      :author "Alice"
		      :target-text "Alpha"
		      :body "Use &quot;quotes&quot; here.")))
    (should (string-match-p "Use .*quotes.* here"
			    (org-comments-heading-title record)))))

(ert-deftest org-comments-model-heading-title-uses-account-id-resolver ()
  "Heading titles resolve remote author ids through the configured callback."
  (let ((record '(:id "remote-1"
		      :remote-author-id "acct-1"
		      :target-text "Alpha"
		      :body "Review."))
	(org-comments-resolve-account-id-function
	 (lambda (account-id directory)
	   (should (equal account-id "acct-1"))
	   (should (equal directory "/tmp/project/"))
	   "Alice Example")))
    (should (string-match-p "Alice Example"
			    (org-comments-heading-title record "/tmp/project/")))))

(ert-deftest org-comments-model-create-record-captures-target-metadata ()
  "Creating a record captures normalized target text and buffer positions."
  (with-temp-buffer
    (insert "Alpha selected text omega")
    (let* ((source "/tmp/article.org")
	   (start (progn (goto-char (point-min))
			 (search-forward "selected")
			 (match-beginning 0)))
	   (end (match-end 0))
	   (record (org-comments-create-record
		    source start end "Review." "local-1" "Alice" "now")))
      (should (equal (plist-get record :id) "local-1"))
      (should (equal (plist-get record :source-file) source))
      (should (equal (plist-get record :target-text) "selected"))
      (should (equal (plist-get record :target-start) start))
      (should (equal (plist-get record :target-end) end))
      (should (equal (plist-get record :sync-kind) "inline")))))

(provide 'org-comments-model-test)
;;; org-comments-model-test.el ends here
