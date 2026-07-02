;;; org-comments-links-test.el --- Link tests for org-comments package -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for org-comment link helpers extracted from the legacy implementation.

;;; Code:

(require 'ert)
(require 'org-comments-links)

(ert-deftest org-comments-links-sanitizes-description-brackets ()
  "Org comment links remove brackets from link descriptions."
  (let* ((default-directory temporary-file-directory)
	 (source (expand-file-name "article.org" temporary-file-directory))
	 (link (org-comments-make-link source "local-1" "[Review] this")))
    (should (string-match-p "\\[Review this\\]\\]\\'" link))))

(ert-deftest org-comments-links-parses-source-and-comment-id ()
  "Org comment link paths split source path and comment id."
  (let* ((dir (make-temp-file "org-comments-links-" t))
	 (source (expand-file-name "article.org" dir)))
    (unwind-protect
	(progn
	  (write-region "#+TITLE: Article\n" nil source nil 'silent)
	  (let ((default-directory dir))
	    (should (equal (org-comments--parse-link-path "article.org::local-1")
			   (cons source "local-1")))))
      (delete-directory dir t))))

(provide 'org-comments-links-test)
;;; org-comments-links-test.el ends here
