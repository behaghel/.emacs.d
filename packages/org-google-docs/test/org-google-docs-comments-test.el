;;; org-google-docs-comments-test.el --- Google Docs comments facade tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for listing Google Docs comments through upstream gdocs API helpers.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'org)

(add-to-list 'load-path (expand-file-name ".." (file-name-directory load-file-name)))
(add-to-list 'load-path (expand-file-name "../../org-sync"
					  (file-name-directory load-file-name)))

(require 'org-google-docs-comments)

(ert-deftest org-google-docs-comments-document-id-from-property-drawer ()
  "Read the upstream gdocs document id from the file-level property drawer."
  (with-temp-buffer
    (org-mode)
    (insert ":PROPERTIES:\n:GDOCS_DOCUMENT_ID: doc-123\n:GDOCS_ACCOUNT: personal\n:END:\n\nBody\n")
    (should (equal (org-google-docs-comments-document-id) "doc-123"))
    (should (equal (org-google-docs-comments-account) "personal"))))

(ert-deftest org-google-docs-comments-document-id-from-property-drawer-after-keywords ()
  "Read gdocs metadata when Org keywords precede the file property drawer."
  (with-temp-buffer
    (org-mode)
    (insert "#+TITLE: Linked doc\n#+AUTHOR: Test\n:PROPERTIES:\n:GDOCS_DOCUMENT_ID: doc-123\n:GDOCS_ACCOUNT: personal\n:END:\n\nBody\n")
    (goto-char (point-max))
    (should (equal (org-google-docs-comments-document-id) "doc-123"))
    (should (equal (org-google-docs-comments-account) "personal"))))

(ert-deftest org-google-docs-comments-list-delegates-to-upstream-api ()
  "List comments for the linked Google Doc and normalize the response."
  (let (calls result)
    (with-temp-buffer
      (org-mode)
      (insert ":PROPERTIES:\n:GDOCS_DOCUMENT_ID: doc-123\n:GDOCS_ACCOUNT: personal\n:END:\n\nBody\n")
      (cl-letf (((symbol-function 'require)
		 (lambda (feature &optional _filename _noerror)
		   (or (eq feature 'gdocs-api)
		       (featurep feature))))
		((symbol-function 'gdocs-api-list-comments)
		 (lambda (file-id callback &optional account)
		   (push (list file-id account) calls)
		   (funcall callback
			    '(((id . "c-1")
			       (content . "Check this")
			       (resolved . :json-false)))))))
	(org-google-docs-comments-list
	 (lambda (comments)
	   (setq result comments)))
	(should (equal calls '(("doc-123" "personal"))))
	(should (equal (plist-get (car result) :remote-id) "c-1"))
	(should (equal (plist-get (car result) :body) "Check this"))))))

(ert-deftest org-google-docs-comments-list-errors-without-linked-document ()
  "Listing comments requires a linked upstream gdocs document."
  (with-temp-buffer
    (org-mode)
    (insert "#+TITLE: Plain\n\nBody\n")
    (should-error (org-google-docs-comments-list #'ignore)
		  :type 'user-error)))

(provide 'org-google-docs-comments-test)
;;; org-google-docs-comments-test.el ends here
