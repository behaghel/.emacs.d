;;; org-google-docs-comments-remote-test.el --- Google comment payload tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for normalizing Google Drive Comments API payloads.

;;; Code:

(require 'ert)

(add-to-list 'load-path (expand-file-name ".." (file-name-directory load-file-name)))

(require 'org-google-docs-comments-remote)

(defconst org-google-docs-comments-remote-test--sample
  '((id . "c-1")
    (content . "Please clarify this paragraph.")
    (quotedFileContent . ((mimeType . "text/plain")
			  (value . "ambiguous launch criteria")))
    (author . ((displayName . "Ada Lovelace")
	       (emailAddress . "ada@example.com")))
    (createdTime . "2026-07-02T10:00:00.000Z")
    (modifiedTime . "2026-07-02T11:00:00.000Z")
    (resolved . :json-false)
    (replies . [((id . "r-1")
		 (content . "I agree.")
		 (author . ((displayName . "Grace Hopper")))
		 (createdTime . "2026-07-02T10:05:00.000Z"))]))
  "Sample Google Drive comment payload.")

(ert-deftest org-google-docs-comments-remote-normalizes-root-comment ()
  "Normalize a Google comment root into the adapter plist shape."
  (let ((comment (org-google-docs-comments-remote-normalize
		  org-google-docs-comments-remote-test--sample)))
    (should (equal (plist-get comment :backend) 'google-docs))
    (should (equal (plist-get comment :remote-id) "c-1"))
    (should (equal (plist-get comment :body) "Please clarify this paragraph."))
    (should (equal (plist-get comment :target-text) "ambiguous launch criteria"))
    (should (equal (plist-get comment :author-name) "Ada Lovelace"))
    (should (equal (plist-get comment :author-email) "ada@example.com"))
    (should (equal (plist-get comment :created-at) "2026-07-02T10:00:00.000Z"))
    (should (equal (plist-get comment :updated-at) "2026-07-02T11:00:00.000Z"))
    (should (equal (plist-get comment :status) "open"))))

(ert-deftest org-google-docs-comments-remote-normalizes-replies ()
  "Normalize Google comment replies under :replies."
  (let* ((comment (org-google-docs-comments-remote-normalize
		   org-google-docs-comments-remote-test--sample))
	 (reply (car (plist-get comment :replies))))
    (should (equal (plist-get reply :remote-id) "r-1"))
    (should (equal (plist-get reply :body) "I agree."))
    (should (equal (plist-get reply :author-name) "Grace Hopper"))
    (should (equal (plist-get reply :created-at) "2026-07-02T10:05:00.000Z"))))

(ert-deftest org-google-docs-comments-remote-marks-resolved-comments ()
  "Normalize Google resolved comments to resolved status."
  (let* ((payload (cons '(resolved . t)
			(assq-delete-all 'resolved
					 (copy-tree org-google-docs-comments-remote-test--sample))))
	 (comment (org-google-docs-comments-remote-normalize payload)))
    (should (equal (plist-get comment :status) "resolved"))))

(provide 'org-google-docs-comments-remote-test)
;;; org-google-docs-comments-remote-test.el ends here
