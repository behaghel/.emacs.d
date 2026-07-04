;;; org-comments-core-test.el --- Core tests for org-comments package -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for core helpers extracted from the legacy Org comments implementation.

;;; Code:

(require 'ert)
(require 'org-comments-core)

(ert-deftest org-comments-core-generates-local-comment-id ()
  "Local comment IDs keep the existing org-comments format."
  (let ((id (org-comments-generate-id)))
    (should (string-match-p "\\`local-[0-9]\\{8\\}T[0-9]\\{6\\}-[0-9a-f]\\{6\\}\\'" id))))

(ert-deftest org-comments-core-current-author-prefers-explicit-custom ()
  "Explicit comment author custom wins over other identity fallbacks."
  (let ((org-comments-author "Alice"))
    (should (equal (org-comments-current-author) "Alice"))))

(ert-deftest org-comments-import-report-format-summarizes-provider-counts ()
  "Import report formatting is provider-neutral and concise."
  (should (equal (org-comments-import-report-format
		  '(:provider "Google Docs"
			      :added 2
			      :updated 3
			      :added-replies 1
			      :updated-replies 1
			      :remote-resolved 1
			      :remote-reopened 1
			      :remote-missing 1
			      :remote-missing-replies 1
			      :remote-present 1
			      :skipped-resolved 1
			      :preserved-local t))
		 "Google Docs comments: added 2, updated 3, added replies 1, updated replies 1, remote resolved 1, remote reopened 1, remote missing 1, remote missing replies 1, remote present 1, skipped resolved 1; local content preserved")))

(ert-deftest org-comments-import-report-format-defaults-missing-counts ()
  "Import report formatting defaults missing counters to zero."
  (should (equal (org-comments-import-report-format '(:provider "Confluence"))
		 "Confluence comments: added 0, updated 0")))

(ert-deftest org-comments-sync-report-format-summarizes-provider-counts ()
  "Sync report formatting is provider-neutral and concise."
  (should (equal (org-comments-sync-report-format
		  '(:provider "Google Docs"
			      :created 1
			      :updated 1
			      :pushed 3
			      :pushed-replies 1
			      :resolved 2
			      :already-pushed 1
			      :errors 1
			      :url-copied t))
		 "Google Docs comments: created 1, updated 1, pushed 3, pushed replies 1, resolved 2, already pushed 1, errors 1; URL copied")))

(ert-deftest org-comments-sync-report-format-defaults-to-no-changes ()
  "Sync report formatting defaults to a no-change summary."
  (should (equal (org-comments-sync-report-format '(:provider "Google Docs"))
		 "Google Docs comments: no changes")))

(ert-deftest org-comments-report-format-grammar-is-provider-neutral ()
  "Import and sync report grammar differs only by provider label."
  (let ((import-counts '(:added 1 :updated 2 :added-replies 1
				:remote-missing 1 :preserved-local t))
	(sync-counts '(:created 1 :updated 2 :pushed-replies 1
				:errors 1 :url-copied t)))
    (should (equal (replace-regexp-in-string
		    "\\`Google Docs" "Provider"
		    (org-comments-import-report-format
		     (append '(:provider "Google Docs") import-counts)))
		   (replace-regexp-in-string
		    "\\`Confluence" "Provider"
		    (org-comments-import-report-format
		     (append '(:provider "Confluence") import-counts)))))
    (should (equal (replace-regexp-in-string
		    "\\`Google Docs" "Provider"
		    (org-comments-sync-report-format
		     (append '(:provider "Google Docs") sync-counts)))
		   (replace-regexp-in-string
		    "\\`Confluence" "Provider"
		    (org-comments-sync-report-format
		     (append '(:provider "Confluence") sync-counts)))))))

(ert-deftest org-comments-sync-state-label-distinguishes-local-and-remote ()
  "Sync state labels describe local-only and synced records generically."
  (should (equal (org-comments-sync-state-label '(:body "Draft")) "unsynced"))
  (should (equal (org-comments-sync-state-label '(:remote-id "r1")) "synced"))
  (should (equal (org-comments-sync-state-label '(:remote-id "r1" :local-updated-at "now"))
		 "edited locally"))
  (should (equal (org-comments-sync-state-label '(:remote-state missing)) "remote missing")))

(provide 'org-comments-core-test)
;;; org-comments-core-test.el ends here
