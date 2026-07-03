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
			      :skipped-resolved 1
			      :preserved-local t))
		 "Google Docs comments: added 2, updated 3, skipped resolved 1; local content preserved")))

(ert-deftest org-comments-import-report-format-defaults-missing-counts ()
  "Import report formatting defaults missing counters to zero."
  (should (equal (org-comments-import-report-format '(:provider "Confluence"))
		 "Confluence comments: added 0, updated 0")))

(ert-deftest org-comments-sync-state-label-distinguishes-local-and-remote ()
  "Sync state labels describe local-only and synced records generically."
  (should (equal (org-comments-sync-state-label '(:body "Draft")) "unsynced"))
  (should (equal (org-comments-sync-state-label '(:remote-id "r1")) "synced"))
  (should (equal (org-comments-sync-state-label '(:remote-id "r1" :local-updated-at "now"))
		 "edited locally"))
  (should (equal (org-comments-sync-state-label '(:remote-state missing)) "remote missing")))

(provide 'org-comments-core-test)
;;; org-comments-core-test.el ends here
