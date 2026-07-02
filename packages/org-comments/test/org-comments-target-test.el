;;; org-comments-target-test.el --- Target tests for org-comments package -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for target helpers extracted from the legacy Org comments implementation.

;;; Code:

(require 'ert)
(require 'org-comments-target)

(ert-deftest org-comments-target-normalizes-whitespace ()
  "Target normalization trims text and collapses whitespace runs."
  (should (equal (org-comments-normalize-target-text "  Alpha\n\tBeta   Gamma  ")
		 "Alpha Beta Gamma")))

(ert-deftest org-comments-target-hash-uses-normalized-text ()
  "Target hashes are stable across whitespace-only target differences."
  (should (equal (org-comments-target-hash "Alpha\nBeta")
		 (org-comments-target-hash " Alpha   Beta "))))

(provide 'org-comments-target-test)
;;; org-comments-target-test.el ends here
