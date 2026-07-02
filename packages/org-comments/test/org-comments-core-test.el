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

(provide 'org-comments-core-test)
;;; org-comments-core-test.el ends here
