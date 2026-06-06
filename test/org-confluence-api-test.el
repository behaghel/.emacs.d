;;; org-confluence-api-test.el --- Tests for Org Confluence cfl API wrappers -*- lexical-binding: t; -*-

;;; Commentary:
;; Behavior tests for Confluence publish command construction.

;;; Code:

(require 'ert)
(require 'org)

;; Ensure repo modules are reachable for isolated batch test runners.
(let ((root (expand-file-name ".." (file-name-directory (or load-file-name buffer-file-name)))))
  (add-to-list 'load-path (expand-file-name "modules" root))
  (add-to-list 'load-path (expand-file-name "modules/org" root))
  (add-to-list 'load-path (expand-file-name "modules/org/export-confluence" root))
  (add-to-list 'load-path (expand-file-name "lisp" root))
  (add-to-list 'load-path (expand-file-name "core" root)))

(load "api" nil 'nomessage)

(defun hub/confluence-api-test--with-org-buffer (contents thunk)
  "Run THUNK in a temporary Org buffer containing CONTENTS."
  (with-temp-buffer
    (insert contents)
    (org-mode)
    (funcall thunk)))

(ert-deftest hub/confluence-api--page-update-command ()
  "Build a cfl page update command string with storage output enabled."
  (should (equal (hub/confluence-api--page-update-command "123")
		 "cfl page edit 123 --storage")))

(ert-deftest hub/confluence-api--page-create-command ()
  "Build a cfl page create command string with space, title, and storage flag."
  (should (equal (hub/confluence-api--page-create-command "ENG" "Roadmap")
		 "cfl page create --space ENG --title Roadmap --storage")))

(ert-deftest hub/confluence-api--page-id-from-buffer ()
  "Read CONFLUENCE_PAGE_ID from the current Org buffer."
  (hub/confluence-api-test--with-org-buffer
   "#+CONFLUENCE_PAGE_ID: 123\n* Title"
   (lambda ()
     (should (equal (hub/confluence-api--page-id-from-buffer) "123")))))

(ert-deftest hub/confluence-api--space-from-buffer ()
  "Read CONFLUENCE_SPACE from the current Org buffer."
  (hub/confluence-api-test--with-org-buffer
   "#+CONFLUENCE_SPACE: ENG\n* Title"
   (lambda ()
     (should (equal (hub/confluence-api--space-from-buffer) "ENG")))))

(ert-deftest hub/confluence-api--page-update-command-with-file ()
  "Build a cfl page update command that reads XHTML from a file."
  (should (equal (hub/confluence-api--page-update-command "123" "/tmp/page.xhtml")
		 "cfl page edit 123 --file /tmp/page.xhtml --storage")))

(ert-deftest hub/confluence-api--page-create-command-with-parent ()
  "Build a cfl page create command with a parent page ID."
  (should (equal (hub/confluence-api--page-create-command "ENG" "Roadmap" nil "456")
		 "cfl page create --space ENG --title Roadmap --parent 456 --storage")))

(ert-deftest hub/confluence-api--page-create-missing-space ()
  "Signal an error when creating a page without a space key."
  (should-error (hub/confluence-api--page-create-command nil "Roadmap") :type 'user-error))

(ert-deftest hub/confluence-api--page-update-missing-id ()
  "Signal an error when updating a page without a page ID."
  (should-error (hub/confluence-api--page-update-command nil) :type 'user-error))

(provide 'org-confluence-api-test)
;;; org-confluence-api-test.el ends here
