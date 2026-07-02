;;; confluence-people-test.el --- Tests for Confluence people helpers -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for Org-backed Confluence people directory lookup and caching.

;;; Code:

(require 'ert)
(require 'test-helpers)
(require 'org-confluence-people-store)

(ert-deftest org-confluence-people-resolves-local-before-global ()
  "People lookup prefers local mappings before global people-store mappings."
  (let* ((dir (make-temp-file "hub-people-local-" t))
	 (global-dir (make-temp-file "hub-people-global-" t))
	 (org-confluence-people-store-directory global-dir))
    (unwind-protect
	(progn
	  (with-temp-file (org-confluence-people-store-global-file)
	    (insert "* Global Alice\n:PROPERTIES:\n:ORG_CONFLUENCE_ACCOUNT_ID: acct-1\n:ORG_CONFLUENCE_DISPLAY_NAME: Global Alice\n:END:\n"))
	  (with-temp-file (org-confluence-people-store-local-file dir)
	    (insert "* Local Alice\n:PROPERTIES:\n:ORG_CONFLUENCE_ACCOUNT_ID: acct-1\n:ORG_CONFLUENCE_DISPLAY_NAME: Local Alice\n:END:\n"))
	  (should (equal (org-confluence-people-resolve-account-id "acct-1" dir)
			 "Local Alice")))
      (delete-directory dir t)
      (delete-directory global-dir t))))

(ert-deftest org-confluence-people-store-cache-identity-creates-global-entry ()
  "Caching a Confluence identity creates a non-destructive global people entry."
  (let* ((global-dir (make-temp-file "hub-people-global-" t))
	 (org-confluence-people-store-directory global-dir))
    (unwind-protect
	(progn
	  (org-confluence-people-store-cache-identity "acct-1" "Alice Example" "alice@example.com")
	  (with-temp-buffer
	    (insert-file-contents (org-confluence-people-store-global-file))
	    (should (search-forward "* Alice Example" nil t))
	    (should (search-forward ":ORG_CONFLUENCE_DISPLAY_NAME: Alice Example" nil t))
	    (should (search-forward ":ORG_CONFLUENCE_ACCOUNT_ID: acct-1" nil t))
	    (should (search-forward ":ORG_CONFLUENCE_EMAIL: alice@example.com" nil t))))
      (delete-directory global-dir t))))

(ert-deftest org-confluence-people-store-update-identity-preserves-manually-edited-display-name ()
  "Resolving an identity preserves non-placeholder display names."
  (let* ((global-dir (make-temp-file "hub-people-global-" t))
	 (org-confluence-people-store-directory global-dir)
	 (people-file (org-confluence-people-store-global-file)))
    (unwind-protect
	(progn
	  (org-confluence-people-store-cache-identity "acct-1" "Manual Name")
	  (should (org-confluence-people-store-update-identity
		   people-file "acct-1" "Remote Name" "alice@example.com" "active" "UTC"))
	  (with-temp-buffer
	    (insert-file-contents people-file)
	    (should (search-forward ":ORG_CONFLUENCE_DISPLAY_NAME: Manual Name" nil t))
	    (should (search-forward ":ORG_CONFLUENCE_EMAIL: alice@example.com" nil t))))
      (delete-directory global-dir t))))

(ert-deftest org-confluence-people-store-mark-me-preserves-manual-display-name ()
  "Marking an account as me preserves existing manual display names."
  (let* ((global-dir (make-temp-file "hub-people-global-" t))
	 (org-confluence-people-store-directory global-dir)
	 (people-file (org-confluence-people-store-global-file)))
    (unwind-protect
	(progn
	  (org-confluence-people-store-cache-identity "acct-me" "Manual Name")
	  (org-confluence-people-store-mark-me "acct-me" "Remote Name" "me@example.com")
	  (with-temp-buffer
	    (insert-file-contents people-file)
	    (should (search-forward ":ORG_CONFLUENCE_DISPLAY_NAME: Manual Name" nil t))
	    (should (search-forward ":ORG_CONFLUENCE_EMAIL: me@example.com" nil t))
	    (should (search-forward ":ORG_CONFLUENCE_ME: t" nil t))))
      (delete-directory global-dir t))))

(ert-deftest org-confluence-people-store-current-user-p-reads-me-marker ()
  "Current-user lookup reads explicit people-directory me markers."
  (let* ((dir (make-temp-file "hub-people-local-" t))
	 (global-dir (make-temp-file "hub-people-global-" t))
	 (org-confluence-people-store-directory global-dir))
    (unwind-protect
	(progn
	  (with-temp-file (org-confluence-people-store-global-file)
	    (insert "* Hubert\n:PROPERTIES:\n:ORG_CONFLUENCE_ACCOUNT_ID: acct-me\n:ORG_CONFLUENCE_DISPLAY_NAME: Hubert\n:ORG_CONFLUENCE_ME: t\n:END:\n"))
	  (should (org-confluence-people-store-current-user-p "acct-me" dir))
	  (should (org-confluence-people-store-current-user-p "Hubert" dir))
	  (should-not (org-confluence-people-store-current-user-p "acct-other" dir)))
      (delete-directory dir t)
      (delete-directory global-dir t))))

(ert-deftest org-confluence-people-store-current-user-p-global-me-survives-local-entry ()
  "A local person entry without me marker does not suppress global identity."
  (let* ((dir (make-temp-file "hub-people-local-" t))
	 (global-dir (make-temp-file "hub-people-global-" t))
	 (org-confluence-people-store-directory global-dir))
    (unwind-protect
	(progn
	  (with-temp-file (org-confluence-people-store-local-file dir)
	    (insert "* Local Hubert\n:PROPERTIES:\n:ORG_CONFLUENCE_ACCOUNT_ID: acct-me\n:ORG_CONFLUENCE_DISPLAY_NAME: Local Hubert\n:END:\n"))
	  (with-temp-file (org-confluence-people-store-global-file)
	    (insert "* Global Hubert\n:PROPERTIES:\n:ORG_CONFLUENCE_ACCOUNT_ID: acct-me\n:ORG_CONFLUENCE_DISPLAY_NAME: Global Hubert\n:ORG_CONFLUENCE_ME: true\n:END:\n"))
	  (should (org-confluence-people-store-current-user-p "acct-me" dir)))
      (delete-directory dir t)
      (delete-directory global-dir t))))

(ert-deftest org-confluence-people-store-cache-identity-creates-account-id-entry ()
  "Caching an account ID without display name still creates an unresolved entry."
  (let* ((global-dir (make-temp-file "hub-people-global-" t))
	 (org-confluence-people-store-directory global-dir))
    (unwind-protect
	(progn
	  (org-confluence-people-store-cache-identity "acct-1" nil)
	  (with-temp-buffer
	    (insert-file-contents (org-confluence-people-store-global-file))
	    (should (search-forward "* acct-1" nil t))
	    (should (search-forward ":ORG_CONFLUENCE_DISPLAY_NAME: acct-1" nil t))
	    (should (search-forward ":ORG_CONFLUENCE_ACCOUNT_ID: acct-1" nil t))))
      (delete-directory global-dir t))))

(provide 'confluence-people-test)
;;; confluence-people-test.el ends here
