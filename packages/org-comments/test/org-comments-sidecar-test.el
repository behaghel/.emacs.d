;;; org-comments-sidecar-test.el --- Sidecar tests for org-comments package -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for sidecar file and write helpers extracted from the legacy implementation.

;;; Code:

(require 'ert)
(require 'org-comments-sidecar)

(ert-deftest org-comments-sidecar-path-preserves-org-extension ()
  "Sidecar paths keep an Org extension for comments."
  (should (string-suffix-p "article.comments.org"
			   (org-comments-sidecar-path "/tmp/article.org")))
  (should (string-suffix-p "article.en.comments.org"
			   (org-comments-sidecar-path "/tmp/article.en.org"))))

(defun org-comments-sidecar-test--record (source &optional id)
  "Return a minimal sidecar record for SOURCE and optional ID."
  (list :id (or id "local-1")
	:title "Check this"
	:status "OPEN"
	:source-file source
	:author "Alice"
	:created-at "2026-06-21T19:00:00+0200"
	:sync-kind "inline"
	:target-start 1
	:target-end 6
	:target-start-line 1
	:target-start-column 0
	:target-end-line 1
	:target-end-column 5
	:target-text "Alpha"
	:body "Please review."))

(ert-deftest org-comments-sidecar-append-creates-header-and-entry ()
  "Appending a titled record creates a sidecar header and comment entry."
  (let* ((dir (make-temp-file "org-comments-sidecar-" t))
	 (source (expand-file-name "article.org" dir))
	 (sidecar (org-comments-sidecar-path source))
	 (record (org-comments-sidecar-test--record source)))
    (unwind-protect
	(progn
	  (write-region "Alpha" nil source nil 'silent)
	  (should (equal (org-comments-append-to-sidecar record) sidecar))
	  (with-temp-buffer
	    (insert-file-contents sidecar)
	    (should (search-forward "#+source: article.org" nil t))
	    (should (search-forward "* OPEN Check this" nil t))
	    (should (search-forward ":ORG_COMMENTS_ID: local-1" nil t))
	    (should (search-forward "Please review." nil t))))
      (delete-directory dir t))))

(ert-deftest org-comments-sidecar-update-anchor-mutates-existing-entry ()
  "Updating an anchor rewrites target metadata for an existing sidecar entry."
  (let* ((dir (make-temp-file "org-comments-sidecar-" t))
	 (source (expand-file-name "article.org" dir))
	 (sidecar (org-comments-sidecar-path source))
	 (record (org-comments-sidecar-test--record source))
	 (updated (list :target-start 7
			:target-end 11
			:target-start-line 2
			:target-start-column 0
			:target-end-line 2
			:target-end-column 4
			:target-text "Beta")))
    (unwind-protect
	(progn
	  (write-region "Alpha\nBeta" nil source nil 'silent)
	  (org-comments-append-to-sidecar record sidecar)
	  (org-comments-update-anchor sidecar "local-1" updated)
	  (with-temp-buffer
	    (insert-file-contents sidecar)
	    (should (search-forward ":ORG_COMMENTS_TARGET: 7 11" nil t))
	    (should (search-forward ":ORG_COMMENTS_TARGET_TEXT: Beta" nil t))))
      (delete-directory dir t))))

(ert-deftest org-comments-sidecar-status-dirty-tracks-remote-divergence ()
  "Setting local status marks remote-backed entries dirty when they diverge."
  (with-temp-buffer
    (org-mode)
    (insert "* OPEN Google comment\n")
    (insert ":PROPERTIES:\n")
    (insert ":ORG_COMMENTS_ID: google-docs:c-1\n")
    (insert ":ORG_COMMENTS_REMOTE_ID: c-1\n")
    (insert ":ORG_COMMENTS_REMOTE_RESOLUTION_STATUS: open\n")
    (insert ":END:\n\nBody.\n")
    (goto-char (point-min))
    (org-comments-entry-mark-status-dirty "RESOLVED")
    (should (equal (org-entry-get nil "ORG_COMMENTS_LOCAL_STATUS_DIRTY") "status"))
    (should-not (org-entry-get nil "ORG_COMMENTS_LOCAL_UPDATED_AT"))
    (org-entry-put nil "ORG_COMMENTS_REMOTE_RESOLUTION_STATUS" "resolved")
    (org-comments-entry-mark-status-dirty "RESOLVED")
    (should-not (org-entry-get nil "ORG_COMMENTS_LOCAL_STATUS_DIRTY"))))

(ert-deftest org-comments-sidecar-reconcile-missing-marks-filtered-remotes ()
  "Missing reconciliation marks matching remote sidecar entries only."
  (let* ((dir (make-temp-file "org-comments-sidecar-reconcile-" t))
	 (sidecar (expand-file-name "article.comments.org" dir))
	 seen)
    (unwind-protect
	(progn
	  (with-temp-file sidecar
	    (insert "#+title: Comments\n\n")
	    (insert "* OPEN Present\n:PROPERTIES:\n")
	    (insert ":ORG_COMMENTS_ID: google-docs:c-present\n")
	    (insert ":ORG_COMMENTS_BACKEND: google-docs\n")
	    (insert ":ORG_COMMENTS_REMOTE_ID: c-present\n:END:\n\n")
	    (insert "* OPEN Missing\n:PROPERTIES:\n")
	    (insert ":ORG_COMMENTS_ID: google-docs:c-missing\n")
	    (insert ":ORG_COMMENTS_BACKEND: google-docs\n")
	    (insert ":ORG_COMMENTS_REMOTE_ID: c-missing\n:END:\n\n")
	    (insert "* OPEN Other provider\n:PROPERTIES:\n")
	    (insert ":ORG_COMMENTS_ID: remote-confluence:c-other\n")
	    (insert ":ORG_COMMENTS_SOURCE: confluence\n")
	    (insert ":ORG_COMMENTS_REMOTE_ID: c-other\n:END:\n\n"))
	  (should (= 1 (org-comments-sidecar-reconcile-missing
			sidecar '("c-present")
			:backend "google-docs"
			:timestamp "2026-07-03T21:00:00+0000"
			:on-missing (lambda (remote-id newly-missing)
				      (push (list remote-id newly-missing) seen)))))
	  (with-temp-buffer
	    (insert-file-contents sidecar)
	    (org-mode)
	    (goto-char (point-min))
	    (should (search-forward ":ORG_COMMENTS_REMOTE_ID: c-missing" nil t))
	    (org-back-to-heading t)
	    (should (equal (org-entry-get nil "ORG_COMMENTS_REMOTE_STATE") "missing"))
	    (should (equal (org-entry-get nil "ORG_COMMENTS_REMOTE_MISSING_AT")
			   "2026-07-03T21:00:00+0000"))
	    (goto-char (point-min))
	    (should (search-forward ":ORG_COMMENTS_REMOTE_ID: c-other" nil t))
	    (org-back-to-heading t)
	    (should-not (org-entry-get nil "ORG_COMMENTS_REMOTE_STATE")))
	  (should (equal seen '(("c-missing" t)))))
      (delete-directory dir t))))

(ert-deftest org-comments-sidecar-remote-presence-helpers-filter-headings ()
  "Remote presence helpers honor provider filters."
  (let* ((dir (make-temp-file "org-comments-sidecar-presence-" t))
	 (sidecar (expand-file-name "article.comments.org" dir)))
    (unwind-protect
	(progn
	  (with-temp-file sidecar
	    (insert "#+title: Comments\n\n")
	    (insert "* OPEN Missing\n:PROPERTIES:\n")
	    (insert ":ORG_COMMENTS_BACKEND: google-docs\n")
	    (insert ":ORG_COMMENTS_REMOTE_ID: same\n")
	    (insert ":ORG_COMMENTS_REMOTE_STATE: missing\n:END:\n\n")
	    (insert "* OPEN Present\n:PROPERTIES:\n")
	    (insert ":ORG_COMMENTS_SOURCE: confluence\n")
	    (insert ":ORG_COMMENTS_REMOTE_ID: same\n:END:\n\n"))
	  (should (org-comments-sidecar-has-remote-p
		   sidecar "same" :source "confluence"))
	  (should-not (org-comments-sidecar-remote-missing-p
		       sidecar "same" :source "confluence"))
	  (should (org-comments-sidecar-remote-missing-p
		   sidecar "same" :backend "google-docs")))
      (delete-directory dir t))))

(ert-deftest org-comments-sidecar-goto-and-replace-body-uses-generic-helpers ()
  "Generic sidecar helpers find comments and replace machine-owned body text."
  (with-temp-buffer
    (org-mode)
    (insert "* OPEN Root\n:PROPERTIES:\n")
    (insert ":ORG_COMMENTS_ID: local-1\n")
    (insert ":ORG_COMMENTS_REMOTE_ID: r1\n")
    (insert ":ORG_COMMENTS_LOCAL_UPDATED_AT: now\n")
    (insert ":END:\n\nOld body.\n** OPEN Reply\n:PROPERTIES:\n")
    (insert ":ORG_COMMENTS_ID: reply-1\n:ORG_COMMENTS_SYNC_KIND: reply\n")
    (insert ":END:\n\nReply body.\n")
    (should (org-comments-sidecar-goto-comment '(:remote-id "r1")))
    (org-comments-sidecar-replace-entry-body "New body.")
    (org-back-to-heading t)
    (org-comments-sidecar-clear-local-body-dirty)
    (should-not (org-entry-get nil "ORG_COMMENTS_LOCAL_UPDATED_AT"))
    (goto-char (point-min))
    (let ((end (save-excursion (org-end-of-subtree t t))))
      (should (equal (org-comments-entry-body end) "New body.")))
    (should (search-forward "Reply body." nil t))))

(ert-deftest org-comments-sidecar-with-remote-heading-updates-matching-entry ()
  "Remote heading helper updates the matching filtered sidecar entry."
  (let* ((dir (make-temp-file "org-comments-sidecar-" t))
	 (sidecar (expand-file-name "article.comments.org" dir)))
    (unwind-protect
	(progn
	  (write-region
	   "* OPEN Parent\n:PROPERTIES:\n:ORG_COMMENTS_REMOTE_ID: r1\n:ORG_COMMENTS_BACKEND: google-docs\n:END:\n\n"
	   nil sidecar nil 'silent)
	  (should
	   (org-comments-sidecar-with-remote-heading
	    sidecar "r1"
	    (lambda ()
	      (org-entry-put nil "ORG_COMMENTS_REMOTE_STATE" "present")
	      t)
	    :backend "google-docs"))
	  (with-temp-buffer
	    (insert-file-contents sidecar)
	    (should (search-forward ":ORG_COMMENTS_REMOTE_STATE: present" nil t))))
      (delete-directory dir t))))

(ert-deftest org-comments-sidecar-append-child-under-remote-inserts-reply ()
  "Generic append-child helper inserts child entries under remote parents."
  (let* ((dir (make-temp-file "org-comments-sidecar-child-" t))
	 (sidecar (expand-file-name "article.comments.org" dir)))
    (unwind-protect
	(progn
	  (with-temp-file sidecar
	    (insert "#+title: Comments\n\n")
	    (insert "* OPEN Parent\n:PROPERTIES:\n")
	    (insert ":ORG_COMMENTS_REMOTE_ID: parent-1\n")
	    (insert ":ORG_COMMENTS_BACKEND: google-docs\n")
	    (insert ":END:\n\nParent body\n"))
	  (should (org-comments-sidecar-append-child-under-remote
		   sidecar "parent-1"
		   "** OPEN Reply\n:PROPERTIES:\n:ORG_COMMENTS_ID: reply-1\n:END:\n\nReply\n"
		   :backend "google-docs"))
	  (with-temp-buffer
	    (insert-file-contents sidecar)
	    (should (search-forward "** OPEN Reply" nil t))
	    (should (search-forward "Reply" nil t))))
      (delete-directory dir t))))

(ert-deftest org-comments-sidecar-stamps-properties-when-missing ()
  "Missing-only property stamping preserves existing sidecar properties."
  (with-temp-buffer
    (org-mode)
    (insert "* OPEN Remote\n:PROPERTIES:\n")
    (insert ":ORG_COMMENTS_TARGET_TEXT: Local target\n")
    (insert ":END:\n\nBody\n")
    (goto-char (point-min))
    (org-comments-sidecar-stamp-properties-when-missing
     '(("ORG_COMMENTS_TARGET_TEXT" . "Remote target")
       ("ORG_COMMENTS_TARGET_MATCH_COUNT" . "2")))
    (should (equal (org-entry-get nil "ORG_COMMENTS_TARGET_TEXT") "Local target"))
    (should (equal (org-entry-get nil "ORG_COMMENTS_TARGET_MATCH_COUNT") "2"))))

(ert-deftest org-comments-sidecar-stamps-missing-normalized-remote-metadata ()
  "Missing-only remote metadata stamping preserves existing sidecar properties."
  (with-temp-buffer
    (org-mode)
    (insert "* OPEN Remote\n:PROPERTIES:\n")
    (insert ":ORG_COMMENTS_REMOTE_AUTHOR_DISPLAY_NAME: Local Alice\n")
    (insert ":END:\n\nBody\n")
    (goto-char (point-min))
    (org-comments-sidecar-stamp-remote-metadata-when-missing
     '(:remote-author-display-name "Remote Alice"
				   :remote-author-id "acct-1"
				   :remote-created-at "2026-07-01T00:00:00Z"))
    (should (equal (org-entry-get nil "ORG_COMMENTS_REMOTE_AUTHOR_DISPLAY_NAME")
		   "Local Alice"))
    (should (equal (org-entry-get nil "ORG_COMMENTS_REMOTE_AUTHOR_ID") "acct-1"))
    (should (equal (org-entry-get nil "ORG_COMMENTS_REMOTE_CREATED_AT")
		   "2026-07-01T00:00:00Z"))))

(ert-deftest org-comments-sidecar-stamps-normalized-remote-metadata ()
  "Remote metadata stamping writes provider-neutral sidecar properties."
  (with-temp-buffer
    (org-mode)
    (insert "* OPEN Remote\n:PROPERTIES:\n")
    (insert ":ORG_COMMENTS_ID: remote-1\n")
    (insert ":ORG_COMMENTS_REMOTE_STATE: missing\n")
    (insert ":ORG_COMMENTS_REMOTE_MISSING_AT: old\n")
    (insert ":END:\n\nBody\n")
    (goto-char (point-min))
    (org-comments-sidecar-stamp-remote-metadata
     '(:backend "google-docs"
		:remote-id "r1"
		:remote-state present
		:remote-author-display-name "Alice"
		:remote-author-email "alice@example.com"
		:remote-resolution-status "open"))
    (should (equal (org-entry-get nil "ORG_COMMENTS_BACKEND") "google-docs"))
    (should (equal (org-entry-get nil "ORG_COMMENTS_REMOTE_ID") "r1"))
    (should (equal (org-entry-get nil "ORG_COMMENTS_REMOTE_AUTHOR_DISPLAY_NAME") "Alice"))
    (should-not (org-entry-get nil "ORG_COMMENTS_REMOTE_STATE"))
    (should-not (org-entry-get nil "ORG_COMMENTS_REMOTE_MISSING_AT"))))

(ert-deftest org-comments-sidecar-normalizes-status-dirty-as-pending-push ()
  "Local status dirty metadata becomes provider-neutral pending push state."
  (let ((record (org-comments--normalize-sidecar-record
		 '(:remote-id "r1" :local-status-dirty "status"))))
    (should (equal (plist-get record :local-state) '(:pending-push)))))

(ert-deftest org-comments-sidecar-delete-entry-removes-subtree ()
  "Deleting an entry removes its sidecar subtree."
  (let* ((dir (make-temp-file "org-comments-sidecar-" t))
	 (source (expand-file-name "article.org" dir))
	 (sidecar (org-comments-sidecar-path source)))
    (unwind-protect
	(progn
	  (write-region "Alpha" nil source nil 'silent)
	  (org-comments-append-to-sidecar
	   (org-comments-sidecar-test--record source "local-1") sidecar)
	  (org-comments-append-to-sidecar
	   (org-comments-sidecar-test--record source "local-2") sidecar)
	  (org-comments-delete-entry sidecar "local-1")
	  (with-temp-buffer
	    (insert-file-contents sidecar)
	    (should-not (search-forward ":ORG_COMMENTS_ID: local-1" nil t))
	    (should (search-forward ":ORG_COMMENTS_ID: local-2" nil t))))
      (delete-directory dir t))))

(ert-deftest org-comments-sidecar-parses-heading-properties-and-body ()
  "Low-level sidecar parsers read properties, target bounds, and body text."
  (with-temp-buffer
    (org-mode)
    (insert "* TODO Check this\n")
    (insert ":PROPERTIES:\n")
    (insert ":ORG_COMMENTS_ID: local-1\n")
    (insert ":ORG_COMMENTS_TARGET: 7 11\n")
    (insert ":ORG_COMMENTS_TARGET_LINES: 2:0 2:4\n")
    (insert ":END:\n\n")
    (insert "Body text.\n")
    (goto-char (point-min))
    (let ((properties (org-comments--parse-properties-at-heading))
	  (end (save-excursion (org-end-of-subtree t t))))
      (should (equal (alist-get "ORG_COMMENTS_ID" properties nil nil #'equal) "local-1"))
      (should (equal (org-comments--target-bounds properties) '(7 . 11)))
      (should (equal (org-comments--heading-status properties) "TODO"))
      (should (equal (org-comments--entry-body end) "Body text.")))))

(ert-deftest org-comments-sidecar-refresh-headings-renames-existing-entry ()
  "Refreshing headings recomputes sidecar titles from existing metadata."
  (let* ((dir (make-temp-file "org-comments-sidecar-" t))
	 (source (expand-file-name "article.org" dir))
	 (sidecar (org-comments-sidecar-path source)))
    (unwind-protect
	(progn
	  (write-region "Alpha" nil source nil 'silent)
	  (org-comments-append-to-sidecar
	   (org-comments-sidecar-test--record source "local-1") sidecar)
	  (with-temp-buffer
	    (insert-file-contents sidecar)
	    (goto-char (point-min))
	    (re-search-forward "^\\* OPEN .*$")
	    (replace-match "* OPEN Stale title")
	    (write-region (point-min) (point-max) sidecar nil 'silent))
	  (should (= 1 (org-comments-refresh-sidecar-headings sidecar)))
	  (with-temp-buffer
	    (insert-file-contents sidecar)
	    (should-not (search-forward "Stale title" nil t))
	    (should (search-forward "Alpha" nil t))
	    (should (search-forward "Please review." nil t))))
      (delete-directory dir t))))

(ert-deftest org-comments-sidecar-compact-metadata-removes-obsolete-properties ()
  "Compacting metadata removes obsolete sidecar properties."
  (with-temp-buffer
    (org-mode)
    (insert "* OPEN Check this\n")
    (insert ":PROPERTIES:\n")
    (insert ":ORG_COMMENTS_ID: local-1\n")
    (insert ":ORG_COMMENTS_TARGET_HASH: old\n")
    (insert ":ORG_COMMENTS_PARENT_ID: parent\n")
    (insert ":ORG_COMMENTS_REMOTE_TARGET_JSON: {}\n")
    (insert ":END:\n\n")
    (goto-char (point-min))
    (should (= 3 (org-comments--compact-heading-metadata)))
    (should-not (org-entry-get nil "ORG_COMMENTS_TARGET_HASH"))
    (should-not (org-entry-get nil "ORG_COMMENTS_PARENT_ID"))
    (should-not (org-entry-get nil "ORG_COMMENTS_REMOTE_TARGET_JSON"))))

(provide 'org-comments-sidecar-test)
;;; org-comments-sidecar-test.el ends here
