;;; org-confluence-comments-backend-test.el --- Confluence comments backend tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for Confluence registration with the generic org-comments backend
;; registry.  This first slice establishes the public integration seam without
;; migrating sync behavior yet.

;;; Code:

(require 'ert)
(require 'org)
(require 'org-comments-backend)
(require 'org-comments-store)
(require 'org-confluence-comments-backend)

(ert-deftest org-confluence-comments-backend-registers-confluence-backend ()
  "The Confluence package registers an org-comments backend."
  (should (equal (org-comments-backend-name 'confluence) "Confluence"))
  (should (memq :sync (org-comments-backend-capabilities 'confluence)))
  (should (memq :push (org-comments-backend-capabilities 'confluence)))
  (should (memq :pull (org-comments-backend-capabilities 'confluence)))
  (should (memq :open-remote (org-comments-backend-capabilities 'confluence)))
  (should (featurep 'org-confluence-comments)))

(ert-deftest org-confluence-comments-backend-detects-confluence-page-id ()
  "Confluence Org metadata selects the Confluence comments backend."
  (let ((org-comments-default-backend 'org))
    (with-temp-buffer
      (org-mode)
      (insert "#+title: Source\n#+CONFLUENCE_PAGE_ID: 123\n\nBody\n")
      (should (eq (org-comments-backend-detect) 'confluence)))))

(ert-deftest org-confluence-comments-backend-does-not-detect-unlinked-org-buffer ()
  "Plain Org buffers keep the default comments backend."
  (let ((org-comments-default-backend 'org))
    (with-temp-buffer
      (org-mode)
      (insert "#+title: Source\n\nBody\n")
      (should (eq (org-comments-backend-detect) 'org)))))

(ert-deftest org-confluence-comments-backend-unsupported-operations-fail-clearly ()
  "Unsupported operations fail through the generic backend dispatcher."
  (should-error (org-comments-backend-create 'confluence '(:body "Draft"))
		:type 'user-error))

(ert-deftest org-confluence-comments-backend-sync-is-comments-only ()
  "Syncing through org-comments delegates to the comments-only helper."
  (let* ((directory (make-temp-file "org-confluence-sync" t))
	 (source-file (expand-file-name "source.org" directory))
	 called)
    (unwind-protect
	(progn
	  (with-temp-file source-file
	    (insert "#+title: Source\n#+CONFLUENCE_PAGE_ID: 123\n\nBody\n"))
	  (cl-letf (((symbol-function 'org-confluence-sync-current)
		     (lambda (&rest _args)
		       (ert-fail "comments backend sync must not call full sync")))
		    ((symbol-function 'org-confluence-sync-page-current)
		     (lambda (&rest _args)
		       (ert-fail "comments backend sync must not sync page content")))
		    ((symbol-function 'org-confluence-comments--sync)
		     (lambda (&optional page-id body-format)
		       (setq called (list :page-id page-id
					  :body-format body-format
					  :buffer-file buffer-file-name))
		       '(:comments-imported 3
					    :comments-pushed 2
					    :comment-push-errors nil))))
	    (should (equal (org-comments-backend-sync
			    'confluence
			    (list :source-file source-file
				  :page-id "123"
				  :body-format "storage"))
			   '(:comments-imported 3
						:comments-pushed 2
						:comment-push-errors nil)))))
      (should (equal called (list :page-id "123"
				  :body-format "storage"
				  :buffer-file source-file)))
      (when-let* ((source-buffer (find-buffer-visiting source-file)))
	(kill-buffer source-buffer))
      (delete-directory directory t))))

(ert-deftest org-confluence-comments-backend-open-remote-browses-comment-url ()
  "Opening a remote Confluence comment goes through the backend protocol."
  (let ((org-confluence-api-base-url "https://example.atlassian.net")
	opened)
    (cl-letf (((symbol-function 'browse-url)
	       (lambda (url &rest _args)
		 (setq opened url))))
      (should (equal (org-comments-backend-open-remote
		      'confluence
		      '(:page-id "123" :remote-id "c456" :space "ENG"))
		     "https://example.atlassian.net/wiki/spaces/ENG/pages/123?focusedCommentId=c456")))
    (should (equal opened "https://example.atlassian.net/wiki/spaces/ENG/pages/123?focusedCommentId=c456"))))

(ert-deftest org-confluence-comments-backend-open-remote-uses-source-metadata ()
  "Confluence remote open derives page metadata from a source file when needed."
  (let* ((directory (make-temp-file "org-confluence-open-remote" t))
	 (source-file (expand-file-name "source.org" directory))
	 (org-confluence-api-base-url "https://example.atlassian.net")
	 opened)
    (unwind-protect
	(progn
	  (with-temp-file source-file
	    (insert "#+title: Source\n#+CONFLUENCE_PAGE_ID: 123\n#+CONFLUENCE_SPACE: ENG\n\nBody\n"))
	  (cl-letf (((symbol-function 'browse-url)
		     (lambda (url &rest _args)
		       (setq opened url))))
	    (should (equal (org-comments-backend-open-remote
			    'confluence
			    (list :remote-id "c456" :source-file source-file))
			   "https://example.atlassian.net/wiki/spaces/ENG/pages/123?focusedCommentId=c456")))
	  (should (equal opened "https://example.atlassian.net/wiki/spaces/ENG/pages/123?focusedCommentId=c456")))
      (when-let* ((buffer (find-buffer-visiting source-file)))
	(kill-buffer buffer))
      (delete-directory directory t))))

(ert-deftest org-confluence-comments-backend-open-remote-requires-page-and-comment ()
  "Opening a remote Confluence comment fails clearly without required metadata."
  (should-error (org-comments-backend-open-remote
		 'confluence '(:remote-id "c456" :space "ENG"))
		:type 'user-error)
  (should-error (org-comments-backend-open-remote
		 'confluence '(:page-id "123" :space "ENG"))
		:type 'user-error))

(ert-deftest org-confluence-comments-backend-pull-delegates-from-source-buffer ()
  "Pulling through the backend visits the source buffer before delegating."
  (let* ((directory (make-temp-file "org-confluence-pull" t))
	 (source-file (expand-file-name "source.org" directory))
	 called)
    (unwind-protect
	(progn
	  (with-temp-file source-file
	    (insert "#+title: Source\n#+CONFLUENCE_PAGE_ID: 123\n\nBody\n"))
	  (cl-letf (((symbol-function 'org-confluence-comments-import)
		     (lambda (&optional page-id body-format)
		       (setq called (list :page-id page-id
					  :body-format body-format
					  :buffer-file buffer-file-name))
		       2)))
	    (should (equal (org-comments-backend-pull
			    'confluence
			    (list :source-file source-file
				  :page-id "123"
				  :body-format "storage"))
			   2)))
	  (should (equal called (list :page-id "123"
				      :body-format "storage"
				      :buffer-file source-file))))
      (when-let* ((buffer (find-buffer-visiting source-file)))
	(kill-buffer buffer))
      (delete-directory directory t))))

(ert-deftest org-confluence-comments-backend-collected-records-are-normalized ()
  "Confluence-import-shaped sidecars collect as normalized collaboration records."
  (let* ((directory (make-temp-file "org-confluence-normalized" t))
	 (source-file (expand-file-name "source.org" directory))
	 (sidecar-file (org-comments-sidecar-path source-file)))
    (unwind-protect
	(progn
	  (with-temp-file source-file
	    (insert "Alpha selected text omega"))
	  (with-temp-file sidecar-file
	    (insert "#+title: Comments\n#+source: source.org\n\n")
	    (insert "* RESOLVED Remote\n")
	    (insert ":PROPERTIES:\n")
	    (insert ":ORG_COMMENTS_ID: remote-confluence-c1\n")
	    (insert ":ORG_COMMENTS_REMOTE_ID: c1\n")
	    (insert ":ORG_COMMENTS_SOURCE: confluence\n")
	    (insert ":ORG_COMMENTS_SYNC_KIND: inline\n")
	    (insert ":ORG_COMMENTS_REMOTE_AUTHOR_ID: acct-1\n")
	    (insert ":ORG_COMMENTS_REMOTE_AUTHOR_DISPLAY_NAME: Alice\n")
	    (insert ":ORG_COMMENTS_REMOTE_RESOLUTION_STATUS: resolved\n")
	    (insert ":ORG_COMMENTS_TARGET: 7 20\n")
	    (insert ":ORG_COMMENTS_TARGET_TEXT: selected text\n")
	    (insert ":END:\n\nRemote body.\n"))
	  (with-current-buffer (find-file-noselect source-file)
	    (org-mode)
	    (let ((comment (car (org-comments-collect (current-buffer) t))))
	      (should (eq (plist-get comment :backend) 'confluence))
	      (should (eq (plist-get comment :remote-state) 'present))
	      (should (plist-get comment :resolved))
	      (should (equal (plist-get comment :remote-author-name) "Alice"))
	      (should-not (plist-get comment :local-state)))))
      (when-let* ((buffer (find-buffer-visiting source-file)))
	(kill-buffer buffer))
      (delete-directory directory t))))

(ert-deftest org-confluence-comments-backend-pull-requires-source-file ()
  "Pulling Confluence comments fails clearly without a source file."
  (should-error (org-comments-backend-pull 'confluence '(:page-id "123"))
		:type 'user-error))

(ert-deftest org-confluence-comments-backend-push-delegates-from-sidecar-heading ()
  "Pushing through the backend visits the local sidecar heading before delegating."
  (let* ((directory (make-temp-file "org-confluence-push" t))
	 (sidecar-file (expand-file-name "source.comments.org" directory))
	 called)
    (unwind-protect
	(progn
	  (with-temp-file sidecar-file
	    (insert "#+title: Comments\n#+source: source.org\n\n")
	    (insert "* OPEN Comment\n:PROPERTIES:\n:ORG_COMMENTS_ID: local-1\n:END:\n\nBody\n"))
	  (cl-letf (((symbol-function 'org-confluence-comments-push-current)
		     (lambda (&optional page-id)
		       (setq called (list :page-id page-id
					  :buffer-file buffer-file-name
					  :comment-id (org-entry-get nil "ORG_COMMENTS_ID")))
		       '(:pushed t))))
	    (should (equal (org-comments-backend-push
			    'confluence
			    (list :id "local-1" :sidecar-file sidecar-file :page-id "123"))
			   '(:pushed t))))
	  (should (equal called (list :page-id "123"
				      :buffer-file sidecar-file
				      :comment-id "local-1"))))
      (when-let* ((buffer (find-buffer-visiting sidecar-file)))
	(kill-buffer buffer))
      (delete-directory directory t))))

(provide 'org-confluence-comments-backend-test)
;;; org-confluence-comments-backend-test.el ends here
