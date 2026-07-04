;;; org-google-docs-comments-backend-test.el --- Google Docs comments backend tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for registering Google Docs with the generic org-comments backend.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'org)
(require 'org-comments-backend)

(add-to-list 'load-path (expand-file-name ".." (file-name-directory load-file-name)))
(add-to-list 'load-path (expand-file-name "../../org-sync"
					  (file-name-directory load-file-name)))
(add-to-list 'load-path (expand-file-name "../../org-comments" (file-name-directory load-file-name)))

(require 'org-google-docs-comments-backend)

(defvar gdocs-api--drive-base-url)

(ert-deftest org-google-docs-comments-backend-registers-backend ()
  "The Google Docs adapter registers an org-comments backend."
  (should (equal (org-comments-backend-name 'google-docs) "Google Docs"))
  (should (memq :sync (org-comments-backend-capabilities 'google-docs)))
  (should (memq :pull (org-comments-backend-capabilities 'google-docs)))
  (should (memq :open-remote (org-comments-backend-capabilities 'google-docs)))
  (should (memq :set-status (org-comments-backend-capabilities 'google-docs)))
  (should (memq :push (org-comments-backend-capabilities 'google-docs))))

(ert-deftest org-google-docs-comments-backend-detects-linked-buffer ()
  "A gdocs-linked Org buffer selects the Google Docs comments backend."
  (let ((org-comments-default-backend 'org))
    (with-temp-buffer
      (org-mode)
      (insert ":PROPERTIES:\n:GDOCS_DOCUMENT_ID: doc-123\n:END:\n\nBody\n")
      (should (eq (org-comments-backend-detect) 'google-docs)))))

(ert-deftest org-google-docs-comments-backend-does-not-detect-plain-org-buffer ()
  "Plain Org buffers keep the default comments backend."
  (let ((org-comments-default-backend 'org))
    (with-temp-buffer
      (org-mode)
      (insert "#+TITLE: Plain\n\nBody\n")
      (should (eq (org-comments-backend-detect) 'org)))))

(ert-deftest org-google-docs-comments-backend-pull-delegates-to-import ()
  "Pulling Google Docs comments imports from the source buffer."
  (let* ((directory (make-temp-file "org-google-docs-backend" t))
	 (source-file (expand-file-name "source.org" directory))
	 called)
    (unwind-protect
	(progn
	  (with-temp-file source-file
	    (insert ":PROPERTIES:\n:GDOCS_DOCUMENT_ID: doc-123\n:END:\n\nBody\n"))
	  (cl-letf (((symbol-function 'org-google-docs-comments-import)
		     (lambda (&optional include-resolved callback)
		       (setq called (list :include-resolved include-resolved
					  :buffer-file buffer-file-name))
		       (when callback (funcall callback "sidecar"))
		       "sidecar")))
	    (should (equal (org-comments-backend-pull
			    'google-docs
			    (list :source-file source-file
				  :include-resolved t))
			   "sidecar")))
	  (should (equal called (list :include-resolved t
				      :buffer-file source-file))))
      (when-let* ((buffer (find-buffer-visiting source-file)))
	(kill-buffer buffer))
      (delete-directory directory t))))

(ert-deftest org-google-docs-comments-backend-open-remote-browses-doc-url ()
  "Opening a remote Google Docs comment opens the best available Docs URL."
  (let (opened)
    (cl-letf (((symbol-function 'browse-url)
	       (lambda (url &rest _args)
		 (setq opened url))))
      (should (equal (org-comments-backend-open-remote
		      'google-docs
		      '(:document-id "doc-123" :remote-id "c-1"))
		     "https://docs.google.com/document/d/doc-123/edit?disco=c-1"))
      (should (equal opened "https://docs.google.com/document/d/doc-123/edit?disco=c-1")))))

(ert-deftest org-google-docs-comments-backend-resolve-uses-drive-reply-action ()
  "Google Docs comments are resolved by posting a Drive reply action."
  (let (called
	(gdocs-api--drive-base-url "https://www.googleapis.com/drive/v3/files"))
    (cl-letf (((symbol-function 'require)
	       (lambda (feature &optional _filename _noerror)
		 (or (eq feature 'gdocs-api)
		     (featurep feature))))
	      ((symbol-function 'gdocs-api--request)
	       (lambda (method url callback &rest args)
		 (setq called (list :method method :url url :args args))
		 (funcall callback '((id . "reply-1") (action . "resolve"))))))
      (should (equal (org-google-docs-comments-backend--resolve-comment
		      "doc-123" "c-1" "Root body" #'identity "personal")
		     '((id . "reply-1") (action . "resolve"))))
      (should (equal called
		     (list :method 'post
			   :url (concat "https://www.googleapis.com/drive/v3/files"
					"/doc-123/comments/c-1"
					"/replies?fields=id,action,content")
			   :args (list :account "personal"
				       :body (json-encode '((action . "resolve"))))))))))

(ert-deftest org-google-docs-comments-backend-resolves-remote-comment ()
  "Resolving a Google Docs comment calls upstream API and updates the sidecar."
  (let* ((directory (make-temp-file "org-google-docs-resolve" t))
	 (source-file (expand-file-name "source.org" directory))
	 (sidecar-file (expand-file-name "source.comments.org" directory))
	 called)
    (unwind-protect
	(progn
	  (with-temp-file source-file
	    (insert ":PROPERTIES:\n:GDOCS_DOCUMENT_ID: doc-123\n:END:\n\nBody\n"))
	  (with-temp-file sidecar-file
	    (insert "#+title: Comments for source.org\n")
	    (insert "#+source: source.org\n")
	    (insert "#+todo: OPEN TODO | RESOLVED\n\n")
	    (insert "* OPEN Google Docs comment\n")
	    (insert ":PROPERTIES:\n")
	    (insert ":ORG_COMMENTS_ID: google-docs:c-1\n")
	    (insert ":ORG_COMMENTS_BACKEND: google-docs\n")
	    (insert ":ORG_COMMENTS_REMOTE_ID: c-1\n")
	    (insert ":ORG_COMMENTS_REMOTE_RESOLUTION_STATUS: open\n")
	    (insert ":END:\n\nBody\n"))
	  (cl-letf (((symbol-function 'require)
		     (lambda (feature &optional _filename _noerror)
		       (or (eq feature 'gdocs-api)
			   (featurep feature))))
		    ((symbol-function 'org-google-docs-comments-backend--resolve-comment)
		     (lambda (file-id comment-id content callback &optional account)
		       (setq called (list :file-id file-id
					  :comment-id comment-id
					  :content content
					  :account account))
		       (funcall callback '((id . "c-1") (resolved . t))))))
	    (should (equal (org-comments-backend-set-status
			    'google-docs
			    (list :document-id "doc-123"
				  :remote-id "c-1"
				  :sidecar-file sidecar-file
				  :id "google-docs:c-1")
			    "RESOLVED")
			   '((id . "c-1") (resolved . t)))))
	  (should (equal called (list :file-id "doc-123"
				      :comment-id "c-1"
				      :content "Body"
				      :account nil)))
	  (with-temp-buffer
	    (insert-file-contents sidecar-file)
	    (should (search-forward "* RESOLVED Google Docs comment" nil t))
	    (should (search-forward ":ORG_COMMENTS_REMOTE_RESOLUTION_STATUS: resolved" nil t))))
      (delete-directory directory t))))

(ert-deftest org-google-docs-comments-backend-rejects-unsupported-status ()
  "The initial Google Docs status backend only supports resolving comments."
  (let ((error (should-error
		(org-comments-backend-set-status
		 'google-docs
		 '(:document-id "doc-123" :remote-id "c-1")
		 "OPEN")
		:type 'user-error)))
    (should (string-match-p "Google Docs does not support reopening Google Docs comments"
			    (error-message-string error)))
    (should (string-match-p "OPEN and TODO remain local states"
			    (error-message-string error)))))

(ert-deftest org-google-docs-comments-backend-builds-reply-payload ()
  "Build a remote reply payload from a local sidecar reply."
  (let* ((directory (make-temp-file "org-google-docs-reply" t))
	 (sidecar-file (expand-file-name "source.comments.org" directory)))
    (unwind-protect
	(progn
	  (with-temp-file sidecar-file
	    (insert "#+title: Comments for source.org\n")
	    (insert "#+source: source.org\n")
	    (insert "#+todo: OPEN TODO | RESOLVED\n\n")
	    (insert "* OPEN Google Docs comment\n")
	    (insert ":PROPERTIES:\n")
	    (insert ":ORG_COMMENTS_ID: google-docs:c-1\n")
	    (insert ":ORG_COMMENTS_REMOTE_ID: c-1\n")
	    (insert ":END:\n\nRoot\n\n")
	    (insert "** OPEN Reply\n")
	    (insert ":PROPERTIES:\n")
	    (insert ":ORG_COMMENTS_ID: reply-1\n")
	    (insert ":ORG_COMMENTS_SYNC_KIND: reply\n")
	    (insert ":ORG_COMMENTS_REMOTE_PARENT_ID: c-1\n")
	    (insert ":END:\n\nThanks for the review.\n"))
	  (should (equal (org-google-docs-comments-backend--reply-payload
			  (list :document-id "doc-123"
				:sidecar-file sidecar-file
				:id "reply-1"))
			 (list :document-id "doc-123"
			       :parent-remote-id "c-1"
			       :remote-id nil
			       :body "Thanks for the review."
			       :sidecar-file sidecar-file
			       :id "reply-1"))))
      (delete-directory directory t))))

(ert-deftest org-google-docs-comments-backend-reply-payload-requires-remote-parent ()
  "Reply payload creation fails clearly without a remote parent comment."
  (let* ((directory (make-temp-file "org-google-docs-reply-local" t))
	 (sidecar-file (expand-file-name "source.comments.org" directory)))
    (unwind-protect
	(progn
	  (with-temp-file sidecar-file
	    (insert "#+title: Comments for source.org\n")
	    (insert "#+source: source.org\n")
	    (insert "#+todo: OPEN TODO | RESOLVED\n\n")
	    (insert "* OPEN Local comment\n")
	    (insert ":PROPERTIES:\n")
	    (insert ":ORG_COMMENTS_ID: local-root\n")
	    (insert ":END:\n\nRoot\n"))
	  (should-error
	   (org-google-docs-comments-backend--reply-payload
	    (list :document-id "doc-123"
		  :sidecar-file sidecar-file
		  :id "local-root"))
	   :type 'user-error))
      (delete-directory directory t))))

(ert-deftest org-google-docs-comments-backend-pushes-local-reply ()
  "Pushing a local reply creates a remote Google Docs reply and records its id."
  (let* ((directory (make-temp-file "org-google-docs-push-reply" t))
	 (sidecar-file (expand-file-name "source.comments.org" directory))
	 called)
    (unwind-protect
	(progn
	  (with-temp-file sidecar-file
	    (insert "#+title: Comments for source.org\n")
	    (insert "#+source: source.org\n")
	    (insert "#+todo: OPEN TODO | RESOLVED\n\n")
	    (insert "* OPEN Google Docs comment\n")
	    (insert ":PROPERTIES:\n")
	    (insert ":ORG_COMMENTS_ID: google-docs:c-1\n")
	    (insert ":ORG_COMMENTS_REMOTE_ID: c-1\n")
	    (insert ":END:\n\nRoot\n\n")
	    (insert "** OPEN Reply\n")
	    (insert ":PROPERTIES:\n")
	    (insert ":ORG_COMMENTS_ID: reply-1\n")
	    (insert ":ORG_COMMENTS_SYNC_KIND: reply\n")
	    (insert ":ORG_COMMENTS_REMOTE_PARENT_ID: c-1\n")
	    (insert ":END:\n\nThanks for the review.\n"))
	  (cl-letf (((symbol-function 'org-google-docs-comments-backend--create-reply)
		     (lambda (document-id parent-id body callback &optional account)
		       (setq called (list :document-id document-id
					  :parent-id parent-id
					  :body body
					  :account account))
		       (funcall callback '((id . "r-1")
					   (content . "Thanks for the review."))))))
	    (should (equal (org-comments-backend-push
			    'google-docs
			    (list :document-id "doc-123"
				  :account "personal"
				  :sidecar-file sidecar-file
				  :id "reply-1"))
			   '((id . "r-1")
			     (content . "Thanks for the review.")))))
	  (should (equal called (list :document-id "doc-123"
				      :parent-id "c-1"
				      :body "Thanks for the review."
				      :account "personal")))
	  (with-temp-buffer
	    (insert-file-contents sidecar-file)
	    (should (search-forward ":ORG_COMMENTS_REMOTE_ID: r-1" nil t))
	    (goto-char (point-min))
	    (should (search-forward ":ORG_COMMENTS_REMOTE_PARENT_ID: c-1" nil t))
	    (should (search-forward ":ORG_COMMENTS_REMOTE_STATE: present" nil t))))
      (delete-directory directory t))))

(ert-deftest org-google-docs-comments-backend-pushes-pending-root-resolution ()
  "Pushing a locally resolved root comment resolves it remotely."
  (let* ((directory (make-temp-file "org-google-docs-push-status" t))
	 (sidecar-file (expand-file-name "source.comments.org" directory))
	 called)
    (unwind-protect
	(progn
	  (with-temp-file sidecar-file
	    (insert "#+title: Comments for source.org\n")
	    (insert "#+source: source.org\n")
	    (insert "#+todo: OPEN TODO | RESOLVED\n\n")
	    (insert "* RESOLVED Google Docs comment\n")
	    (insert ":PROPERTIES:\n")
	    (insert ":ORG_COMMENTS_ID: google-docs:c-1\n")
	    (insert ":ORG_COMMENTS_REMOTE_ID: c-1\n")
	    (insert ":ORG_COMMENTS_REMOTE_RESOLUTION_STATUS: open\n")
	    (insert ":ORG_COMMENTS_LOCAL_STATUS_DIRTY: status\n")
	    (insert ":END:\n\nRoot body.\n"))
	  (cl-letf (((symbol-function 'require)
		     (lambda (feature &optional _filename _noerror)
		       (or (eq feature 'gdocs-api)
			   (featurep feature))))
		    ((symbol-function 'org-google-docs-comments-backend--resolve-comment)
		     (lambda (document-id comment-id content callback &optional account)
		       (setq called (list :document-id document-id
					  :comment-id comment-id
					  :content content
					  :account account))
		       (funcall callback '((id . "c-1") (resolved . t))))))
	    (should (equal (org-comments-backend-push
			    'google-docs
			    (list :document-id "doc-123"
				  :account "personal"
				  :sidecar-file sidecar-file
				  :id "google-docs:c-1"))
			   '((id . "c-1") (resolved . t)))))
	  (should (equal called (list :document-id "doc-123"
				      :comment-id "c-1"
				      :content "Root body."
				      :account "personal")))
	  (with-temp-buffer
	    (insert-file-contents sidecar-file)
	    (should (search-forward ":ORG_COMMENTS_REMOTE_RESOLUTION_STATUS: resolved" nil t))))
      (delete-directory directory t))))

(ert-deftest org-google-docs-comments-backend-sync-pushes-pending-resolution ()
  "Sync pushes pending local root resolution before importing remote comments."
  (let* ((directory (make-temp-file "org-google-docs-sync-status" t))
	 (source-file (expand-file-name "source.org" directory))
	 (sidecar-file (expand-file-name "source.comments.org" directory))
	 called imported)
    (unwind-protect
	(progn
	  (with-temp-file source-file
	    (insert ":PROPERTIES:\n:GDOCS_DOCUMENT_ID: doc-123\n:END:\n\nBody\n"))
	  (with-temp-file sidecar-file
	    (insert "#+title: Comments for source.org\n")
	    (insert "#+source: source.org\n")
	    (insert "#+todo: OPEN TODO | RESOLVED\n\n")
	    (insert "* RESOLVED Google Docs comment\n")
	    (insert ":PROPERTIES:\n")
	    (insert ":ORG_COMMENTS_ID: google-docs:c-1\n")
	    (insert ":ORG_COMMENTS_REMOTE_ID: c-1\n")
	    (insert ":ORG_COMMENTS_REMOTE_RESOLUTION_STATUS: open\n")
	    (insert ":ORG_COMMENTS_LOCAL_STATUS_DIRTY: status\n")
	    (insert ":END:\n\nRoot body.\n"))
	  (cl-letf (((symbol-function 'require)
		     (lambda (feature &optional _filename _noerror)
		       (or (eq feature 'gdocs-api)
			   (featurep feature))))
		    ((symbol-function 'org-google-docs-comments-backend--resolve-comment)
		     (lambda (document-id comment-id content callback &optional account)
		       (setq called (list :document-id document-id
					  :comment-id comment-id
					  :content content
					  :account account))
		       (funcall callback '((id . "c-1") (resolved . t)))))
		    ((symbol-function 'org-google-docs-comments-import)
		     (lambda (&optional include-resolved callback)
		       (setq imported (list :include-resolved include-resolved
					    :buffer-file buffer-file-name))
		       (when callback (funcall callback sidecar-file))
		       sidecar-file)))
	    (should (equal (org-comments-backend-sync
			    'google-docs
			    (list :source-file source-file))
			   sidecar-file)))
	  (should (equal called (list :document-id "doc-123"
				      :comment-id "c-1"
				      :content "Root body."
				      :account nil)))
	  (should (equal imported (list :include-resolved nil
					:buffer-file source-file))))
      (when-let* ((buffer (find-buffer-visiting source-file)))
	(kill-buffer buffer))
      (delete-directory directory t))))

(ert-deftest org-google-docs-comments-backend-push-resolved-root-noops-when-synced ()
  "Pushing an already remote-resolved root comment is a no-op."
  (let* ((directory (make-temp-file "org-google-docs-push-status-noop" t))
	 (sidecar-file (expand-file-name "source.comments.org" directory))
	 called)
    (unwind-protect
	(progn
	  (with-temp-file sidecar-file
	    (insert "#+title: Comments for source.org\n")
	    (insert "#+source: source.org\n")
	    (insert "#+todo: OPEN TODO | RESOLVED\n\n")
	    (insert "* RESOLVED Google Docs comment\n")
	    (insert ":PROPERTIES:\n")
	    (insert ":ORG_COMMENTS_ID: google-docs:c-1\n")
	    (insert ":ORG_COMMENTS_REMOTE_ID: c-1\n")
	    (insert ":ORG_COMMENTS_REMOTE_RESOLUTION_STATUS: resolved\n")
	    (insert ":END:\n\nRoot body.\n"))
	  (cl-letf (((symbol-function 'org-google-docs-comments-backend--resolve-comment)
		     (lambda (&rest _args) (setq called t))))
	    (should (equal (org-comments-backend-push
			    'google-docs
			    (list :document-id "doc-123"
				  :sidecar-file sidecar-file
				  :id "google-docs:c-1"))
			   '(:already-pushed t :remote-id "c-1" :status "RESOLVED"))))
	  (should-not called))
      (delete-directory directory t))))

(ert-deftest org-google-docs-comments-backend-does-not-push-reply-twice ()
  "Pushing an already synced reply does not create a duplicate remote reply."
  (let* ((directory (make-temp-file "org-google-docs-push-reply-dup" t))
	 (sidecar-file (expand-file-name "source.comments.org" directory))
	 called)
    (unwind-protect
	(progn
	  (with-temp-file sidecar-file
	    (insert "#+title: Comments for source.org\n")
	    (insert "#+source: source.org\n")
	    (insert "#+todo: OPEN TODO | RESOLVED\n\n")
	    (insert "* OPEN Google Docs comment\n")
	    (insert ":PROPERTIES:\n")
	    (insert ":ORG_COMMENTS_ID: google-docs:c-1\n")
	    (insert ":ORG_COMMENTS_REMOTE_ID: c-1\n")
	    (insert ":END:\n\nRoot\n\n")
	    (insert "** OPEN Reply\n")
	    (insert ":PROPERTIES:\n")
	    (insert ":ORG_COMMENTS_ID: reply-1\n")
	    (insert ":ORG_COMMENTS_SYNC_KIND: reply\n")
	    (insert ":ORG_COMMENTS_REMOTE_PARENT_ID: c-1\n")
	    (insert ":ORG_COMMENTS_REMOTE_ID: r-1\n")
	    (insert ":END:\n\nThanks for the review.\n"))
	  (cl-letf (((symbol-function 'org-google-docs-comments-backend--create-reply)
		     (lambda (&rest _args) (setq called t))))
	    (should (equal (org-comments-backend-push
			    'google-docs
			    (list :document-id "doc-123"
				  :sidecar-file sidecar-file
				  :id "reply-1"))
			   '(:already-pushed t :remote-id "r-1"))))
	  (should-not called))
      (delete-directory directory t))))

(ert-deftest org-google-docs-comments-backend-push-rejects-local-root-comment ()
  "Google Docs push rejects new root comments until anchored creation is supported."
  (let* ((directory (make-temp-file "org-google-docs-push-root" t))
	 (sidecar-file (expand-file-name "source.comments.org" directory)))
    (unwind-protect
	(progn
	  (with-temp-file sidecar-file
	    (insert "#+title: Comments for source.org\n")
	    (insert "#+source: source.org\n")
	    (insert "#+todo: OPEN TODO | RESOLVED\n\n")
	    (insert "* OPEN Local root\n")
	    (insert ":PROPERTIES:\n")
	    (insert ":ORG_COMMENTS_ID: local-root\n")
	    (insert ":END:\n\nRoot body.\n"))
	  (let ((error (should-error
			(org-comments-backend-push
			 'google-docs
			 (list :document-id "doc-123"
			       :sidecar-file sidecar-file
			       :id "local-root"))
			:type 'user-error)))
	    (should (string-match-p "Google Docs does not support native anchored root comment creation"
				    (error-message-string error)))
	    (should (string-match-p "unanchored Drive comments"
				    (error-message-string error)))))
      (delete-directory directory t))))

(provide 'org-google-docs-comments-backend-test)
;;; org-google-docs-comments-backend-test.el ends here
