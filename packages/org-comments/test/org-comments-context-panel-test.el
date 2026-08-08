;;; org-comments-context-panel-test.el --- Comments context provider tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the org-comments provider layer over context-panels primitives.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'org-comments)

(defun org-comments-context-panel-test--write-sidecar (source-file body)
  "Write BODY to SOURCE-FILE's comments sidecar."
  (let ((sidecar-file (org-comments-sidecar-path source-file)))
    (make-directory (file-name-directory sidecar-file) t)
    (with-temp-file sidecar-file
      (insert body))
    sidecar-file))

(ert-deftest org-comments-context-panel-refresh-creates-provider-overlays ()
  "The comments provider creates range overlays and page markers."
  (let* ((directory (make-temp-file "org-comments-context-panel" t))
	 (source-file (expand-file-name "source.org" directory)))
    (unwind-protect
	(progn
	  (with-temp-file source-file
	    (insert "#+TITLE: Source\n\nAlpha beta gamma\n"))
	  (org-comments-context-panel-test--write-sidecar
	   source-file
	   "#+SOURCE: source.org\n\n* OPEN Page note\n:PROPERTIES:\n:ORG_COMMENTS_ID: p1\n:ORG_COMMENTS_SOURCE_FILE: source.org\n:ORG_COMMENTS_SYNC_KIND: footer\n:END:\n\nPage body\n")
	  (with-current-buffer (find-file-noselect source-file)
	    (org-mode)
	    (goto-char (point-min))
	    (search-forward "Alpha")
	    (org-comments-append-to-sidecar
	     (org-comments-create-record source-file
					 (match-beginning 0)
					 (match-end 0)
					 "Body" "c1" "Alice" "now"))
	    (org-comments-context-panel-refresh)
	    (should (overlayp org-comments-page-comment-overlay))
	    (should (seq-some (lambda (overlay)
				(overlay-get overlay 'org-comments-comment))
			      org-comments-overlays))
	    (org-comments-context-panel-delete-overlays)
	    (should-not org-comments-overlays)
	    (should-not (overlayp org-comments-page-comment-overlay))))
      (delete-directory directory t))))

(ert-deftest org-comments-context-panel-follow-point-highlights-source-and-panel ()
  "Point in either source target or side-panel row highlights both views."
  (let* ((directory (make-temp-file "org-comments-context-panel" t))
	 (source-file (expand-file-name "source.org" directory))
	 (panel-buffer (generate-new-buffer " *org-comments-test-panel*")))
    (unwind-protect
	(progn
	  (with-temp-file source-file
	    (insert "#+TITLE: Source\n\nAlpha beta gamma\n"))
	  (with-current-buffer (find-file-noselect source-file)
	    (org-mode)
	    (goto-char (point-min))
	    (search-forward "Alpha")
	    (org-comments-append-to-sidecar
	     (org-comments-create-record source-file
					 (match-beginning 0)
					 (match-end 0)
					 "Body" "c1" "Alice" "now"))
	    (org-comments-context-panel-enable)
	    (setq context-panels-side-panel-buffer panel-buffer)
	    (with-current-buffer panel-buffer
	      (org-comments-panel-mode)
	      (setq context-panels-source-buffer (find-buffer-visiting source-file))
	      (context-panels-render-side-panel context-panels-source-buffer nil))
	    (goto-char (point-min))
	    (search-forward "Alpha")
	    (goto-char (match-beginning 0))
	    (org-comments-context-panel-follow-point)
	    (let ((source-overlay (cl-find-if
				   (lambda (overlay)
				     (overlay-get overlay 'org-comments-comment))
				   (overlays-at (point)))))
	      (should (eq (overlay-get source-overlay 'face)
			  'org-comments-active-region-face))
	      (with-current-buffer panel-buffer
		(should (overlayp org-comments-active-panel-overlay))
		(should (eq (overlay-get org-comments-active-panel-overlay 'face)
			    'org-comments-active-panel-face))
		(goto-char (point-min))
		(search-forward "Body")
		(org-comments-context-panel-follow-point))
	      (should (eq (overlay-get source-overlay 'face)
			  'org-comments-active-region-face)))))
      (when (buffer-live-p panel-buffer)
	(kill-buffer panel-buffer))
      (when-let* ((source (find-buffer-visiting source-file)))
	(kill-buffer source))
      (delete-directory directory t))))

(ert-deftest org-comments-context-panel-panel-focus-highlights-full-target-range ()
  "Focusing a panel row highlights the full inline target range."
  (with-temp-buffer
    (org-mode)
    (insert "Alpha beta gamma\n")
    (let ((source (current-buffer))
	  (comment (list :type 'comment :id "c1" :target-start 1
			 :target-end 17 :target-text "Alpha beta gamma"
			 :body "Body")))
      (cl-letf (((symbol-function 'org-comments-collect)
		 (lambda (&rest _) (list comment))))
	(org-comments-context-panel-refresh-source-overlays)
	(with-temp-buffer
	  (org-comments-panel-mode)
	  (setq context-panels-source-buffer source)
	  (let ((inhibit-read-only t))
	    (org-comments-panel-render-insert-comment comment))
	  (goto-char (point-min))
	  (org-comments-context-panel-follow-point))
	(let ((active (cl-find-if
		       (lambda (overlay)
			 (eq (overlay-get overlay 'face)
			     'org-comments-active-region-face))
		       org-comments-overlays)))
	  (should active)
	  (should (= (overlay-start active) 1))
	  (should (= (overlay-end active) 17)))))))

(ert-deftest org-comments-context-panel-scope-focus-highlights-heading-line ()
  "Focusing an anchored scope row highlights only its heading line."
  (with-temp-buffer
    (org-mode)
    (insert "* Heading\nBody line\n")
    (let ((comment (list :type 'scope :id "scope-1" :target-start 1
			 :target-end (point-max) :target-text "Heading"
			 :body "Scope body")))
      (cl-letf (((symbol-function 'org-comments-collect)
		 (lambda (&rest _) (list comment))))
	(org-comments-context-panel-refresh-source-overlays)
	(org-comments-context-panel--sync-active-comment (current-buffer) comment)
	(let ((active (cl-find-if
		       (lambda (overlay)
			 (eq (overlay-get overlay 'face)
			     'org-comments-active-region-face))
		       org-comments-overlays)))
	  (should active)
	  (should (= (overlay-start active) 1))
	  (should (= (overlay-end active)
		     (save-excursion
		       (goto-char 1)
		       (line-end-position)))))))))

(ert-deftest org-comments-context-panel-stale-focus-has-no-source-overlay ()
  "Stale comments do not create source overlays when focused."
  (with-temp-buffer
    (org-mode)
    (insert "Alpha beta gamma\n")
    (let ((comment (list :type 'comment :id "stale-1" :target-start 1
			 :target-end 17 :anchor-state 'stale
			 :target-text "Alpha beta gamma" :body "Stale")))
      (cl-letf (((symbol-function 'org-comments-collect)
		 (lambda (&rest _) (list comment))))
	(org-comments-context-panel-refresh-source-overlays)
	(org-comments-context-panel--sync-active-comment (current-buffer) comment)
	(should-not org-comments-overlays)))))

(ert-deftest org-comments-context-panel-collect-side-items-adds-row-icon ()
  "Collected comment items expose the icon used by rendered rows."
  (with-temp-buffer
    (org-mode)
    (let ((comment (list :type 'comment :id "remote-1" :remote-id "42"
			 :target-start 1 :target-end 1 :body "Body")))
      (cl-letf (((symbol-function 'org-comments-collect)
		 (lambda (&rest _) (list comment))))
	(let ((items (org-comments-context-panel-collect-side-items
		      (current-buffer))))
	  (should (equal (plist-get (car items) :icon) "☁️")))))))

(ert-deftest org-comments-context-panel-collect-side-items-normalizes-metadata ()
  "Collected comment items expose source, sidecar, and action metadata."
  (with-temp-buffer
    (org-mode)
    (let ((buffer-file-name "/tmp/org-comments/source.org")
	  (comment (list :type 'comment :id "c1"
			 :suggestion-ids "s1"
			 :target-start 1 :target-end 1 :body "Body")))
      (cl-letf (((symbol-function 'org-comments-collect)
		 (lambda (&rest _) (list comment))))
	(let ((item (car (org-comments-context-panel-collect-side-items
			  (current-buffer)))))
	  (should (equal (plist-get item :source-file)
			 "/tmp/org-comments/source.org"))
	  (should (equal (plist-get item :source-directory)
			 "/tmp/org-comments/"))
	  (should (equal (plist-get item :sidecar-file)
			 "/tmp/org-comments/source.comments.org"))
	  (should (eq (plist-get item :suggestion-linked) t)))))))

(ert-deftest org-comments-context-panel-collect-side-items-resolves-people ()
  "Collected comment items resolve remote author IDs using source context."
  (with-temp-buffer
    (org-mode)
    (let ((buffer-file-name "/tmp/org-comments/source.org")
	  (org-comments-resolve-account-id-function
	   (lambda (account-id directory)
	     (when (and (equal account-id "acct-1")
			(equal directory "/tmp/org-comments/"))
	       "Alice")))
	  (comment (list :type 'comment :id "remote-1" :remote-id "42"
			 :remote-author-id "acct-1"
			 :target-start 1 :target-end 1 :body "Body"
			 :replies (list (list :id "reply-1"
					      :remote-author-id "acct-1"
					      :body "Reply")))))
      (cl-letf (((symbol-function 'org-comments-collect)
		 (lambda (&rest _) (list comment))))
	(let* ((item (car (org-comments-context-panel-collect-side-items
			   (current-buffer))))
	       (reply (car (plist-get item :replies))))
	  (should (equal (plist-get item :remote-author-name) "Alice"))
	  (should (equal (plist-get reply :remote-author-name) "Alice")))))))

(ert-deftest org-comments-context-panel-provider-exposes-collection-functions ()
  "The comments provider descriptor exposes collection and item renderers."
  (let ((provider (org-comments-context-panel-provider)))
    (should (eq (plist-get provider :name) 'comments))
    (should (equal (plist-get provider :icon) "✍️"))
    (should (eq (plist-get provider :collect-side-items)
		#'org-comments-context-panel-collect-side-items))
    (should (eq (plist-get provider :collect-top-markers)
		#'org-comments-context-panel-collect-top-markers))
    (should-not (plist-get provider :render-side-panel))
    (should (eq (plist-get provider :render-side-item)
		#'org-comments-context-panel-render-side-item))
    (should (eq (plist-get provider :refresh-source-overlays)
		#'org-comments-context-panel-refresh-source-overlays))))

(ert-deftest org-comments-mode-enables-context-panel-provider ()
  "`org-comments-mode' enables the comments provider through context-panel mode."
  (with-temp-buffer
    (org-mode)
    (cl-letf (((symbol-function 'context-panels-refresh-source-overlays)
	       (lambda ())))
      (org-comments-mode 1)
      (should org-comments-mode)
      (should context-panels-mode)
      (should (context-panels-registered-provider 'comments))
      (org-comments-mode -1)
      (should-not org-comments-mode)
      (should-not context-panels-mode)
      (should-not (context-panels-registered-provider 'comments)))))

(ert-deftest org-comments-overlays-enable-registers-provider ()
  "Overlay activation registers and unregisters the comments provider."
  (with-temp-buffer
    (org-mode)
    (let ((buffer-file-name nil))
      (cl-letf (((symbol-function 'context-panels-refresh-source-overlays)
		 (lambda ())))
	(org-comments-overlays-enable)
	(should (context-panels-registered-provider 'comments))
	(org-comments-overlays-disable)
	(should-not (context-panels-registered-provider 'comments))))))

(ert-deftest org-comments-overlays-refresh-delegates-through-context-registry ()
  "The public overlay refresh facade delegates through the provider registry."
  (with-temp-buffer
    (org-mode)
    (let (called)
      (cl-letf (((symbol-function 'context-panels-refresh-source-overlays)
		 (lambda ()
		   (setq called t))))
	(org-comments-overlays-refresh))
      (should called)
      (should (context-panels-registered-provider 'comments)))))

(provide 'org-comments-context-panel-test)
;;; org-comments-context-panel-test.el ends here
