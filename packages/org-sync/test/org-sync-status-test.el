;;; org-sync-status-test.el --- Org sync status tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the generic Org sync status bottom panel.

;;; Code:

(require 'ert)
(require 'org)
(require 'org-sync)
(require 'context-panels)
(require 'org-comments)

(defmacro org-sync-status-test--with-providers (providers &rest body)
  "Bind org-sync status PROVIDERS while running BODY."
  (declare (indent 1))
  `(let ((org-sync-providers ,providers))
     ,@body))

(defun org-sync-status-test--fake-provider (&optional detect-result fetch-result)
  "Return a fake org-sync provider with DETECT-RESULT and FETCH-RESULT."
  (list :kind 'fake
	:detect (lambda (_source-buffer)
		  (or detect-result
		      (list :kind 'fake :remote-id "doc-1" :title "Fake Doc")))
	:fetch (lambda (&rest _args) fetch-result)))

(ert-deftest org-sync-detects-single-provider ()
  "Provider detection returns the one matching document descriptor."
  (with-temp-buffer
    (org-mode)
    (org-sync-status-test--with-providers
     (list (org-sync-status-test--fake-provider))
     (let ((descriptor (org-sync-detect-document (current-buffer))))
       (should (equal (plist-get descriptor :kind) 'fake))
       (should (equal (plist-get descriptor :remote-id) "doc-1"))))))

(ert-deftest org-sync-errors-without-provider ()
  "Provider detection fails clearly when no adapter matches."
  (with-temp-buffer
    (org-mode)
    (org-sync-status-test--with-providers nil
					  (should-error (org-sync-detect-document (current-buffer))
							:type 'user-error))))

(ert-deftest org-sync-status-opens-bottom-view ()
  "Status opens a generic context-panels bottom view for the source buffer."
  (with-temp-buffer
    (org-mode)
    (org-sync-status-test--with-providers
     (list (org-sync-status-test--fake-provider))
     (let ((source (current-buffer)))
       (org-sync-status)
       (let ((panel (buffer-local-value 'context-panels-bottom-panel-buffer
					source)))
	 (should (buffer-live-p panel))
	 (with-current-buffer panel
	   (should (derived-mode-p 'org-sync-status-mode))
	   (should (eq context-panels-source-buffer source))
	   (should (string-match-p "Org Sync: fake doc-1" (buffer-string)))
	   (should (string-match-p "Content[[:space:]]+unknown" (buffer-string)))
	   (should (string-match-p "Comments[[:space:]]+unknown" (buffer-string)))))))))

(ert-deftest org-sync-refresh-renders-content-ahead-after-local-edit ()
  "Refreshing after local source edits renders content ahead."
  (let* ((directory (make-temp-file "org-sync-status" t))
	 (source-file (expand-file-name "source.org" directory)))
    (unwind-protect
	(progn
	  (with-temp-file source-file (insert "Body\n"))
	  (org-sync-store-write
	   source-file
	   '(:provider (:kind fake :remote-id "doc-1")
		       :domains ((content :fetched-remote-ref (:version "1"))
				 (comments :fetched-remote-ref (:hash "remote-comments")))))
	  (with-current-buffer (find-file-noselect source-file)
	    (org-mode)
	    (org-sync-status-test--with-providers
	     (list (org-sync-status-test--fake-provider))
	     (org-sync-status)
	     (org-sync-baseline)
	     (with-current-buffer (find-buffer-visiting source-file)
	       (goto-char (point-max))
	       (insert "Local edit\n")
	       (org-sync-refresh))
	     (with-current-buffer (get-buffer org-sync-status-buffer-name)
	       (should (string-match-p "Content[[:space:]]+ahead" (buffer-string)))
	       (should (string-match-p "Comments[[:space:]]+clean" (buffer-string)))))))
      (when-let* ((buffer (find-buffer-visiting source-file)))
	(kill-buffer buffer))
      (delete-directory directory t))))

(ert-deftest org-sync-refresh-renders-comments-ahead-after-local-comment ()
  "Refreshing after local sidecar comment edits renders comments ahead."
  (let* ((directory (make-temp-file "org-sync-status" t))
	 (source-file (expand-file-name "source.org" directory)))
    (unwind-protect
	(progn
	  (with-temp-file source-file (insert "Alpha beta gamma\n"))
	  (org-sync-store-write
	   source-file
	   '(:provider (:kind fake :remote-id "doc-1")
		       :domains ((content :fetched-remote-ref (:version "1"))
				 (comments :fetched-remote-ref (:hash "remote-comments")))))
	  (with-current-buffer (find-file-noselect source-file)
	    (org-mode)
	    (org-sync-status-test--with-providers
	     (list (org-sync-status-test--fake-provider))
	     (org-sync-status)
	     (org-sync-baseline)
	     (with-current-buffer (find-buffer-visiting source-file)
	       (goto-char (point-min))
	       (search-forward "Alpha")
	       (org-comments-append-to-sidecar
		(org-comments-create-record source-file
					    (match-beginning 0) (match-end 0)
					    "Review this." "c1" "Alice" "now"))
	       (org-sync-refresh))
	     (with-current-buffer (get-buffer org-sync-status-buffer-name)
	       (should (string-match-p "Content[[:space:]]+clean" (buffer-string)))
	       (should (string-match-p "Comments[[:space:]]+ahead" (buffer-string)))))))
      (when-let* ((buffer (find-buffer-visiting source-file)))
	(kill-buffer buffer))
      (delete-directory directory t))))

(ert-deftest org-sync-fetch-updates-remote-tracking-only ()
  "Fetching writes remote refs without mutating source or comments sidecars."
  (let* ((directory (make-temp-file "org-sync-status" t))
	 (source-file (expand-file-name "source.org" directory)))
    (unwind-protect
	(progn
	  (with-temp-file source-file (insert "Alpha beta gamma\n"))
	  (org-sync-store-write
	   source-file
	   '(:provider (:kind fake :remote-id "doc-1")
		       :domains ((content :fetched-remote-ref (:version "1"))
				 (comments :fetched-remote-ref (:hash "comments-v1")))))
	  (with-current-buffer (find-file-noselect source-file)
	    (org-mode)
	    (goto-char (point-min))
	    (search-forward "Alpha")
	    (org-comments-append-to-sidecar
	     (org-comments-create-record source-file
					 (match-beginning 0) (match-end 0)
					 "Review this." "c1" "Alice" "now"))
	    (let ((source-before (buffer-string))
		  (comments-before (with-temp-buffer
				     (insert-file-contents (org-comments-sidecar-path source-file))
				     (buffer-string))))
	      (org-sync-status-test--with-providers
	       (list (org-sync-status-test--fake-provider
		      nil
		      '(:provider fake :remote-id "doc-1" :fetched-at "now"
				  :domains ((content :remote-ref (:version "2"))
					    (comments :remote-ref (:hash "comments-v2" :count 1))))))
	       (org-sync-status)
	       (org-sync-baseline)
	       (org-sync-fetch)
	       (let* ((tracking (org-sync-store-read source-file))
		      (content (alist-get 'content (plist-get tracking :domains)))
		      (comments (alist-get 'comments (plist-get tracking :domains))))
		 (should (equal (plist-get content :fetched-remote-ref)
				'(:version "2")))
		 (should (equal (plist-get comments :fetched-remote-ref)
				'(:hash "comments-v2" :count 1)))
		 (with-current-buffer (get-buffer org-sync-status-buffer-name)
		   (should (string-match-p "Content[[:space:]]+behind" (buffer-string)))
		   (should (string-match-p "Comments[[:space:]]+behind" (buffer-string))))))
	      (with-current-buffer (find-buffer-visiting source-file)
		(should (equal source-before (buffer-string))))
	      (with-temp-buffer
		(insert-file-contents (org-comments-sidecar-path source-file))
		(should (equal comments-before (buffer-string)))))))
      (when-let* ((buffer (find-buffer-visiting source-file)))
	(kill-buffer buffer))
      (delete-directory directory t))))

(ert-deftest org-sync-baseline-records-base-refs-and-renders-clean ()
  "Baselining stores current/fetched ref pairs and renders clean domains."
  (let* ((directory (make-temp-file "org-sync-status" t))
	 (source-file (expand-file-name "source.org" directory)))
    (unwind-protect
	(progn
	  (with-temp-file source-file (insert "Body\n"))
	  (org-sync-store-write
	   source-file
	   '(:provider (:kind fake :remote-id "doc-1")
		       :domains ((content :fetched-remote-ref (:version "1"))
				 (comments :fetched-remote-ref (:hash "remote-comments")))))
	  (with-current-buffer (find-file-noselect source-file)
	    (org-mode)
	    (org-sync-status-test--with-providers
	     (list (org-sync-status-test--fake-provider))
	     (org-sync-status)
	     (org-sync-baseline)
	     (let* ((tracking (org-sync-store-read source-file))
		    (content (alist-get 'content (plist-get tracking :domains)))
		    (comments (alist-get 'comments (plist-get tracking :domains)))
		    (panel (get-buffer org-sync-status-buffer-name)))
	       (should (plist-get content :base-local-ref))
	       (should (plist-get content :base-remote-ref))
	       (should (plist-get comments :base-local-ref))
	       (should (plist-get comments :base-remote-ref))
	       (with-current-buffer panel
		 (should (string-match-p "Content[[:space:]]+clean" (buffer-string)))
		 (should (string-match-p "Comments[[:space:]]+clean" (buffer-string))))))))
      (when-let* ((buffer (find-buffer-visiting source-file)))
	(kill-buffer buffer))
      (delete-directory directory t))))

(provide 'org-sync-status-test)
;;; org-sync-status-test.el ends here
