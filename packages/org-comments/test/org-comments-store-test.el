;;; org-comments-store-test.el --- Store tests for org-comments package -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for reading comment records from local sidecar storage.

;;; Code:

(require 'ert)
(require 'org-comments-migrate)
(require 'org-comments-store)

(defmacro org-comments-store-test--with-file-buffer (name contents &rest body)
  "Visit temp file NAME with CONTENTS, then run BODY."
  (declare (indent 2))
  `(let* ((dir (make-temp-file "org-comments-store-" t))
	  (file (expand-file-name ,name dir)))
     (unwind-protect
	 (with-current-buffer (find-file-noselect file)
	   (erase-buffer)
	   (insert ,contents)
	   (save-buffer)
	   (org-mode)
	   ,@body)
       (delete-directory dir t))))

(ert-deftest org-comments-store-collects-valid-sidecar-comments ()
  "Collect returns anchored records from the current source buffer sidecar."
  (org-comments-store-test--with-file-buffer "article.org" "Alpha selected text omega"
					     (let* ((start (progn (goto-char (point-min))
								  (search-forward "selected")
								  (match-beginning 0)))
						    (end (match-end 0))
						    (record (org-comments-create-record
							     buffer-file-name start end "Review." "local-1" "Alice" "now")))
					       (org-comments-append-to-sidecar record)
					       (let ((comments (org-comments-collect (current-buffer))))
						 (should (= 1 (length comments)))
						 (should (equal (plist-get (car comments) :id) "local-1"))
						 (should (equal (plist-get (car comments) :target-text) "selected"))
						 (should (equal (plist-get (car comments) :anchor-pos) start))))))

(ert-deftest org-comments-store-normalizes-collaboration-fields ()
  "Collected sidecar records include normalized collaboration fields."
  (let* ((dir (make-temp-file "org-comments-store-" t))
	 (source (expand-file-name "article.org" dir))
	 (sidecar (org-comments-sidecar-path source)))
    (unwind-protect
	(progn
	  (write-region "Alpha selected text omega" nil source nil 'silent)
	  (with-temp-file sidecar
	    (insert "#+title: Comments\n#+source: article.org\n\n")
	    (insert "* RESOLVED Remote\n")
	    (insert ":PROPERTIES:\n")
	    (insert ":ORG_COMMENTS_ID: local-1\n")
	    (insert ":ORG_COMMENTS_REMOTE_ID: remote-1\n")
	    (insert ":ORG_COMMENTS_REMOTE_AUTHOR_DISPLAY_NAME: Alice\n")
	    (insert ":ORG_COMMENTS_REMOTE_RESOLUTION_STATUS: resolved\n")
	    (insert ":ORG_COMMENTS_TARGET: 7 20\n")
	    (insert ":ORG_COMMENTS_TARGET_TEXT: selected text\n")
	    (insert ":END:\n\nBody\n"))
	  (with-current-buffer (find-file-noselect source)
	    (org-mode)
	    (let ((comment (car (org-comments-collect (current-buffer)))))
	      (should (eq (plist-get comment :remote-state) 'present))
	      (should (equal (plist-get comment :local-state) nil))
	      (should (plist-get comment :resolved))
	      (should (equal (plist-get comment :remote-author-name) "Alice")))))
      (when-let* ((buffer (find-buffer-visiting source)))
	(kill-buffer buffer))
      (delete-directory dir t))))

(ert-deftest org-comments-store-normalizes-page-replies ()
  "Page comment collection normalizes reply collaboration fields."
  (let* ((dir (make-temp-file "org-comments-store-" t))
	 (source (expand-file-name "article.org" dir))
	 (sidecar (org-comments-sidecar-path source)))
    (unwind-protect
	(progn
	  (write-region "#+TITLE: Article\n\nBody" nil source nil 'silent)
	  (with-temp-file sidecar
	    (insert "#+title: Comments\n#+source: article.org\n\n")
	    (insert "* OPEN Page\n")
	    (insert ":PROPERTIES:\n")
	    (insert ":ORG_COMMENTS_ID: page-1\n")
	    (insert ":ORG_COMMENTS_SYNC_KIND: footer\n")
	    (insert ":ORG_COMMENTS_REMOTE_ID: remote-page\n")
	    (insert ":END:\n\nPage body.\n")
	    (insert "** Reply · Bob — Gone\n")
	    (insert ":PROPERTIES:\n")
	    (insert ":ORG_COMMENTS_ID: reply-1\n")
	    (insert ":ORG_COMMENTS_SYNC_KIND: reply\n")
	    (insert ":ORG_COMMENTS_REMOTE_ID: remote-reply\n")
	    (insert ":ORG_COMMENTS_REMOTE_STATE: missing\n")
	    (insert ":END:\n\nReply body.\n"))
	  (with-current-buffer (find-file-noselect source)
	    (org-mode)
	    (let* ((page (car (org-comments-collect-page (current-buffer))))
		   (reply (car (plist-get page :replies))))
	      (should (eq (plist-get page :remote-state) 'present))
	      (should (eq (plist-get reply :remote-state) 'missing))
	      (should (plist-get reply :actionable)))))
      (when-let* ((buffer (find-buffer-visiting source)))
	(kill-buffer buffer))
      (delete-directory dir t))))

(defun org-comments-store-test--legacy-property (suffix value)
  "Return a legacy sidecar property line for SUFFIX and VALUE."
  (format ":%s%s: %s\n"
	  org-comments-migrate-legacy-property-prefix suffix value))

(ert-deftest org-comments-store-collect-requires-explicit-migration ()
  "Normal collection reports legacy sidecars instead of silently reading them."
  (let* ((dir (make-temp-file "org-comments-store-" t))
	 (source (expand-file-name "article.org" dir))
	 (sidecar (org-comments-sidecar-path source)))
    (unwind-protect
	(progn
	  (write-region "Alpha selected text omega" nil source nil 'silent)
	  (with-temp-file sidecar
	    (insert "#+title: Comments\n#+source: article.org\n\n")
	    (insert "* OPEN Legacy\n")
	    (insert ":PROPERTIES:\n")
	    (insert (org-comments-store-test--legacy-property "ID" "local-1"))
	    (insert ":END:\n\nBody\n"))
	  (with-current-buffer (find-file-noselect source)
	    (org-mode)
	    (should-error (org-comments-collect (current-buffer)) :type 'user-error)
	    (should-error (org-comments-collect-page (current-buffer)) :type 'user-error)))
      (when-let* ((buffer (find-buffer-visiting source)))
	(kill-buffer buffer))
      (delete-directory dir t))))

(ert-deftest org-comments-store-local-id-lookup-requires-explicit-migration ()
  "Remote lookup reports legacy sidecars instead of silently reading them."
  (let* ((dir (make-temp-file "org-comments-store-" t))
	 (source (expand-file-name "article.org" dir))
	 (sidecar (org-comments-sidecar-path source)))
    (unwind-protect
	(progn
	  (write-region "#+TITLE: Article\n" nil source nil 'silent)
	  (with-temp-file sidecar
	    (insert "#+title: Comments\n#+source: article.org\n\n")
	    (insert "* OPEN Remote\n")
	    (insert ":PROPERTIES:\n")
	    (insert (org-comments-store-test--legacy-property "ID" "local-1"))
	    (insert (org-comments-store-test--legacy-property "REMOTE_ID" "remote-1"))
	    (insert ":END:\n\nBody\n"))
	  (should-error (org-comments-local-id-for-remote-id sidecar "remote-1")
			:type 'user-error))
      (delete-directory dir t))))

(ert-deftest org-comments-store-finds-local-id-for-remote-id ()
  "Remote IDs can be mapped back to local sidecar IDs."
  (let* ((dir (make-temp-file "org-comments-store-" t))
	 (source (expand-file-name "article.org" dir))
	 (sidecar (org-comments-sidecar-path source)))
    (unwind-protect
	(progn
	  (write-region "#+TITLE: Article\n" nil source nil 'silent)
	  (with-temp-file sidecar
	    (insert "#+title: Comments\n#+source: article.org\n\n")
	    (insert "* OPEN Remote\n")
	    (insert ":PROPERTIES:\n")
	    (insert ":ORG_COMMENTS_ID: local-1\n")
	    (insert ":ORG_COMMENTS_REMOTE_ID: remote-1\n")
	    (insert ":END:\n\nBody\n"))
	  (should (equal (org-comments-local-id-for-remote-id sidecar "remote-1")
			 "local-1")))
      (delete-directory dir t))))

(provide 'org-comments-store-test)
;;; org-comments-store-test.el ends here
