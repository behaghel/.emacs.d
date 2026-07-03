;;; org-google-docs-comments-import-test.el --- Google comment sidecar import tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for importing normalized Google Docs comments into Org sidecars.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'org)

(add-to-list 'load-path (expand-file-name ".." (file-name-directory load-file-name)))
(add-to-list 'load-path (expand-file-name "../../org-comments" (file-name-directory load-file-name)))

(require 'org-google-docs-comments-import)

(defmacro org-google-docs-comments-import-test--with-source (&rest body)
  "Create a temporary Org source buffer and run BODY."
  (declare (indent 0))
  `(let* ((dir (make-temp-file "org-google-docs-comments-" t))
	  (source-file (expand-file-name "source.org" dir)))
     (unwind-protect
	 (with-current-buffer (find-file-noselect source-file)
	   (erase-buffer)
	   (org-mode)
	   (insert ":PROPERTIES:\n:GDOCS_DOCUMENT_ID: doc-123\n:GDOCS_ACCOUNT: personal\n:END:\n\nBody text\n")
	   (save-buffer)
	   ,@body)
       (when (get-file-buffer source-file)
	 (kill-buffer (get-file-buffer source-file)))
       (delete-directory dir t))))

(ert-deftest org-google-docs-comments-import-writes-active-comments-to-sidecar ()
  "Import active Google comments into the source sidecar."
  (org-google-docs-comments-import-test--with-source
   (cl-letf (((symbol-function 'org-google-docs-comments-list)
	      (lambda (callback)
		(funcall callback
			 (list (list :backend 'google-docs
				     :kind 'comment
				     :remote-id "c-1"
				     :body "Please clarify."
				     :target-text "Body text"
				     :author-name "Ada Lovelace"
				     :author-email "ada@example.com"
				     :created-at "2026-07-02T10:00:00Z"
				     :updated-at "2026-07-02T10:01:00Z"
				     :status "open"))))))
     (let ((sidecar (org-google-docs-comments-import)))
       (should (file-exists-p sidecar))
       (with-temp-buffer
	 (insert-file-contents sidecar)
	 (should (search-forward "#+source: source.org" nil t))
	 (should (search-forward "* OPEN Google Docs comment from Ada Lovelace" nil t))
	 (should (search-forward ":ORG_COMMENTS_BACKEND: google-docs" nil t))
	 (should (search-forward ":ORG_COMMENTS_REMOTE_ID: c-1" nil t))
	 (should (search-forward ":ORG_COMMENTS_TARGET_TEXT: Body text" nil t))
	 (should (search-forward "Please clarify." nil t))
	 (should-not (search-forward "#+begin_quote" nil t)))))))

(ert-deftest org-google-docs-comments-import-reports-added-updated-and-skipped ()
  "Google Docs import returns provider-neutral feedback counts."
  (org-google-docs-comments-import-test--with-source
   (let ((sidecar (org-comments-sidecar-path buffer-file-name)))
     (org-comments-ensure-sidecar-header sidecar buffer-file-name)
     (with-temp-buffer
       (insert-file-contents sidecar)
       (goto-char (point-max))
       (insert "* OPEN Existing Google Docs comment\n")
       (insert ":PROPERTIES:\n")
       (insert ":ORG_COMMENTS_ID: google-docs:c-1\n")
       (insert ":ORG_COMMENTS_BACKEND: google-docs\n")
       (insert ":ORG_COMMENTS_REMOTE_ID: c-1\n")
       (insert ":END:\n\nOld body.\n")
       (write-region (point-min) (point-max) sidecar nil 'silent))
     (let ((report (org-google-docs-comments-import--import-list
		    (list (list :backend 'google-docs
				:remote-id "c-1"
				:body "Updated body."
				:status "open")
			  (list :backend 'google-docs
				:remote-id "c-2"
				:body "New body."
				:status "open")
			  (list :backend 'google-docs
				:remote-id "c-3"
				:body "Resolved body."
				:status "resolved"))
		    nil buffer-file-name (current-buffer))))
       (should (equal (plist-get report :provider) "Google Docs"))
       (should (= (plist-get report :added) 1))
       (should (= (plist-get report :updated) 1))
       (should (= (plist-get report :skipped-resolved) 1))
       (should (plist-get report :preserved-local))))))

(ert-deftest org-google-docs-comments-import-writes-remote-replies ()
  "Import appends remote Google replies under their parent comment."
  (org-google-docs-comments-import-test--with-source
   (cl-letf (((symbol-function 'org-google-docs-comments-list)
	      (lambda (callback)
		(funcall callback
			 (list (list :backend 'google-docs
				     :kind 'comment
				     :remote-id "c-1"
				     :body "Root body."
				     :status "open"
				     :replies
				     (list (list :backend 'google-docs
						 :kind 'reply
						 :remote-id "r-1"
						 :body "Remote reply."
						 :author-name "Grace Hopper"
						 :created-at "2026-07-03T10:00:00Z"))))))))
     (let ((sidecar (org-google-docs-comments-import)))
       (with-temp-buffer
	 (insert-file-contents sidecar)
	 (should (search-forward "* OPEN Google Docs comment" nil t))
	 (should (search-forward "** OPEN Reply from Grace Hopper" nil t))
	 (should (search-forward ":ORG_COMMENTS_SYNC_KIND: reply" nil t))
	 (should (search-forward ":ORG_COMMENTS_REMOTE_ID: r-1" nil t))
	 (should (search-forward ":ORG_COMMENTS_REMOTE_PARENT_ID: c-1" nil t))
	 (should (search-forward ":ORG_COMMENTS_REMOTE_STATE: present" nil t))
	 (should (search-forward "Remote reply." nil t)))))))

(ert-deftest org-google-docs-comments-import-does-not-duplicate-remote-replies ()
  "Re-importing remote Google replies is idempotent."
  (org-google-docs-comments-import-test--with-source
   (let ((comments (list (list :backend 'google-docs
			       :kind 'comment
			       :remote-id "c-1"
			       :body "Root body."
			       :status "open"
			       :replies
			       (list (list :backend 'google-docs
					   :kind 'reply
					   :remote-id "r-1"
					   :body "Remote reply."))))))
     (cl-letf (((symbol-function 'org-google-docs-comments-list)
		(lambda (callback) (funcall callback comments))))
       (let ((sidecar (org-google-docs-comments-import)))
	 (org-google-docs-comments-import)
	 (with-temp-buffer
	   (insert-file-contents sidecar)
	   (should (= 1 (how-many ":ORG_COMMENTS_REMOTE_ID: r-1"
				  (point-min) (point-max))))))))))

(ert-deftest org-google-docs-comments-import-skips-resolved-by-default ()
  "Resolved Google comments are not imported unless requested."
  (org-google-docs-comments-import-test--with-source
   (cl-letf (((symbol-function 'org-google-docs-comments-list)
	      (lambda (callback)
		(funcall callback
			 (list (list :backend 'google-docs
				     :remote-id "c-1"
				     :body "Done."
				     :status "resolved"))))))
     (let ((sidecar (org-google-docs-comments-import)))
       (should (file-exists-p sidecar))
       (with-temp-buffer
	 (insert-file-contents sidecar)
	 (should-not (search-forward ":ORG_COMMENTS_REMOTE_ID: c-1" nil t)))))))

(ert-deftest org-google-docs-comments-import-can-include-resolved ()
  "Resolved Google comments are imported when INCLUDE-RESOLVED is non-nil."
  (org-google-docs-comments-import-test--with-source
   (cl-letf (((symbol-function 'org-google-docs-comments-list)
	      (lambda (callback)
		(funcall callback
			 (list (list :backend 'google-docs
				     :remote-id "c-1"
				     :body "Done."
				     :status "resolved"))))))
     (let ((sidecar (org-google-docs-comments-import t)))
       (with-temp-buffer
	 (insert-file-contents sidecar)
	 (should (search-forward "* RESOLVED Google Docs comment" nil t))
	 (should (search-forward ":ORG_COMMENTS_REMOTE_ID: c-1" nil t)))))))

(ert-deftest org-google-docs-comments-import-anchors-unique-quoted-text ()
  "Import records source bounds when quoted text uniquely matches."
  (org-google-docs-comments-import-test--with-source
   (cl-letf (((symbol-function 'org-google-docs-comments-list)
	      (lambda (callback)
		(funcall callback
			 (list (list :backend 'google-docs
				     :remote-id "c-1"
				     :body "Look here."
				     :target-text "Body text"
				     :status "open"))))))
     (let ((sidecar (org-google-docs-comments-import)))
       (with-temp-buffer
	 (insert-file-contents sidecar)
	 (should (search-forward ":ORG_COMMENTS_TARGET: " nil t))
	 (should-not (search-forward ":ORG_COMMENTS_ANCHOR_STATE:" nil t)))))))

(ert-deftest org-google-docs-comments-import-marks-missing-quoted-text ()
  "Import marks comments as missing when quoted text does not match source."
  (org-google-docs-comments-import-test--with-source
   (cl-letf (((symbol-function 'org-google-docs-comments-list)
	      (lambda (callback)
		(funcall callback
			 (list (list :backend 'google-docs
				     :remote-id "c-1"
				     :body "Look here."
				     :target-text "Not present"
				     :status "open"))))))
     (let ((sidecar (org-google-docs-comments-import)))
       (with-temp-buffer
	 (insert-file-contents sidecar)
	 (should (search-forward ":ORG_COMMENTS_ANCHOR_STATE: missing" nil t))
	 (should (search-forward ":ORG_COMMENTS_ANCHOR_MATCH_COUNT: 0" nil t)))))))

(ert-deftest org-google-docs-comments-import-marks-ambiguous-quoted-text ()
  "Import marks comments as ambiguous when quoted text matches more than once."
  (org-google-docs-comments-import-test--with-source
   (goto-char (point-max))
   (insert "Body text again\n")
   (save-buffer)
   (cl-letf (((symbol-function 'org-google-docs-comments-list)
	      (lambda (callback)
		(funcall callback
			 (list (list :backend 'google-docs
				     :remote-id "c-1"
				     :body "Look here."
				     :target-text "Body text"
				     :status "open"))))))
     (let ((sidecar (org-google-docs-comments-import)))
       (with-temp-buffer
	 (insert-file-contents sidecar)
	 (should (search-forward ":ORG_COMMENTS_ANCHOR_STATE: ambiguous" nil t))
	 (should (search-forward ":ORG_COMMENTS_ANCHOR_MATCH_COUNT: 2" nil t)))))))

(ert-deftest org-google-docs-comments-import-updates-existing-resolved-comment ()
  "Re-import marks an existing comment resolved when remote status is resolved."
  (org-google-docs-comments-import-test--with-source
   (let ((sidecar (org-comments-sidecar-path buffer-file-name)))
     (org-comments-ensure-sidecar-header sidecar buffer-file-name)
     (with-temp-buffer
       (insert-file-contents sidecar)
       (goto-char (point-max))
       (insert "* OPEN Google Docs comment\n")
       (insert ":PROPERTIES:\n")
       (insert ":ORG_COMMENTS_ID: google-docs:c-1\n")
       (insert ":ORG_COMMENTS_BACKEND: google-docs\n")
       (insert ":ORG_COMMENTS_REMOTE_ID: c-1\n")
       (insert ":ORG_COMMENTS_REMOTE_RESOLUTION_STATUS: open\n")
       (insert ":END:\n\nBody.\n\n")
       (insert "** Local notes\nKeep this note.\n")
       (write-region (point-min) (point-max) sidecar nil 'silent))
     (let ((report (org-google-docs-comments-import--import-list
		    (list (list :backend 'google-docs
				:remote-id "c-1"
				:body "Body."
				:status "resolved"))
		    nil buffer-file-name (current-buffer))))
       (should (= (plist-get report :updated) 1))
       (should (= (plist-get report :remote-resolved) 1)))
     (with-temp-buffer
       (insert-file-contents sidecar)
       (should (search-forward "* RESOLVED Google Docs comment" nil t))
       (should (search-forward ":ORG_COMMENTS_REMOTE_RESOLUTION_STATUS: resolved" nil t))
       (should (search-forward "** Local notes" nil t))))))

(ert-deftest org-google-docs-comments-import-updates-existing-remote-comment ()
  "Re-import updates remote-owned content while preserving local notes."
  (org-google-docs-comments-import-test--with-source
   (let ((sidecar (org-comments-sidecar-path buffer-file-name)))
     (org-comments-ensure-sidecar-header sidecar buffer-file-name)
     (with-temp-buffer
       (insert-file-contents sidecar)
       (goto-char (point-max))
       (insert "* OPEN Google Docs comment from Ada Lovelace\n")
       (insert ":PROPERTIES:\n")
       (insert ":ORG_COMMENTS_ID: google-docs:c-1\n")
       (insert ":ORG_COMMENTS_BACKEND: google-docs\n")
       (insert ":ORG_COMMENTS_REMOTE_ID: c-1\n")
       (insert ":ORG_COMMENTS_REMOTE_RESOLUTION_STATUS: open\n")
       (insert ":END:\n\n")
       (insert "#+begin_quote\nOld remote body.\n#+end_quote\n\n")
       (insert "** Local notes\nKeep this local note.\n")
       (write-region (point-min) (point-max) sidecar nil 'silent))
     (cl-letf (((symbol-function 'org-google-docs-comments-list)
		(lambda (callback)
		  (funcall callback
			   (list (list :backend 'google-docs
				       :remote-id "c-1"
				       :body "Updated remote body."
				       :target-text "Body text"
				       :author-name "Ada Lovelace"
				       :updated-at "2026-07-02T11:00:00Z"
				       :status "open"))))))
       (org-google-docs-comments-import)
       (with-temp-buffer
	 (insert-file-contents sidecar)
	 (should (= 1 (how-many ":ORG_COMMENTS_REMOTE_ID: c-1" (point-min) (point-max))))
	 (should (search-forward "Updated remote body." nil t))
	 (should-not (search-forward "Old remote body." nil t))
	 (should (search-forward "** Local notes" nil t))
	 (should (search-forward "Keep this local note." nil t)))))))

(provide 'org-google-docs-comments-import-test)
;;; org-google-docs-comments-import-test.el ends here
