;;; org-google-docs-comments-import-test.el --- Google comment sidecar import tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for importing normalized Google Docs comments into Org sidecars.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'org)

(add-to-list 'load-path (expand-file-name ".." (file-name-directory load-file-name)))
(add-to-list 'load-path (expand-file-name "../../org-sync"
					  (file-name-directory load-file-name)))
(add-to-list 'load-path (expand-file-name "../../org-comments" (file-name-directory load-file-name)))

(require 'org-comments-store)
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
	 (should (search-forward ":ORG_COMMENTS_SYNC_KIND: inline" nil t))
	 (should (search-forward ":ORG_COMMENTS_REMOTE_ID: c-1" nil t))
	 (should (search-forward ":ORG_COMMENTS_REMOTE_STATE: present" nil t))
	 (goto-char (point-min))
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

(ert-deftest org-google-docs-comments-import-collected-records-are-normalized ()
  "Imported Google sidecars collect as normalized collaboration records."
  (org-google-docs-comments-import-test--with-source
   (cl-letf (((symbol-function 'org-google-docs-comments-list)
	      (lambda (callback)
		(funcall callback
			 (list (list :backend 'google-docs
				     :kind 'comment
				     :remote-id "c-1"
				     :body "Root body."
				     :target-text "Body text"
				     :author-name "Ada Lovelace"
				     :created-at "2026-07-02T10:00:00Z"
				     :status "open"
				     :replies
				     (list (list :backend 'google-docs
						 :kind 'reply
						 :remote-id "r-1"
						 :body "Remote reply."
						 :author-name "Grace Hopper"
						 :created-at "2026-07-02T10:05:00Z"))))))))
     (org-google-docs-comments-import)
     (let* ((comment (car (org-comments-collect (current-buffer) t)))
	    (reply (car (plist-get comment :replies))))
       (should (eq (plist-get comment :backend) 'google-docs))
       (should (eq (plist-get comment :remote-state) 'present))
       (should (equal (plist-get comment :sync-kind) "inline"))
       (should (equal (plist-get comment :remote-author-name) "Ada Lovelace"))
       (should (equal (plist-get comment :created-at) "2026-07-02T10:00:00Z"))
       (should (equal (plist-get comment :status) "OPEN"))
       (should (eq (plist-get reply :backend) 'google-docs))
       (should (eq (plist-get reply :remote-state) 'present))
       (should (equal (plist-get reply :sync-kind) "reply"))
       (should (equal (plist-get reply :remote-author-name) "Grace Hopper"))
       (should (equal (plist-get reply :created-at) "2026-07-02T10:05:00Z"))))))

(ert-deftest org-google-docs-comments-import-updates-remote-replies ()
  "Re-import updates existing remote Google reply bodies and metadata."
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
					   :body "Updated remote reply."
					   :author-name "Ada Lovelace"
					   :updated-at "2026-07-03T12:00:00Z"))))))
     (let ((report (org-google-docs-comments-import--import-list
		    (list (list :backend 'google-docs
				:kind 'comment
				:remote-id "c-1"
				:body "Root body."
				:status "open"))
		    nil buffer-file-name (current-buffer))))
       (let ((sidecar (plist-get report :sidecar-file)))
	 (with-temp-buffer
	   (insert-file-contents sidecar)
	   (goto-char (point-max))
	   (insert "\n** OPEN Reply from old import\n")
	   (insert ":PROPERTIES:\n")
	   (insert ":ORG_COMMENTS_ID: google-docs-reply:r-1\n")
	   (insert ":ORG_COMMENTS_BACKEND: google-docs\n")
	   (insert ":ORG_COMMENTS_SYNC_KIND: reply\n")
	   (insert ":ORG_COMMENTS_REMOTE_ID: r-1\n")
	   (insert ":ORG_COMMENTS_REMOTE_PARENT_ID: c-1\n")
	   (insert ":ORG_COMMENTS_REMOTE_STATE: missing\n")
	   (insert ":ORG_COMMENTS_REMOTE_MISSING_AT: 2026-07-03T00:00:00+0000\n")
	   (insert ":END:\n\nOld remote reply.\n")
	   (write-region (point-min) (point-max) sidecar nil 'silent))
	 (let ((report (org-google-docs-comments-import--import-list
			comments nil buffer-file-name (current-buffer))))
	   (should (= (plist-get report :updated-replies) 1)))
	 (with-temp-buffer
	   (insert-file-contents sidecar)
	   (should (= 1 (how-many ":ORG_COMMENTS_REMOTE_ID: r-1"
				  (point-min) (point-max))))
	   (should (search-forward ":ORG_COMMENTS_REMOTE_STATE: present" nil t))
	   (should-not (search-forward ":ORG_COMMENTS_REMOTE_MISSING_AT:" nil t))
	   (should (search-forward "Updated remote reply." nil t))))))))

(ert-deftest org-google-docs-comments-import-marks-missing-remote-replies ()
  "Re-import marks missing remote Google replies without touching local replies."
  (org-google-docs-comments-import-test--with-source
   (let ((comments (list (list :backend 'google-docs
			       :kind 'comment
			       :remote-id "c-1"
			       :body "Root body."
			       :status "open"
			       :replies nil))))
     (cl-letf (((symbol-function 'org-google-docs-comments-list)
		(lambda (callback) (funcall callback comments))))
       (let ((sidecar (org-google-docs-comments-import)))
	 (with-temp-buffer
	   (insert-file-contents sidecar)
	   (goto-char (point-max))
	   (insert "\n** OPEN Remote reply\n")
	   (insert ":PROPERTIES:\n")
	   (insert ":ORG_COMMENTS_ID: google-docs-reply:r-missing\n")
	   (insert ":ORG_COMMENTS_BACKEND: google-docs\n")
	   (insert ":ORG_COMMENTS_SYNC_KIND: reply\n")
	   (insert ":ORG_COMMENTS_REMOTE_ID: r-missing\n")
	   (insert ":ORG_COMMENTS_REMOTE_PARENT_ID: c-1\n")
	   (insert ":ORG_COMMENTS_REMOTE_STATE: present\n")
	   (insert ":END:\n\nRemote reply.\n")
	   (insert "\n** OPEN Local unsynced reply\n")
	   (insert ":PROPERTIES:\n")
	   (insert ":ORG_COMMENTS_ID: local-reply\n")
	   (insert ":ORG_COMMENTS_SYNC_KIND: reply\n")
	   (insert ":ORG_COMMENTS_REMOTE_PARENT_ID: c-1\n")
	   (insert ":END:\n\nLocal reply.\n")
	   (write-region (point-min) (point-max) sidecar nil 'silent))
	 (let ((report (org-google-docs-comments-import--import-list
			comments nil buffer-file-name (current-buffer))))
	   (should (= (plist-get report :remote-missing-replies) 1)))
	 (with-temp-buffer
	   (insert-file-contents sidecar)
	   (should (search-forward ":ORG_COMMENTS_REMOTE_ID: r-missing" nil t))
	   (should (search-forward ":ORG_COMMENTS_REMOTE_STATE: missing" nil t))
	   (should (search-forward ":ORG_COMMENTS_REMOTE_MISSING_AT:" nil t))
	   (should (search-forward ":ORG_COMMENTS_ID: local-reply" nil t))))))))

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
       (insert ":ORG_COMMENTS_LOCAL_STATUS_DIRTY: status\n")
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
       (should-not (search-forward ":ORG_COMMENTS_LOCAL_STATUS_DIRTY:" nil t))
       (should (search-forward "** Local notes" nil t))))))

(ert-deftest org-google-docs-comments-import-marks-missing-remote-comments ()
  "Re-import marks existing Google comments missing when absent remotely."
  (org-google-docs-comments-import-test--with-source
   (let ((sidecar (org-comments-sidecar-path buffer-file-name)))
     (org-comments-ensure-sidecar-header sidecar buffer-file-name)
     (with-temp-buffer
       (insert-file-contents sidecar)
       (goto-char (point-max))
       (insert "* OPEN Existing Google Docs comment\n")
       (insert ":PROPERTIES:\n")
       (insert ":ORG_COMMENTS_ID: google-docs:c-missing\n")
       (insert ":ORG_COMMENTS_BACKEND: google-docs\n")
       (insert ":ORG_COMMENTS_REMOTE_ID: c-missing\n")
       (insert ":ORG_COMMENTS_REMOTE_STATE: present\n")
       (insert ":END:\n\nMissing body.\n\n")
       (insert "* OPEN Local comment\n")
       (insert ":PROPERTIES:\n")
       (insert ":ORG_COMMENTS_ID: local-1\n")
       (insert ":END:\n\nLocal body.\n")
       (write-region (point-min) (point-max) sidecar nil 'silent))
     (let ((report (org-google-docs-comments-import--import-list
		    (list (list :backend 'google-docs
				:remote-id "c-present"
				:body "Present body."
				:status "open"))
		    nil buffer-file-name (current-buffer))))
       (should (= (plist-get report :remote-missing) 1)))
     (with-temp-buffer
       (insert-file-contents sidecar)
       (should (search-forward ":ORG_COMMENTS_REMOTE_ID: c-missing" nil t))
       (should (search-forward ":ORG_COMMENTS_REMOTE_STATE: missing" nil t))
       (should (search-forward ":ORG_COMMENTS_REMOTE_MISSING_AT:" nil t))
       (goto-char (point-min))
       (should (search-forward ":ORG_COMMENTS_REMOTE_ID: c-present" nil t))))))

(ert-deftest org-google-docs-comments-import-clears-missing-when-seen-again ()
  "Re-import marks a previously missing Google comment present when seen again."
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
       (insert ":ORG_COMMENTS_REMOTE_STATE: missing\n")
       (insert ":ORG_COMMENTS_REMOTE_MISSING_AT: 2026-07-03T00:00:00+0000\n")
       (insert ":END:\n\nOld body.\n")
       (write-region (point-min) (point-max) sidecar nil 'silent))
     (org-google-docs-comments-import--import-list
      (list (list :backend 'google-docs
		  :remote-id "c-1"
		  :body "Returned body."
		  :status "open"))
      nil buffer-file-name (current-buffer))
     (with-temp-buffer
       (insert-file-contents sidecar)
       (should (search-forward ":ORG_COMMENTS_REMOTE_STATE: present" nil t))
       (should-not (search-forward ":ORG_COMMENTS_REMOTE_MISSING_AT:" nil t))
       (should (search-forward "Returned body." nil t))))))

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
