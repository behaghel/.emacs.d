;;; org-google-docs-standalone-test.el --- Standalone adapter contracts -*- lexical-binding: t; -*-

;;; Commentary:
;; Verify declared dependencies and fail-closed offline boundaries.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'package)
(require 'org-google-docs)

(defconst org-google-docs-standalone-test--directory
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory containing the portable adapter tests.")

(declare-function org-google-docs-live-conformance--enabled-p
		  "org-google-docs-live-conformance-test")

(ert-deftest org-google-docs-standalone-declares-package-dependencies ()
  "The facade supplies installable metadata for its required local packages."
  (with-temp-buffer
    (insert-file-contents (locate-library "org-google-docs"))
    (let ((requirements (package-desc-reqs (package-buffer-info))))
      (dolist (dependency '(emacs org org-comments ox-sync))
	(should (assq dependency requirements))))))

(ert-deftest org-google-docs-standalone-upstream-safety-controls-are-dynamic ()
  "A forced pull preview suppresses upstream controls across its callback boundary."
  (let ((source (generate-new-buffer " *gdocs-control-source*"))
	(temporary (generate-new-buffer " *gdocs-control-temporary*"))
	(variables '(gdocs-auto-push-on-save gdocs-auto-pull-on-open
					     gdocs-pull-integrity-checks-enable))
	(observed 'not-called))
    (unwind-protect
	(cl-progv variables '(t t t)
	  (cl-letf (((symbol-function 'gdocs-sync--apply-pull)
		     (lambda (&rest _)
		       (setq observed (mapcar #'symbol-value variables))))
		    ((symbol-function 'org-google-docs--pull-diff-render) #'ignore))
	    (org-google-docs--pull-diff-apply-temp-pull
	     nil "revision" source "snapshot" temporary "pulled" "directory"))
	  (should (equal observed '(nil nil nil)))
	  (should (equal (mapcar #'symbol-value variables) '(t t t))))
      (kill-buffer source)
      (kill-buffer temporary))))

(ert-deftest org-google-docs-standalone-live-tests-require-exact-opt-in ()
  "Unset, empty, and disabled flags never authorize real document mutation."
  (unless (fboundp 'org-google-docs-live-conformance--enabled-p)
    (load (expand-file-name "org-google-docs-live-conformance-test.el"
			    org-google-docs-standalone-test--directory) nil t))
  (let ((process-environment (copy-sequence process-environment)))
    (dolist (value '(nil "" "0" "false"))
      (setenv "ORG_GOOGLE_DOCS_LIVE_TESTS" value)
      (should-not (org-google-docs-live-conformance--enabled-p)))
    (setenv "ORG_GOOGLE_DOCS_LIVE_TESTS" "1")
    (should (org-google-docs-live-conformance--enabled-p))))

(ert-deftest org-google-docs-standalone-live-account-setup-does-not-call-personal-config ()
  "Live fixture setup consumes explicit accounts, not private consumer callbacks."
  (unless (fboundp 'org-google-docs-live-conformance--configure-accounts)
    (load (expand-file-name "org-google-docs-live-conformance-test.el"
			    org-google-docs-standalone-test--directory) nil t))
  (let ((process-environment (copy-sequence process-environment))
	called)
    (dolist (variable '("ORG_GOOGLE_DOCS_CLIENT_ID" "ORG_GOOGLE_DOCS_CLIENT_SECRET"
			"ORG_GOOGLE_DOCS_REFRESH_TOKEN"))
      (setenv variable nil))
    (cl-progv '(gdocs-accounts) '(nil)
      (cl-letf (((symbol-function 'hub/org-google-docs-configure-accounts-from-auth-source)
		 (lambda (&rest _) (setq called t))))
	(should-not (org-google-docs-live-conformance--configure-accounts)))
      (should-not called))))

(ert-deftest org-google-docs-recovery-replaces-body-and-backs-up-current-edits ()
  "Recovery confirms once, retains metadata, and backs up edits made during fetch."
  (skip-unless (locate-library "gdocs-sync"))
  (require 'gdocs-sync)
  (let* ((directory (make-temp-file "gdocs-recovery-test-" t))
	 (file (expand-file-name "draft.org" directory))
	 (source (find-file-noselect file))
	 (other (generate-new-buffer " *gdocs-recovery-callback*"))
	 (preamble (concat ":PROPERTIES:\n:GDOCS_DOCUMENT_ID: doc-1\n"
			   ":GDOCS_ACCOUNT: account-1\n:END:\n"
			   "#+title: Local title\n#+date: [2026-11-11]\n"
			   "#+filetags: :product:gdocs:\n#+identifier: stable-id\n\n"))
	 (json '((title . "Remote title")
		 (body . ((content . [((startIndex . 1) (endIndex . 9)
				       (paragraph . ((paragraphStyle . ((namedStyleType . "HEADING_3")))
						     (elements . [((textRun . ((content . "Remote\n") (textStyle . ((bold . t))))))]))))
				      ((startIndex . 9) (endIndex . 27)
				       (paragraph . ((elements . [((textRun . ((content . "Remote body Bépo.\n"))))]))))])))))
	 metadata-callback document-callback current-text
	 (confirmations 0)
	 (gdocs-pull-backup-directory directory))
    (unwind-protect
	(cl-letf (((symbol-function 'gdocs-api-get-file-metadata)
		   (lambda (id callback &optional account)
		     (should (equal id "doc-1"))
		     (should (equal account "account-1"))
		     (setq metadata-callback callback)))
		  ((symbol-function 'gdocs-api-get-document)
		   (lambda (id callback &optional account _on-error)
		     (should (equal id "doc-1"))
		     (should (equal account "account-1"))
		     (setq document-callback callback)))
		  ((symbol-function 'yes-or-no-p)
		   (lambda (_prompt)
		     (cl-incf confirmations)
		     (should (eq (current-buffer) source))
		     t)))
	  (with-current-buffer source
	    (org-mode)
	    (insert preamble "* Local\n\nDiscard this body.\n")
	    (setq-local gdocs-sync--document-id "doc-1")
	    (setq-local gdocs-sync--account "account-1")
	    (setq-local gdocs-sync--shadow-ir '((:type paragraph :contents ((:text "Old")))))
	    (setq-local gdocs-sync--revision-id "old-revision")
	    (let ((before-save-hook nil) (after-save-hook nil)) (save-buffer))
	    (org-google-docs-pull-replace-local)
	    (should-not buffer-read-only)
	    (should (= confirmations 0)))
	  (with-current-buffer other
	    (funcall metadata-callback '((headRevisionId . "new-revision"))))
	  (with-current-buffer source
	    (goto-char (point-max))
	    (insert "Late unsaved edit.\n")
	    (setq current-text (buffer-string))
	    (goto-char (point-min))
	    (search-forward "* Local")
	    (narrow-to-region (match-beginning 0) (point-max)))
	  (with-current-buffer other (funcall document-callback json))
	  (should (= confirmations 1))
	  (let ((backups (directory-files directory t "gdocs-pull-backup")))
	    (should (= (length backups) 1))
	    (should (equal current-text
			   (with-temp-buffer
			     (insert-file-contents (car backups)) (buffer-string)))))
	  (with-current-buffer source
	    (widen)
	    (should (string-match-p "^#\\+title: Local title$" (buffer-string)))
	    (should (string-match-p "^#\\+identifier: stable-id$" (buffer-string)))
	    (should (string-match-p "^#\\+filetags: :product:gdocs:$" (buffer-string)))
	    (should (string-match-p "^\\*\\*\\* Remote\n\nRemote body Bépo\\." (buffer-string)))
	    (should-not (string-match-p "Discard this\\|Late unsaved\\|Remote title" (buffer-string)))
	    (should (equal gdocs-sync--revision-id "new-revision"))
	    (should (eq gdocs-sync--status 'synced))
	    (should-not (buffer-modified-p))
	    (should (equal (buffer-string)
			   (with-temp-buffer (insert-file-contents file) (buffer-string))))))
      (when (buffer-live-p source)
	(with-current-buffer source (set-buffer-modified-p nil))
	(kill-buffer source))
      (kill-buffer other)
      (delete-directory directory t))))

(ert-deftest org-google-docs-recovery-blocks-unrepresentable-remote-content ()
  "Unrepresentable chips and table content cannot trigger local replacement."
  (skip-unless (locate-library "gdocs-sync"))
  (require 'gdocs-sync)
  (dolist (payload
	   '("{\"body\":{\"content\":[{\"paragraph\":{\"elements\":[{\"person\":{\"personProperties\":{\"name\":\"Ada\"}}}]}}]}}"
	     "{\"body\":{\"content\":[{\"table\":{\"tableRows\":[{\"tableCells\":[{\"content\":[{\"table\":{\"tableRows\":[]}}]}]}]}}]}}"
	     "{\"body\":{\"content\":[{\"table\":{\"tableRows\":[{\"tableCells\":[{\"content\":[{\"paragraph\":{\"elements\":[{\"textRun\":{\"content\":\"First\\n\"}}]}},{\"paragraph\":{\"elements\":[{\"textRun\":{\"content\":\"Second\\n\"}}]}}]}]}]}}]}}"))
    (let* ((directory (make-temp-file "gdocs-recovery-unsupported-" t))
	   (file (expand-file-name "draft.org" directory))
	   (source (find-file-noselect file))
	   (original "#+title: Local\n\nKeep this body.\n")
	   (json (json-parse-string payload :object-type 'alist))
	   (gdocs-pull-backup-directory directory)
	   confirmed)
      (unwind-protect
	  (with-current-buffer source
	    (org-mode)
	    (insert original)
	    (setq-local gdocs-sync--document-id "doc-1")
	    (setq-local gdocs-sync--account "account-1")
	    (let ((before-save-hook nil) (after-save-hook nil)) (save-buffer))
	    (cl-letf (((symbol-function 'gdocs-api-get-file-metadata)
		       (lambda (_id callback &optional _account)
			 (funcall callback '((headRevisionId . "revision")))))
		      ((symbol-function 'gdocs-api-get-document)
		       (lambda (_id callback &optional _account _on-error)
			 (funcall callback json)))
		      ((symbol-function 'yes-or-no-p)
		       (lambda (_prompt) (setq confirmed t))))
	      (should-error (org-google-docs-pull-replace-local) :type 'user-error))
	    (should-not confirmed)
	    (should (equal original (buffer-string)))
	    (should-not (directory-files directory nil "gdocs-pull-backup")))
	(when (buffer-live-p source)
	  (with-current-buffer source (set-buffer-modified-p nil))
	  (kill-buffer source))
	(delete-directory directory t)))))

(ert-deftest org-google-docs-recovery-cancel-and-failures-leave-local-content-alone ()
  "Cancellation and fetch, conversion, or backup errors do not replace content."
  (skip-unless (locate-library "gdocs-sync"))
  (require 'gdocs-sync)
  (dolist (failure '(cancel fetch conversion backup))
    (let* ((directory (make-temp-file "gdocs-recovery-failure-" t))
	   (file (expand-file-name "draft.org" directory))
	   (source (find-file-noselect file))
	   (original "#+title: Local\n\nKeep this body.\n")
	   (json (json-parse-string
		  "{\"body\":{\"content\":[{\"paragraph\":{\"elements\":[{\"textRun\":{\"content\":\"Remote.\\n\"}}]}}]}}"
		  :object-type 'alist))
	   (gdocs-pull-backup-directory directory)
	   (convert (symbol-function 'gdocs-convert-docs-json-to-ir))
	   confirmed)
      (unwind-protect
	  (with-current-buffer source
	    (org-mode)
	    (insert original)
	    (setq-local gdocs-sync--document-id "doc-1")
	    (setq-local gdocs-sync--account "account-1")
	    (let ((before-save-hook nil) (after-save-hook nil)) (save-buffer))
	    (cl-letf (((symbol-function 'gdocs-api-get-file-metadata)
		       (lambda (_id callback &optional _account)
			 (funcall callback '((headRevisionId . "revision")))))
		      ((symbol-function 'gdocs-api-get-document)
		       (lambda (_id callback &optional _account on-error)
			 (if (eq failure 'fetch)
			     (funcall on-error 'fetch-failed)
			   (funcall callback json))))
		      ((symbol-function 'gdocs-convert-docs-json-to-ir)
		       (lambda (data)
			 (if (eq failure 'conversion)
			     (user-error "Remote conversion failed")
			   (funcall convert data))))
		      ((symbol-function 'gdocs-sync--write-pull-backup)
		       (lambda ()
			 (if (eq failure 'backup)
			     (signal 'file-error '("Cannot write backup"))
			   (ert-fail "Backup should not be attempted"))))
		      ((symbol-function 'yes-or-no-p)
		       (lambda (_prompt)
			 (setq confirmed t)
			 (not (eq failure 'cancel)))))
	      (if (memq failure '(conversion backup))
		  (should-error (org-google-docs-pull-replace-local))
		(org-google-docs-pull-replace-local)))
	    (should (eq (not (null confirmed)) (not (null (memq failure '(cancel backup))))))
	    (should (equal original (buffer-string)))
	    (should (equal original
			   (with-temp-buffer (insert-file-contents file) (buffer-string))))
	    (should-not (directory-files directory nil "gdocs-pull-backup")))
	(when (buffer-live-p source)
	  (with-current-buffer source (set-buffer-modified-p nil))
	  (kill-buffer source))
	(delete-directory directory t)))))

(ert-deftest org-google-docs-standalone-heading-style-roundtrip-and-noop-diff ()
  "Styled levels 1–6 preserve title text and visible emphasis without repeat diffs."
  (skip-unless (locate-library "gdocs-diff"))
  (require 'gdocs-diff)
  (let* ((fixtures (expand-file-name "fixtures/" org-google-docs-standalone-test--directory))
	 (source (with-temp-buffer
		   (insert-file-contents (expand-file-name "headings-all-levels.org" fixtures))
		   (buffer-string)))
	 ;; Representative returned formatting, not a captured live response.
	 (json (with-temp-buffer
		 (insert-file-contents (expand-file-name "headings-all-levels-returned.json" fixtures))
		 (json-parse-buffer :object-type 'alist :false-object :json-false)))
	 (gdocs-style-definitions
	  (append (cl-loop for level from 1 to 6
			   collect (list (intern (format "gdocs-heading-%d" level))
					 :parent (intern (format "heading-%d" level))
					 :text (append '(:font-family "Inter")
						       (when (>= level 3) '(:bold t)))))
		  gdocs-style-definitions))
	 (local-ir (gdocs-convert-org-string-to-ir source))
	 (requests (gdocs-convert-ir-to-docs-requests local-ir))
	 (remote-ir (gdocs-convert-docs-json-to-ir json))
	 (rendered (gdocs-convert-ir-to-org remote-ir))
	 (font-ranges
	  (delq nil (mapcar
		     (lambda (request)
		       (let* ((update (alist-get 'updateTextStyle request))
			      (style (alist-get 'textStyle update)))
			 (when (equal (alist-get 'fontFamily
						 (alist-get 'weightedFontFamily style)) "Inter")
			   (let ((range (alist-get 'range update)))
			     (cons (alist-get 'startIndex range) (alist-get 'endIndex range))))))
		     requests))))
    (should (equal font-ranges '((1 . 10) (11 . 20) (21 . 36)
				 (37 . 46) (47 . 56) (57 . 66))))
    (should (equal rendered source))
    (should-not (gdocs-diff-generate remote-ir local-ir))
    (should-not (gdocs-diff-generate remote-ir (gdocs-convert-org-string-to-ir rendered)))))

(ert-deftest org-google-docs-standalone-combined-publishing-lifecycle ()
  "One source-only lifecycle retains headings, status identity, and block spacing."
  (skip-unless (locate-library "gdocs-diff"))
  (require 'gdocs-diff)
  (let* ((fixtures (expand-file-name "fixtures/" org-google-docs-standalone-test--directory))
	 (source (with-temp-buffer
		   (insert-file-contents (expand-file-name "publishing-integration.org" fixtures))
		   (buffer-string)))
	 ;; Representative provider formatting, independently authored, not live JSON.
	 (json (with-temp-buffer
		 (insert-file-contents (expand-file-name "publishing-integration-returned.json" fixtures))
		 (json-parse-buffer :object-type 'alist :false-object :json-false)))
	 (local-ir (gdocs-convert-org-string-to-ir source))
	 (requests (gdocs-convert-ir-to-docs-requests local-ir))
	 (remote-ir (gdocs-convert-docs-json-to-ir json))
	 (rendered (gdocs-convert-ir-to-org remote-ir))
	 (status (seq-find (lambda (run) (plist-get run :status))
			   (plist-get (car remote-ir) :contents))))
    (should (equal (gdocs-convert-ir-to-org local-ir) source))
    (should (equal rendered source))
    (should (equal (plist-get status :status-marker-ids) '("fixture-status")))
    (should (seq-some
	     (lambda (request)
	       (let ((marker (alist-get 'createNamedRange request)))
		 (and (equal (alist-get 'name marker)
			     "gdocs-org-marker:status:run-9:confluence-status:Green:1")
		      (equal (alist-get 'range marker)
			     '((startIndex . 9) (endIndex . 22))))))
	     requests))
    (should-not (gdocs-diff-generate remote-ir local-ir))
    (should-not (gdocs-diff-generate remote-ir (gdocs-convert-org-string-to-ir rendered)))))

(provide 'org-google-docs-standalone-test)
;;; org-google-docs-standalone-test.el ends here
