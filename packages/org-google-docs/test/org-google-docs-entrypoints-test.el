;;; org-google-docs-entrypoints-test.el --- Org Google Docs entrypoint tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Smoke tests for the safe-loading Org Google Docs adapter facade.

;;; Code:

(require 'cl-lib)
(require 'ert)

(add-to-list 'load-path (expand-file-name ".." (file-name-directory load-file-name)))
(add-to-list 'load-path (expand-file-name "../../org-sync"
					  (file-name-directory load-file-name)))

(require 'org-google-docs)

(ert-deftest org-google-docs-loads-without-upstream-gdocs ()
  "The adapter facade loads even when upstream gdocs is absent."
  (should (featurep 'org-google-docs))
  (should (fboundp 'org-google-docs-doctor))
  (should (fboundp 'org-google-docs-push)))

(ert-deftest org-google-docs-command-errors-when-gdocs-missing ()
  "Commands that require upstream gdocs fail with an actionable error."
  (cl-letf (((symbol-function 'gdocs-push) nil))
    (should-error (org-google-docs-push)
		  :type 'user-error)))

(ert-deftest org-google-docs-facade-commands-delegate-to-upstream-gdocs ()
  "Facade commands call the corresponding upstream gdocs command."
  (let (calls)
    (cl-letf (((symbol-function 'gdocs-create)
	       (lambda () (interactive) (push 'create calls)))
	      ((symbol-function 'gdocs-push)
	       (lambda () (interactive) (push 'push calls)))
	      ((symbol-function 'gdocs-pull)
	       (lambda () (interactive) (push 'pull calls)))
	      ((symbol-function 'gdocs-open-in-browser)
	       (lambda () (interactive) (push 'open-in-browser calls)))
	      ((symbol-function 'gdocs-status)
	       (lambda () (interactive) (push 'status calls))))
      (org-google-docs-create)
      (with-temp-buffer
	(org-mode)
	(org-google-docs-push))
      (org-google-docs-pull)
      (org-google-docs-open)
      (org-google-docs-status)
      (should (equal (nreverse calls)
		     '(create push pull open-in-browser status))))))

(ert-deftest org-google-docs-push-blocks-invalid-footnotes-before-upstream ()
  "Push rejects unsupported footnotes before invoking upstream gdocs."
  (let (calls)
    (cl-letf (((symbol-function 'gdocs-push)
	       (lambda () (interactive) (push 'push calls))))
      (with-temp-buffer
	(insert "* Body\nMissing[fn:missing].\n")
	(org-mode)
	(should-error (org-google-docs-push) :type 'user-error)
	(should-not calls)))))

(ert-deftest org-google-docs-push-resumes-in-source-buffer-after-image-upload ()
  "Async image upload resumes gdocs push in the linked source buffer."
  (let ((source-buffer (generate-new-buffer " *gdocs-source*"))
	(other-buffer (generate-new-buffer " *gdocs-other*"))
	callback-buffer)
    (unwind-protect
	(cl-letf (((symbol-function 'org-google-docs--require-upstream-library)
		   (lambda (_library) t))
		  ((symbol-function 'org-google-docs-images-begin-push)
		   (lambda (_plan callback &optional _account)
		     (with-current-buffer other-buffer
		       (funcall callback))))
		  ((symbol-function 'gdocs-push)
		   (lambda ()
		     (interactive)
		     (setq callback-buffer (current-buffer)))))
	  (with-current-buffer source-buffer
	    (insert "Plain text.\n")
	    (org-mode)
	    (org-google-docs-push))
	  (should (eq callback-buffer source-buffer)))
      (when (buffer-live-p source-buffer)
	(kill-buffer source-buffer))
      (when (buffer-live-p other-buffer)
	(kill-buffer other-buffer)))))

(ert-deftest org-google-docs-push-uploads-standalone-images-before-upstream ()
  "Push prepares image-bearing buffers before invoking upstream gdocs."
  (let ((image-file (make-temp-file "org-google-docs-image" nil ".png"))
	calls)
    (unwind-protect
	(cl-letf (((symbol-function 'org-google-docs--require-upstream-library)
		   (lambda (_library) t))
		  ((symbol-function 'org-google-docs-images-begin-push)
		   (lambda (plan callback &optional _account)
		     (push (length (plist-get plan :images)) calls)
		     (funcall callback)))
		  ((symbol-function 'gdocs-push)
		   (lambda () (interactive) (push 'push calls))))
	  (with-temp-buffer
	    (insert (format "[[file:%s]]\n" image-file))
	    (org-mode)
	    (org-google-docs-push)
	    (should (equal calls '(push 1)))))
      (delete-file image-file))))

(ert-deftest org-google-docs-push-enables-native-footnote-session ()
  "Push starts native footnote handling when a valid footnote plan exists."
  (let (calls)
    (cl-letf (((symbol-function 'org-google-docs--require-upstream-library)
	       (lambda (_library) t))
	      ((symbol-function 'org-google-docs-footnotes-begin-push)
	       (lambda (plan) (push (length (plist-get plan :references)) calls)))
	      ((symbol-function 'gdocs-push)
	       (lambda () (interactive) (push 'push calls))))
      (with-temp-buffer
	(insert "* Body\nText[fn:one].\n\n* Footnotes\n\n[fn:one] Body.\n")
	(org-mode)
	(org-google-docs-push))
      (should (equal calls '(push 1))))))

(ert-deftest org-google-docs-debug-pipeline-writes-local-snapshot ()
  "Pipeline debug writes local preflight details without requiring the network."
  (let ((image-file (make-temp-file "org-google-docs-image" nil ".png")))
    (unwind-protect
	(with-temp-buffer
	  (insert (format "#+CAPTION: Banner\n[[file:%s]]\n" image-file))
	  (org-mode)
	  (let ((debug-buffer (org-google-docs-debug-pipeline)))
	    (with-current-buffer debug-buffer
	      (let ((text (buffer-string)))
		(should (string-match-p "Org Google Docs pipeline trace" text))
		(should (string-match-p ":image-count 1" text))
		(should (string-match-p
			 "local-requests-with-placeholder-image-uris" text))
		(should (string-match-p "Banner" text))))))
      (delete-file image-file))))

(ert-deftest org-google-docs-debug-pipeline-remote-requires-linked-buffer ()
  "Remote pipeline debug fails clearly when the buffer is not linked."
  (with-temp-buffer
    (org-mode)
    (should-error (org-google-docs-debug-pipeline t) :type 'user-error)))

(ert-deftest org-google-docs-sync-current-delegates-to-push-for-now ()
  "The initial sync-current wrapper uses upstream push as the body-sync action."
  (let (calls)
    (cl-letf (((symbol-function 'gdocs-push)
	       (lambda () (interactive) (push 'push calls))))
      (org-google-docs-sync-current)
      (should (equal calls '(push))))))

(ert-deftest org-google-docs-authenticate-ensures-access-token ()
  "The authentication facade reuses upstream token management without printing tokens."
  (let (calls)
    (cl-letf (((symbol-function 'require)
	       (lambda (feature &optional _filename _noerror)
		 (or (eq feature 'gdocs-auth)
		     (featurep feature))))
	      ((symbol-function 'gdocs-auth-select-account)
	       (lambda (&optional _prompt) "personal"))
	      ((symbol-function 'gdocs-auth-get-access-token)
	       (lambda (account callback &optional _errback)
		 (push (list 'token account) calls)
		 (funcall callback "secret-token"))))
      (should (string-match-p "authenticated" (org-google-docs-authenticate)))
      (should (equal calls '((token "personal")))))))

(ert-deftest org-google-docs-doctor-reports-missing-gdocs-and-accounts ()
  "Doctor reports missing upstream gdocs and account configuration."
  (let ((gdocs-accounts nil))
    (cl-letf (((symbol-function 'locate-library)
	       (lambda (library &optional _nosuffix _path _interactive-call)
		 (unless (equal library "gdocs")
		   (locate-library library)))))
      (let ((issues (org-google-docs-doctor)))
	(should (member :missing-gdocs issues))
	(should (member :missing-accounts issues))))))

(ert-deftest org-google-docs-restyle-revision-annotates-typographic-blocks ()
  "Restyle revision metadata applies to typographic blocks."
  (let* ((org-google-docs--restyle-revision "revision")
	 (ir (list (list :type 'source-block :lines '("code"))
		   (list :type 'quote-block
			 :paragraphs (list (list :text "quote")))
		   (list :type 'callout-block
			 :paragraphs (list (list :text "callout")))
		   (list :type 'paragraph :contents (list :text "plain"))))
	 (annotated (org-google-docs--annotate-restyle-revision ir)))
    (should (equal (plist-get (nth 0 annotated) :style-revision) "revision"))
    (should (equal (plist-get (nth 1 annotated) :style-revision) "revision"))
    (should (equal (plist-get (nth 2 annotated) :style-revision) "revision"))
    (should-not (plist-get (nth 3 annotated) :style-revision))))

(ert-deftest org-google-docs-dispatch-runs-selected-action ()
  "Dispatch prompts for an action and runs the selected command."
  (let (calls)
    (cl-letf (((symbol-function 'completing-read)
	       (lambda (&rest _) "Status: Doctor"))
	      ((symbol-function 'org-google-docs-doctor)
	       (lambda () (interactive) (push 'doctor calls))))
      (org-google-docs-dispatch)
      (should (equal calls '(doctor))))))

(ert-deftest org-google-docs-dispatch-labels-use-shared-ux-vocabulary ()
  "Dispatch labels follow the shared remote comments UX vocabulary."
  (should (equal (mapcar #'car org-google-docs--dispatch-actions)
		 '("Status: Doctor"
		   "Status: Upstream gdocs status"
		   "Debug: Pipeline trace"
		   "Sync: Sync current buffer"
		   "Publish: Create Google Doc from buffer"
		   "Publish: Push buffer to Google Docs"
		   "Publish: Restyle typographic blocks"
		   "Pull: Pull Google Doc into buffer"
		   "Images: Cache pulled remote images"
		   "Open: Open linked Google Doc in browser"
		   "Comments: Import comments"
		   "Account: Authenticate Google account"))))

(ert-deftest org-google-docs-mode-enables-org-comments-mode ()
  "Google Docs mode enables `org-comments-mode' for linked Org buffers."
  (with-temp-buffer
    (org-mode)
    (insert ":PROPERTIES:\n:GDOCS_DOCUMENT_ID: doc-123\n:END:\n\nBody\n")
    (goto-char (point-min))
    (let ((org-google-docs-enable-org-comments-mode t))
      (org-google-docs-mode 1)
      (should org-google-docs-mode)
      (should org-comments-mode)
      (should org-google-docs--enabled-org-comments-mode)
      (org-google-docs-mode -1)
      (should-not org-google-docs-mode)
      (should-not org-comments-mode))))

(ert-deftest org-google-docs-mode-maybe-detects-linked-org-buffer ()
  "Google Docs mode auto-activation uses file-level gdocs metadata."
  (with-temp-buffer
    (org-mode)
    (insert ":PROPERTIES:\n:GDOCS_DOCUMENT_ID: doc-123\n:END:\n\nBody\n")
    (goto-char (point-min))
    (let ((org-google-docs-enable-org-comments-mode nil))
      (org-google-docs-mode-maybe)
      (should org-google-docs-mode))))

(ert-deftest org-google-docs-mode-does-not-disable-preexisting-org-comments-mode ()
  "Google Docs mode leaves pre-existing `org-comments-mode' enabled."
  (with-temp-buffer
    (org-mode)
    (insert ":PROPERTIES:\n:GDOCS_DOCUMENT_ID: doc-123\n:END:\n\nBody\n")
    (goto-char (point-min))
    (org-comments-mode 1)
    (let ((org-google-docs-enable-org-comments-mode t))
      (org-google-docs-mode 1)
      (should org-comments-mode)
      (should-not org-google-docs--enabled-org-comments-mode)
      (org-google-docs-mode -1)
      (should org-comments-mode))))

(ert-deftest org-google-docs-mode-map-binds-dispatch-by-default ()
  "Google Docs mode binds one package-native dispatch command by default."
  (let ((org-google-docs-keymap-prefix "C-c C-x G"))
    (org-google-docs-rebuild-mode-map)
    (should (eq (lookup-key org-google-docs-mode-map (kbd "C-c C-x G"))
		#'org-google-docs-dispatch))))

(provide 'org-google-docs-entrypoints-test)
;;; org-google-docs-entrypoints-test.el ends here
