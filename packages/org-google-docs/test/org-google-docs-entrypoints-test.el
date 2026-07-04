;;; org-google-docs-entrypoints-test.el --- Org Google Docs entrypoint tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Smoke tests for the safe-loading Org Google Docs adapter facade.

;;; Code:

(require 'cl-lib)
(require 'ert)

(add-to-list 'load-path (expand-file-name ".." (file-name-directory load-file-name)))

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

(ert-deftest org-google-docs-push-blocks-standalone-images-before-upstream ()
  "Push refuses image-bearing buffers until native image insertion is wired."
  (let ((image-file (make-temp-file "org-google-docs-image" nil ".png"))
	calls)
    (unwind-protect
	(cl-letf (((symbol-function 'gdocs-push)
		   (lambda () (interactive) (push 'push calls))))
	  (with-temp-buffer
	    (insert (format "[[file:%s]]\n" image-file))
	    (org-mode)
	    (should-error (org-google-docs-push) :type 'user-error)
	    (should-not calls)))
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
		   "Sync: Sync current buffer"
		   "Publish: Create Google Doc from buffer"
		   "Publish: Push buffer to Google Docs"
		   "Pull: Pull Google Doc into buffer"
		   "Open: Open linked Google Doc in browser"
		   "Comments: Import comments"
		   "Account: Authenticate Google account"))))

(ert-deftest org-google-docs-mode-map-binds-dispatch-by-default ()
  "Google Docs mode binds one package-native dispatch command by default."
  (let ((org-google-docs-keymap-prefix "C-c C-x G"))
    (org-google-docs-rebuild-mode-map)
    (should (eq (lookup-key org-google-docs-mode-map (kbd "C-c C-x G"))
		#'org-google-docs-dispatch))))

(provide 'org-google-docs-entrypoints-test)
;;; org-google-docs-entrypoints-test.el ends here
