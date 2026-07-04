;;; org-google-docs.el --- Org Google Docs adapter facade -*- lexical-binding: t; -*-

;;; Commentary:
;; Safe-loading facade for a Google Docs publishing adapter.  Body sync is
;; delegated to upstream gdocs when it is installed; this package adds a stable
;; Org-facing command surface and later comment integration points.

;;; Code:

(require 'subr-x)

(autoload 'org-google-docs-comments-import "org-google-docs-comments-import" nil t)
(require 'org-google-docs-comments-backend)
(require 'org-google-docs-footnotes)
(require 'org-google-docs-images)

(defgroup org-google-docs nil
  "Org-facing Google Docs publishing adapter."
  :group 'org)

(defcustom org-google-docs-keymap-prefix "C-c C-x G"
  "Key sequence used to bind `org-google-docs-dispatch' in the mode map.
Set this to nil before rebuilding the map to leave the mode map unbound."
  :type '(choice (const :tag "No default binding" nil) string)
  :group 'org-google-docs)

(defvar org-google-docs-mode-map (make-sparse-keymap)
  "Keymap for `org-google-docs-mode'.")

(defun org-google-docs--upstream-command (command)
  "Return upstream gdocs COMMAND symbol or signal an actionable error."
  (unless (fboundp command)
    (user-error "Missing upstream gdocs command `%s'; install and configure benthamite/gdocs" command))
  command)

(defun org-google-docs--call-upstream (command)
  "Call upstream gdocs COMMAND interactively after checking it exists."
  (call-interactively (org-google-docs--upstream-command command)))

(defun org-google-docs--diagnostic-message (diagnostic)
  "Return a human-readable message for DIAGNOSTIC."
  (or (plist-get diagnostic :message)
      (format "%s" (plist-get diagnostic :code))))

(defun org-google-docs--preflight-footnotes-for-push ()
  "Return a native footnote push plan or signal blocking diagnostics."
  (let ((plan (org-google-docs-footnotes-plan-buffer)))
    (when (plist-get plan :diagnostics)
      (user-error "Google Docs footnote preflight failed: %s"
		  (string-join
		   (mapcar #'org-google-docs--diagnostic-message
			   (plist-get plan :diagnostics))
		   "; ")))
    plan))

(defun org-google-docs--preflight-images-for-push ()
  "Return a native image push plan or signal blocking diagnostics."
  (let ((plan (org-google-docs-images-plan-buffer)))
    (when (plist-get plan :diagnostics)
      (user-error "Google Docs image preflight failed: %s"
		  (string-join
		   (mapcar #'org-google-docs--diagnostic-message
			   (plist-get plan :diagnostics))
		   "; ")))
    (when (plist-get plan :images)
      (user-error "Google Docs image push is not wired yet; refusing to drop %d standalone image(s)"
		  (length (plist-get plan :images))))
    plan))

(defun org-google-docs--require-upstream-library (library)
  "Require upstream gdocs LIBRARY or signal an actionable error."
  (unless (require library nil 'noerror)
    (user-error "Missing upstream gdocs library `%s'; install and configure benthamite/gdocs" library)))

(defconst org-google-docs--dispatch-actions
  '(("Status: Doctor" . org-google-docs-doctor)
    ("Status: Upstream gdocs status" . org-google-docs-status)
    ("Sync: Sync current buffer" . org-google-docs-sync-current)
    ("Publish: Create Google Doc from buffer" . org-google-docs-create)
    ("Publish: Push buffer to Google Docs" . org-google-docs-push)
    ("Pull: Pull Google Doc into buffer" . org-google-docs-pull)
    ("Open: Open linked Google Doc in browser" . org-google-docs-open)
    ("Comments: Import comments" . org-google-docs-comments-import)
    ("Account: Authenticate Google account" . org-google-docs-authenticate))
  "Action labels and commands used by `org-google-docs-dispatch'.")

(defun org-google-docs--accounts-configured-p ()
  "Return non-nil when upstream gdocs accounts appear configured."
  (and (boundp 'gdocs-accounts)
       (symbol-value 'gdocs-accounts)))

(defun org-google-docs--doctor-message (issues)
  "Return a human-readable doctor message for ISSUES."
  (if issues
      (format "Org Google Docs: %s"
	      (string-join
	       (mapcar (lambda (issue)
			 (pcase issue
			   (:missing-gdocs "upstream gdocs library is not installed or not on load-path")
			   (:missing-accounts "gdocs-accounts is not configured")
			   (_ (format "%s" issue))))
		       issues)
	       "; "))
    "Org Google Docs: basic setup looks available"))

;;;###autoload
(defun org-google-docs-doctor ()
  "Report basic Google Docs adapter setup status.
Return a list of issue symbols."
  (interactive)
  (let (issues)
    (unless (locate-library "gdocs")
      (push :missing-gdocs issues))
    (unless (org-google-docs--accounts-configured-p)
      (push :missing-accounts issues))
    (setq issues (nreverse issues))
    (message "%s" (org-google-docs--doctor-message issues))
    issues))

;;;###autoload
(defun org-google-docs-create ()
  "Create a Google Doc from the current Org buffer via upstream gdocs."
  (interactive)
  (org-google-docs--call-upstream 'gdocs-create))

;;;###autoload
(defun org-google-docs-push ()
  "Push the current Org buffer to its linked Google Doc via upstream gdocs.
When named Org footnotes are present, preflight them and enable native Google
Docs footnote creation through the local gdocs conversion seam."
  (interactive)
  (org-google-docs--preflight-images-for-push)
  (let ((plan (org-google-docs--preflight-footnotes-for-push)))
    (when (plist-get plan :references)
      (org-google-docs--require-upstream-library 'gdocs-convert)
      (org-google-docs--require-upstream-library 'gdocs-api)
      (org-google-docs-footnotes-begin-push plan))
    (condition-case err
	(org-google-docs--call-upstream 'gdocs-push)
      ((error quit)
       (org-google-docs-footnotes--deactivate-session)
       (signal (car err) (cdr err))))))

;;;###autoload
(defun org-google-docs-pull ()
  "Pull the linked Google Doc into the current Org buffer via upstream gdocs."
  (interactive)
  (org-google-docs--call-upstream 'gdocs-pull))

;;;###autoload
(defun org-google-docs-sync-current ()
  "Synchronize the current Org buffer with Google Docs.
The initial adapter delegates to upstream `gdocs-push' as the explicit body-sync
action.  Richer stale-check behavior can be added once the wrapper has proven
usable with real linked buffers."
  (interactive)
  (org-google-docs--call-upstream 'gdocs-push))

;;;###autoload
(defun org-google-docs-open ()
  "Open the linked Google Doc in a browser via upstream gdocs."
  (interactive)
  (org-google-docs--call-upstream 'gdocs-open-in-browser))

;;;###autoload
(defun org-google-docs-status ()
  "Show upstream gdocs status for the current Org buffer."
  (interactive)
  (org-google-docs--call-upstream 'gdocs-status))

;;;###autoload
(defun org-google-docs-authenticate ()
  "Ensure a Google account has a usable upstream gdocs access token.
Unlike upstream `gdocs-authenticate', this command does not force a new browser
OAuth flow when a valid token already exists."
  (interactive)
  (org-google-docs--require-upstream-library 'gdocs-auth)
  (let ((account (gdocs-auth-select-account "Google account: ")))
    (gdocs-auth-get-access-token
     account
     (lambda (_token)
       (message "Google Docs account %s is authenticated" account))
     (lambda (message)
       (user-error "%s" message)))))

;;;###autoload
(defun org-google-docs-dispatch ()
  "Prompt for an Org Google Docs action and run it."
  (interactive)
  (let* ((label (completing-read "Org Google Docs: "
				 (mapcar #'car org-google-docs--dispatch-actions)
				 nil t))
	 (command (alist-get label org-google-docs--dispatch-actions nil nil #'equal)))
    (unless command
      (user-error "Unknown Org Google Docs action: %s" label))
    (call-interactively command)))

;;;###autoload
(define-minor-mode org-google-docs-mode
  "Minor mode for Org buffers linked to Google Docs."
  :lighter " GDocs"
  :keymap org-google-docs-mode-map)

(defun org-google-docs-rebuild-mode-map ()
  "Rebuild `org-google-docs-mode-map' from `org-google-docs-keymap-prefix'."
  (setq org-google-docs-mode-map (make-sparse-keymap))
  (when org-google-docs-keymap-prefix
    (define-key org-google-docs-mode-map
		(kbd org-google-docs-keymap-prefix)
		#'org-google-docs-dispatch)))

(org-google-docs-rebuild-mode-map)

(provide 'org-google-docs)
;;; org-google-docs.el ends here
