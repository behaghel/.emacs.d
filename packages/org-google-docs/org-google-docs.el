;;; org-google-docs.el --- Org Google Docs adapter facade -*- lexical-binding: t; -*-

;;; Commentary:
;; Safe-loading facade for a Google Docs publishing adapter.  Body sync is
;; delegated to upstream gdocs when it is installed; this package adds a stable
;; Org-facing command surface and later comment integration points.

;;; Code:

(require 'pp)
(require 'seq)
(require 'subr-x)
(require 'url-util)

(autoload 'org-google-docs-comments-import "org-google-docs-comments-import" nil t)
(require 'org-google-docs-comments-backend)
(require 'org-google-docs-footnotes)
(require 'org-google-docs-images)

(declare-function gdocs-api-get-document "gdocs-api"
		  (document-id callback &optional account on-error))
(declare-function gdocs-convert-docs-json-to-ir "gdocs-convert" (json))
(declare-function gdocs-convert-ir-to-docs-requests "gdocs-convert"
		  (ir &optional start-index))
(declare-function gdocs-convert-org-buffer-to-ir "gdocs-convert" ())
(declare-function gdocs-diff-generate "gdocs-diff"
		  (old-ir new-ir &optional start-index))

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
    plan))

(defun org-google-docs--require-upstream-library (library)
  "Require upstream gdocs LIBRARY or signal an actionable error."
  (unless (require library nil 'noerror)
    (user-error "Missing upstream gdocs library `%s'; install and configure benthamite/gdocs" library)))

(defconst org-google-docs-debug-buffer-name "*org-google-docs-debug*"
  "Buffer name used for Google Docs pipeline traces.")

(defun org-google-docs--safe-diagnostic-value (thunk)
  "Return diagnostic value produced by THUNK, or an error plist."
  (condition-case err
      (funcall thunk)
    ((error quit)
     (list :error (error-message-string err)))))

(defun org-google-docs--bound-value (symbol)
  "Return SYMBOL value when bound, otherwise nil."
  (and (boundp symbol) (symbol-value symbol)))

(defun org-google-docs--debug-pretty (value)
  "Return a pretty printable representation of VALUE."
  (with-temp-buffer
    (let ((print-length nil)
	  (print-level nil))
      (pp value (current-buffer)))
    (string-trim-right (buffer-string))))

(defun org-google-docs--debug-insert-section (title value)
  "Insert debug section TITLE containing VALUE at point."
  (insert (format "\n* %s\n\n" title))
  (insert "#+begin_src emacs-lisp\n")
  (insert (org-google-docs--debug-pretty value))
  (insert "\n#+end_src\n"))

(defun org-google-docs--debug-placeholder-uri (path)
  "Return a non-fetching placeholder image URI for PATH."
  (format "https://example.invalid/org-google-docs-debug/%s"
	  (url-hexify-string (file-name-nondirectory path))))

(defun org-google-docs--debug-enrich-image-ir (element images)
  "Return ELEMENT enriched with placeholder image URIs from IMAGES."
  (if (not (eq (plist-get element :type) 'image))
      element
    (if-let* ((image (seq-find (lambda (candidate)
				 (equal (plist-get candidate :path)
					(plist-get element :path)))
			       images)))
	(plist-put (copy-sequence element)
		   :uri (org-google-docs--debug-placeholder-uri
			 (plist-get image :path)))
      element)))

(defun org-google-docs--debug-enrich-ir-with-placeholder-uris (ir image-plan)
  "Return IR with placeholder image URIs from IMAGE-PLAN for request tracing."
  (mapcar (lambda (element)
	    (org-google-docs--debug-enrich-image-ir
	     element (plist-get image-plan :images)))
	  ir))

(defun org-google-docs--debug-local-snapshot ()
  "Return a local Google Docs pipeline diagnostic snapshot."
  (let* ((footnote-plan
	  (org-google-docs--safe-diagnostic-value
	   #'org-google-docs-footnotes-plan-buffer))
	 (image-plan
	  (org-google-docs--safe-diagnostic-value
	   #'org-google-docs-images-plan-buffer))
	 (local-ir
	  (when (fboundp 'gdocs-convert-org-buffer-to-ir)
	    (org-google-docs--safe-diagnostic-value
	     #'gdocs-convert-org-buffer-to-ir)))
	 (local-ir-with-placeholder-image-uris
	  (when (and local-ir
		     (not (plist-get local-ir :error)))
	    (org-google-docs--debug-enrich-ir-with-placeholder-uris
	     local-ir image-plan)))
	 (local-requests
	  (when (and local-ir
		     (not (plist-get local-ir :error))
		     (fboundp 'gdocs-convert-ir-to-docs-requests))
	    (org-google-docs--safe-diagnostic-value
	     (lambda ()
	       (gdocs-convert-ir-to-docs-requests local-ir)))))
	 (local-requests-with-placeholder-image-uris
	  (when (and local-ir-with-placeholder-image-uris
		     (fboundp 'gdocs-convert-ir-to-docs-requests))
	    (org-google-docs--safe-diagnostic-value
	     (lambda ()
	       (gdocs-convert-ir-to-docs-requests
		local-ir-with-placeholder-image-uris))))))
    (list :buffer-name (buffer-name)
	  :buffer-file buffer-file-name
	  :default-directory default-directory
	  :major-mode major-mode
	  :document-id (org-google-docs--bound-value 'gdocs-sync--document-id)
	  :account (org-google-docs--current-gdocs-account)
	  :push-in-progress (org-google-docs--bound-value
			     'gdocs-sync--push-in-progress)
	  :push-queued (org-google-docs--bound-value 'gdocs-sync--push-queued)
	  :footnote-count (length (plist-get footnote-plan :references))
	  :image-count (length (plist-get image-plan :images))
	  :footnote-plan footnote-plan
	  :image-plan image-plan
	  :local-ir local-ir
	  :local-requests local-requests
	  :local-requests-note
	  "Raw local requests do not include image requests until upload adds fetchable URIs. Use :local-requests-with-placeholder-image-uris to inspect image and caption request generation without network upload."
	  :local-requests-with-placeholder-image-uris
	  local-requests-with-placeholder-image-uris)))

(defun org-google-docs--debug-write-local (source-buffer snapshot include-remote)
  "Write local SNAPSHOT for SOURCE-BUFFER and INCLUDE-REMOTE flag."
  (let ((debug-buffer (get-buffer-create org-google-docs-debug-buffer-name)))
    (with-current-buffer debug-buffer
      (let ((inhibit-read-only t))
	(erase-buffer)
	(insert "#+title: Org Google Docs pipeline trace\n")
	(insert (format "#+date: %s\n"
			(format-time-string "%Y-%m-%d %H:%M:%S %z")))
	(insert (format "#+source-buffer: %s\n"
			(buffer-name source-buffer)))
	(insert (format "#+remote-fetch-requested: %S\n" include-remote))
	(org-google-docs--debug-insert-section "Local snapshot" snapshot)
	(special-mode)))
    debug-buffer))

(defun org-google-docs--debug-append-remote (debug-buffer remote-snapshot)
  "Append REMOTE-SNAPSHOT to DEBUG-BUFFER."
  (with-current-buffer debug-buffer
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (org-google-docs--debug-insert-section "Remote snapshot" remote-snapshot))))

(defun org-google-docs--debug-remote-snapshot (json local-ir)
  "Return remote pipeline snapshot from document JSON and LOCAL-IR."
  (let* ((remote-ir
	  (when (fboundp 'gdocs-convert-docs-json-to-ir)
	    (org-google-docs--safe-diagnostic-value
	     (lambda () (gdocs-convert-docs-json-to-ir json)))))
	 (diff-requests
	  (when (and remote-ir
		     local-ir
		     (not (plist-get remote-ir :error))
		     (not (plist-get local-ir :error))
		     (fboundp 'gdocs-diff-generate))
	    (org-google-docs--safe-diagnostic-value
	     (lambda () (gdocs-diff-generate remote-ir local-ir))))))
    (list :remote-json json
	  :remote-ir remote-ir
	  :diff-requests diff-requests)))

;;;###autoload
(defun org-google-docs-debug-pipeline (&optional include-remote)
  "Write a Google Docs push/pull pipeline trace for the current Org buffer.
With prefix argument INCLUDE-REMOTE, also fetch the linked Google Doc and append
remote JSON, remote IR, and local-vs-remote diff requests asynchronously.  The
trace intentionally avoids authentication token data; it may contain document
content from the local buffer and fetched Google Doc."
  (interactive "P")
  (require 'gdocs-convert nil 'noerror)
  (require 'gdocs-diff nil 'noerror)
  (let* ((source-buffer (current-buffer))
	 (snapshot (org-google-docs--debug-local-snapshot))
	 (debug-buffer (org-google-docs--debug-write-local
			source-buffer snapshot include-remote))
	 (document-id (plist-get snapshot :document-id))
	 (account (plist-get snapshot :account))
	 (local-ir (plist-get snapshot :local-ir)))
    (if include-remote
	(progn
	  (unless document-id
	    (user-error "Buffer is not linked to a Google Doc"))
	  (org-google-docs--require-upstream-library 'gdocs-api)
	  (org-google-docs--require-upstream-library 'gdocs-convert)
	  (org-google-docs--require-upstream-library 'gdocs-diff)
	  (gdocs-api-get-document
	   document-id
	   (lambda (json)
	     (org-google-docs--debug-append-remote
	      debug-buffer
	      (org-google-docs--debug-remote-snapshot json local-ir))
	     (display-buffer debug-buffer)
	     (message "Org Google Docs pipeline trace updated with remote state"))
	   account))
      (display-buffer debug-buffer)
      (message "Org Google Docs pipeline trace written"))
    debug-buffer))

(defconst org-google-docs--dispatch-actions
  '(("Status: Doctor" . org-google-docs-doctor)
    ("Status: Upstream gdocs status" . org-google-docs-status)
    ("Debug: Pipeline trace" . org-google-docs-debug-pipeline)
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

(defun org-google-docs--prepare-footnotes-for-body-write ()
  "Preflight and activate native footnote support for a body write."
  (let ((plan (org-google-docs--preflight-footnotes-for-push)))
    (when (plist-get plan :references)
      (org-google-docs--require-upstream-library 'gdocs-convert)
      (org-google-docs--require-upstream-library 'gdocs-api)
      (org-google-docs-footnotes-begin-push plan))))

(defun org-google-docs--prepare-images-then (callback)
  "Preflight and upload native images, then call CALLBACK in this buffer."
  (let ((image-plan (org-google-docs--preflight-images-for-push))
	(buffer (current-buffer)))
    (when (plist-get image-plan :images)
      (org-google-docs--require-upstream-library 'gdocs-api)
      (org-google-docs--require-upstream-library 'gdocs-convert))
    (org-google-docs-images-begin-push
     image-plan
     (lambda ()
       (if (buffer-live-p buffer)
	   (with-current-buffer buffer
	     (funcall callback))
	 (user-error "Google Docs image upload finished after source buffer was killed")))
     (org-google-docs--current-gdocs-account))))

;;;###autoload
(defun org-google-docs-create ()
  "Create a Google Doc from the current Org buffer via upstream gdocs."
  (interactive)
  (org-google-docs--prepare-images-then
   (lambda ()
     (org-google-docs--prepare-footnotes-for-body-write)
     (org-google-docs--call-upstream 'gdocs-create))))

(defun org-google-docs--current-gdocs-account ()
  "Return current upstream gdocs account for this buffer, or nil."
  (and (boundp 'gdocs-sync--account)
       (symbol-value 'gdocs-sync--account)))

(defun org-google-docs--push-after-image-upload ()
  "Push current buffer after image upload/preparation has completed."
  (condition-case err
      (progn
	(org-google-docs--prepare-footnotes-for-body-write)
	(org-google-docs--call-upstream 'gdocs-push))
    ((error quit)
     (org-google-docs-images-deactivate-session)
     (org-google-docs-footnotes--deactivate-session)
     (signal (car err) (cdr err)))))

;;;###autoload
(defun org-google-docs-push ()
  "Push the current Org buffer to its linked Google Doc via upstream gdocs.
When named Org footnotes are present, preflight them and enable native Google
Docs footnote creation through the local gdocs conversion seam.  When standalone
Org images are present, upload them first and enrich image IR with fetchable
Google Drive URIs for native inline-image insertion."
  (interactive)
  (org-google-docs--prepare-images-then
   #'org-google-docs--push-after-image-upload))

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
