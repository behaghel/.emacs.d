;;; org-google-docs.el --- Org Google Docs adapter facade -*- lexical-binding: t; -*-

;;; Commentary:
;; Safe-loading facade for a Google Docs publishing adapter.  Body sync is
;; delegated to upstream gdocs when it is installed; this package adds a stable
;; Org-facing command surface and later comment integration points.

;;; Code:

(require 'org-comments)
(require 'pp)
(require 'seq)
(require 'subr-x)
(require 'url-util)

(autoload 'org-google-docs-comments-import "org-google-docs-comments-import" nil t)
(require 'org-google-docs-comments-backend)
(require 'org-sync nil 'noerror)
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
(declare-function gdocs-sync-push "gdocs-sync" ())

(defgroup org-google-docs nil
  "Org-facing Google Docs publishing adapter."
  :group 'org)

(defcustom org-google-docs-keymap-prefix "C-c C-x G"
  "Key sequence used to bind `org-google-docs-dispatch' in the mode map.
Set this to nil before rebuilding the map to leave the mode map unbound."
  :type '(choice (const :tag "No default binding" nil) string)
  :group 'org-google-docs)

(defcustom org-google-docs-enable-org-comments-mode t
  "Whether `org-google-docs-mode' also enables `org-comments-mode'."
  :type 'boolean
  :group 'org-google-docs)

(defvar org-google-docs-mode-map (make-sparse-keymap)
  "Keymap for `org-google-docs-mode'.")

(defvar-local org-google-docs--enabled-org-comments-mode nil
  "Non-nil when `org-google-docs-mode' enabled `org-comments-mode'.")

(defvar-local org-google-docs--restyle-revision nil
  "One-shot style revision marker used to force block restyling.")

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

(defun org-google-docs--annotate-restyle-revision (ir)
  "Annotate block elements in IR with the current restyle revision."
  (if (not org-google-docs--restyle-revision)
      ir
    (mapcar (lambda (element)
	      (if (memq (plist-get element :type)
			'(source-block quote-block callout-block))
		  (plist-put (copy-sequence element)
			     :style-revision org-google-docs--restyle-revision)
		element))
	    ir)))

(defun org-google-docs--around-convert-org-buffer-to-ir (orig &rest args)
  "Advise ORIG to add one-shot style revision metadata."
  (org-google-docs--annotate-restyle-revision (apply orig args)))

(defun org-google-docs--clear-restyle-revision (&rest _args)
  "Clear one-shot block restyle state after push completion."
  (setq org-google-docs--restyle-revision nil))

(defun org-google-docs-enable-restyle-advice ()
  "Enable block restyle advice for upstream gdocs."
  (when (fboundp 'gdocs-convert-org-buffer-to-ir)
    (advice-add 'gdocs-convert-org-buffer-to-ir
		:around #'org-google-docs--around-convert-org-buffer-to-ir))
  (when (fboundp 'gdocs-sync--handle-push-success)
    (advice-add 'gdocs-sync--handle-push-success
		:after #'org-google-docs--clear-restyle-revision))
  (when (fboundp 'gdocs-sync--handle-push-error)
    (advice-add 'gdocs-sync--handle-push-error
		:after #'org-google-docs--clear-restyle-revision)))

;;;###autoload
(defun org-google-docs-push-restyle-current ()
  "Push current buffer while forcing block style reapplication.
This is intended for style-policy tuning.  It marks local block IR with a
one-shot style revision so the diff replaces/restyles those blocks even when
text has not changed."
  (interactive)
  (org-google-docs--require-upstream-library 'gdocs-convert)
  (org-google-docs--require-upstream-library 'gdocs-diff)
  (org-google-docs--require-upstream-library 'gdocs-sync)
  (org-google-docs-enable-restyle-advice)
  (setq org-google-docs--restyle-revision
	(format-time-string "%Y%m%dT%H%M%S%z"))
  (org-google-docs-push))

(defconst org-google-docs--dispatch-actions
  '(("Status: Doctor" . org-google-docs-doctor)
    ("Status: Upstream gdocs status" . org-google-docs-status)
    ("Debug: Pipeline trace" . org-google-docs-debug-pipeline)
    ("Sync: Sync current buffer" . org-google-docs-sync-current)
    ("Publish: Create Google Doc from buffer" . org-google-docs-create)
    ("Publish: Push buffer to Google Docs" . org-google-docs-push)
    ("Publish: Restyle typographic blocks" . org-google-docs-push-restyle-current)
    ("Pull: Pull Google Doc into buffer" . org-google-docs-pull)
    ("Images: Cache pulled remote images" . org-google-docs-images-cache-remote-images)
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
(defun org-google-docs-buffer-p (&optional buffer)
  "Return non-nil when BUFFER is an Org buffer linked to Google Docs."
  (with-current-buffer (or buffer (current-buffer))
    (and (derived-mode-p 'org-mode)
	 (org-google-docs-comments-document-id))))

;;;###autoload
(defun org-google-docs-mode-maybe ()
  "Enable `org-google-docs-mode' for Org buffers with Google Docs metadata."
  (when (org-google-docs-buffer-p)
    (org-google-docs-mode 1)))

;;;###autoload
(define-minor-mode org-google-docs-mode
  "Minor mode for Org buffers linked to Google Docs."
  :lighter " GDocs"
  :keymap org-google-docs-mode-map
  (cond
   (org-google-docs-mode
    (unless (derived-mode-p 'org-mode)
      (setq org-google-docs-mode nil)
      (user-error "org-google-docs-mode only works in Org buffers"))
    (org-google-docs-comments-backend-register)
    (when (and org-google-docs-enable-org-comments-mode
	       (not (bound-and-true-p org-comments-mode)))
      (org-comments-mode 1)
      (setq org-google-docs--enabled-org-comments-mode t)))
   (t
    (when (and org-google-docs--enabled-org-comments-mode
	       (bound-and-true-p org-comments-mode))
      (org-comments-mode -1))
    (setq org-google-docs--enabled-org-comments-mode nil))))

(defun org-google-docs-rebuild-mode-map ()
  "Rebuild `org-google-docs-mode-map' from `org-google-docs-keymap-prefix'."
  (setq org-google-docs-mode-map (make-sparse-keymap))
  (when org-google-docs-keymap-prefix
    (define-key org-google-docs-mode-map
		(kbd org-google-docs-keymap-prefix)
		#'org-google-docs-dispatch)))

(org-google-docs-rebuild-mode-map)

(defun org-google-docs-remote--await-document (document-id &optional timeout)
  "Return Google Docs API document for DOCUMENT-ID, waiting up to TIMEOUT seconds."
  (unless (fboundp 'gdocs-api-get-document)
    (user-error "gdocs-api-get-document is not available"))
  (let ((done nil)
	(doc nil)
	(error-message nil))
    (gdocs-api-get-document
     document-id
     (lambda (json)
       (setq doc json)
       (setq done t))
     nil
     (lambda (message)
       (setq error-message message)
       (setq done t)))
    (let ((deadline (+ (float-time) (or timeout 5.0))))
      (while (and (not done) (< (float-time) deadline))
	(accept-process-output nil 0.05)))
    (cond
     (doc doc)
     (error-message (user-error "Google Docs metadata failed: %s" error-message))
     (t (user-error "Google Docs metadata timed out for %s" document-id)))))

(defun org-google-docs-remote--document-title (doc)
  "Return title from Google Docs API DOC."
  (or (alist-get 'title doc)
      (cdr (assoc 'title doc))
      (and (hash-table-p doc) (gethash "title" doc))))

(defun org-google-docs-remote-describe-url (url)
  "Return org-sync descriptor for Google Docs URL, or nil."
  (let ((trimmed (string-trim url)))
    (when (string-match "docs\\.google\\.com/document/d/\\([^/?#]+\\)" trimmed)
      (let* ((id (match-string 1 trimmed))
	     (title (ignore-errors
		      (org-google-docs-remote--document-title
		       (org-google-docs-remote--await-document id 5.0)))))
	(list :kind "google-docs"
	      :id id
	      :url trimmed
	      :title title
	      :supports '(:pull t :activity nil :comments nil))))))

(defun org-google-docs-remote-pull (document-id local-path &rest _options)
  "Pull Google Docs DOCUMENT-ID to LOCAL-PATH and return normalized metadata."
  (unless (fboundp 'org-google-docs-pull)
    (user-error "org-google-docs-pull is not available"))
  (let ((target-file (expand-file-name local-path))
	pulled-revision)
    (make-directory (file-name-directory target-file) t)
    (with-current-buffer (find-file-noselect target-file)
      (erase-buffer)
      (org-mode)
      (insert "#+TITLE: Google Doc\n")
      (insert ":PROPERTIES:\n")
      (insert (format ":GDOCS_DOCUMENT_ID: %s\n" document-id))
      (insert ":END:\n\n")
      (save-buffer)
      (cond
       ((fboundp 'gdocs-sync--init-from-properties)
	(gdocs-sync--init-from-properties))
       ((boundp 'gdocs-sync--document-id)
	(setq-local gdocs-sync--document-id document-id)))
      (when (and (fboundp 'gdocs-mode) (not (bound-and-true-p gdocs-mode)))
	(let ((gdocs-auto-pull-on-open nil))
	  (gdocs-mode 1)))
      (when (and (boundp 'gdocs-sync--document-id)
		 (not (bound-and-true-p gdocs-sync--document-id)))
	(setq-local gdocs-sync--document-id document-id))
      (org-google-docs-pull)
      (setq pulled-revision (and (boundp 'gdocs-sync--revision-id) gdocs-sync--revision-id))
      (save-buffer))
    (list :kind "google-docs"
	  :id document-id
	  :local-path target-file
	  :baseline (list :revision pulled-revision
			  :pulled-at (format-time-string "%FT%TZ" nil t)))))

(defun org-google-docs-remote--await-file-metadata (document-id &optional timeout)
  "Return Google Drive metadata for DOCUMENT-ID, waiting up to TIMEOUT seconds."
  (unless (fboundp 'gdocs-api-get-file-metadata)
    (user-error "gdocs-api-get-file-metadata is not available"))
  (let ((done nil)
	(metadata nil)
	(error-message nil))
    (gdocs-api-get-file-metadata
     document-id
     (lambda (json)
       (setq metadata json)
       (setq done t))
     nil)
    (let ((deadline (+ (float-time) (or timeout 5.0))))
      (while (and (not done) (< (float-time) deadline))
	(accept-process-output nil 0.05)))
    (cond
     (metadata metadata)
     (error-message (user-error "Google Drive metadata failed: %s" error-message))
     (t (user-error "Google Drive metadata timed out for %s" document-id)))))

(defun org-google-docs-remote--metadata-value (key metadata)
  "Return KEY from Google Drive METADATA alist or hash table."
  (or (alist-get key metadata)
      (cdr (assoc key metadata))
      (and (hash-table-p metadata) (gethash (symbol-name key) metadata))))

(defun org-google-docs-remote--local-file-keyword (path key)
  "Return Org file-level property KEY from PATH, or nil."
  (when (and path (file-exists-p path))
    (with-temp-buffer
      (insert-file-contents path)
      (org-mode)
      (goto-char (point-min))
      (org-entry-get nil key))))

(defun org-google-docs-remote--time-after-p (a b)
  "Return non-nil when timestamp A is after timestamp B."
  (when (and a b)
    (condition-case nil
	(time-less-p (date-to-time b) (date-to-time a))
      (error (not (equal a b))))))

(defun org-google-docs-remote-scan (document-id baseline &rest options)
  "Scan Google Docs DOCUMENT-ID against BASELINE and return normalized activity."
  (let* ((metadata (org-google-docs-remote--await-file-metadata document-id 5.0))
	 (remote-revision (org-google-docs-remote--metadata-value 'headRevisionId metadata))
	 (remote-modified-at (org-google-docs-remote--metadata-value 'modifiedTime metadata))
	 (remote-name (org-google-docs-remote--metadata-value 'name metadata))
	 (local-path (plist-get options :local-path))
	 (local-revision (or (plist-get baseline :revision)
			     (plist-get baseline :remote-revision)
			     (org-google-docs-remote--local-file-keyword local-path "GDOCS_REVISION_ID")))
	 (local-modified-at (or (plist-get baseline :modified-at)
				(plist-get baseline :remote-modified-at)))
	 (local-pulled-at (plist-get baseline :pulled-at))
	 (changed (cond
		   ((and remote-revision local-revision)
		    (not (equal remote-revision local-revision)))
		   ((and remote-modified-at local-modified-at)
		    (org-google-docs-remote--time-after-p remote-modified-at local-modified-at))
		   ((and remote-modified-at local-pulled-at)
		    (org-google-docs-remote--time-after-p remote-modified-at local-pulled-at))
		   (t nil)))
	 (signal-id (or (and remote-revision (format "google-docs:%s:rev:%s" document-id remote-revision))
			(and remote-modified-at (format "google-docs:%s:modified:%s" document-id remote-modified-at))))
	 (signals nil))
    (when (and changed signal-id)
      (setq signals
	    (list (list :id signal-id
			:kind "remote-update"
			:remote-at (or remote-modified-at "")
			:author "unknown"
			:summary (if remote-name
				     (format "Google Doc '%s' changed remotely." remote-name)
				   "Google Doc changed remotely.")))))
    (list :kind "google-docs"
	  :id document-id
	  :remote (list :revision remote-revision
			:modified-at remote-modified-at
			:title remote-name)
	  :signals signals)))

(when (fboundp 'org-sync-remote-register-provider)
  (org-sync-remote-register-provider
   :kind "google-docs"
   :describe-url #'org-google-docs-remote-describe-url
   :pull #'org-google-docs-remote-pull
   :scan #'org-google-docs-remote-scan))

(provide 'org-google-docs)
;;; org-google-docs.el ends here
