;;; commands.el --- Org Confluence publish commands -*- lexical-binding: t; -*-

;;; Commentary:
;; User-facing commands for publishing Org buffers to Confluence via cfl.

;;; Code:

(require 'org)
(require 'subr-x)

(let ((dir (file-name-directory (or load-file-name buffer-file-name))))
  (unless (featurep 'org/export-confluence)
    (load (expand-file-name "export" dir) nil 'nomessage))
  (unless (featurep 'org/export-confluence-api)
    (load (expand-file-name "api" dir) nil 'nomessage)))

(defun hub/confluence-commands--write-temp-xhtml (xhtml)
  "Write XHTML to a temporary .xhtml file and return its path."
  (let ((file (make-temp-file "org-confluence-" nil ".xhtml")))
    (with-temp-file file
      (insert xhtml))
    file))

(defun hub/confluence-commands--run (command)
  "Run shell COMMAND and signal `user-error' on failure."
  (unless (hub/confluence-api--cfl-available-p)
    (user-error "Cannot find `%s' on Emacs PATH. Current PATH: %s"
		hub/confluence-api-cfl-command
		(getenv "PATH")))
  (let* ((output-buffer "*hub-confluence-cfl*")
	 (default-directory user-emacs-directory)
	 (exit-code nil))
    (with-current-buffer (get-buffer-create output-buffer)
      (erase-buffer)
      (setq exit-code
	    (process-file shell-file-name nil output-buffer nil
			  shell-command-switch command)))
    (unless (zerop exit-code)
      (let ((output (when-let* ((buffer (get-buffer output-buffer)))
		      (with-current-buffer buffer
			(string-trim (buffer-string))))))
	(user-error "Confluence publish failed with exit code %s.\nCommand: %s\nOutput: %s"
		    exit-code command (if (string-empty-p (or output "")) "<empty>" output))))
    exit-code))

(defun hub/confluence-commands--title-from-buffer ()
  "Return the Org #+TITLE value from the current buffer, or nil."
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search t))
      (when (re-search-forward "^[	]*#\\+TITLE:[	]*\\(.*?\\)[	]*$" nil t)
	(let ((title (string-trim (match-string-no-properties 1))))
	  (unless (string-empty-p title) title))))))

(defun hub/confluence-commands--asset-filename-map (assets)
  "Return export filename mapping for image ASSETS."
  (mapcar (lambda (asset)
	    (cons (plist-get asset :source-link)
		  (plist-get asset :filename)))
	  assets))

(defun hub/confluence-commands--duplicate-attachment-error-p (error)
  "Return non-nil when ERROR is cfl's duplicate attachment failure."
  (string-match-p "Cannot add a new attachment with same file name as an existing attachment"
		  (error-message-string error)))

(defun hub/confluence-commands--upload-asset (page-id asset upload-directory)
  "Upload one image ASSET to Confluence PAGE-ID from UPLOAD-DIRECTORY."
  (let ((upload-path (expand-file-name (plist-get asset :filename) upload-directory)))
    (copy-file (plist-get asset :source-path) upload-path t)
    (condition-case err
	(hub/confluence-commands--run
	 (hub/confluence-api--attachment-upload-command page-id upload-path))
      (user-error
       (if (hub/confluence-commands--duplicate-attachment-error-p err)
	   (message "Confluence attachment already exists, reusing %s"
		    (plist-get asset :filename))
	 (signal (car err) (cdr err)))))))

(defun hub/confluence-commands--upload-assets (page-id assets)
  "Upload image ASSETS to Confluence PAGE-ID."
  (when assets
    (let ((upload-directory (make-temp-file "org-confluence-assets-" t)))
      (unwind-protect
	  (dolist (asset assets)
	    (hub/confluence-commands--upload-asset page-id asset upload-directory))
	(delete-directory upload-directory t)))))

(defun hub/confluence-publish ()
  "Publish the current Org buffer to an existing Confluence page.

The buffer must contain #+CONFLUENCE_PAGE_ID.  The Org document is exported to
Confluence Storage Format XHTML and passed to `cfl page edit --file --storage'."
  (interactive)
  (let* ((page-id (hub/confluence-api--page-id-from-buffer))
	 (assets (org-confluence-image-assets))
	 (xhtml-file nil))
    (unwind-protect
	(progn
	  (setq xhtml-file
		(hub/confluence-commands--write-temp-xhtml
		 (org-confluence-export nil nil nil nil
					(list :confluence-image-filenames
					      (hub/confluence-commands--asset-filename-map assets)))))
	  (hub/confluence-commands--upload-assets page-id assets)
	  (hub/confluence-commands--run
	   (hub/confluence-api--page-update-command page-id xhtml-file))
	  (message "Published Org buffer to Confluence page %s" page-id))
      (when (and xhtml-file (file-exists-p xhtml-file))
	(delete-file xhtml-file)))))

(defun hub/confluence-publish-dwim (&optional title parent-id)
  "Publish current Org buffer to Confluence, updating or creating as needed.

When #+CONFLUENCE_PAGE_ID is present, update that page.  Otherwise require
#+CONFLUENCE_SPACE and create a new page using TITLE, prompting interactively
when needed.  Optional PARENT-ID is passed to cfl as the parent page."
  (interactive)
  (let ((page-id (hub/confluence-api--page-id-from-buffer)))
    (if page-id
	(hub/confluence-publish)
      (let ((assets (org-confluence-image-assets)))
	(when assets
	  (user-error "Image publishing requires #+CONFLUENCE_PAGE_ID in this iteration"))
	(let* ((space (hub/confluence-api--space-from-buffer))
	       (page-title (or title
			       (hub/confluence-commands--title-from-buffer)
			       (read-string "Confluence page title: ")))
	       (xhtml-file nil))
	  (unwind-protect
	      (let ((command nil))
		(setq xhtml-file (hub/confluence-commands--write-temp-xhtml (org-confluence-export)))
		(setq command (hub/confluence-api--page-create-command space page-title xhtml-file parent-id))
		(hub/confluence-commands--run command)
		(message "Created Confluence page %s in space %s" page-title space))
	    (when (and xhtml-file (file-exists-p xhtml-file))
	      (delete-file xhtml-file))))))))

(provide 'org/export-confluence-commands)
;;; commands.el ends here
