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
	 (exit-code (shell-command command output-buffer output-buffer)))
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

(defun hub/confluence-publish ()
  "Publish the current Org buffer to an existing Confluence page.

The buffer must contain #+CONFLUENCE_PAGE_ID.  The Org document is exported to
Confluence Storage Format XHTML and passed to `cfl page edit --file --storage'."
  (interactive)
  (let* ((page-id (hub/confluence-api--page-id-from-buffer))
	 (xhtml-file (hub/confluence-commands--write-temp-xhtml (org-confluence-export)))
	 (command (hub/confluence-api--page-update-command page-id xhtml-file)))
    (hub/confluence-commands--run command)
    (delete-file xhtml-file)
    (message "Published Org buffer to Confluence page %s" page-id)))

(defun hub/confluence-publish-dwim (&optional title parent-id)
  "Publish current Org buffer to Confluence, updating or creating as needed.

When #+CONFLUENCE_PAGE_ID is present, update that page.  Otherwise require
#+CONFLUENCE_SPACE and create a new page using TITLE, prompting interactively
when needed.  Optional PARENT-ID is passed to cfl as the parent page."
  (interactive)
  (let ((page-id (hub/confluence-api--page-id-from-buffer)))
    (if page-id
	(hub/confluence-publish)
      (let* ((space (hub/confluence-api--space-from-buffer))
	     (page-title (or title
			     (hub/confluence-commands--title-from-buffer)
			     (read-string "Confluence page title: ")))
	     (xhtml-file (hub/confluence-commands--write-temp-xhtml (org-confluence-export)))
	     (command (hub/confluence-api--page-create-command space page-title xhtml-file parent-id)))
	(hub/confluence-commands--run command)
	(delete-file xhtml-file)
	(message "Created Confluence page %s in space %s" page-title space)))))

(provide 'org/export-confluence-commands)
;;; commands.el ends here
