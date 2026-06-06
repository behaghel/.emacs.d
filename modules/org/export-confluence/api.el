;;; api.el --- cfl shell wrappers for confluence export -*- lexical-binding: t; -*-

;;; Commentary:
;; Build and run cfl commands for the Org -> Confluence publish flow.

;;; Code:

(require 'org)
(require 'seq)
(require 'subr-x)

(defgroup hub/confluence-api nil
  "Customizations for publishing Org documents to Confluence."
  :group 'hub)

(defcustom hub/confluence-api-cfl-command "cfl"
  "Command used to invoke the cfl Confluence CLI."
  :type 'string
  :group 'hub/confluence-api)

(defun hub/confluence-api--cfl-available-p ()
  "Return non-nil when configured cfl command is available on Emacs PATH."
  (and (executable-find hub/confluence-api-cfl-command) t))

(defun hub/confluence-api--present-string-p (value)
  "Return non-nil when VALUE is a non-empty string after trimming."
  (and (stringp value) (not (string-empty-p (string-trim value)))))

(defun hub/confluence-api--require-string (value description)
  "Return trimmed VALUE or signal `user-error' mentioning DESCRIPTION."
  (unless (hub/confluence-api--present-string-p value)
    (user-error "Missing Confluence %s" description))
  (string-trim value))

(defun hub/confluence-api--shell-join (parts)
  "Join command PARTS into a shell command string."
  (string-join (mapcar #'shell-quote-argument (seq-filter #'identity parts)) " "))

(defun hub/confluence-api--file-flag (file-path)
  "Return a cfl --file argument list for FILE-PATH, or nil."
  (when (hub/confluence-api--present-string-p file-path)
    (list "--file" (string-trim file-path))))

(defun hub/confluence-api--page-update-command (page-id &optional file-path)
  "Build a cfl command to update PAGE-ID from optional XHTML FILE-PATH."
  (let ((id (hub/confluence-api--require-string page-id "page ID")))
    (hub/confluence-api--shell-join
     (append (list hub/confluence-api-cfl-command "page" "edit" id)
	     (hub/confluence-api--file-flag file-path)
	     (list "--storage")))))

(defun hub/confluence-api--attachment-upload-command (page-id file-path)
  "Build a cfl command to upload FILE-PATH as an attachment to PAGE-ID."
  (let ((id (hub/confluence-api--require-string page-id "page ID"))
	(file (hub/confluence-api--require-string file-path "attachment file")))
    (hub/confluence-api--shell-join
     (list hub/confluence-api-cfl-command "attachment" "upload" "--page" id "--file" file))))

(defun hub/confluence-api--page-create-command (space title &optional file-path parent-id)
  "Build a cfl command to create TITLE in SPACE from optional XHTML FILE-PATH.

When PARENT-ID is non-nil, include it as the new page's parent."
  (let ((space-key (hub/confluence-api--require-string space "space"))
	(page-title (hub/confluence-api--require-string title "page title")))
    (hub/confluence-api--shell-join
     (append (list hub/confluence-api-cfl-command
		   "page" "create"
		   "--space" space-key
		   "--title" page-title)
	     (hub/confluence-api--file-flag file-path)
	     (when (hub/confluence-api--present-string-p parent-id)
	       (list "--parent" (string-trim parent-id)))
	     (list "--storage")))))

(defun hub/confluence-api--keyword-from-buffer (keyword)
  "Return the value for Org KEYWORD in the current buffer, or nil."
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search t)
	  (regexp (format "^[	]*#\\+%s:[	]*\\(.*?\\)[	]*$" (regexp-quote keyword))))
      (when (re-search-forward regexp nil t)
	(let ((value (string-trim (match-string-no-properties 1))))
	  (unless (string-empty-p value) value))))))

(defun hub/confluence-api--page-id-from-buffer ()
  "Return the #+CONFLUENCE_PAGE_ID value from the current buffer, or nil."
  (hub/confluence-api--keyword-from-buffer "CONFLUENCE_PAGE_ID"))

(defun hub/confluence-api--space-from-buffer ()
  "Return the #+CONFLUENCE_SPACE value from the current buffer, or nil."
  (hub/confluence-api--keyword-from-buffer "CONFLUENCE_SPACE"))

(provide 'org/export-confluence-api)
;;; api.el ends here
