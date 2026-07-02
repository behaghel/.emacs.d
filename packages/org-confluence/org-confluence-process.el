;;; org-confluence-process.el --- Confluence process helpers -*- lexical-binding: t; -*-

;;; Commentary:
;; Generic helpers for running cfl shell commands and writing temporary XHTML.

;;; Code:

(require 'subr-x)

(require 'org-confluence-api)

(defcustom org-confluence-process-working-directory nil
  "Directory used as the working directory for Confluence CLI commands.
When nil, use the current buffer's `default-directory'."
  :type '(choice (const :tag "Current default-directory" nil)
		 directory)
  :group 'org-confluence-api)

(defvar org-confluence-process-output-buffer "*org-confluence-cfl*"
  "Buffer name used for Confluence CLI command output.")

(defun org-confluence-process-write-temp-xhtml (xhtml)
  "Write XHTML to a temporary .xhtml file and return its path."
  (let ((file (make-temp-file "org-confluence-" nil ".xhtml")))
    (with-temp-file file
      (insert xhtml))
    file))

(defun org-confluence-process-run-process (command output-buffer)
  "Run shell COMMAND into OUTPUT-BUFFER and return its exit code."
  (with-current-buffer (get-buffer-create output-buffer)
    (erase-buffer)
    (process-file shell-file-name nil output-buffer nil shell-command-switch command)))

(defun org-confluence-process-command-output (output-buffer)
  "Return trimmed contents of OUTPUT-BUFFER."
  (when-let* ((buffer (get-buffer output-buffer)))
    (with-current-buffer buffer
      (string-trim (buffer-string)))))

(defun org-confluence-process-run (command)
  "Run shell COMMAND and signal `user-error' on failure."
  (unless (org-confluence-api--cfl-available-p)
    (user-error "Cannot find `%s' on Emacs PATH. Current PATH: %s"
		org-confluence-api-cfl-command
		(getenv "PATH")))
  (let* ((output-buffer org-confluence-process-output-buffer)
	 (default-directory (or org-confluence-process-working-directory
				default-directory))
	 (exit-code (org-confluence-process-run-process command output-buffer)))
    (unless (zerop exit-code)
      (let ((output (org-confluence-process-command-output output-buffer)))
	(user-error "Confluence publish failed with exit code %s.\nCommand: %s\nOutput: %s"
		    exit-code command (if (string-empty-p (or output "")) "<empty>" output))))
    exit-code))

(defun org-confluence-process-run-output (command)
  "Run shell COMMAND and return its trimmed output."
  (unless (org-confluence-api--cfl-available-p)
    (user-error "Cannot find `%s' on Emacs PATH. Current PATH: %s"
		org-confluence-api-cfl-command
		(getenv "PATH")))
  (let* ((output-buffer org-confluence-process-output-buffer)
	 (default-directory (or org-confluence-process-working-directory
				default-directory))
	 (exit-code (org-confluence-process-run-process command output-buffer))
	 (output (org-confluence-process-command-output output-buffer)))
    (unless (zerop exit-code)
      (user-error "Confluence command failed with exit code %s.\nCommand: %s\nOutput: %s"
		  exit-code command (if (string-empty-p (or output "")) "<empty>" output)))
    output))

(provide 'org-confluence-process)
;;; org-confluence-process.el ends here
