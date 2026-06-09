;;; agenda.el --- Org mode: agenda configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Agenda paths and setup helpers for Org mode.

;;; Code:

(require 'seq)

(defcustom hub/org-agenda-file-names '("hubert.org" "inbox.org" "gcal-gmail.org")
  "Agenda file names relative to `org-directory'."
  :type '(repeat string)
  :group 'hub/org)

(defun hub/org--normalize-agenda-files (value)
  "Return VALUE as a list of file paths, or nil.
Accepts a single string or a list of strings."
  (cond
   ((null value) nil)
   ((stringp value) (list value))
   ((listp value) value)
   (t nil)))

(defun hub/org-existing-agenda-files ()
  "Return agenda files that currently exist on disk.
Uses `org-agenda-files' when set, otherwise `hub/org-agenda-file-names'."
  (let* ((candidates (or (hub/org--normalize-agenda-files (bound-and-true-p org-agenda-files))
			 (mapcar (lambda (file)
				   (expand-file-name file org-directory))
				 hub/org-agenda-file-names)))
	 (expanded (mapcar #'expand-file-name candidates)))
    (seq-filter #'file-exists-p expanded)))

(defun hub/org-prune-missing-agenda-files (&optional quiet)
  "Remove missing files from `org-agenda-files'.
When QUIET is non-nil, do not emit informational messages."
  (interactive)
  (let* ((candidates (or (hub/org--normalize-agenda-files org-agenda-files)
			 (mapcar (lambda (file)
				   (expand-file-name file org-directory))
				 hub/org-agenda-file-names)))
	 (expanded (mapcar #'expand-file-name candidates))
	 (existing (seq-filter #'file-exists-p expanded))
	 (missing (seq-remove #'file-exists-p expanded)))
    (setq org-agenda-files existing)
    (unless (or quiet (null missing))
      (message "[org] removed missing agenda files: %s"
	       (mapconcat #'abbreviate-file-name missing ", ")))
    existing))

(defun hub/org-setup-agenda ()
  "Configure Org agenda paths and display behavior."
  (setq org-agenda-window-setup 'other-window
	org-agenda-files (mapcar (lambda (file)
				   (expand-file-name file org-directory))
				 hub/org-agenda-file-names))
  (hub/org-prune-missing-agenda-files t))

(provide 'org/agenda)
;;; agenda.el ends here
