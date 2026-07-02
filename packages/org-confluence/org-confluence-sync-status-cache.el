;;; org-confluence-sync-status-cache.el --- Confluence sync status cache -*- lexical-binding: t; -*-

;;; Commentary:
;; Cache file naming, JSON persistence, timestamps, and staleness handling for
;; Confluence sync status reports.

;;; Code:

(require 'json)

(require 'org-confluence-api)

(defcustom org-confluence-sync-status-stale-after-days 3
  "Number of days before cached Confluence sync status is considered stale."
  :type 'number
  :group 'org-confluence-api)

(defun org-confluence-sync-status--cache-file (&optional source-file)
  "Return hidden Confluence sync cache path for SOURCE-FILE."
  (let* ((file (or source-file buffer-file-name))
	 (directory (and file (file-name-directory file)))
	 (base (and file (file-name-sans-extension (file-name-nondirectory file)))))
    (unless (and directory base)
      (user-error "Current buffer is not visiting a file"))
    (expand-file-name (format ".%s.confluence.cache" base) directory)))

(defun org-confluence-sync-status--timestamp ()
  "Return current timestamp for Confluence sync status cache."
  (format-time-string "%Y-%m-%dT%H:%M:%S%z"))

(defun org-confluence-sync-status--timestamp-age-days (timestamp)
  "Return age in days for TIMESTAMP, or nil when it cannot be parsed."
  (when-let* ((time (ignore-errors (date-to-time timestamp))))
    (/ (float-time (time-subtract (current-time) time)) 86400.0)))

(defun org-confluence-sync-status--read-cache (&optional source-file)
  "Return cached Confluence sync status for SOURCE-FILE, or nil."
  (let ((cache-file (org-confluence-sync-status--cache-file source-file)))
    (when (file-readable-p cache-file)
      (condition-case nil
	  (with-temp-buffer
	    (insert-file-contents cache-file)
	    (json-parse-buffer :object-type 'alist :array-type 'list))
	(error nil)))))

(defun org-confluence-sync-status--write-cache (status &optional source-file)
  "Write Confluence sync STATUS cache next to SOURCE-FILE."
  (let ((cache-file (org-confluence-sync-status--cache-file source-file)))
    (with-temp-file cache-file
      (insert (json-encode status) "\n"))))

(defun org-confluence-sync-status--cache-state (&optional source-file)
  "Return cached sync state for SOURCE-FILE accounting for staleness."
  (if-let* ((status (org-confluence-sync-status--read-cache source-file)))
      (let* ((checked-at (alist-get 'checkedAt status))
	     (age (org-confluence-sync-status--timestamp-age-days checked-at)))
	(if (and age (> age org-confluence-sync-status-stale-after-days))
	    `((state . "unknown")
	      (summary . ,(format "stale %sd" (floor age)))
	      (checkedAt . ,checked-at)
	      (stale . t))
	  status))
    '((state . "unknown")
      (summary . "unchecked")
      (unchecked . t))))

(provide 'org-confluence-sync-status-cache)
;;; org-confluence-sync-status-cache.el ends here
