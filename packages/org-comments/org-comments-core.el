;;; org-comments-core.el --- Core helpers for Org comments -*- lexical-binding: t; -*-

;;; Commentary:
;; Core customization and identity helpers for the initial Org comments
;; extraction.  Names remain in the legacy `org-comments-*' namespace until
;; the public API migration slice.

;;; Code:

(require 'subr-x)

(defgroup org-comments nil
  "Region-targeted Org sidecar comments."
  :group 'org)

(defcustom org-comments-heading-preview-length 60
  "Maximum normalized target text length used in sidecar headings."
  :type 'natnum
  :group 'org-comments)

(defcustom org-comments-heading-target-preview-length 48
  "Maximum normalized target text length used in comment headings."
  :type 'natnum
  :group 'org-comments)

(defcustom org-comments-heading-body-preview-length 60
  "Maximum normalized body text length used in comment headings."
  :type 'natnum
  :group 'org-comments)

(defcustom org-comments-author nil
  "Author name used for newly created local comments.
When nil, comments fall back to Org metadata and then Emacs user identity."
  :type '(choice (const :tag "Infer author" nil) string)
  :group 'org-comments)

(defcustom org-comments-resolve-account-id-function nil
  "Function resolving a remote account id to a display name.
The function is called with ACCOUNT-ID and optional DIRECTORY, and should return
a string display name or nil.  Backends can set this callback to resolve
provider-specific identities without adding backend dependencies to
`org-comments'."
  :type '(choice (const :tag "No resolver" nil) function)
  :group 'org-comments)

(defun org-comments-resolve-account-id (account-id &optional directory)
  "Resolve ACCOUNT-ID to a display name using optional DIRECTORY context."
  (when (and account-id org-comments-resolve-account-id-function)
    (funcall org-comments-resolve-account-id-function account-id directory)))

(defun org-comments--random-hex ()
  "Return a short random hexadecimal suffix for local comment IDs."
  (format "%06x" (random #x1000000)))

(defun org-comments-generate-id ()
  "Return a stable local comment ID."
  (format "local-%s-%s" (format-time-string "%Y%m%dT%H%M%S") (org-comments--random-hex)))

(defun org-comments--present-string (value)
  "Return trimmed VALUE when it is a non-empty string, or nil."
  (when (stringp value)
    (let ((trimmed (string-trim value)))
      (unless (string-empty-p trimmed)
	trimmed))))

(defun org-comments--keyword-from-buffer (keyword)
  "Return Org KEYWORD value from the current buffer, or nil."
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search t)
	  (regexp (format "^[	]*#\\+%s:[	]*\\(.*?\\)[	]*$"
			  (regexp-quote keyword))))
      (when (re-search-forward regexp nil t)
	(org-comments--present-string (match-string-no-properties 1))))))

(defun org-comments-current-author ()
  "Return the best available author name for a new local comment."
  (or (org-comments--present-string org-comments-author)
      (org-comments--keyword-from-buffer "AUTHOR")
      (org-comments--keyword-from-buffer "EMAIL")
      (org-comments--present-string user-full-name)
      (org-comments--present-string user-mail-address)
      (org-comments--present-string user-login-name)
      "unknown"))

(defun org-comments-current-created-at ()
  "Return an ISO-like timestamp for a new local comment."
  (format-time-string "%Y-%m-%dT%H:%M:%S%z"))

(defun org-comments--truncate (text length)
  "Return TEXT truncated to LENGTH with ellipsis when needed."
  (if (> (length text) length)
      (concat (substring text 0 length) "…")
    text))

(defun org-comments-import-report--count (report key)
  "Return numeric KEY count from import REPORT, defaulting to zero."
  (or (plist-get report key) 0))

(defun org-comments-import-report-format (report)
  "Return a concise provider-neutral summary for import REPORT.
REPORT is a plist with optional `:provider', `:added', `:updated',
`:skipped-resolved', and `:preserved-local' keys."
  (let* ((provider (or (plist-get report :provider) "Remote"))
	 (parts (list
		 (format "added %s" (org-comments-import-report--count report :added))
		 (format "updated %s" (org-comments-import-report--count report :updated))))
	 (added-replies (org-comments-import-report--count report :added-replies))
	 (skipped-resolved (org-comments-import-report--count report :skipped-resolved)))
    (when (> added-replies 0)
      (setq parts (append parts (list (format "added replies %s" added-replies)))))
    (let ((updated-replies (org-comments-import-report--count report :updated-replies)))
      (when (> updated-replies 0)
	(setq parts (append parts (list (format "updated replies %s" updated-replies))))))
    (let ((remote-resolved (org-comments-import-report--count report :remote-resolved)))
      (when (> remote-resolved 0)
	(setq parts (append parts (list (format "remote resolved %s" remote-resolved))))))
    (let ((remote-missing (org-comments-import-report--count report :remote-missing)))
      (when (> remote-missing 0)
	(setq parts (append parts (list (format "remote missing %s" remote-missing))))))
    (let ((missing-replies (org-comments-import-report--count report :remote-missing-replies)))
      (when (> missing-replies 0)
	(setq parts (append parts (list (format "remote missing replies %s" missing-replies))))))
    (when (> skipped-resolved 0)
      (setq parts (append parts (list (format "skipped resolved %s" skipped-resolved)))))
    (concat provider " comments: " (string-join parts ", ")
	    (when (plist-get report :preserved-local)
	      "; local content preserved"))))

(defun org-comments-import-report-message (report)
  "Show and return a provider-neutral import REPORT message."
  (message "%s" (org-comments-import-report-format report)))

(defun org-comments-sync-report-format (report)
  "Return a concise provider-neutral summary for sync or push REPORT."
  (let* ((provider (or (plist-get report :provider) "Remote"))
	 (parts nil))
    (dolist (spec '((:pushed-replies . "pushed replies")
		    (:resolved . "resolved")
		    (:pushed-statuses . "pushed statuses")
		    (:already-pushed . "already pushed")
		    (:no-op . "no changes")))
      (let ((count (org-comments-import-report--count report (car spec))))
	(when (> count 0)
	  (setq parts (append parts (list (format "%s %s" (cdr spec) count)))))))
    (concat provider " comments: " (if parts (string-join parts ", ") "no changes"))))

(defun org-comments-sync-report-message (report)
  "Show and return a provider-neutral sync or push REPORT message."
  (message "%s" (org-comments-sync-report-format report)))

(defun org-comments-sync-state (comment)
  "Return provider-neutral sync state for COMMENT."
  (cond
   ((eq (plist-get comment :remote-state) 'missing) 'remote-missing)
   ((plist-get comment :local-updated-at) 'edited)
   ((plist-get comment :remote-id) 'synced)
   (t 'local-only)))

(defun org-comments-sync-state-label (comment)
  "Return a concise provider-neutral sync state label for COMMENT."
  (pcase (org-comments-sync-state comment)
    ('remote-missing "remote missing")
    ('edited "edited locally")
    ('synced "synced")
    ('local-only "unsynced")))

(provide 'org-comments-core)
;;; org-comments-core.el ends here
