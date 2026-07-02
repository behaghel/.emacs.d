;;; org-confluence-people-store.el --- Confluence people directory helpers -*- lexical-binding: t; -*-

;;; Commentary:
;; Small Org-backed people directory helpers for resolving Confluence account IDs
;; to human-readable names.  The directory is deliberately plain Org so it can be
;; inspected, shared, and edited by hand.

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'seq)
(require 'subr-x)

(defgroup org-confluence-people-store nil
  "Confluence people directory helpers."
  :group 'org)

(defcustom org-confluence-people-store-directory
  (locate-user-emacs-file "org-confluence/")
  "Directory for the global Confluence people store.
Local people files next to source documents still take precedence for lookup."
  :type 'directory
  :group 'org-confluence-people-store)

(defcustom org-confluence-people-store-file-name "confluence-people.org"
  "File name used for Confluence people directories."
  :type 'string
  :group 'org-confluence-people-store)

(defun org-confluence-people-store-local-file (&optional directory)
  "Return local people file path for DIRECTORY or `default-directory'."
  (expand-file-name org-confluence-people-store-file-name (or directory default-directory)))

(defun org-confluence-people-store-global-file ()
  "Return global people file path under `org-confluence-people-store-directory'."
  (expand-file-name org-confluence-people-store-file-name
		    org-confluence-people-store-directory))

(defun org-confluence-people-store-files (&optional directory)
  "Return existing people files for DIRECTORY in lookup precedence order."
  (seq-filter #'file-exists-p
	      (delete-dups
	       (list (org-confluence-people-store-local-file directory)
		     (org-confluence-people-store-global-file)))))

(defun org-confluence-people-store-context-files (&optional directory)
  "Return people files to update for DIRECTORY or current context.
When DIRECTORY has a local people file, include it before the global people
file.  When DIRECTORY is nil or the local file is absent, return the existing
global people file only."
  (let ((global (org-confluence-people-store-global-file))
	(local (and directory (org-confluence-people-store-local-file directory))))
    (seq-filter #'file-exists-p
		(delete-dups (append (when local (list local)) (list global))))))

(defun org-confluence-people-store--present-string (value)
  "Return trimmed VALUE when it is a non-empty string, or nil."
  (when (stringp value)
    (let ((trimmed (string-trim value)))
      (unless (string-empty-p trimmed)
	trimmed))))

(defun org-confluence-people-store--entry-value (property)
  "Return current Org entry PROPERTY as a present string, or nil."
  (org-confluence-people-store--present-string (org-entry-get nil property)))

(defun org-confluence-people-store--entry-display-name ()
  "Return display name for the current people entry."
  (or (org-confluence-people-store--entry-value "ORG_CONFLUENCE_DISPLAY_NAME")
      (nth 4 (org-heading-components))))

(defun org-confluence-people-store--entry-unresolved-p ()
  "Return non-nil when the current people entry needs user lookup."
  (when-let* ((account-id (org-confluence-people-store--entry-value "ORG_CONFLUENCE_ACCOUNT_ID")))
    (let ((display-name (org-confluence-people-store--entry-display-name)))
      (or (not (org-confluence-people-store--present-string display-name))
	  (equal account-id display-name)))))

(defun org-confluence-people-store-mention-label (account-id &optional directory)
  "Resolve Confluence ACCOUNT-ID to a people-store mention label.
Search local people file for DIRECTORY first, then the global people file.
`ORG_CONFLUENCE_HANDLE' takes precedence over display name and heading text."
  (when-let* ((id (org-confluence-people-store--present-string account-id)))
    (cl-loop for file in (org-confluence-people-store-files directory)
	     thereis
	     (with-temp-buffer
	       (insert-file-contents file)
	       (org-mode)
	       (goto-char (point-min))
	       (cl-loop while (re-search-forward org-heading-regexp nil t)
			do (goto-char (match-beginning 0))
			when (equal id (org-entry-get nil "ORG_CONFLUENCE_ACCOUNT_ID"))
			return (let ((label (or (org-confluence-people-store--entry-value
						 "ORG_CONFLUENCE_HANDLE")
						(org-confluence-people-store--entry-display-name))))
				 (unless (equal label id)
				   label))
			do (forward-line 1))))))

(defun org-confluence-people-resolve-account-id (account-id &optional directory)
  "Resolve Confluence ACCOUNT-ID to a display name.
Search local people file for DIRECTORY first, then the global people file."
  (org-confluence-people-store-mention-label account-id directory))

(defun org-confluence-people-store--truthy-entry-value-p (property)
  "Return non-nil when current Org entry PROPERTY is truthy."
  (when-let* ((value (org-confluence-people-store--entry-value property)))
    (member (downcase value) '("t" "true" "yes" "1"))))

(defun org-confluence-people-store-current-user-p (identifier &optional directory)
  "Return non-nil when IDENTIFIER is marked as the current user.
IDENTIFIER may be a Confluence account ID or a display name.  Search all people
files visible from DIRECTORY; a local entry without `ORG_CONFLUENCE_ME' does not
suppress a global marker for the same identity."
  (when-let* ((id (org-confluence-people-store--present-string identifier)))
    (cl-loop for file in (org-confluence-people-store-files directory)
	     thereis
	     (with-temp-buffer
	       (insert-file-contents file)
	       (org-mode)
	       (goto-char (point-min))
	       (cl-loop while (re-search-forward org-heading-regexp nil t)
			do (goto-char (match-beginning 0))
			when (and (org-confluence-people-store--truthy-entry-value-p "ORG_CONFLUENCE_ME")
				  (or (equal id (org-confluence-people-store--entry-value
						 "ORG_CONFLUENCE_ACCOUNT_ID"))
				      (equal id (org-confluence-people-store--entry-display-name))))
			return t
			do (forward-line 1))))))

(defun org-confluence-people-store--ensure-file (file)
  "Ensure people directory FILE exists."
  (unless (file-exists-p file)
    (make-directory (file-name-directory file) t)
    (with-temp-file file
      (insert "#+title: Confluence People\n\n"))))

(defun org-confluence-people-store--seen-at ()
  "Return current timestamp for people cache updates."
  (format-time-string "%Y-%m-%dT%H:%M:%S%z"))

(defun org-confluence-people-store--find-account-id (account-id)
  "Move to people entry with ACCOUNT-ID in the current buffer, or return nil."
  (goto-char (point-min))
  (cl-loop while (re-search-forward org-heading-regexp nil t)
	   do (goto-char (match-beginning 0))
	   when (equal account-id (org-entry-get nil "ORG_CONFLUENCE_ACCOUNT_ID"))
	   return t
	   do (forward-line 1)))

(defun org-confluence-people-store--put-missing (property value)
  "Set PROPERTY to VALUE at point when PROPERTY is empty and VALUE is present."
  (when-let* ((present (org-confluence-people-store--present-string value)))
    (unless (org-confluence-people-store--entry-value property)
      (org-entry-put nil property present))))

(defun org-confluence-people-store-cache-identity (account-id display-name &optional email file)
  "Cache Confluence ACCOUNT-ID and DISPLAY-NAME in people FILE.
Existing non-empty properties are not overwritten.  FILE defaults to the global
people directory under `org-confluence-people-store-directory'."
  (when-let* ((id (org-confluence-people-store--present-string account-id)))
    (let ((target-file (or file (org-confluence-people-store-global-file)))
	  (name (or (org-confluence-people-store--present-string display-name) id)))
      (org-confluence-people-store--ensure-file target-file)
      (with-temp-buffer
	(insert-file-contents target-file)
	(org-mode)
	(unless (org-confluence-people-store--find-account-id id)
	  (goto-char (point-max))
	  (unless (bolp) (insert "\n"))
	  (insert (format "* %s\n:PROPERTIES:\n:END:\n" name))
	  (forward-line -2))
	(org-confluence-people-store--put-missing "ORG_CONFLUENCE_DISPLAY_NAME" name)
	(org-confluence-people-store--put-missing "ORG_CONFLUENCE_ACCOUNT_ID" id)
	(org-confluence-people-store--put-missing "ORG_CONFLUENCE_EMAIL" email)
	(org-confluence-people-store--put-missing "ORG_CONFLUENCE_SOURCE" "confluence")
	(org-confluence-people-store--put-missing "ORG_CONFLUENCE_SEEN_AT" (org-confluence-people-store--seen-at))
	(write-region (point-min) (point-max) target-file nil 'silent))
      target-file)))

(defun org-confluence-people-store-mark-me (account-id display-name &optional email file)
  "Cache ACCOUNT-ID and mark it as the current user in people FILE.
DISPLAY-NAME and EMAIL are cached conservatively.  Existing manual display names
are preserved.  FILE defaults to the global people directory."
  (when-let* ((target-file (org-confluence-people-store-cache-identity
			    account-id display-name email file)))
    (with-temp-buffer
      (insert-file-contents target-file)
      (org-mode)
      (unless (org-confluence-people-store--find-account-id account-id)
	(user-error "Could not find cached Confluence account %s" account-id))
      (org-entry-put nil "ORG_CONFLUENCE_ME" "t")
      (write-region (point-min) (point-max) target-file nil 'silent))
    target-file))

(defun org-confluence-people-store-unresolved-account-ids (&optional files)
  "Return unresolved Confluence account IDs from people FILES."
  (delete-dups
   (apply #'append
	  (mapcar
	   (lambda (file)
	     (with-temp-buffer
	       (insert-file-contents file)
	       (org-mode)
	       (let (ids)
		 (goto-char (point-min))
		 (while (re-search-forward org-heading-regexp nil t)
		   (goto-char (match-beginning 0))
		   (when (org-confluence-people-store--entry-unresolved-p)
		     (push (org-confluence-people-store--entry-value "ORG_CONFLUENCE_ACCOUNT_ID") ids))
		   (forward-line 1))
		 (nreverse ids))))
	   files))))

(defun org-confluence-people-store--put-resolved (property value &optional replace-placeholder)
  "Set PROPERTY to VALUE when missing or REPLACE-PLACEHOLDER allows it."
  (when-let* ((present (org-confluence-people-store--present-string value)))
    (let ((current (org-confluence-people-store--entry-value property)))
      (when (or (not current)
		(and replace-placeholder (equal current replace-placeholder)))
	(org-entry-put nil property present)))))

(defun org-confluence-people-store-update-identity (file account-id display-name &optional email status time-zone)
  "Update existing ACCOUNT-ID entry in FILE with resolved identity details.
Return non-nil when an entry was found.  Non-empty manual values are preserved,
except display names equal to ACCOUNT-ID, which are treated as placeholders."
  (when (and (file-exists-p file)
	     (org-confluence-people-store--present-string account-id))
    (with-temp-buffer
      (insert-file-contents file)
      (org-mode)
      (let ((found (org-confluence-people-store--find-account-id account-id)))
	(when found
	  (org-confluence-people-store--put-resolved
	   "ORG_CONFLUENCE_DISPLAY_NAME" display-name account-id)
	  (org-confluence-people-store--put-resolved "ORG_CONFLUENCE_EMAIL" email)
	  (org-confluence-people-store--put-resolved "ORG_CONFLUENCE_ACCOUNT_STATUS" status)
	  (org-confluence-people-store--put-resolved "ORG_CONFLUENCE_TIME_ZONE" time-zone)
	  (write-region (point-min) (point-max) file nil 'silent))
	found))))

(provide 'org-confluence-people-store)
;;; org-confluence-people-store.el ends here
