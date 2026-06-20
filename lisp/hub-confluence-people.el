;;; hub-confluence-people.el --- Confluence people directory helpers -*- lexical-binding: t; -*-

;;; Commentary:
;; Small Org-backed people directory helpers for resolving Confluence account IDs
;; to human-readable names.  The directory is deliberately plain Org so it can be
;; inspected, shared, and edited by hand.

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'seq)
(require 'subr-x)

(defgroup hub/confluence-people nil
  "Confluence people directory helpers."
  :group 'org)

(defcustom hub/confluence-people-file-name "confluence-people.org"
  "File name used for Confluence people directories."
  :type 'string
  :group 'hub/confluence-people)

(defun hub/confluence-people-local-file (&optional directory)
  "Return local people file path for DIRECTORY or `default-directory'."
  (expand-file-name hub/confluence-people-file-name (or directory default-directory)))

(defun hub/confluence-people-global-file ()
  "Return global people file path under `org-directory'."
  (expand-file-name hub/confluence-people-file-name org-directory))

(defun hub/confluence-people-files (&optional directory)
  "Return existing people files for DIRECTORY in lookup precedence order."
  (seq-filter #'file-exists-p
	      (delete-dups
	       (list (hub/confluence-people-local-file directory)
		     (hub/confluence-people-global-file)))))

(defun hub/confluence-people-context-files (&optional directory)
  "Return people files to update for DIRECTORY or current context.
When DIRECTORY has a local people file, include it before the global people
file.  When DIRECTORY is nil or the local file is absent, return the existing
global people file only."
  (let ((global (hub/confluence-people-global-file))
	(local (and directory (hub/confluence-people-local-file directory))))
    (seq-filter #'file-exists-p
		(delete-dups (append (when local (list local)) (list global))))))

(defun hub/confluence-people--present-string (value)
  "Return trimmed VALUE when it is a non-empty string, or nil."
  (when (stringp value)
    (let ((trimmed (string-trim value)))
      (unless (string-empty-p trimmed)
	trimmed))))

(defun hub/confluence-people--entry-value (property)
  "Return current Org entry PROPERTY as a present string, or nil."
  (hub/confluence-people--present-string (org-entry-get nil property)))

(defun hub/confluence-people--entry-display-name ()
  "Return display name for the current people entry."
  (or (hub/confluence-people--entry-value "HUB_PERSON_DISPLAY_NAME")
      (nth 4 (org-heading-components))))

(defun hub/confluence-people--entry-unresolved-p ()
  "Return non-nil when the current people entry needs user lookup."
  (when-let* ((account-id (hub/confluence-people--entry-value "HUB_CONFLUENCE_ACCOUNT_ID")))
    (let ((display-name (hub/confluence-people--entry-display-name)))
      (or (not (hub/confluence-people--present-string display-name))
	  (equal account-id display-name)))))

(defun hub/confluence-people-resolve-account-id (account-id &optional directory)
  "Resolve Confluence ACCOUNT-ID to a display name.
Search local people file for DIRECTORY first, then the global people file."
  (when-let* ((id (hub/confluence-people--present-string account-id)))
    (cl-loop for file in (hub/confluence-people-files directory)
	     thereis
	     (with-temp-buffer
	       (insert-file-contents file)
	       (org-mode)
	       (goto-char (point-min))
	       (cl-loop while (re-search-forward org-heading-regexp nil t)
			do (goto-char (match-beginning 0))
			when (equal id (org-entry-get nil "HUB_CONFLUENCE_ACCOUNT_ID"))
			return (hub/confluence-people--entry-display-name)
			do (forward-line 1))))))

(defun hub/confluence-people--truthy-entry-value-p (property)
  "Return non-nil when current Org entry PROPERTY is truthy."
  (when-let* ((value (hub/confluence-people--entry-value property)))
    (member (downcase value) '("t" "true" "yes" "1"))))

(defun hub/confluence-people-current-user-p (identifier &optional directory)
  "Return non-nil when IDENTIFIER is marked as the current user.
IDENTIFIER may be a Confluence account ID or a display name.  Search all people
files visible from DIRECTORY; a local entry without `HUB_PERSON_ME' does not
suppress a global marker for the same identity."
  (when-let* ((id (hub/confluence-people--present-string identifier)))
    (cl-loop for file in (hub/confluence-people-files directory)
	     thereis
	     (with-temp-buffer
	       (insert-file-contents file)
	       (org-mode)
	       (goto-char (point-min))
	       (cl-loop while (re-search-forward org-heading-regexp nil t)
			do (goto-char (match-beginning 0))
			when (and (hub/confluence-people--truthy-entry-value-p "HUB_PERSON_ME")
				  (or (equal id (hub/confluence-people--entry-value
						 "HUB_CONFLUENCE_ACCOUNT_ID"))
				      (equal id (hub/confluence-people--entry-display-name))))
			return t
			do (forward-line 1))))))

(defun hub/confluence-people--ensure-file (file)
  "Ensure people directory FILE exists."
  (unless (file-exists-p file)
    (make-directory (file-name-directory file) t)
    (with-temp-file file
      (insert "#+title: Confluence People\n\n"))))

(defun hub/confluence-people--seen-at ()
  "Return current timestamp for people cache updates."
  (format-time-string "%Y-%m-%dT%H:%M:%S%z"))

(defun hub/confluence-people--find-account-id (account-id)
  "Move to people entry with ACCOUNT-ID in the current buffer, or return nil."
  (goto-char (point-min))
  (cl-loop while (re-search-forward org-heading-regexp nil t)
	   do (goto-char (match-beginning 0))
	   when (equal account-id (org-entry-get nil "HUB_CONFLUENCE_ACCOUNT_ID"))
	   return t
	   do (forward-line 1)))

(defun hub/confluence-people--put-missing (property value)
  "Set PROPERTY to VALUE at point when PROPERTY is empty and VALUE is present."
  (when-let* ((present (hub/confluence-people--present-string value)))
    (unless (hub/confluence-people--entry-value property)
      (org-entry-put nil property present))))

(defun hub/confluence-people-cache-identity (account-id display-name &optional email file)
  "Cache Confluence ACCOUNT-ID and DISPLAY-NAME in people FILE.
Existing non-empty properties are not overwritten.  FILE defaults to the global
people directory under `org-directory'."
  (when-let* ((id (hub/confluence-people--present-string account-id)))
    (let ((target-file (or file (hub/confluence-people-global-file)))
	  (name (or (hub/confluence-people--present-string display-name) id)))
      (hub/confluence-people--ensure-file target-file)
      (with-temp-buffer
	(insert-file-contents target-file)
	(org-mode)
	(unless (hub/confluence-people--find-account-id id)
	  (goto-char (point-max))
	  (unless (bolp) (insert "\n"))
	  (insert (format "* %s\n:PROPERTIES:\n:END:\n" name))
	  (forward-line -2))
	(hub/confluence-people--put-missing "HUB_PERSON_DISPLAY_NAME" name)
	(hub/confluence-people--put-missing "HUB_CONFLUENCE_ACCOUNT_ID" id)
	(hub/confluence-people--put-missing "HUB_PERSON_EMAIL" email)
	(hub/confluence-people--put-missing "HUB_PERSON_SOURCE" "confluence")
	(hub/confluence-people--put-missing "HUB_PERSON_SEEN_AT" (hub/confluence-people--seen-at))
	(write-region (point-min) (point-max) target-file nil 'silent))
      target-file)))

(defun hub/confluence-people-mark-me (account-id display-name &optional email file)
  "Cache ACCOUNT-ID and mark it as the current user in people FILE.
DISPLAY-NAME and EMAIL are cached conservatively.  Existing manual display names
are preserved.  FILE defaults to the global people directory."
  (when-let* ((target-file (hub/confluence-people-cache-identity
			    account-id display-name email file)))
    (with-temp-buffer
      (insert-file-contents target-file)
      (org-mode)
      (unless (hub/confluence-people--find-account-id account-id)
	(user-error "Could not find cached Confluence account %s" account-id))
      (org-entry-put nil "HUB_PERSON_ME" "t")
      (write-region (point-min) (point-max) target-file nil 'silent))
    target-file))

(defun hub/confluence-people-unresolved-account-ids (&optional files)
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
		   (when (hub/confluence-people--entry-unresolved-p)
		     (push (hub/confluence-people--entry-value "HUB_CONFLUENCE_ACCOUNT_ID") ids))
		   (forward-line 1))
		 (nreverse ids))))
	   files))))

(defun hub/confluence-people--put-resolved (property value &optional replace-placeholder)
  "Set PROPERTY to VALUE when missing or REPLACE-PLACEHOLDER allows it."
  (when-let* ((present (hub/confluence-people--present-string value)))
    (let ((current (hub/confluence-people--entry-value property)))
      (when (or (not current)
		(and replace-placeholder (equal current replace-placeholder)))
	(org-entry-put nil property present)))))

(defun hub/confluence-people-update-identity (file account-id display-name &optional email status time-zone)
  "Update existing ACCOUNT-ID entry in FILE with resolved identity details.
Return non-nil when an entry was found.  Non-empty manual values are preserved,
except display names equal to ACCOUNT-ID, which are treated as placeholders."
  (when (and (file-exists-p file)
	     (hub/confluence-people--present-string account-id))
    (with-temp-buffer
      (insert-file-contents file)
      (org-mode)
      (let ((found (hub/confluence-people--find-account-id account-id)))
	(when found
	  (hub/confluence-people--put-resolved
	   "HUB_PERSON_DISPLAY_NAME" display-name account-id)
	  (hub/confluence-people--put-resolved "HUB_PERSON_EMAIL" email)
	  (hub/confluence-people--put-resolved "HUB_PERSON_ACCOUNT_STATUS" status)
	  (hub/confluence-people--put-resolved "HUB_PERSON_TIME_ZONE" time-zone)
	  (write-region (point-min) (point-max) file nil 'silent))
	found))))

(provide 'hub-confluence-people)
;;; hub-confluence-people.el ends here
