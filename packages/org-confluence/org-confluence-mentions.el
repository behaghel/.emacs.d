;;; org-confluence-mentions.el --- Confluence user mention helpers -*- lexical-binding: t; -*-

;;; Commentary:
;; Shared helpers for round-tripping Confluence user mentions between storage
;; XHTML and Org links.

;;; Code:

(require 'json)
(require 'ol)
(require 'seq)
(require 'subr-x)
(require 'xml)

(require 'org-confluence-api)
(require 'org-confluence-people-store)

(org-link-set-parameters "confluence-user")

(defun org-confluence-mentions--present-string (value)
  "Return trimmed VALUE when VALUE is a non-empty string."
  (when (stringp value)
    (let ((trimmed (string-trim value)))
      (unless (string-empty-p trimmed)
	trimmed))))

(defun org-confluence-mentions--ensure-at (label)
  "Return LABEL with a leading @ when LABEL is present."
  (when-let* ((text (org-confluence-mentions--present-string label)))
    (if (string-prefix-p "@" text)
	text
      (concat "@" text))))

(defun org-confluence-mentions-label-for-account-id (account-id &optional directory fallback)
  "Return an Org-visible mention label for ACCOUNT-ID.
DIRECTORY selects the local/global people stores.  FALLBACK is used when the
people store cannot resolve ACCOUNT-ID.  The people store takes precedence over
FALLBACK."
  (when-let* ((id (org-confluence-mentions--present-string account-id)))
    (org-confluence-mentions--ensure-at
     (or (org-confluence-people-store-mention-label id directory)
	 (org-confluence-mentions--present-string fallback)
	 id))))

(defun org-confluence-mentions-org-link (account-id &optional directory fallback)
  "Return an Org link for Confluence ACCOUNT-ID mention.
DIRECTORY and FALLBACK are passed to
`org-confluence-mentions-label-for-account-id'."
  (when-let* ((id (org-confluence-mentions--present-string account-id))
	      (label (org-confluence-mentions-label-for-account-id id directory fallback)))
    (format "[[confluence-user:%s][%s]]" id label)))

(defun org-confluence-mentions-storage-link (account-id)
  "Return Confluence storage XHTML for a user mention ACCOUNT-ID."
  (when-let* ((id (org-confluence-mentions--present-string account-id)))
    (format "<ac:link><ri:user ri:account-id=\"%s\" /></ac:link>"
	    (xml-escape-string id))))

(defun org-confluence-mentions-extract-account-ids (storage)
  "Return unique Confluence user account IDs mentioned in STORAGE XHTML."
  (let ((start 0)
	ids)
    (while (and (stringp storage)
		(string-match "ri:account-id=\\(['\"]\\)\\([^'\"]+\\)\\1" storage start))
      (push (match-string 2 storage) ids)
      (setq start (match-end 0)))
    (delete-dups (nreverse ids))))

(defun org-confluence-mentions-unresolved-account-ids (storage &optional directory)
  "Return account IDs in STORAGE that are not resolved for DIRECTORY."
  (seq-filter (lambda (account-id)
		(not (org-confluence-people-store-mention-label account-id directory)))
	      (org-confluence-mentions-extract-account-ids storage)))

(defun org-confluence-mentions-cache-users (users file)
  "Cache Confluence USERS into people FILE.
USERS is a list of Confluence user alists."
  (dolist (user users)
    (when-let* ((account-id (or (alist-get 'accountId user)
				(alist-get 'account-id user))))
      (org-confluence-people-store-cache-identity
       account-id
       (or (alist-get 'displayName user)
	   (alist-get 'display-name user)
	   (alist-get 'publicName user)
	   (alist-get 'public-name user))
       (or (alist-get 'email user)
	   (alist-get 'emailAddress user)
	   (alist-get 'email-address user))
       file))))

(defun org-confluence-mentions-resolve-storage-users (storage &optional directory)
  "Resolve and cache Confluence users mentioned in STORAGE for DIRECTORY.
No network request is made when STORAGE has no unresolved mentions."
  (let ((account-ids (org-confluence-mentions-unresolved-account-ids storage directory)))
    (when account-ids
      (let* ((response (org-confluence-api--users-bulk account-ids))
	     (body (plist-get response :body))
	     (json (json-parse-string body :object-type 'alist :array-type 'list))
	     (users (alist-get 'results json))
	     (file (let ((local (and directory
				     (org-confluence-people-store-local-file directory))))
		     (if (and local (file-exists-p local))
			 local
		       (org-confluence-people-store-global-file)))))
	(org-confluence-mentions-cache-users users file)))))

(provide 'org-confluence-mentions)
;;; org-confluence-mentions.el ends here
