;;; org-confluence-people.el --- Confluence people API helpers -*- lexical-binding: t; -*-

;;; Commentary:
;; API-backed Confluence people helpers and interactive commands.  Plain Org
;; people directory storage remains in `org-confluence-people-store'.

;;; Code:

(require 'org-confluence-people-store)
(require 'cl-lib)
(require 'org-confluence-api)
(require 'org-confluence-response)

(defun org-confluence-people-current-user-account-id ()
  "Return authenticated Confluence user's account ID."
  (let* ((response (org-confluence-api--current-user))
	 (user (org-confluence-response-json-alist
		(org-confluence-response-body response)))
	 (account-id (or (alist-get 'accountId user)
			 (alist-get 'account-id user))))
    (unless account-id
      (user-error "Confluence current user response did not include accountId"))
    account-id))

(defun org-confluence-people-source-directory ()
  "Return current buffer file directory for people lookup, or nil."
  (when buffer-file-name
    (file-name-directory buffer-file-name)))

(defun org-confluence-people-user-account-id (user)
  "Return Confluence account ID from USER alist."
  (alist-get 'accountId user))

(defun org-confluence-people-user-display-name (user)
  "Return display name from Confluence USER alist."
  (or (alist-get 'displayName user)
      (alist-get 'publicName user)))

(defun org-confluence-people-current-user ()
  "Return authenticated Confluence user as an alist."
  (org-confluence-response-json-alist
   (org-confluence-response-body (org-confluence-api--current-user))))

(defun org-confluence-people-resolve-in-files (files users)
  "Update people FILES from Confluence USERS and return resolved count."
  (let ((resolved 0))
    (dolist (user users resolved)
      (when-let* ((account-id (org-confluence-people-user-account-id user)))
	(dolist (file files)
	  (when (org-confluence-people-store-update-identity
		 file account-id
		 (org-confluence-people-user-display-name user)
		 (alist-get 'email user)
		 (alist-get 'accountStatus user)
		 (alist-get 'timeZone user))
	    (setq resolved (1+ resolved))))))))

;;;###autoload
(defun org-confluence-people-mark-current-user ()
  "Mark the authenticated Confluence user as the current user in people cache.
The identity is cached in the global people file and marked with
`ORG_CONFLUENCE_ME: t'."
  (interactive)
  (let* ((user (org-confluence-people-current-user))
	 (account-id (or (org-confluence-people-user-account-id user)
			 (user-error "Confluence current user response did not include accountId")))
	 (display-name (org-confluence-people-user-display-name user))
	 (email (alist-get 'email user))
	 (file (org-confluence-people-store-mark-me account-id display-name email)))
    (message "Marked Confluence user %s as me in %s" (or display-name account-id) file)
    file))

;;;###autoload
(defun org-confluence-people-resolve (&optional directory)
  "Resolve unresolved Confluence people in local/global people files.
DIRECTORY controls the local people file context.  Interactively, use the
current Org file's directory when available, otherwise use the global file."
  (interactive)
  (let* ((context-directory (or directory (org-confluence-people-source-directory)))
	 (files (org-confluence-people-store-context-files context-directory))
	 (account-ids (org-confluence-people-store-unresolved-account-ids files)))
    (if (not account-ids)
	(progn
	  (message "No unresolved Confluence people")
	  0)
      (let* ((users (org-confluence-response-user-results
		     (org-confluence-api--users-bulk account-ids)))
	     (resolved (org-confluence-people-resolve-in-files files users))
	     (unresolved (- (length account-ids)
			    (length (delete-dups (mapcar #'org-confluence-people-user-account-id users))))))
	(message "Resolved %s Confluence people (%s unresolved)" resolved unresolved)
	resolved))))

(provide 'org-confluence-people)
;;; org-confluence-people.el ends here
