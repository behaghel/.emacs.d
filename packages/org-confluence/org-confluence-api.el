;;; org-confluence-api.el --- cfl shell wrappers for Confluence export -*- lexical-binding: t; -*-

;;; Commentary:
;; Build and run cfl commands for the Org -> Confluence publish flow.

;;; Code:

(require 'base64)
(require 'json)
(require 'org)
(require 'seq)
(require 'subr-x)
(require 'url)
(require 'url-util)

(defgroup hub/confluence-api nil
  "Customizations for publishing Org documents to Confluence."
  :group 'hub)

(defcustom hub/confluence-api-cfl-command "cfl"
  "Command used to invoke the cfl Confluence CLI."
  :type 'string
  :group 'hub/confluence-api)

(defcustom hub/confluence-api-default-space nil
  "Default Confluence space key used when #+CONFLUENCE_SPACE is absent.

Keep this nil in the reusable package.  Set it from a tracked configuration
module when your normal Org workflow should default to a personal space."
  :type '(choice (const :tag "No default" nil) string)
  :group 'hub/confluence-api)

(defcustom hub/confluence-api-base-url nil
  "Base URL of the Confluence site, without the /wiki path.

For Atlassian Cloud, this is typically a URL like
https://example.atlassian.net.  It is used to open pages in a browser after
publishing."
  :type '(choice (const :tag "No base URL" nil) string)
  :group 'hub/confluence-api)

(defcustom hub/confluence-api-cfl-config-file
  (expand-file-name "~/.config/cfl/config.yml")
  "Path to the cfl configuration file used for Confluence Cloud REST auth."
  :type 'file
  :group 'hub/confluence-api)

(defvar hub/confluence-api--rest-transport #'hub/confluence-api--url-transport
  "Function used by `hub/confluence-api--rest-request' to perform HTTP IO.
The function receives METHOD, URL, HEADERS, and BODY.  Tests bind this variable
so no network access is required.")

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

(defun hub/confluence-api--normalize-base-url (base-url)
  "Return Confluence BASE-URL without trailing slash or /wiki suffix."
  (string-remove-suffix "/wiki"
			(string-remove-suffix "/"
					      (hub/confluence-api--require-string base-url "base URL"))))

(defun hub/confluence-api--strip-yaml-quotes (value)
  "Return YAML scalar VALUE with simple surrounding quotes removed."
  (let ((trimmed (string-trim value)))
    (if (and (>= (length trimmed) 2)
	     (or (and (string-prefix-p "\"" trimmed)
		      (string-suffix-p "\"" trimmed))
		 (and (string-prefix-p "'" trimmed)
		      (string-suffix-p "'" trimmed))))
	(substring trimmed 1 -1)
      trimmed)))

(defun hub/confluence-api--yaml-scalar-alist (file)
  "Return a flat alist of scalar KEY . VALUE pairs from YAML FILE.
This intentionally handles only the simple cfl config scalars needed for REST
comment access; it is not a general YAML parser."
  (unless (file-readable-p file)
    (user-error "Cannot read cfl config file: %s" file))
  (with-temp-buffer
    (insert-file-contents file)
    (let (values)
      (goto-char (point-min))
      (while (re-search-forward "^[[:space:]]*\\([A-Za-z0-9_-]+\\):[[:space:]]*\\(.+?\\)[[:space:]]*$" nil t)
	(let* ((key (replace-regexp-in-string "-" "_" (downcase (match-string 1))))
	       (raw-value (replace-regexp-in-string "[[:space:]]+#.*\\'" "" (match-string 2)))
	       (value (hub/confluence-api--strip-yaml-quotes raw-value)))
	  (unless (string-empty-p value)
	    (push (cons key value) values))))
      (nreverse values))))

(defun hub/confluence-api--config-value (values keys)
  "Return the first scalar in VALUES matching one of KEYS."
  (seq-some (lambda (key)
	      (alist-get key values nil nil #'equal))
	    keys))

(defun hub/confluence-api--require-config-value (value description)
  "Return required config VALUE or signal a redacted error for DESCRIPTION."
  (unless (hub/confluence-api--present-string-p value)
    (user-error "Missing Confluence %s in cfl config" description))
  (string-trim value))

(defun hub/confluence-api--read-cfl-config (&optional file)
  "Read Confluence Cloud REST credentials from cfl config FILE.
Return a plist containing `:base-url', optional `:cloud-id', `:email', and
`:api-token'."
  (let* ((values (hub/confluence-api--yaml-scalar-alist
		  (or file hub/confluence-api-cfl-config-file)))
	 (base-url (hub/confluence-api--normalize-base-url
		    (hub/confluence-api--require-config-value
		     (hub/confluence-api--config-value
		      values '("base_url" "baseurl" "site_url" "url" "host"))
		     "base URL")))
	 (cloud-id (hub/confluence-api--config-value values '("cloud_id" "cloudid")))
	 (email (hub/confluence-api--require-config-value
		 (hub/confluence-api--config-value values '("email" "username" "user"))
		 "email"))
	 (api-token (hub/confluence-api--require-config-value
		     (hub/confluence-api--config-value
		      values '("api_token" "apitoken" "token"))
		     "API token")))
    (list :base-url base-url
	  :cloud-id cloud-id
	  :email email
	  :api-token api-token)))

(defun hub/confluence-api--comment-list-url (page-id comment-kind &optional body-format config)
  "Return a REST URL for PAGE-ID comments of COMMENT-KIND.
COMMENT-KIND should be `inline-comments' or `footer-comments'.  BODY-FORMAT,
when non-nil, is sent as the `body-format' query parameter.  CONFIG defaults to
`hub/confluence-api--read-cfl-config'."
  (let* ((id (hub/confluence-api--require-string page-id "page ID"))
	 (kind (hub/confluence-api--require-string comment-kind "comment endpoint"))
	 (credentials (or config (hub/confluence-api--read-cfl-config)))
	 (base-url (hub/confluence-api--normalize-base-url (plist-get credentials :base-url)))
	 (query (when (hub/confluence-api--present-string-p body-format)
		  (concat "?" (url-build-query-string
			       `(("body-format" ,(string-trim body-format))))))))
    (unless (member kind '("inline-comments" "footer-comments"))
      (user-error "Unsupported Confluence comment endpoint: %s" kind))
    (format "%s/wiki/api/v2/pages/%s/%s%s"
	    base-url (url-hexify-string id) kind (or query ""))))

(defun hub/confluence-api--basic-auth-header (email api-token)
  "Return a Confluence Basic auth header for EMAIL and API-TOKEN."
  (concat "Basic "
	  (base64-encode-string
	   (format "%s:%s"
		   (hub/confluence-api--require-string email "email")
		   (hub/confluence-api--require-string api-token "API token"))
	   t)))

(defun hub/confluence-api--url-transport (method url headers body)
  "Perform METHOD request to URL with HEADERS and BODY using url.el."
  (let ((url-request-method method)
	(url-request-extra-headers headers)
	(url-request-data body))
    (url-retrieve-synchronously url t t)))

(defun hub/confluence-api--url-buffer-response (buffer)
  "Return a plist response from url.el response BUFFER, then kill BUFFER."
  (unwind-protect
      (with-current-buffer buffer
	(let ((status (and (boundp 'url-http-response-status)
			   url-http-response-status)))
	  (goto-char (point-min))
	  (when (re-search-forward "\r?\n\r?\n" nil t)
	    (delete-region (point-min) (point)))
	  (list :status status
		:body (buffer-substring-no-properties (point-min) (point-max)))))
    (when (buffer-live-p buffer)
      (kill-buffer buffer))))

(defun hub/confluence-api--normalize-rest-response (response)
  "Return RESPONSE normalized to a plist when it comes from url.el."
  (if (bufferp response)
      (hub/confluence-api--url-buffer-response response)
    response))

(defun hub/confluence-api--rest-request (method url &optional body config)
  "Perform an authenticated Confluence REST request.
METHOD and URL describe the request.  BODY is sent as request data when non-nil.
CONFIG is a plist with `:email' and `:api-token', defaulting to the cfl config."
  (let* ((credentials (or config (hub/confluence-api--read-cfl-config)))
	 (headers `(("Authorization" . ,(hub/confluence-api--basic-auth-header
					 (plist-get credentials :email)
					 (plist-get credentials :api-token)))
		    ("Accept" . "application/json")))
	 (request-headers (if body
			      (append headers '(("Content-Type" . "application/json")))
			    headers)))
    (hub/confluence-api--normalize-rest-response
     (funcall hub/confluence-api--rest-transport method url request-headers body))))

(defun hub/confluence-api--list-page-comments (page-id comment-kind &optional body-format config)
  "Return PAGE-ID comments from Confluence REST COMMENT-KIND endpoint.
COMMENT-KIND is `footer-comments' or `inline-comments'.  BODY-FORMAT defaults
to storage when nil.  CONFIG is passed to `hub/confluence-api--rest-request'."
  (hub/confluence-api--rest-request
   "GET"
   (hub/confluence-api--comment-list-url page-id comment-kind (or body-format "storage") config)
   nil
   config))

(defun hub/confluence-api--comment-children-url (comment-id comment-kind &optional body-format config)
  "Return REST URL for COMMENT-ID children of COMMENT-KIND.
COMMENT-KIND should be `inline-comments' or `footer-comments'."
  (let* ((id (hub/confluence-api--require-string comment-id "comment ID"))
	 (kind (hub/confluence-api--require-string comment-kind "comment endpoint"))
	 (credentials (or config (hub/confluence-api--read-cfl-config)))
	 (base-url (hub/confluence-api--normalize-base-url (plist-get credentials :base-url)))
	 (query (when (hub/confluence-api--present-string-p body-format)
		  (concat "?" (url-build-query-string
			       `(("body-format" ,(string-trim body-format))))))))
    (unless (member kind '("inline-comments" "footer-comments"))
      (user-error "Unsupported Confluence comment endpoint: %s" kind))
    (format "%s/wiki/api/v2/%s/%s/children%s"
	    base-url kind (url-hexify-string id) (or query ""))))

(defun hub/confluence-api--list-comment-children (comment-id comment-kind &optional body-format config)
  "Return COMMENT-ID child comments from Confluence COMMENT-KIND endpoint."
  (hub/confluence-api--rest-request
   "GET"
   (hub/confluence-api--comment-children-url comment-id comment-kind (or body-format "storage") config)
   nil
   config))

(defun hub/confluence-api--users-bulk-url (&optional config)
  "Return Confluence Cloud REST v2 users-bulk URL using CONFIG."
  (let* ((credentials (or config (hub/confluence-api--read-cfl-config)))
	 (base-url (hub/confluence-api--normalize-base-url (plist-get credentials :base-url))))
    (format "%s/wiki/api/v2/users-bulk" base-url)))

(defun hub/confluence-api--users-bulk (account-ids &optional config)
  "Return user details for ACCOUNT-IDS through Confluence users-bulk API."
  (hub/confluence-api--rest-request
   "POST"
   (hub/confluence-api--users-bulk-url config)
   (json-encode `((accountIds . ,(vconcat account-ids))))
   config))

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

(defun hub/confluence-api--page-view-storage-command (page-id)
  "Build a cfl command to view PAGE-ID as raw storage XHTML."
  (let ((id (hub/confluence-api--require-string page-id "page ID")))
    (hub/confluence-api--shell-join
     (list hub/confluence-api-cfl-command "page" "view" id "--raw" "--content-only"))))

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

(defun hub/confluence-api--property-from-subtree (property)
  "Return PROPERTY from the current Org subtree, or nil."
  (when (and (derived-mode-p 'org-mode)
	     (not (org-before-first-heading-p)))
    (org-entry-get nil property t)))

(defun hub/confluence-api--page-id-from-buffer (&optional subtreep)
  "Return the current Confluence page ID, or nil.

When SUBTREEP is non-nil, prefer a CONFLUENCE_PAGE_ID property on the current
Org subtree.  Otherwise use the buffer-level #+CONFLUENCE_PAGE_ID keyword."
  (or (when subtreep
	(hub/confluence-api--property-from-subtree "CONFLUENCE_PAGE_ID"))
      (hub/confluence-api--keyword-from-buffer "CONFLUENCE_PAGE_ID")))

(defun hub/confluence-api--space-from-buffer ()
  "Return the current Confluence space key, or nil.

Prefer #+CONFLUENCE_SPACE in the current Org buffer.  When absent, fall back to
`hub/confluence-api-default-space'."
  (or (hub/confluence-api--keyword-from-buffer "CONFLUENCE_SPACE")
      hub/confluence-api-default-space))

(defun hub/confluence-api--page-url (page-id &optional space)
  "Return browser URL for Confluence PAGE-ID in optional SPACE."
  (let ((base-url (hub/confluence-api--normalize-base-url hub/confluence-api-base-url))
	(id (hub/confluence-api--require-string page-id "page ID")))
    (if (hub/confluence-api--present-string-p space)
	(format "%s/wiki/spaces/%s/pages/%s" base-url (url-hexify-string (string-trim space)) id)
      (format "%s/wiki/pages/%s" base-url id))))

(provide 'org-confluence-api)
(provide 'org/export-confluence-api)
;;; org-confluence-api.el ends here
