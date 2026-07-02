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

(defgroup org-confluence-api nil
  "Customizations for publishing Org documents to Confluence."
  :group 'hub)

(defcustom org-confluence-api-cfl-command "cfl"
  "Command used to invoke the cfl Confluence CLI."
  :type 'string
  :group 'org-confluence-api)

(defcustom org-confluence-api-default-space nil
  "Default Confluence space key used when #+CONFLUENCE_SPACE is absent.

Keep this nil in the reusable package.  Set it from a tracked configuration
module when your normal Org workflow should default to a personal space."
  :type '(choice (const :tag "No default" nil) string)
  :group 'org-confluence-api)

(defcustom org-confluence-api-base-url nil
  "Base URL of the Confluence site, without the /wiki path.

For Atlassian Cloud, this is typically a URL like
https://example.atlassian.net.  It is used to open pages in a browser after
publishing."
  :type '(choice (const :tag "No base URL" nil) string)
  :group 'org-confluence-api)

(defcustom org-confluence-api-cfl-config-file
  (expand-file-name "~/.config/cfl/config.yml")
  "Path to the cfl configuration file used for Confluence Cloud REST auth."
  :type 'file
  :group 'org-confluence-api)

(defvar org-confluence-api--rest-transport #'org-confluence-api--url-transport
  "Function used by `org-confluence-api--rest-request' to perform HTTP IO.
The function receives METHOD, URL, HEADERS, and BODY.  Tests bind this variable
so no network access is required.")

(defun org-confluence-api--cfl-available-p ()
  "Return non-nil when configured cfl command is available on Emacs PATH."
  (and (executable-find org-confluence-api-cfl-command) t))

(defun org-confluence-api--present-string-p (value)
  "Return non-nil when VALUE is a non-empty string after trimming."
  (and (stringp value) (not (string-empty-p (string-trim value)))))

(defun org-confluence-api--require-string (value description)
  "Return trimmed VALUE or signal `user-error' mentioning DESCRIPTION."
  (unless (org-confluence-api--present-string-p value)
    (user-error "Missing Confluence %s" description))
  (string-trim value))

(defun org-confluence-api--shell-join (parts)
  "Join command PARTS into a shell command string."
  (string-join (mapcar #'shell-quote-argument (seq-filter #'identity parts)) " "))

(defun org-confluence-api--normalize-base-url (base-url)
  "Return Confluence BASE-URL without trailing slash or /wiki suffix."
  (string-remove-suffix "/wiki"
			(string-remove-suffix "/"
					      (org-confluence-api--require-string base-url "base URL"))))

(defun org-confluence-api--strip-yaml-quotes (value)
  "Return YAML scalar VALUE with simple surrounding quotes removed."
  (let ((trimmed (string-trim value)))
    (if (and (>= (length trimmed) 2)
	     (or (and (string-prefix-p "\"" trimmed)
		      (string-suffix-p "\"" trimmed))
		 (and (string-prefix-p "'" trimmed)
		      (string-suffix-p "'" trimmed))))
	(substring trimmed 1 -1)
      trimmed)))

(defun org-confluence-api--yaml-scalar-alist (file)
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
	       (value (org-confluence-api--strip-yaml-quotes raw-value)))
	  (unless (string-empty-p value)
	    (push (cons key value) values))))
      (nreverse values))))

(defun org-confluence-api--config-value (values keys)
  "Return the first scalar in VALUES matching one of KEYS."
  (seq-some (lambda (key)
	      (alist-get key values nil nil #'equal))
	    keys))

(defun org-confluence-api--require-config-value (value description)
  "Return required config VALUE or signal a redacted error for DESCRIPTION."
  (unless (org-confluence-api--present-string-p value)
    (user-error "Missing Confluence %s in cfl config" description))
  (string-trim value))

(defun org-confluence-api--read-cfl-config (&optional file)
  "Read Confluence Cloud REST credentials from cfl config FILE.
Return a plist containing `:base-url', optional `:cloud-id', `:email', and
`:api-token'."
  (let* ((values (org-confluence-api--yaml-scalar-alist
		  (or file org-confluence-api-cfl-config-file)))
	 (base-url (org-confluence-api--normalize-base-url
		    (org-confluence-api--require-config-value
		     (org-confluence-api--config-value
		      values '("base_url" "baseurl" "site_url" "url" "host"))
		     "base URL")))
	 (cloud-id (org-confluence-api--config-value values '("cloud_id" "cloudid")))
	 (email (org-confluence-api--require-config-value
		 (org-confluence-api--config-value values '("email" "username" "user"))
		 "email"))
	 (api-token (org-confluence-api--require-config-value
		     (org-confluence-api--config-value
		      values '("api_token" "apitoken" "token"))
		     "API token")))
    (list :base-url base-url
	  :cloud-id cloud-id
	  :email email
	  :api-token api-token)))

(defun org-confluence-api--page-detail-url (page-id &optional body-format config)
  "Return Confluence Cloud REST v2 page detail URL for PAGE-ID."
  (let* ((id (org-confluence-api--require-string page-id "page ID"))
	 (credentials (or config (org-confluence-api--read-cfl-config)))
	 (base-url (org-confluence-api--normalize-base-url (plist-get credentials :base-url)))
	 (query (when (org-confluence-api--present-string-p body-format)
		  (concat "?" (url-build-query-string
			       `(("body-format" ,(string-trim body-format))))))))
    (format "%s/wiki/api/v2/pages/%s%s" base-url (url-hexify-string id) (or query ""))))

(defun org-confluence-api--get-page (page-id &optional body-format config)
  "Return Confluence PAGE-ID details through REST v2."
  (org-confluence-api--rest-request
   "GET"
   (org-confluence-api--page-detail-url page-id (or body-format "storage") config)
   nil
   config))

(defun org-confluence-api--children-url (content-type content-id &optional limit config)
  "Return Confluence Cloud REST v2 direct-children URL.
CONTENT-TYPE is `page' or `folder'.  CONTENT-ID identifies the parent content.
LIMIT, when non-nil, is sent as the `limit' query parameter."
  (let* ((id (org-confluence-api--require-string content-id "content ID"))
	 (type (org-confluence-api--require-string content-type "content type"))
	 (credentials (or config (org-confluence-api--read-cfl-config)))
	 (base-url (org-confluence-api--normalize-base-url (plist-get credentials :base-url)))
	 (path (pcase type
		 ("page" "pages")
		 ("folder" "folders")
		 (_ (user-error "Unsupported Confluence child parent type: %s" type))))
	 (query (when limit
		  (concat "?" (url-build-query-string `(("limit" ,(format "%s" limit))))))))
    (format "%s/wiki/api/v2/%s/%s/direct-children%s"
	    base-url path (url-hexify-string id) (or query ""))))

(defun org-confluence-api--child-pages-url (page-id &optional limit config)
  "Return Confluence Cloud REST v2 direct-children URL for page PAGE-ID.
LIMIT, when non-nil, is sent as the `limit' query parameter."
  (org-confluence-api--children-url "page" page-id limit config))

(defun org-confluence-api--folder-children-url (folder-id &optional limit config)
  "Return Confluence Cloud REST v2 direct-children URL for FOLDER-ID.
LIMIT, when non-nil, is sent as the `limit' query parameter."
  (org-confluence-api--children-url "folder" folder-id limit config))

(defun org-confluence-api--list-children (content-type content-id &optional limit config)
  "Return direct children of Confluence CONTENT-TYPE CONTENT-ID through REST v2.
CONTENT-TYPE is `page' or `folder'.  LIMIT is passed to
`org-confluence-api--children-url'.  CONFIG is passed to
`org-confluence-api--rest-request'."
  (org-confluence-api--rest-request
   "GET"
   (org-confluence-api--children-url content-type content-id limit config)
   nil
   config))

(defun org-confluence-api--list-child-pages (page-id &optional limit config)
  "Return direct children of Confluence page PAGE-ID through REST v2.
LIMIT is passed to `org-confluence-api--child-pages-url'.  CONFIG is passed to
`org-confluence-api--rest-request'.  Despite the historical name, the result may
include non-page children such as folders."
  (org-confluence-api--list-children "page" page-id limit config))

(defun org-confluence-api--list-folder-children (folder-id &optional limit config)
  "Return direct children of Confluence FOLDER-ID through REST v2.
LIMIT is passed to `org-confluence-api--folder-children-url'.  CONFIG is passed
to `org-confluence-api--rest-request'."
  (org-confluence-api--list-children "folder" folder-id limit config))

(defun org-confluence-api--comment-list-url (page-id comment-kind &optional body-format config)
  "Return a REST URL for PAGE-ID comments of COMMENT-KIND.
COMMENT-KIND should be `inline-comments' or `footer-comments'.  BODY-FORMAT,
when non-nil, is sent as the `body-format' query parameter.  CONFIG defaults to
`org-confluence-api--read-cfl-config'."
  (let* ((id (org-confluence-api--require-string page-id "page ID"))
	 (kind (org-confluence-api--require-string comment-kind "comment endpoint"))
	 (credentials (or config (org-confluence-api--read-cfl-config)))
	 (base-url (org-confluence-api--normalize-base-url (plist-get credentials :base-url)))
	 (query (when (org-confluence-api--present-string-p body-format)
		  (concat "?" (url-build-query-string
			       `(("body-format" ,(string-trim body-format))))))))
    (unless (member kind '("inline-comments" "footer-comments"))
      (user-error "Unsupported Confluence comment endpoint: %s" kind))
    (format "%s/wiki/api/v2/pages/%s/%s%s"
	    base-url (url-hexify-string id) kind (or query ""))))

(defun org-confluence-api--basic-auth-header (email api-token)
  "Return a Confluence Basic auth header for EMAIL and API-TOKEN."
  (concat "Basic "
	  (base64-encode-string
	   (format "%s:%s"
		   (org-confluence-api--require-string email "email")
		   (org-confluence-api--require-string api-token "API token"))
	   t)))

(defun org-confluence-api--url-transport (method url headers body)
  "Perform METHOD request to URL with HEADERS and BODY using url.el."
  (let ((url-request-method method)
	(url-request-extra-headers headers)
	(url-request-data (and body (encode-coding-string body 'utf-8))))
    (url-retrieve-synchronously url t t)))

(defun org-confluence-api--url-buffer-response (buffer)
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

(defun org-confluence-api--normalize-rest-response (response)
  "Return RESPONSE normalized to a plist when it comes from url.el."
  (if (bufferp response)
      (org-confluence-api--url-buffer-response response)
    response))

(defun org-confluence-api--rest-request (method url &optional body config)
  "Perform an authenticated Confluence REST request.
METHOD and URL describe the request.  BODY is sent as request data when non-nil.
CONFIG is a plist with `:email' and `:api-token', defaulting to the cfl config."
  (let* ((credentials (or config (org-confluence-api--read-cfl-config)))
	 (headers `(("Authorization" . ,(org-confluence-api--basic-auth-header
					 (plist-get credentials :email)
					 (plist-get credentials :api-token)))
		    ("Accept" . "application/json")))
	 (request-headers (if body
			      (append headers '(("Content-Type" . "application/json")))
			    headers)))
    (org-confluence-api--normalize-rest-response
     (funcall org-confluence-api--rest-transport method url request-headers body))))

(defun org-confluence-api--list-page-comments (page-id comment-kind &optional body-format config)
  "Return PAGE-ID comments from Confluence REST COMMENT-KIND endpoint.
COMMENT-KIND is `footer-comments' or `inline-comments'.  BODY-FORMAT defaults
to storage when nil.  CONFIG is passed to `org-confluence-api--rest-request'."
  (org-confluence-api--rest-request
   "GET"
   (org-confluence-api--comment-list-url page-id comment-kind (or body-format "storage") config)
   nil
   config))

(defun org-confluence-api--comment-children-url (comment-id comment-kind &optional body-format config)
  "Return REST URL for COMMENT-ID children of COMMENT-KIND.
COMMENT-KIND should be `inline-comments' or `footer-comments'."
  (let* ((id (org-confluence-api--require-string comment-id "comment ID"))
	 (kind (org-confluence-api--require-string comment-kind "comment endpoint"))
	 (credentials (or config (org-confluence-api--read-cfl-config)))
	 (base-url (org-confluence-api--normalize-base-url (plist-get credentials :base-url)))
	 (query (when (org-confluence-api--present-string-p body-format)
		  (concat "?" (url-build-query-string
			       `(("body-format" ,(string-trim body-format))))))))
    (unless (member kind '("inline-comments" "footer-comments"))
      (user-error "Unsupported Confluence comment endpoint: %s" kind))
    (format "%s/wiki/api/v2/%s/%s/children%s"
	    base-url kind (url-hexify-string id) (or query ""))))

(defun org-confluence-api--list-comment-children (comment-id comment-kind &optional body-format config)
  "Return COMMENT-ID child comments from Confluence COMMENT-KIND endpoint."
  (org-confluence-api--rest-request
   "GET"
   (org-confluence-api--comment-children-url comment-id comment-kind (or body-format "storage") config)
   nil
   config))

(defun org-confluence-api--comment-kind (comment-kind)
  "Return validated Confluence COMMENT-KIND endpoint name."
  (let ((kind (org-confluence-api--require-string comment-kind "comment endpoint")))
    (unless (member kind '("inline-comments" "footer-comments"))
      (user-error "Unsupported Confluence comment endpoint: %s" kind))
    kind))

(defun org-confluence-api--comment-create-url (comment-kind &optional config)
  "Return Confluence Cloud REST v2 COMMENT-KIND create URL using CONFIG."
  (let* ((kind (org-confluence-api--comment-kind comment-kind))
	 (credentials (or config (org-confluence-api--read-cfl-config)))
	 (base-url (org-confluence-api--normalize-base-url (plist-get credentials :base-url))))
    (format "%s/wiki/api/v2/%s" base-url kind)))

(defun org-confluence-api--comment-detail-url
    (comment-kind comment-id &optional body-format config)
  "Return Confluence Cloud REST v2 COMMENT-KIND COMMENT-ID URL using CONFIG."
  (let* ((kind (org-confluence-api--comment-kind comment-kind))
	 (id (org-confluence-api--require-string comment-id "comment ID"))
	 (credentials (or config (org-confluence-api--read-cfl-config)))
	 (base-url (org-confluence-api--normalize-base-url (plist-get credentials :base-url)))
	 (query (when (org-confluence-api--present-string-p body-format)
		  (concat "?" (url-build-query-string
			       `(("body-format" ,(string-trim body-format))))))))
    (format "%s/wiki/api/v2/%s/%s%s" base-url kind (url-hexify-string id) (or query ""))))

(defun org-confluence-api--footer-comment-create-url (&optional config)
  "Return Confluence Cloud REST v2 footer comment create URL using CONFIG."
  (org-confluence-api--comment-create-url "footer-comments" config))

(defun org-confluence-api--inline-comment-create-url (&optional config)
  "Return Confluence Cloud REST v2 inline comment create URL using CONFIG."
  (org-confluence-api--comment-create-url "inline-comments" config))

(defun org-confluence-api--comment-create-payload (container-key container-id storage-body
								 &optional inline-properties)
  "Return a comment create payload for CONTAINER-KEY and CONTAINER-ID.
STORAGE-BODY is a Confluence storage XHTML fragment.  INLINE-PROPERTIES, when
non-nil, is added as `inlineCommentProperties'."
  (append `((,container-key . ,(org-confluence-api--require-string
				container-id (symbol-name container-key)))
	    (body . ((representation . "storage")
		     (value . ,(org-confluence-api--require-string
				storage-body "comment body")))))
	  (when inline-properties
	    `((inlineCommentProperties . ,inline-properties)))))

(defun org-confluence-api--create-footer-comment (page-id storage-body &optional config)
  "Create a Confluence footer comment on PAGE-ID with STORAGE-BODY.
STORAGE-BODY is a Confluence storage XHTML fragment.  Return the raw REST
response plist."
  (org-confluence-api--rest-request
   "POST"
   (org-confluence-api--footer-comment-create-url config)
   (json-encode (org-confluence-api--comment-create-payload 'pageId page-id storage-body))
   config))

(defun org-confluence-api--create-inline-comment
    (page-id storage-body inline-properties &optional config)
  "Create a Confluence inline comment on PAGE-ID with STORAGE-BODY.
INLINE-PROPERTIES is sent as `inlineCommentProperties'.  Return the raw REST
response plist."
  (org-confluence-api--rest-request
   "POST"
   (org-confluence-api--inline-comment-create-url config)
   (json-encode (org-confluence-api--comment-create-payload
		 'pageId page-id storage-body inline-properties))
   config))

(defun org-confluence-api--create-comment-reply
    (comment-kind parent-comment-id storage-body &optional config)
  "Create a reply under PARENT-COMMENT-ID using COMMENT-KIND endpoint.
COMMENT-KIND is `footer-comments' or `inline-comments'.  STORAGE-BODY is a
Confluence storage XHTML fragment.  Return the raw REST response plist."
  (org-confluence-api--rest-request
   "POST"
   (org-confluence-api--comment-create-url comment-kind config)
   (json-encode (org-confluence-api--comment-create-payload
		 'parentCommentId parent-comment-id storage-body))
   config))

(defun org-confluence-api--get-comment (comment-kind comment-id &optional body-format config)
  "Return Confluence COMMENT-ID from COMMENT-KIND endpoint."
  (org-confluence-api--rest-request
   "GET"
   (org-confluence-api--comment-detail-url comment-kind comment-id (or body-format "storage") config)
   nil
   config))

(defun org-confluence-api--comment-update-payload (storage-body version-number)
  "Return a comment update payload for STORAGE-BODY at VERSION-NUMBER."
  `((version . ((number . ,version-number)))
    (body . ((representation . "storage")
	     (value . ,(org-confluence-api--require-string storage-body "comment body"))))))

(defun org-confluence-api--update-comment
    (comment-kind comment-id storage-body version-number &optional config)
  "Update Confluence COMMENT-ID in COMMENT-KIND with STORAGE-BODY.
VERSION-NUMBER is the next Confluence version number to send."
  (org-confluence-api--rest-request
   "PUT"
   (org-confluence-api--comment-detail-url comment-kind comment-id nil config)
   (json-encode (org-confluence-api--comment-update-payload storage-body version-number))
   config))

(defun org-confluence-api--current-user-url (&optional config)
  "Return Confluence Cloud current user URL using CONFIG."
  (let* ((credentials (or config (org-confluence-api--read-cfl-config)))
	 (base-url (org-confluence-api--normalize-base-url (plist-get credentials :base-url))))
    (format "%s/wiki/rest/api/user/current" base-url)))

(defun org-confluence-api--current-user (&optional config)
  "Return the current authenticated Confluence user."
  (org-confluence-api--rest-request
   "GET"
   (org-confluence-api--current-user-url config)
   nil
   config))

(defun org-confluence-api--users-bulk-url (&optional config)
  "Return Confluence Cloud REST v2 users-bulk URL using CONFIG."
  (let* ((credentials (or config (org-confluence-api--read-cfl-config)))
	 (base-url (org-confluence-api--normalize-base-url (plist-get credentials :base-url))))
    (format "%s/wiki/api/v2/users-bulk" base-url)))

(defun org-confluence-api--users-bulk (account-ids &optional config)
  "Return user details for ACCOUNT-IDS through Confluence users-bulk API."
  (org-confluence-api--rest-request
   "POST"
   (org-confluence-api--users-bulk-url config)
   (json-encode `((accountIds . ,(vconcat account-ids))))
   config))

(defun org-confluence-api--file-flag (file-path)
  "Return a cfl --file argument list for FILE-PATH, or nil."
  (when (org-confluence-api--present-string-p file-path)
    (list "--file" (string-trim file-path))))

(defun org-confluence-api--page-update-command (page-id &optional file-path)
  "Build a cfl command to update PAGE-ID from optional XHTML FILE-PATH."
  (let ((id (org-confluence-api--require-string page-id "page ID")))
    (org-confluence-api--shell-join
     (append (list org-confluence-api-cfl-command "page" "edit" id)
	     (org-confluence-api--file-flag file-path)
	     (list "--storage")))))

(defun org-confluence-api--page-view-storage-command (page-id)
  "Build a cfl command to view PAGE-ID as raw storage XHTML."
  (let ((id (org-confluence-api--require-string page-id "page ID")))
    (org-confluence-api--shell-join
     (list org-confluence-api-cfl-command "page" "view" id "--raw" "--content-only"))))

(defun org-confluence-api--attachment-upload-command (page-id file-path)
  "Build a cfl command to upload FILE-PATH as an attachment to PAGE-ID."
  (let ((id (org-confluence-api--require-string page-id "page ID"))
	(file (org-confluence-api--require-string file-path "attachment file")))
    (org-confluence-api--shell-join
     (list org-confluence-api-cfl-command "attachment" "upload" "--page" id "--file" file))))

(defun org-confluence-api--page-create-command (space title &optional file-path parent-id)
  "Build a cfl command to create TITLE in SPACE from optional XHTML FILE-PATH.

When PARENT-ID is non-nil, include it as the new page's parent."
  (let ((space-key (org-confluence-api--require-string space "space"))
	(page-title (org-confluence-api--require-string title "page title")))
    (org-confluence-api--shell-join
     (append (list org-confluence-api-cfl-command
		   "page" "create"
		   "--space" space-key
		   "--title" page-title)
	     (org-confluence-api--file-flag file-path)
	     (when (org-confluence-api--present-string-p parent-id)
	       (list "--parent" (string-trim parent-id)))
	     (list "--storage")))))

(defun org-confluence-api--keyword-from-buffer (keyword)
  "Return the value for Org KEYWORD in the current buffer, or nil."
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search t)
	  (regexp (format "^[	]*#\\+%s:[	]*\\(.*?\\)[	]*$" (regexp-quote keyword))))
      (when (re-search-forward regexp nil t)
	(let ((value (string-trim (match-string-no-properties 1))))
	  (unless (string-empty-p value) value))))))

(defun org-confluence-api--property-from-subtree (property)
  "Return PROPERTY from the current Org subtree, or nil."
  (when (and (derived-mode-p 'org-mode)
	     (not (org-before-first-heading-p)))
    (org-entry-get nil property t)))

(defun org-confluence-api--page-id-from-buffer (&optional subtreep)
  "Return the current Confluence page ID, or nil.

When SUBTREEP is non-nil, prefer a CONFLUENCE_PAGE_ID property on the current
Org subtree.  Otherwise use the buffer-level #+CONFLUENCE_PAGE_ID keyword."
  (or (when subtreep
	(org-confluence-api--property-from-subtree "CONFLUENCE_PAGE_ID"))
      (org-confluence-api--keyword-from-buffer "CONFLUENCE_PAGE_ID")))

(defun org-confluence-api--space-from-buffer ()
  "Return the current Confluence space key, or nil.

Prefer #+CONFLUENCE_SPACE in the current Org buffer.  When absent, fall back to
`org-confluence-api-default-space'."
  (or (org-confluence-api--keyword-from-buffer "CONFLUENCE_SPACE")
      org-confluence-api-default-space))

(defun org-confluence-api--page-url (page-id &optional space)
  "Return browser URL for Confluence PAGE-ID in optional SPACE."
  (let ((base-url (org-confluence-api--normalize-base-url org-confluence-api-base-url))
	(id (org-confluence-api--require-string page-id "page ID")))
    (if (org-confluence-api--present-string-p space)
	(format "%s/wiki/spaces/%s/pages/%s" base-url (url-hexify-string (string-trim space)) id)
      (format "%s/wiki/pages/%s" base-url id))))

(defun org-confluence-api--comment-url (page-id comment-id &optional space)
  "Return browser URL for COMMENT-ID focused on PAGE-ID in optional SPACE."
  (format "%s?focusedCommentId=%s"
	  (org-confluence-api--page-url page-id space)
	  (url-hexify-string (org-confluence-api--require-string comment-id "comment ID"))))

(provide 'org-confluence-api)
;;; org-confluence-api.el ends here
