;;; org-confluence-sync.el --- Org Confluence page sync -*- lexical-binding: t; -*-

;;; Commentary:
;; Page and sidecar comment synchronization helpers for Org ↔ Confluence.

;;; Code:

(require 'json)
(require 'org)
(require 'org-comments-core)
(require 'org-comments-sidecar)
(require 'seq)
(require 'subr-x)

(require 'org-confluence-api)
(require 'org-confluence-comments)
(require 'org-confluence-mentions)
(require 'org-confluence-page)
(require 'org-confluence-response)

(autoload 'org-confluence-import--format-org "org-confluence-import" nil nil)
(autoload 'org-confluence-import--line-kind "org-confluence-import" nil nil)
(autoload 'org-confluence-import-storage-to-org "org-confluence-import" nil nil)
(autoload 'org-confluence-publish "org-confluence-publish" nil t)

(defun org-confluence-sync--sha256 (text)
  "Return a sha256 digest for TEXT."
  (concat "sha256:" (secure-hash 'sha256 (or text ""))))

(defun org-confluence-sync--keyword (keyword)
  "Return Org KEYWORD value in the current buffer, or nil."
  (org-confluence-api--keyword-from-buffer keyword))

(defun org-confluence-sync--set-keyword (keyword value)
  "Set Org KEYWORD to VALUE in the current buffer."
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search t)
	  (regexp (format "^[ \t]*#\\+%s:.*$" (regexp-quote keyword))))
      (if (re-search-forward regexp nil t)
	  (replace-match (format "#+%s: %s" keyword value) t t)
	(org-confluence-page-insert-metadata-after-keywords
	 `((,keyword . ,value)))))))

(defun org-confluence-sync--source-without-sync-metadata ()
  "Return current Org buffer text without Confluence sync metadata lines."
  (let ((source-text (buffer-string)))
    (with-temp-buffer
      (insert source-text)
      (goto-char (point-min))
      (while (re-search-forward "^[	 ]*#\\+CONFLUENCE_.*\n?" nil t)
	(replace-match ""))
      (string-trim (buffer-string)))))

(defun org-confluence-sync--collapse-blank-runs (org)
  "Collapse repeated blank lines in ORG outside verbatim blocks."
  (let ((lines (split-string org "\n"))
	(in-verbatim nil)
	(blank-pending nil)
	output)
    (dolist (line lines)
      (let ((kind (org-confluence-import--line-kind line)))
	(cond
	 ((and (not in-verbatim) (eq kind 'blank))
	  (unless blank-pending
	    (push "" output)
	    (setq blank-pending t)))
	 (t
	  (push line output)
	  (setq blank-pending nil)))
	(when (eq kind 'block-begin)
	  (setq in-verbatim (string-match-p "\\`#\\+begin_\\(?:src\\|example\\)" line)))
	(when (and in-verbatim (eq kind 'block-end))
	  (setq in-verbatim nil))))
    (string-trim (string-join (nreverse output) "\n"))))

(defun org-confluence-sync--canonical-org-for-hash (org)
  "Return canonical ORG text for sync hash comparisons."
  (org-confluence-sync--collapse-blank-runs
   (org-confluence-import--format-org org)))

(defun org-confluence-sync--local-org-hash ()
  "Return hash of current Org content excluding sync metadata."
  (org-confluence-sync--sha256
   (org-confluence-sync--canonical-org-for-hash
    (org-confluence-sync--source-without-sync-metadata))))

(defun org-confluence-sync--page-version-value (page)
  "Return Confluence PAGE version number, or nil."
  (alist-get 'number (alist-get 'version page)))

(defun org-confluence-sync--page-version (page)
  "Return Confluence PAGE version number as a string."
  (format "%s" (or (org-confluence-sync--page-version-value page)
		   (user-error "Confluence page response did not include version.number"))))

(defun org-confluence-sync--page-version-optional (page)
  "Return Confluence PAGE version number as a string, or nil."
  (when-let* ((version (org-confluence-sync--page-version-value page)))
    (format "%s" version)))

(defun org-confluence-sync--page-storage (page)
  "Return Confluence PAGE storage XHTML."
  (or (alist-get 'value (alist-get 'storage (alist-get 'body page)))
      (user-error "Confluence page response did not include body.storage.value")))

(defun org-confluence-sync--page-title (page)
  "Return Confluence PAGE title, or nil."
  (alist-get 'title page))

(defun org-confluence-sync--page-space (page)
  "Return Confluence PAGE space key, or nil."
  (or (alist-get 'key (alist-get 'space page))
      (alist-get 'spaceKey page)
      (alist-get 'space-key page)))

(defun org-confluence-sync--page-link (page key)
  "Return PAGE link KEY from common Confluence link shapes."
  (or (alist-get key (alist-get '_links page))
      (alist-get key (alist-get 'links page))))

(defun org-confluence-sync--absolute-url-p (url)
  "Return non-nil when URL looks absolute."
  (and (stringp url)
       (string-match-p "\\`https?://" url)))

(defun org-confluence-sync--page-web-url (page)
  "Return Confluence PAGE browser URL, or nil."
  (when-let* ((webui (org-confluence-sync--page-link page 'webui)))
    (if (org-confluence-sync--absolute-url-p webui)
	webui
      (when-let* ((base (org-confluence-sync--page-link page 'base)))
	(concat (string-remove-suffix "/" base) "/" (string-remove-prefix "/" webui))))))

(defun org-confluence-sync--page-result-metadata (page)
  "Return plist metadata extracted from Confluence PAGE."
  (let (metadata)
    (when-let* ((title (org-confluence-sync--page-title page)))
      (setq metadata (append metadata (list :title title))))
    (when-let* ((space (org-confluence-sync--page-space page)))
      (setq metadata (append metadata (list :space space))))
    (when-let* ((version (org-confluence-sync--page-version-optional page)))
      (setq metadata (append metadata (list :version version :remote-version version))))
    (when-let* ((web-url (org-confluence-sync--page-web-url page)))
      (setq metadata (append metadata (list :web-url web-url))))
    metadata))

(defun org-confluence-sync--page-with-base-link (page base-url)
  "Return PAGE with BASE-URL available in its `_links' alist when possible."
  (if (or (not base-url)
	  (org-confluence-sync--page-link page 'base))
      page
    (let* ((links (alist-get '_links page))
	   (without-links (assq-delete-all '_links (copy-sequence page))))
      (append without-links
	      `((_links . ,(append links `((base . ,base-url)))))))))

(defun org-confluence-sync--child-result (page &optional base-url)
  "Return a normalized child content plist for PAGE, inheriting BASE-URL."
  (let ((child (org-confluence-sync--page-with-base-link page base-url)))
    (append (list :id (or (alist-get 'id child)
			  (user-error "Confluence child response did not include id"))
		  :type (or (alist-get 'type child) "page"))
	    (org-confluence-sync--page-result-metadata child))))

(defun org-confluence-sync--list-children (content-type content-id)
  "Return normalized direct children for Confluence CONTENT-TYPE CONTENT-ID."
  (let* ((body (org-confluence-response-body
		(org-confluence-api--list-children content-type content-id)))
	 (response (org-confluence-response-json-alist body))
	 (base-url (or (org-confluence-sync--page-link response 'base)
		       (alist-get 'base response)))
	 (results (alist-get 'results response)))
    (mapcar (lambda (child)
	      (org-confluence-sync--child-result child base-url))
	    results)))

;;;###autoload
(defun org-confluence-list-child-pages (page-id)
  "Return normalized direct child metadata for Confluence page PAGE-ID.
Each child plist includes :id, :type, and any available :title, :space,
:version, :remote-version, and :web-url metadata from the child-list response.
Children may be pages, folders, or other Confluence content types."
  (org-confluence-sync--list-children "page" page-id))

(defun org-confluence-sync--list-folder-children (folder-id)
  "Return normalized direct child metadata for Confluence FOLDER-ID."
  (org-confluence-sync--list-children "folder" folder-id))

(defun org-confluence-sync--safe-filename-stem (title fallback)
  "Return a filesystem-safe filename stem from TITLE or FALLBACK."
  (let* ((source (if (org-confluence-api--present-string-p title) title fallback))
	 (trimmed (string-trim source))
	 (safe (replace-regexp-in-string "[[:cntrl:]/\\:*?\"<>|]+" "-" trimmed))
	 (collapsed (replace-regexp-in-string "[[:space:]]+" " " safe))
	 (stripped (string-trim collapsed "[ .-]+" "[ .-]+")))
    (if (string-empty-p stripped)
	fallback
      stripped)))

(defun org-confluence-sync--child-page-file (child directory seen)
  "Return a unique Org file path for CHILD under DIRECTORY using SEEN hash table."
  (let* ((id (plist-get child :id))
	 (stem (org-confluence-sync--safe-filename-stem (plist-get child :title) id))
	 (candidate stem))
    (when (gethash candidate seen)
      (setq candidate (format "%s-%s" stem id)))
    (puthash candidate t seen)
    (expand-file-name (concat candidate ".org") directory)))

(defun org-confluence-sync--child-page-files (children directory)
  "Return an alist mapping CHILDREN to unique Org files under DIRECTORY."
  (let ((seen (make-hash-table :test #'equal))
	files)
    (dolist (child children)
      (push (cons child (org-confluence-sync--child-page-file child directory seen)) files))
    (nreverse files)))

(defun org-confluence-sync--child-page-display (child)
  "Return a completion display string for CHILD."
  (let* ((type (plist-get child :type))
	 (folderp (equal type "folder"))
	 (title (or (plist-get child :title)
		    (if folderp "Untitled folder" "Untitled child")))
	 (space (plist-get child :space))
	 (id (plist-get child :id))
	 (prefix (if folderp "📁 " "📄 "))
	 (suffix (if folderp "/" "")))
    (if space
	(format "%s%s%s — %s · %s" prefix title suffix space id)
      (format "%s%s%s — %s" prefix title suffix id))))

(defun org-confluence-sync--read-child-page (children &optional prompt)
  "Read and return one child plist from CHILDREN using completion.
PROMPT defaults to a child-pulling prompt."
  (unless children
    (user-error "Confluence page has no direct child pages"))
  (let* ((candidates (mapcar (lambda (child)
			       (cons (org-confluence-sync--child-page-display child) child))
			     children))
	 (choice (completing-read (or prompt "Pull child page: ") candidates nil t)))
    (cdr (assoc choice candidates))))

(defun org-confluence-sync--read-descendant-page (root-page-id)
  "Browse descendants under ROOT-PAGE-ID and return a selected page plist."
  (let ((content-type "page")
	(content-id root-page-id)
	(path nil)
	children child)
    (while (not child)
      (setq children (if (equal content-type "folder")
			 (org-confluence-sync--list-folder-children content-id)
		       (org-confluence-list-child-pages content-id)))
      (setq child (org-confluence-sync--read-child-page
		   children
		   (if path
		       (format "Pull descendant from %s: " (string-join (reverse path) " / "))
		     "Pull descendant: ")))
      (when (equal (plist-get child :type) "folder")
	(push (or (plist-get child :title) (plist-get child :id)) path)
	(setq content-type "folder"
	      content-id (plist-get child :id)
	      child nil)))
    child))

(defun org-confluence-sync--current-page-directory ()
  "Return the current visited file directory or signal a user error."
  (unless buffer-file-name
    (user-error "Current buffer is not visiting a file"))
  (file-name-directory (expand-file-name buffer-file-name)))

;;;###autoload
(defun org-confluence-pull-child-page (&optional page-id options)
  "Select one descendant page of PAGE-ID and pull it beside the current Org file.
When PAGE-ID is nil, use the current buffer's `#+CONFLUENCE_PAGE_ID'.  Selection
uses `completing-read' as a remote tree browser: choosing folders descends into
them, while choosing a page materializes it through
`org-confluence-pull-to-file'.  The pulled page is opened with `find-file' and
the pull result plist is returned.  OPTIONS is passed to
`org-confluence-pull-to-file'."
  (interactive)
  (let* ((id (or page-id
		 (org-confluence-api--page-id-from-buffer)
		 (user-error "Current Org buffer is not linked to a Confluence page")))
	 (directory (org-confluence-sync--current-page-directory))
	 (children (org-confluence-list-child-pages id))
	 (child (org-confluence-sync--read-descendant-page id))
	 (files (org-confluence-sync--child-page-files children directory))
	 (file (or (cdr (assoc child files))
		   (org-confluence-sync--child-page-file
		    child directory (make-hash-table :test #'equal))))
	 (result (org-confluence-pull-to-file (plist-get child :id) file options)))
    (find-file (plist-get result :file))
    (message "Pulled Confluence child page %s into %s"
	     (plist-get result :page-id)
	     (plist-get result :file))
    result))

;;;###autoload
(defun org-confluence-pull-child-pages (page-id directory &optional options)
  "Pull direct children of Confluence PAGE-ID into Org files under DIRECTORY.
Each child page is materialized with `org-confluence-pull-to-file'.  File names
are derived from child titles, with page IDs used for missing titles and title
collisions.  OPTIONS is passed to `org-confluence-pull-to-file'.  Return a
summary plist containing :status, :page-id, :directory, :children, :created, and
:refreshed."
  (unless (and (stringp directory) (not (string-empty-p directory)))
    (user-error "Target directory is required"))
  (let* ((target-directory (file-name-as-directory (expand-file-name directory)))
	 (children (org-confluence-list-child-pages page-id))
	 (files (org-confluence-sync--child-page-files children target-directory))
	 (created 0)
	 (refreshed 0)
	 results)
    (make-directory target-directory t)
    (dolist (child children)
      (let* ((child-id (plist-get child :id))
	     (file (cdr (assoc child files)))
	     (result (org-confluence-pull-to-file child-id file options)))
	(pcase (plist-get result :status)
	  ('created (setq created (1+ created)))
	  ('refreshed (setq refreshed (1+ refreshed))))
	(push (append result (list :child child)) results)))
    (list :status 'completed
	  :page-id page-id
	  :directory target-directory
	  :children (nreverse results)
	  :created created
	  :refreshed refreshed)))

(defun org-confluence-sync--json-object-string-p (text)
  "Return non-nil when TEXT appears to be a JSON object string."
  (and (stringp text)
       (string-prefix-p "{" (string-trim-left text))))

(defun org-confluence-sync--adf-empty-inline-p (node)
  "Return non-nil when ADF NODE has no visible inline content."
  (and (listp node)
       (not (and (equal "text" (alist-get 'type node))
		 (org-confluence-api--present-string-p (alist-get 'text node))))
       (let ((content (alist-get 'content node)))
	 (or (null content)
	     (and (listp content)
		  (seq-every-p #'org-confluence-sync--adf-empty-inline-p content))))))

(defun org-confluence-sync--empty-adf-p (value)
  "Return non-nil when VALUE is an empty Atlassian Document Format document."
  (when (org-confluence-sync--json-object-string-p value)
    (when-let* ((json (ignore-errors
			(json-parse-string value :object-type 'alist :array-type 'list))))
      (and (equal "doc" (alist-get 'type json))
	   (let ((content (alist-get 'content json)))
	     (or (null content)
		 (and (listp content)
		      (seq-every-p #'org-confluence-sync--adf-empty-inline-p content))))))))

(defun org-confluence-sync--adf-p (value)
  "Return non-nil when VALUE appears to be Atlassian Document Format JSON."
  (when (org-confluence-sync--json-object-string-p value)
    (when-let* ((json (ignore-errors
			(json-parse-string value :object-type 'alist :array-type 'list))))
      (equal "doc" (alist-get 'type json)))))

(defun org-confluence-sync--storage-to-org (page-id storage &optional directory)
  "Return Org imported from PAGE-ID STORAGE.
DIRECTORY, when non-nil, is used to resolve Confluence people labels.  Empty
Atlassian Document Format bodies are treated as empty page bodies.  Non-empty
ADF is refused until an ADF importer exists."
  (cond
   ((org-confluence-sync--empty-adf-p storage) "")
   ((org-confluence-sync--adf-p storage)
    (user-error "Confluence page %s returned unsupported Atlassian Document Format body"
		page-id))
   (t (org-confluence-import-storage-to-org storage directory))))

(defun org-confluence-sync--timestamp ()
  "Return timestamp for page sync metadata."
  (format-time-string "%Y-%m-%dT%H:%M:%S%z"))

(defun org-confluence-sync--stamp-metadata (version storage-hash local-hash)
  "Stamp sync metadata VERSION, STORAGE-HASH, and LOCAL-HASH in current buffer."
  (when version
    (org-confluence-sync--set-keyword "CONFLUENCE_PAGE_VERSION" version))
  (org-confluence-sync--set-keyword "CONFLUENCE_PAGE_STORAGE_HASH" storage-hash)
  (org-confluence-sync--set-keyword "CONFLUENCE_LOCAL_ORG_HASH" local-hash)
  (org-confluence-sync--set-keyword "CONFLUENCE_PAGE_LAST_SYNCED_AT"
				    (org-confluence-sync--timestamp)))

(defun org-confluence-sync--body-start ()
  "Return position after leading Org keyword block."
  (save-excursion
    (goto-char (point-min))
    (while (and (not (eobp))
		(looking-at-p "^[ \t]*#\\+[^:\n]+:.*$"))
      (forward-line 1))
    (while (and (not (eobp))
		(looking-at-p "^[ \t]*$"))
      (forward-line 1))
    (point)))

(defun org-confluence-sync--body-empty-p ()
  "Return non-nil when current Org buffer body has no content."
  (save-excursion
    (goto-char (org-confluence-sync--body-start))
    (string-empty-p (string-trim (buffer-substring-no-properties (point) (point-max))))))

(defun org-confluence-sync--replace-body (org-body)
  "Replace current Org document body with ORG-BODY, preserving leading keywords."
  (let ((start (org-confluence-sync--body-start)))
    (delete-region start (point-max))
    (goto-char start)
    (insert (string-trim org-body) "\n")))

(defun org-confluence-sync--conflict-buffer
    (page-id local-hash stored-local-hash remote-version stored-version remote-org)
  "Open a conflict buffer for PAGE-ID and return it."
  (let ((buffer (get-buffer-create "*Org Confluence Sync Conflict*"))
	(local (buffer-string)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
	(erase-buffer)
	(org-mode)
	(insert (format "* Conflict: Confluence page %s\n" page-id))
	(insert (format "- local hash: %s\n- last synced local hash: %s\n" local-hash stored-local-hash))
	(insert (format "- remote version: %s\n- last synced remote version: %s\n\n" remote-version stored-version))
	(insert "** Local Org\n" local "\n\n")
	(insert "** Remote imported Org\n" remote-org "\n")
	(goto-char (point-min))))
    (pop-to-buffer buffer)
    buffer))

(defun org-confluence-sync--pull-file-buffer (file)
  "Return a buffer for FILE and whether it was already visiting it."
  (let* ((absolute-file (expand-file-name file))
	 (existing-buffer (find-buffer-visiting absolute-file)))
    (list (or existing-buffer (find-file-noselect absolute-file))
	  (and existing-buffer t))))

(defun org-confluence-sync--ensure-safe-pull-target (page-id file)
  "Return visiting-state for existing FILE if it is safe for PAGE-ID refresh."
  (when (file-exists-p file)
    (pcase-let ((`(,buffer ,preexisting) (org-confluence-sync--pull-file-buffer file)))
      (condition-case error
	  (progn
	    (with-current-buffer buffer
	      (unless (derived-mode-p 'org-mode)
		(org-mode))
	      (when (buffer-modified-p)
		(user-error "Refusing to overwrite modified buffer: %s" file))
	      (let ((existing-page-id (org-confluence-sync--keyword "CONFLUENCE_PAGE_ID")))
		(when (and existing-page-id (not (equal existing-page-id page-id)))
		  (user-error "Refusing to overwrite %s linked to Confluence page %s"
			      file existing-page-id)))
	      (let ((stored-local-hash (org-confluence-sync--keyword "CONFLUENCE_LOCAL_ORG_HASH")))
		(cond
		 (stored-local-hash
		  (unless (equal (org-confluence-sync--local-org-hash) stored-local-hash)
		    (user-error "Refusing to overwrite locally edited file: %s" file)))
		 ((not (org-confluence-sync--body-empty-p))
		  (user-error "Refusing to refresh %s without Confluence sync metadata" file)))))
	    (list buffer preexisting))
	(error
	 (when (and (buffer-live-p buffer) (not preexisting))
	   (kill-buffer buffer))
	 (signal (car error) (cdr error)))))))

(defun org-confluence-sync--pull-page (page-id)
  "Return parsed Confluence page PAGE-ID using storage body format."
  (org-confluence-response-json-alist
   (org-confluence-response-body
    (org-confluence-api--get-page page-id "storage"))))

(defun org-confluence-sync--write-pulled-page
    (page-id file title remote-version remote-storage remote-org existing-buffer)
  "Write pulled PAGE-ID data into FILE and return the target buffer.
TITLE, REMOTE-VERSION, REMOTE-STORAGE, and REMOTE-ORG describe the fetched page.
When EXISTING-BUFFER is non-nil, preserve unrelated leading Org keywords."
  (let ((buffer (or existing-buffer (find-file-noselect file))))
    (with-current-buffer buffer
      (unless (derived-mode-p 'org-mode)
	(org-mode))
      (if existing-buffer
	  (progn
	    (org-confluence-sync--set-keyword "CONFLUENCE_PAGE_ID" page-id)
	    (org-confluence-sync--replace-body remote-org))
	(erase-buffer)
	(when title
	  (insert (format "#+TITLE: %s\n" title)))
	(insert (format "#+CONFLUENCE_PAGE_ID: %s\n\n%s\n"
			page-id (string-trim remote-org))))
      (let ((local-hash (org-confluence-sync--local-org-hash)))
	(org-confluence-sync--stamp-metadata
	 remote-version
	 (org-confluence-sync--sha256 remote-storage)
	 local-hash))
      (save-buffer))
    buffer))

(defun org-confluence-pull-to-file--options-plist (options)
  "Return normalized pull OPTIONS plist."
  (if (and (= (length options) 1) (listp (car options)))
      (car options)
    options))

(defun org-confluence-pull-to-file--ensure-safe-sidecar-target (sidecar-file)
  "Refuse to update SIDECAR-FILE when its visiting buffer is modified."
  (when-let* ((buffer (find-buffer-visiting sidecar-file)))
    (when (buffer-modified-p buffer)
      (user-error "Refusing to overwrite modified comments sidecar buffer: %s"
		  sidecar-file))))

(defun org-confluence-pull-to-file--import-comments (page-id target-file target-buffer)
  "Import PAGE-ID comments for TARGET-FILE using TARGET-BUFFER.
Return comment metadata plist for `org-confluence-pull-to-file'."
  (let ((sidecar-file (org-comments-sidecar-path target-file)))
    (org-confluence-pull-to-file--ensure-safe-sidecar-target sidecar-file)
    (with-current-buffer target-buffer
      (let* ((footer-count (org-confluence-comments-import-footer page-id "storage"))
	     (inline-count (org-confluence-comments-import-inline page-id "storage")))
	(list :comments-file sidecar-file
	      :comments-count (+ footer-count inline-count)
	      :footer-comments-count footer-count
	      :inline-comments-count inline-count)))))

;;;###autoload
(defun org-confluence-pull-to-file (page-id file &rest options)
  "Pull Confluence PAGE-ID into Org FILE and return a result plist.

Fetch the remote page storage, convert it to Org, and write FILE.
Existing files keep their leading Org keyword header block.  The refresh refuses
modified buffers, different Confluence page IDs, and unsafe local body edits.
The result plist always includes :status, :page-id, and :file.  When present in
the fetched page response it also includes :title, :space, :version,
:remote-version, and :web-url.  When OPTIONS includes `:include-comments' with a
non-nil value, also import remote comments into FILE's adjacent comments
sidecar and include `:comments-file', `:comments-count',
`:footer-comments-count', and `:inline-comments-count'."
  (unless (and (stringp page-id) (not (string-empty-p page-id)))
    (user-error "Confluence page ID is required"))
  (unless (and (stringp file) (not (string-empty-p file)))
    (user-error "Target file is required"))
  (let* ((target-file (expand-file-name file))
	 (existingp (file-exists-p target-file))
	 (target-state (org-confluence-sync--ensure-safe-pull-target page-id target-file))
	 (target-buffer (car target-state))
	 (preexisting-buffer (cadr target-state))
	 (page (org-confluence-sync--pull-page page-id))
	 (title (org-confluence-sync--page-title page))
	 (remote-version (org-confluence-sync--page-version-optional page))
	 (remote-storage (org-confluence-sync--page-storage page))
	 (include-comments (plist-get (org-confluence-pull-to-file--options-plist options)
				      :include-comments))
	 (target-directory (file-name-directory target-file))
	 (remote-org (progn
		       (org-confluence-mentions-resolve-storage-users remote-storage target-directory)
		       (org-confluence-sync--storage-to-org page-id remote-storage target-directory))))
    (unless existingp
      (make-directory (file-name-directory target-file) t))
    (unwind-protect
	(progn
	  (setq target-buffer
		(org-confluence-sync--write-pulled-page
		 page-id target-file title remote-version remote-storage remote-org target-buffer))
	  (append (list :status (if existingp 'refreshed 'created)
			:page-id page-id
			:file target-file)
		  (org-confluence-sync--page-result-metadata page)
		  (when include-comments
		    (org-confluence-pull-to-file--import-comments
		     page-id target-file target-buffer))))
      (when (and (buffer-live-p target-buffer)
		 (not preexisting-buffer))
	(kill-buffer target-buffer)))))

;;;###autoload
(defun org-confluence-sync-page-current (&optional page-id)
  "Synchronize current Org buffer main page content with Confluence PAGE-ID."
  (interactive)
  (let* ((id (or page-id (org-confluence-api--page-id-from-buffer)
		 (read-string "Confluence page ID: ")))
	 (page (org-confluence-response-json-alist
		(org-confluence-response-body
		 (org-confluence-api--get-page id "storage"))))
	 (remote-version (org-confluence-sync--page-version page))
	 (remote-storage (org-confluence-sync--page-storage page))
	 (remote-storage-hash (org-confluence-sync--sha256 remote-storage))
	 (remote-directory (and buffer-file-name (file-name-directory buffer-file-name)))
	 (remote-org (progn
		       (org-confluence-mentions-resolve-storage-users remote-storage remote-directory)
		       (org-confluence-sync--storage-to-org id remote-storage remote-directory)))
	 (stored-version (org-confluence-sync--keyword "CONFLUENCE_PAGE_VERSION"))
	 (stored-local-hash (org-confluence-sync--keyword "CONFLUENCE_LOCAL_ORG_HASH"))
	 (local-hash (org-confluence-sync--local-org-hash)))
    (cond
     ((not stored-version)
      (org-confluence-sync--stamp-metadata remote-version remote-storage-hash local-hash)
      (save-buffer)
      (message "Initialized Confluence sync metadata for page %s" id)
      (list :initialized 1 :page-id id :remote-version remote-version))
     ((and (not (equal remote-version stored-version))
	   stored-local-hash
	   (not (equal local-hash stored-local-hash)))
      (org-confluence-sync--conflict-buffer
       id local-hash stored-local-hash remote-version stored-version remote-org)
      (list :conflict 1 :page-id id :remote-version remote-version))
     ((not (equal remote-version stored-version))
      (org-confluence-sync--replace-body remote-org)
      (setq local-hash (org-confluence-sync--local-org-hash))
      (org-confluence-sync--stamp-metadata remote-version remote-storage-hash local-hash)
      (save-buffer)
      (message "Pulled Confluence page %s v%s into local Org" id remote-version)
      (list :pulled 1 :page-id id :remote-version remote-version))
     ((and stored-local-hash (not (equal local-hash stored-local-hash)))
      (org-confluence-publish)
      (let* ((updated-page (org-confluence-response-json-alist
			    (org-confluence-response-body
			     (org-confluence-api--get-page id "storage"))))
	     (updated-version (org-confluence-sync--page-version updated-page))
	     (updated-storage (org-confluence-sync--page-storage updated-page))
	     (updated-storage-hash (org-confluence-sync--sha256 updated-storage)))
	(org-confluence-sync--stamp-metadata updated-version updated-storage-hash local-hash)
	(save-buffer)
	(message "Pushed local Org to Confluence page %s v%s" id updated-version)
	(list :pushed 1 :page-id id :remote-version updated-version)))
     (t
      (message "Confluence page %s already synchronized" id)
      (list :noop 1 :page-id id :remote-version remote-version)))))

(defun org-confluence-sync--pending-comment-ids ()
  "Return local sidecar comment IDs that should be pushed to Confluence."
  (let (ids)
    (org-map-entries
     (lambda ()
       (when (and (org-entry-get nil "ORG_COMMENTS_ID")
		  (not (equal "RESOLVED" (org-get-todo-state)))
		  (or (not (org-entry-get nil "ORG_COMMENTS_REMOTE_ID"))
		      (org-entry-get nil "ORG_COMMENTS_LOCAL_UPDATED_AT")))
	 (push (org-entry-get nil "ORG_COMMENTS_ID") ids)))
     nil 'file)
    (nreverse ids)))

(defun org-confluence-sync--goto-sidecar-comment-id (comment-id)
  "Move point to sidecar COMMENT-ID heading and return non-nil when found."
  (org-comments-sidecar-goto-comment (list :id comment-id)))

(defun org-confluence-sync--push-pending-comments (source-buffer &optional page-id)
  "Push pending sidecar comments for SOURCE-BUFFER to Confluence PAGE-ID."
  (let* ((source-file (buffer-file-name source-buffer))
	 (sidecar-file (and source-file (org-comments-sidecar-path source-file)))
	 (pushed 0)
	 errors)
    (when (and sidecar-file (file-exists-p sidecar-file))
      (with-current-buffer (find-file-noselect sidecar-file)
	(unless (derived-mode-p 'org-mode)
	  (org-mode))
	(dolist (comment-id (org-confluence-sync--pending-comment-ids))
	  (condition-case error
	      (when (org-confluence-sync--goto-sidecar-comment-id comment-id)
		(org-confluence-comments-push-current page-id)
		(setq pushed (1+ pushed)))
	    (error
	     (push (format "%s: %s" comment-id (error-message-string error)) errors))))))
    (list :pushed pushed :errors (nreverse errors))))

;;;###autoload
(defun org-confluence-sync-current (&optional page-id body-format)
  "Synchronize current Org page and sidecar comments with Confluence.
PAGE-ID overrides the current buffer's `CONFLUENCE_PAGE_ID'.  BODY-FORMAT is
passed to comment import and defaults to storage.  When page sync detects a
conflict, comments are not imported or pushed."
  (interactive)
  (let* ((source-buffer (current-buffer))
	 (id (org-confluence-page-id-or-read page-id))
	 (page-result (org-confluence-sync-page-current id)))
    (if (plist-get page-result :conflict)
	(progn
	  (message "Confluence page conflict detected; skipped comment sync")
	  (list :page page-result :comments-skipped 1))
      (with-current-buffer source-buffer
	(let* ((comment-result (org-confluence-comments--sync id body-format))
	       (pushed (plist-get comment-result :comments-pushed))
	       (errors (plist-get comment-result :comment-push-errors)))
	  (org-comments-sync-report-provider-message
	   "Confluence" :pushed pushed :errors (length errors))
	  (append (list :page page-result) comment-result))))))

(provide 'org-confluence-sync)
;;; org-confluence-sync.el ends here
