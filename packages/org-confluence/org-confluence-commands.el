;;; org-confluence-commands.el --- Org Confluence publish commands -*- lexical-binding: t; -*-

;;; Commentary:
;; User-facing commands for publishing Org buffers to Confluence via cfl.

;;; Code:

(require 'org)
(require 'seq)
(require 'subr-x)
(require 'xml)

(require 'org-confluence-api)
(require 'org-confluence-export)

(defun hub/confluence-commands--write-temp-xhtml (xhtml)
  "Write XHTML to a temporary .xhtml file and return its path."
  (let ((file (make-temp-file "org-confluence-" nil ".xhtml")))
    (with-temp-file file
      (insert xhtml))
    file))

(defun hub/confluence-commands--run-process (command output-buffer)
  "Run shell COMMAND into OUTPUT-BUFFER and return its exit code."
  (with-current-buffer (get-buffer-create output-buffer)
    (erase-buffer)
    (process-file shell-file-name nil output-buffer nil shell-command-switch command)))

(defun hub/confluence-commands--command-output (output-buffer)
  "Return trimmed contents of OUTPUT-BUFFER."
  (when-let* ((buffer (get-buffer output-buffer)))
    (with-current-buffer buffer
      (string-trim (buffer-string)))))

(defun hub/confluence-commands--run (command)
  "Run shell COMMAND and signal `user-error' on failure."
  (unless (hub/confluence-api--cfl-available-p)
    (user-error "Cannot find `%s' on Emacs PATH. Current PATH: %s"
		hub/confluence-api-cfl-command
		(getenv "PATH")))
  (let* ((output-buffer "*hub-confluence-cfl*")
	 (default-directory user-emacs-directory)
	 (exit-code (hub/confluence-commands--run-process command output-buffer)))
    (unless (zerop exit-code)
      (let ((output (hub/confluence-commands--command-output output-buffer)))
	(user-error "Confluence publish failed with exit code %s.\nCommand: %s\nOutput: %s"
		    exit-code command (if (string-empty-p (or output "")) "<empty>" output))))
    exit-code))

(defun hub/confluence-commands--run-output (command)
  "Run shell COMMAND and return its trimmed output."
  (unless (hub/confluence-api--cfl-available-p)
    (user-error "Cannot find `%s' on Emacs PATH. Current PATH: %s"
		hub/confluence-api-cfl-command
		(getenv "PATH")))
  (let* ((output-buffer "*hub-confluence-cfl*")
	 (default-directory user-emacs-directory)
	 (exit-code (hub/confluence-commands--run-process command output-buffer))
	 (output (hub/confluence-commands--command-output output-buffer)))
    (unless (zerop exit-code)
      (user-error "Confluence command failed with exit code %s.\nCommand: %s\nOutput: %s"
		  exit-code command (if (string-empty-p (or output "")) "<empty>" output)))
    output))

(defun hub/confluence-commands--title-from-buffer ()
  "Return the Org #+TITLE value from the current buffer, or nil."
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search t))
      (when (re-search-forward "^[	]*#\\+TITLE:[	]*\\(.*?\\)[	]*$" nil t)
	(let ((title (string-trim (match-string-no-properties 1))))
	  (unless (string-empty-p title) title))))))

(defun hub/confluence-commands--asset-filename-map (assets)
  "Return export filename mapping for image ASSETS."
  (mapcar (lambda (asset)
	    (cons (plist-get asset :source-link)
		  (plist-get asset :filename)))
	  assets))

(defun hub/confluence-commands--duplicate-attachment-error-p (error)
  "Return non-nil when ERROR is cfl's duplicate attachment failure."
  (string-match-p "Cannot add a new attachment with same file name as an existing attachment"
		  (error-message-string error)))

(defun hub/confluence-import--node-tag (node)
  "Return NODE tag, or nil for text nodes."
  (when (consp node) (car node)))

(defun hub/confluence-import--node-attributes (node)
  "Return NODE attributes."
  (when (consp node) (cadr node)))

(defun hub/confluence-import--node-children (node)
  "Return NODE children."
  (when (consp node) (cddr node)))

(defun hub/confluence-import--attribute (node attribute)
  "Return NODE ATTRIBUTE value."
  (cdr (assq attribute (hub/confluence-import--node-attributes node))))

(defun hub/confluence-import--macro-name (node)
  "Return Confluence macro name for NODE."
  (hub/confluence-import--attribute node 'ac:name))

(defun hub/confluence-import--macro-parameter (node name)
  "Return Confluence macro NODE parameter NAME, or nil."
  (seq-some (lambda (child)
	      (when (and (eq (hub/confluence-import--node-tag child) 'ac:parameter)
			 (string= (or (hub/confluence-import--attribute child 'ac:name) "") name))
		(hub/confluence-import--inline child)))
	    (hub/confluence-import--node-children node)))

(defun hub/confluence-import--status (node)
  "Convert Confluence status macro NODE to an Org status link."
  (let ((title (or (hub/confluence-import--macro-parameter node "title") ""))
	(colour (or (hub/confluence-import--macro-parameter node "colour") "Grey")))
    (format "[[confluence-status:%s][%s]]"
	    (string-trim colour)
	    (string-trim title))))

(defun hub/confluence-import--emoticon-unicode-fallback (node)
  "Return known Unicode fallback for Confluence emoticon NODE."
  (let ((id (hub/confluence-import--attribute node 'ac:emoji-id))
	(name (hub/confluence-import--attribute node 'ac:name))
	(shortname (hub/confluence-import--attribute node 'ac:emoji-shortname)))
    (cond
     ((or (string= (or id "") "atlassian-info")
	  (string= (or name "") "information")
	  (string= (or shortname "") ":info:"))
      "ℹ️")
     ((string= (or shortname "") ":document:")
      "📄"))))

(defun hub/confluence-import--emoticon (node)
  "Convert Confluence emoticon NODE to readable Org text."
  (or (hub/confluence-import--emoticon-unicode-fallback node)
      (hub/confluence-import--attribute node 'ac:emoji-fallback)
      (hub/confluence-import--attribute node 'ac:emoji-shortname)
      (hub/confluence-import--attribute node 'ac:name)
      ""))

(defun hub/confluence-import--callout-macro-p (node)
  "Return non-nil when NODE is a panel-like Confluence macro."
  (member (hub/confluence-import--macro-name node)
	  '("info" "note" "warning" "tip" "panel")))

(defun hub/confluence-import--rich-text-body (node)
  "Return Org text converted from NODE's rich text body."
  (hub/confluence-import--join-blocks
   (mapcar (lambda (child)
	     (when (eq (hub/confluence-import--node-tag child) 'ac:rich-text-body)
	       (hub/confluence-import--block child)))
	   (hub/confluence-import--node-children node))))

(defun hub/confluence-import--callout (node)
  "Convert Confluence panel-like macro NODE to semantic Org callout."
  (let* ((type (hub/confluence-import--macro-name node))
	 (title (hub/confluence-import--macro-parameter node "title"))
	 (attributes (format "#+ATTR_CALLOUT: :type %s" type))
	 (body (hub/confluence-import--rich-text-body node)))
    (when (and title (not (string-empty-p (string-trim title))))
      (setq attributes (format "%s :title %S" attributes (string-trim title))))
    (format "%s\n#+begin_callout\n%s\n#+end_callout" attributes body)))

(defun hub/confluence-import--clean-inline-spacing (text)
  "Clean Org inline TEXT spacing introduced for markup boundaries."
  (let ((cleaned (replace-regexp-in-string "[[:space:]]+" " " text)))
    (setq cleaned (replace-regexp-in-string "[[:space:]]+\\([.,;:!?)]\\)" "\\1" cleaned))
    (string-trim cleaned)))

(defun hub/confluence-import--markup (marker contents)
  "Return Org inline markup using MARKER around CONTENTS."
  (format " %s%s%s " marker (string-trim contents) marker))

(defun hub/confluence-import--inline (node)
  "Convert XHTML NODE to inline Org text."
  (cond
   ((stringp node) node)
   ((not (consp node)) "")
   (t
    (let ((contents (mapconcat #'hub/confluence-import--inline
			       (hub/confluence-import--node-children node) "")))
      (pcase (hub/confluence-import--node-tag node)
	('ac:emoticon (hub/confluence-import--emoticon node))
	('ac:structured-macro
	 (if (string= (or (hub/confluence-import--macro-name node) "") "status")
	     (hub/confluence-import--status node)
	   (hub/confluence-import--clean-inline-spacing contents)))
	((or 'strong 'b) (hub/confluence-import--markup "*" contents))
	((or 'em 'i) (hub/confluence-import--markup "/" contents))
	('u (hub/confluence-import--markup "_" contents))
	('strike (hub/confluence-import--markup "+" contents))
	('code (hub/confluence-import--markup "~" contents))
	('a (let ((href (hub/confluence-import--attribute node 'href)))
	      (if href (format "[[%s][%s]]" href (hub/confluence-import--clean-inline-spacing contents))
		(hub/confluence-import--clean-inline-spacing contents))))
	(_ (hub/confluence-import--clean-inline-spacing contents)))))))

(defun hub/confluence-import--join-blocks (blocks)
  "Join non-empty Org BLOCKS with newlines."
  (string-join (seq-filter (lambda (block)
			     (not (string-empty-p (string-trim (or block "")))))
			   blocks)
	       "\n"))

(defun hub/confluence-import--list-child-p (node)
  "Return non-nil when NODE is a list element."
  (memq (hub/confluence-import--node-tag node) '(ul ol)))

(defun hub/confluence-import--list-item-text (item)
  "Return ITEM direct inline text, excluding nested lists."
  (string-trim
   (mapconcat #'hub/confluence-import--inline
	      (seq-remove #'hub/confluence-import--list-child-p
			  (hub/confluence-import--node-children item))
	      "")))

(defun hub/confluence-import--indent-lines (text spaces)
  "Indent every line in TEXT by SPACES spaces."
  (let ((prefix (make-string spaces ?\s)))
    (string-join
     (mapcar (lambda (line) (concat prefix line))
	     (split-string text "\n"))
     "\n")))

(defun hub/confluence-import--list (list-node &optional level)
  "Convert LIST-NODE to an Org list at nesting LEVEL."
  (let ((index 0)
	(level (or level 0))
	(ordered (eq (hub/confluence-import--node-tag list-node) 'ol)))
    (hub/confluence-import--join-blocks
     (mapcar
      (lambda (child)
	(when (eq (hub/confluence-import--node-tag child) 'li)
	  (when ordered (setq index (1+ index)))
	  (let* ((marker (if ordered (format "%d." index) "-"))
		 (line (format "%s%s %s" (make-string (* 2 level) ?\s)
			       marker
			       (hub/confluence-import--list-item-text child)))
		 (nested (hub/confluence-import--join-blocks
			  (mapcar (lambda (nested-list)
				    (when (hub/confluence-import--list-child-p nested-list)
				      (hub/confluence-import--list nested-list (1+ level))))
				  (hub/confluence-import--node-children child)))))
	    (hub/confluence-import--join-blocks (list line nested)))))
      (hub/confluence-import--node-children list-node)))))

(defun hub/confluence-import--children-by-tag (node tag)
  "Return direct children of NODE with TAG."
  (seq-filter (lambda (child) (eq (hub/confluence-import--node-tag child) tag))
	      (hub/confluence-import--node-children node)))

(defun hub/confluence-import--table-rows (table)
  "Return Confluence storage TABLE rows."
  (let ((direct-rows (hub/confluence-import--children-by-tag table 'tr)))
    (if direct-rows
	direct-rows
      (apply #'append
	     (mapcar (lambda (section)
		       (hub/confluence-import--children-by-tag section 'tr))
		     (seq-filter (lambda (child)
				   (memq (hub/confluence-import--node-tag child) '(thead tbody)))
				 (hub/confluence-import--node-children table)))))))

(defun hub/confluence-import--table-row-cells (row)
  "Return ROW cells as Org-safe inline text."
  (mapcar (lambda (cell)
	    (string-trim
	     (replace-regexp-in-string "|" "\\\\vert{}"
				       (hub/confluence-import--inline cell)
				       t t)))
	  (seq-filter (lambda (child)
			(memq (hub/confluence-import--node-tag child) '(th td)))
		      (hub/confluence-import--node-children row))))

(defun hub/confluence-import--org-table-line (cells)
  "Return an Org table row for CELLS."
  (format "| %s |" (string-join cells " | ")))

(defun hub/confluence-import--org-table-separator (cells)
  "Return an Org table separator sized for CELLS."
  (format "|%s|"
	  (string-join
	   (mapcar (lambda (cell)
		     (make-string (+ 2 (length cell)) ?-))
		   cells)
	   "+")))

(defun hub/confluence-import--table (table)
  "Convert Confluence storage TABLE to an Org table."
  (let ((rows (mapcar #'hub/confluence-import--table-row-cells
		      (hub/confluence-import--table-rows table))))
    (hub/confluence-import--join-blocks
     (append (list (hub/confluence-import--org-table-line (car rows))
		   (hub/confluence-import--org-table-separator (car rows)))
	     (mapcar #'hub/confluence-import--org-table-line (cdr rows))))))

(defun hub/confluence-import--block (node)
  "Convert XHTML NODE to Org block text."
  (pcase (hub/confluence-import--node-tag node)
    ((or 'html 'body 'ac:rich-text-body)
     (hub/confluence-import--join-blocks
      (mapcar #'hub/confluence-import--block
	      (hub/confluence-import--node-children node))))
    ('ac:structured-macro
     (cond
      ((string= (or (hub/confluence-import--macro-name node) "") "status")
       (hub/confluence-import--status node))
      ((hub/confluence-import--callout-macro-p node)
       (hub/confluence-import--callout node))
      (t (hub/confluence-import--rich-text-body node))))
    ('h1 (format "* %s" (hub/confluence-import--inline node)))
    ('h2 (format "** %s" (hub/confluence-import--inline node)))
    ('h3 (format "*** %s" (hub/confluence-import--inline node)))
    ('h4 (format "**** %s" (hub/confluence-import--inline node)))
    ('p (hub/confluence-import--inline node))
    ((or 'ul 'ol) (hub/confluence-import--list node))
    ('table (hub/confluence-import--table node))
    (_ (hub/confluence-import--inline node))))

(defun hub/confluence-import-storage-to-org (xhtml)
  "Convert Confluence storage XHTML string XHTML to Org text."
  (with-temp-buffer
    (insert xhtml)
    (let ((tree (libxml-parse-html-region (point-min) (point-max))))
      (string-trim (hub/confluence-import--block tree)))))

(defun hub/confluence-commands--upload-asset (page-id asset upload-directory)
  "Upload one image ASSET to Confluence PAGE-ID from UPLOAD-DIRECTORY."
  (let ((upload-path (expand-file-name (plist-get asset :filename) upload-directory)))
    (copy-file (plist-get asset :source-path) upload-path t)
    (condition-case err
	(hub/confluence-commands--run
	 (hub/confluence-api--attachment-upload-command page-id upload-path))
      (user-error
       (if (hub/confluence-commands--duplicate-attachment-error-p err)
	   (message "Confluence attachment already exists, reusing %s"
		    (plist-get asset :filename))
	 (signal (car err) (cdr err)))))))

(defun hub/confluence-commands--unique-upload-assets (assets)
  "Return ASSETS deduplicated by generated attachment filename."
  (let ((seen nil)
	(unique nil))
    (dolist (asset assets (nreverse unique))
      (let ((filename (plist-get asset :filename)))
	(unless (member filename seen)
	  (push filename seen)
	  (push asset unique))))))

(defun hub/confluence-commands--upload-assets (page-id assets)
  "Upload image ASSETS to Confluence PAGE-ID."
  (when assets
    (let ((upload-directory (make-temp-file "org-confluence-assets-" t)))
      (unwind-protect
	  (dolist (asset (hub/confluence-commands--unique-upload-assets assets))
	    (hub/confluence-commands--upload-asset page-id asset upload-directory))
	(delete-directory upload-directory t)))))

;;;###autoload
(defun hub/confluence-publish (&optional async subtreep visible-only body-only ext-plist)
  "Publish the current Org buffer or subtree to an existing Confluence page.

The document must contain #+CONFLUENCE_PAGE_ID.  When SUBTREEP is non-nil, a
CONFLUENCE_PAGE_ID Org property on the current subtree takes precedence.  The
Org document is exported to Confluence Storage Format XHTML and passed to `cfl
page edit --file --storage'.  ASYNC, SUBTREEP, VISIBLE-ONLY, BODY-ONLY, and
EXT-PLIST follow Org export conventions."
  (interactive)
  (ignore async)
  (let* ((page-id (hub/confluence-api--page-id-from-buffer subtreep))
	 (assets (org-confluence-image-assets subtreep))
	 (xhtml-file nil))
    (unwind-protect
	(progn
	  (setq xhtml-file
		(hub/confluence-commands--write-temp-xhtml
		 (org-confluence-export nil subtreep visible-only body-only
					(append (list :confluence-image-filenames
						      (hub/confluence-commands--asset-filename-map assets))
						ext-plist))))
	  (hub/confluence-commands--upload-assets page-id assets)
	  (hub/confluence-commands--run
	   (hub/confluence-api--page-update-command page-id xhtml-file))
	  (message "Published Org buffer to Confluence page %s" page-id)
	  page-id)
      (when (and xhtml-file (file-exists-p xhtml-file))
	(delete-file xhtml-file)))))

;;;###autoload
(defun hub/confluence-publish-from-export-dispatch
    (&optional async subtreep visible-only body-only ext-plist)
  "Publish or create through `org-export-dispatch' using Org export options."
  (interactive)
  (hub/confluence-publish-dwim nil nil async subtreep visible-only body-only ext-plist))

;;;###autoload
(defun hub/confluence-open-page (&optional page-id space)
  "Open Confluence PAGE-ID in browser using optional SPACE."
  (interactive)
  (let* ((id (or page-id (hub/confluence-api--page-id-from-buffer)))
	 (page-space (or space (hub/confluence-api--space-from-buffer)))
	 (url (hub/confluence-api--page-url id page-space)))
    (browse-url url)
    url))

;;;###autoload
(defun hub/confluence-publish-and-open-from-export-dispatch
    (&optional async subtreep visible-only body-only ext-plist)
  "Publish or create through `org-export-dispatch', then open the page."
  (interactive)
  (let* ((page-id (hub/confluence-publish-dwim nil nil async subtreep visible-only body-only ext-plist))
	 (space (hub/confluence-api--space-from-buffer)))
    (hub/confluence-open-page page-id space)))

(defun hub/confluence-commands--created-page-id (output)
  "Return Confluence page ID parsed from cfl create OUTPUT, or nil."
  (cond
   ((string-match "\"id\"[[:space:]]*:[[:space:]]*\"?\\([0-9]+\\)\"?" output)
    (match-string 1 output))
   ((string-match "\\b\\(?:Page[[:space:]]+\\)?ID[[:space:]]*[:=][[:space:]]*\\([0-9]+\\)" output)
    (match-string 1 output))
   ((string-match "/pages/\\([0-9]+\\)" output)
    (match-string 1 output))))

(defun hub/confluence-commands--metadata-keyword-present-p (keyword)
  "Return non-nil when Org metadata KEYWORD is present in the current buffer."
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search t)
	  (regexp (format "^[ \t]*#\\+%s:" (regexp-quote keyword))))
      (re-search-forward regexp nil t))))

(defun hub/confluence-commands--insert-metadata-after-keywords (metadata)
  "Insert missing Confluence METADATA after leading Org keyword lines.

METADATA is an alist of Org keyword names to values."
  (let ((missing (seq-filter (lambda (entry)
			       (not (hub/confluence-commands--metadata-keyword-present-p (car entry))))
			     metadata)))
    (when missing
      (save-excursion
	(goto-char (point-min))
	(while (looking-at "^[ \t]*#\\+[^:\n]+:.*$")
	  (forward-line 1))
	(dolist (entry missing)
	  (insert (format "#+%s: %s\n" (car entry) (cdr entry))))))))

(defun hub/confluence-publish--record-created-page (page-id space)
  "Record created PAGE-ID and SPACE metadata in the current Org buffer."
  (hub/confluence-commands--insert-metadata-after-keywords
   `(("CONFLUENCE_PAGE_ID" . ,page-id)
     ("CONFLUENCE_SPACE" . ,space))))

;;;###autoload
(defun hub/confluence-pull (&optional page-id)
  "Fetch Confluence PAGE-ID and open a new Org buffer with imported content.

When PAGE-ID is nil, default to #+CONFLUENCE_PAGE_ID in the current Org buffer
and prompt if no page ID is available.  The page is fetched as raw Confluence
storage XHTML using cfl and converted to a conservative Org representation."
  (interactive)
  (let* ((id (or page-id
		 (hub/confluence-api--page-id-from-buffer)
		 (read-string "Confluence page ID: ")))
	 (xhtml (hub/confluence-commands--run-output
		 (hub/confluence-api--page-view-storage-command id)))
	 (org (hub/confluence-import-storage-to-org xhtml))
	 (buffer (generate-new-buffer (format "*Confluence %s*" id))))
    (with-current-buffer buffer
      (org-mode)
      (insert (format "#+CONFLUENCE_PAGE_ID: %s\n\n%s\n" id org))
      (goto-char (point-min)))
    (pop-to-buffer buffer)))

;;;###autoload
(defun hub/confluence-publish-dwim
    (&optional title parent-id async subtreep visible-only body-only ext-plist)
  "Publish current Org buffer to Confluence, updating or creating as needed.

When #+CONFLUENCE_PAGE_ID is present, update that page.  Otherwise use
#+CONFLUENCE_SPACE or `hub/confluence-api-default-space' and create a new page
using TITLE, prompting interactively when needed.  Optional PARENT-ID is passed
to cfl as the parent page.  When the new page ID can be parsed from cfl output,
record Confluence metadata, upload image assets, and re-save the page content.
ASYNC, SUBTREEP, VISIBLE-ONLY, BODY-ONLY, and EXT-PLIST follow Org export
conventions."
  (interactive)
  (let ((page-id (hub/confluence-api--page-id-from-buffer subtreep)))
    (if page-id
	(hub/confluence-publish async subtreep visible-only body-only ext-plist)
      (let ((assets (org-confluence-image-assets subtreep)))
	(let* ((space (hub/confluence-api--space-from-buffer))
	       (page-title (or title
			       (hub/confluence-commands--title-from-buffer)
			       (read-string "Confluence page title: ")))
	       (xhtml-file nil))
	  (unwind-protect
	      (let ((command nil)
		    (output nil)
		    (created-page-id nil))
		(setq xhtml-file
		      (hub/confluence-commands--write-temp-xhtml
		       (org-confluence-export async subtreep visible-only body-only
					      (append (list :confluence-image-filenames
							    (hub/confluence-commands--asset-filename-map assets))
						      ext-plist))))
		(setq command (hub/confluence-api--page-create-command space page-title xhtml-file parent-id))
		(setq output (hub/confluence-commands--run-output command))
		(setq created-page-id (hub/confluence-commands--created-page-id output))
		(when created-page-id
		  (hub/confluence-publish--record-created-page created-page-id space)
		  (hub/confluence-commands--upload-assets created-page-id assets)
		  (hub/confluence-commands--run
		   (hub/confluence-api--page-update-command created-page-id xhtml-file)))
		(message "Created Confluence page %s in space %s%s"
			 page-title space
			 (if created-page-id (format " (ID %s)" created-page-id) ""))
		created-page-id)
	    (when (and xhtml-file (file-exists-p xhtml-file))
	      (delete-file xhtml-file))))))))

(provide 'org-confluence-commands)
(provide 'org/export-confluence-commands)
;;; org-confluence-commands.el ends here
