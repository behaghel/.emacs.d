;;; org-confluence-commands.el --- Org Confluence publish commands -*- lexical-binding: t; -*-

;;; Commentary:
;; User-facing commands for publishing Org buffers to Confluence via cfl.

;;; Code:

(require 'hub-confluence-people)
(require 'hub-org-comments)
(require 'browse-url)
(require 'cl-lib)
(require 'json)
(require 'org)
(require 'ox-html)
(require 'seq)
(require 'subr-x)
(require 'xml)

(require 'org-confluence-api)
(require 'org-confluence-export)

(defcustom hub/confluence-comment-import-resolve-people t
  "Whether comment import resolves Confluence people in bulk afterwards."
  :type 'boolean
  :group 'hub/confluence-api)

(defcustom hub/confluence-publish-preserve-inline-comments t
  "Whether publishing should reinsert Confluence inline comment markers.

When non-nil, existing page storage and inline comment metadata are fetched before
page update.  Active inline comments with marker references are wrapped back
around matching selected text in the newly exported storage XHTML so Confluence
keeps their anchors."
  :type 'boolean
  :group 'hub/confluence-api)

(defconst hub/confluence-commands--inline-comment-marker-regexp
  "[<]ac:inline-comment-marker[^>]*ac:ref=\"\\([^\"]+\\)\"[^>]*>\\([^<]*\\)[<]/ac:inline-comment-marker>"
  "Regexp matching a simple Confluence storage inline comment marker.")

(defconst hub/confluence-commands--inline-comment-context-window 100
  "Number of storage bytes used around inline comment markers for relocation.")

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

(defun hub/confluence-commands--response-body (response)
  "Return JSON body string from REST RESPONSE."
  (cond
   ((stringp response) response)
   ((plist-member response :body) (plist-get response :body))
   (t (user-error "Confluence REST response did not include a body"))))

(defun hub/confluence-commands--json-alist (json)
  "Parse JSON string JSON into alists and lists."
  (json-parse-string json :object-type 'alist :array-type 'list :null-object nil :false-object nil))

(defun hub/confluence-commands--comment-results (response)
  "Return comment results from REST RESPONSE."
  (let* ((json (hub/confluence-commands--json-alist
		(hub/confluence-commands--response-body response)))
	 (results (or (alist-get 'results json)
		      (alist-get 'values json))))
    (unless (listp results)
      (user-error "Confluence comment response did not include results"))
    results))

(defun hub/confluence-commands--page-body-storage-value (response)
  "Return page body storage value from REST RESPONSE."
  (let* ((json (hub/confluence-commands--json-alist
		(hub/confluence-commands--response-body response)))
	 (storage (alist-get 'storage (alist-get 'body json)))
	 (value (and (listp storage) (alist-get 'value storage))))
    (unless (stringp value)
      (user-error "Confluence page response did not include body.storage.value"))
    value))

(defun hub/confluence-commands--user-results (response)
  "Return user results from REST RESPONSE."
  (let* ((json (hub/confluence-commands--json-alist
		(hub/confluence-commands--response-body response)))
	 (results (alist-get 'results json)))
    (unless (listp results)
      (user-error "Confluence user response did not include results"))
    results))

(defun hub/confluence-commands--comment-body-storage (comment)
  "Return COMMENT storage body alist, or nil."
  (alist-get 'storage (alist-get 'body comment)))

(defun hub/confluence-commands--raw-resolution-fields (comment)
  "Return raw resolution fields found in COMMENT."
  (let ((properties (alist-get 'properties comment))
	fields)
    (dolist (key '(resolutionStatus resolution-status resolved))
      (when-let* ((value (alist-get key comment)))
	(push (format "%s=%S" key value) fields)))
    (when (listp properties)
      (dolist (key '(resolutionStatus resolution-status resolved))
	(when-let* ((value (alist-get key properties)))
	  (push (format "properties.%s=%S" key value) fields))))
    (nreverse fields)))

(defun hub/confluence-commands--raw-status-fields (comment)
  "Return raw non-resolution status fields found in COMMENT."
  (let ((properties (alist-get 'properties comment))
	fields)
    (when-let* ((value (alist-get 'status comment)))
      (push (format "status=%S" value) fields))
    (when (listp properties)
      (when-let* ((value (alist-get 'status properties)))
	(push (format "properties.status=%S" value) fields)))
    (nreverse fields)))

(defun hub/confluence-commands--insert-comment-diagnostic (comment)
  "Insert one remote COMMENT diagnostic entry at point."
  (let* ((id (or (alist-get 'id comment) "<missing id>"))
	 (storage (hub/confluence-commands--comment-body-storage comment))
	 (representation (or (alist-get 'representation storage) "unknown"))
	 (resolution (or (hub/confluence-commands--remote-comment-resolution-status comment)
			 "unknown"))
	 (raw-resolution-fields (hub/confluence-commands--raw-resolution-fields comment))
	 (raw-status-fields (hub/confluence-commands--raw-status-fields comment))
	 (value (or (alist-get 'value storage) "")))
    (insert (format "- id: %s\n" id))
    (insert (format "  resolution: %s\n" resolution))
    (when raw-resolution-fields
      (insert (format "  resolution-fields: %s\n"
		      (string-join raw-resolution-fields ", "))))
    (when raw-status-fields
      (insert (format "  status-fields: %s\n"
		      (string-join raw-status-fields ", "))))
    (insert (format "  representation: %s\n" representation))
    (unless (string-empty-p value)
      (insert "  body:\n")
      (dolist (line (split-string value "\n"))
	(insert (format "    %s\n" line))))))

(defun hub/confluence-commands--insert-comment-section (kind body-format response)
  "Insert diagnostic section for comment KIND and REST RESPONSE."
  (let ((comments (hub/confluence-commands--comment-results response)))
    (insert (format "* %s\n" kind))
    (insert (format "body-format: %s\n" body-format))
    (insert (format "count: %s\n\n" (length comments)))
    (if comments
	(dolist (comment comments)
	  (hub/confluence-commands--insert-comment-diagnostic comment))
      (insert "No comments.\n"))))

(defun hub/confluence-commands--remote-comment-id (comment)
  "Return remote Confluence COMMENT id as a string, or nil."
  (when-let* ((id (alist-get 'id comment)))
    (format "%s" id)))

(defun hub/confluence-commands--remote-comment-body (comment)
  "Return raw storage body for remote Confluence COMMENT."
  (or (alist-get 'value (hub/confluence-commands--comment-body-storage comment)) ""))

(defun hub/confluence-commands--remote-comment-author-id (comment)
  "Return remote author ID for Confluence COMMENT, or nil."
  (or (alist-get 'authorId comment)
      (alist-get 'author-id comment)
      (alist-get 'authorId (alist-get 'version comment))
      (alist-get 'author-id (alist-get 'version comment))
      (alist-get 'accountId (alist-get 'author comment))))

(defun hub/confluence-commands--remote-comment-author-display-name (comment)
  "Return display author name for Confluence COMMENT, or nil."
  (or (alist-get 'displayName (alist-get 'author comment))
      (alist-get 'publicName (alist-get 'author comment))))

(defun hub/confluence-commands--remote-comment-author-name (comment)
  "Return best author label for Confluence COMMENT, or nil."
  (or (hub/confluence-commands--remote-comment-author-display-name comment)
      (hub/confluence-commands--remote-comment-author-id comment)))

(defun hub/confluence-commands--remote-comment-created-at (comment)
  "Return remote created timestamp for Confluence COMMENT, or nil."
  (or (alist-get 'createdAt comment)
      (alist-get 'created-at comment)
      (alist-get 'createdAt (alist-get 'version comment))
      (alist-get 'created-at (alist-get 'version comment))))

(defun hub/confluence-commands--remote-comment-version-number (comment)
  "Return current Confluence version number for COMMENT, or nil."
  (let ((number (or (alist-get 'number (alist-get 'version comment))
		    (alist-get 'versionNumber comment)
		    (alist-get 'version-number comment))))
    (cond
     ((numberp number) number)
     ((stringp number) (string-to-number number)))))

(defun hub/confluence-commands--remote-comment-updated-at (comment)
  "Return remote updated timestamp for Confluence COMMENT, or nil."
  (or (alist-get 'createdAt (alist-get 'version comment))
      (alist-get 'created-at (alist-get 'version comment))
      (alist-get 'updatedAt comment)
      (alist-get 'updated-at comment)))

(defun hub/confluence-commands--current-user-account-id ()
  "Return authenticated Confluence user's account ID."
  (let* ((response (hub/confluence-api--current-user))
	 (user (hub/confluence-commands--json-alist
		(hub/confluence-commands--response-body response)))
	 (account-id (or (alist-get 'accountId user)
			 (alist-get 'account-id user))))
    (unless account-id
      (user-error "Confluence current user response did not include accountId"))
    account-id))

(defun hub/confluence-commands--remote-comment-resolution-status (comment)
  "Return normalized remote resolution status for Confluence COMMENT, or nil."
  (let* ((properties (alist-get 'properties comment))
	 (raw (or (alist-get 'resolutionStatus comment)
		  (alist-get 'resolution-status comment)
		  (alist-get 'resolved comment)
		  (and (listp properties)
		       (or (alist-get 'resolutionStatus properties)
			   (alist-get 'resolution-status properties)
			   (alist-get 'resolved properties))))))
    (cond
     ((eq raw t) "resolved")
     ((null raw) nil)
     ((stringp raw)
      (let ((value (downcase (string-trim raw))))
	(cond
	 ((member value '("resolved" "closed" "done" "true")) "resolved")
	 ((member value '("dangling")) "dangling")
	 ((member value '("open" "unresolved" "reopened" "active" "false")) "open"))))
     (t nil))))

(defun hub/confluence-commands--remote-inline-target-object (comment)
  "Return best available raw inline target object from COMMENT, or nil."
  (or (alist-get 'inlineCommentProperties comment)
      (alist-get 'inline-comment-properties comment)
      (alist-get 'target comment)
      (alist-get 'properties comment)))

(defun hub/confluence-commands--remote-inline-target-text (comment)
  "Return best available selected target text from inline COMMENT, or nil."
  (let ((target (hub/confluence-commands--remote-inline-target-object comment)))
    (or (alist-get 'originalSelection comment)
	(alist-get 'original-selection comment)
	(alist-get 'inlineOriginalSelection comment)
	(alist-get 'inline-original-selection comment)
	(alist-get 'selectedText comment)
	(alist-get 'selected-text comment)
	(alist-get 'text comment)
	(and (listp target)
	     (or (alist-get 'originalSelection target)
		 (alist-get 'original-selection target)
		 (alist-get 'inlineOriginalSelection target)
		 (alist-get 'inline-original-selection target)
		 (alist-get 'selectedText target)
		 (alist-get 'selected-text target)
		 (alist-get 'text target))))))

(defun hub/confluence-commands--remote-inline-marker-ref (comment)
  "Return Confluence inline marker reference for COMMENT, or nil."
  (let ((target (hub/confluence-commands--remote-inline-target-object comment)))
    (or (alist-get 'inlineMarkerRef comment)
	(alist-get 'inline-marker-ref comment)
	(alist-get 'markerRef comment)
	(alist-get 'marker-ref comment)
	(and (listp target)
	     (or (alist-get 'inlineMarkerRef target)
		 (alist-get 'inline-marker-ref target)
		 (alist-get 'markerRef target)
		 (alist-get 'marker-ref target))))))

(defun hub/confluence-commands--html-escape-text (text)
  "Escape TEXT for Confluence storage text-node matching."
  (replace-regexp-in-string
   ">" "&gt;"
   (replace-regexp-in-string
    "<" "&lt;"
    (replace-regexp-in-string "&" "&amp;" (or text "") t t)
    t t)
   t t))

(defun hub/confluence-commands--context-before (string position)
  "Return storage context before POSITION in STRING."
  (let ((start (max 0 (- position hub/confluence-commands--inline-comment-context-window))))
    (substring string start position)))

(defun hub/confluence-commands--context-after (string position)
  "Return storage context after POSITION in STRING."
  (substring string position
	     (min (length string)
		  (+ position hub/confluence-commands--inline-comment-context-window))))

(defun hub/confluence-commands--common-prefix-length (a b)
  "Return length of common prefix between A and B."
  (let ((limit (min (length a) (length b)))
	(index 0))
    (while (and (< index limit)
		(= (aref a index) (aref b index)))
      (setq index (1+ index)))
    index))

(defun hub/confluence-commands--common-suffix-length (a b)
  "Return length of common suffix between A and B."
  (let ((limit (min (length a) (length b)))
	(index 0))
    (while (and (< index limit)
		(= (aref a (- (length a) index 1))
		   (aref b (- (length b) index 1))))
      (setq index (1+ index)))
    index))

(defun hub/confluence-commands--inline-marker-ranges (storage)
  "Return inline comment marker ranges parsed from STORAGE."
  (let ((stack nil)
	(markers nil))
    (with-temp-buffer
      (insert storage)
      (goto-char (point-min))
      (while (re-search-forward "<\\(/?\\)ac:inline-comment-marker\\b\\([^>]*\\)>" nil t)
	(let* ((tag-start (match-beginning 0))
	       (tag-end (match-end 0))
	       (closing (equal (match-string 1) "/"))
	       (attrs (match-string 2))
	       (ref (save-match-data
		      (and (string-match "ac:ref=\"\\([^\"]+\\)\"" attrs)
			   (match-string 1 attrs))))
	       (self-closing (and (not closing)
				  (string-match-p "/[[:space:]]*\\='" attrs))))
	  (cond
	   (self-closing
	    (push (list :ref ref
			:start tag-start
			:start-tag-end tag-end
			:body-start tag-end
			:body-end tag-end
			:end tag-end
			:self-closing t)
		  markers))
	   (closing
	    (when-let* ((opening (pop stack)))
	      (push (append opening
			    (list :body-end tag-start
				  :end tag-end))
		    markers)))
	   (t
	    (push (list :ref ref
			:start tag-start
			:start-tag-end tag-end
			:body-start tag-end
			:self-closing nil)
		  stack))))))
    (sort markers (lambda (a b) (< (plist-get a :start) (plist-get b :start))))))

(defun hub/confluence-commands--complex-inline-marker-ref-reasons (storage)
  "Return an alist of complex inline marker refs to reasons in STORAGE."
  (let ((markers (hub/confluence-commands--inline-marker-ranges storage))
	(counts nil)
	reasons)
    (dolist (marker markers)
      (when-let* ((ref (plist-get marker :ref)))
	(push ref (alist-get ref counts nil nil #'equal))))
    (dolist (entry counts)
      (when (> (length (cdr entry)) 1)
	(push (cons (car entry) "split inline marker topology") reasons)))
    (dolist (outer markers)
      (dolist (inner markers)
	(when (and (not (eq outer inner))
		   (plist-get outer :ref)
		   (plist-get inner :ref)
		   (not (equal (plist-get outer :ref) (plist-get inner :ref)))
		   (< (plist-get outer :start) (plist-get inner :start))
		   (< (plist-get inner :end) (plist-get outer :end)))
	  (unless (assoc-string (plist-get outer :ref) reasons t)
	    (push (cons (plist-get outer :ref) "nested inline marker topology") reasons))
	  (unless (assoc-string (plist-get inner :ref) reasons t)
	    (push (cons (plist-get inner :ref) "nested inline marker topology") reasons)))))
    reasons))

(defun hub/confluence-commands--inline-marker-contexts (old-storage)
  "Return an alist of marker ref to context plist from OLD-STORAGE."
  (let (contexts)
    (with-temp-buffer
      (insert old-storage)
      (goto-char (point-min))
      (while (re-search-forward hub/confluence-commands--inline-comment-marker-regexp nil t)
	(let ((ref (match-string-no-properties 1))
	      (start (match-beginning 0))
	      (end (match-end 0)))
	  (push (cons ref (list :before (string-remove-suffix
					 "<" (hub/confluence-commands--context-before old-storage start))
				:after (hub/confluence-commands--context-after old-storage end)))
		contexts))))
    contexts))

(defun hub/confluence-commands--inline-comment-exact-candidates (storage selection)
  "Return exact candidate bounds in STORAGE for COMMENT SELECTION."
  (let* ((escaped (hub/confluence-commands--html-escape-text selection))
	 (forms (delete-dups (delq nil (list escaped selection))))
	 candidates)
    (dolist (form forms)
      (when (and (stringp form) (not (string-empty-p form)))
	(let ((start 0))
	  (while (string-match (regexp-quote form) storage start)
	    (push (list :start (match-beginning 0)
			:end (match-end 0))
		  candidates)
	    (setq start (1+ (match-beginning 0)))))))
    (nreverse candidates)))

(defun hub/confluence-commands--decode-storage-entity (entity)
  "Return visible text for storage ENTITY, or nil when unknown."
  (cond
   ((equal entity "amp") "&")
   ((equal entity "lt") "<")
   ((equal entity "gt") ">")
   ((equal entity "quot") "\"")
   ((equal entity "apos") "'")
   ((equal entity "nbsp") " ")
   ((string-match-p "\\`#x[[:xdigit:]]+\\'" entity)
    (char-to-string (string-to-number (substring entity 2) 16)))
   ((string-match-p "\\`#[0-9]+\\'" entity)
    (char-to-string (string-to-number (substring entity 1) 10)))))

(defun hub/confluence-commands--push-visible-storage-span (text start end text-parts starts ends)
  "Push TEXT spanning START to END into TEXT-PARTS, STARTS, and ENDS.
Return a list `(TEXT-PARTS STARTS ENDS)' with one storage span per visible
character."
  (dolist (char (string-to-list text))
    (push (char-to-string char) text-parts)
    (push start starts)
    (push end ends))
  (list text-parts starts ends))

(defun hub/confluence-commands--storage-text-index (storage)
  "Return a plist mapping visible text positions to STORAGE positions.
Tags are skipped, so text split by inline storage tags can still be matched as
one contiguous visible selection.  Common XML/HTML entities are decoded while
retaining their original storage span."
  (let ((index 0)
	(text-parts nil)
	(starts nil)
	(ends nil))
    (while (< index (length storage))
      (cond
       ((= (aref storage index) ?<)
	(let ((tag-end (string-match ">" storage index)))
	  (setq index (if tag-end (1+ tag-end) (length storage)))))
       ((= (aref storage index) ?&)
	(let* ((entity-end (string-match ";" storage index))
	       (entity (and entity-end (substring storage (1+ index) entity-end)))
	       (decoded (and entity (hub/confluence-commands--decode-storage-entity entity))))
	  (if decoded
	      (pcase-let ((`(,new-parts ,new-starts ,new-ends)
			   (hub/confluence-commands--push-visible-storage-span
			    decoded index (1+ entity-end) text-parts starts ends)))
		(setq text-parts new-parts
		      starts new-starts
		      ends new-ends
		      index (1+ entity-end)))
	    (let ((start index))
	      (push (char-to-string (aref storage index)) text-parts)
	      (push start starts)
	      (setq index (1+ index))
	      (push index ends)))))
       (t
	(let ((start index))
	  (push (char-to-string (aref storage index)) text-parts)
	  (push start starts)
	  (setq index (1+ index))
	  (push index ends)))))
    (list :text (apply #'concat (nreverse text-parts))
	  :starts (vconcat (nreverse starts))
	  :ends (vconcat (nreverse ends)))))

(defun hub/confluence-commands--inline-comment-visible-candidates (storage selection)
  "Return tag-insensitive candidate bounds in STORAGE for COMMENT SELECTION."
  (let* ((index (hub/confluence-commands--storage-text-index storage))
	 (text (plist-get index :text))
	 (starts (plist-get index :starts))
	 (ends (plist-get index :ends))
	 candidates)
    (when (and (hub/confluence-api--present-string-p selection)
	       (> (length text) 0))
      (let ((start 0))
	(while (string-match (regexp-quote selection) text start)
	  (let* ((text-start (match-beginning 0))
		 (text-end (match-end 0))
		 (storage-start (aref starts text-start))
		 (storage-end (aref ends (1- text-end))))
	    (push (list :start storage-start
			:end storage-end)
		  candidates))
	  (setq start (1+ (match-beginning 0))))))
    (nreverse candidates)))

(defun hub/confluence-commands--dedupe-inline-comment-candidates (candidates)
  "Return CANDIDATES without duplicate start/end bounds."
  (let (seen result)
    (dolist (candidate candidates (nreverse result))
      (let ((key (cons (plist-get candidate :start) (plist-get candidate :end))))
	(unless (member key seen)
	  (push key seen)
	  (push candidate result))))))

(defun hub/confluence-commands--storage-tag-ranges (storage tag)
  "Return ranges for matching XML TAG in STORAGE."
  (let (ranges)
    (with-temp-buffer
      (insert storage)
      (goto-char (point-min))
      (while (re-search-forward
	      (format "<%s\\(?:[[:space:]][^>]*\\)?>\\(?:.\\|\n\\)*?</%s>"
		      (regexp-quote tag) (regexp-quote tag))
	      nil t)
	(push (cons (match-beginning 0) (match-end 0)) ranges)))
    (nreverse ranges)))

(defun hub/confluence-commands--storage-cdata-ranges (storage)
  "Return CDATA ranges in STORAGE."
  (let ((start 0)
	ranges)
    (while (string-match "<!\\[CDATA\\[" storage start)
      (let* ((range-start (match-beginning 0))
	     (body-start (match-end 0))
	     (range-end (if (string-match "\\]\\]>" storage body-start)
			    (match-end 0)
			  (length storage))))
	(push (cons range-start range-end) ranges)
	(setq start range-end)))
    (nreverse ranges)))

(defun hub/confluence-commands--storage-ri-tag-ranges (storage)
  "Return resource identifier tag ranges in STORAGE."
  (let (ranges)
    (with-temp-buffer
      (insert storage)
      (goto-char (point-min))
      (while (re-search-forward "<ri:[^>]+/?>" nil t)
	(push (cons (match-beginning 0) (match-end 0)) ranges)))
    (nreverse ranges)))

(defun hub/confluence-commands--range-overlaps-p (start end range)
  "Return non-nil when START..END overlaps RANGE."
  (and (< start (cdr range))
       (> end (car range))))

(defun hub/confluence-commands--range-contained-in-ranges-p (start end ranges)
  "Return non-nil when START..END is contained in one of RANGES."
  (seq-some (lambda (range)
	      (and (<= (car range) start)
		   (<= end (cdr range))))
	    ranges))

(defun hub/confluence-commands--storage-text-container-ranges (storage)
  "Return safe text container ranges in STORAGE."
  (let (ranges)
    (dolist (tag '("p" "li" "td" "th" "h1" "h2" "h3" "h4" "h5" "h6"
		   "ac:link-body" "ac:rich-text-body"))
      (setq ranges (append ranges
			   (hub/confluence-commands--storage-tag-ranges storage tag))))
    ranges))

(defun hub/confluence-commands--candidate-crosses-text-container-p (storage start end)
  "Return non-nil when START..END crosses text container boundaries in STORAGE."
  (let* ((ranges (hub/confluence-commands--storage-text-container-ranges storage))
	 (overlapping (seq-filter (lambda (range)
				    (hub/confluence-commands--range-overlaps-p start end range))
				  ranges)))
    (and overlapping
	 (not (hub/confluence-commands--range-contained-in-ranges-p
	       start end overlapping)))))

(defun hub/confluence-commands--unsafe-inline-comment-candidate-p (storage candidate)
  "Return non-nil when CANDIDATE is in unsafe Confluence storage."
  (let* ((start (plist-get candidate :start))
	 (end (plist-get candidate :end))
	 (blocked-ranges (append
			  (hub/confluence-commands--storage-tag-ranges storage "ac:parameter")
			  (hub/confluence-commands--storage-tag-ranges storage "ac:plain-text-body")
			  (hub/confluence-commands--storage-cdata-ranges storage)
			  (hub/confluence-commands--storage-ri-tag-ranges storage)))
	 (macro-ranges (hub/confluence-commands--storage-tag-ranges storage "ac:structured-macro"))
	 (rich-text-ranges (hub/confluence-commands--storage-tag-ranges storage "ac:rich-text-body")))
    (or (hub/confluence-commands--candidate-crosses-text-container-p storage start end)
	(seq-some (lambda (range)
		    (hub/confluence-commands--range-overlaps-p start end range))
		  blocked-ranges)
	(and (hub/confluence-commands--range-contained-in-ranges-p start end macro-ranges)
	     (not (hub/confluence-commands--range-contained-in-ranges-p
		   start end rich-text-ranges))))))

(defun hub/confluence-commands--safe-inline-comment-candidates (storage candidates)
  "Return CANDIDATES that are safe marker insertion ranges in STORAGE."
  (seq-remove (lambda (candidate)
		(hub/confluence-commands--unsafe-inline-comment-candidate-p storage candidate))
	      candidates))

(defun hub/confluence-commands--inline-comment-candidates (storage selection)
  "Return candidate bounds in STORAGE for COMMENT SELECTION."
  (hub/confluence-commands--safe-inline-comment-candidates
   storage
   (hub/confluence-commands--dedupe-inline-comment-candidates
    (append (hub/confluence-commands--inline-comment-exact-candidates storage selection)
	    (hub/confluence-commands--inline-comment-visible-candidates storage selection)))))

(defun hub/confluence-commands--score-inline-comment-candidate (storage candidate context)
  "Return context score for CANDIDATE in STORAGE using old marker CONTEXT."
  (let ((before (plist-get context :before))
	(after (plist-get context :after)))
    (+ (if before
	   (hub/confluence-commands--common-suffix-length
	    before (hub/confluence-commands--context-before storage (plist-get candidate :start)))
	 0)
       (if after
	   (hub/confluence-commands--common-prefix-length
	    after (hub/confluence-commands--context-after storage (plist-get candidate :end)))
	 0))))

(defun hub/confluence-commands--best-inline-comment-candidate (storage selection context)
  "Return best STORAGE candidate for SELECTION using optional CONTEXT."
  (let ((best nil)
	(best-score -1))
    (dolist (candidate (hub/confluence-commands--inline-comment-candidates storage selection))
      (let ((score (if context
		       (hub/confluence-commands--score-inline-comment-candidate storage candidate context)
		     0)))
	(when (> score best-score)
	  (setq best candidate
		best-score score))))
    best))

(defun hub/confluence-commands--inline-comment-replacements (new-storage old-storage comments)
  "Return marker replacements for COMMENTS from OLD-STORAGE into NEW-STORAGE."
  (let ((contexts (hub/confluence-commands--inline-marker-contexts old-storage))
	(seen nil)
	replacements)
    (dolist (comment comments)
      (let ((status (hub/confluence-commands--remote-comment-resolution-status comment))
	    (ref (hub/confluence-commands--remote-inline-marker-ref comment))
	    (selection (hub/confluence-commands--remote-inline-target-text comment)))
	(when (and (not (member status '("resolved" "dangling")))
		   (hub/confluence-api--present-string-p ref)
		   (hub/confluence-api--present-string-p selection)
		   (not (member ref seen)))
	  (push ref seen)
	  (when-let* ((candidate (hub/confluence-commands--best-inline-comment-candidate
				  new-storage selection (cdr (assoc-string ref contexts t)))))
	    (push (append candidate (list :ref ref)) replacements)))))
    (sort replacements (lambda (a b) (> (plist-get a :start) (plist-get b :start))))))

(defun hub/confluence-commands--insert-inline-comment-markers (new-storage old-storage comments)
  "Return NEW-STORAGE with Confluence inline markers restored from COMMENTS.
OLD-STORAGE supplies surrounding context for duplicate target disambiguation."
  (let ((min-applied-start (length new-storage)))
    (dolist (replacement (hub/confluence-commands--inline-comment-replacements
			  new-storage old-storage comments))
      (let ((start (plist-get replacement :start))
	    (end (plist-get replacement :end))
	    (ref (plist-get replacement :ref)))
	(unless (> end min-applied-start)
	  (setq min-applied-start start)
	  (setq new-storage
		(concat (substring new-storage 0 start)
			(format "<ac:inline-comment-marker ac:ref=\"%s\">%s</ac:inline-comment-marker>"
				(xml-escape-string ref)
				(substring new-storage start end))
			(substring new-storage end))))))
    new-storage))

(defun hub/confluence-commands--preserve-inline-comments (page-id new-storage)
  "Return NEW-STORAGE with active inline comment markers preserved for PAGE-ID."
  (if (or (not hub/confluence-publish-preserve-inline-comments)
	  (not (member (format "%s" page-id)
		       hub/confluence-publish--pages-needing-inline-marker-preservation)))
      new-storage
    (let* ((page-response (hub/confluence-api--get-page page-id "storage"))
	   (old-storage (hub/confluence-commands--page-body-storage-value page-response))
	   (comments-response (hub/confluence-api--list-page-comments
			       page-id "inline-comments" "storage"))
	   (comments (hub/confluence-commands--comment-results comments-response)))
      (hub/confluence-commands--insert-inline-comment-markers
       new-storage old-storage comments))))

(defun hub/confluence-repair--source-occurrence-index (source-file target-text target-start)
  "Return zero-based occurrence index of TARGET-TEXT before TARGET-START in SOURCE-FILE."
  (when (and source-file
	     (file-readable-p source-file)
	     (hub/confluence-api--present-string-p target-text)
	     (integerp target-start))
    (with-temp-buffer
      (insert-file-contents source-file)
      (let ((count 0)
	    (start 0)
	    found)
	(while (and (not found)
		    (string-match (regexp-quote target-text) (buffer-string) start))
	  (if (= (1+ (match-beginning 0)) target-start)
	      (setq found count)
	    (setq count (1+ count)
		  start (1+ (match-beginning 0)))))
	found))))

(defun hub/confluence-repair--sidecar-target-info (remote-id source-file)
  "Return sidecar target info for REMOTE-ID in SOURCE-FILE, or nil."
  (when-let* ((sidecar-file (and source-file (hub/org-comment-sidecar-path source-file))))
    (when (and remote-id (file-readable-p sidecar-file))
      (with-temp-buffer
	(insert-file-contents sidecar-file)
	(org-mode)
	(goto-char (point-min))
	(cl-loop while (re-search-forward org-heading-regexp nil t)
		 do (goto-char (match-beginning 0))
		 when (equal remote-id (org-entry-get nil "HUB_COMMENT_REMOTE_ID"))
		 return (let* ((target-text (org-entry-get nil "HUB_COMMENT_TARGET_TEXT"))
			       (target (org-entry-get nil "HUB_COMMENT_TARGET"))
			       (target-start (and target
						  (string-match "\\`[[:space:]]*\\([0-9]+\\)" target)
						  (string-to-number (match-string 1 target)))))
			  (list :target-text target-text
				:target-start target-start
				:occurrence-index
				(hub/confluence-repair--source-occurrence-index
				 source-file target-text target-start)))
		 do (forward-line 1))))))

(defun hub/confluence-repair--comment-marker-present-p (storage comment)
  "Return non-nil when COMMENT marker ref is present in STORAGE."
  (when-let* ((ref (hub/confluence-commands--remote-inline-marker-ref comment)))
    (string-match-p (regexp-quote ref) storage)))

(defun hub/confluence-repair--candidate-for-comment (storage comment &optional source-file)
  "Return repair candidate plist for COMMENT in STORAGE using SOURCE-FILE hints."
  (let* ((remote-id (hub/confluence-commands--remote-comment-id comment))
	 (ref (hub/confluence-commands--remote-inline-marker-ref comment))
	 (selection (hub/confluence-commands--remote-inline-target-text comment))
	 (candidates (and selection
			  (hub/confluence-commands--inline-comment-candidates storage selection)))
	 (sidecar-info (hub/confluence-repair--sidecar-target-info remote-id source-file))
	 (occurrence-index (plist-get sidecar-info :occurrence-index))
	 chosen reason)
    (cond
     ((not (hub/confluence-api--present-string-p ref))
      (setq reason "missing marker ref"))
     ((not (hub/confluence-api--present-string-p selection))
      (setq reason "missing original selection"))
     ((hub/confluence-repair--comment-marker-present-p storage comment)
      (setq reason "marker already present"))
     ((null candidates)
      (setq reason "selection not found"))
     ((= (length candidates) 1)
      (setq chosen (car candidates)))
     ((and (integerp occurrence-index)
	   (< occurrence-index (length candidates)))
      (setq chosen (nth occurrence-index candidates)))
     (t
      (setq reason (format "ambiguous selection (%s candidates)" (length candidates)))))
    (append (list :comment-id remote-id
		  :ref ref
		  :selection selection
		  :candidate-count (length candidates)
		  :reason reason)
	    chosen)))

(defun hub/confluence-repair--apply-candidates (storage candidates)
  "Return STORAGE with repair CANDIDATES inserted as inline comment markers."
  (dolist (candidate (sort (seq-copy candidates)
			   (lambda (a b) (> (plist-get a :start) (plist-get b :start)))))
    (let* ((start (plist-get candidate :start))
	   (end (plist-get candidate :end))
	   (ref (plist-get candidate :ref))
	   (selection (substring storage start end)))
      (setq storage
	    (concat (substring storage 0 start)
		    (format "<ac:inline-comment-marker ac:ref=\"%s\">%s</ac:inline-comment-marker>"
			    (xml-escape-string ref)
			    selection)
		    (substring storage end)))))
  storage)

(defun hub/confluence-repair--report-buffer (page-id repaired skipped backup-file repair-file)
  "Show repair report for PAGE-ID with REPAIRED and SKIPPED records."
  (let ((buffer (get-buffer-create "*Confluence Inline Comment Repair*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
	(erase-buffer)
	(insert (format "Confluence inline comment anchor repair for page %s\n\n" page-id))
	(insert (format "Backup: %s\nRepair: %s\n\n" backup-file repair-file))
	(insert (format "* Repairable (%s)\n" (length repaired)))
	(if repaired
	    (dolist (record repaired)
	      (insert (format "- comment %s ref %s selection %S candidates %s\n"
			      (plist-get record :comment-id)
			      (plist-get record :ref)
			      (plist-get record :selection)
			      (plist-get record :candidate-count))))
	  (insert "None.\n"))
	(insert (format "\n* Skipped (%s)\n" (length skipped)))
	(if skipped
	    (dolist (record skipped)
	      (insert (format "- comment %s: %s; selection %S; candidates %s\n"
			      (plist-get record :comment-id)
			      (plist-get record :reason)
			      (plist-get record :selection)
			      (plist-get record :candidate-count))))
	  (insert "None.\n"))
	(goto-char (point-min))
	(org-mode)
	(setq buffer-read-only t)))
    (display-buffer buffer)
    buffer))

;;;###autoload
(defun hub/confluence-repair-inline-comment-anchors (&optional page-id apply)
  "Repair missing Confluence inline comment markers for PAGE-ID.

The command fetches current page storage and inline comment metadata, reinserts
missing `ac:inline-comment-marker' tags when a safe target can be identified,
writes before/after XHTML files under `temporary-file-directory', and applies
the repaired storage when APPLY is non-nil or the user confirms interactively."
  (interactive (list nil current-prefix-arg))
  (let* ((id (hub/confluence-commands--page-id-or-read page-id))
	 (source-file buffer-file-name)
	 (page-response (hub/confluence-api--get-page id "storage"))
	 (storage (hub/confluence-commands--page-body-storage-value page-response))
	 (comments-response (hub/confluence-api--list-page-comments id "inline-comments" "storage"))
	 (comments (hub/confluence-commands--comment-results comments-response))
	 repaired skipped)
    (dolist (comment comments)
      (let ((candidate (hub/confluence-repair--candidate-for-comment storage comment source-file)))
	(if (and (plist-member candidate :start)
		 (not (plist-get candidate :reason)))
	    (push candidate repaired)
	  (push candidate skipped))))
    (setq repaired (nreverse repaired)
	  skipped (nreverse skipped))
    (let* ((backup-file (make-temp-file "confluence-before-anchor-repair-" nil ".xhtml"))
	   (repair-file (make-temp-file "confluence-after-anchor-repair-" nil ".xhtml"))
	   (repaired-storage (hub/confluence-repair--apply-candidates storage repaired)))
      (with-temp-file backup-file (insert storage))
      (with-temp-file repair-file (insert repaired-storage))
      (hub/confluence-repair--report-buffer id repaired skipped backup-file repair-file)
      (when (and repaired
		 (or apply
		     (yes-or-no-p (format "Apply %s inline comment anchor repair(s) to page %s? "
					  (length repaired) id))))
	(hub/confluence-commands--run
	 (hub/confluence-api--page-update-command id repair-file))
	(message "Repaired %s Confluence inline comment anchor(s) on page %s"
		 (length repaired) id))
      (list :repaired repaired
	    :skipped skipped
	    :backup-file backup-file
	    :repair-file repair-file))))

(defun hub/confluence-commands--property-line (key value)
  "Return an Org property line for KEY and VALUE when VALUE is present."
  (when (and (stringp value) (not (string-empty-p (string-trim value))))
    (format ":%s: %s\n" key (string-trim value))))

(defun hub/confluence-commands--people-cache-file (directory)
  "Return people cache file for DIRECTORY, preferring an existing local file."
  (let ((local (and directory (hub/confluence-people-local-file directory))))
    (if (and local (file-exists-p local))
	local
      (hub/confluence-people-global-file))))

(defun hub/confluence-commands--cache-comment-author (comment &optional directory)
  "Cache Confluence author identity from COMMENT when author data is present."
  (let ((author-id (hub/confluence-commands--remote-comment-author-id comment))
	(display-name (hub/confluence-commands--remote-comment-author-display-name comment)))
    (when author-id
      (hub/confluence-people-cache-identity
       author-id display-name nil (hub/confluence-commands--people-cache-file directory)))))

(defun hub/confluence-commands--remote-comment-parent-id (comment)
  "Return remote parent comment ID for COMMENT, or nil."
  (when-let* ((id (or (alist-get 'parentCommentId comment)
		      (alist-get 'parent-comment-id comment))))
    (format "%s" id)))

(defun hub/confluence-commands--sidecar-has-remote-comment-p (sidecar-file remote-id)
  "Return non-nil when SIDECAR-FILE already has Confluence REMOTE-ID."
  (when (file-exists-p sidecar-file)
    (with-temp-buffer
      (insert-file-contents sidecar-file)
      (org-mode)
      (goto-char (point-min))
      (cl-loop while (re-search-forward org-heading-regexp nil t)
	       do (goto-char (match-beginning 0))
	       when (equal remote-id (org-entry-get nil "HUB_COMMENT_REMOTE_ID"))
	       return t
	       do (forward-line 1)))))

(defun hub/confluence-commands--put-property-when-missing (property value)
  "Set Org PROPERTY to VALUE at point only when missing."
  (when (and (stringp value)
	     (not (string-empty-p (string-trim value)))
	     (not (org-entry-get nil property)))
    (org-entry-put nil property (string-trim value))))

(defun hub/confluence-commands--backfill-remote-footer-metadata (sidecar-file comment)
  "Backfill missing sidecar metadata for existing remote footer COMMENT."
  (let ((remote-id (hub/confluence-commands--remote-comment-id comment)))
    (when (and remote-id (file-exists-p sidecar-file))
      (with-temp-buffer
	(insert-file-contents sidecar-file)
	(org-mode)
	(goto-char (point-min))
	(when (cl-loop while (re-search-forward org-heading-regexp nil t)
		       do (goto-char (match-beginning 0))
		       when (and (equal "confluence" (org-entry-get nil "HUB_COMMENT_SOURCE"))
				 (equal remote-id (org-entry-get nil "HUB_COMMENT_REMOTE_ID")))
		       return t
		       do (forward-line 1))
	  (let ((author-name (hub/confluence-commands--remote-comment-author-name comment))
		(author-id (hub/confluence-commands--remote-comment-author-id comment))
		(created-at (hub/confluence-commands--remote-comment-created-at comment)))
	    (hub/confluence-commands--put-property-when-missing "HUB_COMMENT_REMOTE_AUTHOR_ID" author-id)
	    (hub/confluence-commands--put-property-when-missing
	     "HUB_COMMENT_REMOTE_AUTHOR_DISPLAY_NAME" author-name)
	    (hub/confluence-commands--put-property-when-missing
	     "HUB_COMMENT_REMOTE_CREATED_AT" created-at)
	    (write-region (point-min) (point-max) sidecar-file nil 'silent)))))))

(defun hub/confluence-commands--sync-timestamp ()
  "Return timestamp used for Confluence comment sync metadata."
  (format-time-string "%Y-%m-%dT%H:%M:%S%z"))

(defun hub/confluence-commands--remote-comment-common-properties (comment body-format sync-kind)
  "Return common sidecar property text for COMMENT, BODY-FORMAT, and SYNC-KIND."
  (let* ((remote-id (hub/confluence-commands--remote-comment-id comment))
	 (local-id (format "remote-confluence-%s" remote-id))
	 (author-name (hub/confluence-commands--remote-comment-author-name comment))
	 (author-id (hub/confluence-commands--remote-comment-author-id comment))
	 (resolution-status (hub/confluence-commands--remote-comment-resolution-status comment))
	 (created-at (hub/confluence-commands--remote-comment-created-at comment)))
    (concat
     (format ":HUB_COMMENT_ID: %s\n" local-id)
     (format ":HUB_COMMENT_REMOTE_ID: %s\n" remote-id)
     ":HUB_COMMENT_SOURCE: confluence\n"
     (format ":HUB_COMMENT_SYNC_KIND: %s\n" sync-kind)
     (unless (equal body-format "storage")
       (format ":HUB_COMMENT_BODY_FORMAT: %s\n" body-format))
     (hub/confluence-commands--property-line "HUB_COMMENT_REMOTE_LAST_SEEN_AT"
					     (hub/confluence-commands--sync-timestamp))
     (hub/confluence-commands--property-line "HUB_COMMENT_REMOTE_RESOLUTION_STATUS" resolution-status)
     (hub/confluence-commands--property-line "HUB_COMMENT_REMOTE_AUTHOR_ID" author-id)
     (hub/confluence-commands--property-line "HUB_COMMENT_REMOTE_AUTHOR_DISPLAY_NAME" author-name)
     (hub/confluence-commands--property-line "HUB_COMMENT_REMOTE_CREATED_AT" created-at))))

(defun hub/confluence-commands--remote-title-record (comment body-format sync-kind)
  "Return title record for remote COMMENT with BODY-FORMAT and SYNC-KIND."
  (list :author (hub/confluence-commands--remote-comment-author-name comment)
	:remote-author-id (hub/confluence-commands--remote-comment-author-id comment)
	:remote-author-display-name (hub/confluence-commands--remote-comment-author-name comment)
	:created-at (hub/confluence-commands--remote-comment-created-at comment)
	:sync-kind sync-kind
	:body-format body-format
	:target-text (hub/confluence-commands--remote-inline-target-text comment)
	:body (hub/confluence-commands--remote-comment-body comment)))

(defun hub/confluence-commands--remote-reply-entry (comment body-format parent-remote-id &optional directory)
  "Return sidecar Org child entry text for reply COMMENT."
  (let ((parent-id (or parent-remote-id
		       (hub/confluence-commands--remote-comment-parent-id comment))))
    (concat
     (format "** %s\n"
	     (hub/org-comment-reply-heading-title
	      (hub/confluence-commands--remote-title-record comment body-format "reply")
	      directory))
     ":PROPERTIES:\n"
     (hub/confluence-commands--remote-comment-common-properties comment body-format "reply")
     (hub/confluence-commands--property-line "HUB_COMMENT_REMOTE_PARENT_ID" parent-id)
     ":END:\n\n"
     (string-trim-right (hub/confluence-commands--remote-comment-body comment))
     "\n")))

(defun hub/confluence-commands--remote-orphan-thread-entry (parent-remote-id &optional _directory)
  "Return placeholder sidecar root entry for missing PARENT-REMOTE-ID."
  (concat
   (format "* OPEN Remote conversation %s - Missing parent comment\n" parent-remote-id)
   ":PROPERTIES:\n"
   (format ":HUB_COMMENT_ID: remote-confluence-%s\n" parent-remote-id)
   (format ":HUB_COMMENT_REMOTE_ID: %s\n" parent-remote-id)
   ":HUB_COMMENT_SOURCE: confluence\n"
   ":HUB_COMMENT_SYNC_KIND: orphan-thread\n"
   ":HUB_COMMENT_ANCHOR_STATE: missing-parent\n"
   ":END:\n\n"
   "Parent comment was not returned by Confluence import.\n"))

(defun hub/confluence-commands--initial-local-status (comment)
  "Return initial local TODO status for remote COMMENT."
  (if (equal "resolved" (hub/confluence-commands--remote-comment-resolution-status comment))
      "RESOLVED"
    "OPEN"))

(defun hub/confluence-commands--remote-footer-entry (comment body-format &optional directory)
  "Return sidecar Org entry text for remote footer COMMENT with BODY-FORMAT."
  (let ((remote-id (hub/confluence-commands--remote-comment-id comment)))
    (concat
     (format "* %s %s\n" (hub/confluence-commands--initial-local-status comment)
	     (hub/org-comment-heading-title
	      (hub/confluence-commands--remote-title-record comment body-format "footer")
	      directory))
     ":PROPERTIES:\n"
     (hub/confluence-commands--remote-comment-common-properties comment body-format "footer")
     ":END:\n\n"
     (string-trim-right (hub/confluence-commands--remote-comment-body comment))
     "\n")))

(defun hub/confluence-commands--remote-inline-entry (comment body-format &optional directory)
  "Return sidecar Org entry text for remote inline COMMENT with BODY-FORMAT."
  (let* ((remote-id (hub/confluence-commands--remote-comment-id comment))
	 (target-text (hub/confluence-commands--remote-inline-target-text comment)))
    (concat
     (format "* %s %s\n" (hub/confluence-commands--initial-local-status comment)
	     (hub/org-comment-heading-title
	      (hub/confluence-commands--remote-title-record comment body-format "inline")
	      directory))
     ":PROPERTIES:\n"
     (hub/confluence-commands--remote-comment-common-properties comment body-format "inline")
     (hub/confluence-commands--property-line "HUB_COMMENT_TARGET_TEXT" target-text)
     ":END:\n\n"
     (string-trim-right (hub/confluence-commands--remote-comment-body comment))
     "\n")))

(defun hub/confluence-commands--append-remote-comment-entry (sidecar-file source-file entry)
  "Append remote comment ENTRY to SIDECAR-FILE for SOURCE-FILE."
  (hub/org-comment--ensure-sidecar-header sidecar-file source-file)
  (with-temp-buffer
    (insert-file-contents sidecar-file)
    (goto-char (point-max))
    (unless (bolp) (insert "\n"))
    (unless (save-excursion
	      (forward-line -1)
	      (looking-at-p "[[:space:]]*$"))
      (insert "\n"))
    (insert entry)
    (write-region (point-min) (point-max) sidecar-file nil 'silent)))

(defun hub/confluence-commands--append-remote-footer-comment (sidecar-file source-file comment body-format)
  "Append remote footer COMMENT to SIDECAR-FILE for SOURCE-FILE."
  (hub/confluence-commands--append-remote-comment-entry
   sidecar-file source-file
   (hub/confluence-commands--remote-footer-entry
    comment body-format (file-name-directory sidecar-file))))

(defun hub/confluence-commands--append-remote-inline-comment (sidecar-file source-file comment body-format)
  "Append remote inline COMMENT to SIDECAR-FILE for SOURCE-FILE."
  (hub/confluence-commands--append-remote-comment-entry
   sidecar-file source-file
   (hub/confluence-commands--remote-inline-entry
    comment body-format (file-name-directory sidecar-file))))

(defun hub/confluence-commands--append-remote-reply-comment
    (sidecar-file source-file comment body-format parent-remote-id)
  "Append remote reply COMMENT under PARENT-REMOTE-ID in SIDECAR-FILE."
  (hub/org-comment--ensure-sidecar-header sidecar-file source-file)
  (with-temp-buffer
    (insert-file-contents sidecar-file)
    (org-mode)
    (goto-char (point-min))
    (unless (cl-loop while (re-search-forward org-heading-regexp nil t)
		     do (goto-char (match-beginning 0))
		     when (and (equal "confluence" (org-entry-get nil "HUB_COMMENT_SOURCE"))
			       (equal parent-remote-id (org-entry-get nil "HUB_COMMENT_REMOTE_ID")))
		     return (let ((end (save-excursion (org-end-of-subtree t t))))
			      (goto-char end)
			      (unless (bolp) (insert "\n"))
			      (unless (save-excursion
					(forward-line -1)
					(looking-at-p "[[:space:]]*$"))
				(insert "\n"))
			      (insert (hub/confluence-commands--remote-reply-entry
				       comment body-format parent-remote-id
				       (file-name-directory sidecar-file)))
			      t)
		     do (forward-line 1))
      (hub/confluence-commands--append-remote-comment-entry
       sidecar-file source-file
       (hub/confluence-commands--remote-orphan-thread-entry
	parent-remote-id (file-name-directory sidecar-file)))
      (goto-char (point-min))
      (re-search-forward org-heading-regexp nil t)
      (goto-char (point-max))
      (insert (hub/confluence-commands--remote-reply-entry
	       comment body-format parent-remote-id
	       (file-name-directory sidecar-file))))
    (write-region (point-min) (point-max) sidecar-file nil 'silent)))

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

(defvar hub/confluence-import--cdata-map nil
  "Alist mapping temporary CDATA tokens to original text bodies.")

(defun hub/confluence-import--preserve-cdata (xhtml)
  "Return XHTML with CDATA bodies replaced by temporary text tokens."
  (let ((start 0)
	(index 0)
	(output ""))
    (setq hub/confluence-import--cdata-map nil)
    (while (string-match "<!\\[CDATA\\[" xhtml start)
      (let* ((open-start (match-beginning 0))
	     (body-start (match-end 0))
	     (body-end (string-match "\\]\\]>" xhtml body-start)))
	(unless body-end
	  (setq body-end (length xhtml)))
	(let ((token (format "HUB_CONFLUENCE_CDATA_%d" index))
	      (body (substring xhtml body-start body-end)))
	  (setq output (concat output (substring xhtml start open-start) token))
	  (push (cons token body) hub/confluence-import--cdata-map)
	  (setq index (1+ index)
		start (min (length xhtml) (+ body-end 3))))))
    (concat output (substring xhtml start))))

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

(defun hub/confluence-import--anchor-macro-p (node)
  "Return non-nil when NODE is a Confluence anchor macro."
  (and (eq (hub/confluence-import--node-tag node) 'ac:structured-macro)
       (string= (or (hub/confluence-import--macro-name node) "") "anchor")))

(defun hub/confluence-import--anchor-macro-name (node)
  "Return Confluence anchor macro name from NODE, or nil."
  (when (hub/confluence-import--anchor-macro-p node)
    (or (hub/confluence-import--macro-parameter node "")
	(seq-some (lambda (child)
		    (when (eq (hub/confluence-import--node-tag child) 'ac:default-parameter)
		      (hub/confluence-import--inline child)))
		  (hub/confluence-import--node-children node)))))

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

(defun hub/confluence-import--plain-text-body (node)
  "Return plain text macro body from NODE, or an empty string."
  (let ((body (or (seq-some (lambda (child)
			      (when (eq (hub/confluence-import--node-tag child) 'ac:plain-text-body)
				(mapconcat #'hub/confluence-import--inline
					   (hub/confluence-import--node-children child) "")))
			    (hub/confluence-import--node-children node))
		  "")))
    (or (cdr (assoc body hub/confluence-import--cdata-map))
	(if (string-match "\\`\\[CDATA\\[\\(\\(?:.\\|
\\)*\\)\\]\\]\\'" body)
	    (match-string 1 body)
	  body))))

(defun hub/confluence-import--code-macro (node)
  "Convert Confluence code macro NODE to an Org source block."
  (let ((language (hub/confluence-import--macro-parameter node "language"))
	(body (hub/confluence-import--plain-text-body node)))
    (format "#+begin_src%s\n%s\n#+end_src"
	    (if (and language (not (string-empty-p (string-trim language))))
		(format " %s" (string-trim language))
	      "")
	    body)))

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
	 (cond
	  ((hub/confluence-import--anchor-macro-p node) "")
	  ((string= (or (hub/confluence-import--macro-name node) "") "status")
	   (hub/confluence-import--status node))
	  (t (hub/confluence-import--clean-inline-spacing contents))))
	('ac:link
	 (let ((anchor (hub/confluence-import--attribute node 'ac:anchor)))
	   (cond
	    ((and anchor (string-match "\\`fn-\\(.+\\)\\'" anchor))
	     (format "[fn:%s]" (match-string 1 anchor)))
	    ((and anchor (string-match "\\`fnref-.+\\'" anchor)) "")
	    (t (hub/confluence-import--clean-inline-spacing contents)))))
	((or 'ac:link-body 'ac:plain-text-link-body 'sup)
	 (hub/confluence-import--clean-inline-spacing contents))
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
  (hub/confluence-import--clean-inline-spacing
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

(defun hub/confluence-import--description-list-line (item indent)
  "Return Org description-list line for ITEM at INDENT, or nil."
  (let* ((children (seq-remove #'hub/confluence-import--list-child-p
			       (hub/confluence-import--node-children item)))
	 (first (car children)))
    (when (memq (hub/confluence-import--node-tag first) '(strong b))
      (let ((term (hub/confluence-import--clean-inline-spacing
		   (mapconcat #'hub/confluence-import--inline
			      (hub/confluence-import--node-children first) "")))
	    (rest (hub/confluence-import--clean-inline-spacing
		   (mapconcat #'hub/confluence-import--inline (cdr children) ""))))
	(when (string-suffix-p ":" term)
	  (format "%s- %s :: %s" indent (string-remove-suffix ":" term) rest))))))

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
		 (indent (make-string (* 2 level) ?\s))
		 (line (or (and (not ordered)
				(hub/confluence-import--description-list-line child indent))
			   (format "%s%s %s" indent marker
				   (hub/confluence-import--list-item-text child))))
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

(defun hub/confluence-import--footnote-definition-label (node)
  "Return footnote definition label when paragraph NODE starts with fn anchor."
  (when (eq (hub/confluence-import--node-tag node) 'p)
    (seq-some (lambda (child)
		(when-let* ((anchor (hub/confluence-import--anchor-macro-name child)))
		  (when (string-match "\\`fn-\\(.+\\)\\'" anchor)
		    (match-string 1 anchor))))
	      (hub/confluence-import--node-children node))))

(defun hub/confluence-import--footnote-definition (node label)
  "Convert footnote definition paragraph NODE with LABEL to Org."
  (let ((body (hub/confluence-import--clean-inline-spacing
	       (mapconcat (lambda (child)
			    (cond
			     ((hub/confluence-import--anchor-macro-p child) "")
			     ((and (eq (hub/confluence-import--node-tag child) 'ac:link)
				   (string-match "\\`fnref-.+\\'"
						 (or (hub/confluence-import--attribute child 'ac:anchor) "")))
			      "")
			     (t (hub/confluence-import--inline child))))
			  (hub/confluence-import--node-children node) ""))))
    (setq body (replace-regexp-in-string
		(format "\\`\\*%s\\.\\* ?" (regexp-quote label)) "" body))
    (format "[fn:%s] %s" label (string-trim body))))

(defun hub/confluence-import--content-entity-id (node)
  "Return content entity id from Confluence link NODE, or nil."
  (when (eq (hub/confluence-import--node-tag node) 'ac:link)
    (seq-some (lambda (child)
		(when (eq (hub/confluence-import--node-tag child) 'ri:content-entity)
		  (hub/confluence-import--attribute child 'ri:content-id)))
	      (hub/confluence-import--node-children node))))

(defun hub/confluence-import--subpage-placeholder (node)
  "Return Org property drawer for a subpage placeholder paragraph NODE."
  (seq-some (lambda (child)
	      (when-let* ((id (hub/confluence-import--content-entity-id child)))
		(format ":PROPERTIES:\n:CONFLUENCE_PAGE_ID: %s\n:END:" id)))
	    (hub/confluence-import--node-children node)))

(defun hub/confluence-import--attachment-filename (node)
  "Return attachment filename from Confluence image NODE, or nil."
  (when (eq (hub/confluence-import--node-tag node) 'ac:image)
    (seq-some (lambda (child)
		(when (eq (hub/confluence-import--node-tag child) 'ri:attachment)
		  (hub/confluence-import--attribute child 'ri:filename)))
	      (hub/confluence-import--node-children node))))

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
      ((string= (or (hub/confluence-import--macro-name node) "") "code")
       (hub/confluence-import--code-macro node))
      ((hub/confluence-import--callout-macro-p node)
       (hub/confluence-import--callout node))
      (t (hub/confluence-import--rich-text-body node))))
    ('h1 (format "* %s" (hub/confluence-import--inline node)))
    ('h2 (format "** %s" (hub/confluence-import--inline node)))
    ('h3 (format "*** %s" (hub/confluence-import--inline node)))
    ('h4 (format "**** %s" (hub/confluence-import--inline node)))
    ('p (cond
	 ((hub/confluence-import--subpage-placeholder node))
	 ((if-let* ((label (hub/confluence-import--footnote-definition-label node)))
	      (hub/confluence-import--footnote-definition node label)))
	 (t (hub/confluence-import--inline node))))
    ((or 'ul 'ol) (hub/confluence-import--list node))
    ('table (hub/confluence-import--table node))
    ('ac:image (if-let* ((filename (hub/confluence-import--attachment-filename node)))
		   (format "[[./%s]]" filename)
		 ""))
    ('blockquote (format "#+begin_quote\n%s\n#+end_quote"
			 (hub/confluence-import--join-blocks
			  (mapcar #'hub/confluence-import--block
				  (hub/confluence-import--node-children node)))))
    ('hr "-----")
    (_ (hub/confluence-import--inline node))))

(defun hub/confluence-import--restore-footnotes-heading (org)
  "Restore the Org Footnotes heading from imported ORG when appropriate."
  (replace-regexp-in-string "\\(?:\\`\\|\n\\)-----\n\\(\\[fn:[^]]+\\]\\)"
			    "\n* Footnotes\n\n\\1"
			    org t))

(defun hub/confluence-import--line-kind (line)
  "Return structural kind for imported Org LINE."
  (cond
   ((string-empty-p (string-trim line)) 'blank)
   ((string-match-p "\\`\\*+ " line) 'heading)
   ((string-match-p "\\`:PROPERTIES:\\|:END:\\|:[A-Z0-9_]+:" line) 'property)
   ((string-match-p "\\`#\\+begin_" line) 'block-begin)
   ((string-match-p "\\`#\\+end_" line) 'block-end)
   ((string-match-p "\\`#\\+ATTR_" line) 'attr)
   ((string-match-p "\\`|" line) 'table)
   ((string-match-p "\\`[[:space:]]*- " line) 'unordered-list)
   ((string-match-p "\\`[[:space:]]*[0-9]+\\. " line) 'ordered-list)
   ((string-match-p "\\`\\[\\[\.\/.*\\]\\]" line) 'image)
   ((string-match-p "\\`\\[fn:[^]]+\\]" line) 'footnote)
   (t 'paragraph)))

(defun hub/confluence-import--blank-before-p (previous-kind current-kind _next-kind)
  "Return non-nil when a blank line should precede CURRENT-KIND.
PREVIOUS-KIND provides neighboring context."
  (and previous-kind
       (not (eq previous-kind 'blank))
       (not (eq current-kind 'blank))
       (not (and (eq previous-kind 'heading) (eq current-kind 'property)))
       (not (and (eq previous-kind 'property) (eq current-kind 'property)))
       (not (and (eq previous-kind 'attr) (eq current-kind 'block-begin)))
       (not (and (eq previous-kind 'table) (eq current-kind 'table)))
       (not (and (memq previous-kind '(unordered-list ordered-list))
		 (eq current-kind previous-kind)))
       (or (memq current-kind '(heading block-begin attr table image))
	   (and (memq previous-kind '(heading block-end table image))
		(not (eq current-kind 'property)))
	   (and (memq previous-kind '(unordered-list ordered-list))
		(not (memq current-kind '(property))))
	   (and (eq current-kind 'footnote)
		(not (eq previous-kind 'heading))))))

(defun hub/confluence-import--format-org (org)
  "Return imported ORG with stable blank lines between block constructs."
  (let* ((lines (split-string (string-trim org) "\n"))
	 (kinds (mapcar #'hub/confluence-import--line-kind lines))
	 (in-verbatim nil)
	 output previous-kind)
    (cl-loop for line in lines
	     for kind in kinds
	     for index from 0
	     for next-kind = (nth (1+ index) kinds)
	     do
	     (when (and (not in-verbatim)
			(hub/confluence-import--blank-before-p previous-kind kind next-kind)
			output
			(not (string-empty-p (car output))))
	       (push "" output))
	     (push line output)
	     (when (eq kind 'block-begin)
	       (setq in-verbatim (string-match-p "\\`#\\+begin_\\(?:src\\|example\\)" line)))
	     (when (and in-verbatim (eq kind 'block-end))
	       (setq in-verbatim nil))
	     (setq previous-kind kind))
    (string-trim (string-join (nreverse output) "\n"))))

(defun hub/confluence-import-storage-to-org (xhtml)
  "Convert Confluence storage XHTML string XHTML to Org text."
  (with-temp-buffer
    (insert (hub/confluence-import--preserve-cdata xhtml))
    (let ((tree (libxml-parse-html-region (point-min) (point-max))))
      (hub/confluence-import--format-org
       (hub/confluence-import--restore-footnotes-heading
	(hub/confluence-import--block tree))))))

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

(defun hub/confluence-commands--subpage-root-position ()
  "Return current subtree root position when point is at a subpage export root."
  (when (and (not (org-before-first-heading-p))
	     (org-entry-get nil "CONFLUENCE_PAGE_ID"))
    (point)))

(defun hub/confluence-commands--inside-another-subpage-p (root-position)
  "Return non-nil when current heading is inside another subpage after ROOT-POSITION."
  (save-excursion
    (let ((inside nil))
      (while (and (not inside) (org-up-heading-safe))
	(when (and (> (point) root-position)
		   (org-entry-get nil "CONFLUENCE_PAGE_ID"))
	  (setq inside t)))
      inside)))

(defun hub/confluence-commands--direct-subpage-markers (&optional subtreep)
  "Return markers for direct child subpages in current export scope.
When SUBTREEP is non-nil, the current subtree root itself is not included."
  (save-excursion
    (save-restriction
      (let ((root-position (if subtreep
			       (progn (org-back-to-heading t) (point))
			     (point-min)))
	    markers)
	(when subtreep
	  (org-narrow-to-subtree))
	(org-map-entries
	 (lambda ()
	   (when (and (org-entry-get nil "CONFLUENCE_PAGE_ID")
		      (not (= (point) root-position))
		      (not (hub/confluence-commands--inside-another-subpage-p root-position)))
	     (push (point-marker) markers)))
	 nil (when subtreep 'tree))
	(nreverse markers)))))

(defvar hub/confluence-publish--skip-inline-comment-preflight nil
  "Non-nil means recursive publish calls skip inline comment preflight.")

(defvar hub/confluence-publish--pages-needing-inline-marker-preservation nil
  "Page IDs whose active inline comment markers should be preserved on update.")

(defvar hub/confluence-publish--pages-needing-inline-marker-repair nil
  "Page IDs with safely repairable missing inline comment markers.")

(defun hub/confluence-publish--interactive-repair-p ()
  "Return non-nil when publish may prompt for inline marker repair."
  (not noninteractive))

(defun hub/confluence-commands--publish-subpages (subtreep visible-only body-only ext-plist)
  "Publish direct child subpages for current export scope before their parent."
  (dolist (marker (hub/confluence-commands--direct-subpage-markers subtreep))
    (save-excursion
      (goto-char marker)
      (hub/confluence-publish nil t visible-only body-only ext-plist))))

(defun hub/confluence-publish--target-page-ids (subtreep)
  "Return Confluence page IDs that would be updated for SUBTREEP publish."
  (save-excursion
    (save-restriction
      (let ((root-id (hub/confluence-api--page-id-from-buffer subtreep))
	    ids)
	(when subtreep
	  (org-back-to-heading t)
	  (org-narrow-to-subtree))
	(when root-id
	  (push (format "%s" root-id) ids))
	(org-map-entries
	 (lambda ()
	   (when-let* ((id (org-entry-get nil "CONFLUENCE_PAGE_ID")))
	     (push (format "%s" id) ids)))
	 nil (when subtreep 'tree))
	(delete-dups (nreverse ids))))))

(defun hub/confluence-publish--inline-comment-blocking-p (comment)
  "Return non-nil when inline COMMENT should block page publish.

Anchored comments with marker refs and selected text are preserved during page
update, so they do not block.  Active comments without enough anchor metadata
still block because they cannot be safely reinserted."
  (let ((status (hub/confluence-commands--remote-comment-resolution-status comment)))
    (and (not (member status '("resolved" "dangling")))
	 (or (not hub/confluence-publish-preserve-inline-comments)
	     (not (hub/confluence-api--present-string-p
		   (hub/confluence-commands--remote-inline-marker-ref comment)))
	     (not (hub/confluence-api--present-string-p
		   (hub/confluence-commands--remote-inline-target-text comment)))))))

(defun hub/confluence-publish--sidecar-comment-property (sidecar-file comment-id property)
  "Return PROPERTY for COMMENT-ID in SIDECAR-FILE, or nil."
  (when (and sidecar-file comment-id (file-exists-p sidecar-file))
    (with-temp-buffer
      (insert-file-contents sidecar-file)
      (org-mode)
      (when (hub/org-comment-goto-id comment-id)
	(org-entry-get nil property)))))

(defun hub/confluence-publish--sidecar-remote-property (sidecar-file remote-id property)
  "Return PROPERTY for REMOTE-ID in SIDECAR-FILE, or nil."
  (when (and sidecar-file remote-id (file-exists-p sidecar-file))
    (with-temp-buffer
      (insert-file-contents sidecar-file)
      (org-mode)
      (goto-char (point-min))
      (cl-loop while (re-search-forward org-heading-regexp nil t)
	       do (goto-char (match-beginning 0))
	       when (equal remote-id (org-entry-get nil "HUB_COMMENT_REMOTE_ID"))
	       return (org-entry-get nil property)
	       do (forward-line 1)))))

(defun hub/confluence-publish--sidecar-remote-missing-p (sidecar-file record)
  "Return non-nil when sidecar says RECORD is remote-missing."
  (let ((local-id (plist-get record :local-comment-id))
	(remote-id (plist-get record :comment-id)))
    (or (equal "missing" (plist-get record :remote-state))
	(equal "missing"
	       (hub/confluence-publish--sidecar-comment-property
		sidecar-file local-id "HUB_COMMENT_REMOTE_STATE"))
	(equal "missing"
	       (hub/confluence-publish--sidecar-remote-property
		sidecar-file remote-id "HUB_COMMENT_REMOTE_STATE")))))

(defun hub/confluence-publish--inline-comment-record (page-id comment &optional sidecar-file source-file)
  "Return report record for PAGE-ID inline COMMENT."
  (let* ((remote-id (hub/confluence-commands--remote-comment-id comment))
	 (local-id (or (and sidecar-file
			    (hub/org-comment-local-id-for-remote-id sidecar-file remote-id))
		       (and remote-id (format "remote-confluence-%s" remote-id))))
	 (remote-state (hub/confluence-publish--sidecar-comment-property
			sidecar-file local-id "HUB_COMMENT_REMOTE_STATE")))
    (list :page-id page-id
	  :comment-id remote-id
	  :local-comment-id local-id
	  :source-file source-file
	  :remote-state remote-state
	  :author (hub/confluence-commands--remote-comment-author-name comment)
	  :status (or (hub/confluence-commands--remote-comment-resolution-status comment) "unknown")
	  :target-text (hub/confluence-commands--remote-inline-target-text comment)
	  :anchored (hub/confluence-commands--remote-inline-anchor-confirmed-p comment))))

(defun hub/confluence-publish--preflight-record-link (record)
  "Return actionable Org link text for preflight RECORD."
  (let ((source-file (plist-get record :source-file))
	(local-id (plist-get record :local-comment-id))
	(remote-id (plist-get record :comment-id)))
    (if (and source-file local-id)
	(hub/org-comment-make-link source-file local-id (format "comment %s" remote-id))
      (format "comment %s" (or remote-id "<unknown>")))))

(defun hub/confluence-publish--preflight-next-link ()
  "Move point to the next link in the Confluence publish preflight report."
  (interactive)
  (if (fboundp 'org-next-link)
      (org-next-link)
    (unless (re-search-forward org-link-any-re nil t)
      (goto-char (point-min))
      (re-search-forward org-link-any-re nil t))))

(defun hub/confluence-publish--record-sidecar-file (record)
  "Return sidecar file for preflight RECORD, or nil."
  (when-let* ((source-file (plist-get record :source-file)))
    (hub/org-comment-sidecar-path source-file)))

(defun hub/confluence-publish--record-remote-missing-p (record)
  "Return non-nil when RECORD is marked remote-missing in its sidecar."
  (let ((sidecar-file (hub/confluence-publish--record-sidecar-file record)))
    (hub/confluence-publish--sidecar-remote-missing-p sidecar-file record)))

(defun hub/confluence-publish--nonblocking-record-p (record)
  "Return non-nil when RECORD should not block publish."
  (or (equal "dangling" (plist-get record :status))
      (hub/confluence-publish--record-remote-missing-p record)))

(defun hub/confluence-publish--reclassify-nonblocking-records (report)
  "Move remote-missing blockers in REPORT to the non-blocking list."
  (let (blockers nonblocking)
    (dolist (record (plist-get report :blockers))
      (if (hub/confluence-publish--nonblocking-record-p record)
	  (progn
	    (plist-put record :remote-state "missing")
	    (push record nonblocking))
	(push record blockers)))
    (plist-put report :blockers (nreverse blockers))
    (plist-put report :dangling
	       (append (nreverse nonblocking) (plist-get report :dangling)))
    report))

(defun hub/confluence-publish--preflight-report-buffer (report &optional force)
  "Show inline comment preflight REPORT.  FORCE notes non-aborting publish."
  (let ((buffer (get-buffer-create "*Org Confluence Publish Preflight*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
	(erase-buffer)
	(insert "Org Confluence publish preflight\n")
	(when force
	  (insert "FORCE: publish will continue despite blocking inline comments.\n"))
	(insert "\n")
	(insert "* Blocking active inline comments\n")
	(if-let* ((blockers (plist-get report :blockers)))
	    (dolist (record blockers)
	      (insert (format "- page %s %s [%s]%s%s%s\n"
			      (plist-get record :page-id)
			      (hub/confluence-publish--preflight-record-link record)
			      (plist-get record :status)
			      (if (plist-get record :anchored) " anchored" " anchor-unconfirmed")
			      (if-let* ((reason (plist-get record :repair-reason)))
				  (format " reason=%s" reason)
				"")
			      (if-let* ((text (plist-get record :target-text)))
				  (format " — %s" text)
				""))))
	  (insert "None.\n"))
	(insert "\n* Safely repairable missing inline markers\n")
	(if-let* ((repairable (plist-get report :repairable)))
	    (dolist (record repairable)
	      (insert (format "- page %s %s [%s]%s%s\n"
			      (plist-get record :page-id)
			      (hub/confluence-publish--preflight-record-link record)
			      (or (plist-get record :status) "open")
			      (if-let* ((count (plist-get record :candidate-count)))
				  (format " candidates=%s" count)
				"")
			      (if-let* ((text (plist-get record :target-text)))
				  (format " — %s" text)
				""))))
	  (insert "None.\n"))
	(insert "\n* Already dangling or remote-missing / not blocking\n")
	(if-let* ((dangling (plist-get report :dangling)))
	    (dolist (record dangling)
	      (insert (format "- page %s %s [%s%s]%s\n"
			      (plist-get record :page-id)
			      (hub/confluence-publish--preflight-record-link record)
			      (plist-get record :status)
			      (if-let* ((state (plist-get record :remote-state)))
				  (format ", %s" state)
				"")
			      (if-let* ((text (plist-get record :target-text)))
				  (format " — %s" text)
				""))))
	  (insert "None.\n"))
	(insert "\n* Updated sidecars\n")
	(if-let* ((sidecars (plist-get report :sidecars)))
	    (dolist (sidecar (delete-dups sidecars))
	      (insert (format "- %s\n" sidecar)))
	  (insert "None.\n"))
	(goto-char (point-min))
	(org-mode)
	(use-local-map (copy-keymap org-mode-map))
	(dolist (key (list (kbd "TAB") (kbd "<tab>") (kbd "C-i")))
	  (local-set-key key #'hub/confluence-publish--preflight-next-link))
	(when (fboundp 'evil-local-set-key)
	  (evil-local-set-key 'normal (kbd "TAB") #'hub/confluence-publish--preflight-next-link)
	  (evil-local-set-key 'normal (kbd "<tab>") #'hub/confluence-publish--preflight-next-link))
	(setq buffer-read-only t)))
    (display-buffer buffer)
    buffer))

(defun hub/confluence-publish--inline-comment-preflight (page-ids &optional force)
  "Import and classify inline comments for PAGE-IDS before publish.
When FORCE is nil, signal `user-error' if active inline comments would be
orphaned by a page update."
  (let ((report (list :blockers nil :dangling nil :repairable nil :sidecars nil))
	(body-format "storage"))
    (dolist (page-id page-ids)
      (let* ((response (hub/confluence-api--list-page-comments
			page-id "inline-comments" body-format))
	     (comments (hub/confluence-commands--comment-results response))
	     (sidecar (and buffer-file-name
			   (hub/org-comment-sidecar-path buffer-file-name)))
	     page-storage)
	(cl-labels ((storage ()
		      (or page-storage
			  (setq page-storage
				(hub/confluence-commands--page-body-storage-value
				 (hub/confluence-api--get-page page-id "storage"))))))
	  (hub/confluence-commands--import-remote-comments
	   page-id "inline-comments" body-format
	   #'hub/confluence-commands--append-remote-inline-comment t)
	  (when sidecar
	    (plist-put report :sidecars (cons sidecar (plist-get report :sidecars))))
	  (dolist (comment comments)
	    (let ((record (hub/confluence-publish--inline-comment-record
			   page-id comment sidecar buffer-file-name)))
	      (cond
	       ((or (equal "dangling" (plist-get record :status))
		    (hub/confluence-publish--sidecar-remote-missing-p sidecar record))
		(plist-put record :remote-state "missing")
		(plist-put report :dangling (cons record (plist-get report :dangling))))
	       ((hub/confluence-publish--inline-comment-blocking-p comment)
		(plist-put report :blockers (cons record (plist-get report :blockers))))
	       ((hub/confluence-repair--comment-marker-present-p (storage) comment)
		(let ((complex-reason
		       (cdr (assoc-string
			     (hub/confluence-commands--remote-inline-marker-ref comment)
			     (hub/confluence-commands--complex-inline-marker-ref-reasons
			      (storage))
			     t))))
		  (if (and complex-reason
			   (not (member (plist-get record :status) '("resolved" "dangling"))))
		      (progn
			(plist-put record :repair-reason complex-reason)
			(plist-put report :blockers (cons record (plist-get report :blockers))))
		    (unless (member (format "%s" page-id)
				    hub/confluence-publish--pages-needing-inline-marker-preservation)
		      (push (format "%s" page-id)
			    hub/confluence-publish--pages-needing-inline-marker-preservation)))))
	       (t
		(let ((candidate (hub/confluence-repair--candidate-for-comment
				  (storage) comment buffer-file-name)))
		  (setq record (append record
				       (list :candidate-count (plist-get candidate :candidate-count)
					     :repair-reason (plist-get candidate :reason))))
		  (if (and (plist-member candidate :start)
			   (not (plist-get candidate :reason)))
		      (progn
			(plist-put report :repairable (cons record (plist-get report :repairable)))
			(unless (member (format "%s" page-id)
					hub/confluence-publish--pages-needing-inline-marker-repair)
			  (push (format "%s" page-id)
				hub/confluence-publish--pages-needing-inline-marker-repair)))
		    (plist-put report :blockers (cons record (plist-get report :blockers))))))))))))
    (plist-put report :blockers (nreverse (plist-get report :blockers)))
    (plist-put report :repairable (nreverse (plist-get report :repairable)))
    (plist-put report :dangling (nreverse (plist-get report :dangling)))
    (hub/confluence-publish--reclassify-nonblocking-records report)
    (when (or (plist-get report :blockers)
	      (plist-get report :repairable)
	      (plist-get report :dangling))
      (hub/confluence-publish--preflight-report-buffer report force))
    (when (and (not force) (plist-get report :blockers))
      (user-error "Refusing to publish: %s active inline Confluence comment(s) would lose anchors"
		  (length (plist-get report :blockers))))
    (when (plist-get report :repairable)
      (cond
       ((not (hub/confluence-publish--interactive-repair-p))
	(user-error "Refusing to publish: %s inline comment anchor(s) need repair; run hub/confluence-repair-inline-comment-anchors"
		    (length (plist-get report :repairable))))
       ((yes-or-no-p (format "Repair %s missing inline comment anchor(s) before publishing? "
			     (length (plist-get report :repairable))))
	(dolist (repair-page-id hub/confluence-publish--pages-needing-inline-marker-repair)
	  (hub/confluence-repair-inline-comment-anchors repair-page-id t)))
       (t
	(user-error "Refusing to publish with unrepaired inline comment anchors"))))
    report))

;;;###autoload
(defun hub/confluence-publish (&optional async subtreep visible-only body-only ext-plist)
  "Publish the current Org buffer or subtree to an existing Confluence page.

The document must contain #+CONFLUENCE_PAGE_ID.  When SUBTREEP is non-nil, a
CONFLUENCE_PAGE_ID Org property on the current subtree takes precedence.  The
Org document is exported to Confluence Storage Format XHTML and passed to `cfl
page edit --file --storage'.  Interactively, a prefix argument forces publishing
despite active inline comment anchors.  ASYNC, SUBTREEP, VISIBLE-ONLY,
BODY-ONLY, and EXT-PLIST follow Org export conventions."
  (interactive (list current-prefix-arg))
  (let ((force (and (called-interactively-p 'interactive) async))
	(top-level-p (not hub/confluence-publish--skip-inline-comment-preflight)))
    (when top-level-p
      (setq hub/confluence-publish--pages-needing-inline-marker-preservation nil
	    hub/confluence-publish--pages-needing-inline-marker-repair nil)
      (hub/confluence-publish--inline-comment-preflight
       (hub/confluence-publish--target-page-ids subtreep) force))
    (let ((hub/confluence-publish--skip-inline-comment-preflight t))
      (hub/confluence-commands--publish-subpages subtreep visible-only body-only ext-plist)
      (let* ((page-id (hub/confluence-api--page-id-from-buffer subtreep))
	     (assets (org-confluence-image-assets subtreep))
	     (export-plist (append (when subtreep '(:confluence-omit-root-heading t))
				   ext-plist))
	     (xhtml-file nil))
	(unwind-protect
	    (progn
	      (let ((xhtml (org-confluence-export nil subtreep visible-only body-only
						  (append (list :confluence-image-filenames
								(hub/confluence-commands--asset-filename-map assets))
							  export-plist))))
		(setq xhtml-file
		      (hub/confluence-commands--write-temp-xhtml
		       (hub/confluence-commands--preserve-inline-comments page-id xhtml))))
	      (hub/confluence-commands--upload-assets page-id assets)
	      (hub/confluence-commands--run
	       (hub/confluence-api--page-update-command page-id xhtml-file))
	      (message "Published Org buffer to Confluence page %s" page-id)
	      page-id)
	  (when (and xhtml-file (file-exists-p xhtml-file))
	    (delete-file xhtml-file))
	  (when top-level-p
	    (setq hub/confluence-publish--pages-needing-inline-marker-preservation nil
		  hub/confluence-publish--pages-needing-inline-marker-repair nil)))))))

;;;###autoload
(defun hub/confluence-publish-force (&optional subtreep visible-only body-only ext-plist)
  "Publish to Confluence even when active inline comment anchors are present."
  (interactive)
  (setq hub/confluence-publish--pages-needing-inline-marker-preservation nil
	hub/confluence-publish--pages-needing-inline-marker-repair nil)
  (hub/confluence-publish--inline-comment-preflight
   (hub/confluence-publish--target-page-ids subtreep) t)
  (let ((hub/confluence-publish--skip-inline-comment-preflight t))
    (hub/confluence-publish nil subtreep visible-only body-only ext-plist)))

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

(defun hub/confluence-sync--sha256 (text)
  "Return a sha256 digest for TEXT."
  (concat "sha256:" (secure-hash 'sha256 (or text ""))))

(defun hub/confluence-sync--keyword (keyword)
  "Return Org KEYWORD value in the current buffer, or nil."
  (hub/confluence-api--keyword-from-buffer keyword))

(defun hub/confluence-sync--set-keyword (keyword value)
  "Set Org KEYWORD to VALUE in the current buffer."
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search t)
	  (regexp (format "^[ \t]*#\\+%s:.*$" (regexp-quote keyword))))
      (if (re-search-forward regexp nil t)
	  (replace-match (format "#+%s: %s" keyword value) t t)
	(hub/confluence-commands--insert-metadata-after-keywords
	 `((,keyword . ,value)))))))

(defun hub/confluence-sync--source-without-sync-metadata ()
  "Return current Org buffer text without Confluence sync metadata lines."
  (let ((source-text (buffer-string)))
    (with-temp-buffer
      (insert source-text)
      (goto-char (point-min))
      (while (re-search-forward "^[	 ]*#\\+CONFLUENCE_.*\n?" nil t)
	(replace-match ""))
      (string-trim (buffer-string)))))

(defun hub/confluence-sync--collapse-blank-runs (org)
  "Collapse repeated blank lines in ORG outside verbatim blocks."
  (let ((lines (split-string org "\n"))
	(in-verbatim nil)
	(blank-pending nil)
	output)
    (dolist (line lines)
      (let ((kind (hub/confluence-import--line-kind line)))
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

(defun hub/confluence-sync--canonical-org-for-hash (org)
  "Return canonical ORG text for sync hash comparisons."
  (hub/confluence-sync--collapse-blank-runs
   (hub/confluence-import--format-org org)))

(defun hub/confluence-sync--local-org-hash ()
  "Return hash of current Org content excluding sync metadata."
  (hub/confluence-sync--sha256
   (hub/confluence-sync--canonical-org-for-hash
    (hub/confluence-sync--source-without-sync-metadata))))

(defun hub/confluence-sync--page-version (page)
  "Return Confluence PAGE version number as a string."
  (format "%s" (or (alist-get 'number (alist-get 'version page))
		   (user-error "Confluence page response did not include version.number"))))

(defun hub/confluence-sync--page-storage (page)
  "Return Confluence PAGE storage XHTML."
  (or (alist-get 'value (alist-get 'storage (alist-get 'body page)))
      (user-error "Confluence page response did not include body.storage.value")))

(defun hub/confluence-sync--timestamp ()
  "Return timestamp for page sync metadata."
  (format-time-string "%Y-%m-%dT%H:%M:%S%z"))

(defun hub/confluence-sync--stamp-metadata (version storage-hash local-hash)
  "Stamp sync metadata VERSION, STORAGE-HASH, and LOCAL-HASH in current buffer."
  (hub/confluence-sync--set-keyword "CONFLUENCE_PAGE_VERSION" version)
  (hub/confluence-sync--set-keyword "CONFLUENCE_PAGE_STORAGE_HASH" storage-hash)
  (hub/confluence-sync--set-keyword "CONFLUENCE_LOCAL_ORG_HASH" local-hash)
  (hub/confluence-sync--set-keyword "CONFLUENCE_PAGE_LAST_SYNCED_AT"
				    (hub/confluence-sync--timestamp)))

(defun hub/confluence-sync--body-start ()
  "Return position after leading Org keyword block."
  (save-excursion
    (goto-char (point-min))
    (while (looking-at-p "^[ \t]*#\\+[^:\n]+:.*$")
      (forward-line 1))
    (while (looking-at-p "^[ \t]*$")
      (forward-line 1))
    (point)))

(defun hub/confluence-sync--replace-body (org-body)
  "Replace current Org document body with ORG-BODY, preserving leading keywords."
  (let ((start (hub/confluence-sync--body-start)))
    (delete-region start (point-max))
    (goto-char start)
    (insert (string-trim org-body) "\n")))

(defun hub/confluence-sync--conflict-buffer
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

;;;###autoload
(defun hub/confluence-sync-page-current (&optional page-id)
  "Synchronize current Org buffer main page content with Confluence PAGE-ID."
  (interactive)
  (let* ((id (or page-id (hub/confluence-api--page-id-from-buffer)
		 (read-string "Confluence page ID: ")))
	 (page (hub/confluence-commands--json-alist
		(hub/confluence-commands--response-body
		 (hub/confluence-api--get-page id "storage"))))
	 (remote-version (hub/confluence-sync--page-version page))
	 (remote-storage (hub/confluence-sync--page-storage page))
	 (remote-storage-hash (hub/confluence-sync--sha256 remote-storage))
	 (remote-org (hub/confluence-import-storage-to-org remote-storage))
	 (stored-version (hub/confluence-sync--keyword "CONFLUENCE_PAGE_VERSION"))
	 (stored-local-hash (hub/confluence-sync--keyword "CONFLUENCE_LOCAL_ORG_HASH"))
	 (local-hash (hub/confluence-sync--local-org-hash)))
    (cond
     ((not stored-version)
      (hub/confluence-sync--stamp-metadata remote-version remote-storage-hash local-hash)
      (save-buffer)
      (message "Initialized Confluence sync metadata for page %s" id)
      (list :initialized 1 :page-id id :remote-version remote-version))
     ((and (not (equal remote-version stored-version))
	   stored-local-hash
	   (not (equal local-hash stored-local-hash)))
      (hub/confluence-sync--conflict-buffer
       id local-hash stored-local-hash remote-version stored-version remote-org)
      (list :conflict 1 :page-id id :remote-version remote-version))
     ((not (equal remote-version stored-version))
      (hub/confluence-sync--replace-body remote-org)
      (setq local-hash (hub/confluence-sync--local-org-hash))
      (hub/confluence-sync--stamp-metadata remote-version remote-storage-hash local-hash)
      (save-buffer)
      (message "Pulled Confluence page %s v%s into local Org" id remote-version)
      (list :pulled 1 :page-id id :remote-version remote-version))
     ((and stored-local-hash (not (equal local-hash stored-local-hash)))
      (hub/confluence-publish)
      (let* ((updated-page (hub/confluence-commands--json-alist
			    (hub/confluence-commands--response-body
			     (hub/confluence-api--get-page id "storage"))))
	     (updated-version (hub/confluence-sync--page-version updated-page))
	     (updated-storage (hub/confluence-sync--page-storage updated-page))
	     (updated-storage-hash (hub/confluence-sync--sha256 updated-storage)))
	(hub/confluence-sync--stamp-metadata updated-version updated-storage-hash local-hash)
	(save-buffer)
	(message "Pushed local Org to Confluence page %s v%s" id updated-version)
	(list :pushed 1 :page-id id :remote-version updated-version)))
     (t
      (message "Confluence page %s already synchronized" id)
      (list :noop 1 :page-id id :remote-version remote-version)))))

(defun hub/confluence-sync--pending-comment-ids ()
  "Return local sidecar comment IDs that should be pushed to Confluence."
  (let (ids)
    (org-map-entries
     (lambda ()
       (when (and (org-entry-get nil "HUB_COMMENT_ID")
		  (not (equal "RESOLVED" (org-get-todo-state)))
		  (or (not (org-entry-get nil "HUB_COMMENT_REMOTE_ID"))
		      (org-entry-get nil "HUB_COMMENT_LOCAL_UPDATED_AT")))
	 (push (org-entry-get nil "HUB_COMMENT_ID") ids)))
     nil 'file)
    (nreverse ids)))

(defun hub/confluence-sync--goto-sidecar-comment-id (comment-id)
  "Move point to sidecar COMMENT-ID heading and return non-nil when found."
  (goto-char (point-min))
  (let ((regexp (format "^[ \t]*:HUB_COMMENT_ID:[ \t]+%s[ \t]*$"
			(regexp-quote comment-id))))
    (when (re-search-forward regexp nil t)
      (org-back-to-heading t)
      t)))

(defun hub/confluence-sync--push-pending-comments (source-buffer &optional page-id)
  "Push pending sidecar comments for SOURCE-BUFFER to Confluence PAGE-ID."
  (let* ((source-file (buffer-file-name source-buffer))
	 (sidecar-file (and source-file (hub/org-comment-sidecar-path source-file)))
	 (pushed 0)
	 errors)
    (when (and sidecar-file (file-exists-p sidecar-file))
      (with-current-buffer (find-file-noselect sidecar-file)
	(unless (derived-mode-p 'org-mode)
	  (org-mode))
	(dolist (comment-id (hub/confluence-sync--pending-comment-ids))
	  (condition-case error
	      (when (hub/confluence-sync--goto-sidecar-comment-id comment-id)
		(hub/confluence-comment-push-current page-id)
		(setq pushed (1+ pushed)))
	    (error
	     (push (format "%s: %s" comment-id (error-message-string error)) errors))))))
    (list :pushed pushed :errors (nreverse errors))))

;;;###autoload
(defun hub/confluence-sync-current (&optional page-id body-format)
  "Synchronize current Org page and sidecar comments with Confluence.
PAGE-ID overrides the current buffer's `CONFLUENCE_PAGE_ID'.  BODY-FORMAT is
passed to comment import and defaults to storage.  When page sync detects a
conflict, comments are not imported or pushed."
  (interactive)
  (let* ((source-buffer (current-buffer))
	 (id (hub/confluence-commands--page-id-or-read page-id))
	 (page-result (hub/confluence-sync-page-current id)))
    (if (plist-get page-result :conflict)
	(progn
	  (message "Confluence page conflict detected; skipped comment sync")
	  (list :page page-result :comments-skipped 1))
      (with-current-buffer source-buffer
	(let* ((imported (hub/confluence-comment-import id body-format))
	       (push-result (hub/confluence-sync--push-pending-comments source-buffer id))
	       (pushed (plist-get push-result :pushed))
	       (errors (plist-get push-result :errors)))
	  (message "Confluence sync complete: imported %s comments, pushed %s comments%s"
		   imported pushed
		   (if errors (format ", %s push errors" (length errors)) ""))
	  (list :page page-result :comments-imported imported
		:comments-pushed pushed :comment-push-errors errors))))))

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
(defun hub/confluence-comment-list (&optional page-id body-format)
  "Fetch Confluence comments for PAGE-ID and render a diagnostic buffer.
This command is intentionally read-only: it does not write sidecar files or
modify the source Org buffer.  BODY-FORMAT defaults to storage."
  (interactive)
  (let* ((id (or page-id
		 (hub/confluence-api--page-id-from-buffer)
		 (read-string "Confluence page ID: ")))
	 (format-name (or body-format "storage"))
	 (footer-response (hub/confluence-api--list-page-comments
			   id "footer-comments" format-name))
	 (inline-response (hub/confluence-api--list-page-comments
			   id "inline-comments" format-name))
	 (buffer (generate-new-buffer (format "*Confluence Comments %s*" id))))
    (with-current-buffer buffer
      (special-mode)
      (let ((inhibit-read-only t))
	(erase-buffer)
	(insert (format "Confluence comments for page %s\n\n" id))
	(hub/confluence-commands--insert-comment-section
	 "footer-comments" format-name footer-response)
	(insert "\n")
	(hub/confluence-commands--insert-comment-section
	 "inline-comments" format-name inline-response)
	(goto-char (point-min))))
    (pop-to-buffer buffer)))

(defun hub/confluence-commands--page-id-or-read (page-id)
  "Return PAGE-ID, current buffer page ID, or prompt for one."
  (or page-id
      (hub/confluence-api--page-id-from-buffer)
      (read-string "Confluence page ID: ")))

(defun hub/confluence-commands--sidecar-source-file ()
  "Return source Org file for the current comments sidecar buffer."
  (unless (and buffer-file-name
	       (string-suffix-p ".comments.org" buffer-file-name))
    (user-error "Current buffer is not a comments sidecar"))
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search t))
      (unless (re-search-forward "^[\t ]*#\\+source:[\t ]*\\(.*?\\)[\t ]*$" nil t)
	(user-error "Comments sidecar has no #+source keyword"))
      (expand-file-name (string-trim (match-string-no-properties 1))
			(file-name-directory buffer-file-name)))))

(defun hub/confluence-commands--page-id-from-sidecar-source (&optional page-id)
  "Return PAGE-ID or read Confluence page ID from current sidecar source file."
  (or page-id
      (let ((source-file (hub/confluence-commands--sidecar-source-file)))
	(unless (file-readable-p source-file)
	  (user-error "Cannot read source Org file: %s" source-file))
	(with-current-buffer (find-file-noselect source-file)
	  (or (hub/confluence-api--page-id-from-buffer)
	      (read-string "Confluence page ID: "))))))

(defun hub/confluence-commands--source-page-metadata-from-sidecar ()
  "Return plist with Confluence page metadata from current sidecar source file."
  (let ((source-file (hub/confluence-commands--sidecar-source-file)))
    (unless (file-readable-p source-file)
      (user-error "Cannot read source Org file: %s" source-file))
    (with-current-buffer (find-file-noselect source-file)
      (list :page-id (or (hub/confluence-api--page-id-from-buffer)
			 (read-string "Confluence page ID: "))
	    :space (hub/confluence-api--keyword-from-buffer "CONFLUENCE_SPACE")))))

(defun hub/confluence-commands--html-escape (text)
  "Return TEXT escaped for an XHTML text node."
  (replace-regexp-in-string
   ">" "&gt;"
   (replace-regexp-in-string
    "<" "&lt;"
    (replace-regexp-in-string
     "&" "&amp;" (or text "") t t)
    t t)
   t t))

(defun hub/confluence-commands--sanitize-comment-storage (html)
  "Return Confluence-friendly storage XHTML from Org-exported HTML."
  (let ((storage (string-trim html)))
    (setq storage (replace-regexp-in-string "<b>" "<strong>" storage t t))
    (setq storage (replace-regexp-in-string "</b>" "</strong>" storage t t))
    (setq storage (replace-regexp-in-string "<i>" "<em>" storage t t))
    (setq storage (replace-regexp-in-string "</i>" "</em>" storage t t))
    (setq storage (replace-regexp-in-string "<ul class=\"org-ul\">" "<ul>" storage t t))
    (setq storage (replace-regexp-in-string "<ol class=\"org-ol\">" "<ol>" storage t t))
    (setq storage (replace-regexp-in-string "\n+" " " storage t t))
    (setq storage (replace-regexp-in-string "<p>[[:space:]]+" "<p>" storage t t))
    (setq storage (replace-regexp-in-string "[[:space:]]+</p>" "</p>" storage t t))
    (setq storage (replace-regexp-in-string "</p>[[:space:]]+<" "</p><" storage t t))
    (setq storage (replace-regexp-in-string "</ul>[[:space:]]+<" "</ul><" storage t t))
    (setq storage (replace-regexp-in-string "</ol>[[:space:]]+<" "</ol><" storage t t))
    (setq storage (replace-regexp-in-string "<ul>[[:space:]]+" "<ul>" storage t t))
    (setq storage (replace-regexp-in-string "[[:space:]]+</ul>" "</ul>" storage t t))
    (setq storage (replace-regexp-in-string "<ol>[[:space:]]+" "<ol>" storage t t))
    (setq storage (replace-regexp-in-string "[[:space:]]+</ol>" "</ol>" storage t t))
    (setq storage (replace-regexp-in-string ">[[:space:]]+</li>" "></li>" storage t t))
    (setq storage (replace-regexp-in-string "</li>[[:space:]]+<li" "</li><li" storage t t))
    (string-trim storage)))

(defun hub/confluence-commands--org-comment-body-to-storage (body)
  "Return Confluence storage XHTML for sidecar Org comment BODY.
Supported authoring syntax includes paragraphs, basic inline markup, links, and
plain Org ordered/unordered lists."
  (if (string-empty-p (string-trim (or body "")))
      ""
    (hub/confluence-commands--sanitize-comment-storage
     (org-export-string-as
      (string-trim body) 'html t
      '(:with-toc nil :section-numbers nil :with-title nil)))))

(defun hub/confluence-commands--current-sidecar-comment-body ()
  "Return body text for the current sidecar comment heading."
  (hub/org-comment--entry-body (save-excursion (org-end-of-subtree t t))))

(defun hub/confluence-commands--current-comment-sync-kind ()
  "Return current sidecar comment sync kind for push."
  (let ((kind (org-entry-get nil "HUB_COMMENT_SYNC_KIND"))
	(target (org-entry-get nil "HUB_COMMENT_TARGET_TEXT")))
    (cond
     ((member kind '("footer" "inline" "reply")) kind)
     (target "inline")
     (t nil))))

(defun hub/confluence-commands--root-comment-info ()
  "Return plist for nearest root sidecar comment ancestor."
  (save-excursion
    (while (> (org-outline-level) 1)
      (org-up-heading-safe))
    (list :remote-id (org-entry-get nil "HUB_COMMENT_REMOTE_ID")
	  :sync-kind (org-entry-get nil "HUB_COMMENT_SYNC_KIND"))))

(defun hub/confluence-commands--ensure-current-comment-pushable ()
  "Validate the current sidecar heading for Confluence comment push.
Return the comment sync kind, one of `footer', `inline', or `reply'."
  (unless (and buffer-file-name
	       (string-suffix-p ".comments.org" buffer-file-name))
    (user-error "Current buffer is not a comments sidecar"))
  (unless (derived-mode-p 'org-mode)
    (org-mode))
  (when (org-before-first-heading-p)
    (user-error "Point is not on a sidecar comment"))
  (org-back-to-heading t)
  (unless (org-entry-get nil "HUB_COMMENT_ID")
    (user-error "Point is not on a sidecar comment"))
  (when (and (> (org-outline-level) 1)
	     (not (equal "reply" (org-entry-get nil "HUB_COMMENT_SYNC_KIND"))))
    (user-error "Child comments must be marked with HUB_COMMENT_SYNC_KIND: reply"))
  (when (and (org-entry-get nil "HUB_COMMENT_REMOTE_ID")
	     (not (org-entry-get nil "HUB_COMMENT_LOCAL_UPDATED_AT")))
    (user-error "Comment is already linked to Confluence and has no local edits to update"))
  (when (equal "RESOLVED" (org-get-todo-state))
    (user-error "Refusing to push RESOLVED local comment; reopen it first with OPEN or TODO"))
  (when (string-empty-p (string-trim (hub/confluence-commands--current-sidecar-comment-body)))
    (user-error "Cannot push an empty sidecar comment"))
  (let ((sync-kind (or (hub/confluence-commands--current-comment-sync-kind)
		       (user-error "Comment is not marked as a footer/page, inline, or reply comment"))))
    (when (equal sync-kind "reply")
      (let* ((root (hub/confluence-commands--root-comment-info))
	     (parent-id (plist-get root :remote-id))
	     (parent-kind (plist-get root :sync-kind))
	     (stored-parent (org-entry-get nil "HUB_COMMENT_REMOTE_PARENT_ID")))
	(unless (member parent-kind '("footer" "inline"))
	  (user-error "Cannot determine Confluence endpoint for reply parent"))
	(unless parent-id
	  (user-error "Cannot push reply before parent comment has HUB_COMMENT_REMOTE_ID"))
	(when (and stored-parent (not (equal stored-parent parent-id)))
	  (user-error "Reply parent ID %s does not match ancestor remote ID %s"
		      stored-parent parent-id))))
    sync-kind))

(defun hub/confluence-commands--stamp-created-comment (comment seen-at sync-kind &optional parent-id)
  "Stamp current sidecar heading with created remote COMMENT metadata at SEEN-AT.
SYNC-KIND is stored as `HUB_COMMENT_SYNC_KIND'.  PARENT-ID, when non-nil, is
stored as `HUB_COMMENT_REMOTE_PARENT_ID'."
  (let ((remote-id (hub/confluence-commands--remote-comment-id comment))
	(author-name (hub/confluence-commands--remote-comment-author-name comment))
	(author-id (hub/confluence-commands--remote-comment-author-id comment))
	(created-at (hub/confluence-commands--remote-comment-created-at comment))
	(resolution-status (hub/confluence-commands--remote-comment-resolution-status comment)))
    (unless remote-id
      (user-error "Confluence create response did not include a comment id"))
    (org-entry-put nil "HUB_COMMENT_REMOTE_ID" remote-id)
    (org-entry-put nil "HUB_COMMENT_SOURCE" "confluence")
    (org-entry-put nil "HUB_COMMENT_SYNC_KIND" sync-kind)
    (when parent-id
      (org-entry-put nil "HUB_COMMENT_REMOTE_PARENT_ID" parent-id))
    (org-entry-put nil "HUB_COMMENT_REMOTE_LAST_SEEN_AT" seen-at)
    (when resolution-status
      (org-entry-put nil "HUB_COMMENT_REMOTE_RESOLUTION_STATUS" resolution-status))
    (when (equal sync-kind "inline")
      (hub/confluence-commands--stamp-inline-match-info)
      (hub/confluence-commands--stamp-remote-inline-anchor-state comment))
    (hub/confluence-commands--put-property-when-missing "HUB_COMMENT_REMOTE_AUTHOR_ID" author-id)
    (hub/confluence-commands--put-property-when-missing
     "HUB_COMMENT_REMOTE_AUTHOR_DISPLAY_NAME" author-name)
    (hub/confluence-commands--put-property-when-missing "HUB_COMMENT_REMOTE_CREATED_AT" created-at)
    remote-id))

(defun hub/confluence-commands--target-bounds-at-heading ()
  "Return current sidecar target bounds as a cons cell, or nil."
  (when-let* ((value (org-entry-get nil "HUB_COMMENT_TARGET"))
	      (parts (split-string value "[[:space:]]+" t)))
    (when (= 2 (length parts))
      (cons (string-to-number (car parts))
	    (string-to-number (cadr parts))))))

(defun hub/confluence-commands--inline-match-info (source-file target-text target-bounds)
  "Return strict inline match info for SOURCE-FILE TARGET-TEXT and TARGET-BOUNDS.
Signal `user-error' when the sidecar anchor is missing, stale, or ambiguous in a
way that prevents computing Confluence's text selection match index."
  (unless (hub/confluence-api--present-string-p target-text)
    (user-error "Inline comment is missing HUB_COMMENT_TARGET_TEXT"))
  (unless target-bounds
    (user-error "Inline comment is missing HUB_COMMENT_TARGET"))
  (unless (and source-file (file-readable-p source-file))
    (user-error "Cannot read source Org file for inline anchor preflight"))
  (with-current-buffer (find-file-noselect source-file)
    (let* ((matches (hub/org-comment--anchor-matches-for-text (current-buffer) target-text))
	   (index (cl-position target-bounds matches :test #'equal)))
      (unless index
	(user-error "Inline comment anchor is stale; reanchor before pushing"))
      (list :count (length matches) :index index))))

(defun hub/confluence-commands--inline-comment-properties ()
  "Return strict `inlineCommentProperties' for the current sidecar comment."
  (let* ((target-text (org-entry-get nil "HUB_COMMENT_TARGET_TEXT"))
	 (match-info (hub/confluence-commands--inline-match-info
		      (hub/confluence-commands--sidecar-source-file)
		      target-text
		      (hub/confluence-commands--target-bounds-at-heading))))
    `((textSelection . ,target-text)
      (textSelectionMatchCount . ,(plist-get match-info :count))
      (textSelectionMatchIndex . ,(plist-get match-info :index)))))

(defun hub/confluence-commands--stamp-inline-match-info ()
  "Store local inline target match diagnostics at the current heading."
  (let ((match-info (hub/confluence-commands--inline-match-info
		     (hub/confluence-commands--sidecar-source-file)
		     (org-entry-get nil "HUB_COMMENT_TARGET_TEXT")
		     (hub/confluence-commands--target-bounds-at-heading))))
    (org-entry-put nil "HUB_COMMENT_TARGET_MATCH_COUNT"
		   (number-to-string (plist-get match-info :count)))
    (org-entry-put nil "HUB_COMMENT_TARGET_MATCH_INDEX"
		   (number-to-string (plist-get match-info :index)))))

(defun hub/confluence-commands--comment-endpoint-for-kind (sync-kind)
  "Return Confluence REST endpoint for SYNC-KIND at point."
  (pcase sync-kind
    ("footer" "footer-comments")
    ("inline" "inline-comments")
    ("reply" (plist-get (hub/confluence-commands--reply-parent-info) :endpoint))))

(defun hub/confluence-commands--reply-parent-info ()
  "Return plist with reply parent remote id, sync kind, and endpoint."
  (let* ((root (hub/confluence-commands--root-comment-info))
	 (parent-kind (plist-get root :sync-kind))
	 (parent-id (plist-get root :remote-id)))
    (list :parent-id parent-id
	  :parent-kind parent-kind
	  :endpoint (pcase parent-kind
		      ("footer" "footer-comments")
		      ("inline" "inline-comments")))))

(defun hub/confluence-commands--remote-inline-anchor-confirmed-p (comment)
  "Return non-nil when COMMENT response includes Confluence inline anchor properties."
  (let ((properties (hub/confluence-commands--remote-inline-target-object comment)))
    (and (listp properties)
	 (or (hub/confluence-api--present-string-p
	      (alist-get 'inlineMarkerRef properties))
	     (hub/confluence-api--present-string-p
	      (alist-get 'inline-marker-ref properties))
	     (hub/confluence-api--present-string-p
	      (alist-get 'markerRef properties))
	     (hub/confluence-api--present-string-p
	      (alist-get 'marker-ref properties))
	     (hub/confluence-api--present-string-p
	      (alist-get 'inlineOriginalSelection properties))
	     (hub/confluence-api--present-string-p
	      (alist-get 'inline-original-selection properties))
	     (hub/confluence-api--present-string-p
	      (alist-get 'originalSelection properties))
	     (hub/confluence-api--present-string-p
	      (alist-get 'original-selection properties))))))

(defun hub/confluence-commands--stamp-remote-inline-anchor-state (comment)
  "Stamp remote inline anchor state from Confluence COMMENT response."
  (cond
   ((equal "dangling" (hub/confluence-commands--remote-comment-resolution-status comment))
    (org-entry-put nil "HUB_COMMENT_REMOTE_ANCHOR_STATE" "dangling"))
   ((hub/confluence-commands--remote-inline-anchor-confirmed-p comment)
    (org-entry-delete nil "HUB_COMMENT_REMOTE_ANCHOR_STATE"))
   (t
    (org-entry-put nil "HUB_COMMENT_REMOTE_ANCHOR_STATE" "unconfirmed"))))

(defun hub/confluence-commands--response-comment (response)
  "Return a Confluence comment alist parsed from REST RESPONSE."
  (hub/confluence-commands--json-alist
   (hub/confluence-commands--response-body response)))

(defun hub/confluence-commands--ensure-current-user-authored-comment (remote-comment)
  "Signal unless REMOTE-COMMENT was authored by the authenticated user."
  (let ((author-id (hub/confluence-commands--remote-comment-author-id remote-comment))
	(account-id (hub/confluence-commands--current-user-account-id)))
    (unless (and author-id (equal author-id account-id))
      (user-error "Refusing to update Confluence comment authored by %s as %s"
		  (or author-id "<unknown>") account-id))))

(defun hub/confluence-commands--stamp-updated-comment (comment seen-at)
  "Stamp current sidecar heading after remote COMMENT update at SEEN-AT."
  (org-entry-delete nil "HUB_COMMENT_LOCAL_UPDATED_AT")
  (org-entry-put nil "HUB_COMMENT_REMOTE_LAST_SEEN_AT" seen-at)
  (when-let* ((updated-at (hub/confluence-commands--remote-comment-updated-at comment)))
    (org-entry-put nil "HUB_COMMENT_REMOTE_UPDATED_AT" updated-at))
  (when-let* ((status (hub/confluence-commands--remote-comment-resolution-status comment)))
    (org-entry-put nil "HUB_COMMENT_REMOTE_RESOLUTION_STATUS" status))
  (when (equal "inline" (org-entry-get nil "HUB_COMMENT_SYNC_KIND"))
    (hub/confluence-commands--stamp-remote-inline-anchor-state comment)))

(defun hub/confluence-commands--update-current-comment (sync-kind storage seen-at)
  "Update current remote-linked comment of SYNC-KIND with STORAGE at SEEN-AT."
  (let* ((remote-id (org-entry-get nil "HUB_COMMENT_REMOTE_ID"))
	 (endpoint (hub/confluence-commands--comment-endpoint-for-kind sync-kind))
	 (current (hub/confluence-commands--response-comment
		   (hub/confluence-api--get-comment endpoint remote-id "storage")))
	 (version (hub/confluence-commands--remote-comment-version-number current)))
    (unless version
      (user-error "Confluence comment %s response did not include version.number" remote-id))
    (hub/confluence-commands--ensure-current-user-authored-comment current)
    (let* ((response (hub/confluence-api--update-comment endpoint remote-id storage (1+ version)))
	   (comment (hub/confluence-commands--response-comment response)))
      (hub/confluence-commands--stamp-updated-comment comment seen-at)
      remote-id)))

(defun hub/confluence-commands--sidecar-comment-id-at-point ()
  "Return remote Confluence comment ID from current sidecar heading."
  (unless (and buffer-file-name
	       (string-suffix-p ".comments.org" buffer-file-name))
    (user-error "Current buffer is not a comments sidecar"))
  (unless (derived-mode-p 'org-mode)
    (org-mode))
  (when (org-before-first-heading-p)
    (user-error "Point is not on a sidecar comment"))
  (org-back-to-heading t)
  (or (org-entry-get nil "HUB_COMMENT_REMOTE_ID")
      (user-error "Comment is not linked to Confluence")))

(defun hub/confluence-commands--active-source-comment ()
  "Return active sidecar comment record at point in the current source buffer."
  (let ((point (point)))
    (or (seq-find (lambda (comment)
		    (let ((start (plist-get comment :target-start))
			  (end (plist-get comment :target-end)))
		      (and start end (<= start point) (<= point end))))
		  (hub/org-comment-collect (current-buffer)))
	(user-error "No active sidecar comment at point"))))

(defun hub/confluence-commands--source-comment-id-at-point ()
  "Return remote Confluence comment ID for active source comment at point."
  (or (plist-get (hub/confluence-commands--active-source-comment) :remote-id)
      (user-error "Comment is not linked to Confluence")))

;;;###autoload
(defun hub/confluence-comment-open-current (&optional page-id comment-id)
  "Open current or explicit Confluence COMMENT-ID for PAGE-ID in a browser.
When COMMENT-ID is nil, use the current sidecar heading or active source comment."
  (interactive)
  (let* ((metadata (if (and buffer-file-name
			    (string-suffix-p ".comments.org" buffer-file-name))
		       (hub/confluence-commands--source-page-metadata-from-sidecar)
		     (list :page-id (or page-id
					(hub/confluence-api--page-id-from-buffer)
					(read-string "Confluence page ID: "))
			   :space (hub/confluence-api--keyword-from-buffer "CONFLUENCE_SPACE"))))
	 (id (or comment-id
		 (if (and buffer-file-name
			  (string-suffix-p ".comments.org" buffer-file-name))
		     (hub/confluence-commands--sidecar-comment-id-at-point)
		   (hub/confluence-commands--source-comment-id-at-point))))
	 (url (hub/confluence-api--comment-url
	       (plist-get metadata :page-id) id (plist-get metadata :space))))
    (browse-url url)
    url))

;;;###autoload
(defun hub/confluence-comment-push-current (&optional page-id)
  "Push the current local sidecar comment to Confluence.
This command works from inside a `.comments.org' sidecar, on a root local footer
or inline comment.  PAGE-ID, when non-nil, overrides the page ID read from the
sidecar source Org file."
  (interactive)
  (let* ((sync-kind (hub/confluence-commands--ensure-current-comment-pushable))
	 (sidecar-file buffer-file-name)
	 (heading (org-get-heading t t t t))
	 (body (hub/confluence-commands--current-sidecar-comment-body))
	 (storage (hub/confluence-commands--org-comment-body-to-storage body))
	 (reply-info (when (equal sync-kind "reply")
		       (hub/confluence-commands--reply-parent-info)))
	 (page-metadata (hub/confluence-commands--source-page-metadata-from-sidecar))
	 (id (or page-id (plist-get page-metadata :page-id)))
	 (space (plist-get page-metadata :space))
	 (seen-at (hub/confluence-commands--sync-timestamp))
	 (existing-remote-id (org-entry-get nil "HUB_COMMENT_REMOTE_ID"))
	 response comment remote-id action)
    (if existing-remote-id
	(setq remote-id (hub/confluence-commands--update-current-comment sync-kind storage seen-at)
	      action :updated)
      (setq response (pcase sync-kind
		       ("footer" (hub/confluence-api--create-footer-comment id storage))
		       ("inline" (hub/confluence-api--create-inline-comment
				  id storage
				  (hub/confluence-commands--inline-comment-properties)))
		       ("reply" (hub/confluence-api--create-comment-reply
				 (plist-get reply-info :endpoint)
				 (plist-get reply-info :parent-id)
				 storage)))
	    comment (hub/confluence-commands--response-comment response)
	    action :created)
      (setq remote-id
	    (condition-case error
		(hub/confluence-commands--stamp-created-comment
		 comment seen-at sync-kind (plist-get reply-info :parent-id))
	      (error
	       (let ((created-id (hub/confluence-commands--remote-comment-id comment)))
		 (if created-id
		     (user-error "Created Confluence comment %s, but failed to update sidecar: %s"
				 created-id (error-message-string error))
		   (signal (car error) (cdr error))))))))
    (let ((url (hub/confluence-api--comment-url id remote-id space)))
      (save-buffer)
      (hub/org-comment-refresh-sidecar-headings sidecar-file)
      (hub/org-comment-fold-sidecar-property-drawers)
      (kill-new url)
      (message "%s Confluence %s comment %s for %s; URL copied: %s"
	       (if (eq action :updated) "Updated" "Created")
	       sync-kind remote-id heading url)
      (list action 1 :remote-id remote-id :sync-kind sync-kind
	    :sidecar-file sidecar-file :url url))))

(defun hub/confluence-commands--maybe-resolve-people-and-refresh (sidecar-file directory)
  "Resolve people for DIRECTORY and refresh SIDECAR-FILE headings when enabled."
  (when hub/confluence-comment-import-resolve-people
    (condition-case error
	(progn
	  (hub/confluence-people-resolve directory)
	  (when (file-exists-p sidecar-file)
	    (hub/org-comment-refresh-sidecar-headings sidecar-file)))
      (error (message "Confluence people resolution skipped: %s" (error-message-string error))))))

(defun hub/confluence-commands--set-remote-present-properties (comment)
  "Set present-state properties for remote COMMENT at current Org heading."
  (org-entry-delete nil "HUB_COMMENT_REMOTE_STATE")
  (org-entry-put nil "HUB_COMMENT_REMOTE_LAST_SEEN_AT"
		 (hub/confluence-commands--sync-timestamp))
  (org-entry-delete nil "HUB_COMMENT_REMOTE_MISSING_AT")
  (if-let* ((status (hub/confluence-commands--remote-comment-resolution-status comment)))
      (org-entry-put nil "HUB_COMMENT_REMOTE_RESOLUTION_STATUS" status)
    (org-entry-delete nil "HUB_COMMENT_REMOTE_RESOLUTION_STATUS")))

(defun hub/confluence-commands--apply-remote-status-at-heading (comment report)
  "Apply COMMENT remote resolution status to current root heading and update REPORT."
  (let ((status (hub/confluence-commands--remote-comment-resolution-status comment))
	(local (or (org-get-todo-state) "OPEN"))
	(org-log-done nil)
	(org-inhibit-logging t))
    (pcase status
      ("resolved"
       (unless (equal local "RESOLVED")
	 (plist-put report :remote-resolved
		    (1+ (or (plist-get report :remote-resolved) 0)))
	 (when (equal local "TODO")
	   (plist-put report :todo-resolved
		      (cons (or (org-entry-get nil "HUB_COMMENT_REMOTE_ID") "<unknown>")
			    (plist-get report :todo-resolved))))
	 (org-todo "RESOLVED")))
      ("open"
       (when (equal local "RESOLVED")
	 (plist-put report :remote-reopened
		    (1+ (or (plist-get report :remote-reopened) 0)))
	 (org-todo "OPEN"))))))

(defun hub/confluence-commands--backfill-remote-metadata (sidecar-file comment &optional report)
  "Backfill/update sidecar metadata for existing remote COMMENT.
+REPORT is updated for TODO-impacting status changes when non-nil."
  (let ((remote-id (hub/confluence-commands--remote-comment-id comment)))
    (when (and remote-id (file-exists-p sidecar-file))
      (with-temp-buffer
	(insert-file-contents sidecar-file)
	(org-mode)
	(goto-char (point-min))
	(when (cl-loop while (re-search-forward org-heading-regexp nil t)
		       do (goto-char (match-beginning 0))
		       when (equal remote-id (org-entry-get nil "HUB_COMMENT_REMOTE_ID"))
		       return t
		       do (forward-line 1))
	  (let ((author-name (hub/confluence-commands--remote-comment-author-name comment))
		(author-id (hub/confluence-commands--remote-comment-author-id comment))
		(created-at (hub/confluence-commands--remote-comment-created-at comment)))
	    (hub/confluence-commands--put-property-when-missing "HUB_COMMENT_REMOTE_AUTHOR_ID" author-id)
	    (hub/confluence-commands--put-property-when-missing
	     "HUB_COMMENT_REMOTE_AUTHOR_DISPLAY_NAME" author-name)
	    (hub/confluence-commands--put-property-when-missing
	     "HUB_COMMENT_REMOTE_CREATED_AT" created-at)
	    (when (equal "inline" (org-entry-get nil "HUB_COMMENT_SYNC_KIND"))
	      (hub/confluence-commands--put-property-when-missing
	       "HUB_COMMENT_TARGET_TEXT"
	       (hub/confluence-commands--remote-inline-target-text comment)))
	    (hub/confluence-commands--set-remote-present-properties comment)
	    (unless (equal "reply" (org-entry-get nil "HUB_COMMENT_SYNC_KIND"))
	      (hub/confluence-commands--apply-remote-status-at-heading comment report))
	    (write-region (point-min) (point-max) sidecar-file nil 'silent)))))))

(defun hub/confluence-commands--import-remote-replies
    (sidecar-file source-file parent-comment endpoint body-format report)
  "Import remote replies under PARENT-COMMENT from ENDPOINT.
Return plist with `:imported', `:seen-ids', `:parent-id', and `:complete'."
  (let ((parent-id (hub/confluence-commands--remote-comment-id parent-comment))
	(imported 0)
	(seen-ids nil)
	(complete nil))
    (when parent-id
      (let ((replies (condition-case error
			 (prog1 (hub/confluence-commands--comment-results
				 (hub/confluence-api--list-comment-children parent-id endpoint body-format))
			   (setq complete t))
		       (error
			(message "Confluence reply import skipped for %s: %s"
				 parent-id (error-message-string error))
			nil))))
	(dolist (reply replies)
	  (hub/confluence-commands--cache-comment-author reply (file-name-directory source-file))
	  (when-let* ((remote-id (hub/confluence-commands--remote-comment-id reply)))
	    (push remote-id seen-ids)
	    (if (hub/confluence-commands--sidecar-has-remote-comment-p sidecar-file remote-id)
		(hub/confluence-commands--backfill-remote-metadata sidecar-file reply report)
	      (hub/confluence-commands--append-remote-reply-comment
	       sidecar-file source-file reply body-format parent-id)
	      (plist-put report :imported-ids
			 (cons remote-id (plist-get report :imported-ids)))
	      (plist-put report :imported-reply-ids
			 (cons remote-id (plist-get report :imported-reply-ids)))
	      (setq imported (1+ imported)))))))
    (list :imported imported :seen-ids seen-ids :parent-id parent-id :complete complete)))

(defun hub/confluence-commands--endpoint-sync-kind (endpoint)
  "Return sidecar sync kind for Confluence ENDPOINT."
  (pcase endpoint
    ("footer-comments" "footer")
    ("inline-comments" "inline")))

(defun hub/confluence-commands--mark-missing-at-heading ()
  "Mark current remote-linked Org heading as remote missing."
  (org-entry-put nil "HUB_COMMENT_REMOTE_STATE" "missing")
  (unless (org-entry-get nil "HUB_COMMENT_REMOTE_MISSING_AT")
    (org-entry-put nil "HUB_COMMENT_REMOTE_MISSING_AT"
		   (hub/confluence-commands--sync-timestamp))))

(defun hub/confluence-commands--reconcile-missing-roots
    (sidecar-file sync-kind seen-ids report)
  "Mark remote-linked root comments of SYNC-KIND absent from SEEN-IDS missing."
  (when (file-exists-p sidecar-file)
    (with-temp-buffer
      (insert-file-contents sidecar-file)
      (org-mode)
      (let ((changed nil))
	(goto-char (point-min))
	(while (re-search-forward org-heading-regexp nil t)
	  (goto-char (match-beginning 0))
	  (let ((remote-id (org-entry-get nil "HUB_COMMENT_REMOTE_ID"))
		(kind (org-entry-get nil "HUB_COMMENT_SYNC_KIND")))
	    (when (and remote-id
		       (equal kind sync-kind)
		       (not (member remote-id seen-ids)))
	      (when (equal (org-get-todo-state) "TODO")
		(plist-put report :todo-missing
			   (cons remote-id (plist-get report :todo-missing))))
	      (unless (equal (org-entry-get nil "HUB_COMMENT_REMOTE_STATE") "missing")
		(plist-put report :marked-missing (1+ (or (plist-get report :marked-missing) 0))))
	      (hub/confluence-commands--mark-missing-at-heading)
	      (setq changed t)))
	  (forward-line 1))
	(when changed
	  (write-region (point-min) (point-max) sidecar-file nil 'silent))))))

(defun hub/confluence-commands--reconcile-missing-replies
    (sidecar-file parent-id seen-ids report)
  "Mark remote-linked replies under PARENT-ID absent from SEEN-IDS missing."
  (when (and parent-id (file-exists-p sidecar-file))
    (with-temp-buffer
      (insert-file-contents sidecar-file)
      (org-mode)
      (let ((changed nil))
	(goto-char (point-min))
	(while (re-search-forward org-heading-regexp nil t)
	  (goto-char (match-beginning 0))
	  (let ((remote-id (org-entry-get nil "HUB_COMMENT_REMOTE_ID"))
		(parent (org-entry-get nil "HUB_COMMENT_REMOTE_PARENT_ID"))
		(kind (org-entry-get nil "HUB_COMMENT_SYNC_KIND")))
	    (when (and remote-id
		       (equal kind "reply")
		       (equal parent parent-id)
		       (not (member remote-id seen-ids)))
	      (unless (equal (org-entry-get nil "HUB_COMMENT_REMOTE_STATE") "missing")
		(plist-put report :marked-missing (1+ (or (plist-get report :marked-missing) 0))))
	      (hub/confluence-commands--mark-missing-at-heading)
	      (setq changed t)))
	  (forward-line 1))
	(when changed
	  (write-region (point-min) (point-max) sidecar-file nil 'silent))))))

(defun hub/confluence-commands--record-present-again (sidecar-file remote-id report)
  "Record REMOTE-ID present-again in REPORT when SIDECAR-FILE says missing."
  (when (and remote-id (file-exists-p sidecar-file))
    (with-temp-buffer
      (insert-file-contents sidecar-file)
      (org-mode)
      (goto-char (point-min))
      (when (cl-loop while (re-search-forward org-heading-regexp nil t)
		     do (goto-char (match-beginning 0))
		     when (equal remote-id (org-entry-get nil "HUB_COMMENT_REMOTE_ID"))
		     return (equal (org-entry-get nil "HUB_COMMENT_REMOTE_STATE") "missing")
		     do (forward-line 1))
	(plist-put report :present-again (1+ (or (plist-get report :present-again) 0)))))))

(defun hub/confluence-commands--import-report-message (report fallback)
  "Show concise import REPORT with FALLBACK message when it is empty."
  (let* ((root-ids (reverse (plist-get report :imported-root-ids)))
	 (reply-ids (reverse (plist-get report :imported-reply-ids)))
	 (lines (delq nil
		      (list
		       (when (or root-ids reply-ids)
			 (string-join
			  (delq nil
				(list
				 (when root-ids
				   (format "Imported roots: %s (%s)"
					   (length root-ids) (string-join root-ids ", ")))
				 (when reply-ids
				   (format "replies: %s (%s)"
					   (length reply-ids) (string-join reply-ids ", ")))))
			  "; "))
		       (unless (or root-ids reply-ids)
			 (when-let* ((count (plist-get report :imported)))
			   (when (> count 0)
			     (let ((ids (reverse (plist-get report :imported-ids))))
			       (if ids
				   (format "Imported new: %s (%s)" count (string-join ids ", "))
				 (format "Imported new: %s" count))))))
		       (when-let* ((count (plist-get report :marked-missing)))
			 (when (> count 0) (format "Remote missing: %s" count)))
		       (when-let* ((count (plist-get report :present-again)))
			 (when (> count 0) (format "Remote present again: %s" count)))
		       (when-let* ((count (plist-get report :remote-resolved)))
			 (when (> count 0) (format "Remote resolved locally: %s" count)))
		       (when-let* ((count (plist-get report :remote-reopened)))
			 (when (> count 0) (format "Remote reopened locally: %s" count)))
		       (when-let* ((items (plist-get report :todo-resolved)))
			 (when items (format "Local TODO closed by remote resolution: %s" (length items))))
		       (when-let* ((items (plist-get report :todo-missing)))
			 (when items (format "Local TODO now remote-missing: %s" (length items))))))))
    (message "%s" (if lines (string-join lines "; ") fallback))
    (when (or (plist-get report :todo-resolved)
	      (plist-get report :todo-missing))
      (with-current-buffer (get-buffer-create "*Confluence Comment Sync Report*")
	(let ((inhibit-read-only t))
	  (erase-buffer)
	  (insert "Confluence comment sync report\n\n")
	  (when-let* ((items (plist-get report :todo-resolved)))
	    (insert "* TODO closed by remote resolution\n")
	    (dolist (id (reverse items)) (insert "- " id "\n")))
	  (when-let* ((items (plist-get report :todo-missing)))
	    (insert "* TODO remote-missing\n")
	    (dolist (id (reverse items)) (insert "- " id "\n")))
	  (special-mode))))
    report))

(defun hub/confluence-commands--import-remote-comments
    (page-id endpoint body-format append-function &optional resolve-after report)
  "Import PAGE-ID comments from ENDPOINT using APPEND-FUNCTION.
BODY-FORMAT defaults to storage.  Return import report plist."
  (unless buffer-file-name
    (user-error "Current buffer is not visiting a file"))
  (let* ((source-file buffer-file-name)
	 (source-directory (file-name-directory source-file))
	 (sidecar-file (hub/org-comment-sidecar-path source-file))
	 (id (hub/confluence-commands--page-id-or-read page-id))
	 (format-name (or body-format "storage"))
	 (response (hub/confluence-api--list-page-comments id endpoint format-name))
	 (comments (hub/confluence-commands--comment-results response))
	 (sync-kind (hub/confluence-commands--endpoint-sync-kind endpoint))
	 (report (or report (list :imported 0)))
	 seen-ids)
    (dolist (comment comments)
      (hub/confluence-commands--cache-comment-author comment source-directory)
      (when-let* ((remote-id (hub/confluence-commands--remote-comment-id comment)))
	(push remote-id seen-ids)
	(hub/confluence-commands--record-present-again sidecar-file remote-id report)
	(if (hub/confluence-commands--sidecar-has-remote-comment-p sidecar-file remote-id)
	    (hub/confluence-commands--backfill-remote-metadata sidecar-file comment report)
	  (funcall append-function sidecar-file source-file comment format-name)
	  (plist-put report :imported (1+ (or (plist-get report :imported) 0)))
	  (plist-put report :imported-ids
		     (cons remote-id (plist-get report :imported-ids)))
	  (plist-put report :imported-root-ids
		     (cons remote-id (plist-get report :imported-root-ids))))
	(let ((reply-summary (hub/confluence-commands--import-remote-replies
			      sidecar-file source-file comment endpoint format-name report)))
	  (plist-put report :imported (+ (or (plist-get report :imported) 0)
					 (plist-get reply-summary :imported)))
	  (when (plist-get reply-summary :complete)
	    (hub/confluence-commands--reconcile-missing-replies
	     sidecar-file remote-id (plist-get reply-summary :seen-ids) report)))))
    (hub/confluence-commands--reconcile-missing-roots sidecar-file sync-kind seen-ids report)
    (when (file-exists-p sidecar-file)
      (hub/org-comment--anchor-imported-inline-comments (current-buffer) sidecar-file)
      (hub/org-comment-refresh-sidecar-headings sidecar-file))
    (when resolve-after
      (hub/confluence-commands--maybe-resolve-people-and-refresh sidecar-file source-directory))
    report))

;;;###autoload
(defun hub/confluence-comment-import-footer (&optional page-id body-format)
  "Import remote footer comments for PAGE-ID into the current Org sidecar.
This command is append-only and idempotent: existing remote Confluence comments
are detected by remote ID and are not overwritten.  BODY-FORMAT defaults to
storage, and the raw remote body is preserved verbatim."
  (interactive)
  (let ((report (hub/confluence-commands--import-remote-comments
		 page-id "footer-comments" body-format
		 #'hub/confluence-commands--append-remote-footer-comment t)))
    (hub/confluence-commands--import-report-message
     report "Imported 0 Confluence footer comments")
    (or (plist-get report :imported) 0)))

;;;###autoload
(defun hub/confluence-comment-import-inline (&optional page-id body-format)
  "Import remote inline comments for PAGE-ID into the current Org sidecar.
Imported inline comments are left unanchored until manually reanchored."
  (interactive)
  (let ((report (hub/confluence-commands--import-remote-comments
		 page-id "inline-comments" body-format
		 #'hub/confluence-commands--append-remote-inline-comment t)))
    (hub/confluence-commands--import-report-message
     report "Imported 0 Confluence inline comments")
    (or (plist-get report :imported) 0)))

;;;###autoload
(defun hub/confluence-comment-import (&optional page-id body-format)
  "Import remote footer and inline comments for PAGE-ID into the sidecar."
  (interactive)
  (let* ((id (hub/confluence-commands--page-id-or-read page-id))
	 (footer-count (hub/confluence-comment-import-footer id body-format))
	 (inline-count (hub/confluence-comment-import-inline id body-format)))
    (message "Imported %s footer and %s inline Confluence comments"
	     footer-count inline-count)
    (+ footer-count inline-count)))

(defun hub/confluence-commands--source-directory ()
  "Return current buffer file directory for people lookup, or nil."
  (when buffer-file-name
    (file-name-directory buffer-file-name)))

(defun hub/confluence-commands--user-account-id (user)
  "Return Confluence account ID from USER alist."
  (alist-get 'accountId user))

(defun hub/confluence-commands--user-display-name (user)
  "Return display name from Confluence USER alist."
  (or (alist-get 'displayName user)
      (alist-get 'publicName user)))

(defun hub/confluence-commands--current-user ()
  "Return authenticated Confluence user as an alist."
  (hub/confluence-commands--json-alist
   (hub/confluence-commands--response-body (hub/confluence-api--current-user))))

(defun hub/confluence-commands--resolve-people-in-files (files users)
  "Update people FILES from Confluence USERS and return resolved count."
  (let ((resolved 0))
    (dolist (user users resolved)
      (when-let* ((account-id (hub/confluence-commands--user-account-id user)))
	(dolist (file files)
	  (when (hub/confluence-people-update-identity
		 file account-id
		 (hub/confluence-commands--user-display-name user)
		 (alist-get 'email user)
		 (alist-get 'accountStatus user)
		 (alist-get 'timeZone user))
	    (setq resolved (1+ resolved))))))))

;;;###autoload
(defun hub/confluence-people-mark-current-user ()
  "Mark the authenticated Confluence user as the current user in people cache.
The identity is cached in the global people file and marked with
`HUB_PERSON_ME: t'."
  (interactive)
  (let* ((user (hub/confluence-commands--current-user))
	 (account-id (or (hub/confluence-commands--user-account-id user)
			 (user-error "Confluence current user response did not include accountId")))
	 (display-name (hub/confluence-commands--user-display-name user))
	 (email (alist-get 'email user))
	 (file (hub/confluence-people-mark-me account-id display-name email)))
    (message "Marked Confluence user %s as me in %s" (or display-name account-id) file)
    file))

;;;###autoload
(defun hub/confluence-people-resolve (&optional directory)
  "Resolve unresolved Confluence people in local/global people files.
DIRECTORY controls the local people file context.  Interactively, use the
current Org file's directory when available and the global people file otherwise."
  (interactive)
  (let* ((context-directory (or directory (hub/confluence-commands--source-directory)))
	 (files (hub/confluence-people-context-files context-directory))
	 (account-ids (hub/confluence-people-unresolved-account-ids files)))
    (if (not account-ids)
	(progn
	  (message "No unresolved Confluence people")
	  0)
      (let* ((users (hub/confluence-commands--user-results
		     (hub/confluence-api--users-bulk account-ids)))
	     (resolved (hub/confluence-commands--resolve-people-in-files files users))
	     (unresolved (- (length account-ids)
			    (length (delete-dups (mapcar #'hub/confluence-commands--user-account-id users))))))
	(message "Resolved %s Confluence people (%s unresolved)" resolved unresolved)
	resolved))))

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
