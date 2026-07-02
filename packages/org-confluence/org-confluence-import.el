;;; org-confluence-import.el --- Import Confluence storage XHTML to Org -*- lexical-binding: t; -*-

;;; Commentary:
;; Convert Confluence storage XHTML to a conservative Org representation.

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'seq)
(require 'subr-x)
(require 'xml)

(require 'org-confluence-api)
(require 'org-confluence-mentions)
(require 'org-confluence-process)
(require 'org-confluence-sync)

(defvar org-confluence-import--cdata-map nil
  "Alist mapping temporary CDATA tokens to original text bodies.")

(defvar org-confluence-import--directory nil
  "Directory used for resolving people while importing Confluence storage.")

(defun org-confluence-import--preserve-cdata (xhtml)
  "Return XHTML with CDATA bodies replaced by temporary text tokens."
  (let ((start 0)
	(index 0)
	(output ""))
    (setq org-confluence-import--cdata-map nil)
    (while (string-match "<!\\[CDATA\\[" xhtml start)
      (let* ((open-start (match-beginning 0))
	     (body-start (match-end 0))
	     (body-end (string-match "\\]\\]>" xhtml body-start)))
	(unless body-end
	  (setq body-end (length xhtml)))
	(let ((token (format "ORG_CONFLUENCE_CDATA_%d" index))
	      (body (substring xhtml body-start body-end)))
	  (setq output (concat output (substring xhtml start open-start) token))
	  (push (cons token body) org-confluence-import--cdata-map)
	  (setq index (1+ index)
		start (min (length xhtml) (+ body-end 3))))))
    (concat output (substring xhtml start))))

(defun org-confluence-import--node-tag (node)
  "Return NODE tag, or nil for text nodes."
  (when (consp node) (car node)))

(defun org-confluence-import--node-attributes (node)
  "Return NODE attributes."
  (when (consp node) (cadr node)))

(defun org-confluence-import--node-children (node)
  "Return NODE children."
  (when (consp node) (cddr node)))

(defun org-confluence-import--present-string (value)
  "Return trimmed VALUE when it is a non-empty string, or nil."
  (when (stringp value)
    (let ((trimmed (string-trim value)))
      (unless (string-empty-p trimmed)
	trimmed))))

(defun org-confluence-import--attribute (node attribute)
  "Return NODE ATTRIBUTE value."
  (cdr (assq attribute (org-confluence-import--node-attributes node))))

(defun org-confluence-import--macro-name (node)
  "Return Confluence macro name for NODE."
  (org-confluence-import--attribute node 'ac:name))

(defun org-confluence-import--macro-parameter (node name)
  "Return Confluence macro NODE parameter NAME, or nil."
  (seq-some (lambda (child)
	      (when (and (eq (org-confluence-import--node-tag child) 'ac:parameter)
			 (string= (or (org-confluence-import--attribute child 'ac:name) "") name))
		(org-confluence-import--inline child)))
	    (org-confluence-import--node-children node)))

(defun org-confluence-import--anchor-macro-p (node)
  "Return non-nil when NODE is a Confluence anchor macro."
  (and (eq (org-confluence-import--node-tag node) 'ac:structured-macro)
       (string= (or (org-confluence-import--macro-name node) "") "anchor")))

(defun org-confluence-import--anchor-macro-name (node)
  "Return Confluence anchor macro name from NODE, or nil."
  (when (org-confluence-import--anchor-macro-p node)
    (or (org-confluence-import--macro-parameter node "")
	(seq-some (lambda (child)
		    (when (eq (org-confluence-import--node-tag child) 'ac:default-parameter)
		      (org-confluence-import--inline child)))
		  (org-confluence-import--node-children node)))))

(defun org-confluence-import--status (node)
  "Convert Confluence status macro NODE to an Org status link."
  (let ((title (or (org-confluence-import--macro-parameter node "title") ""))
	(colour (or (org-confluence-import--macro-parameter node "colour") "Grey")))
    (format "[[confluence-status:%s][%s]]"
	    (string-trim colour)
	    (string-trim title))))

(defun org-confluence-import--emoticon-unicode-fallback (node)
  "Return known Unicode fallback for Confluence emoticon NODE."
  (let ((id (org-confluence-import--attribute node 'ac:emoji-id))
	(name (org-confluence-import--attribute node 'ac:name))
	(shortname (org-confluence-import--attribute node 'ac:emoji-shortname)))
    (cond
     ((or (string= (or id "") "atlassian-info")
	  (string= (or name "") "information")
	  (string= (or shortname "") ":info:"))
      "ℹ️")
     ((string= (or shortname "") ":document:")
      "📄"))))

(defun org-confluence-import--emoticon (node)
  "Convert Confluence emoticon NODE to readable Org text."
  (or (org-confluence-import--emoticon-unicode-fallback node)
      (org-confluence-import--attribute node 'ac:emoji-fallback)
      (org-confluence-import--attribute node 'ac:emoji-shortname)
      (org-confluence-import--attribute node 'ac:name)
      ""))

(defun org-confluence-import--cdata-text (text)
  "Return original CDATA for temporary TEXT token, or TEXT itself."
  (or (cdr (assoc text org-confluence-import--cdata-map)) text))

(defun org-confluence-import--user-mention (node &optional body)
  "Return Org link mention text for Confluence user NODE using optional BODY."
  (when-let* ((account-id (or (org-confluence-import--attribute node 'ri:account-id)
			      (org-confluence-import--attribute node 'ri:username)
			      (org-confluence-import--attribute node 'ri:userkey)))
	      (link (org-confluence-mentions-org-link
		     account-id org-confluence-import--directory body)))
    (format " %s " link)))

(defun org-confluence-import--callout-macro-p (node)
  "Return non-nil when NODE is a panel-like Confluence macro."
  (member (org-confluence-import--macro-name node)
	  '("info" "note" "warning" "tip" "panel")))

(defun org-confluence-import--rich-text-body (node)
  "Return Org text converted from NODE's rich text body."
  (org-confluence-import--join-blocks
   (mapcar (lambda (child)
	     (when (eq (org-confluence-import--node-tag child) 'ac:rich-text-body)
	       (org-confluence-import--block child)))
	   (org-confluence-import--node-children node))))

(defun org-confluence-import--callout (node)
  "Convert Confluence panel-like macro NODE to semantic Org callout."
  (let* ((type (org-confluence-import--macro-name node))
	 (title (org-confluence-import--macro-parameter node "title"))
	 (attributes (format "#+ATTR_CALLOUT: :type %s" type))
	 (body (org-confluence-import--rich-text-body node)))
    (when (and title (not (string-empty-p (string-trim title))))
      (setq attributes (format "%s :title %S" attributes (string-trim title))))
    (format "%s\n#+begin_callout\n%s\n#+end_callout" attributes body)))

(defun org-confluence-import--plain-text-body (node)
  "Return plain text macro body from NODE, or an empty string."
  (let ((body (or (seq-some (lambda (child)
			      (when (eq (org-confluence-import--node-tag child) 'ac:plain-text-body)
				(mapconcat #'org-confluence-import--inline
					   (org-confluence-import--node-children child) "")))
			    (org-confluence-import--node-children node))
		  "")))
    (or (cdr (assoc body org-confluence-import--cdata-map))
	(if (string-match "\\`\\[CDATA\\[\\(\\(?:.\\|
\\)*\\)\\]\\]\\'" body)
	    (match-string 1 body)
	  body))))

(defun org-confluence-import--code-macro (node)
  "Convert Confluence code macro NODE to an Org source block."
  (let ((language (org-confluence-import--macro-parameter node "language"))
	(body (org-confluence-import--plain-text-body node)))
    (format "#+begin_src%s\n%s\n#+end_src"
	    (if (and language (not (string-empty-p (string-trim language))))
		(format " %s" (string-trim language))
	      "")
	    body)))

(defun org-confluence-import--clean-inline-spacing (text)
  "Clean Org inline TEXT spacing introduced for markup boundaries."
  (let ((cleaned (replace-regexp-in-string "[[:space:]]+" " " text)))
    (setq cleaned (replace-regexp-in-string "[[:space:]]+\\([.,;:!?)]\\)" "\\1" cleaned))
    (string-trim cleaned)))

(defun org-confluence-import--markup (marker contents)
  "Return Org inline markup using MARKER around CONTENTS."
  (format " %s%s%s " marker (string-trim contents) marker))

(defun org-confluence-import--time (node)
  "Convert Confluence time NODE to an inactive Org date timestamp."
  (if-let* ((datetime (org-confluence-import--attribute node 'datetime))
	    (date (and (string-match "\\`[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}" datetime)
		       (match-string 0 datetime))))
      (format "[%s]" date)
    ""))

(defun org-confluence-import--inline (node)
  "Convert XHTML NODE to inline Org text."
  (cond
   ((stringp node) (org-confluence-import--cdata-text node))
   ((not (consp node)) "")
   (t
    (let ((contents (mapconcat #'org-confluence-import--inline
			       (org-confluence-import--node-children node) "")))
      (pcase (org-confluence-import--node-tag node)
	('ac:emoticon (org-confluence-import--emoticon node))
	('ac:structured-macro
	 (cond
	  ((org-confluence-import--anchor-macro-p node) "")
	  ((string= (or (org-confluence-import--macro-name node) "") "status")
	   (org-confluence-import--status node))
	  (t (org-confluence-import--clean-inline-spacing contents))))
	('ac:link
	 (let* ((anchor (org-confluence-import--attribute node 'ac:anchor))
		(user (seq-some (lambda (child)
				  (when (eq (org-confluence-import--node-tag child) 'ri:user)
				    child))
				(org-confluence-import--node-children node)))
		(link-body (org-confluence-import--clean-inline-spacing
			    (mapconcat
			     (lambda (child)
			       (when (memq (org-confluence-import--node-tag child)
					   '(ac:link-body ac:plain-text-link-body))
				 (org-confluence-import--inline child)))
			     (org-confluence-import--node-children node) ""))))
	   (cond
	    ((and anchor (string-match "\\`fn-\\(.+\\)\\'" anchor))
	     (format "[fn:%s]" (match-string 1 anchor)))
	    ((and anchor (string-match "\\`fnref-.+\\'" anchor)) "")
	    (user
	     (or (org-confluence-import--user-mention user link-body) ""))
	    (t (org-confluence-import--clean-inline-spacing contents)))))
	((or 'ac:link-body 'ac:plain-text-link-body 'sup)
	 (org-confluence-import--clean-inline-spacing contents))
	('ri:user (or (org-confluence-import--user-mention node) ""))
	('time (org-confluence-import--time node))
	((or 'strong 'b) (org-confluence-import--markup "*" contents))
	((or 'em 'i) (org-confluence-import--markup "/" contents))
	('u (org-confluence-import--markup "_" contents))
	('strike (org-confluence-import--markup "+" contents))
	('code (org-confluence-import--markup "~" contents))
	('a (let ((href (org-confluence-import--attribute node 'href)))
	      (if href (format "[[%s][%s]]" href (org-confluence-import--clean-inline-spacing contents))
		(org-confluence-import--clean-inline-spacing contents))))
	(_ (org-confluence-import--clean-inline-spacing contents)))))))

(defun org-confluence-import--join-blocks (blocks)
  "Join non-empty Org BLOCKS with newlines."
  (string-join (seq-filter (lambda (block)
			     (not (string-empty-p (string-trim (or block "")))))
			   blocks)
	       "\n"))

(defun org-confluence-import--list-child-p (node)
  "Return non-nil when NODE is a list element."
  (memq (org-confluence-import--node-tag node) '(ul ol)))

(defun org-confluence-import--list-item-text (item)
  "Return ITEM direct inline text, excluding nested lists."
  (org-confluence-import--clean-inline-spacing
   (mapconcat #'org-confluence-import--inline
	      (seq-remove #'org-confluence-import--list-child-p
			  (org-confluence-import--node-children item))
	      "")))

(defun org-confluence-import--indent-lines (text spaces)
  "Indent every line in TEXT by SPACES spaces."
  (let ((prefix (make-string spaces ?\s)))
    (string-join
     (mapcar (lambda (line) (concat prefix line))
	     (split-string text "\n"))
     "\n")))

(defun org-confluence-import--description-list-line (item indent)
  "Return Org description-list line for ITEM at INDENT, or nil."
  (let* ((children (seq-remove #'org-confluence-import--list-child-p
			       (org-confluence-import--node-children item)))
	 (first (car children)))
    (when (memq (org-confluence-import--node-tag first) '(strong b))
      (let ((term (org-confluence-import--clean-inline-spacing
		   (mapconcat #'org-confluence-import--inline
			      (org-confluence-import--node-children first) "")))
	    (rest (org-confluence-import--clean-inline-spacing
		   (mapconcat #'org-confluence-import--inline (cdr children) ""))))
	(when (string-suffix-p ":" term)
	  (format "%s- %s :: %s" indent (string-remove-suffix ":" term) rest))))))

(defun org-confluence-import--list (list-node &optional level)
  "Convert LIST-NODE to an Org list at nesting LEVEL."
  (let ((index 0)
	(level (or level 0))
	(ordered (eq (org-confluence-import--node-tag list-node) 'ol)))
    (org-confluence-import--join-blocks
     (mapcar
      (lambda (child)
	(when (eq (org-confluence-import--node-tag child) 'li)
	  (when ordered (setq index (1+ index)))
	  (let* ((marker (if ordered (format "%d." index) "-"))
		 (indent (make-string (* 2 level) ?\s))
		 (line (or (and (not ordered)
				(org-confluence-import--description-list-line child indent))
			   (format "%s%s %s" indent marker
				   (org-confluence-import--list-item-text child))))
		 (nested (org-confluence-import--join-blocks
			  (mapcar (lambda (nested-list)
				    (when (org-confluence-import--list-child-p nested-list)
				      (org-confluence-import--list nested-list (1+ level))))
				  (org-confluence-import--node-children child)))))
	    (org-confluence-import--join-blocks (list line nested)))))
      (org-confluence-import--node-children list-node)))))

(defun org-confluence-import--children-by-tag (node tag)
  "Return direct children of NODE with TAG."
  (seq-filter (lambda (child) (eq (org-confluence-import--node-tag child) tag))
	      (org-confluence-import--node-children node)))

(defun org-confluence-import--table-rows (table)
  "Return Confluence storage TABLE rows."
  (let ((direct-rows (org-confluence-import--children-by-tag table 'tr)))
    (if direct-rows
	direct-rows
      (apply #'append
	     (mapcar (lambda (section)
		       (org-confluence-import--children-by-tag section 'tr))
		     (seq-filter (lambda (child)
				   (memq (org-confluence-import--node-tag child) '(thead tbody)))
				 (org-confluence-import--node-children table)))))))

(defun org-confluence-import--table-row-cells (row)
  "Return ROW cells as Org-safe inline text."
  (mapcar (lambda (cell)
	    (string-trim
	     (replace-regexp-in-string "|" "\\\\vert{}"
				       (org-confluence-import--inline cell)
				       t t)))
	  (seq-filter (lambda (child)
			(memq (org-confluence-import--node-tag child) '(th td)))
		      (org-confluence-import--node-children row))))

(defun org-confluence-import--org-table-line (cells)
  "Return an Org table row for CELLS."
  (format "| %s |" (string-join cells " | ")))

(defun org-confluence-import--org-table-separator (cells)
  "Return an Org table separator sized for CELLS."
  (format "|%s|"
	  (string-join
	   (mapcar (lambda (cell)
		     (make-string (+ 2 (length cell)) ?-))
		   cells)
	   "+")))

(defun org-confluence-import--table (table)
  "Convert Confluence storage TABLE to an Org table."
  (let ((rows (mapcar #'org-confluence-import--table-row-cells
		      (org-confluence-import--table-rows table))))
    (org-confluence-import--join-blocks
     (append (list (org-confluence-import--org-table-line (car rows))
		   (org-confluence-import--org-table-separator (car rows)))
	     (mapcar #'org-confluence-import--org-table-line (cdr rows))))))

(defun org-confluence-import--footnote-definition-label (node)
  "Return footnote definition label when paragraph NODE starts with fn anchor."
  (when (eq (org-confluence-import--node-tag node) 'p)
    (seq-some (lambda (child)
		(when-let* ((anchor (org-confluence-import--anchor-macro-name child)))
		  (when (string-match "\\`fn-\\(.+\\)\\'" anchor)
		    (match-string 1 anchor))))
	      (org-confluence-import--node-children node))))

(defun org-confluence-import--footnote-definition (node label)
  "Convert footnote definition paragraph NODE with LABEL to Org."
  (let ((body (org-confluence-import--clean-inline-spacing
	       (mapconcat (lambda (child)
			    (cond
			     ((org-confluence-import--anchor-macro-p child) "")
			     ((and (eq (org-confluence-import--node-tag child) 'ac:link)
				   (string-match "\\`fnref-.+\\'"
						 (or (org-confluence-import--attribute child 'ac:anchor) "")))
			      "")
			     (t (org-confluence-import--inline child))))
			  (org-confluence-import--node-children node) ""))))
    (setq body (replace-regexp-in-string
		(format "\\`\\*%s\\.\\* ?" (regexp-quote label)) "" body))
    (format "[fn:%s] %s" label (string-trim body))))

(defun org-confluence-import--content-entity-id (node)
  "Return content entity id from Confluence link NODE, or nil."
  (when (eq (org-confluence-import--node-tag node) 'ac:link)
    (seq-some (lambda (child)
		(when (eq (org-confluence-import--node-tag child) 'ri:content-entity)
		  (org-confluence-import--attribute child 'ri:content-id)))
	      (org-confluence-import--node-children node))))

(defun org-confluence-import--subpage-placeholder (node)
  "Return Org property drawer for a subpage placeholder paragraph NODE."
  (seq-some (lambda (child)
	      (when-let* ((id (org-confluence-import--content-entity-id child)))
		(format ":PROPERTIES:\n:CONFLUENCE_PAGE_ID: %s\n:END:" id)))
	    (org-confluence-import--node-children node)))

(defun org-confluence-import--attachment-filename (node)
  "Return attachment filename from Confluence image NODE, or nil."
  (when (eq (org-confluence-import--node-tag node) 'ac:image)
    (seq-some (lambda (child)
		(when (eq (org-confluence-import--node-tag child) 'ri:attachment)
		  (org-confluence-import--attribute child 'ri:filename)))
	      (org-confluence-import--node-children node))))

(defun org-confluence-import--block (node)
  "Convert XHTML NODE to Org block text."
  (pcase (org-confluence-import--node-tag node)
    ((or 'html 'body 'ac:rich-text-body)
     (org-confluence-import--join-blocks
      (mapcar #'org-confluence-import--block
	      (org-confluence-import--node-children node))))
    ('ac:structured-macro
     (cond
      ((string= (or (org-confluence-import--macro-name node) "") "status")
       (org-confluence-import--status node))
      ((string= (or (org-confluence-import--macro-name node) "") "code")
       (org-confluence-import--code-macro node))
      ((org-confluence-import--callout-macro-p node)
       (org-confluence-import--callout node))
      (t (org-confluence-import--rich-text-body node))))
    ('h1 (format "* %s" (org-confluence-import--inline node)))
    ('h2 (format "** %s" (org-confluence-import--inline node)))
    ('h3 (format "*** %s" (org-confluence-import--inline node)))
    ('h4 (format "**** %s" (org-confluence-import--inline node)))
    ('p (cond
	 ((org-confluence-import--subpage-placeholder node))
	 ((if-let* ((label (org-confluence-import--footnote-definition-label node)))
	      (org-confluence-import--footnote-definition node label)))
	 (t (org-confluence-import--inline node))))
    ((or 'ul 'ol) (org-confluence-import--list node))
    ('table (org-confluence-import--table node))
    ('ac:image (if-let* ((filename (org-confluence-import--attachment-filename node)))
		   (format "[[./%s]]" filename)
		 ""))
    ('blockquote (format "#+begin_quote\n%s\n#+end_quote"
			 (org-confluence-import--join-blocks
			  (mapcar #'org-confluence-import--block
				  (org-confluence-import--node-children node)))))
    ('hr "-----")
    (_ (org-confluence-import--inline node))))

(defun org-confluence-import--restore-footnotes-heading (org)
  "Restore the Org Footnotes heading from imported ORG when appropriate."
  (replace-regexp-in-string "\\(?:\\`\\|\n\\)-----\n\\(\\[fn:[^]]+\\]\\)"
			    "\n* Footnotes\n\n\\1"
			    org t))

(defun org-confluence-import--line-kind (line)
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

(defun org-confluence-import--blank-before-p (previous-kind current-kind _next-kind)
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

(defun org-confluence-import--format-org (org)
  "Return imported ORG with stable blank lines between block constructs."
  (let* ((lines (split-string (string-trim org) "\n"))
	 (kinds (mapcar #'org-confluence-import--line-kind lines))
	 (in-verbatim nil)
	 output previous-kind)
    (cl-loop for line in lines
	     for kind in kinds
	     for index from 0
	     for next-kind = (nth (1+ index) kinds)
	     do
	     (when (and (not in-verbatim)
			(org-confluence-import--blank-before-p previous-kind kind next-kind)
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

(defun org-confluence-import-storage-to-org (xhtml &optional directory)
  "Convert Confluence storage XHTML string XHTML to Org text.
DIRECTORY, when non-nil, is used to resolve Confluence people labels."
  (let ((org-confluence-import--directory directory))
    (with-temp-buffer
      (insert (org-confluence-import--preserve-cdata xhtml))
      (let ((tree (libxml-parse-html-region (point-min) (point-max))))
	(org-confluence-import--format-org
	 (org-confluence-import--restore-footnotes-heading
	  (org-confluence-import--block tree)))))))

;;;###autoload
(defun org-confluence-pull (&optional page-id)
  "Refresh the current Org file from Confluence PAGE-ID.

When PAGE-ID is nil, default to #+CONFLUENCE_PAGE_ID in the current Org buffer
and prompt if no page ID is available.  The current buffer must visit a file;
that file is refreshed through `org-confluence-pull-to-file'."
  (interactive)
  (unless buffer-file-name
    (user-error "Current buffer is not visiting a file"))
  (let ((result (org-confluence-pull-to-file
		 (or page-id
		     (org-confluence-api--page-id-from-buffer)
		     (read-string "Confluence page ID: "))
		 buffer-file-name)))
    (revert-buffer :ignore-auto :noconfirm)
    result))


(provide 'org-confluence-import)
;;; org-confluence-import.el ends here
