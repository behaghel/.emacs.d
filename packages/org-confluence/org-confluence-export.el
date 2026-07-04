;;; org-confluence-export.el --- Org to Confluence Storage Format backend -*- lexical-binding: t; -*-

;;; Commentary:
;; Pure Org -> Confluence Storage Format export support.

;;; Code:

(require 'org)
(require 'ol)
(require 'org-sync-assets)
(require 'ox)
(require 'seq)
(require 'subr-x)
(require 'xml)

(require 'org-confluence-mentions)

(defun org-confluence--trim (string)
  "Trim STRING, returning an empty string for nil."
  (string-trim (or string "")))

(defun org-confluence--callout-unquote (value)
  "Return VALUE without one layer of Org attribute string quoting."
  (cond
   ((not (stringp value)) value)
   ((string-match-p "\\`\".*\"\\'" value)
    (condition-case nil
	(car (read-from-string value))
      (error (string-trim value "\"" "\""))))
   (t value)))

(defun org-confluence--callout-attributes (special-block)
  "Return Confluence callout attributes for SPECIAL-BLOCK."
  (let* ((attr (org-export-read-attribute :attr_callout special-block))
	 (type (org-confluence--callout-unquote (plist-get attr :type)))
	 (title (org-confluence--callout-unquote (plist-get attr :title))))
    (list :type type :title title)))

(defun org-confluence--callout-type (special-block &optional default)
  "Return SPECIAL-BLOCK's callout type, or DEFAULT."
  (or (plist-get (org-confluence--callout-attributes special-block) :type) default))

(defun org-confluence--callout-title (special-block)
  "Return SPECIAL-BLOCK's callout title, or nil."
  (plist-get (org-confluence--callout-attributes special-block) :title))

(defun org-confluence--join-lines (&rest parts)
  "Join non-empty XHTML PARTS with newlines."
  (string-join (seq-filter (lambda (part)
			     (not (string-empty-p (org-confluence--trim part))))
			   parts)
	       "\n"))

(defun org-confluence--plain-text (text _info)
  "Transcode plain TEXT to XHTML-safe text."
  (xml-escape-string text))

(defun org-confluence--normalize-soft-wraps (text)
  "Replace soft paragraph line breaks in TEXT with spaces."
  (replace-regexp-in-string "[ \t]*\n[ \t]*" " " (org-confluence--trim text)))

(defun org-confluence--format-inline (tag contents)
  "Wrap CONTENTS in XHTML TAG when CONTENTS is non-empty."
  (let ((body (org-confluence--trim contents)))
    (if (string-empty-p body) "" (format "<%s>%s</%s>" tag body tag))))

(defun org-confluence--bold (_bold contents _info)
  "Transcode bold CONTENTS to XHTML."
  (org-confluence--format-inline "strong" contents))

(defun org-confluence--italic (_italic contents _info)
  "Transcode italic CONTENTS to XHTML."
  (org-confluence--format-inline "em" contents))

(defun org-confluence--code (code _contents _info)
  "Transcode inline CODE to XHTML."
  (format "<code>%s</code>" (xml-escape-string (org-element-property :value code))))

(defun org-confluence--strike-through (_strike-through contents _info)
  "Transcode strike-through CONTENTS to XHTML."
  (org-confluence--format-inline "strike" contents))

(defun org-confluence--underline (_underline contents _info)
  "Transcode underline CONTENTS to XHTML."
  (org-confluence--format-inline "u" contents))

(defun org-confluence--timestamp (timestamp _contents _info)
  "Transcode Org TIMESTAMP to a Confluence time element."
  (format "<time datetime=\"%04d-%02d-%02d\" />"
	  (org-element-property :year-start timestamp)
	  (org-element-property :month-start timestamp)
	  (org-element-property :day-start timestamp)))

(defcustom org-confluence-image-max-width 760
  "Maximum Confluence image width in pixels.
This is emitted as `ac:width' because Confluence may ignore CSS max-width on
storage-format images."
  :type 'integer
  :group 'org-export)

(defvar org-confluence--root-headline-begin nil
  "Buffer position of the root headline for the current subtree export.")

(defvar org-confluence--omit-root-heading nil
  "Non-nil means omit the current subtree root heading from export output.")

(org-link-set-parameters "confluence-status")

(defun org-confluence--local-image-link-p (link)
  "Return non-nil when LINK is a plain local image link."
  (org-sync-assets-image-link-p link))

(defun org-confluence--status (colour title)
  "Return Confluence storage XHTML for a status macro with COLOUR and TITLE."
  (format "<ac:structured-macro ac:name=\"status\" ac:schema-version=\"1\"><ac:parameter ac:name=\"title\">%s</ac:parameter><ac:parameter ac:name=\"colour\">%s</ac:parameter></ac:structured-macro>"
	  (xml-escape-string (org-confluence--trim title))
	  (xml-escape-string (org-confluence--trim colour))))

(defun org-confluence--link (link contents _info)
  "Transcode LINK with CONTENTS to XHTML."
  (let* ((type (or (org-element-property :type link) ""))
	 (path (or (org-element-property :path link) ""))
	 (href (or (org-element-property :raw-link link) path ""))
	 (label (org-confluence--trim (or contents href))))
    (cond
     ((string= type "confluence-status")
      (org-confluence--status path label))
     ((string= type "confluence-user")
      (or (org-confluence-mentions-storage-link path) ""))
     (t
      (format "<a href=\"%s\">%s</a>" (xml-escape-string href) label)))))

(defun org-confluence--image-filename (link info)
  "Return Confluence attachment filename for image LINK using INFO."
  (let* ((path (org-element-property :path link))
	 (filenames (plist-get info :confluence-image-filenames)))
    (or (cdr (assoc path filenames))
	(file-name-nondirectory path))))

(defun org-confluence--image (link info &optional caption)
  "Return Confluence storage XHTML for image LINK using INFO and CAPTION."
  (let* ((filename (org-confluence--image-filename link info))
	 (alt (org-confluence--trim caption))
	 (attributes (format " ac:width=\"%s\" ac:style=\"max-width: 100%%; height: auto;\""
			     org-confluence-image-max-width)))
    (unless (string-empty-p alt)
      (setq attributes (concat attributes
			       (format " ac:alt=\"%s\"" (xml-escape-string alt)))))
    (concat (format "<ac:image%s><ri:attachment ri:filename=\"%s\"/></ac:image>"
		    attributes
		    (xml-escape-string filename))
	    (unless (string-empty-p alt)
	      (format "\n<p><em>%s</em></p>" (xml-escape-string alt))))))

(defun org-confluence--standalone-image-link (paragraph)
  "Return PARAGRAPH's standalone local image link, or nil."
  (org-sync-assets-standalone-image-link paragraph))

(defun org-confluence--caption (element info)
  "Return ELEMENT caption text using INFO, or nil."
  (when-let* ((caption (org-element-property :caption element)))
    (org-confluence--trim (org-export-data caption info))))

(defun org-confluence--resolve-image-path (path)
  "Resolve local image PATH for the current Org buffer."
  (org-sync-assets-resolve-local-image path nil 'require-buffer-file))

(defun org-confluence--maybe-resolve-image-path (path)
  "Resolve local image PATH, or return nil when it does not exist."
  (org-sync-assets-maybe-resolve-local-image path nil 'require-buffer-file))

(defun org-confluence--hashed-attachment-filename-p (filename)
  "Return non-nil when FILENAME looks like a generated Confluence attachment."
  (org-sync-assets-hashed-filename-p filename))

(defun org-confluence--hashed-image-filename (path)
  "Return a content-hashed Confluence attachment filename for PATH."
  (org-sync-assets-hashed-filename path))

(defun org-confluence--image-asset (link)
  "Return an upload asset plist for image LINK."
  (org-sync-assets-entry
   (org-element-lineage link '(paragraph) t)
   link
   (list :reuse-imported-missing-source t
	 :require-buffer-file-for-relative t)))

(defun org-confluence--normalize-image-asset (asset)
  "Return Confluence-compatible image ASSET plist shape."
  (append (list :path (plist-get asset :source-path)
		:source-path (plist-get asset :source-path)
		:source-link (plist-get asset :source-link)
		:filename (plist-get asset :filename))
	  (when (plist-get asset :missing-source)
	    (list :missing-source t))))

(defun org-confluence--validate-image-assets (assets)
  "Validate Confluence image ASSETS and return them.

Repeated references to the same source image are allowed.  Signal only when a
single generated attachment filename maps to different source files."
  (let ((seen nil))
    (dolist (asset assets)
      (let ((filename (plist-get asset :filename))
	    (source-path (plist-get asset :source-path)))
	(when-let* ((previous-source-path (cdr (assoc filename seen))))
	  (unless (string= previous-source-path source-path)
	    (user-error "Duplicate Confluence attachment filename after hashing: %s" filename)))
	(push (cons filename source-path) seen))))
  assets)

(defun org-confluence-image-assets (&optional subtreep)
  "Return local image assets referenced by the current Org buffer.

When SUBTREEP is non-nil, inspect only the current Org subtree.  Each asset is
a plist with `:path' as an absolute source file and `:filename' as the
Confluence attachment basename."
  (let* ((plan (org-sync-assets-plan-buffer
		(list :subtreep subtreep
		      :reuse-imported-missing-source t
		      :require-buffer-file-for-relative t)))
	 (diagnostics (plist-get plan :diagnostics)))
    (when diagnostics
      (user-error "%s" (plist-get (car diagnostics) :message)))
    (org-confluence--validate-image-assets
     (mapcar #'org-confluence--normalize-image-asset
	     (plist-get plan :assets)))))

(defun org-confluence--compact-list-contents (contents)
  "Return CONTENTS formatted for inclusion in an XHTML list item."
  (let ((body (replace-regexp-in-string "[[:space:]\n]+" " "
					(org-confluence--trim contents))))
    (setq body (replace-regexp-in-string ">[[:space:]]+<" "><" body))
    (if (string-match "\\`<p>\\(.*?\\)</p>\\(.*\\)\\'" body)
	(concat (match-string 1 body) (match-string 2 body))
      body)))

(defun org-confluence--item (_item contents _info)
  "Transcode a list item with CONTENTS to XHTML."
  (format "<li>%s</li>" (org-confluence--compact-list-contents contents)))

(defun org-confluence--item-list-tag (item list-type)
  "Return XHTML list tag for ITEM, falling back to LIST-TYPE."
  (let ((bullet (org-element-property :bullet item)))
    (if (or (eq list-type 'ordered)
	    (and bullet (string-match-p "[[:digit:]]+[.)][[:space:]]*" bullet)))
	"ol"
      "ul")))

(defun org-confluence--definition-list (plain-list info)
  "Transcode descriptive PLAIN-LIST to a Confluence-safe list using INFO."
  (format "<ul>%s</ul>"
	  (mapconcat
	   (lambda (item)
	     (let ((term (org-confluence--trim
			  (org-export-data (org-element-property :tag item) info)))
		   (definition (org-confluence--compact-list-contents
				(org-export-data (org-element-contents item) info))))
	       (format "<li><strong>%s:</strong> %s</li>" term definition)))
	   (seq-filter (lambda (element) (eq (org-element-type element) 'item))
		       (org-element-contents plain-list))
	   "")))

(defun org-confluence--plain-list (plain-list _contents info)
  "Transcode PLAIN-LIST to XHTML using INFO.

Org can group adjacent unordered and ordered items into one `plain-list'.  Split
output whenever an item's bullet type changes so Confluence receives separate
`ul' and `ol' elements."
  (if (eq (org-element-property :type plain-list) 'descriptive)
      (org-confluence--definition-list plain-list info)
    (let ((list-type (org-element-property :type plain-list))
	  (current-tag nil)
	  (result nil))
      (dolist (item (seq-filter (lambda (element) (eq (org-element-type element) 'item))
				(org-element-contents plain-list)))
	(let ((tag (org-confluence--item-list-tag item list-type))
	      (item-xhtml (replace-regexp-in-string
			   ">[[:space:]\n]+<" "><"
			   (org-export-data item info))))
	  (unless (equal tag current-tag)
	    (when current-tag (push (format "</%s>" current-tag) result))
	    (push (format "<%s>" tag) result)
	    (setq current-tag tag))
	  (push item-xhtml result)))
      (when current-tag (push (format "</%s>" current-tag) result))
      (replace-regexp-in-string ">[[:space:]\n]+<" "><" (apply #'concat (nreverse result))))))

(defun org-confluence--anchor-macro (name)
  "Return Confluence storage anchor macro for NAME."
  (format "<ac:structured-macro ac:name=\"anchor\" ac:schema-version=\"1\"><ac:parameter ac:name=\"\">%s</ac:parameter></ac:structured-macro>"
	  (xml-escape-string name)))

(defun org-confluence--anchor-link (target body)
  "Return Confluence storage link to TARGET with rich link BODY."
  (format "<ac:link ac:anchor=\"%s\"><ac:link-body>%s</ac:link-body></ac:link>"
	  (xml-escape-string target)
	  body))

(defun org-confluence--footnote-reference (footnote-reference _contents _info)
  "Transcode FOOTNOTE-REFERENCE to a Confluence anchor link.

Use Confluence's documented anchor macro plus rich `ac:link-body'.  The rich
body avoids the visible CDATA leaks observed with `ac:plain-text-link-body' in
footnote-sized links."
  (let* ((label (org-element-property :label footnote-reference))
	 (reference-id (format "fnref-%s" label))
	 (definition-id (format "fn-%s" label)))
    (concat (org-confluence--anchor-macro reference-id)
	    (org-confluence--anchor-link
	     definition-id
	     (format "<sup>%s</sup>" (xml-escape-string label))))))

(defun org-confluence--footnote-definition (footnote-definition contents _info)
  "Transcode FOOTNOTE-DEFINITION with CONTENTS to Confluence-safe XHTML."
  (let* ((label (org-element-property :label footnote-definition))
	 (definition-id (format "fn-%s" label))
	 (reference-id (format "fnref-%s" label))
	 (anchor (org-confluence--anchor-macro definition-id))
	 (marker (format "<strong>%s.</strong> " (xml-escape-string label)))
	 (backlink (format " %s" (org-confluence--anchor-link reference-id "↩")))
	 (body (org-confluence--trim contents)))
    (if (string-match "\\`<p>\\(.*\\)</p>\\'" body)
	(format "<p>%s%s%s%s</p>" anchor marker (match-string 1 body) backlink)
      (format "<p>%s%s%s%s</p>" anchor marker body backlink))))

(defun org-confluence--horizontal-rule (_horizontal-rule _contents _info)
  "Transcode a horizontal rule to XHTML."
  "<hr/>")

(defun org-confluence--quote-block (_quote-block contents _info)
  "Transcode a quote block with CONTENTS to XHTML."
  (format "<blockquote>%s</blockquote>" (org-confluence--trim contents)))

(defun org-confluence--table-cell-repair-literal-bold (text)
  "Repair literal Org bold markup in table cell TEXT.

Org does not parse emphasis when a marker immediately follows emoji or other
non-whitespace text.  Pulled Confluence tables can contain cells such as an
emoji immediately followed by literal bold markup; repair those before pushing
back to Confluence."
  (let ((start 0))
    (while (string-match "\\(^\\|[^[:alnum:]<]\\)\\*\\([^*\n]+?\\)\\*" text start)
      (let ((replacement (format "%s<strong>%s</strong>"
				 (match-string 1 text)
				 (string-trim (match-string 2 text)))))
	(setq text (replace-match replacement t t text))
	(setq start (+ (match-beginning 0) (length replacement)))))
    text))

(defun org-confluence--table-cell-text (cell info)
  "Return XHTML-safe text for table CELL using INFO."
  (org-confluence--table-cell-repair-literal-bold
   (org-confluence--trim (org-export-data (org-element-contents cell) info))))

(defun org-confluence--table-row-cells (row tag info)
  "Return table ROW cells wrapped with TAG using INFO."
  (concat "<tr>"
	  (mapconcat (lambda (cell)
		       (format "<%s><p>%s</p></%s>" tag (org-confluence--table-cell-text cell info) tag))
		     (org-element-map row 'table-cell #'identity nil nil nil t)
		     "")
	  "</tr>"))

(defun org-confluence--table (table _contents info)
  "Transcode TABLE to Confluence storage XHTML using INFO."
  (let ((before-rule t)
	rows)
    (dolist (row (org-element-map table 'table-row #'identity nil nil nil t))
      (cond
       ((eq (org-element-property :type row) 'rule)
	(setq before-rule nil))
       (t
	(push (org-confluence--table-row-cells row (if before-rule "th" "td") info) rows))))
    (format "<table><tbody>%s</tbody></table>" (mapconcat #'identity (nreverse rows) ""))))

(defun org-confluence--cdata (text)
  "Return TEXT safe for use inside a CDATA body."
  (replace-regexp-in-string "]]>" "]]]]><![CDATA[>" (org-confluence--trim text) t t))

(defun org-confluence--src-block (src-block _contents _info)
  "Transcode SRC-BLOCK to a Confluence code macro."
  (let ((language (org-element-property :language src-block))
	(value (org-confluence--cdata (org-element-property :value src-block))))
    (concat "<ac:structured-macro ac:name=\"code\" ac:schema-version=\"1\">"
	    (when language
	      (format "<ac:parameter ac:name=\"language\">%s</ac:parameter>"
		      (xml-escape-string language)))
	    (format "<ac:plain-text-body><![CDATA[%s]]></ac:plain-text-body>" value)
	    "</ac:structured-macro>")))

(defun org-confluence--special-block (special-block contents _info)
  "Transcode SPECIAL-BLOCK with CONTENTS to XHTML."
  (if (string= (downcase (or (org-element-property :type special-block) "")) "callout")
      (let ((kind (org-confluence--callout-type special-block "info"))
	    (title (org-confluence--callout-title special-block)))
	(concat (format "<ac:structured-macro ac:name=\"%s\" ac:schema-version=\"1\">" (xml-escape-string kind))
		(when title
		  (format "<ac:parameter ac:name=\"title\">%s</ac:parameter>"
			  (xml-escape-string title)))
		(format "<ac:rich-text-body>%s</ac:rich-text-body>" (org-confluence--trim contents))
		"</ac:structured-macro>"))
    (org-confluence--trim contents)))

(defun org-confluence--paragraph (paragraph contents info)
  "Transcode PARAGRAPH with CONTENTS to XHTML using INFO."
  (if-let* ((image-link (org-confluence--standalone-image-link paragraph)))
      (org-confluence--image image-link info (org-confluence--caption paragraph info))
    (let ((body (org-confluence--normalize-soft-wraps contents)))
      (if (string-empty-p body) "" (format "<p>%s</p>" body)))))

(defun org-confluence--section (_section contents _info)
  "Transcode a section with CONTENTS to XHTML."
  (org-confluence--trim contents))

(defun org-confluence--subpage-placeholder (headline title level)
  "Return parent-page placeholder for subpage HEADLINE with TITLE at LEVEL."
  (let ((page-id (org-element-property :CONFLUENCE_PAGE_ID headline)))
    (org-confluence--join-lines
     (format "<h%d>%s</h%d>" level title level)
     (format "<p><ac:link><ri:content-entity ri:content-id=\"%s\"/><ac:link-body>Open subpage: %s</ac:link-body></ac:link></p>"
	     (xml-escape-string page-id)
	     title))))

(defun org-confluence--headline (headline contents info)
  "Transcode HEADLINE and CONTENTS to XHTML using INFO."
  (let* ((level (min 4 (org-element-property :level headline)))
	 (title (org-confluence--trim
		 (org-export-data (org-element-property :title headline) info)))
	 (begin (org-element-property :begin headline))
	 (rootp (and org-confluence--root-headline-begin
		     (= begin org-confluence--root-headline-begin)))
	 (subpagep (and (org-element-property :CONFLUENCE_PAGE_ID headline)
			(not rootp)))
	 (heading (cond
		   (subpagep (org-confluence--subpage-placeholder headline title level))
		   ((and rootp org-confluence--omit-root-heading) nil)
		   ((string= title "Footnotes") "<hr/>")
		   (t (format "<h%d>%s</h%d>" level title level)))))
    (if subpagep
	heading
      (org-confluence--join-lines heading contents))))

(defun org-confluence--template (contents _info)
  "Return exported document CONTENTS without a wrapper template."
  (replace-regexp-in-string "\n\\{2,\\}" "\n" (org-confluence--trim contents)))

(org-export-define-backend 'confluence
			   '((bold . org-confluence--bold)
			     (code . org-confluence--code)
			     (footnote-definition . org-confluence--footnote-definition)
			     (footnote-reference . org-confluence--footnote-reference)
			     (headline . org-confluence--headline)
			     (horizontal-rule . org-confluence--horizontal-rule)
			     (italic . org-confluence--italic)
			     (item . org-confluence--item)
			     (link . org-confluence--link)
			     (paragraph . org-confluence--paragraph)
			     (plain-list . org-confluence--plain-list)
			     (plain-text . org-confluence--plain-text)
			     (quote-block . org-confluence--quote-block)
			     (section . org-confluence--section)
			     (special-block . org-confluence--special-block)
			     (src-block . org-confluence--src-block)
			     (strike-through . org-confluence--strike-through)
			     (table . org-confluence--table)
			     (template . org-confluence--template)
			     (timestamp . org-confluence--timestamp)
			     (underline . org-confluence--underline))
			   :menu-entry
			   '(?C "Export to Confluence"
				((?C "Publish/create page" org-confluence-publish-from-export-dispatch)
				 (?O "Publish/create and open page" org-confluence-publish-and-open-from-export-dispatch)
				 (?X "To temporary XHTML buffer" org-confluence-export-as-xhtml)
				 (?x "To XHTML file" org-confluence-export-to-xhtml))))

(defun org-confluence-export (&optional async subtreep visible-only body-only ext-plist)
  "Export current Org buffer to Confluence Storage Format XHTML.

SUBTREEP, VISIBLE-ONLY, BODY-ONLY, and EXT-PLIST are passed through to
`org-export-as'.  The default is body-only output suitable for `cfl --storage'.
ASYNC is accepted for Org export-dispatch compatibility."
  (ignore async)
  (let ((org-confluence--root-headline-begin
	 (when subtreep
	   (save-excursion
	     (org-back-to-heading t)
	     (point))))
	(org-confluence--omit-root-heading
	 (plist-get ext-plist :confluence-omit-root-heading)))
    (replace-regexp-in-string
     "\n\\{2,\\}" "\n"
     (org-confluence--trim
      (org-export-as 'confluence subtreep visible-only (or body-only t) ext-plist)))))

(defun org-confluence-export-as-xhtml (&optional async subtreep visible-only body-only ext-plist)
  "Export current Org buffer or subtree to a temporary Confluence XHTML buffer.

ASYNC, SUBTREEP, VISIBLE-ONLY, BODY-ONLY, and EXT-PLIST follow Org export
conventions, so this command works from `org-export-dispatch'."
  (interactive)
  (org-export-to-buffer 'confluence "*Org Confluence Export*"
			async subtreep visible-only (or body-only t) ext-plist
			(lambda () (text-mode))))

(defun org-confluence-export-to-xhtml (&optional async subtreep visible-only body-only ext-plist)
  "Export current Org buffer or subtree to a Confluence .xhtml file.

ASYNC, SUBTREEP, VISIBLE-ONLY, BODY-ONLY, and EXT-PLIST follow Org export
conventions, so this command works from `org-export-dispatch'."
  (interactive)
  (let ((file (org-export-output-file-name ".xhtml" subtreep)))
    (org-export-to-file 'confluence file async subtreep visible-only (or body-only t) ext-plist)))

(provide 'org-confluence-export)
;;; org-confluence-export.el ends here
