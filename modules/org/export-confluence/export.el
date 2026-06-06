;;; export.el --- Org -> Confluence Storage Format (XHTML) export backend -*- lexical-binding: t; -*-

;;; Commentary:
;; Pure Org -> Confluence Storage Format export support.

;;; Code:

(require 'hub-org-callout)
(require 'org)
(require 'ox)
(require 'seq)
(require 'subr-x)
(require 'xml)

(defun org-confluence--trim (string)
  "Trim STRING, returning an empty string for nil."
  (string-trim (or string "")))

(defun org-confluence--join-lines (&rest parts)
  "Join non-empty XHTML PARTS with newlines."
  (string-join (seq-filter (lambda (part)
			     (not (string-empty-p (org-confluence--trim part))))
			   parts)
	       "\n"))

(defun org-confluence--plain-text (text _info)
  "Transcode plain TEXT to XHTML-safe text."
  (xml-escape-string text))

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

(defcustom org-confluence-image-max-width 760
  "Maximum Confluence image width in pixels.
This is emitted as `ac:width' because Confluence may ignore CSS max-width on
storage-format images."
  :type 'integer
  :group 'org-export)

(defconst org-confluence--image-extensions '("png" "jpg" "jpeg" "gif" "webp" "svg")
  "File extensions exported as Confluence attachment images.")

(defun org-confluence--local-image-link-p (link)
  "Return non-nil when LINK is a plain local image link."
  (let ((path (org-element-property :path link)))
    (and (string= (or (org-element-property :type link) "") "file")
	 (not (org-element-contents link))
	 (stringp path)
	 (member (downcase (or (file-name-extension path) ""))
		 org-confluence--image-extensions))))

(defun org-confluence--link (link contents _info)
  "Transcode LINK with CONTENTS to XHTML."
  (let* ((href (or (org-element-property :raw-link link)
		   (org-element-property :path link)
		   ""))
	 (label (org-confluence--trim (or contents href))))
    (format "<a href=\"%s\">%s</a>" (xml-escape-string href) label)))

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
  (let ((contents (seq-remove (lambda (element)
				(when (stringp element)
				  (string-empty-p (string-trim element))))
			      (org-element-contents paragraph))))
    (when (and (= (length contents) 1)
	       (eq (org-element-type (car contents)) 'link)
	       (org-confluence--local-image-link-p (car contents)))
      (car contents))))

(defun org-confluence--caption (element info)
  "Return ELEMENT caption text using INFO, or nil."
  (when-let* ((caption (org-element-property :caption element)))
    (org-confluence--trim (org-export-data caption info))))

(defun org-confluence--resolve-image-path (path)
  "Resolve local image PATH for the current Org buffer."
  (let ((absolute-path
	 (if (file-name-absolute-p path)
	     (expand-file-name path)
	   (unless buffer-file-name
	     (user-error "Cannot resolve relative Confluence image path in unsaved Org buffer: %s" path))
	   (expand-file-name path (file-name-directory buffer-file-name)))))
    (unless (file-exists-p absolute-path)
      (user-error "Missing Confluence image file: %s" absolute-path))
    absolute-path))

(defun org-confluence--hashed-image-filename (path)
  "Return a content-hashed Confluence attachment filename for PATH."
  (let* ((extension (file-name-extension path t))
	 (stem (file-name-sans-extension (file-name-nondirectory path)))
	 (hash (substring (secure-hash 'sha256 path) 0 12)))
    (format "%s-%s%s" stem hash extension)))

(defun org-confluence--image-asset (link)
  "Return an upload asset plist for image LINK."
  (let* ((source-path (org-confluence--resolve-image-path (org-element-property :path link)))
	 (filename (org-confluence--hashed-image-filename source-path)))
    (list :path source-path
	  :source-path source-path
	  :source-link (org-element-property :path link)
	  :filename filename)))

(defun org-confluence--validate-image-assets (assets)
  "Validate Confluence image ASSETS and return them."
  (let ((seen nil))
    (dolist (asset assets)
      (let ((filename (plist-get asset :filename)))
	(when (member filename seen)
	  (user-error "Duplicate Confluence attachment filename after hashing: %s" filename))
	(push filename seen))))
  assets)

(defun org-confluence-image-assets ()
  "Return local image assets referenced by the current Org buffer.
Each asset is a plist with `:path' as an absolute source file and `:filename'
as the Confluence attachment basename."
  (org-confluence--validate-image-assets
   (org-element-map (org-element-parse-buffer) 'paragraph
		    (lambda (paragraph)
		      (when-let* ((link (org-confluence--standalone-image-link paragraph)))
			(org-confluence--image-asset link))))))

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

(defun org-confluence--plain-list (plain-list _contents info)
  "Transcode PLAIN-LIST to XHTML using INFO.

Org can group adjacent unordered and ordered items into one `plain-list'.  Split
output whenever an item's bullet type changes so Confluence receives separate
`ul' and `ol' elements."
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
    (replace-regexp-in-string ">[[:space:]\n]+<" "><" (apply #'concat (nreverse result)))))

(defun org-confluence--horizontal-rule (_horizontal-rule _contents _info)
  "Transcode a horizontal rule to XHTML."
  "<hr/>")

(defun org-confluence--quote-block (_quote-block contents _info)
  "Transcode a quote block with CONTENTS to XHTML."
  (format "<blockquote>%s</blockquote>" (org-confluence--trim contents)))

(defun org-confluence--table-cell-text (cell info)
  "Return XHTML-safe text for table CELL using INFO."
  (org-confluence--trim (org-export-data (org-element-contents cell) info)))

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
      (let ((kind (hub/org-callout-type special-block "info"))
	    (title (hub/org-callout-title special-block)))
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
    (let ((body (org-confluence--trim contents)))
      (if (string-empty-p body) "" (format "<p>%s</p>" body)))))

(defun org-confluence--section (_section contents _info)
  "Transcode a section with CONTENTS to XHTML."
  (org-confluence--trim contents))

(defun org-confluence--headline (headline contents info)
  "Transcode HEADLINE and CONTENTS to XHTML using INFO."
  (let* ((level (min 4 (org-element-property :level headline)))
	 (title (org-export-data (org-element-property :title headline) info))
	 (heading (format "<h%d>%s</h%d>" level (org-confluence--trim title) level)))
    (org-confluence--join-lines heading contents)))

(defun org-confluence--template (contents _info)
  "Return exported document CONTENTS without a wrapper template."
  (org-confluence--trim contents))

(org-export-define-backend 'confluence
			   '((bold . org-confluence--bold)
			     (code . org-confluence--code)
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
			     (underline . org-confluence--underline)))

(defun org-confluence-export (&optional async subtreep visible-only body-only ext-plist)
  "Export current Org buffer to Confluence Storage Format XHTML.

ASYNC, SUBTREEP, VISIBLE-ONLY, BODY-ONLY, and EXT-PLIST are passed through to
`org-export-as'.  The default is body-only output suitable for `cfl --storage'."
  (org-confluence--trim
   (org-export-as 'confluence subtreep visible-only (or body-only t) ext-plist)))

(provide 'org/export-confluence)
;;; export.el ends here
