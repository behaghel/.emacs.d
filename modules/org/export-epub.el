;;; export-epub.el --- Org EPUB export support -*- lexical-binding: t; -*-

;;; Commentary:
;; Controlled Org -> XHTML -> Pandoc EPUB export support.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'org)
(require 'ox)
(require 'ox-html)
(require 'subr-x)
(require 'ucs-normalize)

(defgroup hub/org-epub nil
  "Customizations for Org EPUB export."
  :group 'org)

(defcustom hub/org-epub-output-root
  (expand-file-name "~/Audiobooks/Hubert J. Behaghel/")
  "Root directory for final EPUB library artifacts."
  :type 'directory
  :group 'hub/org-epub)

(defcustom hub/org-epub-work-root
  (expand-file-name "var/org-epub" user-emacs-directory)
  "Root directory for generated EPUB intermediate artifacts."
  :type 'directory
  :group 'hub/org-epub)

(defcustom hub/org-epub-pandoc-executable nil
  "Optional Pandoc executable used for EPUB packaging.
When nil, discover Pandoc with `executable-find'."
  :type '(choice (const :tag "Discover automatically" nil) file)
  :group 'hub/org-epub)

(defcustom hub/org-epub-cover-target-bytes 300000
  "Target maximum byte size for optimized EPUB cover images."
  :type 'integer
  :group 'hub/org-epub)

(defcustom hub/org-epub-cover-max-geometry "1200x1800>"
  "ImageMagick resize geometry for EPUB cover optimization."
  :type 'string
  :group 'hub/org-epub)

(defconst hub/org-epub--reader-css-source
  (expand-file-name "etc/epub/reader.css"
		    (file-name-directory
		     (directory-file-name
		      (file-name-directory
		       (directory-file-name
			(file-name-directory (or load-file-name buffer-file-name)))))))
  "Tracked reader stylesheet copied into EPUB work directories.")

(defconst hub/org-epub--image-extensions
  '(".jpg" ".jpeg" ".png" ".gif" ".svg" ".webp")
  "File extensions treated as EPUB image assets.")

(defconst hub/org-epub--english-month-names
  '("January" "February" "March" "April" "May" "June"
    "July" "August" "September" "October" "November" "December")
  "English month names for locale-owned generated dates.")

(defconst hub/org-epub--french-month-names
  '("janvier" "février" "mars" "avril" "mai" "juin"
    "juillet" "août" "septembre" "octobre" "novembre" "décembre")
  "French month names for locale-owned generated dates.")

(defvar hub/org-epub--note-report-items nil
  "Note-related report items collected during the current EPUB export.")

(defvar hub/org-epub--preflight-items nil
  "Structured preflight items collected during the current EPUB export.")

(defvar hub/org-epub--permissive nil
  "Non-nil means degradable EPUB preflight items may continue export.")

(defvar hub/org-epub--language "en"
  "Language code for the current EPUB export.")

(defun hub/org-epub--keyword-value (keyword)
  "Return trimmed value for Org KEYWORD in the current buffer."
  (when-let* ((entry (assoc-string keyword (org-collect-keywords (list keyword)) t))
	      (value (car (cdr entry))))
    (string-trim value)))

(defun hub/org-epub--blank-string-p (value)
  "Return non-nil when VALUE is nil or blank."
  (or (not (stringp value))
      (string-empty-p (string-trim value))))

(defun hub/org-epub--source-stem ()
  "Return current buffer source filename stem, or nil."
  (when-let* ((file (buffer-file-name)))
    (file-name-sans-extension (file-name-nondirectory file))))

(defun hub/org-epub--denote-parts ()
  "Return parsed Denote filename parts for current buffer, or nil.
The returned plist contains `:identifier', `:title', and `:subjects'."
  (when-let* ((stem (hub/org-epub--source-stem)))
    (when (string-match "\\`\\([0-9]\\{8\\}T[0-9]\\{6\\}--[^_]+\\)\\(?:__\\(.+\\)\\)?\\'" stem)
      (let* ((identifier (match-string 1 stem))
	     (tags (match-string 2 stem))
	     (title-part (replace-regexp-in-string
			  "\\`[0-9]\\{8\\}T[0-9]\\{6\\}--" "" identifier))
	     (title (string-join
		     (mapcar #'capitalize
			     (split-string
			      (replace-regexp-in-string "[-_]+" " " title-part)
			      " " t))
		     " "))
	     (subjects (and tags (split-string tags "_" t))))
	(list :identifier identifier :title title :subjects subjects)))))

(defun hub/org-epub--keyword-subjects ()
  "Return EPUB subjects from standard Org `KEYWORDS'."
  (when-let* ((keywords (hub/org-epub--keyword-value "KEYWORDS")))
    (split-string keywords "[[:space:]]*,[[:space:]]*" t "[[:space:]]+")))

(defun hub/org-epub--date-components-from-org-date (date)
  "Return (YEAR MONTH DAY) parsed from Org DATE keyword text."
  (cond
   ((and (fboundp 'hub/org-export--date-components-from-iso-string)
	 (hub/org-export--date-components-from-iso-string date)))
   ((and (stringp date)
	 (string-match "\\([0-9]\\{4\\}\\)-\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\)" date))
    (list (string-to-number (match-string 1 date))
	  (string-to-number (match-string 2 date))
	  (string-to-number (match-string 3 date))))))

(defun hub/org-epub--iso-date-from-components (components)
  "Return ISO date string from date COMPONENTS."
  (pcase-let ((`(,year ,month ,day) components))
    (when (and year month day (<= 1 month) (<= month 12))
      (format "%04d-%02d-%02d" year month day))))

(defun hub/org-epub--denote-iso-date (denote)
  "Return ISO date inferred from DENOTE metadata."
  (when-let* ((identifier (plist-get denote :identifier)))
    (when (string-match "\\`\\([0-9]\\{4\\}\\)\\([0-9]\\{2\\}\\)\\([0-9]\\{2\\}\\)T" identifier)
      (format "%s-%s-%s"
	      (match-string 1 identifier)
	      (match-string 2 identifier)
	      (match-string 3 identifier)))))

(defun hub/org-epub--current-iso-date ()
  "Return today's date as an ISO string."
  (format-time-string "%Y-%m-%d"))

(defun hub/org-epub--format-date (date language)
  "Return locale-aware EPUB title-page DATE for LANGUAGE."
  (let ((components (hub/org-epub--date-components-from-org-date date)))
    (or (pcase-let ((`(,year ,month ,day) components))
	  (when (and year month day (<= 1 month) (<= month 12))
	    (pcase language
	      ("fr" (format "%s %s %s"
			    (if (= day 1) "1er" (number-to-string day))
			    (nth (1- month) hub/org-epub--french-month-names)
			    year))
	      (_ (format "%s %s, %s"
			 (nth (1- month) hub/org-epub--english-month-names)
			 day year)))))
	date)))

(defun hub/org-epub--metadata ()
  "Return EPUB metadata collected from the current Org buffer."
  (let* ((denote (hub/org-epub--denote-parts))
	 (title (or (hub/org-epub--keyword-value "TITLE")
		    (plist-get denote :title)))
	 (author-keyword (hub/org-epub--keyword-value "AUTHOR"))
	 (author-inferred (hub/org-epub--blank-string-p author-keyword))
	 (author (if author-inferred user-full-name author-keyword))
	 (identifier (or (plist-get denote :identifier)
			 (hub/org-epub--keyword-value "EPUB_IDENTIFIER")))
	 (subjects (delete-dups
		    (append (copy-sequence (plist-get denote :subjects))
			    (hub/org-epub--keyword-subjects))))
	 (cover (hub/org-epub--keyword-value "EPUB_COVER"))
	 (footer-note (hub/org-epub--keyword-value "EXPORT_FOOTER_NOTE"))
	 (date (hub/org-epub--keyword-value "DATE"))
	 (language (or (hub/org-epub--keyword-value "LANGUAGE") "en"))
	 (package-date (or (hub/org-epub--iso-date-from-components
			    (hub/org-epub--date-components-from-org-date date))
			   (hub/org-epub--denote-iso-date denote)
			   (hub/org-epub--current-iso-date))))
    (when (hub/org-epub--blank-string-p title)
      (user-error "Org EPUB export requires #+TITLE or a Denote title filename"))
    (when (hub/org-epub--blank-string-p author)
      (user-error "Org EPUB export requires #+AUTHOR or `user-full-name'"))
    (when (hub/org-epub--blank-string-p identifier)
      (user-error "Org EPUB export requires #+EPUB_IDENTIFIER when no Denote filename is available"))
    (list :title title
	  :author author
	  :author-inferred author-inferred
	  :identifier identifier
	  :subjects subjects
	  :cover cover
	  :footer-note footer-note
	  :date (hub/org-epub--format-date date language)
	  :package-date package-date
	  :language language)))

(defun hub/org-epub--sanitize-filename (name)
  "Return NAME as a readable filesystem component."
  (let ((sanitized (replace-regexp-in-string "[/:]+" " - " name)))
    (string-trim (replace-regexp-in-string "[[:space:]]+" " " sanitized))))

(defun hub/org-epub--timestamp ()
  "Return a compact timestamp for work directory names."
  (format-time-string "%Y%m%dT%H%M%S"))

(defun hub/org-epub--ensure-directory (directory)
  "Create DIRECTORY and return it."
  (make-directory directory t)
  directory)

(defun hub/org-epub--xhtml-document (title body)
  "Return an XHTML-compatible document with TITLE and BODY."
  (concat "<!doctype html>\n"
	  "<html xmlns=\"http://www.w3.org/1999/xhtml\">\n"
	  "<head>\n"
	  "<meta charset=\"utf-8\" />\n"
	  "<title>" (org-html-encode-plain-text title) "</title>\n"
	  "<link rel=\"stylesheet\" href=\"reader.css\" />\n"
	  "</head>\n"
	  "<body>\n"
	  body
	  "\n</body>\n"
	  "</html>\n"))

(defun hub/org-epub--write-file (file contents)
  "Write CONTENTS to FILE, creating parent directories."
  (make-directory (file-name-directory file) t)
  (with-temp-file file
    (insert contents))
  file)

(defun hub/org-epub--standfirst-blocks ()
  "Return standfirst block body strings from the current buffer."
  (org-element-map (org-element-parse-buffer) 'special-block
		   (lambda (block)
		     (when (equal (org-element-property :type block) "standfirst")
		       (string-trim
			(buffer-substring-no-properties
			 (org-element-property :contents-begin block)
			 (org-element-property :contents-end block)))))))

(defun hub/org-epub--standfirst ()
  "Return the single standfirst body, or nil.
Signal a `user-error' when more than one standfirst block exists."
  (let ((blocks (hub/org-epub--standfirst-blocks)))
    (when (> (length blocks) 1)
      (user-error "Org EPUB export supports only one standfirst block"))
    (car blocks)))

(defun hub/org-epub--without-standfirst-blocks ()
  "Return current buffer contents with standfirst blocks removed."
  (let ((contents (buffer-string))
	(regions (org-element-map (org-element-parse-buffer) 'special-block
				  (lambda (block)
				    (when (equal (org-element-property :type block) "standfirst")
				      (cons (org-element-property :begin block)
					    (org-element-property :end block)))))))
    (with-temp-buffer
      (insert contents)
      (dolist (region (sort regions (lambda (left right) (> (car left) (car right)))))
	(delete-region (car region) (cdr region)))
      (buffer-string))))

(defun hub/org-epub--content-paragraphs (contents)
  "Return paragraph strings from CONTENTS, excluding Org keyword lines."
  (split-string
   (string-join
    (cl-remove-if (lambda (line)
		    (string-match-p "\\`[[:space:]]*#\\+" line))
		  (split-string contents "\n"))
    "\n")
   "\n[[:space:]]*\n" t "[[:space:]\n]+"))

(defun hub/org-epub--ascii-fold (text)
  "Return TEXT folded to conservative ASCII for EPUB anchors."
  (let ((folded (ucs-normalize-NFD-string text)))
    (dolist (mapping '(("œ" . "oe") ("Œ" . "oe")
		       ("æ" . "ae") ("Æ" . "ae")
		       ("ß" . "ss")))
      (setq folded (replace-regexp-in-string (car mapping) (cdr mapping) folded t t)))
    (replace-regexp-in-string "[̀-ͯ]" "" folded)))

(defun hub/org-epub--slug (title)
  "Return a stable ASCII XHTML anchor slug for TITLE."
  (let* ((downcased (downcase (hub/org-epub--ascii-fold title)))
	 (slug (replace-regexp-in-string "[^a-z0-9]+" "-" downcased)))
    (string-trim slug "-+" "-+")))

(defun hub/org-epub--link-map (chapters)
  "Return an alist mapping headline titles in CHAPTERS to hrefs."
  (cl-loop for chapter in chapters
	   for index from 1
	   collect (cons (plist-get chapter :title)
			 (format "chapter-%d.xhtml#%s"
				 index
				 (hub/org-epub--slug (plist-get chapter :title))))))

(defun hub/org-epub--prepare-link-markup (text link-map)
  "Replace simple Org headline links in TEXT with HTML links from LINK-MAP."
  (let ((start 0)
	(output ""))
    (while (string-match "\\[\\[\\*\\([^]]+\\)\\]\\[\\([^]]+\\)\\]\\]" text start)
      (let* ((target (match-string 1 text))
	     (label (match-string 2 text))
	     (href (cdr (assoc target link-map))))
	(unless href
	  (user-error "Broken EPUB internal link: %s" target))
	(setq output
	      (concat output
		      (substring text start (match-beginning 0))
		      (format "@@html:<a href=\"%s\">%s</a>@@"
			      (org-html-encode-plain-text href)
			      (org-html-encode-plain-text label))))
	(setq start (match-end 0))))
    (concat output (substring text start))))

(defun hub/org-epub--plain-code-blocks (html)
  "Convert Org HTML source block wrappers in HTML to plain pre/code blocks."
  (let ((start 0)
	(output ""))
    (while (string-match "<div class=\"org-src-container\">[[:space:]\n]*<pre class=\"src src-\\([^\"]+\\)\">\\([[:ascii:][:nonascii:]]*?\\)</pre>[[:space:]\n]*</div>" html start)
      (setq output
	    (concat output
		    (substring html start (match-beginning 0))
		    (format "<pre><code class=\"language-%s\">%s</code></pre>"
			    (match-string 1 html)
			    (match-string 2 html))))
      (setq start (match-end 0)))
    (concat output (substring html start))))

(defun hub/org-epub--preflight-add (severity code message &rest properties)
  "Record preflight item with SEVERITY, CODE, MESSAGE, and PROPERTIES."
  (push `((severity . ,(symbol-name severity))
	  (code . ,(symbol-name code))
	  (message . ,message)
	  ,@properties)
	hub/org-epub--preflight-items))

(defun hub/org-epub--preflight-count (severity)
  "Return count of current preflight items matching SEVERITY."
  (cl-count (symbol-name severity) hub/org-epub--preflight-items
	    :key (lambda (item) (alist-get 'severity item))
	    :test #'equal))

(defun hub/org-epub--preflight-status ()
  "Return current preflight status string."
  (cond
   ((> (hub/org-epub--preflight-count 'error) 0) "failed")
   ((> (hub/org-epub--preflight-count 'warning) 0) "degraded")
   (t "ok")))

(defun hub/org-epub--preflight-report ()
  "Return JSON-ready current preflight report."
  `((schema . "hub-org-epub-preflight-v1")
    (status . ,(hub/org-epub--preflight-status))
    (counts . ((error . ,(hub/org-epub--preflight-count 'error))
	       (warning . ,(hub/org-epub--preflight-count 'warning))
	       (info . ,(hub/org-epub--preflight-count 'info))))
    (items . ,(vconcat (nreverse (copy-sequence hub/org-epub--preflight-items))))))

(defun hub/org-epub--write-preflight-json (work-dir)
  "Write structured preflight report into WORK-DIR."
  (let ((json-encoding-pretty-print t))
    (hub/org-epub--write-file
     (expand-file-name "preflight.json" work-dir)
     (json-encode (hub/org-epub--preflight-report)))))

(defun hub/org-epub--render-preflight-buffer ()
  "Render current preflight items in a human report buffer."
  (with-current-buffer (get-buffer-create "*Org EPUB Preflight*")
    (erase-buffer)
    (insert "Org EPUB Preflight\n\n")
    (insert (format "Status: %s\n" (hub/org-epub--preflight-status)))
    (dolist (item (nreverse (copy-sequence hub/org-epub--preflight-items)))
      (insert (format "- [%s] %s: %s\n"
		      (alist-get 'severity item)
		      (alist-get 'code item)
		      (alist-get 'message item))))
    (current-buffer)))

(defun hub/org-epub--remote-image-line-to-link (contents)
  "Degrade standalone remote image links in CONTENTS to ordinary links."
  (replace-regexp-in-string
   "^\\[\\[\\(https?://[^]]+\\.\\(?:jpg\\|jpeg\\|png\\|gif\\|svg\\|webp\\)\\)\\]\\]$"
   (lambda (match)
     (string-match "\\[\\[\\(.+\\)\\]\\]" match)
     (let ((target (match-string 1 match)))
       (hub/org-epub--preflight-add
	'warning
	'remote-image-degraded-to-link
	"Standalone remote images degrade to hyperlinks in permissive EPUB export."
	`(target . ,target))
       (format "[[%s][%s]]" target target)))
   contents))

(defun hub/org-epub--image-path-p (path)
  "Return non-nil when PATH names a supported image file."
  (member (downcase (or (file-name-extension path t) ""))
	  hub/org-epub--image-extensions))

(defun hub/org-epub--paragraph-link (paragraph)
  "Return sole link in PARAGRAPH, or nil."
  (let ((links (org-element-map paragraph 'link #'identity)))
    (and (= 1 (length links)) (car links))))

(defun hub/org-epub--caption-text (paragraph)
  "Return plain caption text for PARAGRAPH, or nil."
  (when-let* ((caption (org-element-property :caption paragraph)))
    (string-trim (org-element-interpret-data caption))))

(defun hub/org-epub--copy-image-assets (contents work-dir)
  "Copy standalone local image assets from CONTENTS into WORK-DIR.
Return an alist mapping original link paths to (COPIED-NAME . ALT)."
  (let (assets)
    (with-temp-buffer
      (insert contents)
      (org-mode)
      (let ((tree (org-element-parse-buffer)))
	(org-element-map tree 'link
			 (lambda (link)
			   (let ((type (org-element-property :type link))
				 (path (org-element-property :path link)))
			     (when (and (equal type "file")
					(not (hub/org-epub--image-path-p path)))
			       (user-error "EPUB export does not support local non-image links: %s" path)))))
	(org-element-map tree 'paragraph
			 (lambda (paragraph)
			   (when-let* ((link (hub/org-epub--paragraph-link paragraph)))
			     (let* ((type (org-element-property :type link))
				    (path (org-element-property :path link))
				    (raw-path (concat (and (equal type "file") "") path))
				    (described (org-element-property :contents-begin link))
				    (image (hub/org-epub--image-path-p path)))
			       (cond
				((and (member type '("http" "https")) image (not described))
				 (if hub/org-epub--permissive
				     (hub/org-epub--preflight-add
				      'warning
				      'remote-image-degraded-to-link
				      "Standalone remote images degrade to hyperlinks in permissive EPUB export."
				      `(target . ,path))
				   (user-error "EPUB export does not support standalone remote images: %s" path)))
				((and (equal type "file") (not image))
				 (user-error "EPUB export does not support local non-image links: %s" path))
				((and (equal type "file") image (not described))
				 (let* ((source (expand-file-name path default-directory))
					(name (file-name-nondirectory path))
					(attr (org-export-read-attribute :attr_epub paragraph))
					(alt (or (hub/org-epub--unquote (plist-get attr :alt))
						 (hub/org-epub--caption-text paragraph)
						 name)))
				   (unless (file-readable-p source)
				     (user-error "EPUB image is not readable: %s" source))
				   (copy-file source (expand-file-name name work-dir) t)
				   (push (cons path (cons name alt)) assets)))))))))
      assets)))

(defun hub/org-epub--postprocess-images (html asset-map)
  "Rewrite image references in HTML using ASSET-MAP."
  (dolist (asset asset-map)
    (let* ((original (regexp-quote (car asset)))
	   (name (org-html-encode-plain-text (cadr asset)))
	   (alt (org-html-encode-plain-text (cddr asset))))
      (setq html
	    (replace-regexp-in-string
	     (format "src=\"%s\" alt=\"[^\"]*\"" original)
	     (format "src=\"%s\" alt=\"%s\"" name alt)
	     html t))))
  html)

(defun hub/org-epub--record-note-degradations (contents)
  "Record note degradation report items found in CONTENTS."
  (with-temp-buffer
    (insert contents)
    (goto-char (point-min))
    (while (re-search-forward "^[[:space:]]*:HUB_NOTE_KIND:[[:space:]]*marginalia[[:space:]]*$" nil t)
      (push '((kind . "marginalia")
	      (status . "degraded-to-endnote")
	      (message . "Marginalia placement is degraded to an EPUB endnote."))
	    hub/org-epub--note-report-items)
      (hub/org-epub--preflight-add
       'warning
       'marginalia-degraded-to-endnote
       "Marginalia placement is degraded to an EPUB endnote."))))

(defun hub/org-epub--strip-note-kind-lines (contents)
  "Return CONTENTS without internal HUB_NOTE_KIND marker lines."
  (replace-regexp-in-string
   "^[[:space:]]*:HUB_NOTE_KIND:[[:space:]]*[^[:space:]\n]+[[:space:]]*\n?"
   ""
   contents))

(defun hub/org-epub--inline-footdefs (html)
  "Return HTML with Org footnote marker and body in one paragraph."
  (let ((start 0)
	(output "")
	(pattern "<div class=\"footdef\"><sup>\\(<a[^>]+>[^<]+</a>\\)</sup>[[:space:]\n]*<div class=\"footpara\" role=\"doc-footnote\"><p class=\"footpara\">[[:space:]\n]*\\([[:ascii:][:nonascii:]]*?\\)[[:space:]\n]*</p></div></div>"))
    (while (string-match pattern html start)
      (setq output
	    (concat output
		    (substring html start (match-beginning 0))
		    (format "<p class=\"footdef\"><sup>%s</sup> %s</p>"
			    (match-string 1 html)
			    (string-trim (match-string 2 html)))))
      (setq start (match-end 0)))
    (concat output (substring html start))))

(defun hub/org-epub--postprocess-footnotes (html)
  "Convert Org HTML footnotes in HTML to EPUB endnote hooks."
  (setq html (replace-regexp-in-string "fnr\\.\\([[:alnum:]_-]+\\)" "noteref-\\1" html))
  (setq html (replace-regexp-in-string "fn\\.\\([[:alnum:]_-]+\\)" "note-\\1" html))
  (setq html (replace-regexp-in-string "class=\"footnum\"" "class=\"note-backref\"" html))
  (setq html (replace-regexp-in-string "class=\"footref\"" "class=\"note-ref\"" html))
  (setq html (replace-regexp-in-string "<div id=\"footnotes\">" "<section class=\"endnotes\" epub:type=\"endnotes\">" html))
  (setq html (replace-regexp-in-string "<h2 class=\"footnotes\">Footnotes: </h2>" "<h2>Notes</h2>" html))
  (setq html (replace-regexp-in-string "<div id=\"text-footnotes\">" "" html))
  (setq html (hub/org-epub--inline-footdefs html))
  (replace-regexp-in-string "</div>[[:space:]\n]*</div>[[:space:]\n]*\\(</body>\\)" "</section>\n\\1" html))

(defun hub/org-epub--postprocess-html (html &optional asset-map)
  "Apply EPUB-specific postprocessing to exported HTML.
ASSET-MAP rewrites copied image paths and alt text."
  (setq html (replace-regexp-in-string "<hr[[:space:]]*/?>" "<hr class=\"section-break\" />" html))
  (setq html (replace-regexp-in-string "<b>" "<strong>" html))
  (setq html (replace-regexp-in-string "</b>" "</strong>" html))
  (setq html (replace-regexp-in-string "<i>" "<em>" html))
  (setq html (replace-regexp-in-string "</i>" "</em>" html))
  (setq html (hub/org-epub--postprocess-images html asset-map))
  (setq html (hub/org-epub--postprocess-footnotes html))
  (hub/org-epub--plain-code-blocks html))

(defun hub/org-epub--attribute (element attribute property)
  "Return ELEMENT ATTRIBUTE PROPERTY using Org export parsing."
  (plist-get (org-export-read-attribute attribute element) property))

(defun hub/org-epub--unquote (value)
  "Return VALUE without one layer of Org attribute quoting."
  (cond
   ((not (stringp value)) value)
   ((string-match-p "\\`\".*\"\\'" value)
    (condition-case nil
	(car (read-from-string value))
      (error (string-trim value "\"" "\""))))
   (t value)))

(defun hub/org-epub--raw-html-block (html)
  "Return Org raw HTML export block containing HTML."
  (concat "#+begin_export html\n" html "\n#+end_export\n"))

(defun hub/org-epub--inner-html (contents)
  "Return body-only HTML for CONTENTS without EPUB semantic transforms."
  (let ((org-export-with-toc nil)
	(org-export-with-section-numbers nil)
	(org-html-htmlize-output-type nil)
	(org-export-with-todo-keywords nil)
	(org-export-with-tags nil))
    (hub/org-epub--postprocess-html (org-export-string-as contents 'html t))))

(defun hub/org-epub--callout-type-label (type)
  "Return localized display label for callout TYPE."
  (pcase (list hub/org-epub--language type)
    (`("fr" "tip") "astuce")
    (`("fr" "warning") "attention")
    (`("fr" "important") "important")
    (`("fr" "note") "note")
    (_ (capitalize type))))

(defun hub/org-epub--callout-label (type title)
  "Return display label for callout TYPE with optional TITLE."
  (let ((type-label (hub/org-epub--callout-type-label type)))
    (cond
     ((and (equal type "note") title) title)
     ((equal type "note") type-label)
     (title (format "%s%s %s"
		    type-label
		    (if (equal hub/org-epub--language "fr") " :" ":")
		    title))
     (t type-label))))

(defun hub/org-epub--callout-html (block)
  "Return EPUB HTML for callout BLOCK."
  (let* ((type (or (hub/org-epub--unquote
		    (hub/org-epub--attribute block :attr_callout :type))
		   "note"))
	 (title (hub/org-epub--unquote
		 (hub/org-epub--attribute block :attr_callout :title)))
	 (allowed '("note" "tip" "warning" "important"))
	 (label (hub/org-epub--callout-label type title))
	 (contents (buffer-substring-no-properties
		    (org-element-property :contents-begin block)
		    (org-element-property :contents-end block))))
    (unless (member type allowed)
      (user-error "Unsupported EPUB callout type: %s" type))
    (format "<aside class=\"callout callout-%s\" epub:type=\"notice\">\n<hr class=\"callout-rule callout-rule-before\" />\n<p class=\"callout-label\"><strong>%s</strong></p>\n<div class=\"callout-body\">\n%s\n</div>\n<hr class=\"callout-rule callout-rule-after\" />\n</aside>"
	    (org-html-encode-plain-text type)
	    (org-html-encode-plain-text (upcase label))
	    (hub/org-epub--inner-html contents))))

(defun hub/org-epub--quote-html (block)
  "Return EPUB HTML for attributed quote BLOCK."
  (let ((author (hub/org-epub--unquote
		 (hub/org-epub--attribute block :attr_quote :author)))
	(contents (buffer-substring-no-properties
		   (org-element-property :contents-begin block)
		   (org-element-property :contents-end block))))
    (when (and author (not (string-empty-p (string-trim author))))
      (format "<figure class=\"quote\">\n<blockquote>\n%s\n</blockquote>\n<figcaption>— %s</figcaption>\n</figure>"
	      (hub/org-epub--inner-html contents)
	      (org-html-encode-plain-text author)))))

(defun hub/org-epub--metric-value (block)
  "Return EPUB metric value for BLOCK."
  (or (hub/org-epub--unquote (hub/org-epub--attribute block :attr_epub :value))
      (when-let* ((options (hub/org-epub--attribute block :attr_latex :options)))
	(when (string-match "\\`\\[\\(.+\\)\\]\\'" options)
	  (match-string 1 options)))))

(defun hub/org-epub--metric-html (block)
  "Return EPUB HTML for metric BLOCK."
  (let ((value (or (hub/org-epub--metric-value block) ""))
	(contents (buffer-substring-no-properties
		   (org-element-property :contents-begin block)
		   (org-element-property :contents-end block))))
    (format "<aside class=\"metric\">\n<p class=\"metric-value\">%s</p>\n<div class=\"metric-label\">\n%s\n</div>\n</aside>"
	    (org-html-encode-plain-text value)
	    (hub/org-epub--inner-html contents))))

(defun hub/org-epub--graph-html (block work-dir)
  "Return EPUB HTML for graph BLOCK, copying fallback image into WORK-DIR."
  (let* ((image (hub/org-epub--attribute block :attr_epub :image))
	 (alt (or (hub/org-epub--unquote
		   (hub/org-epub--attribute block :attr_epub :alt))
		  "Graph"))
	 (contents (buffer-substring-no-properties
		    (org-element-property :contents-begin block)
		    (org-element-property :contents-end block))))
    (when (and (not image) (string-match-p "#\\+begin_export[[:space:]]+latex" contents))
      (user-error "EPUB graph requires #+ATTR_EPUB: :image fallback for LaTeX-only graph"))
    (if image
	(let* ((source (expand-file-name image default-directory))
	       (name (file-name-nondirectory image)))
	  (unless (file-readable-p source)
	    (user-error "EPUB graph image is not readable: %s" source))
	  (copy-file source (expand-file-name name work-dir) t)
	  (format "<figure class=\"graph\">\n<img src=\"%s\" alt=\"%s\" />\n</figure>"
		  (org-html-encode-plain-text name)
		  (org-html-encode-plain-text alt)))
      (format "<aside class=\"graph-placeholder\">\n%s\n</aside>"
	      (hub/org-epub--inner-html contents)))))

(defun hub/org-epub--semantic-replacement (element work-dir)
  "Return raw HTML replacement for ELEMENT, or nil."
  (pcase (org-element-type element)
    ('quote-block (when-let* ((html (hub/org-epub--quote-html element)))
		    (hub/org-epub--raw-html-block html)))
    ('special-block
     (pcase (org-element-property :type element)
       ("callout" (hub/org-epub--raw-html-block (hub/org-epub--callout-html element)))
       ("metric" (hub/org-epub--raw-html-block (hub/org-epub--metric-html element)))
       ("graph" (hub/org-epub--raw-html-block (hub/org-epub--graph-html element work-dir)))
       (_ nil)))
    (_ nil)))

(defun hub/org-epub--semantic-replacements (work-dir)
  "Return semantic replacements for current buffer and WORK-DIR."
  (let (replacements)
    (org-element-map (org-element-parse-buffer) '(quote-block special-block)
		     (lambda (element)
		       (when-let* ((replacement (hub/org-epub--semantic-replacement element work-dir)))
			 (push (list (org-element-property :begin element)
				     (org-element-property :end element)
				     replacement)
			       replacements))))
    replacements))

(defun hub/org-epub--apply-semantic-replacements (contents work-dir)
  "Return CONTENTS with EPUB semantic blocks replaced for WORK-DIR."
  (with-temp-buffer
    (insert contents)
    (dolist (replacement (sort (hub/org-epub--semantic-replacements work-dir)
			       (lambda (left right) (> (car left) (car right)))))
      (delete-region (nth 0 replacement) (nth 1 replacement))
      (goto-char (nth 0 replacement))
      (insert (nth 2 replacement)))
    (buffer-string)))

(defun hub/org-epub--content-html (contents &optional link-map work-dir)
  "Return minimal XHTML content for CONTENTS.
LINK-MAP rewrites simple internal links.  WORK-DIR receives semantic assets."
  (let* ((transformed (if work-dir
			  (hub/org-epub--apply-semantic-replacements contents work-dir)
			contents))
	 (transformed (if hub/org-epub--permissive
			  (hub/org-epub--remote-image-line-to-link transformed)
			transformed))
	 (asset-map (and work-dir (hub/org-epub--copy-image-assets transformed work-dir)))
	 (prepared (hub/org-epub--prepare-link-markup
		    (hub/org-epub--strip-note-kind-lines transformed)
		    link-map))
	 (org-export-with-toc nil)
	 (org-export-with-section-numbers nil)
	 (org-html-htmlize-output-type nil)
	 (org-export-with-todo-keywords nil)
	 (org-export-with-tags nil)
	 (html (progn
		 (hub/org-epub--record-note-degradations transformed)
		 (org-export-string-as prepared 'html t))))
    (hub/org-epub--postprocess-html html asset-map)))

(defun hub/org-epub--body-html ()
  "Return minimal XHTML body content for the current Org buffer."
  (hub/org-epub--content-html (hub/org-epub--without-standfirst-blocks)))

(defun hub/org-epub--footnote-definitions (contents)
  "Return an alist of footnote definitions found in CONTENTS."
  (with-temp-buffer
    (insert contents)
    (org-mode)
    (org-element-map (org-element-parse-buffer) 'footnote-definition
		     (lambda (definition)
		       (cons (org-element-property :label definition)
			     (buffer-substring-no-properties
			      (org-element-property :begin definition)
			      (org-element-property :end definition)))))))

(defun hub/org-epub--footnote-labels (contents)
  "Return standard footnote reference labels used in CONTENTS."
  (with-temp-buffer
    (insert contents)
    (org-mode)
    (delete-dups
     (org-element-map (org-element-parse-buffer) 'footnote-reference
		      (lambda (reference)
			(when (eq (org-element-property :type reference) 'standard)
			  (org-element-property :label reference)))))))

(defun hub/org-epub--ensure-footnote-definitions (contents definitions)
  "Return CONTENTS with referenced footnote DEFINITIONS appended."
  (let ((missing
	 (cl-loop for label in (hub/org-epub--footnote-labels contents)
		  for definition = (cdr (assoc label definitions))
		  when (and definition
			    (not (string-match-p
				  (format "^[[:space:]]*\\[fn:%s\\]"
					  (regexp-quote label))
				  contents)))
		  collect definition)))
    (if missing
	(concat contents "\n\n" (string-join missing "\n"))
      contents)))

(defun hub/org-epub--footnotes-heading-p (title)
  "Return non-nil when TITLE names a conventional footnotes section."
  (member title '("Footnotes" "Notes" "Notes de bas de page")))

(defun hub/org-epub--publishable-preamble-p (contents)
  "Return non-nil when CONTENTS should become a visible Introduction."
  (with-temp-buffer
    (insert contents)
    (org-mode)
    (catch 'publishable
      (org-element-map (org-element-parse-buffer) '(paragraph plain-list verse-block quote-block)
		       (lambda (element)
			 (unless (org-element-lineage element '(special-block footnote-definition) t)
			   (throw 'publishable t))))
      nil)))

(defun hub/org-epub--body-plan ()
  "Return body split plan for the current Org buffer."
  (let* ((contents (hub/org-epub--without-standfirst-blocks))
	 (footnote-definitions (hub/org-epub--footnote-definitions contents))
	 headlines chapters)
    (with-temp-buffer
      (insert contents)
      (org-mode)
      (setq headlines
	    (org-element-map (org-element-parse-buffer) 'headline
			     (lambda (headline)
			       (let ((title (org-element-property :raw-value headline)))
				 (when (and (= (org-element-property :level headline) 1)
					    (not (hub/org-epub--footnotes-heading-p title)))
				   (list :title title
					 :begin (org-element-property :begin headline)
					 :contents-begin (org-element-property :contents-begin headline)
					 :end (org-element-property :end headline)))))))
      (dolist (headline headlines)
	(push (list :title (plist-get headline :title)
		    :contents (hub/org-epub--ensure-footnote-definitions
			       (buffer-substring-no-properties
				(or (plist-get headline :contents-begin) (plist-get headline :end))
				(plist-get headline :end))
			       footnote-definitions))
	      chapters)))
    (let* ((first (car headlines))
	   (preamble (if first
			 (substring contents 0 (1- (plist-get first :begin)))
		       contents)))
      (list :preamble preamble
	    :preamble-visible (hub/org-epub--publishable-preamble-p preamble)
	    :chapters (nreverse chapters)))))

(defun hub/org-epub--localized-label (language label)
  "Return localized LABEL for LANGUAGE."
  (pcase (list language label)
    (`("fr" contents) "Table des matières")
    (_ "Contents")))

(defun hub/org-epub--toc-html (toc-title chapters include-introduction)
  "Return visible TOC HTML with TOC-TITLE for CHAPTERS.
When INCLUDE-INTRODUCTION is non-nil, include the generated introduction page."
  (concat
   (format "<nav class=\"toc\" epub:type=\"toc\">\n<h1>%s</h1>\n<ol>\n"
	   (org-html-encode-plain-text toc-title))
   (when include-introduction
     "<li><a href=\"#introduction\">Introduction</a></li>\n")
   (cl-loop for chapter in chapters
	    concat (format "<li><a href=\"#%s\">%s</a></li>\n"
			   (org-html-encode-plain-text
			    (hub/org-epub--slug (plist-get chapter :title)))
			   (org-html-encode-plain-text (plist-get chapter :title))))
   "</ol>\n</nav>"))

(defun hub/org-epub--write-minimal-xhtml (work-dir metadata)
  "Write minimal XHTML files for METADATA into WORK-DIR.
Return the spine file list in reading order."
  (let* ((title (plist-get metadata :title))
	 (cover-page (expand-file-name "cover.xhtml" work-dir))
	 (titlepage (expand-file-name "titlepage.xhtml" work-dir))
	 (standfirst-page (expand-file-name "standfirst.xhtml" work-dir))
	 (toc (expand-file-name "toc.xhtml" work-dir))
	 (body (expand-file-name "body.xhtml" work-dir))
	 (footer-note-page (expand-file-name "footer-note.xhtml" work-dir))
	 (standfirst (hub/org-epub--standfirst))
	 (toc-title (hub/org-epub--localized-label (plist-get metadata :language) 'contents))
	 (body-plan (hub/org-epub--body-plan))
	 (chapters (plist-get body-plan :chapters))
	 (link-map (hub/org-epub--link-map chapters))
	 spine)
    (when-let* ((cover (plist-get metadata :cover)))
      (hub/org-epub--write-file
       cover-page
       (hub/org-epub--xhtml-document
	"Cover"
	(format "<section class=\"cover-page\">\n<img src=\"%s\" alt=\"Cover: %s\" />\n</section>"
		(org-html-encode-plain-text (file-name-nondirectory cover))
		(org-html-encode-plain-text title))))
      (push cover-page spine))
    (hub/org-epub--write-file
     titlepage
     (hub/org-epub--xhtml-document
      title
      (format "<section class=\"title-page\">\n<h1>%s</h1>\n<p class=\"author\">%s</p>%s\n</section>"
	      (org-html-encode-plain-text title)
	      (org-html-encode-plain-text (plist-get metadata :author))
	      (if-let* ((date (plist-get metadata :date)))
		  (format "\n<p class=\"date\">%s</p>" (org-html-encode-plain-text date))
		""))))
    (push titlepage spine)
    (when standfirst
      (hub/org-epub--write-file
       standfirst-page
       (hub/org-epub--xhtml-document
	"Standfirst"
	(format "<section class=\"standfirst-page\">\n<p>%s</p>\n</section>"
		(org-html-encode-plain-text standfirst))))
      (push standfirst-page spine))
    (hub/org-epub--write-file
     toc
     (hub/org-epub--xhtml-document
      toc-title
      (hub/org-epub--toc-html toc-title chapters (plist-get body-plan :preamble-visible))))
    (push toc spine)
    (if chapters
	(progn
	  (when (plist-get body-plan :preamble-visible)
	    (let ((introduction (expand-file-name "introduction.xhtml" work-dir)))
	      (hub/org-epub--write-file
	       introduction
	       (hub/org-epub--xhtml-document
		"Introduction"
		(concat "<section class=\"chapter introduction\">\n<h1 id=\"introduction\">Introduction</h1>\n"
			(hub/org-epub--content-html (plist-get body-plan :preamble) link-map work-dir)
			"\n</section>")))
	      (push introduction spine)))
	  (cl-loop for chapter in chapters
		   for index from 1
		   do (let* ((chapter-file (expand-file-name (format "chapter-%d.xhtml" index) work-dir))
			     (chapter-title (plist-get chapter :title))
			     (anchor (hub/org-epub--slug chapter-title)))
			(hub/org-epub--write-file
			 chapter-file
			 (hub/org-epub--xhtml-document
			  chapter-title
			  (format "<section class=\"chapter\">\n<h1 id=\"%s\">%s</h1>\n%s\n</section>"
				  (org-html-encode-plain-text anchor)
				  (org-html-encode-plain-text chapter-title)
				  (hub/org-epub--content-html (plist-get chapter :contents) link-map work-dir))))
			(push chapter-file spine))))
      (hub/org-epub--write-file
       body
       (hub/org-epub--xhtml-document title (hub/org-epub--content-html (hub/org-epub--without-standfirst-blocks) nil work-dir)))
      (push body spine))
    (when-let* ((footer-note (plist-get metadata :footer-note)))
      (hub/org-epub--write-file
       footer-note-page
       (hub/org-epub--xhtml-document
	"Footer note"
	(format "<section class=\"footer-note-page\">\n<p>%s</p>\n</section>"
		(org-html-encode-plain-text footer-note))))
      (push footer-note-page spine))
    (nreverse spine)))

(defun hub/org-epub--pandoc-executable ()
  "Return the Pandoc executable path or command name."
  (or hub/org-epub-pandoc-executable
      (executable-find "pandoc")
      (user-error "Pandoc executable not found; customize `hub/org-epub-pandoc-executable' or install Pandoc in the Emacs runtime environment")))

(defun hub/org-epub--call-pandoc (spine output-file metadata work-dir)
  "Package SPINE XHTML files into OUTPUT-FILE using Pandoc.
METADATA supplies EPUB package metadata and WORK-DIR is the resource root."
  (let* ((program (hub/org-epub--pandoc-executable))
	 (cover (plist-get metadata :cover))
	 (spine-inputs (if cover
			   (cl-remove-if (lambda (file)
					   (equal (file-name-nondirectory file) "cover.xhtml"))
					 spine)
			 spine))
	 (args (append (list "--from" "html" "--to" "epub3"
			     "--epub-title-page=false"
			     "--split-level" "1"
			     "--metadata" (concat "title=" (plist-get metadata :title))
			     "--metadata" (concat "author=" (plist-get metadata :author))
			     "--metadata" (concat "identifier=" (plist-get metadata :identifier))
			     "--metadata" (concat "date=" (plist-get metadata :package-date))
			     "--metadata" (concat "language=" (plist-get metadata :language))
			     "--resource-path" work-dir
			     "--css" "reader.css")
		       (cl-loop for subject in (plist-get metadata :subjects)
				append (list "--metadata" (concat "subject=" subject)))
		       (when cover (list "--epub-cover-image" cover))
		       (mapcar #'file-name-nondirectory spine-inputs)
		       (list "-o" output-file)))
	 (exit-code (let ((default-directory work-dir))
		      (apply #'process-file program nil nil nil args))))
    (unless (zerop exit-code)
      (user-error "Pandoc EPUB export failed with exit code %s" exit-code))))

(defun hub/org-epub--confirm-overwrite (file)
  "Ask before overwriting FILE in interactive Emacs sessions."
  (when (and (file-exists-p file)
	     (not noninteractive)
	     (not (y-or-n-p (format "Overwrite existing EPUB %s? " file))))
    (user-error "EPUB export cancelled")))

(defun hub/org-epub--metadata-json-alist (metadata epub-file work-dir)
  "Return JSON-ready alist for METADATA, EPUB-FILE, and WORK-DIR."
  `((schema . "hub-org-epub-metadata-v1")
    (title . ,(plist-get metadata :title))
    (author . ,(plist-get metadata :author))
    (author_inferred . ,(if (plist-get metadata :author-inferred) t :json-false))
    (identifier . ,(plist-get metadata :identifier))
    (subjects . ,(vconcat (plist-get metadata :subjects)))
    (cover . ,(or (plist-get metadata :cover) :json-false))
    (epub_file . ,epub-file)
    (date . ,(or (plist-get metadata :date) :json-false))
    (package_date . ,(plist-get metadata :package-date))
    (work_dir . ,work-dir)))

(defun hub/org-epub--copy-reader-css (work-dir)
  "Copy tracked reader CSS into WORK-DIR."
  (unless (file-readable-p hub/org-epub--reader-css-source)
    (user-error "EPUB reader CSS is not readable: %s" hub/org-epub--reader-css-source))
  (copy-file hub/org-epub--reader-css-source
	     (expand-file-name "reader.css" work-dir)
	     t))

(defun hub/org-epub--image-magick-executable ()
  "Return the ImageMagick executable for cover optimization, or nil."
  (or (executable-find "magick")
      (executable-find "convert")))

(defun hub/org-epub--cover-size (file)
  "Return byte size of FILE, or nil when FILE is missing."
  (when (file-exists-p file)
    (file-attribute-size (file-attributes file))))

(defun hub/org-epub--run-cover-conversion (program source output quality)
  "Use ImageMagick PROGRAM to convert SOURCE to OUTPUT at QUALITY."
  (let ((args (list source
		    "-auto-orient"
		    "-resize" hub/org-epub-cover-max-geometry
		    "-background" "white"
		    "-alpha" "remove"
		    "-alpha" "off"
		    "-strip"
		    "-interlace" "Plane"
		    "-quality" (number-to-string quality)
		    output)))
    (with-temp-buffer
      (zerop (apply #'process-file program nil t nil args)))))

(defun hub/org-epub--optimized-cover-or-copy (source target-dir)
  "Return optimized cover copied from SOURCE into TARGET-DIR.
Fall back to a plain copy when ImageMagick cannot process SOURCE."
  (let ((program (hub/org-epub--image-magick-executable))
	(best nil)
	(best-size nil))
    (if program
	(progn
	  (catch 'optimized
	    (dolist (quality '(82 72 62 52 42 32))
	      (let ((candidate (expand-file-name (format "cover-q%d.jpg" quality) target-dir)))
		(when (hub/org-epub--run-cover-conversion program source candidate quality)
		  (let ((size (hub/org-epub--cover-size candidate)))
		    (when (and size (or (not best-size) (< size best-size)))
		      (setq best candidate
			    best-size size))
		    (when (and size (<= size hub/org-epub-cover-target-bytes))
		      (setq best candidate
			    best-size size)
		      (throw 'optimized best)))))))
	  (if best
	      (let ((target (expand-file-name "cover.jpg" target-dir)))
		(rename-file best target t)
		(dolist (leftover (directory-files target-dir t "\\`cover-q[0-9]+\\.jpg\\'"))
		  (delete-file leftover))
		target)
	    (let* ((extension (file-name-extension source t))
		   (target (expand-file-name (concat "cover" extension) target-dir)))
	      (copy-file source target t)
	      target)))
      (let* ((extension (file-name-extension source t))
	     (target (expand-file-name (concat "cover" extension) target-dir)))
	(copy-file source target t)
	target))))

(defun hub/org-epub--copy-cover (metadata title-dir work-dir)
  "Copy configured cover from METADATA into TITLE-DIR and WORK-DIR."
  (when-let* ((cover (plist-get metadata :cover)))
    (let* ((source (expand-file-name cover default-directory))
	   (final-cover nil)
	   (name nil)
	   (work-cover nil))
      (unless (file-readable-p source)
	(user-error "EPUB cover is not readable: %s" source))
      (setq final-cover (hub/org-epub--optimized-cover-or-copy source title-dir))
      (setq name (file-name-nondirectory final-cover))
      (setq work-cover (expand-file-name name work-dir))
      (plist-put metadata :cover name)
      (copy-file final-cover work-cover t))))

(defun hub/org-epub--write-metadata-json (directory metadata epub-file work-dir)
  "Write final metadata JSON into DIRECTORY for EPUB-FILE and WORK-DIR."
  (let ((json-encoding-pretty-print t))
    (hub/org-epub--write-file
     (expand-file-name "metadata.json" directory)
     (json-encode (hub/org-epub--metadata-json-alist metadata epub-file work-dir)))))

(defun hub/org-epub--write-notes-report (work-dir)
  "Write note degradation report into WORK-DIR when needed."
  (when hub/org-epub--note-report-items
    (let ((json-encoding-pretty-print t))
      (hub/org-epub--write-file
       (expand-file-name "notes-report.json" work-dir)
       (json-encode `((items . ,(vconcat (nreverse hub/org-epub--note-report-items)))))))))

;;;###autoload
(defun hub/org-epub-export-to-epub (&optional permissive)
  "Export the current Org buffer to an EPUB file.
Return a plist describing the generated artifact paths.
With PERMISSIVE, continue past degradable preflight items."
  (interactive "P")
  (let* ((metadata (hub/org-epub--metadata))
	 (title (plist-get metadata :title))
	 (safe-title (hub/org-epub--sanitize-filename title))
	 (book-stem (or (plist-get metadata :identifier) safe-title))
	 (title-dir (hub/org-epub--ensure-directory
		     (expand-file-name safe-title hub/org-epub-output-root)))
	 (epub-file (expand-file-name (concat safe-title ".epub") title-dir))
	 (work-dir (hub/org-epub--ensure-directory
		    (expand-file-name (hub/org-epub--timestamp)
				      (expand-file-name book-stem hub/org-epub-work-root))))
	 (spine nil)
	 (hub/org-epub--note-report-items nil)
	 (hub/org-epub--preflight-items nil)
	 (hub/org-epub--permissive permissive)
	 (hub/org-epub--language (plist-get metadata :language)))
    (condition-case err
	(progn
	  (hub/org-epub--confirm-overwrite epub-file)
	  (hub/org-epub--copy-reader-css work-dir)
	  (hub/org-epub--copy-cover metadata title-dir work-dir)
	  (setq spine (hub/org-epub--write-minimal-xhtml work-dir metadata))
	  (hub/org-epub--write-notes-report work-dir)
	  (hub/org-epub--write-preflight-json work-dir)
	  (hub/org-epub--call-pandoc spine epub-file metadata work-dir)
	  (hub/org-epub--write-metadata-json title-dir metadata epub-file work-dir)
	  (message "Exported EPUB: %s" epub-file)
	  (list :epub-file epub-file
		:work-dir work-dir
		:spine spine))
      (user-error
       (hub/org-epub--preflight-add
	'error
	'export-failed
	(error-message-string err))
       (hub/org-epub--write-preflight-json work-dir)
       (hub/org-epub--render-preflight-buffer)
       (signal (car err) (cdr err))))))

(org-export-define-derived-backend 'hub-epub 'html
				   :menu-entry
				   '(?E "Export to EPUB"
					((?E "To EPUB file" hub/org-epub-export-to-epub))))

(with-eval-after-load 'org/export
  (when (boundp 'org-export-backends)
    (add-to-list 'org-export-backends 'hub-epub t)))

(provide 'org/export-epub)
;;; export-epub.el ends here
