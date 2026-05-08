;;; export.el --- Org LaTeX export classes and helpers -*- lexical-binding: t; -*-

;;; Commentary:
;; Minimal Org -> LaTeX -> PDF export support for the Veriff class family.

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'ox)
(require 'ox-latex)

(defgroup hub/org-export nil
  "Customizations for Org LaTeX export classes."
  :group 'hub/org)

(defcustom hub/org-export-output-root (expand-file-name "var/org-latex-pdf" user-emacs-directory)
  "Root directory for generated Org export artifacts."
  :type 'directory
  :group 'hub/org-export)

(defcustom hub/org-export-supported-locales '("en" "fr")
  "Supported locale codes for Org LaTeX export."
  :type '(repeat string)
  :group 'hub/org-export)

(defcustom hub/org-export-pdf-process
  '("xelatex -interaction nonstopmode -output-directory %o %f"
    "xelatex -interaction nonstopmode -output-directory %o %f")
  "Commands used to compile exported LaTeX files into PDFs."
  :type '(repeat string)
  :group 'hub/org-export)

(defconst hub/org-export--veriff-compiler "xelatex"
  "Required LaTeX compiler for `veriff'.")

(defconst hub/org-export--veriff-class-name "veriff"
  "Public LaTeX class name for the Veriff export family.")

(defconst hub/org-export--veriff-default-variant "refresh-overdrive"
  "Default variant for the Veriff export family.")

(defconst hub/org-export--veriff-variants
  '("refresh-overdrive" "dark-campaign")
  "Valid variants for the Veriff export family.")

(defconst hub/org-export--babel-package '("AUTO" "babel" nil)
  "Package entry enabling locale-aware Babel wiring in Org LaTeX exports.")

(defconst hub/org-export--veriff-class-file
  (expand-file-name "etc/latex/veriff.cls" user-emacs-directory)
  "Tracked LaTeX class asset for `veriff'.")

(defconst hub/org-export--veriff-assets-directory
  (expand-file-name "assets" (file-name-directory hub/org-export--veriff-class-file))
  "Tracked helper asset directory for `veriff'.")

(defvar hub/org-export--active-output-dir nil
  "Current export artifact directory for helper staging and header wiring.")

(defconst hub/org-export--veriff-title-command
  (string-join
   '("\\begin{hubhero}"
     "\\noindent\\begin{minipage}[t]{118mm}"
     "\\raggedright"
     "\\HubExportEyebrowBlock"
     "\\HubHeroTitle{%t}"
     "\\HubHeroDek{%s}"
     "\\HubHeroMeta{%a}{%D}"
     "\\end{minipage}"
     "\\end{hubhero}")
   "\n")
  "Title command used for the `veriff' LaTeX class.")

(defun hub/org-export--locale-code (&optional backend)
  "Return the current export locale code for BACKEND.
Defaults to English when the Org buffer does not specify one."
  (or (plist-get (org-export-get-environment (or backend 'latex)) :language)
      "en"))

(defun hub/org-export--supported-locale-p (locale)
  "Return non-nil when LOCALE is supported by the export layer."
  (member locale hub/org-export-supported-locales))

(defun hub/org-export--keyword-string (keyword)
  "Return the first string value for Org KEYWORD in the current buffer."
  (car (hub/org-export--keyword-values keyword)))

(defun hub/org-export--keyword-values (keyword)
  "Return all trimmed string values for Org KEYWORD in the current buffer."
  (when-let* ((entry (assoc-string keyword (org-collect-keywords (list keyword)) t)))
    (mapcar #'string-trim (cdr entry))))

(defun hub/org-export--class-asset-source (&optional class-name)
  "Return the tracked class asset path for CLASS-NAME.
Default to the current buffer's LaTeX class when CLASS-NAME is nil."
  (pcase (or class-name (hub/org-export--class-name))
    ("veriff" hub/org-export--veriff-class-file)
    (_ nil)))

(defun hub/org-export--latex-escape (text)
  "Return TEXT with a minimal LaTeX-safe escaping.
This is intentionally narrow and targets metadata-like strings."
  (let ((result (or text "")))
    (dolist (pair '(("\\" . "\\textbackslash{}")
		    ("{" . "\\{")
		    ("}" . "\\}")
		    ("%" . "\\%")
		    ("&" . "\\&")
		    ("$" . "\\$")
		    ("#" . "\\#")
		    ("_" . "\\_")
		    ("^" . "\\textasciicircum{}")
		    ("~" . "\\textasciitilde{}")))
      (setq result
	    (replace-regexp-in-string (regexp-quote (car pair)) (cdr pair) result t t)))
    result))

(defun hub/org-export--compiler-ready-p (compiler)
  "Return non-nil when COMPILER is ready for use in this environment."
  (and (string= compiler "xelatex")
       (executable-find compiler)
       (file-exists-p hub/org-export--veriff-class-file)))

(defun hub/org-export--effective-compiler ()
  "Return the compiler to use for the current export.

Requires XeLaTeX and the veriff class asset; signals `user-error' when unavailable."
  (if (hub/org-export--compiler-ready-p "xelatex")
      "xelatex"
    (user-error "XeLaTeX and the veriff class asset are required for this export")))

(defun hub/org-export--pdf-process-for-compiler (compiler)
  "Return a minimal two-pass PDF process for COMPILER."
  (let ((command (format "%s -interaction nonstopmode -shell-escape -output-directory %%o %%f"
			 compiler)))
    (list command command)))

(defun hub/org-export--class-name ()
  "Return the current Org LaTeX class name from buffer metadata."
  (hub/org-export--keyword-string "LATEX_CLASS"))

(defun hub/org-export--veriff-p ()
  "Return non-nil when the current buffer targets `veriff'."
  (equal (hub/org-export--class-name) hub/org-export--veriff-class-name))

(defun hub/org-export--effective-veriff-variant (&optional variants)
  "Return the effective Veriff variant for the current buffer.

When VARIANTS is non-nil, validate that list instead of re-reading the
buffer metadata.  Missing variants default to
`hub/org-export--veriff-default-variant'."
  (when (hub/org-export--veriff-p)
    (let ((values (or variants (hub/org-export--keyword-values "LATEX_VARIANT"))))
      (cond
       ((> (length values) 1)
	(user-error "Duplicate LATEX_VARIANT keywords are not allowed"))
       ((null values)
	hub/org-export--veriff-default-variant)
       (t
	(let ((variant (car values)))
	  (unless (member variant hub/org-export--veriff-variants)
	    (user-error "Invalid LATEX_VARIANT: %s. Valid variants: %s"
			variant
			(string-join hub/org-export--veriff-variants ", ")))
	  variant))))))

(defun hub/org-export--insert-header-extra (latex-line)
  "Insert LATEX-LINE as a `LATEX_HEADER_EXTRA' keyword in the current buffer."
  (goto-char (point-min))
  (insert "#+LATEX_HEADER_EXTRA: " latex-line "\n"))

(defun hub/org-export--append-footer-note ()
  "Append the class-owned footer note block when metadata provides one."
  (when-let* ((footer (hub/org-export--keyword-string "EXPORT_FOOTER_NOTE"))
	      ((not (string-empty-p footer))))
    (goto-char (point-max))
    (insert "\n#+begin_export latex\n"
	    "\\begin{hubfootnote}\n"
	    (hub/org-export--latex-escape footer)
	    "\n\\end{hubfootnote}\n"
	    "#+end_export\n")))

(defun hub/org-export--configure-veriff-title ()
  "Configure title and chrome variables for the Veriff class."
  (let ((eyebrow (hub/org-export--keyword-string "EXPORT_EYEBROW"))
	(code-theme (hub/org-export--keyword-string "EXPORT_CODE_THEME"))
	(variant (or (hub/org-export--effective-veriff-variant)
		     hub/org-export--veriff-default-variant)))
    (when (and code-theme (not (string-empty-p code-theme)) (not (member code-theme '("light" "dark"))))
      (user-error "Invalid EXPORT_CODE_THEME: %s. Must be 'light' or 'dark'." code-theme))
    (setq-local org-export-with-toc nil)
    (setq-local org-latex-compiler (hub/org-export--effective-compiler))
    (setq-local org-latex-pdf-process
		(hub/org-export--pdf-process-for-compiler org-latex-compiler))
    (setq-local org-latex-title-command hub/org-export--veriff-title-command)
    (setq-local org-latex-src-block-backend 'minted)
    (setq-local org-latex-minted-options
		'(("fontsize" "\\footnotesize")
		  ("breaklines" "true")
		  ("autogobble" "true")
		  ("xleftmargin" "14pt")
		  ("numbersep" "8pt")))
    (when hub/org-export--active-output-dir
      (let ((logo-asset (if (equal variant "dark-campaign") "hero-logo-dark.pdf" "hero-logo.pdf")))
	(hub/org-export--insert-header-extra
	 (format "\\renewcommand{\\HubHeroLogoGraphic}{\\includegraphics[width=32mm]{%s}}"
		 (hub/org-export--latex-escape
		  (expand-file-name logo-asset hub/org-export--active-output-dir))))
	(hub/org-export--insert-header-extra
	 (format "\\renewcommand{\\HubFooterLogoGraphic}{\\includegraphics[width=16mm]{%s}}"
		 (hub/org-export--latex-escape
		  (expand-file-name logo-asset hub/org-export--active-output-dir)))))
      (if (equal variant "dark-campaign")
	  (hub/org-export--insert-header-extra
	   (format "\\renewcommand{\\HubHeroPatternGraphic}{\\includegraphics[width=118mm]{%s}}"
		   (hub/org-export--latex-escape
		    (expand-file-name "hero-pattern-dark.pdf" hub/org-export--active-output-dir))))
	(hub/org-export--insert-header-extra
	 (format "\\renewcommand{\\HubHeroPatternGraphic}{\\includegraphics[width=118mm]{%s}}"
		 (hub/org-export--latex-escape
		  (expand-file-name "hero-pattern.png" hub/org-export--active-output-dir))))))
    (hub/org-export--insert-header-extra
     (format "\\renewcommand{\\HubExportEyebrowBlock}{%s}"
	     (if (and eyebrow (not (string-empty-p eyebrow)))
		 (format "\\HubHeroEyebrow{%s}"
			 (hub/org-export--latex-escape eyebrow))
	       "")))
    (when (equal variant hub/org-export--veriff-default-variant)
      (hub/org-export--insert-header-extra "% hub-veriff-default-variant")
      (hub/org-export--insert-header-extra "\\HubVeriffVariant{refresh-overdrive}"))
    (when (equal variant "dark-campaign")
      (hub/org-export--insert-header-extra "% hub-veriff-dark-campaign")
      (hub/org-export--insert-header-extra "\\HubVeriffVariant{dark-campaign}"))
    (when (equal code-theme "dark")
      (hub/org-export--insert-header-extra "\\HubCodeThemeDark"))
    (when (equal code-theme "light")
      (hub/org-export--insert-header-extra "\\HubCodeThemeLight"))))

(defun hub/org-export--validate-veriff-metadata (backend)
  "Reject invalid Veriff class and variant metadata for BACKEND."
  (when (org-export-derived-backend-p backend 'latex)
    (let ((class (hub/org-export--class-name))
	  (variants (hub/org-export--keyword-values "LATEX_VARIANT")))
      (cond
       ((equal class "pro-refresh-overdrive")
	(user-error "Legacy LATEX_CLASS: pro-refresh-overdrive is unsupported; use #+LATEX_CLASS: veriff with #+LATEX_VARIANT: refresh-overdrive"))
       ((and variants (not (equal class hub/org-export--veriff-class-name)))
	(user-error "LATEX_VARIANT requires LATEX_CLASS: veriff"))
       ((equal class hub/org-export--veriff-class-name)
	(hub/org-export--effective-veriff-variant variants))))))

(defun hub/org-export--configure-class-buffer (backend)
  "Configure export-time buffer state for BACKEND.
This runs on the temporary export buffer only."
  (when (and (org-export-derived-backend-p backend 'latex)
	     (hub/org-export--veriff-p))
    (hub/org-export--configure-veriff-title)
    (hub/org-export--append-footer-note)))

(defun hub/org-export--validate-locale (backend)
  "Reject unsupported locale selections for Org export BACKEND."
  (when (org-export-derived-backend-p backend 'latex)
    (let ((locale (hub/org-export--locale-code backend)))
      (unless (hub/org-export--supported-locale-p locale)
	(user-error "Unsupported Org export locale: %s" locale)))))

(defun hub/org-export--ensure-babel-package ()
  "Ensure Babel locale wiring is added to `org-latex-packages-alist'."
  (unless (member hub/org-export--babel-package org-latex-packages-alist)
    (push hub/org-export--babel-package org-latex-packages-alist)))

(defun hub/org-export--info-veriff-p (info)
  "Return non-nil when INFO targets the `veriff' class."
  (equal (plist-get info :latex-class) hub/org-export--veriff-class-name))


(defun hub/org-export--table-row-cells (row info)
  "Return exported cell strings for table ROW using INFO."
  (org-element-map row 'table-cell
		   (lambda (cell)
		     (org-trim (org-export-data (org-element-contents cell) info)))
		   info))

(defun hub/org-export--split-table-rows (table info)
  "Return TABLE rows split into header and body lists using INFO.

The return value is a cons cell of the form (HEADERS . BODY), where
each row is a list of already-exported cell strings.  Rows before the
first Org rule separator become header rows."
  (let ((headers nil)
	(body nil)
	(seen-rule nil))
    (dolist (row (org-element-map table 'table-row #'identity info))
      (pcase (org-element-property :type row)
	('rule (setq seen-rule t))
	('standard
	 (let ((cells (hub/org-export--table-row-cells row info)))
	   (if seen-rule (push cells body) (push cells headers))))))
    (cons (nreverse headers) (nreverse body))))

(defun hub/org-export--table-row-string (cells &optional header-p)
  "Return LaTeX for a table row made of CELLS.

When HEADER-P is non-nil, render cells with the class-owned header macro."
  (concat
   (mapconcat
    (lambda (cell)
      (if header-p
	  (format "\\HubTableHeaderCell{%s}" cell)
	(format "\\HubTableBodyCell{%s}" cell)))
    cells
    " & ")
   "\\\\"))

(defun hub/org-export--format-branded-table (table info)
  "Return class-owned LaTeX for plain Org TABLE using INFO."
  (pcase-let* ((`(,headers . ,body) (hub/org-export--split-table-rows table info))
	       (all-rows (append headers body))
	       (column-count (length (car all-rows)))
	       (column-spec (make-string column-count ?Y)))
    (when (zerop column-count)
      (user-error "Cannot export an empty Org table"))
    (let* ((attr (copy-sequence (org-export-read-attribute :attr_latex table)))
	   (caption (org-latex--caption/label-string table info))
	   (above? (org-latex--caption-above-p table info))
	   (body-rows (if body body (cdr headers)))
	   (header-rows (if body headers (list (car headers))))
	   (table-string
	    (concat
	     "\\begin{hubtable}\n"
	     (format "\\begin{tabularx}{\\linewidth}{%s}\n" column-spec)
	     (mapconcat (lambda (row)
			  (hub/org-export--table-row-string row t))
			header-rows
			"\n")
	     "\n\\HubTableHeadRule\n"
	     (mapconcat
	      (lambda (row)
		(hub/org-export--table-row-string row nil))
	      body-rows
	      "\n\\HubTableBodyRule\n")
	     "\n\\end{tabularx}\n"
	     "\\end{hubtable}")))
      (unless (plist-member attr :center)
	(setq attr (plist-put attr :center nil)))
      (org-latex--decorate-table table-string attr caption above? info))))

(defun hub/org-export--advice-org-latex-org-table (orig table contents info)
  "Render TABLE through ORIG, with branded defaults for the flagship class."
  (let ((attr (org-export-read-attribute :attr_latex table)))
    (if (and (hub/org-export--info-veriff-p info)
	     (not (plist-get attr :environment))
	     (not (plist-get attr :mode))
	     (not (plist-get attr :align))
	     (not (plist-get attr :width))
	     (not (plist-get attr :options)))
	(hub/org-export--format-branded-table table info)
      (funcall orig table contents info))))

(defun hub/org-export--advice-org-latex-special-block (orig special-block contents info)
  "Escape LaTeX special characters in the `:options' attribute of SPECIAL-BLOCK.
This is narrowly scoped to `metric' blocks in the `veriff' class."
  (let* ((attr-latex (org-element-property :attr_latex special-block)))
    (when (and attr-latex
	       (hub/org-export--info-veriff-p info)
	       (equal (org-element-property :type special-block) "metric"))
      (let ((new-attr (mapcar (lambda (s)
				(if (string-match ":options\\s-+\\(.+\\)" s)
				    (let* ((opt-val (match-string 1 s))
					   (unescaped opt-val))
				      (dolist (char '("{" "}" "%" "&" "$" "#" "_" "^" "~"))
					(setq unescaped (replace-regexp-in-string (concat "\\\\" (regexp-quote char)) char unescaped t t)))
				      (replace-match (hub/org-export--latex-escape unescaped) t t s 1))
				  s))
			      attr-latex)))
	(org-element-put-property special-block :attr_latex new-attr))))
  (funcall orig special-block contents info))

(defun hub/org-export--register-latex-class (name header)
  "Register LaTeX class NAME with HEADER and standard sectioning mappings."
  (setq org-latex-classes
	(cons
	 (list name header
	       '("\\section{%s}" . "\\section*{%s}")
	       '("\\subsection{%s}" . "\\subsection*{%s}")
	       '("\\subsubsection{%s}" . "\\subsubsection*{%s}")
	       '("\\paragraph{%s}" . "\\paragraph*{%s}")
	       '("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
	 (cl-remove-if (lambda (entry)
			 (equal (car entry) name))
		       org-latex-classes))))

(defun hub/org-export-register-veriff ()
  "Register the public Veriff Org LaTeX export class."
  (hub/org-export--register-latex-class
   hub/org-export--veriff-class-name
   (string-join
    '("\\documentclass[11pt,a4paper]{veriff}"
      "% hub-veriff"
      "% hub-veriff-refresh-overdrive"
      "% hub-veriff-refresh-overdrive-title"
      "% hub-veriff-refresh-overdrive-headings"
      "[DEFAULT-PACKAGES]"
      "[PACKAGES]")
    "\n")))

(defun hub/org-export--ensure-output-directory (&optional output-dir)
  "Return OUTPUT-DIR with directories created, or the default export root."
  (let ((dir (file-name-as-directory (or output-dir hub/org-export-output-root))))
    (unless (file-directory-p dir)
      (make-directory dir t))
    dir))

(defun hub/org-export--stage-class-asset (output-dir &optional class-name)
  "Copy the tracked class asset for CLASS-NAME into OUTPUT-DIR.
Default to the current buffer's LaTeX class when CLASS-NAME is nil."
  (let ((resolved-class (or class-name (hub/org-export--class-name))))
    (when-let* ((source (hub/org-export--class-asset-source resolved-class)))
      (unless (file-exists-p source)
	(user-error "Missing Org export class asset: %s" source))
      (copy-file source
		 (expand-file-name (file-name-nondirectory source) output-dir)
		 t)
      (when (equal resolved-class hub/org-export--veriff-class-name)
	(when (file-directory-p hub/org-export--veriff-assets-directory)
	  (dolist (asset (directory-files hub/org-export--veriff-assets-directory t "\\.\\(png\\|pdf\\)\\'"))
	    (copy-file asset
		       (expand-file-name (file-name-nondirectory asset) output-dir)
		       t)))))))

(defun hub/org-export-buffer-to-latex (&optional output-dir)
  "Export the current Org buffer to LaTeX in OUTPUT-DIR.
Return the generated `.tex' path."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Current buffer is not in Org mode"))
  (hub/org-export--validate-locale 'latex)
  (hub/org-export--validate-veriff-metadata 'latex)
  (hub/org-export--ensure-babel-package)
  (let* ((outdir (hub/org-export--ensure-output-directory output-dir))
	 (hub/org-export--active-output-dir outdir)
	 (org-export-show-temporary-export-buffer nil)
	 (base-name (file-name-base (or (buffer-file-name) (buffer-name))))
	 (tex-path (expand-file-name (concat base-name ".tex") outdir)))
    (hub/org-export--stage-class-asset outdir)
    (org-export-to-file 'latex tex-path nil nil nil nil nil)))

(defun hub/org-export-buffer-to-pdf (&optional output-dir)
  "Export the current Org buffer to PDF in OUTPUT-DIR.
Return the generated `.pdf' path."
  (interactive)
  (let* ((compiler (and (hub/org-export--veriff-p)
			(hub/org-export--effective-compiler)))
	 (org-latex-compiler (or compiler org-latex-compiler))
	 (org-latex-pdf-process (if compiler
				    (hub/org-export--pdf-process-for-compiler compiler)
				  hub/org-export-pdf-process))
	 (tex-path (hub/org-export-buffer-to-latex output-dir))
	 (pdf-path (concat (file-name-sans-extension tex-path) ".pdf")))
    (org-latex-compile tex-path)
    pdf-path))

(hub/org-export-register-veriff)
(hub/org-export--ensure-babel-package)
(advice-add 'org-latex--org-table :around #'hub/org-export--advice-org-latex-org-table)
(advice-add 'org-latex-special-block :around #'hub/org-export--advice-org-latex-special-block)
(add-hook 'org-export-before-processing-functions #'hub/org-export--validate-locale)
(add-hook 'org-export-before-processing-functions #'hub/org-export--validate-veriff-metadata)
(add-hook 'org-export-before-processing-functions #'hub/org-export--configure-class-buffer)

(provide 'org/export)
;;; export.el ends here
