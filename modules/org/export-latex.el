;;; export.el --- Org LaTeX export classes and helpers -*- lexical-binding: t; -*-

;;; Commentary:
;; Minimal Org -> LaTeX -> PDF export support for local LaTeX classes.

;;; Code:

(require 'hub-utils)

(require 'cl-lib)
(require 'hub-org-callout)
(require 'org)
(require 'ox)
(require 'hub-keys)
(require 'ox-latex)
(require 'seq)

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

(defcustom hub/org-export-use-devenv-compiler t
  "When non-nil, run local LaTeX exports through `devenv shell' when available.
This makes GUI-launched Emacs use the same TeX Live package set as the
project shell, including packages such as minted."
  :type 'boolean
  :group 'hub/org-export)

(defcustom hub/org-export-devenv-executable nil
  "Optional absolute path to the devenv executable used for LaTeX exports.
When nil, discover devenv from `exec-path' and common Nix profile locations."
  :type '(choice (const :tag "Discover automatically" nil) file)
  :group 'hub/org-export)

(defconst hub/org-export--veriff-compiler "xelatex"
  "Required LaTeX compiler for `veriff'.")

(defconst hub/org-export--veriff-class-name "veriff"
  "Public LaTeX class name for the Veriff export family.")

(defconst hub/org-export--hub-article-class-name "hub-article"
  "Public LaTeX class name for the personal article export family.")

(defconst hub/org-export--veriff-default-variant "refresh-overdrive"
  "Default variant for the Veriff export family.")

(defconst hub/org-export--veriff-variants
  '("refresh-overdrive" "dark-campaign" "gallery-white")
  "Valid variants for the Veriff export family.")

(defconst hub/org-export--babel-package '("AUTO" "babel" nil)
  "Package entry enabling locale-aware Babel wiring in Org LaTeX exports.")

(defconst hub/org-export--french-month-names
  '("janvier" "février" "mars" "avril" "mai" "juin"
    "juillet" "août" "septembre" "octobre" "novembre" "décembre")
  "French month names for locale-owned generated dates.")

(defconst hub/org-export--latex-class-directory
  (expand-file-name "etc/latex" user-emacs-directory)
  "Directory containing repo-local LaTeX class assets.")

(defconst hub/org-export--xelatex-class-names
  (list hub/org-export--veriff-class-name hub/org-export--hub-article-class-name)
  "Local LaTeX class names that require the XeLaTeX compiler path.")

(defconst hub/org-export--veriff-assets-directory
  (expand-file-name "assets" hub/org-export--latex-class-directory)
  "Tracked helper asset directory for `veriff'.")

(defcustom hub/org-export-texinputs-directories
  (list hub/org-export--latex-class-directory)
  "Directories made visible to TeX subprocesses through TEXINPUTS.
Each directory is added recursively so GUI-launched Emacs can find
repo-local classes and assets without inheriting a devenv shell."
  :type '(repeat directory)
  :group 'hub/org-export)

(defun hub/org-export--texinputs-entry (directory)
  "Return DIRECTORY as a recursive Kpathsea TEXINPUTS entry."
  (concat (file-name-as-directory (expand-file-name directory)) "//"))

(defun hub/org-export--merge-texinputs (directories &optional current)
  "Return TEXINPUTS with DIRECTORIES prepended before CURRENT.
The trailing path separator keeps Kpathsea's default search path enabled."
  (let* ((entries (delete-dups
		   (mapcar #'hub/org-export--texinputs-entry
			   (seq-filter #'file-directory-p directories))))
	 (existing (split-string (or current "") path-separator t))
	 (merged (delete-dups (append entries existing))))
    (concat (mapconcat #'identity merged path-separator) path-separator)))

(defun hub/org-export--ensure-texinputs ()
  "Expose repo-local LaTeX assets to TeX subprocesses."
  (setenv "TEXINPUTS"
	  (hub/org-export--merge-texinputs hub/org-export-texinputs-directories
					   (getenv "TEXINPUTS"))))

(defvar hub/org-export--active-output-dir nil
  "Current export artifact directory for helper staging and header wiring.")

(defconst hub/org-export--standard-sectioning
  '("\\section{%s}" . "\\section*{%s}")
  "Top-level sectioning mapping for local Org LaTeX classes.")

(defconst hub/org-export--standard-subsectioning
  '(("\\subsection{%s}" . "\\subsection*{%s}")
    ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
    ("\\paragraph{%s}" . "\\paragraph*{%s}")
    ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
  "Nested sectioning mappings for local Org LaTeX classes.")

(defconst hub/org-export--veriff-title-command
  (string-join
   '("\\begin{hubhero}"
     "\noindent\\begin{minipage}[t]{118mm}"
     "\\raggedright"
     "\\HubExportEyebrowBlock"
     "\\HubHeroTitle{%t}"
     "\\HubHeroDek{%s}"
     "\\HubHeroMeta{%a}{%D}"
     "\\end{minipage}"
     "\\end{hubhero}")
   "\n")
  "Title command used for the `veriff' LaTeX class.")

(defconst hub/org-export--veriff-gallery-white-title-command
  (string-join
   '("\\twocolumn[{"
     "\\begin{hubhero}"
     "\noindent\\begin{minipage}{118mm}"
     "\\raggedright"
     "\\HubExportEyebrowBlock"
     "\\HubHeroTitle{%t}"
     "\\HubHeroDek{%s}"
     "\\HubHeroMeta{%a}{%D}"
     "\\end{minipage}"
     "\\end{hubhero}")
   "\n")
  "Title command used for the `gallery-white' Veriff variant.
The gallery-white `standfirst' special-block export closes the full-width
two-column opener with a braced literal optional argument in the generated
LaTeX.")

(defconst hub/org-export--hub-article-title-command
  (string-join
   '("\\begin{hubarticleopener}"
     "\\HubArticleEyebrowBlock"
     "\\HubArticleTitle{%t}"
     "\\HubArticleDek{%s}"
     "\\HubArticleMeta{%a}{%D}"
     "\\end{hubarticleopener}")
   "\n")
  "Title command used for the `hub-article' LaTeX class.")

(defun hub/org-export--locale-code (&optional backend)
  "Return the current export locale code for BACKEND.
Defaults to English when the Org buffer does not specify one."
  (or (plist-get (org-export-get-environment (or backend 'latex)) :language)
      "en"))

(defun hub/org-export--supported-locale-p (locale)
  "Return non-nil when LOCALE is supported by the export layer."
  (member locale hub/org-export-supported-locales))

(defun hub/org-export--keyword-string (keyword)
  "Return the string value for Org KEYWORD, with local priority.
Per Org semantics, a headline property drawer (:EXPORT_EYEBROW: value)
takes precedence over a file-level #+KEYWORD: line."
  (or
   ;; 1. Per-headline property drawer (higher priority)
   (when (derived-mode-p 'org-mode)
     (let ((v (org-entry-get (point) keyword 'selective)))
       (and (stringp v) (string-trim v))))
   ;; 2. File-level #+KEYWORD: line (lower priority)
   (car (hub/org-export--keyword-values keyword))))

(defun hub/org-export--keyword-values (keyword)
  "Return all trimmed string values for Org KEYWORD in the current buffer."
  (when-let* ((entry (assoc-string keyword (org-collect-keywords (list keyword)) t)))
    (mapcar #'string-trim (cdr entry))))

(defun hub/org-export--local-class-assets (&optional directory)
  "Return (CLASS-NAME . FILE) pairs for local .cls assets in DIRECTORY.
When DIRECTORY is nil, use `hub/org-export--latex-class-directory'."
  (let ((class-directory (or directory hub/org-export--latex-class-directory)))
    (when (file-directory-p class-directory)
      (sort
       (mapcar (lambda (file)
		 (cons (file-name-base file) file))
	       (directory-files class-directory t "\\.cls\\'" 'nosort))
       (lambda (left right)
	 (string< (car left) (car right)))))))

(defun hub/org-export--local-class-names (&optional directory)
  "Return local LaTeX class names discovered from DIRECTORY."
  (mapcar #'car (hub/org-export--local-class-assets directory)))

(defun hub/org-export--class-asset-source (&optional class-name)
  "Return the tracked class asset path for CLASS-NAME.
Default to the current buffer's LaTeX class when CLASS-NAME is nil."
  (alist-get (or class-name (hub/org-export--class-name))
	     (hub/org-export--local-class-assets)
	     nil nil #'string=))

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

(defun hub/org-export--date-components-from-iso-string (date)
  "Return (YEAR MONTH DAY) parsed from ISO DATE, or nil."
  (when (and (stringp date)
	     (string-match "\\`\\([0-9][0-9][0-9][0-9]\\)-\\([0-9][0-9]\\)-\\([0-9][0-9]\\)\\'" date))
    (list (string-to-number (match-string 1 date))
	  (string-to-number (match-string 2 date))
	  (string-to-number (match-string 3 date)))))

(defun hub/org-export--date-components-from-timestamp (date)
  "Return (YEAR MONTH DAY) from single timestamp DATE, or nil."
  (when (and date
	     (not (cdr date))
	     (org-element-type-p (car date) 'timestamp))
    (let ((timestamp (car date)))
      (list (org-element-property :year-start timestamp)
	    (org-element-property :month-start timestamp)
	    (org-element-property :day-start timestamp)))))

(defun hub/org-export--french-date-from-components (components)
  "Return fine French LaTeX date text from date COMPONENTS."
  (pcase-let ((`(,year ,month ,day) components))
    (when (and year month day
	       (<= 1 month)
	       (<= month (length hub/org-export--french-month-names)))
      (format "%s~%s~%s"
	      (if (= day 1) "1er" (number-to-string day))
	      (nth (1- month) hub/org-export--french-month-names)
	      year))))

(defun hub/org-export--format-hub-article-date (info fallback)
  "Return locale-aware hub-article date for INFO, or FALLBACK."
  (if (and (hub/org-export--info-hub-article-p info)
	   (equal (plist-get info :language) "fr"))
      (or (hub/org-export--french-date-from-components
	   (or (hub/org-export--date-components-from-timestamp (plist-get info :date))
	       (hub/org-export--date-components-from-iso-string fallback)))
	  fallback)
    fallback))

(defun hub/org-export--advice-org-latex--format-spec (orig info)
  "Make Org LaTeX metadata format spec locale-aware around ORIG for INFO."
  (let* ((spec (funcall orig info))
	 (date (cdr (assq ?D spec)))
	 (formatted-date (hub/org-export--format-hub-article-date info date)))
    (if (equal formatted-date date)
	spec
      (cons (cons ?D formatted-date)
	    (assq-delete-all ?D (copy-sequence spec))))))

(defun hub/org-export--kpsewhich-file (file)
  "Return non-empty kpsewhich result for FILE, or nil."
  (when (executable-find "kpsewhich")
    (let ((result (string-trim
		   (shell-command-to-string
		    (format "kpsewhich %s" (shell-quote-argument file))))))
      (unless (string-empty-p result)
	result))))

(defun hub/org-export--latex-runtime-ready-p (compiler)
  "Return non-nil when COMPILER has the packages needed for local exports."
  (and (executable-find compiler)
       (hub/org-export--kpsewhich-file "minted.sty")
       (executable-find "pygmentize")))

(defun hub/org-export--compiler-ready-p (compiler)
  "Return non-nil when COMPILER is ready for use in this environment."
  (and (string= compiler "xelatex")
       (or (hub/org-export--latex-runtime-ready-p compiler)
	   (hub/org-export--devenv-compiler-command compiler))))

(defun hub/org-export--effective-compiler (&optional class-name)
  "Return the compiler to use for the current export.

Requires XeLaTeX and the tracked asset for CLASS-NAME, or the current buffer's
class when CLASS-NAME is nil; signals `user-error' when unavailable."
  (let* ((resolved-class (or class-name (hub/org-export--class-name)))
	 (class-asset (hub/org-export--class-asset-source resolved-class)))
    (cond
     ((not (hub/org-export--compiler-ready-p "xelatex"))
      (user-error "XeLaTeX is required for this export"))
     ((and class-asset (not (file-exists-p class-asset)))
      (user-error "Missing Org export class asset: %s" class-asset))
     (t "xelatex"))))

(defun hub/org-export--candidate-devenv-executables ()
  "Return possible devenv executable paths for GUI-launched Emacs."
  (delete-dups
   (seq-filter
    #'identity
    (list hub/org-export-devenv-executable
	  (executable-find "devenv")
	  (expand-file-name "bin/devenv"
			    (expand-file-name (user-login-name)
					      "/etc/profiles/per-user"))))))

(defun hub/org-export--devenv-executable ()
  "Return a usable devenv executable path, or nil."
  (seq-find #'file-executable-p (hub/org-export--candidate-devenv-executables)))

(defun hub/org-export--devenv-compiler-command (compiler)
  "Return COMPILER command wrapped in the project devenv shell when possible.
When already inside a complete devenv shell, skip the wrapper and run COMPILER
directly.  Some GUI sessions inherit IN_NIX_SHELL without the matching PATH, so
runtime readiness is checked instead of trusting that variable alone."
  (if (and hub/org-export-use-devenv-compiler
	   (file-exists-p (expand-file-name "devenv.nix" user-emacs-directory))
	   (not (hub/org-export--latex-runtime-ready-p compiler)))
      (when-let* ((devenv (hub/org-export--devenv-executable))
		  (project-root (directory-file-name
				 (expand-file-name user-emacs-directory))))
	(format "cd %s && TEXINPUTS=%s %s shell -- %s"
		(shell-quote-argument project-root)
		(shell-quote-argument
		 (hub/org-export--merge-texinputs hub/org-export-texinputs-directories
						  (getenv "TEXINPUTS")))
		(shell-quote-argument devenv)
		(shell-quote-argument compiler)))
    nil))

(defun hub/org-export--compiler-command (compiler)
  "Return the shell command prefix used to invoke COMPILER."
  (or (hub/org-export--devenv-compiler-command compiler)
      (format "TEXINPUTS=%s %s"
	      (shell-quote-argument
	       (hub/org-export--merge-texinputs hub/org-export-texinputs-directories
						(getenv "TEXINPUTS")))
	      (shell-quote-argument compiler))))

(defun hub/org-export--pdf-process-for-texfile (texfile)
  "Return local PDF process for TEXFILE, or nil for Org defaults."
  (let ((compiler (with-temp-buffer
		    (insert-file-contents texfile nil 0 512)
		    (when (re-search-forward
			   (format "^%% Intended LaTeX compiler: \\(%s\\)"
				   (regexp-opt '("xelatex")))
			   nil t)
		      "xelatex"))))
    (when compiler
      (hub/org-export--pdf-process-for-compiler compiler))))

(defun hub/org-export--advice-org-latex-compile (orig texfile &optional snippet)
  "Compile local class TEXFILE with the repo-aware PDF process.
ORIG and SNIPPET are the original `org-latex-compile' function and snippet flag."
  (let ((org-latex-pdf-process (or (and (not snippet)
					(hub/org-export--pdf-process-for-texfile texfile))
				   org-latex-pdf-process)))
    (funcall orig texfile snippet)))

(defun hub/org-export--pdf-process-for-compiler (compiler)
  "Return a minimal two-pass PDF process for COMPILER."
  (let ((command (format "%s -interaction nonstopmode -shell-escape -output-directory \"$(dirname %%O)\" %%F"
			 (hub/org-export--compiler-command compiler))))
    (list command command)))

(defun hub/org-export--class-name ()
  "Return the current Org LaTeX class name from buffer metadata."
  (hub/org-export--keyword-string "LATEX_CLASS"))

(defun hub/org-export--veriff-p ()
  "Return non-nil when the current buffer targets `veriff'."
  (equal (hub/org-export--class-name) hub/org-export--veriff-class-name))

(defun hub/org-export--hub-article-p ()
  "Return non-nil when the current buffer targets `hub-article'."
  (equal (hub/org-export--class-name) hub/org-export--hub-article-class-name))

(defun hub/org-export--info-hub-article-p (info)
  "Return non-nil when INFO targets the `hub-article' class."
  (equal (plist-get info :latex-class) hub/org-export--hub-article-class-name))

(defun hub/org-export--xelatex-class-p (&optional class-name)
  "Return non-nil when CLASS-NAME, or the current buffer class, uses XeLaTeX."
  (let ((resolved-class (or class-name (hub/org-export--class-name))))
    (or (member resolved-class hub/org-export--xelatex-class-names)
	(hub/org-export--class-asset-source resolved-class))))

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

(defun hub/org-export--gallery-white-standfirst-lines ()
  "Return line numbers for gallery-white standfirst block starts."
  (let (lines)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^[[:space:]]*#\\+begin_standfirst\\_>" nil t)
	(push (line-number-at-pos) lines)))
    (nreverse lines)))

(defun hub/org-export--gallery-white-leading-standfirst-p ()
  "Return non-nil when the first content block is a standfirst."
  (save-excursion
    (goto-char (point-min))
    (while (and (not (eobp))
		(or (looking-at-p "[[:space:]]*$")
		    (looking-at-p "[[:space:]]*#\\(?:[[:space:]].*\\)?$")
		    (looking-at-p "[[:space:]]*#\\+\\(?:TITLE\\|SUBTITLE\\|AUTHOR\\|DATE\\|LANGUAGE\\|LATEX_CLASS\\|LATEX_VARIANT\\|EXPORT_[^:]+\\|OPTIONS\\|STARTUP\\|LATEX_HEADER\\|LATEX_HEADER_EXTRA\\):")))
      (forward-line 1))
    (looking-at-p "[[:space:]]*#\\+begin_standfirst\\_>")))

(defun hub/org-export--validate-gallery-white-standfirst ()
  "Validate gallery-white standfirst placement for full-width opening flow."
  (let ((standfirst-lines (hub/org-export--gallery-white-standfirst-lines)))
    (cond
     ((null standfirst-lines)
      (user-error "gallery-white requires one leading standfirst block"))
     ((cdr standfirst-lines)
      (user-error "gallery-white requires exactly one standfirst block"))
     ((not (hub/org-export--gallery-white-leading-standfirst-p))
      (user-error "gallery-white standfirst must be the first content block")))))

(defun hub/org-export--package-entry-name (entry)
  "Return the LaTeX package name from package-alist ENTRY."
  (cadr entry))

(defun hub/org-export--filter-latex-package (package entries)
  "Return ENTRIES without LaTeX PACKAGE entries."
  (seq-remove (lambda (entry)
		(equal (hub/org-export--package-entry-name entry) package))
	      entries))

(defun hub/org-export--use-class-owned-fontspec ()
  "Keep fontspec in the class file instead of generated LaTeX preamble."
  (setq-local org-latex-default-packages-alist
	      (hub/org-export--filter-latex-package
	       "fontspec" org-latex-default-packages-alist))
  (setq-local org-latex-packages-alist
	      (hub/org-export--filter-latex-package
	       "fontspec" org-latex-packages-alist)))

(defun hub/org-export--insert-header-extra (latex-line)
  "Insert LATEX-LINE as a `LATEX_HEADER_EXTRA' keyword at the end of the header.
Insertion happens after `#+COLUMNS:' and similar buffer-wide settings,
not at `point-min', to avoid interfering with `#+TITLE:' keyword parsing."
  (goto-char (point-min))
  ;; Find the last keyword line (e.g. #+COLUMNS) by skipping past
  ;; all contiguous keyword lines starting from point-min.
  (while (and (not (eobp))
	      (looking-at "^[ \t]*#\\+"))
    (forward-line 1))
  ;; If we moved past any keyword lines, insert before the blank
  ;; line separator, which is right before the first heading.
  (skip-chars-forward "\n")
  (beginning-of-line)
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
    (hub/org-export--use-class-owned-fontspec)
    (setq-local org-latex-compiler (hub/org-export--effective-compiler))
    (setq-local org-latex-pdf-process
		(hub/org-export--pdf-process-for-compiler org-latex-compiler))
    (setq-local org-latex-title-command
		(if (equal variant "gallery-white")
		    hub/org-export--veriff-gallery-white-title-command
		  hub/org-export--veriff-title-command))
    (setq-local org-latex-src-block-backend 'minted)
    (setq-local org-latex-minted-options
		'(("fontsize" "\\footnotesize")
		  ("breaklines" "true")
		  ("autogobble" "true")
		  ("xleftmargin" "14pt")
		  ("numbersep" "8pt")))
    (setq-local org-latex-text-markup-alist
		'((bold . "\\HubInlineBold{%s}")
		  (italic . "\\HubInlineItalic{%s}")
		  (underline . "\\HubInlineUnderline{%s}")
		  (strike-through . "\\HubInlineStrike{%s}")
		  (code . hub-protected-code)
		  (verbatim . hub-protected-code)))
    (setq-local org-latex-default-footnote-command "\\footnote{\\HubFootnoteText{%s}%s}")
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
      (cond
       ((equal variant "dark-campaign")
	(hub/org-export--insert-header-extra
	 (format "\\renewcommand{\\HubHeroPatternGraphic}{\\includegraphics[width=118mm]{%s}}"
		 (hub/org-export--latex-escape
		  (expand-file-name "hero-pattern-dark.pdf" hub/org-export--active-output-dir)))))
       ((equal variant "gallery-white")
	(hub/org-export--insert-header-extra
	 "\\renewcommand{\\HubHeroPatternGraphic}{\\relax}"))
       (t
	(hub/org-export--insert-header-extra
	 (format "\\renewcommand{\\HubHeroPatternGraphic}{\\includegraphics[width=118mm]{%s}}"
		 (hub/org-export--latex-escape
		  (expand-file-name "hero-pattern.png" hub/org-export--active-output-dir)))))))
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
    (when (equal variant "gallery-white")
      (hub/org-export--insert-header-extra "% hub-veriff-gallery-white")
      (hub/org-export--insert-header-extra "\\HubVeriffVariant{gallery-white}"))
    (when (equal code-theme "dark")
      (hub/org-export--insert-header-extra "\\HubCodeThemeDark"))
    (when (equal code-theme "light")
      (hub/org-export--insert-header-extra "\\HubCodeThemeLight"))))

(defun hub/org-export--configure-hub-article-title ()
  "Configure title variables for the personal article class."
  (let ((eyebrow (hub/org-export--keyword-string "EXPORT_EYEBROW")))
    (setq-local org-export-with-toc nil)
    (hub/org-export--use-class-owned-fontspec)
    ;; Strip any toc: setting from #+OPTIONS in the buffer copy,
    ;; so the `setq-local' above is the single authority for TOC.
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "^[ \t]*#\\+OPTIONS:" nil t)
	(save-restriction
	  (narrow-to-region (line-beginning-position) (line-end-position))
	  (goto-char (point-min))
	  (while (re-search-forward "\\btoc:[^ \t]*[ \t]*" nil t)
	    (replace-match "")))))
    (setq-local org-latex-compiler (hub/org-export--effective-compiler))
    (setq-local org-latex-pdf-process
		(hub/org-export--pdf-process-for-compiler org-latex-compiler))
    (setq-local org-latex-title-command hub/org-export--hub-article-title-command)
    (setq-local org-latex-caption-above nil)
    (setq-local org-latex-src-block-backend 'minted)
    (setq-local org-latex-minted-options
		'(("fontsize" "\\footnotesize")
		  ("breaklines" "true")
		  ("autogobble" "true")
		  ("xleftmargin" "14pt")
		  ("numbersep" "8pt")))
    (setq-local org-latex-text-markup-alist
		(cons '(bold . "\\HubArticleBold{%s}")
		      (assq-delete-all 'bold (copy-tree org-latex-text-markup-alist))))
    (hub/org-export--insert-header-extra
     (format "\\renewcommand{\\HubArticleEyebrowBlock}{%s}"
	     (if (and eyebrow (not (string-empty-p eyebrow)))
		 (format "\\HubArticleEyebrow{%s}"
			 (hub/org-export--latex-escape eyebrow))
	       "")))))

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
	(when (equal (hub/org-export--effective-veriff-variant variants) "gallery-white")
	  (hub/org-export--validate-gallery-white-standfirst)))))))

(defun hub/org-export--configure-class-buffer (backend)
  "Configure export-time buffer state for BACKEND.
This runs on the temporary export buffer only."
  (when (org-export-derived-backend-p backend 'latex)
    (cond
     ((hub/org-export--veriff-p)
      (hub/org-export--configure-veriff-title)
      (hub/org-export--append-footer-note))
     ((hub/org-export--hub-article-p)
      (hub/org-export--configure-hub-article-title))
     ((hub/org-export--xelatex-class-p)
      (hub/org-export--use-class-owned-fontspec)
      (setq-local org-latex-compiler (hub/org-export--effective-compiler))
      (setq-local org-latex-pdf-process
		  (hub/org-export--pdf-process-for-compiler org-latex-compiler))))))

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


(defun hub/org-export--gallery-white-info-p (info)
  "Return non-nil when INFO targets the `gallery-white' Veriff variant."
  (and (hub/org-export--info-veriff-p info)
       (equal (hub/org-export--effective-veriff-variant) "gallery-white")))


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

(defun hub/org-export--table-row-string (cells &optional header-p header-macro body-macro)
  "Return LaTeX for a table row made of CELLS.

When HEADER-P is non-nil, render cells with the class-owned header macro."
  (let ((header-macro (or header-macro "HubTableHeaderCell"))
	(body-macro (or body-macro "HubTableBodyCell")))
    (concat
     (mapconcat
      (lambda (cell)
	(if header-p
	    (format "\\%s{%s}" header-macro cell)
	  (format "\\%s{%s}" body-macro cell)))
      cells
      " & ")
     "\\\\")))

(defun hub/org-export--hub-article-column-spec (column-count)
  "Return an adaptive hub-article table column spec for COLUMN-COUNT.

Leading columns keep natural width; the final column receives the flexible
remaining width so compact labels do not force prose cells to wrap early."
  (if (= column-count 1)
      "H"
    (concat (make-string (1- column-count) ?l) "H")))

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

(defun hub/org-export--format-hub-article-table (table info)
  "Return hub-article-owned LaTeX for plain Org TABLE using INFO."
  (pcase-let* ((`(,headers . ,body) (hub/org-export--split-table-rows table info))
	       (all-rows (append headers body))
	       (column-count (length (car all-rows)))
	       (column-spec (hub/org-export--hub-article-column-spec column-count)))
    (when (zerop column-count)
      (user-error "Cannot export an empty Org table"))
    (let* ((attr (copy-sequence (org-export-read-attribute :attr_latex table)))
	   (caption (org-latex--caption/label-string table info))
	   (above? (org-latex--caption-above-p table info))
	   (body-rows (if body body (cdr headers)))
	   (header-rows (if body headers (list (car headers))))
	   (table-string
	    (concat
	     "\\begin{HubArticleTable}\n"
	     (format "\\begin{tabularx}{\\HubArticleTableWidth}{%s}\n" column-spec)
	     (mapconcat (lambda (row)
			  (hub/org-export--table-row-string
			   row t "HubArticleTableHeaderCell" "HubArticleTableBodyCell"))
			header-rows
			"\n")
	     "\n\\HubArticleTableHeadRule\n"
	     (mapconcat
	      (lambda (row)
		(hub/org-export--table-row-string
		 row nil "HubArticleTableHeaderCell" "HubArticleTableBodyCell"))
	      body-rows
	      "\n")
	     "\n\\end{tabularx}\n"
	     "\\end{HubArticleTable}")))
      (unless (plist-member attr :center)
	(setq attr (plist-put attr :center nil)))
      (org-latex--decorate-table table-string attr caption above? info))))

(defun hub/org-export--advice-org-latex-org-table (orig table contents info)
  "Render TABLE through ORIG, with branded defaults for the flagship class."
  (let ((attr (org-export-read-attribute :attr_latex table)))
    (if (and (not (plist-get attr :environment))
	     (not (plist-get attr :mode))
	     (not (plist-get attr :align))
	     (not (plist-get attr :width))
	     (not (plist-get attr :options)))
	(cond
	 ((hub/org-export--info-veriff-p info)
	  (hub/org-export--format-branded-table table info))
	 ((equal (plist-get info :latex-class) hub/org-export--hub-article-class-name)
	  (hub/org-export--format-hub-article-table table info))
	 (t (funcall orig table contents info)))
      (funcall orig table contents info))))

(defun hub/org-export--advice-org-latex-special-block (orig special-block contents info)
  "Apply Veriff-specific LaTeX handling to SPECIAL-BLOCK before ORIG.
Escape metric `:options' values and allow graph blocks to opt into full-width
two-column rendering with `:float multicolumn'."
  (let* ((attr-latex (org-element-property :attr_latex special-block))
	 (attr (org-export-read-attribute :attr_latex special-block)))
    (if (and (hub/org-export--gallery-white-info-p info)
	     (equal (org-element-property :type special-block) "standfirst"))
	(format "\\begin{standfirst}\n%s\n\\end{standfirst}\n}]\n" contents)
      (when (equal (org-element-property :type special-block) "callout")
	(if-let* ((title (hub/org-callout-title special-block)))
	    (org-element-put-property special-block :attr_latex
				      (list (format ":options [%s]"
						    (hub/org-export--latex-escape title))))
	  (org-element-put-property special-block :attr_latex nil)))
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
	  (org-element-put-property special-block :attr_latex new-attr)))
      (when (and (hub/org-export--info-veriff-p info)
		 (equal (org-element-property :type special-block) "graph")
		 (equal (plist-get attr :float) "multicolumn"))
	(org-element-put-property special-block :type "graph*"))
      (funcall orig special-block contents info))))

(defun hub/org-export--advice-org-latex--text-markup (orig text markup info)
  "Render `hub-protected-code' using `\\HubInlineCode' while escaping special characters."
  (let ((fmt (cdr (assq markup (plist-get info :latex-text-markup-alist)))))
    (if (eq fmt 'hub-protected-code)
	(format "\\HubInlineCode{%s}"
		(replace-regexp-in-string
		 "--\\|<<\\|>>\\|[\\{}$%&_#~^]"
		 (lambda (m)
		   (cond ((equal m "--") "-{}-{}")
			 ((equal m "<<") "<{}<{}")
			 ((equal m ">>") ">{}>{}")
			 ((equal m "\\") "\\textbackslash{}")
			 ((equal m "~") "\\textasciitilde{}")
			 ((equal m "^") "\\textasciicircum{}")
			 (t (org-latex--protect-text m))))
		 text nil t))
      (funcall orig text markup info))))

(defun hub/org-export--image-source-path (path info)
  "Return an absolute source path for relative image PATH using INFO."
  (unless (file-name-absolute-p path)
    (seq-find #'file-exists-p
	      (delq nil
		    (list
		     (and hub/org-export--active-output-dir
			  (expand-file-name path hub/org-export--active-output-dir))
		     (when-let* ((input-file (plist-get info :input-file)))
		       (expand-file-name path (file-name-directory input-file))))))))

(defun hub/org-export--advice-org-latex--inline-image (orig link info)
  "Render Org image LINK through class-owned figure image wrappers using INFO."
  (let ((image-latex (funcall orig link info)))
    (if (string-match "\\\\includegraphics\\(\\[[^]]*\\]\\)?{\\([^}]+\\)}" image-latex)
	(let ((includegraphics (match-string 0 image-latex)))
	  (cond
	   ((hub/org-export--info-veriff-p info)
	    (let* ((path (match-string 2 image-latex))
		   (source-path (hub/org-export--image-source-path path info)))
	      (when source-path
		(setq includegraphics
		      (replace-regexp-in-string
		       (format "{%s}\\'" (regexp-quote path))
		       (format "{%s}" (hub/org-export--latex-escape source-path))
		       includegraphics t t)))
	      (replace-match (format "\\HubFigureImage{%s}" includegraphics) t t image-latex)))
	   ((equal (plist-get info :latex-class) hub/org-export--hub-article-class-name)
	    (replace-match (format "\\HubArticleFigureImage{%s}" includegraphics) t t image-latex))
	   (t image-latex)))
      image-latex)))

(defun hub/org-export--hub-note-kind-from-region (begin end)
  "Return HUB_NOTE_KIND between BEGIN and END, or nil."
  (save-excursion
    (save-restriction
      (narrow-to-region begin end)
      (goto-char (point-min))
      (when (re-search-forward "^[ \t]*:HUB_NOTE_KIND:[ \t]*\\([^ \t\n]+\\)" nil t)
	(downcase (match-string-no-properties 1))))))

(defun hub/org-export--hub-note-kind-from-label (label)
  "Return HUB_NOTE_KIND for footnote LABEL in the current export buffer."
  (when label
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward (format "^[ \t]*\\[fn:%s\\]" (regexp-quote label)) nil t)
	(let ((begin (line-beginning-position))
	      (end (save-excursion
		     (forward-line 1)
		     (if (re-search-forward "^[ \t]*\\(?:\\[fn:[^]\n]+\\]\\|\\*+\\s-+\\)" nil t)
			 (match-beginning 0)
		       (point-max)))))
	  (hub/org-export--hub-note-kind-from-region begin end))))))

(defun hub/org-export--hub-note-kind-from-definition (definition &optional label)
  "Return HUB_NOTE_KIND from footnote DEFINITION or LABEL, or nil."
  (or (when-let* ((begin (org-element-property :begin definition))
		  (end (org-element-property :end definition)))
	(hub/org-export--hub-note-kind-from-region begin end))
      (hub/org-export--hub-note-kind-from-label label)))

(defun hub/org-export--hub-article-footnote-body (definition info)
  "Return LaTeX body for footnote DEFINITION using INFO, excluding metadata."
  (string-trim
   (mapconcat
    (lambda (child)
      (if (and (eq (org-element-type child) 'drawer)
	       (equal (org-element-property :drawer-name child) "PROPERTIES"))
	  ""
	(org-export-data child info)))
    (org-element-contents definition)
    "")))

(defun hub/org-export--latex-footnote-separator (footnote-reference info)
  "Return LaTeX separator before FOOTNOTE-REFERENCE using INFO, if needed."
  (let ((previous (org-export-get-previous-element footnote-reference info)))
    (if (org-element-type-p previous 'footnote-reference)
	(plist-get info :latex-footnote-separator)
      "")))

(defun hub/org-export--hub-article-footnote-label (label definition footnote-reference info)
  "Return repeated footnote LABEL for DEFINITION and FOOTNOTE-REFERENCE using INFO."
  (cond
   ((not label) "")
   ((org-element-map (plist-get info :parse-tree)
		     'footnote-reference
		     (lambda (reference)
		       (and (not (eq reference footnote-reference))
			    (equal (org-element-property :label reference) label)
			    (org-trim (org-latex--label definition info t t))))
		     info t))
   (t "")))

(defun hub/org-export--advice-org-latex-footnote-reference (orig footnote-reference contents info)
  "Render hub-article FOOTNOTE-REFERENCE as sidenote-aware LaTeX.
ORIG, CONTENTS, and INFO follow `org-latex-footnote-reference'."
  (if (not (hub/org-export--info-hub-article-p info))
      (funcall orig footnote-reference contents info)
    (let ((label (org-element-property :label footnote-reference)))
      (cond
       ((not (org-export-footnote-first-reference-p footnote-reference info))
	(funcall orig footnote-reference contents info))
       ((or (org-element-lineage footnote-reference
				 '(footnote-reference footnote-definition table-cell verse-block))
	    (org-element-type-p (org-element-parent-element footnote-reference) 'item))
	(funcall orig footnote-reference contents info))
       (t
	(let* ((definition (org-export-get-footnote-definition footnote-reference info))
	       (kind (hub/org-export--hub-note-kind-from-definition definition label))
	       (body (hub/org-export--hub-article-footnote-body definition info))
	       (separator (hub/org-export--latex-footnote-separator footnote-reference info)))
	  (pcase kind
	    ("footnote"
	     (concat separator
		     (format (plist-get info :latex-default-footnote-command)
			     body
			     (hub/org-export--hub-article-footnote-label
			      label definition footnote-reference info))
		     (org-latex--delayed-footnotes-definitions definition info)))
	    (_
	     (concat separator
		     (format "\\HubArticleSidenote{%s}" body)
		     (org-latex--delayed-footnotes-definitions definition info))))))))))

(defun hub/org-export--advice-org-latex-item (orig item contents info)
  "Render Org checkbox ITEM markers through Veriff-owned macros using CONTENTS and INFO."
  (let ((item-latex (funcall orig item contents info)))
    (cond
     ((and (equal (plist-get info :latex-class) hub/org-export--hub-article-class-name)
	   (org-element-property :tag item)
	   (not (org-element-property :checkbox item)))
      (let* ((tag (org-export-data (org-element-property :tag item) info))
	     (tag-footnotes
	      (or (org-latex--delayed-footnotes-definitions
		   (org-element-property :tag item) info)
		  "")))
	(format "\\item[{\\HubArticleDefinitionTerm{%s}}] %s%s"
		tag
		tag-footnotes
		(or (and contents (org-trim contents)) ""))))
     ((hub/org-export--info-veriff-p info)
      (let ((result item-latex))
	(dolist (pair '(("$\\boxtimes$" . "\\HubCheckboxChecked{}")
			("$\\square$" . "\\HubCheckboxUnchecked{}")
			("$\\boxminus$" . "\\HubCheckboxPartial{}")))
	  (setq result (string-replace (car pair) (cdr pair) result)))
	result))
     (t item-latex))))

(defun hub/org-export--register-latex-class (name header)
  "Register LaTeX class NAME with HEADER and standard sectioning mappings."
  (setq org-latex-classes
	(cons
	 (append (list name header hub/org-export--standard-sectioning)
		 hub/org-export--standard-subsectioning)
	 (cl-remove-if (lambda (entry)
			 (equal (car entry) name))
		       org-latex-classes))))

(defun hub/org-export--register-local-latex-classes ()
  "Register discovered local .cls assets not already registered specially."
  (dolist (class (hub/org-export--local-class-names))
    (unless (assoc-string class org-latex-classes t)
      (hub/org-export--register-latex-class
       class
       (string-join
	(list (format "\\documentclass[11pt,a4paper]{%s}" class)
	      "[DEFAULT-PACKAGES]"
	      "[PACKAGES]")
	"\n")))))

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

(defun hub/org-export-register-hub-article ()
  "Register the public personal article Org LaTeX export class."
  (hub/org-export--register-latex-class
   hub/org-export--hub-article-class-name
   (string-join
    '("\\documentclass[11pt,a4paper]{hub-article}"
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

When OUTPUT-DIR is nil, export into the same directory as the Org file,
so that relative references (images, includes) resolve naturally.
Falls back to `hub/org-export-output-root' for buffers without a file.

Return the generated `.tex' path."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Current buffer is not in Org mode"))
  (hub/org-export--validate-locale 'latex)
  (hub/org-export--validate-veriff-metadata 'latex)
  (hub/org-export--ensure-babel-package)
  (let* ((default-dir (and (buffer-file-name)
			   (file-name-directory (buffer-file-name))))
	 (outdir (hub/org-export--ensure-output-directory
		  (or output-dir default-dir)))
	 (hub/org-export--active-output-dir outdir)
	 (org-export-show-temporary-export-buffer nil)
	 (base-name (file-name-base (or (buffer-file-name) (buffer-name))))
	 (tex-path (expand-file-name (concat base-name ".tex") outdir)))
    (hub/org-export--stage-class-asset outdir)
    (org-export-to-file 'latex tex-path nil nil nil nil nil)))

(defun hub/org-export-buffer-to-pdf (&optional output-dir)
  "Export the current Org buffer to PDF in OUTPUT-DIR.

When OUTPUT-DIR is nil, export into the same directory as the Org file
(see `hub/org-export-buffer-to-latex').  Return the generated `.pdf' path."
  (interactive)
  (let* ((compiler (and (hub/org-export--xelatex-class-p)
			(hub/org-export--effective-compiler)))
	 (org-latex-compiler (or compiler org-latex-compiler))
	 (org-latex-pdf-process (if compiler
				    (hub/org-export--pdf-process-for-compiler compiler)
				  hub/org-export-pdf-process))
	 (tex-path (hub/org-export-buffer-to-latex output-dir))
	 (pdf-path (concat (file-name-sans-extension tex-path) ".pdf")))
    (let ((default-directory (file-name-directory tex-path)))
      (org-latex-compile tex-path))
    pdf-path))

(hub/org-export-register-veriff)
(hub/org-export-register-hub-article)
(hub/org-export--register-local-latex-classes)
(hub/org-export--ensure-babel-package)
(hub/org-export--ensure-texinputs)
(advice-add 'org-latex--org-table :around #'hub/org-export--advice-org-latex-org-table)
(advice-add 'org-latex-special-block :around #'hub/org-export--advice-org-latex-special-block)
(advice-add 'org-latex--format-spec :around #'hub/org-export--advice-org-latex--format-spec)
(advice-add 'org-latex--text-markup :around #'hub/org-export--advice-org-latex--text-markup)
(advice-add 'org-latex--inline-image :around #'hub/org-export--advice-org-latex--inline-image)
(advice-add 'org-latex-item :around #'hub/org-export--advice-org-latex-item)
(advice-add 'org-latex-footnote-reference :around #'hub/org-export--advice-org-latex-footnote-reference)
(defun hub/org-export--advice-org-latex-template (orig contents info)
  "Remove TOC boilerplate from hub-article LaTeX template.
This is a belt-and-suspenders guard: the class config already sets
`org-export-with-toc' to nil, but subtree export and export-order
timing can let `:with-toc' slip through.  Stripping at the output
level is the definitive fix."
  (let ((result (funcall orig contents info)))
    (if (hub/org-export--info-hub-article-p info)
	(replace-regexp-in-string
	 "\\\\setcounter{tocdepth}{[0-9]+}\n\\\\tableofcontents\n"
	 "" result)
      result)))

(advice-add 'org-latex-compile :around #'hub/org-export--advice-org-latex-compile)
(advice-add 'org-latex-template :around #'hub/org-export--advice-org-latex-template)
(add-hook 'org-export-before-processing-functions #'hub/org-export--validate-locale)
(add-hook 'org-export-before-processing-functions #'hub/org-export--validate-veriff-metadata)
(add-hook 'org-export-before-processing-functions #'hub/org-export--configure-class-buffer)


;; ── Drop cap insertion ────────────────────────────────────────────────

(defun hub/org-insert-dropcap (beg end)
  "Wrap region from BEG to END with \\HubArticleDropCap.
The first character becomes the drop cap letter (dropping 2 lines),
the rest becomes the small-caps lead words.

Example: selecting [Hello] produces:
  \\HubArticleDropCap{H}{ello}"
  (interactive "r")
  (if (use-region-p)
      (let* ((text (buffer-substring-no-properties beg end))
	     (clean (string-trim text))
	     (first (substring clean 0 1))
	     (rest (substring clean 1)))
	(delete-region beg end)
	(insert (format "\\HubArticleDropCap{%s}{%s}" first rest)))
    (user-error "Select the word(s) to transform into a drop cap")))

(defun hub/org-read-latex-class-and-variant ()
  "Prompt for LaTeX class and optionally its variant.
Returns both class and variant header lines as a string."
  (let* ((class (hub/org-read-latex-class))
	 (variant
	  (and (equal class hub/org-export--veriff-class-name)
	       (let ((v (completing-read
			 "Variant (default refresh-overdrive): "
			 hub/org-export--veriff-variants
			 nil t nil nil "refresh-overdrive")))
		 (and (not (string-empty-p v)) v))))
	 (class-line (format "#+LATEX_CLASS: %s" class)))
    (if variant
	(format "%s\n#+LATEX_VARIANT: %s" class-line variant)
      class-line)))

(hub/leader-bind :states '(normal visual) :keymap 'org-mode-map
		 :label "Drop cap"
		 "x d" #'hub/org-insert-dropcap)

(provide 'org/export-latex)
;;; export-latex.el ends here
