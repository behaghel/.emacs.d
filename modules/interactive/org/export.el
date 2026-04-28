;;; export.el --- Org LaTeX export classes and helpers -*- lexical-binding: t; -*-

;;; Commentary:
;; Minimal Org -> LaTeX -> PDF export support for the first pro-refresh-overdrive
;; vertical slice.

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
  '("pdflatex -interaction nonstopmode -output-directory %o %f"
    "pdflatex -interaction nonstopmode -output-directory %o %f")
  "Commands used to compile exported LaTeX files into PDFs."
  :type '(repeat string)
  :group 'hub/org-export)

(defcustom hub/org-export-pro-refresh-overdrive-preferred-compiler "xelatex"
  "Preferred LaTeX compiler for `pro-refresh-overdrive'.

This is the fidelity-oriented engine path for brand-font work.  The
implementation will automatically fall back to `pdflatex' until the TeX
environment provides the packages needed for that engine path, notably
`fontspec'."
  :type '(choice (const "pdflatex")
		 (const "xelatex")
		 (const "lualatex"))
  :group 'hub/org-export)

(defcustom hub/org-export-pro-refresh-overdrive-main-font "Inter"
  "Main text and sans-serif font family for `pro-refresh-overdrive'."
  :type 'string
  :group 'hub/org-export)

(defconst hub/org-export--babel-package '("AUTO" "babel" nil)
  "Package entry enabling locale-aware Babel wiring in Org LaTeX exports.")

(defvar hub/org-export--font-probe-cache (make-hash-table :test 'equal)
  "Cache of compiler and font probe results for Org export.")

(defconst hub/org-export--pro-refresh-overdrive-title-command
  (string-join
   '("\\begin{hubhero}"
     "\\HubExportEyebrowBlock"
     "\\noindent\\parbox[t]{0.78\\linewidth}{{\\raggedright\\fontsize{34}{33}\\selectfont\\bfseries %t\\par}}"
     "\\vspace{0.45em}"
     "{\\color{HubMuted}\\noindent\\parbox[t]{0.72\\linewidth}{\\raggedright\\normalsize %s\\par}}"
     "\\vspace{0.9em}"
     "{\\color{HubMuted}\\small %a\\HubMetaSeparator %D\\par}"
     "\\end{hubhero}")
   "\n")
  "Title command used for the `pro-refresh-overdrive' LaTeX class.")

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
  (when-let* ((entry (assoc-string keyword (org-collect-keywords (list keyword)) t))
	      (values (cdr entry)))
    (car values)))

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

(defun hub/org-export--fontspec-font-ready-p (compiler font-name)
  "Return non-nil when COMPILER can use FONT-NAME through fontspec.

The result is cached per compiler and font family so normal exports do not
re-run the probe repeatedly."
  (let ((cache-key (list compiler font-name)))
    (if-let* ((cached (gethash cache-key hub/org-export--font-probe-cache 'missing))
	      ((not (eq cached 'missing))))
	cached
      (let ((result
	     (when-let* ((compiler-bin (executable-find compiler))
			 (tmpdir (make-temp-file "hub-org-fontspec-" t))
			 (texfile (expand-file-name "probe.tex" tmpdir)))
	       (unwind-protect
		   (progn
		     (with-temp-file texfile
		       (insert "\\documentclass{article}\n"
			       "\\usepackage{fontspec}\n"
			       (format "\\setmainfont{%s}\n" font-name)
			       "\\begin{document}\n"
			       "Probe\n"
			       "\\end{document}\n"))
		     (eq 0 (call-process compiler-bin nil nil nil
					 "-interaction=nonstopmode"
					 "-halt-on-error"
					 (format "-output-directory=%s" tmpdir)
					 texfile)))
		 (when (file-directory-p tmpdir)
		   (delete-directory tmpdir t))))))
	(puthash cache-key result hub/org-export--font-probe-cache)
	result))))

(defun hub/org-export--compiler-ready-p (compiler)
  "Return non-nil when COMPILER is ready for use in this environment."
  (pcase compiler
    ((or "xelatex" "lualatex")
     (hub/org-export--fontspec-font-ready-p
      compiler hub/org-export-pro-refresh-overdrive-main-font))
    (_ t)))

(defun hub/org-export--effective-compiler ()
  "Return the compiler to use for the current export.

Prefer the class-specific fidelity engine when the environment is ready;
otherwise fall back to `pdflatex' so the current pipeline keeps working."
  (let ((preferred hub/org-export-pro-refresh-overdrive-preferred-compiler))
    (if (hub/org-export--compiler-ready-p preferred)
	preferred
      "pdflatex")))

(defun hub/org-export--pdf-process-for-compiler (compiler)
  "Return a minimal two-pass PDF process for COMPILER."
  (let ((command (format "%s -interaction nonstopmode -output-directory %%o %%f"
			 compiler)))
    (list command command)))

(defun hub/org-export--class-name ()
  "Return the current Org LaTeX class name from buffer metadata."
  (hub/org-export--keyword-string "LATEX_CLASS"))

(defun hub/org-export--pro-refresh-overdrive-p ()
  "Return non-nil when current buffer exports with `pro-refresh-overdrive'."
  (equal (hub/org-export--class-name) "pro-refresh-overdrive"))

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

(defun hub/org-export--configure-pro-refresh-overdrive-title ()
  "Configure title and chrome variables for the flagship class."
  (let ((eyebrow (hub/org-export--keyword-string "EXPORT_EYEBROW")))
    (setq-local org-export-with-toc nil)
    (setq-local org-latex-compiler (hub/org-export--effective-compiler))
    (setq-local org-latex-pdf-process
		(hub/org-export--pdf-process-for-compiler org-latex-compiler))
    (setq-local org-latex-title-command hub/org-export--pro-refresh-overdrive-title-command)
    (setq-local org-latex-src-block-backend 'custom)
    (setq-local org-latex-custom-lang-environments
		'((emacs-lisp "hubcode")
		  (elisp "hubcode")
		  (python "hubcode")
		  (bash "hubcode")
		  (sh "hubcode")
		  (shell "hubcode")))
    (unless (string= org-latex-compiler "pdflatex")
      (hub/org-export--insert-header-extra "\\defaultfontfeatures{Ligatures=TeX}")
      (hub/org-export--insert-header-extra
       (format "\\setmainfont{%s}"
	       (hub/org-export--latex-escape hub/org-export-pro-refresh-overdrive-main-font)))
      (hub/org-export--insert-header-extra
       (format "\\setsansfont{%s}"
	       (hub/org-export--latex-escape hub/org-export-pro-refresh-overdrive-main-font))))
    (hub/org-export--insert-header-extra
     (format "\\renewcommand{\\HubExportEyebrowBlock}{%s}"
	     (if (and eyebrow (not (string-empty-p eyebrow)))
		 (format "{\\color{HubAccent}\\small\\bfseries\\MakeUppercase{%s}\\par}\\vspace{0.6em}"
			 (hub/org-export--latex-escape eyebrow))
	       "")))))

(defun hub/org-export--configure-class-buffer (backend)
  "Configure export-time buffer state for BACKEND.
This runs on the temporary export buffer only."
  (when (and (org-export-derived-backend-p backend 'latex)
	     (hub/org-export--pro-refresh-overdrive-p))
    (hub/org-export--configure-pro-refresh-overdrive-title)
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

(defun hub/org-export-register-pro-refresh-overdrive ()
  "Register the first professional Org LaTeX export class."
  (hub/org-export--register-latex-class
   "pro-refresh-overdrive"
   (string-join
    '("\\documentclass[11pt,a4paper]{article}"
      "% hub-pro-refresh-overdrive"
      "% hub-pro-refresh-overdrive-title"
      "% hub-pro-refresh-overdrive-headings"
      "[DEFAULT-PACKAGES]"
      "[PACKAGES]"
      "\\usepackage{geometry}"
      "\\geometry{margin=25mm}"
      "\\usepackage{xcolor}"
      "\\usepackage{array}"
      "\\usepackage{tabularx}"
      "\\usepackage{colortbl}"
      "\\usepackage{fancyhdr}"
      "\\definecolor{HubPaper}{HTML}{FCF5EE}"
      "\\definecolor{HubInk}{HTML}{0C3035}"
      "\\definecolor{HubAccent}{HTML}{FF550F}"
      "\\definecolor{HubMuted}{HTML}{543F21}"
      "\\definecolor{HubLine}{HTML}{D8B5A4}"
      "\\definecolor{HubSurface}{HTML}{FFF4EC}"
      "\\AtBeginDocument{\\pagecolor{HubPaper}\\color{HubInk}}"
      "\\renewcommand{\\familydefault}{\\sfdefault}"
      "\\pagestyle{fancy}"
      "\\fancyhf{}"
      "\\renewcommand{\\headrulewidth}{0.4pt}"
      "\\fancyhead[L]{\\color{HubMuted}\\footnotesize\\textsf{Veriff}}"
      "\\fancyhead[R]{\\color{HubMuted}\\footnotesize\\textsf{\\nouppercase{\\rightmark}}}"
      "\\fancyfoot[C]{\\color{HubMuted}\\footnotesize\\textsf{\\thepage}}"
      "\\setlength{\\parindent}{0pt}"
      "\\setlength{\\parskip}{0.6em}"
      "\\setlength{\\tabcolsep}{5pt}"
      "\\renewcommand{\\arraystretch}{1.2}"
      "\\arrayrulecolor{HubLine}"
      "\\makeatletter"
      "\\newcommand{\\HubExportEyebrowBlock}{}"
      "\\newcommand{\\HubMetaSeparator}{\\hspace{0.8em}{\\color{HubLine}\\textbullet}\\hspace{0.8em}}"
      "\\renewcommand{\\sectionmark}[1]{\\markright{#1}}"
      "\\def\\@seccntformat#1{{\\color{HubAccent}\\csname the#1\\endcsname}\\hspace{0.75em}}"
      "\\renewcommand\\section{\\@startsection{section}{1}{\\z@}{2.8ex}{1.1ex}{\\normalfont\\Large\\bfseries\\raggedright\\color{HubInk}}}"
      "\\renewcommand\\subsection{\\@startsection{subsection}{2}{\\z@}{2.2ex}{0.9ex}{\\normalfont\\large\\bfseries\\raggedright\\color{HubMuted}}}"
      "\\makeatother"
      "\\newenvironment{hubhero}{\\par\\noindent\\rule{\\linewidth}{0.8pt}\\par\\vspace{1.2em}\\noindent}{\\par\\vspace{1.0em}\\noindent\\color{HubLine}\\rule{\\linewidth}{0.8pt}\\par\\vspace{1.2em}}"
      "\\renewenvironment{quote}{\\par\\medskip\\noindent\\color{HubInk}\\itshape\\large}{\\par\\medskip}"
      "\\newenvironment{standfirst}{\\par\\medskip\\noindent\\color{HubMuted}\\large\\bfseries}{\\par\\medskip}"
      "\\newenvironment{pullquote}[1][]{\\def\\HubPullquoteAttribution{#1}\\par\\medskip\\noindent\\itshape\\Large\\color{HubInk}}{\\par\\ifx\\HubPullquoteAttribution\\empty\\else{\\raggedleft\\normalfont\\small\\color{HubMuted}\\HubPullquoteAttribution\\par}\\fi\\medskip}"
      "\\newenvironment{callout}[1][]{\\par\\medskip\\noindent\\setlength{\\fboxsep}{8pt}\\fcolorbox{HubLine}{HubSurface}{\\begin{minipage}{0.94\\linewidth}\\ifx\\relax#1\\relax\\else{\\color{HubMuted}\\bfseries\\small\\MakeUppercase{#1}\\par\\smallskip}\\fi\\small\\color{HubInk}\\ignorespaces}}{\\end{minipage}\\par\\medskip}"
      "\\newenvironment{metrics}{\\par\\medskip\\noindent}{\\par\\medskip}"
      "\\newenvironment{metric}[1][]{\\begin{minipage}[t]{0.235\\linewidth}\\raggedright\\textcolor{HubAccent}{\\bfseries\\Large #1}\\par\\small\\color{HubMuted}\\ignorespaces}{\\par\\end{minipage}\\hfill}"
      "\\newenvironment{pillars}{\\par\\medskip\\noindent}{\\par\\medskip}"
      "\\newenvironment{pillar}[1][]{\\begin{minipage}[t]{0.48\\linewidth}\\setlength{\\fboxsep}{6pt}\\fcolorbox{HubLine}{white}{\\begin{minipage}[t]{0.9\\linewidth}\\textbf{#1}\\par\\smallskip\\small\\color{HubInk}\\ignorespaces}}{\\end{minipage}\\end{minipage}\\hfill}"
      "\\newenvironment{hubcode}{\\par\\medskip\\noindent{\\color{HubAccent}\\rule{\\linewidth}{0.8pt}}\\par\\smallskip\\ttfamily\\small\\color{HubInk}}{\\par\\smallskip\\noindent{\\color{HubAccent}\\rule{\\linewidth}{0.8pt}}\\par\\medskip}"
      "\\newenvironment{hubfootnote}{\\par\\medskip\\footnotesize\\color{HubMuted}}{\\par}"
      "\\hypersetup{colorlinks=true,linkcolor=HubAccent,urlcolor=HubAccent}")
    "\n")))

(defun hub/org-export--ensure-output-directory (&optional output-dir)
  "Return OUTPUT-DIR with directories created, or the default export root."
  (let ((dir (file-name-as-directory (or output-dir hub/org-export-output-root))))
    (unless (file-directory-p dir)
      (make-directory dir t))
    dir))

(defun hub/org-export-buffer-to-latex (&optional output-dir)
  "Export the current Org buffer to LaTeX in OUTPUT-DIR.
Return the generated `.tex' path."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Current buffer is not in Org mode"))
  (hub/org-export--validate-locale 'latex)
  (hub/org-export--ensure-babel-package)
  (let* ((outdir (hub/org-export--ensure-output-directory output-dir))
	 (org-export-show-temporary-export-buffer nil)
	 (base-name (file-name-base (or (buffer-file-name) (buffer-name))))
	 (tex-path (expand-file-name (concat base-name ".tex") outdir)))
    (org-export-to-file 'latex tex-path nil nil nil nil nil)))

(defun hub/org-export-buffer-to-pdf (&optional output-dir)
  "Export the current Org buffer to PDF in OUTPUT-DIR.
Return the generated `.pdf' path."
  (interactive)
  (let* ((org-latex-pdf-process hub/org-export-pdf-process)
	 (tex-path (hub/org-export-buffer-to-latex output-dir))
	 (pdf-path (concat (file-name-sans-extension tex-path) ".pdf")))
    (org-latex-compile tex-path)
    pdf-path))

(hub/org-export-register-pro-refresh-overdrive)
(hub/org-export--ensure-babel-package)
(add-hook 'org-export-before-processing-functions #'hub/org-export--validate-locale)
(add-hook 'org-export-before-processing-functions #'hub/org-export--configure-class-buffer)

(provide 'org/export)
;;; export.el ends here
