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
  '("xelatex -interaction nonstopmode -output-directory %o %f"
    "xelatex -interaction nonstopmode -output-directory %o %f")
  "Commands used to compile exported LaTeX files into PDFs."
  :type '(repeat string)
  :group 'hub/org-export)

(defconst hub/org-export--pro-refresh-overdrive-compiler "xelatex"
  "Required LaTeX compiler for `pro-refresh-overdrive'.")

(defconst hub/org-export--babel-package '("AUTO" "babel" nil)
  "Package entry enabling locale-aware Babel wiring in Org LaTeX exports.")

(defconst hub/org-export--pro-refresh-overdrive-class-file
  (expand-file-name "etc/latex/pro-refresh-overdrive.cls" user-emacs-directory)
  "Tracked LaTeX class asset for `pro-refresh-overdrive'.")

(defconst hub/org-export--pro-refresh-overdrive-assets-directory
  (expand-file-name "assets" (file-name-directory hub/org-export--pro-refresh-overdrive-class-file))
  "Tracked helper asset directory for `pro-refresh-overdrive'.")

(defvar hub/org-export--active-output-dir nil
  "Current export artifact directory for helper staging and header wiring.")

(defconst hub/org-export--pro-refresh-overdrive-title-command
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

(defun hub/org-export--class-asset-source (&optional class-name)
  "Return the tracked class asset path for CLASS-NAME.
Default to the current buffer's LaTeX class when CLASS-NAME is nil."
  (pcase (or class-name (hub/org-export--class-name))
    ("pro-refresh-overdrive" hub/org-export--pro-refresh-overdrive-class-file)
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
       (file-exists-p hub/org-export--pro-refresh-overdrive-class-file)))

(defun hub/org-export--effective-compiler ()
  "Return the compiler to use for the current export.

Prefer the class-specific fidelity engine when the environment is ready;
otherwise fall back to `pdflatex' so the current pipeline keeps working."
  (if (hub/org-export--compiler-ready-p "xelatex")
      "xelatex"
    (user-error "XeLaTeX and the pro-refresh-overdrive class asset are required for this export")))

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
    (when hub/org-export--active-output-dir
      (hub/org-export--insert-header-extra
       (format "\\renewcommand{\\HubHeroLogoGraphic}{\\includegraphics[width=32mm]{%s}}"
	       (hub/org-export--latex-escape
		(expand-file-name "hero-logo.pdf" hub/org-export--active-output-dir))))
      (hub/org-export--insert-header-extra
       (format "\\renewcommand{\\HubHeroPatternGraphic}{\\includegraphics[width=118mm]{%s}}"
	       (hub/org-export--latex-escape
		(expand-file-name "hero-pattern.png" hub/org-export--active-output-dir)))))
    (hub/org-export--insert-header-extra
     (format "\\renewcommand{\\HubExportEyebrowBlock}{%s}"
	     (if (and eyebrow (not (string-empty-p eyebrow)))
		 (format "\\HubHeroEyebrow{%s}"
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
    '("\\documentclass[11pt,a4paper]{pro-refresh-overdrive}"
      "% hub-pro-refresh-overdrive"
      "% hub-pro-refresh-overdrive-title"
      "% hub-pro-refresh-overdrive-headings"
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
      (when (equal resolved-class "pro-refresh-overdrive")
	(when (file-directory-p hub/org-export--pro-refresh-overdrive-assets-directory)
	  (dolist (asset (directory-files hub/org-export--pro-refresh-overdrive-assets-directory t "\\.\\(png\\|pdf\\)\\'"))
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
  (let* ((compiler (and (hub/org-export--pro-refresh-overdrive-p)
			(hub/org-export--effective-compiler)))
	 (org-latex-compiler (or compiler org-latex-compiler))
	 (org-latex-pdf-process (if compiler
				    (hub/org-export--pdf-process-for-compiler compiler)
				  hub/org-export-pdf-process))
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
