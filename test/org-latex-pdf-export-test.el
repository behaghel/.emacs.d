;;; org-latex-pdf-export-test.el --- Org LaTeX PDF slice tests -*- lexical-binding: t; -*-

;;; Commentary:
;; ERT coverage for the Org -> LaTeX -> PDF export slices.

;;; Code:

(require 'cl-lib)
(require 'ert)

(defconst hub/test-repo-root
  (file-name-as-directory
   (expand-file-name ".." (file-name-directory (or load-file-name buffer-file-name))))
  "Repository root for Org LaTeX PDF export tests.")

(let ((user-emacs-directory hub/test-repo-root)
      (default-directory hub/test-repo-root))
  (add-to-list 'load-path (expand-file-name "core" user-emacs-directory))
  (add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
  (add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
  (add-to-list 'load-path (expand-file-name "modules/lang" user-emacs-directory))
  (add-to-list 'load-path (expand-file-name "modules/interactive" user-emacs-directory))
  (require 'core-packages)
  (require 'editing/evil)
  (require 'org/core)
  (require 'org/export))

(load-file (expand-file-name "scripts/approval-refresh-overdrive-page1.el" hub/test-repo-root))

(defun hub/test-read-file-as-string (path)
  "Return PATH contents as a string."
  (with-temp-buffer
    (insert-file-contents path)
    (buffer-string)))

(defun hub/test-exported-class-path (artifact-root class-name)
  "Return the staged class path for CLASS-NAME in ARTIFACT-ROOT."
  (expand-file-name (format "%s.cls" class-name) artifact-root))

(defun hub/test-make-export-artifact-root ()
  "Create a temporary artifact root under `var/'."
  (make-temp-file (expand-file-name "var/org-latex-pdf-test-" hub/test-repo-root) t))

(defmacro hub/test-with-export-compiler-readiness (ready-p &rest body)
  "Run BODY with exporter readiness forced to READY-P."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'hub/org-export--compiler-ready-p)
	      (lambda (_compiler) ,ready-p)))
     ,@body))

(defmacro hub/test-with-stubbed-latex-compile (&rest body)
  "Run BODY with `org-latex-compile' replaced by a PDF stub writer."
  (declare (indent 0))
  `(cl-letf (((symbol-function 'org-latex-compile)
	      (lambda (tex-path)
		(with-temp-file (concat (file-name-sans-extension tex-path) ".pdf")
		  (insert "stub")))))
     ,@body))

(ert-deftest hub/script-export-approval-refresh-overdrive-page1-prints-buffer-and-path ()
  "The approval export helper prints LaTeX output and the PDF path."
  (let ((log-buffer (get-buffer-create hub/script-org-pdf-output-buffer-name))
	(specimen-buffer (generate-new-buffer " *approval-specimen*"))
	output)
    (unwind-protect
	(progn
	  (with-current-buffer log-buffer
	    (erase-buffer)
	    (insert "latex output\nline two\n"))
	  (setq output
		(with-temp-buffer
		  (let ((standard-output (current-buffer)))
		    (cl-letf (((symbol-function 'hub/script--setup-load-paths)
			       (lambda (_repo-root) nil))
			      ((symbol-function 'find-file-noselect)
			       (lambda (_path) specimen-buffer))
			      ((symbol-function 'hub/org-export-buffer-to-pdf)
			       (lambda (_artifact-root) "/tmp/approval.pdf")))
		      (hub/script-export-approval-refresh-overdrive-page1 hub/test-repo-root))
		    (buffer-string))))
	  (should (string-match-p (regexp-quote "[Org PDF LaTeX Output] BEGIN") output))
	  (should (string-match-p (regexp-quote "latex output") output))
	  (should (string-match-p (regexp-quote "/tmp/approval.pdf") output)))
      (when (buffer-live-p log-buffer)
	(kill-buffer log-buffer))
      (when (buffer-live-p specimen-buffer)
	(kill-buffer specimen-buffer)))))

(ert-deftest hub/org-export-pro-refresh-overdrive-uses-xelatex-only ()
  "`pro-refresh-overdrive' always selects the XeLaTeX compiler path."
  (should (equal (hub/org-export--effective-compiler) "xelatex")))

(ert-deftest hub/org-export-buffer-to-pdf-uses-selected-compiler-process ()
  "The PDF compile step uses the compiler-specific process selected for the class."
  (let* ((specimen (expand-file-name "test/fixtures/org-export/approval-refresh-overdrive-page1.org"
				     hub/test-repo-root))
	 (artifact-root (hub/test-make-export-artifact-root))
	 (specimen-buffer nil)
	 (captured-process nil)
	 (captured-compiler nil))
    (unwind-protect
	(progn
	  (setq specimen-buffer (find-file-noselect specimen))
	  (with-current-buffer specimen-buffer
	    (cl-letf (((symbol-function 'hub/org-export--compiler-ready-p)
		       (lambda (_compiler) t))
		      ((symbol-function 'org-latex-compile)
		       (lambda (tex-path)
			 (setq captured-process org-latex-pdf-process)
			 (setq captured-compiler org-latex-compiler)
			 (with-temp-file (concat (file-name-sans-extension tex-path) ".pdf")
			   (insert "stub")))))
	      (hub/org-export-buffer-to-pdf artifact-root)
	      (should (equal captured-compiler "xelatex"))
	      (should (equal captured-process
			     (hub/org-export--pdf-process-for-compiler "xelatex"))))))
      (when (buffer-live-p specimen-buffer)
	(kill-buffer specimen-buffer))
      (when (file-directory-p artifact-root)
	(delete-directory artifact-root t)))))

(ert-deftest hub/org-export-slice-en-pro-refresh-overdrive-produces-latex-and-pdf ()
  "The first English flagship slice exports to LaTeX and PDF with the XeLaTeX path."
  (let* ((specimen (expand-file-name "test/fixtures/org-export/slice-en-pro-refresh-overdrive.org"
				     hub/test-repo-root))
	 (artifact-root (hub/test-make-export-artifact-root))
	 (specimen-buffer nil))
    (unwind-protect
	(progn
	  (setq specimen-buffer (find-file-noselect specimen))
	  (with-current-buffer specimen-buffer
	    (hub/test-with-export-compiler-readiness t
						     (hub/test-with-stubbed-latex-compile
						      (let* ((hub/org-export-output-root artifact-root)
							     (tex-path (hub/org-export-buffer-to-latex artifact-root))
							     (tex-contents (hub/test-read-file-as-string tex-path))
							     (class-path (hub/test-exported-class-path artifact-root "pro-refresh-overdrive"))
							     (class-contents (hub/test-read-file-as-string class-path))
							     (pdf-path (hub/org-export-buffer-to-pdf artifact-root)))
							(should (string-match-p (regexp-quote "% Intended LaTeX compiler: xelatex") tex-contents))
							(should (string-match-p (regexp-quote "% hub-pro-refresh-overdrive") tex-contents))
							(should (string-match-p (regexp-quote "\\documentclass[11pt,a4paper]{pro-refresh-overdrive}") tex-contents))
							(should (string-match-p (regexp-quote "\\RequirePackage{fontspec}") class-contents))
							(should (string-match-p (regexp-quote "\\setmainfont{Inter}") class-contents))
							(should (string-match-p (regexp-quote "\\setsansfont{Inter}") class-contents))
							(should (string-match-p (regexp-quote "\\setmonofont{Menlo}") class-contents))
							(should (string-match-p (regexp-quote "\\newfontface\\HubDisplayFont{Inter ExtraBold}") class-contents))
							(should (string-match-p (regexp-quote "\\RequirePackage{tcolorbox}") class-contents))
							(should (string-match-p (regexp-quote "\\tcbuselibrary{raster,minted}") class-contents))
							(should-not (string-match-p (regexp-quote "\\usepackage[minted]{tcolorbox}") class-contents))
							(should (string-match-p (regexp-quote "\\AtBeginEnvironment{MintedVerbatim}") class-contents))
							(should (string-match-p (regexp-quote "\\@namedef{PYG@tok@k}") class-contents))
							(should (string-match-p (regexp-quote "\\renewcommand{\\theFancyVerbLine}") class-contents))
							(should (string-match-p (regexp-quote "raster columns=4") class-contents))
							(should (string-match-p (regexp-quote "\\vspace{1.2em}") class-contents))
							(should (string-match-p (regexp-quote "raster columns=\\hub@cols") class-contents))
							(should (string-match-p (regexp-quote "hubcard/.style=") class-contents))
							(should (string-match-p (regexp-quote "\\vspace{1em}") class-contents))
							(should (string-match-p (regexp-quote "\\parbox{\\linewidth}") class-contents))
							(should-not (string-match-p (regexp-quote "\\begin{minipage}[t]{0.235\\linewidth}") class-contents))
							(should-not (string-match-p (regexp-quote "\\begin{minipage}[t]{0.48\\linewidth}") class-contents))
							(should-not (string-match-p (regexp-quote "\\fcolorbox{HubLine}{white}") class-contents))
							(should-not (string-match-p (regexp-quote "\\fcolorbox{HubLine}{HubSurface}") class-contents))
							(should (string-match-p (regexp-quote "\\HubHeroTitle{Refresh overdrive slice}") tex-contents))
							(should (string-match-p (regexp-quote "\\HubHeroDek{}") tex-contents))
							(should (string-match-p (regexp-quote "\\HubHeroMeta{Hubert Behaghel}{2026-04-27}") tex-contents))
							(should-not (string-match-p (regexp-quote "\\usepackage{fontspec}") tex-contents))
							(should (string-match-p "\\usepackage\[[^]]*english[^]]*\]{babel}" tex-contents))
							(should (string-match-p "\\title{Refresh overdrive slice}" tex-contents))
							(should (string-match-p "\\author{Hubert Behaghel}" tex-contents))
							(should (string-match-p "\\date{" tex-contents))
							(should (string-match-p "Minimal heading" tex-contents))
							(should (string-match-p "Minimal slice paragraph" tex-contents))
							(should (file-exists-p class-path))
							(should (file-exists-p pdf-path)))))))
      (when (buffer-live-p specimen-buffer)
	(kill-buffer specimen-buffer))
      (when (file-directory-p artifact-root)
	(delete-directory artifact-root t)))))

(ert-deftest hub/org-export-semantic-full-en-uses-class-owned-latex-and-pdf ()
  "The semantic specimen exports with class-owned layout markers and PDF output on the XeLaTeX path."
  (let* ((specimen (expand-file-name "test/fixtures/org-export/semantic-full-en.org"
				     hub/test-repo-root))
	 (artifact-root (hub/test-make-export-artifact-root))
	 (specimen-buffer nil))
    (unwind-protect
	(progn
	  (setq specimen-buffer (find-file-noselect specimen))
	  (with-current-buffer specimen-buffer
	    (hub/test-with-export-compiler-readiness t
						     (hub/test-with-stubbed-latex-compile
						      (let* ((hub/org-export-output-root artifact-root)
							     (tex-path (hub/org-export-buffer-to-latex artifact-root))
							     (tex-contents (hub/test-read-file-as-string tex-path))
							     (class-path (hub/test-exported-class-path artifact-root "pro-refresh-overdrive"))
							     (class-contents (hub/test-read-file-as-string class-path))
							     (pdf-path (hub/org-export-buffer-to-pdf artifact-root)))
							(should (string-match-p (regexp-quote "% Intended LaTeX compiler: xelatex") tex-contents))
							(should (string-match-p (regexp-quote "\\documentclass[11pt,a4paper]{pro-refresh-overdrive}") tex-contents))
							(should-not (string-match-p "\\maketitle" tex-contents))
							(should-not (string-match-p "\\tableofcontents" tex-contents))
							(should (string-match-p (regexp-quote "% hub-pro-refresh-overdrive-title") tex-contents))
							(should (string-match-p (regexp-quote "% hub-pro-refresh-overdrive-headings") tex-contents))
							(should (string-match-p (regexp-quote "\\pagestyle{hubpage}") class-contents))
							(should (string-match-p (regexp-quote "\\newlength{\\HubFooterLift}") class-contents))
							(should (string-match-p (regexp-quote "\\setlength{\\HubFooterLift}{4mm}") class-contents))
							(should (string-match-p (regexp-quote "\\newlength{\\HubFooterBleed}") class-contents))
							(should (string-match-p (regexp-quote "\\setlength{\\HubFooterBleed}{7mm}") class-contents))
							(should (string-match-p (regexp-quote "\\newlength{\\HubFooterLogoGap}") class-contents))
							(should (string-match-p (regexp-quote "\\setlength{\\HubFooterLogoGap}{3cm}") class-contents))
							(should (string-match-p (regexp-quote "\\newlength{\\HubFooterRuleOffset}") class-contents))
							(should (string-match-p (regexp-quote "\\setlength{\\HubFooterRuleOffset}{-1mm}") class-contents))
							(should (string-match-p (regexp-quote "\\newlength{\\HubFooterContentOffset}") class-contents))
							(should (string-match-p (regexp-quote "\\setlength{\\HubFooterContentOffset}{2mm}") class-contents))
							(should (string-match-p (regexp-quote "\\def\\ps@hubpage") class-contents))
							(should (string-match-p (regexp-quote "\\ifnum\\value{page}>1\\relax") class-contents))
							(should-not (string-match-p (regexp-quote "\\usepackage{fancyhdr}") class-contents))
							(should (string-match-p (regexp-quote "\\RequirePackage{fontspec}") class-contents))
							(should (string-match-p (regexp-quote "\\setmainfont{Inter}") class-contents))
							(should (string-match-p (regexp-quote "\\setsansfont{Inter}") class-contents))
							(should (string-match-p (regexp-quote "\\newcommand{\\HubHeroTitle}") class-contents))
							(should (string-match-p (regexp-quote "\\newcommand{\\HubHeroDek}") class-contents))
							(should (string-match-p (regexp-quote "\\newcommand{\\HubHeroMeta}") class-contents))
							(should (string-match-p (regexp-quote "\\begin{hubhero}") tex-contents))
							(should (string-match-p (regexp-quote "\\HubExportEyebrowBlock") tex-contents))
							(should (string-match-p (regexp-quote "\\HubHeroTitle{Make the refresh impossible to miss}") tex-contents))
							(should (string-match-p (regexp-quote "\\HubHeroDek{A temporary high-energy mode") tex-contents))
							(should (string-match-p (regexp-quote "\\HubHeroMeta{Hubert Behaghel}{2026-04-28}") tex-contents))
							(should (string-match-p "Refresh mode" tex-contents))
							(should (string-match-p "A temporary high-energy mode" tex-contents))
							(should (string-match-p (regexp-quote "\\begin{standfirst}") tex-contents))
							(should (string-match-p (regexp-quote "\\begin{quote}[Prototype testimonial · Global platform operator]") tex-contents))
							(should (string-match-p (regexp-quote "\\fontsize{12.5}{16}\\selectfont\\bfseries\\color{HubInk}\\ignorespaces") class-contents))
							(should (string-match-p (regexp-quote "\\begin{pullquote}") tex-contents))
							(should (string-match-p (regexp-quote "\\begin{callout}[Design signal]") tex-contents))
							(should (string-match-p (regexp-quote "\\begin{hubtable}") tex-contents))
							(should (string-match-p (regexp-quote "\\begin{tabularx}{\\linewidth}{YYY}") tex-contents))
							(should (string-match-p (regexp-quote "\\HubTableHeadRule") tex-contents))
							(should (string-match-p (regexp-quote "\\begin{metrics}") tex-contents))
							(should (string-match-p (regexp-quote "\\begin{metric}[230+]") tex-contents))
							(should (string-match-p (regexp-quote "\\begin{metric}[99.6\\%]") tex-contents))
							(should (string-match-p (regexp-quote "\\RequirePackage{tcolorbox}") class-contents))
							(should (string-match-p (regexp-quote "\\tcbuselibrary{raster,minted}") class-contents))
							(should-not (string-match-p (regexp-quote "\\usepackage[minted]{tcolorbox}") class-contents))
							(should (string-match-p (regexp-quote "\\AtBeginEnvironment{MintedVerbatim}") class-contents))
							(should (string-match-p (regexp-quote "\\@namedef{PYG@tok@k}") class-contents))
							(should (string-match-p (regexp-quote "\\renewcommand{\\theFancyVerbLine}") class-contents))
							(should (string-match-p (regexp-quote "raster columns=4") class-contents))
							(should (string-match-p (regexp-quote "\\vspace{1.2em}") class-contents))
							(should (string-match-p (regexp-quote "raster columns=\\hub@cols") class-contents))
							(should (string-match-p (regexp-quote "hubcard/.style=") class-contents))
							(should (string-match-p (regexp-quote "\\vspace{1em}") class-contents))
							(should (string-match-p (regexp-quote "\\parbox{\\linewidth}") class-contents))
							(should-not (string-match-p (regexp-quote "\\begin{minipage}[t]{0.235\\linewidth}") class-contents))
							(should-not (string-match-p (regexp-quote "\\begin{minipage}[t]{0.48\\linewidth}") class-contents))
							(should-not (string-match-p (regexp-quote "\\fcolorbox{HubLine}{white}") class-contents))
							(should-not (string-match-p (regexp-quote "\\fcolorbox{HubLine}{HubSurface}") class-contents))
							(should (string-match-p (regexp-quote "\\begin{pillars}") tex-contents))
							(should (string-match-p (regexp-quote "\\begin{pillar}[Trust engineered]") tex-contents))
							(should (string-match-p (regexp-quote "\\begin{graph}") tex-contents))
							(should (string-match-p (regexp-quote "\\begin{tikzpicture}") tex-contents))
							(should (string-match-p (regexp-quote "\\begin{axis}[") tex-contents))
							(should (string-match-p (regexp-quote "width=\\linewidth") tex-contents))
							(should (string-match-p (regexp-quote "hubgraphaxis") tex-contents))
							(should (string-match-p (regexp-quote "axis lines=left") class-contents))
							(should (string-match-p (regexp-quote "font=\\sffamily") class-contents))
							(should (string-match-p (regexp-quote "assume math mode=true") class-contents))
							(should (string-match-p (regexp-quote "at={(0.98,0.05)}") class-contents))
							(should (string-match-p (regexp-quote "\\RequirePackage{pgfplots}") class-contents))
							(should (string-match-p (regexp-quote "\\newenvironment{graph}") class-contents))
							(should (string-match-p (regexp-quote "\\begin{minted}") tex-contents))
							(should (string-match-p (regexp-quote "\\begin{hubfootnote}") tex-contents))
							(should (string-match-p "Operating principle" tex-contents))
							(should (file-exists-p pdf-path)))))))
      (when (buffer-live-p specimen-buffer)
	(kill-buffer specimen-buffer))
      (when (file-directory-p artifact-root)
	(delete-directory artifact-root t)))))

(ert-deftest hub/org-export-approval-page-one-uses-font-focused-preamble ()
  "The approval specimen uses a class-owned XeLaTeX preamble without running chrome."
  (let* ((specimen (expand-file-name "test/fixtures/org-export/approval-refresh-overdrive-page1.org"
				     hub/test-repo-root))
	 (artifact-root (hub/test-make-export-artifact-root))
	 (specimen-buffer nil))
    (unwind-protect
	(progn
	  (setq specimen-buffer (find-file-noselect specimen))
	  (with-current-buffer specimen-buffer
	    (hub/test-with-export-compiler-readiness t
						     (hub/test-with-stubbed-latex-compile
						      (let* ((hub/org-export-output-root artifact-root)
							     (tex-path (hub/org-export-buffer-to-latex artifact-root))
							     (tex-contents (hub/test-read-file-as-string tex-path))
							     (class-path (hub/test-exported-class-path artifact-root "pro-refresh-overdrive"))
							     (class-contents (hub/test-read-file-as-string class-path))
							     (pdf-path (hub/org-export-buffer-to-pdf artifact-root)))
							(should (string-match-p (regexp-quote "% Intended LaTeX compiler: xelatex") tex-contents))
							(should (string-match-p (regexp-quote "\\RequirePackage{fontspec}") class-contents))
							(should (string-match-p (regexp-quote "\\setmainfont{Inter}") class-contents))
							(should (string-match-p (regexp-quote "\\setsansfont{Inter}") class-contents))
							(should-not (string-match-p (regexp-quote "\\usepackage{fontspec}") tex-contents))
							(should (string-match-p (regexp-quote (format "\\renewcommand{\\HubHeroPatternGraphic}{\\includegraphics[width=118mm]{%s}}" (expand-file-name "hero-pattern.png" artifact-root))) tex-contents))
							(should (string-match-p (regexp-quote (format "\\renewcommand{\\HubHeroLogoGraphic}{\\includegraphics[width=32mm]{%s}}" (expand-file-name "hero-logo.pdf" artifact-root))) tex-contents))
							(should (string-match-p (regexp-quote (format "\\renewcommand{\\HubFooterLogoGraphic}{\\includegraphics[width=16mm]{%s}}" (expand-file-name "hero-logo.pdf" artifact-root))) tex-contents))
							(should (string-match-p (regexp-quote "\\setmonofont{Menlo}") class-contents))
							(should (string-match-p (regexp-quote "\\newfontface\\HubDisplayFont{Inter ExtraBold}") class-contents))
							(should (string-match-p (regexp-quote "\\begin{minipage}[t]{118mm}") tex-contents))
							(should (string-match-p (regexp-quote (concat "\\begin{minipage}[t]{118mm}\n"
												      "\\raggedright\n"
												      "\\HubExportEyebrowBlock\n")) tex-contents))
							(should-not (string-match-p (regexp-quote "\\parbox[t]{0.78\\linewidth}") tex-contents))
							(should-not (string-match-p (regexp-quote "\\parbox[t]{0.72\\linewidth}") tex-contents))
							(should-not (string-match-p (regexp-quote "\\newenvironment{hubhero}{\\par\\noindent\\rule{\\linewidth}{0.8pt}") tex-contents))
							(should (string-match-p (regexp-quote "\\HubHeroTitle{Make the refresh impossible to miss}") tex-contents))
							(should (string-match-p (regexp-quote "\\HubHeroDek{A temporary high-energy mode for launch-season communications: brighter wedges, louder accent choreography, and more obvious page-level motion while keeping the reading structure intact.}") tex-contents))
							(should (string-match-p (regexp-quote "\\HubHeroMeta{Prototype direction · refresh-overdrive}{April 2026 · Visual direction study}") tex-contents))
							(should (string-match-p (regexp-quote "\\newcommand{\\HubHeroDek}") class-contents))
							(should (string-match-p (regexp-quote "\\parbox[t]{112mm}{\\raggedright\\normalsize") class-contents))
							(should (string-match-p (regexp-quote "\\newcommand{\\HubHeroMeta}") class-contents))
							(should (string-match-p (regexp-quote "\\RequirePackage{needspace}") class-contents))
							(should (string-match-p (regexp-quote "\\newenvironment{hubtable}") class-contents))
							(should (string-match-p (regexp-quote "\\setcounter{secnumdepth}{1}") class-contents))
							(should (string-match-p (regexp-quote "\\color{HubLine}\\rule{116mm}{0.8pt}") class-contents))
							(should (string-match-p (regexp-quote "\\pagestyle{hubpage}") class-contents))
							(should (string-match-p (regexp-quote "\\newlength{\\HubFooterLift}") class-contents))
							(should (string-match-p (regexp-quote "\\setlength{\\HubFooterLift}{4mm}") class-contents))
							(should (string-match-p (regexp-quote "\\newlength{\\HubFooterBleed}") class-contents))
							(should (string-match-p (regexp-quote "\\setlength{\\HubFooterBleed}{7mm}") class-contents))
							(should (string-match-p (regexp-quote "\\newlength{\\HubFooterLogoGap}") class-contents))
							(should (string-match-p (regexp-quote "\\setlength{\\HubFooterLogoGap}{3cm}") class-contents))
							(should (string-match-p (regexp-quote "\\newlength{\\HubFooterRuleOffset}") class-contents))
							(should (string-match-p (regexp-quote "\\setlength{\\HubFooterRuleOffset}{-1mm}") class-contents))
							(should (string-match-p (regexp-quote "\\newlength{\\HubFooterContentOffset}") class-contents))
							(should (string-match-p (regexp-quote "\\setlength{\\HubFooterContentOffset}{2mm}") class-contents))
							(should (string-match-p (regexp-quote "\\def\\ps@hubpage") class-contents))
							(should (string-match-p (regexp-quote "\\ifnum\\value{page}>1\\relax") class-contents))
							(should-not (string-match-p (regexp-quote "\\usepackage{fancyhdr}") class-contents))
							(should-not (string-match-p (regexp-quote "\\pagestyle{fancy}") class-contents))
							(should-not (string-match-p (regexp-quote "\\fancyhead") class-contents))
							(should-not (string-match-p (regexp-quote "\\fancyfoot") class-contents))
							(should (string-match-p (regexp-quote "\\definecolor{HubPaper}{HTML}{FCF5EE}") class-contents))
							(should (string-match-p (regexp-quote "\\fill[HubPaper]") class-contents))
							(should-not (string-match-p (regexp-quote "\\pagecolor{white}") class-contents))
							(should (string-match-p (regexp-quote "\\newenvironment{hubhero}") class-contents))
							(should (string-match-p (regexp-quote "\\begin{tikzpicture}[overlay]") class-contents))
							(should (string-match-p (regexp-quote "hero-pattern.png") class-contents))
							(should (string-match-p (regexp-quote "hero-logo.pdf") class-contents))
							(should (string-match-p "Make the refresh impossible to miss" tex-contents))
							(should (string-match-p (regexp-quote "\\begin{callout}[Design signal]") tex-contents))
							(should (string-match-p (regexp-quote "\\begin{quote}[Prototype testimonial · Global platform operator]") tex-contents))
							(should (string-match-p (regexp-quote "\\fontsize{12.5}{16}\\selectfont\\bfseries\\color{HubInk}\\ignorespaces") class-contents))
							(should-not (string-match-p (regexp-quote "\\begin{pullquote}[Prototype testimonial · Global platform operator]") tex-contents))
							(should (string-match-p (regexp-quote "\\begin{description}") tex-contents))
							(should (string-match-p (regexp-quote "\\item[{Verified transparency}]") tex-contents))
							(should (string-match-p (regexp-quote "\\begin{hubtable}") tex-contents))
							(should (string-match-p (regexp-quote "\\begin{tabularx}{\\linewidth}{YYYY}") tex-contents))
							(should (string-match-p (regexp-quote "\\HubTableHeaderCell{Measure}") tex-contents))
							(should (string-match-p (regexp-quote "\\HubTableBodyRule") tex-contents))
							(should (string-match-p (regexp-quote "\\begin{metric}[230+]") tex-contents))
							(should (string-match-p (regexp-quote "\\begin{metric}[99.6\\%]") tex-contents))
							(should (string-match-p (regexp-quote "\\RequirePackage{tcolorbox}") class-contents))
							(should (string-match-p (regexp-quote "\\tcbuselibrary{raster,minted}") class-contents))
							(should-not (string-match-p (regexp-quote "\\usepackage[minted]{tcolorbox}") class-contents))
							(should (string-match-p (regexp-quote "\\AtBeginEnvironment{MintedVerbatim}") class-contents))
							(should (string-match-p (regexp-quote "\\@namedef{PYG@tok@k}") class-contents))
							(should (string-match-p (regexp-quote "\\renewcommand{\\theFancyVerbLine}") class-contents))
							(should (string-match-p (regexp-quote "raster columns=4") class-contents))
							(should (string-match-p (regexp-quote "\\vspace{1.2em}") class-contents))
							(should (string-match-p (regexp-quote "raster columns=\\hub@cols") class-contents))
							(should (string-match-p (regexp-quote "hubcard/.style=") class-contents))
							(should (string-match-p (regexp-quote "\\vspace{1em}") class-contents))
							(should (string-match-p (regexp-quote "\\parbox{\\linewidth}") class-contents))
							(should-not (string-match-p (regexp-quote "\\begin{minipage}[t]{0.235\\linewidth}") class-contents))
							(should-not (string-match-p (regexp-quote "\\begin{minipage}[t]{0.48\\linewidth}") class-contents))
							(should-not (string-match-p (regexp-quote "\\fcolorbox{HubLine}{white}") class-contents))
							(should-not (string-match-p (regexp-quote "\\fcolorbox{HubLine}{HubSurface}") class-contents))
							(should (string-match-p "Countries and territories covered" tex-contents))
							(should (string-match-p (regexp-quote "\\begin{pillar}[Trust engineered]") tex-contents))
							(should (string-match-p "Even automation-heavy experiences still need to feel respectful" tex-contents))
							(should (file-exists-p pdf-path))
							(should (file-exists-p (expand-file-name "hero-pattern.png" artifact-root)))
							(should (file-exists-p (expand-file-name "hero-logo.pdf" artifact-root))))))))
      (when (buffer-live-p specimen-buffer)
	(kill-buffer specimen-buffer))
      (when (file-directory-p artifact-root)
	(delete-directory artifact-root t)))))

(ert-deftest hub/org-export-source-blocks-minted-formatting ()
  "Source blocks export to minted environments with and without line numbers."
  (let* ((specimen (expand-file-name "test/fixtures/org-export/source-blocks.org"
				     hub/test-repo-root))
	 (artifact-root (hub/test-make-export-artifact-root))
	 (specimen-buffer nil))
    (unwind-protect
	(progn
	  (setq specimen-buffer (find-file-noselect specimen))
	  (with-current-buffer specimen-buffer
	    (hub/test-with-export-compiler-readiness t
						     (hub/test-with-stubbed-latex-compile
						      (let* ((hub/org-export-output-root artifact-root)
							     (tex-path (hub/org-export-buffer-to-latex artifact-root))
							     (tex-contents (hub/test-read-file-as-string tex-path)))
							(should (string-match-p (regexp-quote "\\begin{minted}[fontsize=\\footnotesize,breaklines=true,autogobble=true,xleftmargin=14pt,numbersep=8pt]{python}") tex-contents))
							(should (string-match-p (regexp-quote "\\begin{minted}[linenos,firstnumber=1,fontsize=\\footnotesize,breaklines=true,autogobble=true,xleftmargin=14pt,numbersep=8pt]{python}") tex-contents))
							(should-not (string-match-p (regexp-quote "\\HubCodeThemeDark") tex-contents))
							(should-not (string-match-p (regexp-quote "\\HubCodeThemeLight") tex-contents)))))))
      (when (buffer-live-p specimen-buffer)
	(kill-buffer specimen-buffer))
      (when (file-directory-p artifact-root)
	(delete-directory artifact-root t)))))

(ert-deftest hub/org-export-source-blocks-theme-dark ()
  "Source blocks export with dark theme when requested."
  (let* ((specimen (expand-file-name "test/fixtures/org-export/source-blocks-dark.org"
				     hub/test-repo-root))
	 (artifact-root (hub/test-make-export-artifact-root))
	 (specimen-buffer nil))
    (unwind-protect
	(progn
	  (setq specimen-buffer (find-file-noselect specimen))
	  (with-current-buffer specimen-buffer
	    (hub/test-with-export-compiler-readiness t
						     (hub/test-with-stubbed-latex-compile
						      (let* ((hub/org-export-output-root artifact-root)
							     (tex-path (hub/org-export-buffer-to-latex artifact-root))
							     (tex-contents (hub/test-read-file-as-string tex-path)))
							(should (string-match-p (regexp-quote "\\HubCodeThemeDark") tex-contents))
							(should-not (string-match-p (regexp-quote "\\HubCodeThemeLight") tex-contents)))))))
      (when (buffer-live-p specimen-buffer)
	(kill-buffer specimen-buffer))
      (when (file-directory-p artifact-root)
	(delete-directory artifact-root t)))))

(ert-deftest hub/org-export-source-blocks-theme-light ()
  "Source blocks export with explicit light theme when requested."
  (let* ((specimen (expand-file-name "test/fixtures/org-export/source-blocks-light.org"
				     hub/test-repo-root))
	 (artifact-root (hub/test-make-export-artifact-root))
	 (specimen-buffer nil))
    (unwind-protect
	(progn
	  (setq specimen-buffer (find-file-noselect specimen))
	  (with-current-buffer specimen-buffer
	    (hub/test-with-export-compiler-readiness t
						     (hub/test-with-stubbed-latex-compile
						      (let* ((hub/org-export-output-root artifact-root)
							     (tex-path (hub/org-export-buffer-to-latex artifact-root))
							     (tex-contents (hub/test-read-file-as-string tex-path)))
							(should (string-match-p (regexp-quote "\\HubCodeThemeLight") tex-contents))
							(should-not (string-match-p (regexp-quote "\\HubCodeThemeDark") tex-contents)))))))
      (when (buffer-live-p specimen-buffer)
	(kill-buffer specimen-buffer))
      (when (file-directory-p artifact-root)
	(delete-directory artifact-root t)))))

(ert-deftest hub/org-export-source-blocks-theme-invalid ()
  "Source blocks export fails with invalid theme."
  (let* ((specimen (expand-file-name "test/fixtures/org-export/source-blocks-invalid.org"
				     hub/test-repo-root))
	 (artifact-root (hub/test-make-export-artifact-root))
	 (specimen-buffer nil))
    (unwind-protect
	(progn
	  (setq specimen-buffer (find-file-noselect specimen))
	  (with-current-buffer specimen-buffer
	    (hub/test-with-export-compiler-readiness t
						     (hub/test-with-stubbed-latex-compile
						      (let ((hub/org-export-output-root artifact-root))
							(should-error (hub/org-export-buffer-to-latex artifact-root) :type 'user-error))))))
      (when (buffer-live-p specimen-buffer)
	(kill-buffer specimen-buffer))
      (when (file-directory-p artifact-root)
	(delete-directory artifact-root t)))))

(ert-deftest hub/org-export-source-blocks-theme-matrix ()
  "Source blocks export matrix with light/dark and numbered/unnumbered."
  (let* ((specimen (expand-file-name "test/fixtures/org-export/source-blocks-theme-matrix.org"
				     hub/test-repo-root))
	 (artifact-root (hub/test-make-export-artifact-root))
	 (specimen-buffer nil))
    (unwind-protect
	(progn
	  (setq specimen-buffer (find-file-noselect specimen))
	  (with-current-buffer specimen-buffer
	    (hub/test-with-export-compiler-readiness t
						     (hub/test-with-stubbed-latex-compile
						      (let* ((hub/org-export-output-root artifact-root)
							     (tex-path (hub/org-export-buffer-to-latex artifact-root))
							     (tex-contents (hub/test-read-file-as-string tex-path)))
							(should (string-match-p (regexp-quote "\\HubCodeThemeLight") tex-contents))
							(should (string-match-p (regexp-quote "\\HubCodeThemeDark") tex-contents))
							(should (string-match-p (regexp-quote "\\begin{minted}[fontsize=\\footnotesize,breaklines=true,autogobble=true,xleftmargin=14pt,numbersep=8pt]{python}") tex-contents))
							(should (string-match-p (regexp-quote "\\begin{minted}[linenos,firstnumber=1,fontsize=\\footnotesize,breaklines=true,autogobble=true,xleftmargin=14pt,numbersep=8pt]{python}") tex-contents)))))))
      (when (buffer-live-p specimen-buffer)
	(kill-buffer specimen-buffer))
      (when (file-directory-p artifact-root)
	(delete-directory artifact-root t)))))

(ert-deftest hub/org-export-pro-refresh-overdrive-mint-accents ()
  "The class defines brand palette colors, a custom page style, and uses them for code themes."
  (let* ((specimen (expand-file-name "test/fixtures/org-export/slice-en-pro-refresh-overdrive.org"
				     hub/test-repo-root))
	 (artifact-root (hub/test-make-export-artifact-root))
	 (specimen-buffer nil))
    (unwind-protect
	(progn
	  (setq specimen-buffer (find-file-noselect specimen))
	  (with-current-buffer specimen-buffer
	    (hub/test-with-export-compiler-readiness t
						     (hub/test-with-stubbed-latex-compile
						      (let* ((hub/org-export-output-root artifact-root)
							     (tex-path (hub/org-export-buffer-to-latex artifact-root))
							     (class-path (hub/test-exported-class-path artifact-root "pro-refresh-overdrive"))
							     (class-contents (hub/test-read-file-as-string class-path)))
							(should (string-match-p (regexp-quote "\\definecolor{HubMint}{HTML}{20C997}") class-contents))
							(should-not (string-match-p (regexp-quote (concat "\\definecolor{Hub" "Emerald}")) class-contents))
							(should (string-match-p (regexp-quote "\\def\\ps@hubpage") class-contents))
							(should (string-match-p (regexp-quote "\\HubDocumentTitle") class-contents))
							(should (string-match-p (regexp-quote "\\HubFooterLogoGraphic") class-contents))
							(should (string-match-p (regexp-quote "\\def\\HubCodeNameColor{HubStandfirst}") class-contents))
							(should (string-match-p (regexp-quote "\\def\\HubCodeStringColor{HubTableRule}") class-contents))
							(should (string-match-p (regexp-quote "\\def\\HubCodeMintColor{HubMint}") class-contents))
							(should (string-match-p (regexp-quote "\\@namedef{PYG@tok@nb}{\\def\\PYG@tc##1{\\textcolor{\\HubCodeMintColor}{##1}}}") class-contents)))))))
      (when (buffer-live-p specimen-buffer)
	(kill-buffer specimen-buffer))
      (when (file-directory-p artifact-root)
	(delete-directory artifact-root t)))))

(ert-deftest hub/org-export-demo-adaptive-cards-preserves-adaptive-pillars ()
  "The adaptive cards demo exports with adaptive pillar counts."
  (let* ((specimen (expand-file-name "test/fixtures/org-export/demo-adaptive-cards.org"
				     hub/test-repo-root))
	 (artifact-root (hub/test-make-export-artifact-root))
	 (specimen-buffer nil))
    (unwind-protect
	(progn
	  (setq specimen-buffer (find-file-noselect specimen))
	  (with-current-buffer specimen-buffer
	    (hub/test-with-export-compiler-readiness t
						     (hub/test-with-stubbed-latex-compile
						      (let* ((hub/org-export-output-root artifact-root)
							     (tex-path (hub/org-export-buffer-to-latex artifact-root))
							     (tex-contents (hub/test-read-file-as-string tex-path))
							     (class-path (hub/test-exported-class-path artifact-root "pro-refresh-overdrive"))
							     (class-contents (hub/test-read-file-as-string class-path)))
							(should (string-match-p (regexp-quote "\\begin{pillars}") tex-contents))
							(should (string-match-p (regexp-quote "\\begin{pillar}[First]") tex-contents))
							(should (string-match-p (regexp-quote "\\begin{pillar}[One]") tex-contents))
							(should (string-match-p (regexp-quote "\\newcounter{hubpillarenv}") class-contents))
							(should (string-match-p (regexp-quote "\\newcounter{hubpillaritem}") class-contents))
							(should (string-match-p (regexp-quote "\\stepcounter{hubpillaritem}") class-contents))
							(should (string-match-p (regexp-quote "hub@pillar@count@") class-contents))
							(should (string-match-p (regexp-quote "raster columns=\\hub@cols") class-contents))
							(should-not (string-match-p (regexp-quote "raster columns=2") class-contents))
							(should (string-match-p (regexp-quote "\\immediate\\write\\@auxout{\\string\\expandafter\\string\\gdef\\string\\csname\\space hub@pillar@count@\\thehubpillarenv\\string\\endcsname{\\thehubpillaritem}}") class-contents)))))))
      (when (buffer-live-p specimen-buffer)
	(kill-buffer specimen-buffer))
      (when (file-directory-p artifact-root)
	(delete-directory artifact-root t)))))

(provide 'org-latex-pdf-export-test)
;;; org-latex-pdf-export-test.el ends here
