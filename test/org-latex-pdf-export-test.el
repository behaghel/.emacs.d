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
							(should (string-match-p (regexp-quote "\\pagestyle{empty}") class-contents))
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
							(should (string-match-p (regexp-quote "\\begin{quote}") tex-contents))
							(should (string-match-p (regexp-quote "\\begin{pullquote}") tex-contents))
							(should (string-match-p (regexp-quote "\\begin{callout}") tex-contents))
							(should (string-match-p (regexp-quote "\\begin{hubtable}") tex-contents))
							(should (string-match-p (regexp-quote "\\begin{tabularx}{\\linewidth}{YYY}") tex-contents))
							(should (string-match-p (regexp-quote "\\HubTableHeadRule") tex-contents))
							(should (string-match-p (regexp-quote "\\begin{metrics}") tex-contents))
							(should (string-match-p (regexp-quote "\\begin{metric}[230+]") tex-contents))
							(should (string-match-p (regexp-quote "\\begin{pillars}") tex-contents))
							(should (string-match-p (regexp-quote "\\begin{pillar}[Trust engineered]") tex-contents))
							(should (string-match-p (regexp-quote "\\begin{hubcode}") tex-contents))
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
							(should (string-match-p (regexp-quote "\\pagestyle{empty}") class-contents))
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
							(should-not (string-match-p (regexp-quote "\\begin{pullquote}[Prototype testimonial · Global platform operator]") tex-contents))
							(should (string-match-p (regexp-quote "\\begin{description}") tex-contents))
							(should (string-match-p (regexp-quote "\\item[{Verified transparency}]") tex-contents))
							(should (string-match-p (regexp-quote "\\begin{hubtable}") tex-contents))
							(should (string-match-p (regexp-quote "\\begin{tabularx}{\\linewidth}{YYYY}") tex-contents))
							(should (string-match-p (regexp-quote "\\HubTableHeaderCell{Measure}") tex-contents))
							(should (string-match-p (regexp-quote "\\HubTableBodyRule") tex-contents))
							(should (string-match-p (regexp-quote "\\begin{metric}[230+]") tex-contents))
							(should (string-match-p (regexp-quote "\\begin{metric}[99.6\\%]") tex-contents))
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

(provide 'org-latex-pdf-export-test)
;;; org-latex-pdf-export-test.el ends here
