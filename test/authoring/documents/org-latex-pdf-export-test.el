;;; org-latex-pdf-export-test.el --- Org LaTeX PDF slice tests -*- lexical-binding: t; -*-

;;; Commentary:
;; ERT coverage for the Org -> LaTeX -> PDF export slices.

;;; Code:

(require 'cl-lib)
(require 'ert)

(defconst hub/test-repo-root
  (file-name-as-directory
   (locate-dominating-file (or load-file-name buffer-file-name) "domains.yaml"))
  "Repository root for Org LaTeX PDF export tests.")

(let ((user-emacs-directory hub/test-repo-root)
      (default-directory hub/test-repo-root))
  (add-to-list 'load-path (expand-file-name "core" user-emacs-directory))
  (add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
  (add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
  (add-to-list 'load-path (expand-file-name "modules/lang" user-emacs-directory))
  (add-to-list 'load-path (expand-file-name "modules/org" user-emacs-directory))
  (add-to-list 'load-path (expand-file-name "modules/interactive" user-emacs-directory))
  (require 'core-packages)
  (require 'editing/evil)
  (require 'org/core)
  (require 'org/export-latex))

(load-file (expand-file-name "scripts/approval-refresh-overdrive-page1.el" hub/test-repo-root))

(defun hub/test-read-file-as-string (path)
  "Return PATH contents as a string."
  (with-temp-buffer
    (insert-file-contents path)
    (buffer-string)))

(defun hub/test-exported-class-path (artifact-root class-name)
  "Return the staged class path for CLASS-NAME in ARTIFACT-ROOT."
  (expand-file-name (format "%s.cls" class-name) artifact-root))

(ert-deftest hub/org-discovers-local-latex-classes ()
  "Local Org LaTeX template choices come from tracked .cls assets."
  (let* ((class-directory (expand-file-name "etc/latex" hub/test-repo-root))
	 (expected (sort (mapcar #'file-name-base
				 (directory-files class-directory t "\\.cls\\'" 'nosort))
			 #'string<)))
    (let ((hub/org-latex-class-directory class-directory))
      (should (equal (hub/org-discover-local-latex-classes) expected))
      (dolist (class expected)
	(should (member class (hub/org-latex-class-candidates)))))))

(ert-deftest hub/org-reads-latex-class-from-discovered-candidates ()
  "Org LaTeX template selector prompts from discovered candidates."
  (let ((hub/org-latex-class-directory (expand-file-name "etc/latex" hub/test-repo-root))
	captured-candidates)
    (cl-letf (((symbol-function 'completing-read)
	       (lambda (_prompt candidates &rest _args)
		 (setq captured-candidates candidates)
		 (car candidates))))
      (should (member (hub/org-read-latex-class) captured-candidates))
      (should (member "hub-article" captured-candidates))
      (should (member "veriff" captured-candidates)))))

(ert-deftest hub/org-export-resolves-local-class-assets-by-convention ()
  "Export staging resolves local classes from tracked .cls asset names."
  (let ((hub/org-export--latex-class-directory (expand-file-name "etc/latex" hub/test-repo-root)))
    (dolist (asset (hub/org-export--local-class-assets))
      (should (equal (hub/org-export--class-asset-source (car asset))
		     (cdr asset))))))

(ert-deftest hub/org-export-texinputs-includes-local-latex-assets ()
  "TeX subprocesses can resolve repo-local classes outside devenv shells."
  (let* ((class-directory (expand-file-name "etc/latex" hub/test-repo-root))
	 (recursive-entry (concat (file-name-as-directory class-directory) "//")))
    (should (member recursive-entry
		    (split-string (hub/org-export--merge-texinputs (list class-directory))
				  path-separator t)))
    (should (string-suffix-p path-separator
			     (hub/org-export--merge-texinputs (list class-directory))))))

(ert-deftest hub/org-export-wraps-xelatex-with-devenv-when-available ()
  "GUI-launched Emacs uses the project TeX Live closure for local exports."
  (let ((hub/org-export-use-devenv-compiler t)
	(hub/org-export-devenv-executable "/tmp/devenv")
	(user-emacs-directory hub/test-repo-root))
    (cl-letf (((symbol-function 'file-executable-p)
	       (lambda (path) (string= path "/tmp/devenv"))))
      (setenv "IN_NIX_SHELL" nil)
      (should (equal (hub/org-export--compiler-command "xelatex")
		     (format "TEXINPUTS=%s %s shell --from %s -- xelatex"
			     (shell-quote-argument
			      (hub/org-export--merge-texinputs hub/org-export-texinputs-directories
							       (getenv "TEXINPUTS")))
			     (shell-quote-argument "/tmp/devenv")
			     (shell-quote-argument
			      (concat "path:" (directory-file-name
					       (expand-file-name hub/test-repo-root))))))))))

(ert-deftest hub/org-export-falls-back-to-plain-xelatex-without-devenv ()
  "Compiler process remains usable when devenv is absent."
  (let ((hub/org-export-use-devenv-compiler t)
	(hub/org-export-devenv-executable nil))
    (cl-letf (((symbol-function 'executable-find) (lambda (_name) nil))
	      ((symbol-function 'file-executable-p) (lambda (_path) nil)))
      (should (equal (hub/org-export--compiler-command "xelatex")
		     (format "TEXINPUTS=%s xelatex"
			     (shell-quote-argument
			      (hub/org-export--merge-texinputs hub/org-export-texinputs-directories
							       (getenv "TEXINPUTS")))))))))

(ert-deftest hub/org-export-veriff-inline-images-use-source-absolute-paths ()
  "Relative Veriff images resolve when TeX compiles from another directory."
  (let* ((source-root (make-temp-file "hub-org-image-source-" t))
	 (output-root (make-temp-file "hub-org-image-output-" t))
	 (image-directory (expand-file-name "img" source-root))
	 (image-path (expand-file-name "sample.png" image-directory))
	 (org-path (expand-file-name "source.org" source-root))
	 (tex-path (expand-file-name "source.tex" output-root)))
    (unwind-protect
	(progn
	  (make-directory image-directory t)
	  (with-temp-file image-path
	    (insert "not-a-real-png"))
	  (with-temp-file org-path
	    (insert "#+TITLE: Image source\n#+LATEX_CLASS: veriff\n\n[[./img/sample.png]]\n"))
	  (with-current-buffer (find-file-noselect org-path)
	    (unwind-protect
		(progn
		  (org-mode)
		  (let ((hub/org-export--active-output-dir nil))
		    (org-export-to-file 'latex tex-path nil nil nil nil nil)))
	      (kill-buffer)))
	  (should (string-match-p
		   (regexp-quote
		    (format "\\HubFigureImage{\\includegraphics[width=.9\\linewidth]{%s}}"
			    (hub/org-export--latex-escape image-path)))
		   (hub/test-read-file-as-string tex-path))))
      (delete-directory source-root t)
      (delete-directory output-root t))))

(ert-deftest hub/org-export-registers-discovered-local-latex-classes ()
  "Discovered .cls files become generic Org LaTeX class registrations."
  (let* ((class-directory (make-temp-file "hub-org-latex-classes-" t))
	 (class-path (expand-file-name "sample-local.cls" class-directory))
	 (hub/org-export--latex-class-directory class-directory)
	 (org-latex-classes nil))
    (unwind-protect
	(progn
	  (with-temp-file class-path
	    (insert "\\NeedsTeXFormat{LaTeX2e}\n"
		    "\\ProvidesClass{sample-local}\n"
		    "\\LoadClass{article}\n"))
	  (hub/org-export--register-local-latex-classes)
	  (let ((entry (assoc "sample-local" org-latex-classes)))
	    (should entry)
	    (should (string-match-p
		     (regexp-quote "\\documentclass[11pt,a4paper]{sample-local}")
		     (cadr entry)))))
      (delete-directory class-directory t))))

(defun hub/test-assert-hub-article-code-syntax-contract (tex-contents class-contents)
  "Assert hub article code is no-background minted with class-owned tokens."
  (dolist (forbidden '("HubArticleCodeLightBackground"
		       "HubArticleLightCodeBackground"
		       "HubArticleCodeDarkBackground"
		       "HubArticleDarkCodeBackground"
		       "HubArticleDarkCodeInk"
		       "HubArticleDarkCodeRule"
		       "HubArticleCodeBackground"
		       "HubArticleLightCodeInk"
		       "HubArticleLightCodeRule"
		       "bgcolor="
		       "style=friendly"
		       "style=monokai"
		       "fillcolor="
		       "\\colorbox{HubArticleCode"
		       "\\fcolorbox"))
    (should-not (string-match-p (regexp-quote forbidden) tex-contents))
    (should-not (string-match-p (regexp-quote forbidden) class-contents)))
  (should (string-match-p (regexp-quote "\\RequirePackage{minted}") class-contents))
  (should (string-match-p (regexp-quote "\\RequirePackage{etoolbox}") class-contents))
  (should (string-match-p (regexp-quote "\\setminted{fontsize=\\footnotesize,breaklines,autogobble}") class-contents))
  (should (string-match-p (regexp-quote "\\AtBeginEnvironment{MintedVerbatim}") class-contents))
  (should (string-match-p (regexp-quote "\\definecolor{HubArticleCodeBaseInk}{HTML}{1B2421}") class-contents))
  (should (string-match-p (regexp-quote "\\definecolor{HubArticleCodeKeywordInk}{HTML}{164F82}") class-contents))
  (should (string-match-p (regexp-quote "\\definecolor{HubArticleCodeCommentInk}{HTML}{5F6C65}") class-contents))
  (should (string-match-p (regexp-quote "\\definecolor{HubArticleCodeStringInk}{HTML}{1F6E3F}") class-contents))
  (should (string-match-p (regexp-quote "\\definecolor{HubArticleCodeConstantInk}{HTML}{8F3F16}") class-contents))
  (should (string-match-p (regexp-quote "\\definecolor{HubArticleCodeDefinitionInk}{HTML}{66325F}") class-contents))
  (should (string-match-p (regexp-quote "\\definecolor{HubArticleCodeOperatorInk}{HTML}{444D56}") class-contents))
  (should (string-match-p (regexp-quote "\\definecolor{HubArticleCodeLineNumberInk}{HTML}{7A857F}") class-contents))
  (should (string-match-p (regexp-quote "\\colorlet{HubArticleCodeInk}{HubArticleCodeBaseInk}") class-contents))
  (should (string-match-p (regexp-quote "\\colorlet{HubArticleCodeKeyword}{HubArticleCodeKeywordInk}") class-contents))
  (should (string-match-p (regexp-quote "\\colorlet{HubArticleCodeComment}{HubArticleCodeCommentInk}") class-contents))
  (should (string-match-p (regexp-quote "\\colorlet{HubArticleCodeString}{HubArticleCodeStringInk}") class-contents))
  (should (string-match-p (regexp-quote "\\colorlet{HubArticleCodeConstant}{HubArticleCodeConstantInk}") class-contents))
  (should (string-match-p (regexp-quote "\\colorlet{HubArticleCodeDefinition}{HubArticleCodeDefinitionInk}") class-contents))
  (should (string-match-p (regexp-quote "\\colorlet{HubArticleCodeOperator}{HubArticleCodeOperatorInk}") class-contents))
  (should (string-match-p (regexp-quote "\\colorlet{HubArticleCodeLineNumber}{HubArticleCodeLineNumberInk}") class-contents))
  (should (string-match-p (regexp-quote "\\def\\HubArticleCodeKeywordColor{HubArticleCodeKeyword}") class-contents))
  (should (string-match-p (regexp-quote "\\def\\HubArticleCodeStringColor{HubArticleCodeString}") class-contents))
  (should (string-match-p (regexp-quote "\\def\\HubArticleCodeCommentColor{HubArticleCodeComment}") class-contents))
  (should (string-match-p (regexp-quote "\\def\\HubArticleCodeConstantColor{HubArticleCodeConstant}") class-contents))
  (should (string-match-p (regexp-quote "\\def\\HubArticleCodeDefinitionColor{HubArticleCodeDefinition}") class-contents))
  (should (string-match-p (regexp-quote "\\@namedef{PYG@tok@k}") class-contents))
  (should (string-match-p (regexp-quote "\\@namedef{PYG@tok@c}") class-contents))
  (should (string-match-p (regexp-quote "\\@namedef{PYG@tok@s}") class-contents))
  (should (string-match-p (regexp-quote "\\@namedef{PYG@tok@nf}") class-contents))
  (should (string-match-p (regexp-quote "\\@namedef{PYG@tok@nc}") class-contents))
  (should (string-match-p (regexp-quote "\\@namedef{PYG@tok@o}") class-contents))
  (should (string-match-p (regexp-quote "\\@namedef{PYG@tok@p}") class-contents))
  (should (string-match-p (regexp-quote "\\@namedef{PYG@tok@m}") class-contents)))

(defun hub/test-veriff-slice-specimen (variant)
  "Return the slice specimen path for Veriff VARIANT."
  (expand-file-name
   (format "test/fixtures/org-export/slice-en-veriff-%s.org" variant)
   hub/test-repo-root))

(defun hub/test-veriff-slice-artifact-root (variant)
  "Return the stable real-smoke artifact root for Veriff VARIANT."
  (expand-file-name
   (format "var/org-latex-pdf/veriff-%s-slice" variant)
   hub/test-repo-root))

(defun hub/test-veriff-real-export-readiness-reason ()
  "Return nil when the real Veriff PDF smoke can run, else a reason."
  (cond
   ((not (executable-find "xelatex"))
    "XeLaTeX executable is not available")
   ((not (file-exists-p (expand-file-name "etc/latex/veriff.cls" hub/test-repo-root)))
    "tracked veriff.cls asset is not available")
   ((not (file-exists-p (expand-file-name "etc/latex/assets/hero-pattern.png" hub/test-repo-root)))
    "hero-pattern.png asset is not available")
   ((not (file-exists-p (expand-file-name "etc/latex/assets/hero-logo.pdf" hub/test-repo-root)))
    "hero-logo.pdf asset is not available")
   ((not (file-exists-p (expand-file-name "etc/latex/assets/hero-logo-dark.pdf" hub/test-repo-root)))
    "hero-logo-dark.pdf asset is not available")))

(defun hub/test-skip-unless-veriff-real-export-ready ()
  "Skip the current ERT test unless real Veriff PDF export is ready."
  (when-let* ((reason (hub/test-veriff-real-export-readiness-reason)))
    (ert-skip (format "Real Veriff PDF smoke unavailable: %s" reason))))

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

(defmacro hub/test-should-user-error-message (message-regexp &rest body)
  "Expect BODY to signal `user-error' whose message matches MESSAGE-REGEXP."
  (declare (indent 1))
  `(condition-case err
       (progn
	 ,@body
	 (ert-fail "Expected user-error"))
     (user-error
      (should (string-match-p ,message-regexp (error-message-string err))))))

(defmacro hub/test-with-org-buffer (contents &rest body)
  "Run BODY in a temporary Org buffer initialised with CONTENTS."
  (declare (indent 1))
  `(with-temp-buffer
     (insert ,contents)
     (org-mode)
     ,@body))

(defun hub/test-export-veriff-slice-with-stubbed-pdf (variant)
  "Assert VARIANT exports to a generated `.tex' file and stubbed PDF."
  (let* ((specimen (hub/test-veriff-slice-specimen variant))
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
							     (pdf-path (hub/org-export-buffer-to-pdf artifact-root)))
							(should (file-exists-p tex-path))
							(should (file-exists-p pdf-path))
							(should (string-suffix-p ".tex" tex-path))
							(should (string-suffix-p ".pdf" pdf-path))
							(should (string-match-p (regexp-quote "\\documentclass[11pt,a4paper]{veriff}") tex-contents))
							(should (string-match-p (regexp-quote (format "\\HubVeriffVariant{%s}" variant)) tex-contents)))))))
      (when (buffer-live-p specimen-buffer)
	(kill-buffer specimen-buffer))
      (when (file-directory-p artifact-root)
	(delete-directory artifact-root t)))))

(defun hub/test-export-veriff-slice-with-real-pdf (variant)
  "Assert VARIANT exports to a real PDF under its stable artifact root."
  (let* ((specimen (hub/test-veriff-slice-specimen variant))
	 (artifact-root (hub/test-veriff-slice-artifact-root variant))
	 (specimen-buffer nil))
    (when (file-directory-p artifact-root)
      (delete-directory artifact-root t))
    (unwind-protect
	(progn
	  (setq specimen-buffer (find-file-noselect specimen))
	  (with-current-buffer specimen-buffer
	    (let* ((hub/org-export-output-root artifact-root)
		   (tex-path (hub/org-export-buffer-to-latex artifact-root))
		   (tex-contents (hub/test-read-file-as-string tex-path))
		   (pdf-path (hub/org-export-buffer-to-pdf artifact-root)))
	      (should (string-prefix-p artifact-root tex-path))
	      (should (string-prefix-p artifact-root pdf-path))
	      (should (file-exists-p tex-path))
	      (should (file-exists-p pdf-path))
	      (should (string-match-p (regexp-quote "\\documentclass[11pt,a4paper]{veriff}") tex-contents))
	      (should (string-match-p (regexp-quote (format "\\HubVeriffVariant{%s}" variant)) tex-contents)))))
      (when (buffer-live-p specimen-buffer)
	(kill-buffer specimen-buffer)))))

(ert-deftest hub/org-export-veriff-variant-defaults-to-refresh-overdrive ()
  "The Veriff variant defaults to refresh-overdrive when omitted."
  (hub/test-with-org-buffer "#+LATEX_CLASS: veriff\n"
			    (should (equal (hub/org-export--effective-veriff-variant)
					   "refresh-overdrive"))))

(ert-deftest hub/org-export-veriff-variant-trims-surrounding-whitespace ()
  "Variant parsing trims surrounding whitespace before validation."
  (hub/test-with-org-buffer "#+LATEX_CLASS: veriff\n#+LATEX_VARIANT:   dark-campaign  \n"
			    (should (equal (hub/org-export--effective-veriff-variant)
					   "dark-campaign"))))

(ert-deftest hub/org-export-veriff-variant-rejects-duplicates ()
  "Duplicate variant keywords fail with a user error."
  (hub/test-with-org-buffer "#+LATEX_CLASS: veriff\n#+LATEX_VARIANT: refresh-overdrive\n#+LATEX_VARIANT: dark-campaign\n"
			    (hub/test-should-user-error-message "LATEX_VARIANT"
								(hub/org-export--validate-veriff-metadata 'latex))))

(ert-deftest hub/org-export-veriff-variant-rejects-mixed-case-values ()
  "Mixed-case variants fail with a deterministic error."
  (hub/test-with-org-buffer "#+LATEX_CLASS: veriff\n#+LATEX_VARIANT: Refresh-Overdrive\n"
			    (hub/test-should-user-error-message (regexp-quote "refresh-overdrive, dark-campaign, gallery-white")
								(hub/org-export--validate-veriff-metadata 'latex))))

(ert-deftest hub/org-export-veriff-variant-rejects-non-veriff-class ()
  "Variants are only valid when the document targets veriff."
  (hub/test-with-org-buffer "#+LATEX_CLASS: article\n#+LATEX_VARIANT: refresh-overdrive\n"
			    (hub/test-should-user-error-message (regexp-quote "LATEX_VARIANT requires LATEX_CLASS: veriff")
								(hub/org-export--validate-veriff-metadata 'latex))))

(ert-deftest hub/org-export-veriff-variant-rejects-legacy-class ()
  "Legacy pro-refresh-overdrive authoring fails with migration guidance."
  (hub/test-with-org-buffer "#+LATEX_CLASS: pro-refresh-overdrive\n#+LATEX_VARIANT: refresh-overdrive\n"
			    (hub/test-should-user-error-message (regexp-quote "LATEX_CLASS: veriff")
								(hub/org-export--validate-veriff-metadata 'latex))
			    (hub/test-should-user-error-message (regexp-quote "LATEX_VARIANT: refresh-overdrive")
								(hub/org-export--validate-veriff-metadata 'latex))))

(ert-deftest hub/org-export-veriff-gallery-white-requires-leading-standfirst ()
  "Gallery-white requires exactly one standfirst as its first content block."
  (dolist (fixture '("#+TITLE: Missing standfirst\n#+LATEX_CLASS: veriff\n#+LATEX_VARIANT: gallery-white\n\n* Body\nText.\n"
		     "#+TITLE: Late standfirst\n#+LATEX_CLASS: veriff\n#+LATEX_VARIANT: gallery-white\n\nIntro first.\n\n#+begin_standfirst\nLate abstract.\n#+end_standfirst\n\n* Body\nText.\n"
		     "#+TITLE: Duplicate standfirst\n#+LATEX_CLASS: veriff\n#+LATEX_VARIANT: gallery-white\n\n#+begin_standfirst\nFirst abstract.\n#+end_standfirst\n\n#+begin_standfirst\nSecond abstract.\n#+end_standfirst\n\n* Body\nText.\n"))
    (hub/test-with-org-buffer fixture
			      (hub/test-should-user-error-message (regexp-quote "gallery-white")
								  (hub/org-export--validate-veriff-metadata 'latex)))))

(ert-deftest hub/org-export-veriff-gallery-white-protects-standfirst-brackets ()
  "Gallery-white wraps the full-width opener so standfirst brackets are safe."
  (let ((artifact-root (hub/test-make-export-artifact-root)))
    (unwind-protect
	(hub/test-with-org-buffer "#+TITLE: Bracket standfirst\n#+LATEX_CLASS: veriff\n#+LATEX_VARIANT: gallery-white\n\n#+begin_standfirst\nThis abstract contains a literal ] bracket.\n#+end_standfirst\n\n* Body\nText.\n"
				  (let* ((hub/org-export-output-root artifact-root)
					 (tex-path (hub/org-export-buffer-to-latex artifact-root))
					 (tex-contents (hub/test-read-file-as-string tex-path)))
				    (should (string-match-p (regexp-quote "\\twocolumn[{") tex-contents))
				    (should (string-match-p (regexp-quote "literal ] bracket") tex-contents))
				    (should (string-match-p (regexp-quote "\\end{standfirst}\n}]\n\\section") tex-contents))))
      (when (file-directory-p artifact-root)
	(delete-directory artifact-root t)))))

(ert-deftest hub/org-export-veriff-refresh-overdrive-stubbed-pdf-end-to-end ()
  "Refresh-overdrive exports to `.tex' and a stubbed PDF path."
  (hub/test-export-veriff-slice-with-stubbed-pdf "refresh-overdrive"))

(ert-deftest hub/org-export-veriff-dark-campaign-stubbed-pdf-end-to-end ()
  "Dark-campaign exports to `.tex' and a stubbed PDF path."
  (hub/test-export-veriff-slice-with-stubbed-pdf "dark-campaign"))

(ert-deftest hub/org-export-veriff-gallery-white-stubbed-pdf-end-to-end ()
  "Gallery-white exports to `.tex' and a stubbed PDF path."
  (hub/test-export-veriff-slice-with-stubbed-pdf "gallery-white"))

(ert-deftest hub/org-export-veriff-dark-campaign-visual-tokens-and-spec-exclusion ()
  "Dark-campaign has named contrast tokens and excludes the white artefact."
  (let ((class-contents (hub/test-read-file-as-string
			 (expand-file-name "etc/latex/veriff.cls" hub/test-repo-root)))
	(spec-contents (hub/test-read-file-as-string
			(expand-file-name "spec/org-latex-pdf/validation/pdf-fidelity.md" hub/test-repo-root))))
    (should (string-match-p (regexp-quote "\\definecolor{HubPaper}{HTML}{0C3035}") class-contents))
    (should (string-match-p (regexp-quote "\\definecolor{HubPaleBlue}{HTML}{F6FDFC}") class-contents))
    (should (string-match-p (regexp-quote "\\colorlet{HubInk}{HubPaleBlue}") class-contents))
    (should (string-match-p (regexp-quote "\\colorlet{HubStandfirst}{HubPaleBlue}") class-contents))
    (should (string-match-p (regexp-quote "\\definecolor{HubSurface}{HTML}{0C3035}") class-contents))
    (should (string-match-p (regexp-quote "\\definecolor{HubSurfaceSoft}{HTML}{0C3035}") class-contents))
    (should (string-match-p (regexp-quote "\\definecolor{HubMetricSurface}{HTML}{085559}") class-contents))
    (should (string-match-p (regexp-quote "\\definecolor{HubTeal}{HTML}{14E5C5}") class-contents))
    (should (string-match-p (regexp-quote "\\definecolor{HubLightTeal}{HTML}{9DF5EA}") class-contents))
    (should (string-match-p (regexp-quote "\\colorlet{HubEyebrowColor}{HubTeal}") class-contents))
    (should (string-match-p (regexp-quote "\\colorlet{HubSectionNumColor}{HubTeal}") class-contents))
    (should (string-match-p (regexp-quote "\\colorlet{HubGraphPrimaryColor}{HubTeal}") class-contents))
    (should (string-match-p (regexp-quote "\\colorlet{HubDekColor}{HubLightTeal}") class-contents))
    (should (string-match-p (regexp-quote "\\colorlet{HubTableHeaderColor}{HubLightTeal}") class-contents))
    (should (string-match-p (regexp-quote "\\colorlet{HubTermColor}{HubLightTeal}") class-contents))
    (should (string-match-p (regexp-quote "\\colorlet{HubMetricValueColor}{HubWhite}") class-contents))
    (should (string-match-p (regexp-quote "\\colorlet{HubCardTitleColor}{HubWhite}") class-contents))
    (should (string-match-p (regexp-quote "\\colorlet{HubCalloutTitleColor}{HubCalloutEdgeColor}") class-contents))
    (should (string-match-p (regexp-quote "\\colorlet{HubStringColor}{HubLightTeal}") class-contents))
    (should (string-match-p (regexp-quote "\\colorlet{HubSubsectionColor}{HubWhite}") class-contents))
    (should (string-match-p (regexp-quote "\\definecolor{HubWhite}{HTML}{F6FDFC}") class-contents))
    (should (string-match-p (regexp-quote "\\colorlet{HubTitleColor}{HubWhite}") class-contents))
    (should (string-match-p (regexp-quote "\\colorlet{HubSectionTitleColor}{HubWhite}") class-contents))
    (should (string-match-p (regexp-quote "\\colorlet{HubQuoteTextColor}{HubWhite}") class-contents))
    (should (string-match-p (regexp-quote "\\colorlet{HubCalloutTextColor}{HubWhite}") class-contents))
    (should-not (string-match-p (regexp-quote "\\colorlet{HubSubsectionColor}{HubTeal}") class-contents))
    (should (string-match-p (regexp-quote "\\color{HubEyebrowColor}") class-contents))
    (should (string-match-p (regexp-quote "\\color{HubSectionNumColor}") class-contents))
    (should (string-match-p (regexp-quote "\\color{HubTableHeaderColor}") class-contents))
    (should (string-match-p (regexp-quote "\\color{HubTermColor}") class-contents))
    (should (string-match-p (regexp-quote "\\textcolor{HubMetricValueColor}") class-contents))
    (should (string-match-p (regexp-quote "\\textcolor{HubCardTitleColor}") class-contents))
    (should (string-match-p (regexp-quote "\\textcolor{HubCalloutTitleColor}") class-contents))
    (should (string-match-p (regexp-quote "\\color{HubTitleColor}") class-contents))
    (should (string-match-p (regexp-quote "\\color{HubSectionTitleColor}") class-contents))
    (should (string-match-p (regexp-quote "\\color{HubQuoteTextColor}") class-contents))
    (should (string-match-p (regexp-quote "\\color{HubCalloutTextColor}") class-contents))
    (should (string-match-p (regexp-quote "{HubGraphPrimaryColor, solid}") class-contents))
    (should (string-match-p (regexp-quote "\\def\\HubCodeStringColor{HubStringColor}") class-contents))
    (should (string-match-p (regexp-quote "/Users/hubertbehaghel/tmp/veriff-article-prototypes/out/01-dark-campaign.pdf") spec-contents))
    (should (string-match-p (regexp-quote "white-background webpage-print artefact/region") spec-contents))
    (should (string-match-p (regexp-quote "must be ignored") spec-contents))))

(ert-deftest hub/org-export-veriff-real-xelatex-smoke-readiness-gated ()
  "Real XeLaTeX smoke exports both variants or reports readiness clearly."
  (hub/test-skip-unless-veriff-real-export-ready)
  (hub/test-export-veriff-slice-with-real-pdf "refresh-overdrive")
  (hub/test-export-veriff-slice-with-real-pdf "dark-campaign")
  (hub/test-export-veriff-slice-with-real-pdf "gallery-white"))

(ert-deftest hub/script-export-approval-refresh-overdrive-page1-prints-buffer-and-path ()
  "The approval export helper prints LaTeX output and uses the variant-aware artifact root."
  (let ((log-buffer (get-buffer-create hub/script-org-pdf-output-buffer-name))
	(specimen-buffer (generate-new-buffer " *approval-specimen*"))
	captured-artifact-root
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
			       (lambda (artifact-root)
				 (setq captured-artifact-root artifact-root)
				 (expand-file-name "page1.pdf" artifact-root))))
		      (hub/script-export-approval-refresh-overdrive-page1 hub/test-repo-root))
		    (buffer-string))))
	  (should (string-match-p (regexp-quote "[Org PDF LaTeX Output] BEGIN") output))
	  (should (string-match-p (regexp-quote "latex output") output))
	  (should (string-match-p (regexp-quote (expand-file-name "page1.pdf" (expand-file-name "var/org-latex-pdf/veriff-refresh-overdrive-page1" hub/test-repo-root))) output))
	  (should (equal captured-artifact-root (expand-file-name "var/org-latex-pdf/veriff-refresh-overdrive-page1" hub/test-repo-root))))
      (when (buffer-live-p log-buffer)
	(kill-buffer log-buffer))
      (when (buffer-live-p specimen-buffer)
	(kill-buffer specimen-buffer)))))

(ert-deftest hub/org-export-veriff-uses-xelatex-only ()
  "`veriff' always selects the XeLaTeX compiler path."
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

(ert-deftest hub/org-export-callout-title-uses-semantic-attribute ()
  "LaTeX callout titles come from `#+ATTR_CALLOUT:'."
  (let ((tex (org-export-string-as "#+ATTR_CALLOUT: :type warning :title \"Heads up\"\n#+begin_callout\nCareful\n#+end_callout"
				   'latex t)))
    (should (string-match-p (regexp-quote "\\begin{callout}[Heads up]") tex))
    (should (string-match-p "Careful" tex))))

(ert-deftest hub/org-export-standard-pdf-compile-uses-generated-compiler-marker ()
  "Standard Org PDF export compilation uses the repo-aware process for local classes."
  (let* ((tex-path (make-temp-file "hub-org-standard-export-" nil ".tex"))
	 (captured-process nil))
    (unwind-protect
	(progn
	  (with-temp-file tex-path
	    (insert "% Intended LaTeX compiler: xelatex\n"
		    "\\documentclass{hub-article}\n"
		    "\\begin{document}\nBody\\end{document}\n"))
	  (cl-letf (((symbol-function 'org-compile-file)
		     (lambda (_source process _ext &rest _args)
		       (setq captured-process process)
		       (concat (file-name-sans-extension tex-path) ".pdf")))
		    ((symbol-function 'org-latex-compile--postprocess)
		     (lambda (outfile _log-buf &optional _snippet) outfile)))
	    (org-latex-compile tex-path))
	  (should (equal captured-process
			 (hub/org-export--pdf-process-for-compiler "xelatex"))))
      (when (file-exists-p tex-path)
	(delete-file tex-path)))))

(ert-deftest hub/org-export-slice-en-veriff-refresh-overdrive-produces-latex-and-pdf ()
  "The first English flagship slice exports to LaTeX and PDF with the XeLaTeX path."
  (let* ((specimen (expand-file-name "test/fixtures/org-export/slice-en-veriff-refresh-overdrive.org"
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
							     (class-path (hub/test-exported-class-path artifact-root "veriff"))
							     (class-contents (hub/test-read-file-as-string class-path))
							     (pdf-path (hub/org-export-buffer-to-pdf artifact-root)))
							(should (string-match-p (regexp-quote "% Intended LaTeX compiler: xelatex") tex-contents))
							(should (string-match-p (regexp-quote "% hub-veriff-refresh-overdrive") tex-contents))
							(should (string-match-p (regexp-quote "\\HubVeriffVariant{refresh-overdrive}") tex-contents))
							(should-not (string-match-p (regexp-quote "% hub-veriff-dark-campaign") tex-contents))
							(should (string-match-p (regexp-quote "\\documentclass[11pt,a4paper]{veriff}") tex-contents))
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

(ert-deftest hub/org-export-hub-article-minimal ()
  "The minimal hub article slice exports with the personal article class."
  (let* ((specimen (expand-file-name "test/fixtures/org-export/slice-en-hub-article-minimal.org"
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
							     (class-path (hub/test-exported-class-path artifact-root "hub-article"))
							     (pdf-path (hub/org-export-buffer-to-pdf artifact-root)))
							(should (string-match-p (regexp-quote "\\documentclass[11pt,a4paper]{hub-article}") tex-contents))
							(should-not (string-match-p (regexp-quote "\\documentclass[11pt,a4paper]{veriff}") tex-contents))
							(should (string-match-p (regexp-quote "\\title{Hub article minimal slice") tex-contents))
							(should (string-match-p "\\author{Hubert Behaghel}" tex-contents))
							(should (string-match-p "\\date{" tex-contents))
							(should (string-match-p "Minimal article heading" tex-contents))
							(should (string-match-p "Minimal hub article paragraph" tex-contents))
							(should (file-exists-p class-path))
							(should-not (file-exists-p (hub/test-exported-class-path artifact-root "veriff")))
							(should (file-exists-p pdf-path)))))))
      (when (buffer-live-p specimen-buffer)
	(kill-buffer specimen-buffer))
      (when (file-directory-p artifact-root)
	(delete-directory artifact-root t)))))

(ert-deftest hub/org-export-hub-article-opener ()
  "The hub article opener is class-owned and avoids Veriff hero leakage."
  (let* ((specimen (expand-file-name "test/fixtures/org-export/slice-en-hub-article-opener.org"
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
							     (class-path (hub/test-exported-class-path artifact-root "hub-article"))
							     (class-contents (hub/test-read-file-as-string class-path))
							     (pdf-path (hub/org-export-buffer-to-pdf artifact-root)))
							(should (string-match-p (regexp-quote "\\documentclass[11pt,a4paper]{hub-article}") tex-contents))
							(should (string-match-p (regexp-quote "\\HubArticleEyebrow{Personal systems}") tex-contents))
							(should (string-match-p (regexp-quote "\\HubArticleTitle{A quiet article opener}") tex-contents))
							(should (string-match-p (regexp-quote "\\HubArticleDek{Notes toward calmer technical authority}") tex-contents))
							(should (string-match-p (regexp-quote "\\HubArticleMeta{Hubert Behaghel}{2026-05-10}") tex-contents))
							(should (string-match-p (regexp-quote "\\begin{standfirst}") tex-contents))
							(should (string-match-p "Reading the surface" tex-contents))
							(should (string-match-p "The opener should feel personal" tex-contents))
							(should-not (string-match-p (regexp-quote "\\maketitle") tex-contents))
							(should-not (string-match-p (regexp-quote "\\HubHeroTitle") tex-contents))
							(should-not (string-match-p (regexp-quote "\\HubVeriffVariant") tex-contents))
							(should-not (string-match-p (regexp-quote "HubHeroLogoGraphic") tex-contents))
							(should-not (string-match-p (regexp-quote "HubHeroPatternGraphic") tex-contents))
							(should (string-match-p (regexp-quote "\\RequirePackage{fontspec}") class-contents))
							(should (string-match-p (regexp-quote "\\setmainfont") class-contents))
							(should (string-match-p (regexp-quote "\\setsansfont") class-contents))
							(should (string-match-p (regexp-quote "\\setmonofont") class-contents))
							(should (string-match-p (regexp-quote "HubArticleSerif") class-contents))
							(should (string-match-p (regexp-quote "HubArticleSans") class-contents))
							(should (string-match-p (regexp-quote "HubArticleMono") class-contents))
							(should (string-match-p (regexp-quote "HubArticleAccentMuted") class-contents))
							(should (string-match-p (regexp-quote "\\newcommand{\\HubArticleTitle}") class-contents))
							(should (file-exists-p pdf-path)))))))
      (when (buffer-live-p specimen-buffer)
	(kill-buffer specimen-buffer))
      (when (file-directory-p artifact-root)
	(delete-directory artifact-root t)))))

(ert-deftest hub/org-export-hub-article-longform ()
  "The long-form hub article slice expects single-column measure markers."
  (let* ((specimen (expand-file-name "test/fixtures/org-export/slice-en-hub-article-longform.org"
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
							     (class-path (hub/test-exported-class-path artifact-root "hub-article"))
							     (class-contents (hub/test-read-file-as-string class-path))
							     (pdf-path (hub/org-export-buffer-to-pdf artifact-root)))
							(should (string-match-p (regexp-quote "\\documentclass[11pt,a4paper]{hub-article}") tex-contents))
							(should-not (string-match-p (regexp-quote "dark]{hub-article}") tex-contents))
							(should (string-match-p "Later reading section" tex-contents))
							(should (string-match-p "The margin should behave like a promise" tex-contents))
							(should (string-match-p "article-reading-path" tex-contents))
							(should (string-match-p "article-review-note" tex-contents))
							(should (string-match-p (regexp-quote "\\begin{minted}") tex-contents))
							(should (string-match-p (regexp-quote "breaklines") tex-contents))
							(hub/test-assert-hub-article-code-syntax-contract tex-contents class-contents)
							(should (string-match-p "keep the prose in one column" tex-contents))
							(should (string-match-p "Footnote text should export as supporting evidence" tex-contents))
							(should (string-match-p "single-column prose" tex-contents))
							(should (string-match-p "compact table that should stay within the article reading measure" tex-contents))
							(should-not (string-match-p (regexp-quote "\\twocolumn") tex-contents))
							(should-not (string-match-p (regexp-quote "% hub-veriff-gallery-white") tex-contents))
							(should-not (string-match-p (regexp-quote "\\begin{figure*}") tex-contents))
							(should-not (string-match-p (regexp-quote "\\begin{table*}") tex-contents))
							(should-not (string-match-p (regexp-quote "hubgallerywhite") class-contents))
							(should (string-match-p (regexp-quote "\\RequirePackage{geometry}") class-contents))
							(should (string-match-p (regexp-quote "\\newlength{\\HubArticleReadingMeasure}") class-contents))
							(should (string-match-p (regexp-quote "\\newlength{\\HubArticleMarginZoneWidth}") class-contents))
							(should (string-match-p (regexp-quote "\\newcommand{\\HubArticleMarginMarker}") class-contents))
							(should (string-match-p (regexp-quote "\\HubArticleMarginZone") class-contents))
							(should (file-exists-p pdf-path)))))))
      (when (buffer-live-p specimen-buffer)
	(kill-buffer specimen-buffer))
      (when (file-directory-p artifact-root)
	(delete-directory artifact-root t)))))

(ert-deftest hub/org-export-hub-article-code ()
  "The hub article code slice keeps technical artifacts quiet and personal."
  (let* ((specimen (expand-file-name "test/fixtures/org-export/slice-en-hub-article-code.org"
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
							     (class-path (hub/test-exported-class-path artifact-root "hub-article"))
							     (class-contents (hub/test-read-file-as-string class-path))
							     (pdf-path (hub/org-export-buffer-to-pdf artifact-root)))
							(should (string-match-p (regexp-quote "\\documentclass[11pt,a4paper]{hub-article}") tex-contents))
							(should-not (string-match-p (regexp-quote "dark]{hub-article}") tex-contents))
							(should (string-match-p "article-cache-key" tex-contents))
							(should (string-match-p "cache-key" tex-contents))
							(should (string-match-p (regexp-quote "\\HubArticleBold{bold\nemphasis}") tex-contents))
							(should-not (string-match-p (regexp-quote "\\textbf{bold\nemphasis}") tex-contents))
							(should (string-match-p "italic aside" tex-contents))
							(should (string-match-p "struck-through draft" tex-contents))
							(should (string-match-p "https://example.com/personal-systems" tex-contents))
							(should (string-match-p "related note" tex-contents))
							(should (string-match-p "Unordered reader note" tex-contents))
							(should (string-match-p "First numbered checkpoint" tex-contents))
							(should (string-match-p "Term marker" tex-contents))
							(should (string-match-p "Definition list copy" tex-contents))
							(should (string-match-p (regexp-quote "\\HubArticleDefinitionTerm{Term marker}") tex-contents))
							(should (string-match-p (regexp-quote "\\HubArticleDefinitionTerm{Quiet artifact}") tex-contents))
							(should (string-match-p "quiet prose-level sample" tex-contents))
							(should (string-match-p "Technical artifacts are furniture for thought" tex-contents))
							(should (string-match-p "after wrapping" tex-contents))
							(should (string-match-p "Small technical decisions" tex-contents))
							(should (string-match-p "A small image should share the same quiet caption hierarchy" tex-contents))
							(should (string-match-p "Source block" tex-contents))
							(should (string-match-p "callout-like prose" tex-contents))
							(should (file-exists-p class-path))
							(should-not (file-exists-p (hub/test-exported-class-path artifact-root "veriff")))
							(should-not (string-match-p (regexp-quote "\\HubVeriff") tex-contents))
							(should-not (string-match-p (regexp-quote "hubgallerywhite") tex-contents))
							(should-not (string-match-p (regexp-quote "gallery-white") tex-contents))
							(should-not (string-match-p (regexp-quote "\\twocolumn") tex-contents))
							(should-not (string-match-p (regexp-quote "\\begin{figure*}") tex-contents))
							(should-not (string-match-p (regexp-quote "\\begin{table*}") tex-contents))
							(should-not (string-match-p (regexp-quote "HubVeriff") class-contents))
							(should-not (string-match-p (regexp-quote "hubgallerywhite") class-contents))
							(should-not (string-match-p (regexp-quote "gallery-white") class-contents))
							(should-not (string-match-p (regexp-quote "\\twocolumn") class-contents))
							(should-not (string-match-p (regexp-quote "\\begin{figure*}") class-contents))
							(should-not (string-match-p (regexp-quote "\\begin{table*}") class-contents))
							(should-not (string-match-p (regexp-quote "\\newif\\ifHubArticleDarkMode") class-contents))
							(should-not (string-match-p (regexp-quote "\\DeclareOption{light}") class-contents))
							(should-not (string-match-p (regexp-quote "\\DeclareOption{dark}") class-contents))
							(should-not (string-match-p (regexp-quote "HubArticleDarkPaper") class-contents))
							(should-not (string-match-p (regexp-quote "HubArticlePaper") class-contents))
							(should-not (string-match-p (regexp-quote "\\pagecolor") class-contents))
							(should (string-match-p (regexp-quote "\\colorlet{HubArticleBoldColor}{HubArticleLightAccentMuted}") class-contents))
							(should (string-match-p (regexp-quote "\\newcommand{\\HubArticleBold}[1]{{\\color{HubArticleBoldColor}\\textbf{#1}}}") class-contents))
							(should (string-match-p (regexp-quote "\\AtBeginDocument{\\color{HubArticleInk}}") class-contents))
							(should (string-match-p (regexp-quote "\\setcounter{secnumdepth}{0}") class-contents))
							(should (string-match-p (regexp-quote "\\newcommand{\\HubArticleInlineCode}") class-contents))
							(should (string-match-p (regexp-quote "\\begin{minted}") tex-contents))
							(should (string-match-p (regexp-quote "breaklines=true") tex-contents))
							(hub/test-assert-hub-article-code-syntax-contract tex-contents class-contents)
							(should (string-match-p (regexp-quote "\\RequirePackage{tabularx}") class-contents))
							(should (string-match-p (regexp-quote "\\RequirePackage{caption}") class-contents))
							(should (string-match-p (regexp-quote "\\captionsetup[table]") class-contents))
							(should (string-match-p (regexp-quote "\\captionsetup[figure]") class-contents))
							(should (string-match-p (regexp-quote "justification=centering") class-contents))
							(should (string-match-p (regexp-quote "\\HubArticleSans\\normalsize") class-contents))
							(should (string-match-p (regexp-quote "\\newlength{\\HubArticleTableWidth}") class-contents))
							(should (string-match-p (regexp-quote "\\newlength{\\HubArticleFigureAlignCorrection}") class-contents))
							(should (string-match-p (regexp-quote "\\newcommand{\\HubArticleCenteredBlock}") class-contents))
							(should (string-match-p (regexp-quote "\\newcommand{\\HubArticleFigureImage}") class-contents))
							(should (string-match-p (regexp-quote "\\newcommand{\\HubArticleTableHeaderCell}") class-contents))
							(should (string-match-p (regexp-quote "\\newcommand{\\HubArticleDefinitionTerm}") class-contents))
							(should (string-match-p (regexp-quote "\\begin{HubArticleTable}") tex-contents))
							(should (string-match-p (regexp-quote "\\begin{tabularx}{\\HubArticleTableWidth}") tex-contents))
							(should (string-match-p (regexp-quote "\\begin{tabularx}{\\HubArticleTableWidth}{lH}") tex-contents))
							(should (string-match-p (regexp-quote "\\HubArticleTableHeaderCell{Artifact}") tex-contents))
							(should (string-match-p (regexp-quote "\\HubArticleTableHeaderCell{Personal treatment}") tex-contents))
							(should (string-match-p (regexp-quote "\\HubArticleTableHeadRule") tex-contents))
							(should (string-match-p (regexp-quote "\\HubArticleFigureImage{\\includegraphics") tex-contents))
							(should-not (string-match-p (regexp-quote "\\RecustomVerbatimEnvironment{verbatim}{Verbatim}") class-contents))
							(should-not (string-match-p (regexp-quote "fillcolor=\\color{HubArticleCodeBackground}") class-contents))
							(should-not (string-match-p (regexp-quote "fillcolor=HubArticleCodeBackground") class-contents))
							(should (string-match-p (regexp-quote "\\renewenvironment{HubArticleCallout}") class-contents))
							(should (string-match-p (regexp-quote "\\newenvironment{callout}") class-contents))
							(should (string-match-p (regexp-quote "\\newenvironment{HubArticleQuote}") class-contents))
							(should (string-match-p (regexp-quote "\\renewenvironment{quote}") class-contents))
							(should (string-match-p (regexp-quote "\\begin{tabular}{@{}!{") class-contents))
							(should (string-match-p (regexp-quote "\\vrule width 0.55pt") class-contents))
							(should (string-match-p (regexp-quote "p{\\dimexpr\\linewidth-0.85em-0.55pt\\relax}") class-contents))
							(should-not (string-match-p (regexp-quote "\\llap{\\textcolor{HubArticleQuoteRule}") class-contents))
							(should (string-match-p (regexp-quote "\\colorlet{HubArticleTableRule}{HubArticleLightTableRule}") class-contents))
							(should-not (string-match-p (regexp-quote "\\colorlet{HubArticleTableRule}{HubArticleDarkTableRule}") class-contents))
							(should (string-match-p (regexp-quote "\\textcolor{HubArticleRule}{\\thepage}") class-contents))
							(should (string-match-p (regexp-quote "\\renewenvironment{table}[1][]") class-contents))
							(should (string-match-p (regexp-quote "\\color{HubArticleInk}") class-contents))
							(should (file-exists-p pdf-path)))))))
      (when (buffer-live-p specimen-buffer)
	(kill-buffer specimen-buffer))
      (when (file-directory-p artifact-root)
	(delete-directory artifact-root t)))))

(ert-deftest hub/org-export-hub-article-french ()
  "The hub article French slice uses shared locale wiring and preserves text."
  (let* ((specimen (expand-file-name "test/fixtures/org-export/slice-fr-hub-article-minimal.org"
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
							     (class-path (hub/test-exported-class-path artifact-root "hub-article"))
							     (class-contents (hub/test-read-file-as-string class-path))
							     (pdf-path (hub/org-export-buffer-to-pdf artifact-root)))
							(should (string-match-p (regexp-quote "\\documentclass[11pt,a4paper]{hub-article}") tex-contents))
							(should (string-match-p "\\\\usepackage\\[[^]]*french[^]]*\\]{babel}" tex-contents))
							(should-not (string-match-p (regexp-quote "\\usepackage{polyglossia}") tex-contents))
							(should (string-match-p (regexp-quote "Précision française") tex-contents))
							(should (string-match-p (regexp-quote "« système »") tex-contents))
							(should (string-match-p (regexp-quote "Pourquoi ça compte ?") tex-contents))
							(should (string-match-p (regexp-quote "Attention : le détail compte ;") tex-contents))
							(should (string-match-p (regexp-quote "aussi !") tex-contents))
							(should (string-match-p (regexp-quote "clé-technique") tex-contents))
							(should (string-match-p (regexp-quote "État") tex-contents))
							(should (file-exists-p class-path))
							(should-not (string-match-p (regexp-quote "babel") class-contents))
							(should-not (string-match-p (regexp-quote "polyglossia") class-contents))
							(should-not (string-match-p (regexp-quote "french") class-contents))
							(should-not (file-exists-p (hub/test-exported-class-path artifact-root "veriff")))
							(should (file-exists-p pdf-path)))))))
      (when (buffer-live-p specimen-buffer)
	(kill-buffer specimen-buffer))
      (when (file-directory-p artifact-root)
	(delete-directory artifact-root t)))))

(ert-deftest hub/org-export-slice-en-veriff-default-variant-produces-latex-and-pdf ()
  "The omitted variant defaults to refresh-overdrive and stages veriff.cls."
  (let* ((specimen (expand-file-name "test/fixtures/org-export/slice-en-veriff-default-variant.org"
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
							     (class-path (hub/test-exported-class-path artifact-root "veriff"))
							     (class-contents (hub/test-read-file-as-string class-path))
							     (pdf-path (hub/org-export-buffer-to-pdf artifact-root)))
							(should (string-match-p (regexp-quote "\\documentclass[11pt,a4paper]{veriff}") tex-contents))
							(should (string-match-p (regexp-quote "% hub-veriff-default-variant") tex-contents))
							(should (string-match-p (regexp-quote "\\HubVeriffVariant{refresh-overdrive}") tex-contents))
							(should (string-match-p (regexp-quote "% hub-veriff-refresh-overdrive") tex-contents))
							(should-not (string-match-p (regexp-quote "% hub-veriff-dark-campaign") tex-contents))
							(should (string-match-p (regexp-quote "\\RequirePackage{fontspec}") class-contents))
							(should (file-exists-p class-path))
							(should (file-exists-p pdf-path)))))))
      (when (buffer-live-p specimen-buffer)
	(kill-buffer specimen-buffer))
      (when (file-directory-p artifact-root)
	(delete-directory artifact-root t)))))

(ert-deftest hub/org-export-slice-en-veriff-dark-campaign-produces-latex-and-pdf ()
  "The dark campaign variant stages veriff.cls and the dark marker."
  (let* ((specimen (expand-file-name "test/fixtures/org-export/slice-en-veriff-dark-campaign.org"
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
							     (class-path (hub/test-exported-class-path artifact-root "veriff"))
							     (class-contents (hub/test-read-file-as-string class-path))
							     (pdf-path (hub/org-export-buffer-to-pdf artifact-root)))
							(should (string-match-p (regexp-quote "\\documentclass[11pt,a4paper]{veriff}") tex-contents))
							(should (string-match-p (regexp-quote "% hub-veriff-dark-campaign") tex-contents))
							(should (string-match-p (regexp-quote "\\HubVeriffVariant{dark-campaign}") tex-contents))
							(should (string-match-p (regexp-quote "\\definecolor{HubPaper}{HTML}{0C3035}") class-contents))
							(should (string-match-p (regexp-quote "\\colorlet{HubInlineCodeBg}{HubInlineCodeBrown}") class-contents))
							(should (string-match-p (regexp-quote "\\colorlet{HubInlineCodeText}{HubInlineCodeOrange}") class-contents))
							(should-not (string-match-p (regexp-quote "% hub-veriff-default-variant") tex-contents))
							(should (string-match-p (regexp-quote "\\RequirePackage{fontspec}") class-contents))
							(should (string-match-p (regexp-quote "hero-logo-dark.pdf") tex-contents))
							(should (string-match-p (regexp-quote "hero-pattern-dark.pdf") tex-contents))
							(should-not (string-match-p (regexp-quote "\\renewcommand{\\HubHeroPatternGraphic}{}") tex-contents))
							(should-not (string-match-p (regexp-quote "hero-pattern.png") tex-contents))
							(should (file-exists-p class-path))
							(should (file-exists-p pdf-path)))))))
      (when (buffer-live-p specimen-buffer)
	(kill-buffer specimen-buffer))
      (when (file-directory-p artifact-root)
	(delete-directory artifact-root t)))))

(ert-deftest hub/org-export-slice-en-veriff-gallery-white-produces-latex-and-pdf ()
  "The gallery white variant stages veriff.cls and white-paper layout markers."
  (let* ((specimen (expand-file-name "test/fixtures/org-export/slice-en-veriff-gallery-white.org"
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
							     (class-path (hub/test-exported-class-path artifact-root "veriff"))
							     (class-contents (hub/test-read-file-as-string class-path))
							     (pdf-path (hub/org-export-buffer-to-pdf artifact-root)))
							(should (string-match-p (regexp-quote "\\documentclass[11pt,a4paper]{veriff}") tex-contents))
							(should (string-match-p (regexp-quote "% hub-veriff-gallery-white") tex-contents))
							(should (string-match-p (regexp-quote "\\HubVeriffVariant{gallery-white}") tex-contents))
							(should (string-match-p (regexp-quote "\\twocolumn[{") tex-contents))
							(should (string-match-p (regexp-quote "\\end{standfirst}\n}]\n\\section") tex-contents))
							(should (string-match-p (regexp-quote "\\newif\\ifhubgallerywhite") class-contents))
							(should (string-match-p (regexp-quote "\\definecolor{HubGalleryPaper}{HTML}{F6FDFC}") class-contents))
							(should (string-match-p (regexp-quote "\\newcommand{\\HubGalleryWhiteBodyFont}") class-contents))
							(should (string-match-p (regexp-quote "\\AtBeginDocument{\\ifhubgallerywhite\\HubGalleryWhiteBodyFont\\fi}") class-contents))
							(should (string-match-p (regexp-quote "\\begin{minipage}{0.86\\textwidth}") class-contents))
							(should (string-match-p (regexp-quote "\\color{HubStandfirst}\\itshape\\fontsize{12.6}{16.2}\\selectfont") class-contents))
							(should (string-match-p (regexp-quote "\\begin{minipage}{0.88\\linewidth}") class-contents))
							(should (string-match-p (regexp-quote "\\fontsize{10.8}{14}\\selectfont") class-contents))
							(should (string-match-p (regexp-quote "\\begin{figure*}") tex-contents))
							(should (string-match-p (regexp-quote "\\begin{table*}") tex-contents))
							(should (string-match-p (regexp-quote "\\begin{graph*}") tex-contents))
							(should-not (string-match-p (regexp-quote "fullwidth") tex-contents))
							(should (string-match-p (regexp-quote "Gallery white slice") tex-contents))
							(should (file-exists-p class-path))
							(should (file-exists-p pdf-path)))))))
      (when (buffer-live-p specimen-buffer)
	(kill-buffer specimen-buffer))
      (when (file-directory-p artifact-root)
	(delete-directory artifact-root t)))))

(ert-deftest hub/org-export-slice-en-veriff-invalid-variant-fails ()
  "Unknown variants fail with a user error."
  (let* ((specimen (expand-file-name "test/fixtures/org-export/slice-en-veriff-invalid-variant.org"
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
							(hub/test-should-user-error-message
							 (regexp-quote "refresh-overdrive, dark-campaign, gallery-white")
							 (hub/org-export-buffer-to-latex artifact-root)))))))
      (when (buffer-live-p specimen-buffer)
	(kill-buffer specimen-buffer))
      (when (file-directory-p artifact-root)
	(delete-directory artifact-root t)))))

(ert-deftest hub/org-export-slice-en-veriff-duplicate-variant-fails ()
  "Duplicate variant keywords fail with a user error."
  (let* ((specimen (expand-file-name "test/fixtures/org-export/slice-en-veriff-duplicate-variant.org"
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
							(hub/test-should-user-error-message
							 "LATEX_VARIANT"
							 (hub/org-export-buffer-to-latex artifact-root)))))))
      (when (buffer-live-p specimen-buffer)
	(kill-buffer specimen-buffer))
      (when (file-directory-p artifact-root)
	(delete-directory artifact-root t)))))

(ert-deftest hub/org-export-slice-en-veriff-mixed-case-variant-fails ()
  "Mixed-case variant keywords fail with a user error."
  (let* ((specimen (expand-file-name "test/fixtures/org-export/slice-en-veriff-mixed-case-variant.org"
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
							(hub/test-should-user-error-message
							 "Refresh-Overdrive"
							 (hub/org-export-buffer-to-latex artifact-root)))))))
      (when (buffer-live-p specimen-buffer)
	(kill-buffer specimen-buffer))
      (when (file-directory-p artifact-root)
	(delete-directory artifact-root t)))))

(ert-deftest hub/org-export-slice-en-legacy-pro-refresh-overdrive-fails ()
  "The legacy class fails with migration guidance."
  (let* ((specimen (expand-file-name "test/fixtures/org-export/slice-en-legacy-pro-refresh-overdrive.org"
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
							(hub/test-should-user-error-message
							 (regexp-quote "#+LATEX_CLASS: veriff")
							 (hub/org-export-buffer-to-latex artifact-root))
							(hub/test-should-user-error-message
							 (regexp-quote "#+LATEX_VARIANT: refresh-overdrive")
							 (hub/org-export-buffer-to-latex artifact-root)))))))
      (when (buffer-live-p specimen-buffer)
	(kill-buffer specimen-buffer))
      (when (file-directory-p artifact-root)
	(delete-directory artifact-root t)))))

(ert-deftest hub/org-export-slice-en-non-veriff-with-variant-fails ()
  "A non-veriff class with a variant fails with a user error."
  (let* ((specimen (expand-file-name "test/fixtures/org-export/slice-en-non-veriff-with-variant.org"
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
							(hub/test-should-user-error-message
							 "LATEX_VARIANT"
							 (hub/org-export-buffer-to-latex artifact-root)))))))
      (when (buffer-live-p specimen-buffer)
	(kill-buffer specimen-buffer))
      (when (file-directory-p artifact-root)
	(delete-directory artifact-root t)))))

(ert-deftest hub/org-export-slice-en-non-veriff-inline-markup-unaffected ()
  "A non-veriff class does not use Veriff inline markup macros."
  (let* ((specimen (expand-file-name "test/fixtures/org-export/slice-en-non-veriff-inline.org"
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
							(should-not (string-match-p (regexp-quote "\\HubInlineBold") tex-contents))
							(should-not (string-match-p (regexp-quote "\\HubInlineItalic") tex-contents))
							(should-not (string-match-p (regexp-quote "\\HubInlineUnderline") tex-contents))
							(should-not (string-match-p (regexp-quote "\\HubInlineStrike") tex-contents))
							(should-not (string-match-p (regexp-quote "\\HubInlineCode") tex-contents))
							(should (string-match-p (regexp-quote "\\texttt{code}") tex-contents))
							(should (string-match-p (regexp-quote "\\texttt{verbatim}") tex-contents)))))))
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
							     (class-path (hub/test-exported-class-path artifact-root "veriff"))
							     (class-contents (hub/test-read-file-as-string class-path))
							     (pdf-path (hub/org-export-buffer-to-pdf artifact-root)))
							(should (string-match-p (regexp-quote "% Intended LaTeX compiler: xelatex") tex-contents))
							(should (string-match-p (regexp-quote "\\documentclass[11pt,a4paper]{veriff}") tex-contents))
							(should-not (string-match-p "\\maketitle" tex-contents))
							(should-not (string-match-p "\\tableofcontents" tex-contents))
							(should (string-match-p (regexp-quote "% hub-veriff-refresh-overdrive-title") tex-contents))
							(should (string-match-p (regexp-quote "% hub-veriff-refresh-overdrive-headings") tex-contents))
							(should (string-match-p (regexp-quote "\\pagestyle{hubpage}") class-contents))
							(should (string-match-p (regexp-quote "\\newlength{\\HubFooterLift}") class-contents))
							(should (string-match-p (regexp-quote "\\setlength{\\HubFooterLift}{-0.5mm}") class-contents))
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
							(should (string-match-p (regexp-quote "\\HubInlineBold{bold}") tex-contents))
							(should (string-match-p (regexp-quote "\\HubInlineItalic{italic}") tex-contents))
							(should (string-match-p (regexp-quote "\\HubInlineUnderline{underline}") tex-contents))
							(should (string-match-p (regexp-quote "\\HubInlineStrike{strike-through}") tex-contents))
							(should (string-match-p (regexp-quote "\\HubInlineCode{risk\\_score}") tex-contents))
							(should (string-match-p (regexp-quote "\\HubInlineCode{verbatim\\_flag}") tex-contents))
							(should (string-match-p (regexp-quote "\\definecolor{HubInlineCodeBrown}{HTML}{543F21}") class-contents))
							(should (string-match-p (regexp-quote "\\definecolor{HubInlineCodeBrownLifted}{HTML}{845D33}") class-contents))
							(should (string-match-p (regexp-quote "\\definecolor{HubInlineCodeOrange}{HTML}{FF9A52}") class-contents))
							(should (string-match-p (regexp-quote "\\definecolor{HubInlineCodeOrangeSoft}{HTML}{FBEDE8}") class-contents))
							(should (string-match-p (regexp-quote "\\colorlet{HubInlineCodeBg}{HubInlineCodeOrangeSoft}") class-contents))
							(should (string-match-p (regexp-quote "\\colorlet{HubInlineCodeText}{HubInlineCodeBrownLifted}") class-contents))
							(should (string-match-p (regexp-quote "\\colorlet{HubInlineCodeFrame}{HubAccent}") class-contents))
							(should (string-match-p (regexp-quote "\\newtcbox{\\HubInlineCode}") class-contents))
							(should (string-match-p (regexp-quote "fontupper=\\ttfamily\\bfseries\\fontsize{7.5pt}{9pt}\\selectfont") class-contents))
							(should (string-match-p (regexp-quote "\\definecolor{HubDekWarm}{HTML}{7D1F1F}") class-contents))
							(should (string-match-p (regexp-quote "\\colorlet{HubEmphasisColor}{HubAccent}") class-contents))
							(should (string-match-p (regexp-quote "\\colorlet{HubCalloutEdgeColor}{HubDekWarm}") class-contents))
							(should (string-match-p (regexp-quote "\\colorlet{HubCalloutTitleColor}{HubCalloutEdgeColor}") class-contents))
							(should (string-match-p (regexp-quote "\\colorlet{HubCalloutEdgeColor}{HubDekWarm}
\\colorlet{HubCalloutTitleColor}{HubCalloutEdgeColor}") class-contents))
							(should (string-match-p (regexp-quote "\\newcommand{\\HubInlineBold}[1]{{\\color{HubEmphasisColor}\\textbf{#1}}}") class-contents))
							(should (string-match-p (regexp-quote "\\newcommand{\\HubInlineItalic}") class-contents))
							(should (string-match-p (regexp-quote "\\newcommand{\\HubInlineUnderline}") class-contents))
							(should (string-match-p (regexp-quote "\\newcommand{\\HubInlineStrike}") class-contents))
							(should (string-match-p (regexp-quote "\\begin{standfirst}") tex-contents))
							(should (string-match-p (regexp-quote "\\begin{epigraph}[Operational proverb]") tex-contents))
							(should (string-match-p (regexp-quote "Proof is strongest when every decision can explain itself") tex-contents))
							(should (string-match-p (regexp-quote "\\newenvironment{epigraph}") class-contents))
							(should (string-match-p (regexp-quote "\\HubEpigraphFrame") class-contents))
							(should (string-match-p (regexp-quote "\\node[inner xsep=18pt,inner ysep=14pt,outer sep=0pt] (hubepigraph)") class-contents))
							(should (string-match-p (regexp-quote "\\draw[HubAccent,line width=0.8pt]") class-contents))
							(should-not (string-match-p (regexp-quote "draw=HubLine,line width=0.6pt,fill=HubSurfaceSoft") class-contents))
							(should (string-match-p (regexp-quote "\\begin{quote}[Prototype testimonial · Global platform operator]") tex-contents))
							(should (string-match-p (regexp-quote "\\fontsize{12.5}{16}\\selectfont\\bfseries\\color{HubQuoteTextColor}\\ignorespaces") class-contents))
							(should (string-match-p (regexp-quote "\\begin{minipage}{\\dimexpr\\linewidth-64pt\\relax}") class-contents))
							(should (string-match-p (regexp-quote "rectangle ([xshift=10.2pt,yshift=-0.4pt]hubquote.south west)") class-contents))
							(should (string-match-p (regexp-quote "\\par\\vspace{18pt}{\\raggedleft\\normalfont\\small\\color{HubMuted}\\HubQuoteAttribution\\par}\\vspace{-10pt}") class-contents))
							(should-not (string-match-p (regexp-quote "\\HubDecorativeQuoteMark") class-contents))
							(should-not (string-match-p (regexp-quote "path picture bounding box.south east") class-contents))
							(should (string-match-p (regexp-quote "\\begin{pullquote}") tex-contents))
							(should (string-match-p (regexp-quote "\\begin{callout}[Design signal]") tex-contents))
							(should (string-match-p (regexp-quote "\\tcbuselibrary{raster,minted}") class-contents))
							(should (string-match-p (regexp-quote "\\definecolor{HubPatternMintVivid}{HTML}{9DF5EA}") class-contents))
							(should (string-match-p (regexp-quote "\\newcommand{\\HubCalloutFrame}") class-contents))
							(should (string-match-p (regexp-quote "draw=HubLine,line width=0.8pt,fill=HubSurface") class-contents))
							(should (string-match-p (regexp-quote "\\fill[HubCalloutEdgeColor]") class-contents))
							(should (string-match-p (regexp-quote "\\begin{minipage}{\\dimexpr\\linewidth-36pt\\relax}") class-contents))
							(should (string-match-p (regexp-quote "\\href{https://www.veriff.com/}{Veriff trust platform}") tex-contents))
							(should (string-match-p (regexp-quote "\\begin{itemize}") tex-contents))
							(should (string-match-p (regexp-quote "\\begin{enumerate}") tex-contents))
							(should (string-match-p (regexp-quote "\\HubCheckboxChecked{}") tex-contents))
							(should (string-match-p (regexp-quote "\\HubCheckboxUnchecked{}") tex-contents))
							(should (string-match-p (regexp-quote "\\HubFootnoteText{Native Org footnotes should read as supporting notes") tex-contents))
							(should (string-match-p (regexp-quote "\\colorlet{HubFootnoteMarkerColor}{HubAccent}") class-contents))
							(should (string-match-p (regexp-quote "\\renewcommand{\\@makefnmark}{\\hbox{\\@textsuperscript{\\normalfont\\textcolor{HubFootnoteMarkerColor}{\\@thefnmark}}}}") class-contents))
							(should (string-match-p (regexp-quote "\\definecolor{HubTeal}{HTML}{14E5C5}") class-contents))
							(should (string-match-p (regexp-quote "\\definecolor{HubLightTeal}{HTML}{9DF5EA}") class-contents))
							(should (string-match-p (regexp-quote "\\colorlet{HubLinkColor}{HubAccent}") class-contents))
							(should (string-match-p (regexp-quote "\\colorlet{HubUrlColor}{HubAccent}") class-contents))
							(should (string-match-p (regexp-quote "\\colorlet{HubEmphasisColor}{HubTeal}") class-contents))
							(should (string-match-p (regexp-quote "\\colorlet{HubCalloutEdgeColor}{HubPatternMintVivid}") class-contents))
							(should (string-match-p (regexp-quote "\\colorlet{HubCalloutEdgeColor}{HubPatternMintVivid}%
    \\colorlet{HubCalloutTitleColor}{HubCalloutEdgeColor}%") class-contents))
							(should (string-match-p (regexp-quote "\\colorlet{HubLinkColor}{HubTeal}") class-contents))
							(should (string-match-p (regexp-quote "\\AtBeginDocument{\\hypersetup{colorlinks=true,linkcolor=HubLinkColor,urlcolor=HubUrlColor,citecolor=HubLinkColor}}") class-contents))
							(should (string-match-p (regexp-quote "\\definecolor{HubLine}{HTML}{4B5F5A}") class-contents))
							(should (string-match-p (regexp-quote "\\definecolor{HubTableRule}{HTML}{4B5F5A}") class-contents))
							(should (string-match-p (regexp-quote "\\newcommand{\\HubListBullet}{{\\textcolor{HubListMarkerColor}{\\raisebox{-0.3mm}{\\scalebox{1.5}{\\textbullet}}}}}") class-contents))
							(should (string-match-p (regexp-quote "\\renewcommand{\\labelitemi}{\\HubListBullet}") class-contents))
							(should (string-match-p (regexp-quote "\\renewcommand*\\descriptionlabel[1]{\\normalfont\\HubListBullet") class-contents))
							(should (string-match-p (regexp-quote "\\renewcommand{\\labelenumi}{\\HubListNumber{\\arabic{enumi}}}") class-contents))
							(should (string-match-p (regexp-quote "\\AtBeginEnvironment{itemize}") class-contents))
							(should (string-match-p (regexp-quote "\\AtBeginEnvironment{enumerate}") class-contents))
							(should (string-match-p (regexp-quote "\\newcommand{\\HubCheckboxChecked}") class-contents))
							(should (string-match-p (regexp-quote "\\tikz[baseline=0.16em]") class-contents))
							(should (string-match-p (regexp-quote "\\newcommand{\\HubFootnoteText}") class-contents))
							(should (string-match-p (regexp-quote "\\begin{figure}") tex-contents))
							(should (string-match-p (regexp-quote "\\HubFigureImage{\\includegraphics[width=0.38\\linewidth]{") tex-contents))
							(should (string-match-p (regexp-quote "hero-logo.pdf}}") tex-contents))
							(should (string-match-p (regexp-quote "A compact brand marker used as a native Org figure.") tex-contents))
							(should (string-match-p (regexp-quote "\\caption{\\label{fig:org") tex-contents))
							(should (string-match-p (regexp-quote "\\RequirePackage{caption}") class-contents))
							(should (string-match-p (regexp-quote "\\colorlet{HubFigureCaptionTextColor}{HubStandfirst}") class-contents))
							(should (string-match-p (regexp-quote "\\colorlet{HubFigureCaptionTextColor}{HubMuted}") class-contents))
							(should (string-match-p (regexp-quote "\\DeclareCaptionFont{hubfigurecaption}{\\fontsize{8.5}{10.2}\\selectfont}") class-contents))
							(should (string-match-p (regexp-quote "\\captionsetup[figure]{font=hubfigurecaption,labelfont={color=HubAccent},textfont={it,color=HubFigureCaptionTextColor},labelsep=period,justification=centering,singlelinecheck=false,skip=4.5pt}") class-contents))
							(should (string-match-p (regexp-quote "\\newcommand{\\HubFigureImage}") class-contents))
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
							     (class-path (hub/test-exported-class-path artifact-root "veriff"))
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
							(should (string-match-p (regexp-quote "\\setlength{\\HubFooterLift}{-0.5mm}") class-contents))
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
							(should (string-match-p (regexp-quote "\\definecolor{HubPaper}{HTML}{FBEDE8}") class-contents))
							(should (string-match-p (regexp-quote "\\fill[HubPaper]") class-contents))
							(should-not (string-match-p (regexp-quote "\\pagecolor{white}") class-contents))
							(should (string-match-p (regexp-quote "\\newenvironment{hubhero}") class-contents))
							(should (string-match-p (regexp-quote "\\begin{tikzpicture}[overlay]") class-contents))
							(should (string-match-p (regexp-quote "hero-pattern.png") class-contents))
							(should (string-match-p (regexp-quote "hero-logo.pdf") class-contents))
							(should (string-match-p "Make the refresh impossible to miss" tex-contents))
							(should (string-match-p (regexp-quote "\\begin{callout}[Design signal]") tex-contents))
							(should (string-match-p (regexp-quote "\\begin{quote}[Prototype testimonial · Global platform operator]") tex-contents))
							(should (string-match-p (regexp-quote "\\fontsize{12.5}{16}\\selectfont\\bfseries\\color{HubQuoteTextColor}\\ignorespaces") class-contents))
							(should-not (string-match-p (regexp-quote "\\begin{pullquote}[Prototype testimonial · Global platform operator]") tex-contents))
							(should (string-match-p (regexp-quote "\\begin{description}") tex-contents))
							(should-not (string-match-p (regexp-quote "\\labelwidth\\z@") class-contents))
							(should (string-match-p (regexp-quote "\\labelwidth0pt") class-contents))
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

(ert-deftest hub/org-export-veriff-refresh-overdrive-mint-accents ()
  "The class defines brand palette colors, a custom page style, and uses them for code themes."
  (let* ((specimen (expand-file-name "test/fixtures/org-export/slice-en-veriff-refresh-overdrive.org"
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
							     (class-path (hub/test-exported-class-path artifact-root "veriff"))
							     (class-contents (hub/test-read-file-as-string class-path)))
							(should (string-match-p (regexp-quote "\\definecolor{HubMint}{HTML}{14E5C5}") class-contents))
							(should-not (string-match-p (regexp-quote (concat "\\definecolor{Hub" "Emerald}")) class-contents))
							(should (string-match-p (regexp-quote "\\def\\ps@hubpage") class-contents))
							(should (string-match-p (regexp-quote "\\HubDocumentTitle") class-contents))
							(should (string-match-p (regexp-quote "\\HubFooterLogoGraphic") class-contents))
							(should (string-match-p (regexp-quote "\\def\\HubCodeNameColor{HubStandfirst}") class-contents))
							(should (string-match-p (regexp-quote "\\def\\HubCodeStringColor{HubStringColor}") class-contents))
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
							     (class-path (hub/test-exported-class-path artifact-root "veriff"))
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
							(should-not (string-match-p (regexp-quote "colback=white") class-contents))
							(should (string-match-p (regexp-quote "colback=HubSurface") class-contents))
							(should (string-match-p (regexp-quote "\\immediate\\write\\@auxout{\\string\\expandafter\\string\\gdef\\string\\csname\\space hub@pillar@count@\\thehubpillarenv\\string\\endcsname{\\thehubpillaritem}}") class-contents)))))))
      (when (buffer-live-p specimen-buffer)
	(kill-buffer specimen-buffer))
      (when (file-directory-p artifact-root)
	(delete-directory artifact-root t)))))

(provide 'org-latex-pdf-export-test)
;;; org-latex-pdf-export-test.el ends here
