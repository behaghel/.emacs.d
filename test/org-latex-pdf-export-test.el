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

(defun hub/test-read-file-as-string (path)
  "Return PATH contents as a string."
  (with-temp-buffer
    (insert-file-contents path)
    (buffer-string)))

(defun hub/test-make-export-artifact-root ()
  "Create a temporary artifact root under `var/'."
  (make-temp-file (expand-file-name "var/org-latex-pdf-test-" hub/test-repo-root) t))

(defmacro hub/test-with-export-compiler-readiness (ready-p &rest body)
  "Run BODY with exporter readiness forced to READY-P."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'hub/org-export--compiler-ready-p)
	      (lambda (_compiler) ,ready-p)))
     ,@body))

(ert-deftest hub/org-export-slice-en-pro-refresh-overdrive-produces-latex-and-pdf ()
  "The first English flagship slice exports to LaTeX and PDF with the XeLaTeX path when ready."
  (let* ((specimen (expand-file-name "test/fixtures/org-export/slice-en-pro-refresh-overdrive.org"
				     hub/test-repo-root))
	 (artifact-root (hub/test-make-export-artifact-root))
	 (specimen-buffer nil))
    (unwind-protect
	(progn
	  (setq specimen-buffer (find-file-noselect specimen))
	  (with-current-buffer specimen-buffer
	    (hub/test-with-export-compiler-readiness t
						     (let* ((hub/org-export-output-root artifact-root)
							    (tex-path (hub/org-export-buffer-to-latex artifact-root))
							    (tex-contents (hub/test-read-file-as-string tex-path))
							    (pdf-path (hub/org-export-buffer-to-pdf artifact-root)))
						       (should (string-match-p (regexp-quote "% Intended LaTeX compiler: xelatex") tex-contents))
						       (should (string-match-p (regexp-quote "% hub-pro-refresh-overdrive") tex-contents))
						       (should (string-match-p (regexp-quote "\\setmainfont{Inter}") tex-contents))
						       (should (string-match-p (regexp-quote "\\setsansfont{Inter}") tex-contents))
						       (should (string-match-p "\\usepackage\[[^]]*english[^]]*\]{babel}" tex-contents))
						       (should (string-match-p "\\title{Refresh overdrive slice}" tex-contents))
						       (should (string-match-p "\\author{Hubert Behaghel}" tex-contents))
						       (should (string-match-p "\\date{" tex-contents))
						       (should (string-match-p "Minimal heading" tex-contents))
						       (should (string-match-p "Minimal slice paragraph" tex-contents))
						       (should (file-exists-p pdf-path))))))
      (when (buffer-live-p specimen-buffer)
	(kill-buffer specimen-buffer))
      (when (file-directory-p artifact-root)
	(delete-directory artifact-root t)))))

(ert-deftest hub/org-export-slice-en-pro-refresh-overdrive-falls-back-to-pdflatex ()
  "The exporter preserves a pdflatex fallback when font readiness is absent."
  (let* ((specimen (expand-file-name "test/fixtures/org-export/slice-en-pro-refresh-overdrive.org"
				     hub/test-repo-root))
	 (artifact-root (hub/test-make-export-artifact-root))
	 (specimen-buffer nil))
    (unwind-protect
	(progn
	  (setq specimen-buffer (find-file-noselect specimen))
	  (with-current-buffer specimen-buffer
	    (hub/test-with-export-compiler-readiness nil
						     (let* ((hub/org-export-output-root artifact-root)
							    (tex-path (hub/org-export-buffer-to-latex artifact-root))
							    (tex-contents (hub/test-read-file-as-string tex-path))
							    (pdf-path (hub/org-export-buffer-to-pdf artifact-root)))
						       (should (string-match-p (regexp-quote "% Intended LaTeX compiler: pdflatex") tex-contents))
						       (should-not (string-match-p (regexp-quote "\\setmainfont{Inter}") tex-contents))
						       (should-not (string-match-p (regexp-quote "\\setsansfont{Inter}") tex-contents))
						       (should (file-exists-p pdf-path))))))
      (when (buffer-live-p specimen-buffer)
	(kill-buffer specimen-buffer))
      (when (file-directory-p artifact-root)
	(delete-directory artifact-root t)))))

(ert-deftest hub/org-export-semantic-full-en-uses-class-owned-latex-and-pdf ()
  "The iteration-2 specimen exports with class-owned layout markers and PDF output on the XeLaTeX path."
  (let* ((specimen (expand-file-name "test/fixtures/org-export/semantic-full-en.org"
				     hub/test-repo-root))
	 (artifact-root (hub/test-make-export-artifact-root))
	 (specimen-buffer nil))
    (unwind-protect
	(progn
	  (setq specimen-buffer (find-file-noselect specimen))
	  (with-current-buffer specimen-buffer
	    (hub/test-with-export-compiler-readiness t
						     (let* ((hub/org-export-output-root artifact-root)
							    (tex-path (hub/org-export-buffer-to-latex artifact-root))
							    (tex-contents (hub/test-read-file-as-string tex-path))
							    (pdf-path (hub/org-export-buffer-to-pdf artifact-root)))
						       (should (string-match-p (regexp-quote "% Intended LaTeX compiler: xelatex") tex-contents))
						       (should-not (string-match-p "\\maketitle" tex-contents))
						       (should-not (string-match-p "\\tableofcontents" tex-contents))
						       (should (string-match-p (regexp-quote "% hub-pro-refresh-overdrive-title") tex-contents))
						       (should (string-match-p (regexp-quote "% hub-pro-refresh-overdrive-headings") tex-contents))
						       (should (string-match-p (regexp-quote "\\setmainfont{Inter}") tex-contents))
						       (should (string-match-p (regexp-quote "\\setsansfont{Inter}") tex-contents))
						       (should (string-match-p (regexp-quote "\\begin{hubhero}") tex-contents))
						       (should (string-match-p "Refresh mode" tex-contents))
						       (should (string-match-p "A temporary high-energy mode" tex-contents))
						       (should (string-match-p (regexp-quote "\\begin{standfirst}") tex-contents))
						       (should (string-match-p (regexp-quote "\\begin{quote}") tex-contents))
						       (should (string-match-p (regexp-quote "\\begin{pullquote}") tex-contents))
						       (should (string-match-p (regexp-quote "\\begin{callout}") tex-contents))
						       (should (string-match-p (regexp-quote "\\begin{tabular}{lll}") tex-contents))
						       (should (string-match-p (regexp-quote "\\begin{metrics}") tex-contents))
						       (should (string-match-p (regexp-quote "\\begin{metric}[230+]") tex-contents))
						       (should (string-match-p (regexp-quote "\\begin{pillars}") tex-contents))
						       (should (string-match-p (regexp-quote "\\begin{pillar}[Trust engineered]") tex-contents))
						       (should (string-match-p (regexp-quote "\\begin{hubcode}") tex-contents))
						       (should (string-match-p (regexp-quote "\\begin{hubfootnote}") tex-contents))
						       (should (string-match-p "Operating principle" tex-contents))
						       (should (file-exists-p pdf-path))))))
      (when (buffer-live-p specimen-buffer)
	(kill-buffer specimen-buffer))
      (when (file-directory-p artifact-root)
	(delete-directory artifact-root t)))))

(provide 'org-latex-pdf-export-test)
;;; org-latex-pdf-export-test.el ends here
