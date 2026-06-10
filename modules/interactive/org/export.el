;;; export.el --- Org mode: export and citation configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Export, citation, and LaTeX helper configuration for Org mode.

;;; Code:

(require 'hub-utils)

(defcustom hub/org-bibliography-file (expand-file-name "Dropbox/Documents/library.bib" (getenv "HOME"))
  "Default bibliography file for org-cite."
  :type 'file
  :group 'hub/org)

(defcustom hub/org-latex-class-directory
  (expand-file-name "etc/latex" user-emacs-directory)
  "Directory containing repo-local LaTeX class assets for Org templates."
  :type 'directory
  :group 'hub/org)

(defcustom hub/org-re-reveal-root (expand-file-name "Apps/reveal.js" (getenv "HOME"))
  "Root directory for reveal.js used by org-re-reveal."
  :type 'directory
  :group 'hub/org)

(defun hub/org-discover-local-latex-classes (&optional directory)
  "Return class names discovered from .cls files under DIRECTORY.
When DIRECTORY is nil, use `hub/org-latex-class-directory'."
  (let ((class-directory (or directory hub/org-latex-class-directory)))
    (when (file-directory-p class-directory)
      (sort
       (mapcar #'file-name-base
	       (directory-files class-directory t "\\.cls\\'" 'nosort))
       #'string<))))

(defun hub/org-registered-latex-classes ()
  "Return class names currently registered in `org-latex-classes'."
  (when (boundp 'org-latex-classes)
    (sort (mapcar #'car org-latex-classes) #'string<)))

(defun hub/org-latex-class-candidates ()
  "Return available Org LaTeX class names for template selection.
Repo-local .cls files are listed before classes already registered with Org."
  (delete-dups
   (append (hub/org-discover-local-latex-classes)
	   (hub/org-registered-latex-classes))))

(defun hub/org-read-latex-class ()
  "Prompt for an Org LaTeX class discovered from local and registered classes."
  (let* ((candidates (hub/org-latex-class-candidates))
	 (default (car candidates)))
    (unless candidates
      (user-error "No Org LaTeX classes are available"))
    (completing-read
     (if default
	 (format "LaTeX class (default %s): " default)
       "LaTeX class: ")
     candidates nil t nil nil default)))

(defun hub/org-setup-export ()
  "Configure Org export, citation, and reveal.js settings."
  (setq org-export-allow-bind-keywords t
	org-export-backends '(ascii html latex md odt)
	org-cite-global-bibliography (list hub/org-bibliography-file)
	org-cite-csl-styles-dir (expand-file-name "straight/repos/org/etc/csl" user-emacs-directory)
	org-cite-csl-locales-dir (expand-file-name "straight/repos/org/etc/csl" user-emacs-directory)
	org-re-reveal-root hub/org-re-reveal-root
	org-html-htmlize-output-type 'css
	org-html-head-include-default-style nil))

(provide 'org/export)
;;; export.el ends here
