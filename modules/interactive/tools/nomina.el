;;; nomina.el --- Build local Spanish payslips -*- lexical-binding: t; -*-

;;; Commentary:
;; Helpers for `.tex' files using the repo-local `nomina-es' LaTeX class.

;;; Code:

(require 'hub-utils)

(require 'hub-keys)
(require 'seq)
(require 'subr-x)
(require 'tools/pdf)

(defgroup hub/nomina nil
  "Build Spanish payslip LaTeX files."
  :group 'hub)

(defcustom hub/nomina-latex-class-directory
  (expand-file-name "etc/latex" user-emacs-directory)
  "Directory containing the repo-local `nomina-es' LaTeX class."
  :type 'directory
  :group 'hub/nomina)

(defcustom hub/nomina-devenv-executable nil
  "Optional absolute path to the devenv executable used for payslip builds.
When nil, discover devenv from `exec-path'."
  :type '(choice (const :tag "Discover automatically" nil) file)
  :group 'hub/nomina)

(defcustom hub/nomina-pdf-viewer 'doc-view
  "Viewer used by `hub/nomina-open-pdf'.
Use `doc-view' to render the PDF inside Emacs, or `external' to open it with
the operating system default application."
  :type '(choice (const :tag "Emacs DocView" doc-view)
		 (const :tag "External application" external))
  :group 'hub/nomina)


(defun hub/nomina--candidate-devenv-executables ()
  "Return possible devenv executable paths for GUI-launched Emacs."
  (delete-dups
   (seq-filter
    #'identity
    (list hub/nomina-devenv-executable
	  (executable-find "devenv")
	  (expand-file-name "bin/devenv"
			    (expand-file-name (user-login-name)
					      "/etc/profiles/per-user"))))))

(defun hub/nomina--devenv-executable ()
  "Return a usable devenv executable path, or nil."
  (seq-find #'file-executable-p (hub/nomina--candidate-devenv-executables)))

(defun hub/nomina--texinputs ()
  "Return TEXINPUTS exposing the repo-local LaTeX class directory."
  (concat (file-name-as-directory (expand-file-name hub/nomina-latex-class-directory))
	  "//"
	  path-separator
	  (or (getenv "TEXINPUTS") "")))


(defun hub/nomina--buffer-p ()
  "Return non-nil when the current buffer appears to be a nomina `.tex' file."
  (or (and buffer-file-name
	   (string-match-p "nomina" (downcase (file-name-nondirectory buffer-file-name))))
      (save-excursion
	(save-restriction
	  (widen)
	  (goto-char (point-min))
	  (re-search-forward "\\\\documentclass\(?:\[[^]]*\]\)?{nomina-es}" 2000 t)))))

(defun hub/nomina--compile-command ()
  "Return the shell command used to build the current payslip PDF."
  (unless buffer-file-name
    (user-error "Save this payslip before building it"))
  (let* ((source-path (expand-file-name buffer-file-name))
	 (source-dir (file-name-directory source-path))
	 (devenv (and (not (getenv "IN_NIX_SHELL"))
		      (file-exists-p (expand-file-name "devenv.nix" user-emacs-directory))
		      (hub/nomina--devenv-executable)))
	 (texinputs (format "TEXINPUTS=%s" (shell-quote-argument (hub/nomina--texinputs))))
	 (xelatex (string-join
		   (list "xelatex"
			 "-interaction=nonstopmode"
			 "-halt-on-error"
			 "-file-line-error"
			 "-output-directory" (shell-quote-argument source-dir)
			 (shell-quote-argument source-path))
		   " ")))
    (if devenv
	(string-join
	 (list "cd" (shell-quote-argument user-emacs-directory)
	       "&&" (shell-quote-argument devenv) "-q" "shell" "--"
	       "env" texinputs xelatex)
	 " ")
      (string-join
       (list "cd" (shell-quote-argument source-dir)
	     "&&" "env" texinputs xelatex)
       " "))))

(defun hub/nomina-compile (&optional edit-command)
  "Build the current `nomina-es' LaTeX file with native `compile'.
With prefix argument EDIT-COMMAND, offer the generated command for editing."
  (interactive "P")
  (unless (hub/nomina--buffer-p)
    (user-error "This does not look like a nomina-es LaTeX file"))
  (save-buffer)
  (let ((compile-command (hub/nomina--compile-command)))
    (if edit-command
	(call-interactively #'compile)
      (compile compile-command))))

(defun hub/nomina--open-pdf-externally (pdf)
  "Open PDF with the operating system default application."
  (cond
   ((eq system-type 'darwin)
    (start-process "nomina-open-pdf" nil "open" pdf))
   ((executable-find "xdg-open")
    (start-process "nomina-open-pdf" nil "xdg-open" pdf))
   (t (browse-url-of-file pdf))))

(defun hub/nomina-open-pdf (&optional external)
  "Open the PDF corresponding to the current payslip source file.
With prefix argument EXTERNAL, open with the operating system default viewer."
  (interactive "P")
  (unless buffer-file-name
    (user-error "Save this payslip before opening its PDF"))
  (let ((pdf (concat (file-name-sans-extension buffer-file-name) ".pdf")))
    (unless (file-exists-p pdf)
      (user-error "No PDF found at %s; build it first" pdf))
    (if (or external (eq hub/nomina-pdf-viewer 'external))
	(hub/nomina--open-pdf-externally pdf)
      (hub/pdf-view-file pdf 'other-window))))

(defun hub/nomina-setup-buffer ()
  "Install payslip build defaults in the current buffer when applicable."
  (when (hub/nomina--buffer-p)
    (setq-local compile-command (hub/nomina--compile-command))
    (local-set-key (kbd "C-c C-b") #'hub/nomina-compile)
    (local-set-key (kbd "C-c C-o") #'hub/nomina-open-pdf)
    (when (fboundp 'evil-define-key)
      (evil-define-key 'normal (current-local-map)
		       (kbd "; b") #'hub/nomina-compile
		       (kbd "; o") #'hub/nomina-open-pdf))))

(add-hook 'latex-mode-hook #'hub/nomina-setup-buffer)
(add-hook 'LaTeX-mode-hook #'hub/nomina-setup-buffer)

(provide 'tools/nomina)
;;; nomina.el ends here
