;;; pdf.el --- Interactive PDF viewing defaults -*- lexical-binding: t; -*-

;;; Commentary:
;; Standard PDF viewing support for GUI/interactive Emacs.
;; DocView needs Ghostscript (`gs') to render PDF pages.  GUI-launched Emacs may
;; not inherit the devenv shell PATH, so we discover common locations and fall
;; back to a small wrapper that runs Ghostscript through this repo's devenv.

;;; Code:

(require 'seq)
(require 'subr-x)

(defgroup hub/pdf nil
  "Interactive PDF viewing defaults."
  :group 'hub)

(defcustom hub/pdf-ghostscript-executable nil
  "Optional absolute path to Ghostscript for DocView PDF rendering.
When nil, discover `gs' from `exec-path', common system paths, or a generated
wrapper around the project devenv shell."
  :type '(choice (const :tag "Discover automatically" nil) file)
  :group 'hub/pdf)

(defcustom hub/pdf-devenv-executable nil
  "Optional absolute path to the devenv executable used by PDF helpers.
When nil, discover devenv from `exec-path' and common Nix profile locations."
  :type '(choice (const :tag "Discover automatically" nil) file)
  :group 'hub/pdf)

(defun hub/pdf--candidate-devenv-executables ()
  "Return possible devenv executable paths for GUI-launched Emacs."
  (delete-dups
   (seq-filter
    #'identity
    (list hub/pdf-devenv-executable
	  (executable-find "devenv")
	  (expand-file-name "bin/devenv"
			    (expand-file-name (user-login-name)
					      "/etc/profiles/per-user"))))))

(defun hub/pdf--devenv-executable ()
  "Return a usable devenv executable path, or nil."
  (seq-find #'file-executable-p (hub/pdf--candidate-devenv-executables)))

(defun hub/pdf--candidate-ghostscript-executables ()
  "Return candidate Ghostscript executable paths."
  (delete-dups
   (seq-filter
    #'identity
    (list hub/pdf-ghostscript-executable
	  (executable-find "gs")
	  "/opt/homebrew/bin/gs"
	  "/usr/local/bin/gs"
	  "/run/current-system/sw/bin/gs"
	  (expand-file-name "bin/gs"
			    (expand-file-name (user-login-name)
					      "/etc/profiles/per-user"))))))

(defun hub/pdf--ghostscript-wrapper ()
  "Return a generated Ghostscript wrapper that runs through devenv."
  (when-let* ((devenv (hub/pdf--devenv-executable)))
    (let ((wrapper (expand-file-name "var/pdf/bin/gs" user-emacs-directory)))
      (make-directory (file-name-directory wrapper) t)
      (with-temp-file wrapper
	(insert "#!/bin/sh\n"
		"cd " (shell-quote-argument user-emacs-directory) " || exit 1\n"
		"exec " (shell-quote-argument devenv) " -q shell -- gs \"$@\"\n"))
      (set-file-modes wrapper #o755)
      wrapper)))

(defun hub/pdf--prepend-executable-directory (program)
  "Prepend PROGRAM's directory to `exec-path' and PATH."
  (let ((directory (file-name-directory (expand-file-name program))))
    (add-to-list 'exec-path directory)
    (setenv "PATH"
	    (string-join
	     (delete-dups
	      (cons (directory-file-name directory)
		    (split-string (or (getenv "PATH") "") path-separator t)))
	     path-separator))))

(defun hub/pdf-ensure-ghostscript-on-path ()
  "Ensure `executable-find' can resolve `gs' for DocView.
DocView checks converter availability with `executable-find'.  Keeping a `gs'
wrapper on `exec-path' makes direct `find-file' of PDFs work, even before any
nómina-specific helper runs."
  (unless (executable-find "gs")
    (when-let* ((wrapper (hub/pdf--ghostscript-wrapper)))
      (hub/pdf--prepend-executable-directory wrapper))))

(defun hub/pdf-ghostscript-program ()
  "Return a Ghostscript executable suitable for DocView."
  (or (seq-find #'file-executable-p (hub/pdf--candidate-ghostscript-executables))
      (hub/pdf--ghostscript-wrapper)
      (user-error "DocView needs Ghostscript (`gs') to render PDFs")))

(defun hub/pdf-configure-doc-view ()
  "Configure DocView to find Ghostscript in GUI-launched Emacs."
  (hub/pdf-ensure-ghostscript-on-path)
  (setq doc-view-ghostscript-program (hub/pdf-ghostscript-program)))

(defun hub/pdf-view-file (file &optional other-window)
  "Open FILE as a rendered PDF using DocView.
When OTHER-WINDOW is non-nil, display the PDF in another window."
  (interactive "fPDF file: ")
  (require 'doc-view)
  (hub/pdf-configure-doc-view)
  (let ((buffer (if other-window
		    (find-file-other-window file)
		  (find-file file))))
    (with-current-buffer buffer
      (unless (derived-mode-p 'doc-view-mode)
	(doc-view-mode)))
    buffer))

(hub/pdf-ensure-ghostscript-on-path)

(with-eval-after-load 'doc-view
  (hub/pdf-configure-doc-view))

(provide 'tools/pdf)
;;; pdf.el ends here
