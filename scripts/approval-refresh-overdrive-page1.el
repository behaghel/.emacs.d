;;; approval-refresh-overdrive-page1.el --- Export Veriff approval PDF with log -*- lexical-binding: t; -*-

;;; Commentary:
;; Batch helper to regenerate the Veriff refresh-overdrive approval PDF and print the
;; upstream `*Org PDF LaTeX Output*' buffer to stdout.

;;; Code:

(require 'subr-x)

(defconst hub/script-approval-refresh-overdrive-page1--source-file
  (or load-file-name buffer-file-name)
  "Source file path captured when this helper is loaded.")

(defconst hub/script-org-pdf-output-buffer-name "*Org PDF LaTeX Output*"
  "Org LaTeX compilation output buffer name.")

(defun hub/script--repo-root (&optional this-file)
  "Return the repository root derived from THIS-FILE.
When THIS-FILE is nil, use the current script file."
  (file-name-directory
   (directory-file-name
    (file-name-directory (or this-file
			     hub/script-approval-refresh-overdrive-page1--source-file
			     load-file-name
			     buffer-file-name)))))

(defun hub/script--setup-load-paths (repo-root)
  "Load the Org export stack from REPO-ROOT."
  (dolist (path '("core" "lisp" "modules" "modules/lang" "modules/org" "modules/interactive"))
    (add-to-list 'load-path (expand-file-name path repo-root)))
  (require 'core-packages)
  (require 'editing/evil)
  (require 'org/core)
  (require 'org/export-latex))

(defun hub/script--print-latex-output-buffer ()
  "Print the `*Org PDF LaTeX Output*' buffer to standard output when present."
  (when-let* ((buffer (get-buffer hub/script-org-pdf-output-buffer-name)))
    (with-current-buffer buffer
      (let ((contents (buffer-string)))
	(princ "[Org PDF LaTeX Output] BEGIN\n")
	(princ contents)
	(unless (or (string-empty-p contents)
		    (eq (aref contents (1- (length contents))) ?\n))
	  (princ "\n"))
	(princ "[Org PDF LaTeX Output] END\n")))))

(defun hub/script-export-approval-refresh-overdrive-page1 (&optional repo-root)
  "Export the Veriff refresh-overdrive page-one PDF from REPO-ROOT.
Print the LaTeX output buffer contents and the generated PDF path to stdout."
  (let* ((repo-root (file-name-as-directory
		     (or repo-root
			 (hub/script--repo-root hub/script-approval-refresh-overdrive-page1--source-file))))
	 (user-emacs-directory repo-root)
	 (default-directory repo-root)
	 (specimen (expand-file-name
		    "test/fixtures/org-export/approval-refresh-overdrive-page1.org"
		    repo-root))
	 (artifact-root (expand-file-name
			 "var/org-latex-pdf/veriff-refresh-overdrive-page1"
			 repo-root))
	 (specimen-buffer nil)
	 pdf-path)
    (hub/script--setup-load-paths repo-root)
    (unwind-protect
	(condition-case err
	    (progn
	      (setq specimen-buffer (find-file-noselect specimen))
	      (with-current-buffer specimen-buffer
		(setq pdf-path (hub/org-export-buffer-to-pdf artifact-root)))
	      (hub/script--print-latex-output-buffer)
	      (princ (format "%s\n" pdf-path))
	      pdf-path)
	  (error
	   (hub/script--print-latex-output-buffer)
	   (signal (car err) (cdr err))))
      (when (buffer-live-p specimen-buffer)
	(kill-buffer specimen-buffer)))))

(provide 'approval-refresh-overdrive-page1)
;;; approval-refresh-overdrive-page1.el ends here
