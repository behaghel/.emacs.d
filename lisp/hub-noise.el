;;; hub-noise.el --- Workspace noise filtering helpers -*- lexical-binding: t; -*-

;;; Commentary:
;; Central predicates for buffers and files that are useful implementation
;; details but should not appear in normal writing/navigation discovery UI.

;;; Code:

(require 'seq)
(require 'subr-x)

(defvar completion-ignored-buffer-names)

(defgroup hub/noise nil
  "Workspace noise filtering helpers."
  :group 'convenience)

(defconst hub/noise-sidecar-suffixes
  '(".comments.org" ".copilot.org" ".suggestions.org"
    ".comments.org_archive" ".copilot.org_archive" ".suggestions.org_archive")
  "Source-adjacent sidecar suffixes hidden from discovery surfaces.")

(defvar hub/noise-file-functions nil
  "Predicate hooks for files hidden from discovery surfaces.
Each function receives an absolute file name and should return non-nil when the
file is generated or auxiliary rather than user-authored source.")

(defconst hub/noise-auxiliary-buffer-regexps
  '("\\`\\*Org Copilot\\*\\'"
    "\\`\\*Org Copilot Chat\\*\\'"
    "\\`\\*Org Copilot Diff\\*\\'"
    "\\`\\*Org Copilot Suggestion\\*\\'"
    "\\`\\*Org Context Panel\\*\\'"
    "\\`\\*Org Context View\\*\\'")
  "Auxiliary buffer regexps hidden from normal buffer switching.")

(defun hub/noise-sidecar-file-p (path)
  "Return non-nil when PATH is an Org implementation sidecar."
  (and (stringp path)
       (let ((file (file-name-nondirectory (expand-file-name path))))
	 (seq-some (lambda (suffix) (string-suffix-p suffix file))
		   hub/noise-sidecar-suffixes))))

(defun hub/noise-generated-backup-file-p (path)
  "Return non-nil when PATH is a generated backup artifact."
  (and (stringp path)
       (string-match-p
	(rx "." (or "gdocs-pull-backup") "." (+ nonl) string-end)
	(file-name-nondirectory path))))

(defun hub/noise-file-p (path)
  "Return non-nil when PATH should be hidden from discovery surfaces."
  (and (stringp path)
       (let ((expanded (expand-file-name path)))
	 (or (hub/noise-sidecar-file-p expanded)
	     (hub/noise-generated-backup-file-p expanded)
	     (run-hook-with-args-until-success
	      'hub/noise-file-functions expanded)))))

(defun hub/noise-auxiliary-buffer-name-p (name)
  "Return non-nil when buffer NAME is an auxiliary implementation buffer."
  (and (stringp name)
       (seq-some (lambda (regexp) (string-match-p regexp name))
		 hub/noise-auxiliary-buffer-regexps)))

(defun hub/noise-buffer-p (buffer)
  "Return non-nil when BUFFER should be hidden from normal switching."
  (let ((buffer (if (bufferp buffer) buffer (get-buffer buffer))))
    (and (buffer-live-p buffer)
	 (or (hub/noise-auxiliary-buffer-name-p (buffer-name buffer))
	     (with-current-buffer buffer
	       (and buffer-file-name
		    (hub/noise-file-p buffer-file-name)))))))

(defun hub/noise-install-buffer-filters ()
  "Install standard buffer and file discovery filters."
  (unless (boundp 'completion-ignored-buffer-names)
    (setq completion-ignored-buffer-names nil))
  (dolist (regexp hub/noise-auxiliary-buffer-regexps)
    (add-to-list 'completion-ignored-buffer-names regexp)
    (when (boundp 'consult-buffer-filter)
      (add-to-list 'consult-buffer-filter regexp)))
  (when (boundp 'recentf-exclude)
    (dolist (suffix hub/noise-sidecar-suffixes)
      (add-to-list 'recentf-exclude (concat (regexp-quote suffix) "\\'")))
    (add-to-list 'recentf-exclude
		 (rx "." (or "gdocs-pull-backup") "." (+ nonl) string-end))))

(provide 'hub-noise)
;;; hub-noise.el ends here
