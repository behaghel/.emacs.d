;;; org-comments-migrate.el --- Sidecar migration helpers for Org comments -*- lexical-binding: t; -*-

;;; Commentary:
;; Explicit migration helpers for legacy sidecar metadata.  This module is kept
;; isolated so support for older persisted property names can be removed later
;; without spreading compatibility through normal read/write paths.

;;; Code:

(require 'org-comments-sidecar)
(require 'subr-x)

(defconst org-comments-migrate-legacy-property-prefix "HUB_COMMENT_"
  "Legacy sidecar property prefix migrated by `org-comments-migrate-sidecar'.")

(defconst org-comments-migrate-property-prefix "ORG_COMMENTS_"
  "Package-native sidecar property prefix used after migration.")

(defun org-comments-migrate--legacy-property-regexp ()
  "Return regexp matching legacy sidecar property names."
  (format "^\\([[:space:]]*:\\)%s\\([[:alnum:]_]+:\\)"
	  (regexp-quote org-comments-migrate-legacy-property-prefix)))

(defun org-comments-legacy-sidecar-p (&optional sidecar-file)
  "Return non-nil when SIDECAR-FILE contains legacy comment properties.
When SIDECAR-FILE is nil, inspect the current buffer."
  (if sidecar-file
      (when (file-exists-p sidecar-file)
	(with-temp-buffer
	  (insert-file-contents sidecar-file)
	  (org-comments-legacy-sidecar-p)))
    (save-excursion
      (goto-char (point-min))
      (re-search-forward (org-comments-migrate--legacy-property-regexp) nil t))))

(defun org-comments-migrate--buffer ()
  "Migrate legacy properties in the current buffer and return replacement count."
  (let ((count 0))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward (org-comments-migrate--legacy-property-regexp) nil t)
	(replace-match
	 (concat (match-string 1)
		 org-comments-migrate-property-prefix
		 (match-string 2))
	 t t)
	(setq count (1+ count))))
    count))

;;;###autoload
(defun org-comments-migrate-sidecar (&optional sidecar-file)
  "Explicitly migrate SIDECAR-FILE from legacy to package-native properties.
When called interactively, use the current sidecar file or the current Org
source buffer's sidecar.  Return the number of rewritten property names."
  (interactive)
  (let* ((file (or sidecar-file
		   (if (and buffer-file-name
			    (string-suffix-p ".comments.org" buffer-file-name))
		       buffer-file-name
		     (and buffer-file-name
			  (org-comments-sidecar-path buffer-file-name))))))
    (unless file
      (user-error "No sidecar file to migrate"))
    (unless (file-exists-p file)
      (user-error "Sidecar file does not exist: %s" file))
    (let ((count 0))
      (with-temp-buffer
	(insert-file-contents file)
	(setq count (org-comments-migrate--buffer))
	(when (> count 0)
	  (write-region (point-min) (point-max) file nil 'silent)))
      (when (called-interactively-p 'interactive)
	(message "Migrated %s comment properties in %s" count file))
      count)))

;;;###autoload
(defun org-comments-migrate-directory-sidecars (directory)
  "Explicitly migrate all `.comments.org' sidecars under DIRECTORY.
Return an alist of (FILE . COUNT) for files that were inspected."
  (interactive "DDirectory: ")
  (let ((results nil))
    (dolist (file (directory-files-recursively directory "\\.comments\\.org\\'"))
      (push (cons file (org-comments-migrate-sidecar file)) results))
    (setq results (nreverse results))
    (when (called-interactively-p 'interactive)
      (message "Migrated %s sidecar files (%s properties)"
	       (length results)
	       (apply #'+ (mapcar #'cdr results))))
    results))

(provide 'org-comments-migrate)
;;; org-comments-migrate.el ends here
