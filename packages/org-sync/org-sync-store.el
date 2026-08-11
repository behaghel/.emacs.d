;;; org-sync-store.el --- Org sync tracking sidecars -*- lexical-binding: t; -*-

;;; Commentary:
;; Provider-neutral read/write helpers for Org sync remote-tracking sidecars.

;;; Code:

(require 'subr-x)

(defconst org-sync-store--begin-marker "#+BEGIN_SRC emacs-lisp"
  "Begin marker for machine-readable sync tracking data.")

(defconst org-sync-store--end-marker "#+END_SRC"
  "End marker for machine-readable sync tracking data.")

(defun org-sync-store-path (&optional source-file)
  "Return sync sidecar path for SOURCE-FILE."
  (let ((file (or source-file buffer-file-name)))
    (unless file
      (user-error "Source buffer is not visiting a file"))
    (concat (file-name-sans-extension file) ".sync.org")))

(defun org-sync-store--extract-data-string ()
  "Return sync data source block contents from the current buffer."
  (goto-char (point-min))
  (unless (search-forward org-sync-store--begin-marker nil t)
    (user-error "Org sync sidecar has no tracking data block"))
  (forward-line 1)
  (let ((start (point)))
    (unless (search-forward org-sync-store--end-marker nil t)
      (user-error "Org sync sidecar has unterminated tracking data block"))
    (string-trim (buffer-substring-no-properties
		  start (line-beginning-position)))))

(defun org-sync-store-read (&optional source-file)
  "Read tracking data for SOURCE-FILE, or nil when absent."
  (let ((sidecar (org-sync-store-path source-file)))
    (when (file-exists-p sidecar)
      (with-temp-buffer
	(insert-file-contents sidecar)
	(let ((data (org-sync-store--extract-data-string)))
	  (when (not (string-empty-p data))
	    (read data)))))))

(defun org-sync-store-write (source-file tracking)
  "Write TRACKING data to SOURCE-FILE's sync sidecar and return its path."
  (let ((sidecar (org-sync-store-path source-file)))
    (make-directory (file-name-directory sidecar) t)
    (with-temp-file sidecar
      (insert "#+TITLE: Org Sync Tracking\n")
      (insert "#+PROPERTY: org_sync_sidecar_version 1\n\n")
      (insert "* Tracking\n")
      (insert org-sync-store--begin-marker "\n")
      (prin1 tracking (current-buffer))
      (insert "\n" org-sync-store--end-marker "\n"))
    sidecar))

(provide 'org-sync-store)
;;; org-sync-store.el ends here
