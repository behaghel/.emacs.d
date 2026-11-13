;;; ox-sync-store.el --- Org sync tracking sidecars -*- lexical-binding: t; -*-

;;; Commentary:
;; Provider-neutral read/write helpers for Org sync remote-tracking sidecars.

;;; Code:

(require 'subr-x)

(defconst ox-sync-store--begin-marker "#+BEGIN_SRC emacs-lisp"
  "Begin marker for machine-readable sync tracking data.")

(defconst ox-sync-store--end-marker "#+END_SRC"
  "End marker for machine-readable sync tracking data.")

(defun ox-sync-store-path (&optional source-file)
  "Return sync sidecar path for SOURCE-FILE."
  (let ((file (or source-file buffer-file-name)))
    (unless file
      (user-error "Source buffer is not visiting a file"))
    (concat (file-name-sans-extension file) ".sync.org")))

(defun ox-sync-store--extract-data-string ()
  "Return sync data source block contents from the current buffer."
  (goto-char (point-min))
  (unless (search-forward ox-sync-store--begin-marker nil t)
    (user-error "Org sync sidecar has no tracking data block"))
  (forward-line 1)
  (let ((start (point)))
    (unless (search-forward ox-sync-store--end-marker nil t)
      (user-error "Org sync sidecar has unterminated tracking data block"))
    (string-trim (buffer-substring-no-properties
		  start (line-beginning-position)))))

(defun ox-sync-store-read (&optional source-file)
  "Read tracking data for SOURCE-FILE, or nil when absent."
  (let ((sidecar (ox-sync-store-path source-file)))
    (when (file-exists-p sidecar)
      (with-temp-buffer
	(insert-file-contents sidecar)
	(let ((data (ox-sync-store--extract-data-string)))
	  (when (not (string-empty-p data))
	    (read data)))))))

(defun ox-sync-store-write (source-file tracking)
  "Write TRACKING data to SOURCE-FILE's sync sidecar and return its path."
  (let ((sidecar (ox-sync-store-path source-file)))
    (make-directory (file-name-directory sidecar) t)
    (with-temp-file sidecar
      (insert "#+TITLE: Org Sync Tracking\n")
      (insert "#+PROPERTY: org_sync_sidecar_version 1\n\n")
      (insert "* Tracking\n")
      (insert ox-sync-store--begin-marker "\n")
      (prin1 tracking (current-buffer))
      (insert "\n" ox-sync-store--end-marker "\n"))
    sidecar))

(defun ox-sync-store-archive (source-file)
  "Archive SOURCE-FILE's sync sidecar and return backup path, or nil."
  (let ((sidecar (ox-sync-store-path source-file)))
    (when (file-exists-p sidecar)
      (let ((backup (format "%s.%s.bak"
			    sidecar
			    (format-time-string "%Y%m%d%H%M%S"))))
	(copy-file sidecar backup t)
	backup))))

(provide 'ox-sync-store)
;;; ox-sync-store.el ends here
