;;; org-comments-page.el --- Page comments for Org comments -*- lexical-binding: t; -*-

;;; Commentary:
;; Page-level comment helpers for Org comments.

;;; Code:

(require 'org-comments-core)
(require 'org-comments-model)
(require 'org-comments-overlays)
(require 'org-comments-page-panel)
(require 'org-comments-sidecar)
(require 'org-comments-ui)
(require 'subr-x)

(declare-function org-comments-goto-sidecar-body "org-comments-commands")

(defun org-comments-page--create-record (source-file &optional body id author created-at)
  "Return a page-level sidecar comment record for SOURCE-FILE.
BODY defaults to an empty string.  ID, AUTHOR, and CREATED-AT default to local
comment metadata."
  (list :id (or id (org-comments-generate-id))
	:status "OPEN"
	:source-file source-file
	:author (or author (org-comments-current-author))
	:created-at (or created-at (org-comments-current-created-at))
	:sync-kind "footer"
	:body (or body "")))

(defun org-comments-page--format-entry (record sidecar-file)
  "Return sidecar Org entry text for page comment RECORD in SIDECAR-FILE."
  (let ((title (org-comments-heading-title record (file-name-directory sidecar-file))))
    (concat
     (format "* %s %s\n" (or (plist-get record :status) "OPEN") title)
     ":PROPERTIES:\n"
     (org-comments--property-line "ORG_COMMENTS_ID" (plist-get record :id))
     (org-comments--property-line "ORG_COMMENTS_AUTHOR" (plist-get record :author))
     (org-comments--property-line "ORG_COMMENTS_CREATED_AT" (plist-get record :created-at))
     (org-comments--property-line "ORG_COMMENTS_SYNC_KIND" "footer")
     ":END:\n\n"
     (string-trim-right (or (plist-get record :body) ""))
     "\n")))

(defun org-comments-page--append-to-sidecar (record &optional sidecar-file)
  "Append page comment RECORD to SIDECAR-FILE and return SIDECAR-FILE."
  (let* ((source-file (plist-get record :source-file))
	 (target-file (or sidecar-file (org-comments-sidecar-path source-file))))
    (org-comments--ensure-sidecar-header target-file source-file)
    (with-temp-buffer
      (insert-file-contents target-file)
      (goto-char (point-max))
      (unless (bolp) (insert "\n"))
      (unless (save-excursion
		(forward-line -1)
		(looking-at-p "[[:space:]]*$"))
	(insert "\n"))
      (insert (org-comments-page--format-entry record target-file))
      (write-region (point-min) (point-max) target-file nil 'silent))
    target-file))

;;;###autoload
(defun org-comments-page-create (&optional body)
  "Create a page/footer sidecar comment with BODY and edit it."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Page comments only work in Org buffers"))
  (unless buffer-file-name
    (user-error "Current buffer is not visiting a file"))
  (let* ((record (org-comments-page--create-record buffer-file-name body))
	 (sidecar-file (org-comments-page--append-to-sidecar record)))
    (when (and (bound-and-true-p org-comments-mode)
	       (derived-mode-p 'org-mode))
      (org-comments-overlays-refresh))
    (org-comments-ui-refresh)
    (org-comments-goto-sidecar-body
     (list :id (plist-get record :id)
	   :sidecar-file sidecar-file))))

;;;###autoload
(defun org-comments-page-open ()
  "Open the page-context window for the current Org buffer."
  (interactive)
  (org-comments-ui-page-open #'org-comments-page-panel-open))

(provide 'org-comments-page)
;;; org-comments-page.el ends here
