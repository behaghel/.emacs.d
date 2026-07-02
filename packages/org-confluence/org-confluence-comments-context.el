;;; org-confluence-comments-context.el --- Confluence comment source context -*- lexical-binding: t; -*-

;;; Commentary:
;; Helpers for resolving source Org files and Confluence page metadata from
;; `.comments.org' sidecar buffers.

;;; Code:

(require 'subr-x)

(require 'org-confluence-api)

(defun org-confluence-comments-page-id-or-read (&optional page-id)
  "Return PAGE-ID, current buffer page ID, or prompt for one."
  (or page-id
      (org-confluence-api--page-id-from-buffer)
      (read-string "Confluence page ID: ")))

(defun org-confluence-comments-sidecar-source-file ()
  "Return source Org file for the current comments sidecar buffer."
  (unless (and buffer-file-name
	       (string-suffix-p ".comments.org" buffer-file-name))
    (user-error "Current buffer is not a comments sidecar"))
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search t))
      (unless (re-search-forward "^[\t ]*#\\+source:[\t ]*\\(.*?\\)[\t ]*$" nil t)
	(user-error "Comments sidecar has no #+source keyword"))
      (expand-file-name (string-trim (match-string-no-properties 1))
			(file-name-directory buffer-file-name)))))

(defun org-confluence-comments-page-id-from-sidecar-source (&optional page-id)
  "Return PAGE-ID or read Confluence page ID from current sidecar source file."
  (or page-id
      (let ((source-file (org-confluence-comments-sidecar-source-file)))
	(unless (file-readable-p source-file)
	  (user-error "Cannot read source Org file: %s" source-file))
	(with-current-buffer (find-file-noselect source-file)
	  (or (org-confluence-api--page-id-from-buffer)
	      (read-string "Confluence page ID: "))))))

(defun org-confluence-comments-source-page-metadata-from-sidecar ()
  "Return plist with Confluence page metadata from current sidecar source file."
  (let ((source-file (org-confluence-comments-sidecar-source-file)))
    (unless (file-readable-p source-file)
      (user-error "Cannot read source Org file: %s" source-file))
    (with-current-buffer (find-file-noselect source-file)
      (list :page-id (or (org-confluence-api--page-id-from-buffer)
			 (read-string "Confluence page ID: "))
	    :space (org-confluence-api--keyword-from-buffer "CONFLUENCE_SPACE")))))


(provide 'org-confluence-comments-context)
;;; org-confluence-comments-context.el ends here
