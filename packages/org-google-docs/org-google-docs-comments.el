;;; org-google-docs-comments.el --- Google Docs comments facade -*- lexical-binding: t; -*-

;;; Commentary:
;; Read-only comment operations for Org buffers linked to Google Docs through
;; upstream gdocs.  This layer stays small and delegates network access to
;; upstream gdocs API helpers.

;;; Code:

(require 'org)
(require 'subr-x)

(require 'org-google-docs-comments-remote)

(defun org-google-docs-comments--require-upstream-library (library)
  "Require upstream gdocs LIBRARY or signal an actionable error."
  (unless (require library nil 'noerror)
    (user-error "Missing upstream gdocs library `%s'; install and configure benthamite/gdocs" library)))

(defun org-google-docs-comments--file-property (property)
  "Return top-level Org PROPERTY from the file property drawer, or nil."
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search t)
	  (heading-start (save-excursion
			   (or (re-search-forward org-heading-regexp nil t)
			       (point-max)))))
      (when (re-search-forward "^[	]*:PROPERTIES:[		]*$" heading-start t)
	(let ((drawer-end (save-excursion
			    (or (re-search-forward "^[	]*:END:[	]*$" heading-start t)
				heading-start))))
	  (when (re-search-forward
		 (format "^[	]*:%s:[		]*\\(.*?\\)[	]*$"
			 (regexp-quote property))
		 drawer-end t)
	    (string-trim (match-string-no-properties 1))))))))

(defun org-google-docs-comments-document-id ()
  "Return the current Org buffer's upstream gdocs document id.
The id is read from buffer-local upstream state or the file-level
`GDOCS_DOCUMENT_ID' property drawer written by upstream gdocs."
  (or (and (boundp 'gdocs-sync--document-id)
	   (symbol-value 'gdocs-sync--document-id))
      (org-google-docs-comments--file-property "GDOCS_DOCUMENT_ID")))

(defun org-google-docs-comments-account ()
  "Return the current Org buffer's upstream gdocs account, or nil."
  (or (and (boundp 'gdocs-sync--account)
	   (symbol-value 'gdocs-sync--account))
      (org-google-docs-comments--file-property "GDOCS_ACCOUNT")))

(defun org-google-docs-comments--require-document-id ()
  "Return current linked Google Docs document id or signal a user error."
  (or (org-google-docs-comments-document-id)
      (user-error "Current Org buffer is not linked to a Google Doc")))

;;;###autoload
(defun org-google-docs-comments-list (callback)
  "Fetch Google Docs comments for the current buffer and call CALLBACK.
CALLBACK receives a list of normalized comment plists."
  (org-google-docs-comments--require-upstream-library 'gdocs-api)
  (let ((document-id (org-google-docs-comments--require-document-id))
	(account (org-google-docs-comments-account)))
    (gdocs-api-list-comments
     document-id
     (lambda (comments)
       (funcall callback
		(mapcar #'org-google-docs-comments-remote-normalize comments)))
     account)))

(provide 'org-google-docs-comments)
;;; org-google-docs-comments.el ends here
