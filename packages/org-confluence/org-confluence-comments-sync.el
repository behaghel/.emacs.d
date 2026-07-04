;;; org-confluence-comments-sync.el --- Confluence comment synchronization -*- lexical-binding: t; -*-

;;; Commentary:
;; Comments-only Confluence synchronization for the org-comments backend.
;; Full page synchronization remains in `org-confluence-sync.el'.

;;; Code:

(require 'subr-x)

(require 'org-comments-core)
(require 'org-confluence-comments-context)

(autoload 'org-confluence-comments-import "org-confluence-comments-import" nil nil)
(declare-function org-confluence-sync--push-pending-comments "org-confluence-sync" (source-buffer &optional page-id))

(defun org-confluence-comments--sync (&optional page-id body-format)
  "Synchronize current Org buffer comments with Confluence.
PAGE-ID overrides the current buffer's `CONFLUENCE_PAGE_ID'.  BODY-FORMAT is
passed to comment import and defaults to storage.  This helper is comments-only
and does not synchronize page content."
  (let* ((source-buffer (current-buffer))
	 (id (org-confluence-comments-page-id-or-read page-id))
	 (imported (org-confluence-comments-import id body-format))
	 (push-result (org-confluence-sync--push-pending-comments source-buffer id))
	 (pushed (plist-get push-result :pushed))
	 (errors (plist-get push-result :errors)))
    (org-comments-sync-report-provider-message
     "Confluence" :pushed pushed :errors (length errors))
    (list :comments-imported imported
	  :comments-pushed pushed
	  :comment-push-errors errors)))

(provide 'org-confluence-comments-sync)
;;; org-confluence-comments-sync.el ends here
