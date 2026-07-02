;;; org-confluence-comments-open.el --- Open Confluence comments -*- lexical-binding: t; -*-

;;; Commentary:
;; Resolve the current Org sidecar/source comment and open its remote
;; Confluence URL through the generic org-comments backend seam.

;;; Code:

(require 'org)
(require 'org-comments-backend)
(require 'org-comments-store)
(require 'seq)
(require 'subr-x)

(require 'org-confluence-api)
(require 'org-confluence-comments-context)

(defun org-confluence-comments-open-sidecar-comment-id-at-point ()
  "Return remote Confluence comment ID from current sidecar heading."
  (unless (and buffer-file-name
	       (string-suffix-p ".comments.org" buffer-file-name))
    (user-error "Current buffer is not a comments sidecar"))
  (unless (derived-mode-p 'org-mode)
    (org-mode))
  (when (org-before-first-heading-p)
    (user-error "Point is not on a sidecar comment"))
  (org-back-to-heading t)
  (or (org-entry-get nil "ORG_COMMENTS_REMOTE_ID")
      (user-error "Comment is not linked to Confluence")))

(defun org-confluence-comments-open-active-source-comment ()
  "Return active sidecar comment record at point in the current source buffer."
  (let ((point (point)))
    (or (seq-find (lambda (comment)
		    (let ((start (plist-get comment :target-start))
			  (end (plist-get comment :target-end)))
		      (and start end (<= start point) (<= point end))))
		  (org-comments-collect (current-buffer)))
	(user-error "No active sidecar comment at point"))))

(defun org-confluence-comments-open-source-comment-id-at-point ()
  "Return remote Confluence comment ID for active source comment at point."
  (or (plist-get (org-confluence-comments-open-active-source-comment) :remote-id)
      (user-error "Comment is not linked to Confluence")))

;;;###autoload
(defun org-confluence-comments-open-current (&optional page-id comment-id)
  "Open Confluence COMMENT-ID for PAGE-ID in a browser.
When COMMENT-ID is nil, use the current sidecar heading or active source
comment."
  (interactive)
  (let* ((metadata (if (and buffer-file-name
			    (string-suffix-p ".comments.org" buffer-file-name))
		       (org-confluence-comments-source-page-metadata-from-sidecar)
		     (list :page-id (or page-id
					(org-confluence-api--page-id-from-buffer)
					(read-string "Confluence page ID: "))
			   :space (org-confluence-api--keyword-from-buffer "CONFLUENCE_SPACE"))))
	 (id (or comment-id
		 (if (and buffer-file-name
			  (string-suffix-p ".comments.org" buffer-file-name))
		     (org-confluence-comments-open-sidecar-comment-id-at-point)
		   (org-confluence-comments-open-source-comment-id-at-point)))))
    (require 'org-confluence-comments-backend)
    (org-comments-backend-open-remote
     'confluence
     (list :page-id (plist-get metadata :page-id)
	   :remote-id id
	   :space (plist-get metadata :space)
	   :source-file (buffer-file-name)))))



(provide 'org-confluence-comments-open)
;;; org-confluence-comments-open.el ends here
