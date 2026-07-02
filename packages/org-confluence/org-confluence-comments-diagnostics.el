;;; org-confluence-comments-diagnostics.el --- Confluence comment diagnostics -*- lexical-binding: t; -*-

;;; Commentary:
;; Read-only diagnostic rendering for remote Confluence comments.

;;; Code:

(require 'subr-x)

(require 'org-confluence-api)
(require 'org-confluence-comments-remote)
(require 'org-confluence-response)

(defun org-confluence-comments-diagnostics-raw-resolution-fields (comment)
  "Return raw resolution fields found in COMMENT."
  (let ((properties (alist-get 'properties comment))
	fields)
    (dolist (key '(resolutionStatus resolution-status resolved))
      (when-let* ((value (alist-get key comment)))
	(push (format "%s=%S" key value) fields)))
    (when (listp properties)
      (dolist (key '(resolutionStatus resolution-status resolved))
	(when-let* ((value (alist-get key properties)))
	  (push (format "properties.%s=%S" key value) fields))))
    (nreverse fields)))

(defun org-confluence-comments-diagnostics-raw-status-fields (comment)
  "Return raw non-resolution status fields found in COMMENT."
  (let ((properties (alist-get 'properties comment))
	fields)
    (when-let* ((value (alist-get 'status comment)))
      (push (format "status=%S" value) fields))
    (when (listp properties)
      (when-let* ((value (alist-get 'status properties)))
	(push (format "properties.status=%S" value) fields)))
    (nreverse fields)))

(defun org-confluence-comments-diagnostics-insert-comment (comment)
  "Insert one remote COMMENT diagnostic entry at point."
  (let* ((id (or (alist-get 'id comment) "<missing id>"))
	 (storage (org-confluence-comments-remote-body-storage comment))
	 (representation (or (alist-get 'representation storage) "unknown"))
	 (resolution (or (org-confluence-comments-remote-resolution-status comment)
			 "unknown"))
	 (raw-resolution-fields (org-confluence-comments-diagnostics-raw-resolution-fields comment))
	 (raw-status-fields (org-confluence-comments-diagnostics-raw-status-fields comment))
	 (value (or (alist-get 'value storage) "")))
    (insert (format "- id: %s\n" id))
    (insert (format "  resolution: %s\n" resolution))
    (when raw-resolution-fields
      (insert (format "  resolution-fields: %s\n"
		      (string-join raw-resolution-fields ", "))))
    (when raw-status-fields
      (insert (format "  status-fields: %s\n"
		      (string-join raw-status-fields ", "))))
    (insert (format "  representation: %s\n" representation))
    (unless (string-empty-p value)
      (insert "  body:\n")
      (dolist (line (split-string value "\n"))
	(insert (format "    %s\n" line))))))

(defun org-confluence-comments-diagnostics-insert-section (kind body-format response)
  "Insert diagnostic section for comment KIND and REST RESPONSE."
  (let ((comments (org-confluence-response-comment-results response)))
    (insert (format "* %s\n" kind))
    (insert (format "body-format: %s\n" body-format))
    (insert (format "count: %s\n\n" (length comments)))
    (if comments
	(dolist (comment comments)
	  (org-confluence-comments-diagnostics-insert-comment comment))
      (insert "No comments.\n"))))

;;;###autoload
(defun org-confluence-comments-list (&optional page-id body-format)
  "Fetch Confluence comments for PAGE-ID and render a diagnostic buffer.
This command is intentionally read-only: it does not write sidecar files or
modify the source Org buffer.  BODY-FORMAT defaults to storage."
  (interactive)
  (let* ((id (or page-id
		 (org-confluence-api--page-id-from-buffer)
		 (read-string "Confluence page ID: ")))
	 (format-name (or body-format "storage"))
	 (footer-response (org-confluence-api--list-page-comments
			   id "footer-comments" format-name))
	 (inline-response (org-confluence-api--list-page-comments
			   id "inline-comments" format-name))
	 (buffer (generate-new-buffer (format "*Confluence Comments %s*" id))))
    (with-current-buffer buffer
      (special-mode)
      (let ((inhibit-read-only t))
	(erase-buffer)
	(insert (format "Confluence comments for page %s\n\n" id))
	(org-confluence-comments-diagnostics-insert-section
	 "footer-comments" format-name footer-response)
	(insert "\n")
	(org-confluence-comments-diagnostics-insert-section
	 "inline-comments" format-name inline-response)
	(goto-char (point-min))))
    (pop-to-buffer buffer)))


(provide 'org-confluence-comments-diagnostics)
;;; org-confluence-comments-diagnostics.el ends here
