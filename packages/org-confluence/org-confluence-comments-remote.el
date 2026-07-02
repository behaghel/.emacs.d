;;; org-confluence-comments-remote.el --- Confluence comment payload helpers -*- lexical-binding: t; -*-

;;; Commentary:
;; Helpers for reading Confluence remote comment payloads.

;;; Code:

(require 'subr-x)

(defun org-confluence-comments-remote-body-storage (comment)
  "Return COMMENT storage body alist, or nil."
  (alist-get 'storage (alist-get 'body comment)))

(defun org-confluence-comments-remote-id (comment)
  "Return remote Confluence COMMENT id as a string, or nil."
  (when-let* ((id (alist-get 'id comment)))
    (format "%s" id)))

(defun org-confluence-comments-remote-body (comment)
  "Return raw storage body for remote Confluence COMMENT."
  (or (alist-get 'value (org-confluence-comments-remote-body-storage comment)) ""))

(defun org-confluence-comments-remote-author-id (comment)
  "Return remote author ID for Confluence COMMENT, or nil."
  (or (alist-get 'authorId comment)
      (alist-get 'author-id comment)
      (alist-get 'authorId (alist-get 'version comment))
      (alist-get 'author-id (alist-get 'version comment))
      (alist-get 'accountId (alist-get 'author comment))))

(defun org-confluence-comments-remote-author-display-name (comment)
  "Return display author name for Confluence COMMENT, or nil."
  (or (alist-get 'displayName (alist-get 'author comment))
      (alist-get 'publicName (alist-get 'author comment))))

(defun org-confluence-comments-remote-author-name (comment)
  "Return best author label for Confluence COMMENT, or nil."
  (or (org-confluence-comments-remote-author-display-name comment)
      (org-confluence-comments-remote-author-id comment)))

(defun org-confluence-comments-remote-created-at (comment)
  "Return remote created timestamp for Confluence COMMENT, or nil."
  (or (alist-get 'createdAt comment)
      (alist-get 'created-at comment)
      (alist-get 'createdAt (alist-get 'version comment))
      (alist-get 'created-at (alist-get 'version comment))))

(defun org-confluence-comments-remote-version-number (comment)
  "Return current Confluence version number for COMMENT, or nil."
  (let ((number (or (alist-get 'number (alist-get 'version comment))
		    (alist-get 'versionNumber comment)
		    (alist-get 'version-number comment))))
    (cond
     ((numberp number) number)
     ((stringp number) (string-to-number number)))))

(defun org-confluence-comments-remote-updated-at (comment)
  "Return remote updated timestamp for Confluence COMMENT, or nil."
  (or (alist-get 'createdAt (alist-get 'version comment))
      (alist-get 'created-at (alist-get 'version comment))
      (alist-get 'updatedAt comment)
      (alist-get 'updated-at comment)))

(defun org-confluence-comments-remote-resolution-status (comment)
  "Return normalized remote resolution status for Confluence COMMENT, or nil."
  (let* ((properties (alist-get 'properties comment))
	 (raw (or (alist-get 'resolutionStatus comment)
		  (alist-get 'resolution-status comment)
		  (alist-get 'resolved comment)
		  (and (listp properties)
		       (or (alist-get 'resolutionStatus properties)
			   (alist-get 'resolution-status properties)
			   (alist-get 'resolved properties))))))
    (cond
     ((eq raw t) "resolved")
     ((null raw) nil)
     ((stringp raw)
      (let ((value (downcase (string-trim raw))))
	(cond
	 ((member value '("resolved" "closed" "done" "true")) "resolved")
	 ((member value '("dangling")) "dangling")
	 ((member value '("open" "unresolved" "reopened" "active" "false")) "open"))))
     (t nil))))

(defun org-confluence-comments-remote-inline-target-object (comment)
  "Return best available raw inline target object from COMMENT, or nil."
  (or (alist-get 'inlineCommentProperties comment)
      (alist-get 'inline-comment-properties comment)
      (alist-get 'target comment)
      (alist-get 'properties comment)))

(defun org-confluence-comments-remote-inline-target-text (comment)
  "Return best available selected target text from inline COMMENT, or nil."
  (let ((target (org-confluence-comments-remote-inline-target-object comment)))
    (or (alist-get 'originalSelection comment)
	(alist-get 'original-selection comment)
	(alist-get 'inlineOriginalSelection comment)
	(alist-get 'inline-original-selection comment)
	(alist-get 'selectedText comment)
	(alist-get 'selected-text comment)
	(alist-get 'text comment)
	(and (listp target)
	     (or (alist-get 'originalSelection target)
		 (alist-get 'original-selection target)
		 (alist-get 'inlineOriginalSelection target)
		 (alist-get 'inline-original-selection target)
		 (alist-get 'selectedText target)
		 (alist-get 'selected-text target)
		 (alist-get 'text target))))))

(defun org-confluence-comments-remote-inline-marker-ref (comment)
  "Return Confluence inline marker reference for COMMENT, or nil."
  (let ((target (org-confluence-comments-remote-inline-target-object comment)))
    (or (alist-get 'inlineMarkerRef comment)
	(alist-get 'inline-marker-ref comment)
	(alist-get 'markerRef comment)
	(alist-get 'marker-ref comment)
	(and (listp target)
	     (or (alist-get 'inlineMarkerRef target)
		 (alist-get 'inline-marker-ref target)
		 (alist-get 'markerRef target)
		 (alist-get 'marker-ref target))))))

(defun org-confluence-comments-remote-inline-match-number (comment keys)
  "Return numeric inline match property from COMMENT using KEYS."
  (let ((target (org-confluence-comments-remote-inline-target-object comment))
	value)
    (dolist (key keys)
      (setq value (or value
		      (alist-get key comment)
		      (and (listp target) (alist-get key target)))))
    (cond
     ((numberp value) value)
     ((and (stringp value) (string-match-p "\\`[0-9]+\\'" value))
      (string-to-number value)))))

(defun org-confluence-comments-remote-inline-match-count (comment)
  "Return Confluence text selection match count for inline COMMENT, or nil."
  (org-confluence-comments-remote-inline-match-number
   comment '(textSelectionMatchCount text-selection-match-count matchCount match-count)))

(defun org-confluence-comments-remote-inline-match-index (comment)
  "Return Confluence text selection match index for inline COMMENT, or nil."
  (org-confluence-comments-remote-inline-match-number
   comment '(textSelectionMatchIndex text-selection-match-index matchIndex match-index)))

(defun org-confluence-comments-remote-marker-occurrence-count (comment)
  "Return remote marker occurrence count from COMMENT, or nil."
  (alist-get 'org-confluence-marker-occurrence-count comment))

(defun org-confluence-comments-remote-marker-occurrence-index (comment)
  "Return remote marker occurrence index from COMMENT, or nil."
  (alist-get 'org-confluence-marker-occurrence-index comment))


(provide 'org-confluence-comments-remote)
;;; org-confluence-comments-remote.el ends here
