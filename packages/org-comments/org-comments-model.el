;;; org-comments-model.el --- Comment records and headings for Org comments -*- lexical-binding: t; -*-

;;; Commentary:
;; Comment record creation and human-readable heading helpers for the initial
;; Org comments extraction.  Names remain in the legacy `org-comments-*'
;; namespace until the public API migration slice.

;;; Code:

(require 'dom)
(require 'org-comments-core)
(require 'org-comments-target)
(require 'seq)
(require 'subr-x)

(defun org-comments--heading-preview (target-text)
  "Return sidecar heading preview for normalized TARGET-TEXT."
  (org-comments--truncate
   (org-comments-normalize-target-text target-text)
   org-comments-heading-preview-length))

(defun org-comments--html-fragment-text (text)
  "Return TEXT as decoded plain text using the HTML parser when possible."
  (condition-case nil
      (with-temp-buffer
	(insert (or text ""))
	(let* ((document (libxml-parse-html-region (point-min) (point-max)))
	       (body (car (dom-by-tag document 'body))))
	  (or (and body (dom-texts body)) text)))
    (error text)))

(defun org-comments--preview-plain-text (text)
  "Return TEXT stripped of markup and decoded for heading previews."
  (if (string-match-p "\\(&[#[:alnum:]]+;\\|<[[:alpha:]!/][^>]*>\\)" (or text ""))
      (org-comments--html-fragment-text text)
    (or text "")))

(defun org-comments-preview-plain-text (text)
  "Return TEXT stripped of markup and decoded for display previews."
  (org-comments--preview-plain-text text))

(defun org-comments--preview-text (text length)
  "Return TEXT normalized, decoded for heading use, and truncated to LENGTH."
  (let ((preview (org-comments-normalize-target-text
		  (org-comments--preview-plain-text (or text "")))))
    (unless (string-empty-p preview)
      (org-comments--truncate preview length))))

(defun org-comments--heading-author (record &optional directory)
  "Return display author for comment RECORD using optional DIRECTORY people lookup."
  (let ((author (plist-get record :author))
	(remote-author-display-name (plist-get record :remote-author-display-name))
	(remote-author-id (plist-get record :remote-author-id)))
    (or (org-comments-resolve-account-id remote-author-id directory)
	remote-author-display-name
	author
	remote-author-id)))

(defun org-comments-heading-title (record &optional directory)
  "Return human-readable sidecar heading title for comment RECORD.
DIRECTORY is used for resolving Confluence account IDs through people files."
  (let* ((page-comment (equal (plist-get record :sync-kind) "footer"))
	 (reply-count (plist-get record :reply-count))
	 (author (org-comments--heading-author record directory))
	 (target (org-comments--preview-text
		  (plist-get record :target-text)
		  org-comments-heading-target-preview-length))
	 (body (org-comments--preview-text
		(plist-get record :body)
		org-comments-heading-body-preview-length))
	 (prefix-parts (delq nil (list (when page-comment "Page") author)))
	 (prefix (string-join prefix-parts " · "))
	 (subject (if target (format "“%s”" target) body))
	 (main (string-join (delq nil (list (unless (string-empty-p prefix) prefix)
					    subject))
			    " · ")))
    (setq main (if (and target body)
		   (format "%s — %s" main body)
		 main))
    (if (and reply-count (> reply-count 0))
	(format "[%s %s] %s" reply-count (if (= reply-count 1) "reply" "replies") main)
      main)))

(defun org-comments-reply-heading-title (record &optional directory)
  "Return human-readable sidecar reply heading title for comment RECORD."
  (let* ((author (org-comments--heading-author record directory))
	 (created-at (org-comments--preview-text (plist-get record :created-at) 16))
	 (body (org-comments--preview-text
		(plist-get record :body)
		org-comments-heading-body-preview-length)))
    (string-join (delq nil (list "Reply" author created-at (when body (concat "— " body))))
		 " · ")))

(defun org-comments-create-record (source-file start end body &optional id author created-at)
  "Return a comment plist for SOURCE-FILE region START to END with BODY.
ID defaults to a new local ID.  AUTHOR and CREATED-AT default to local metadata."
  (let* ((target-raw (buffer-substring-no-properties start end))
	 (target-text (org-comments-normalize-target-text target-raw))
	 (start-line-column (org-comments--line-column-at start))
	 (end-line-column (org-comments--line-column-at end)))
    (list :id (or id (org-comments-generate-id))
	  :status "OPEN"
	  :source-file source-file
	  :author (or author (org-comments-current-author))
	  :created-at (or created-at (org-comments-current-created-at))
	  :target-text target-text
	  :target-hash (org-comments-target-hash target-text)
	  :target-start start
	  :target-end end
	  :target-start-line (car start-line-column)
	  :target-start-column (cdr start-line-column)
	  :target-end-line (car end-line-column)
	  :target-end-column (cdr end-line-column)
	  :sync-kind "inline"
	  :body body)))

(provide 'org-comments-model)
;;; org-comments-model.el ends here
