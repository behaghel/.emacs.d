;;; org-comments-panel-render.el --- Rendering for Org comments panel -*- lexical-binding: t; -*-

;;; Commentary:
;; Generic rendering helpers for the standalone Org comments panel.

;;; Code:

(require 'org)
(require 'org-comments-collaboration)
(require 'org-comments-core)
(require 'org-comments-model)
(require 'subr-x)

(defcustom org-comments-panel-target-preview-length 20
  "Maximum length of quoted comment target previews in comment panels."
  :type 'natnum
  :group 'org-comments)

(defcustom org-comments-panel-overview-comment-lines 2
  "Maximum number of comment body lines shown in comment panel overview rows."
  :type 'natnum
  :group 'org-comments)

(defcustom org-comments-panel-overview-comment-line-length 80
  "Maximum length of each comment overview line."
  :type 'natnum
  :group 'org-comments)

(defun org-comments-panel-render--compact (text)
  "Return TEXT trimmed with whitespace collapsed for panel display."
  (string-trim (replace-regexp-in-string "[[:space:]]+" " " (or text ""))))

(defun org-comments-panel-render--truncate (text length)
  "Return TEXT truncated to LENGTH characters with an ellipsis."
  (if (> (length text) length)
      (concat (substring text 0 length) "…")
    text))

(defun org-comments-panel-render--stale-comment-p (comment)
  "Return non-nil when COMMENT is an unanchored stale comment."
  (and (eq (plist-get comment :type) 'comment)
       (eq (plist-get comment :anchor-state) 'stale)))

(defun org-comments-panel-render--page-comment-p (comment)
  "Return non-nil when COMMENT is page-level."
  (and (eq (plist-get comment :type) 'comment)
       (plist-get comment :page-comment)))

(defun org-comments-panel-render--remote-missing-p (comment)
  "Return non-nil when COMMENT is linked to a missing remote comment."
  (eq (plist-get (org-comments-normalize-record comment) :remote-state) 'missing))

(defun org-comments-panel-render--sync-badge (comment)
  "Return an emoji-only sync-state badge for COMMENT."
  (cond
   ((org-comments-panel-render--remote-missing-p comment) "⚠")
   ((equal (plist-get comment :remote-anchor-state) "dangling") "⚠")
   ((equal (plist-get comment :remote-anchor-state) "unconfirmed") "❓")
   ((plist-get comment :local-updated-at) "✍️")
   ((plist-get comment :remote-id) "🔗")
   (t "✍️")))

(defun org-comments-panel-render--readable-body (comment)
  "Return readable projection of COMMENT body for display."
  (org-comments-preview-plain-text (or (plist-get comment :body) "")))

(defun org-comments-panel-render--org-fontified-text (text)
  "Return TEXT fontified as Org without changing the current buffer mode."
  (with-temp-buffer
    (org-mode)
    (insert text)
    (font-lock-ensure (point-min) (point-max))
    (buffer-string)))

(defun org-comments-panel-render--apply-list-wrap-prefix (start end)
  "Indent wrapped list continuation lines between START and END by two spaces."
  (save-excursion
    (goto-char start)
    (while (< (point) end)
      (when (looking-at-p "[[:space:]]*\\([-+*]\\|[0-9]+[.)]\\)[[:space:]]+")
	(add-text-properties (line-beginning-position) (line-end-position)
			     '(wrap-prefix "  ")))
      (forward-line 1))))

(defun org-comments-panel-render--insert-body (body)
  "Insert readable comment BODY with Org fontification and wrapping hints."
  (unless (string-empty-p body)
    (let ((start (point)))
      (insert (org-comments-panel-render--org-fontified-text body) "\n")
      (org-comments-panel-render--apply-list-wrap-prefix start (point)))))

(defun org-comments-panel-render--format-created-at (created-at)
  "Return compact display text for CREATED-AT."
  (if-let* ((time (ignore-errors (date-to-time created-at))))
      (format-time-string "%Y-%m-%d %H:%M" time)
    created-at))

(defun org-comments-panel-render--insert-metadata (comment)
  "Insert author/date metadata for COMMENT when present."
  (let ((author (or (plist-get comment :remote-author-name)
		    (plist-get comment :remote-author-display-name)
		    (plist-get comment :author)))
	(created-at (plist-get comment :created-at)))
    (when (or author created-at)
      (when author
	(insert author))
      (when (and author created-at)
	(insert " · "))
      (when created-at
	(insert (org-comments-panel-render--format-created-at created-at)))
      (insert "\n"))))

(defun org-comments-panel-render--overview-lines (body)
  "Return compact overview lines for comment BODY."
  (let ((text (org-comments-panel-render--compact body))
	(lines nil)
	(limit org-comments-panel-overview-comment-line-length))
    (while (and (not (string-empty-p text))
		(< (length lines) org-comments-panel-overview-comment-lines))
      (if (<= (length text) limit)
	  (setq lines (append lines (list text))
		text "")
	(let ((chunk (substring text 0 limit)))
	  (setq lines (append lines (list chunk))
		text (string-trim-left (substring text limit))))))
    (when (and lines (not (string-empty-p text)))
      (let* ((last-index (1- (length lines)))
	     (last-line (nth last-index lines)))
	(setf (nth last-index lines)
	      (if (> (length last-line) 1)
		  (concat (substring last-line 0 (1- (length last-line))) "…")
		"…"))))
    lines))

(defun org-comments-panel-render--insert-replies (comment)
  "Insert full reply conversation for COMMENT."
  (dolist (reply (plist-get comment :replies))
    (let ((start (point))
	  (has-metadata (or (plist-get reply :author)
			    (plist-get reply :remote-author-name)
			    (plist-get reply :remote-author-display-name)
			    (plist-get reply :created-at))))
      (insert "\n↳ " (org-comments-panel-render--sync-badge reply)
	      " " (org-comments-sync-state-label reply))
      (if has-metadata
	  (progn
	    (insert " ")
	    (org-comments-panel-render--insert-metadata reply))
	(insert "\n"))
      (org-comments-panel-render--insert-body
       (org-comments-panel-render--readable-body reply))
      (let ((row (copy-sequence reply)))
	(plist-put row :provider 'comments)
	(add-text-properties start (point)
			     `(org-comments-comment ,row
						    org-context-panel-item ,row))))))

(defun org-comments-panel-render-comment-summary (comment)
  "Return a one-line summary for COMMENT."
  (let ((body (org-comments-panel-render--compact (org-comments-panel-render--readable-body comment)))
	(target (org-comments-panel-render--compact (plist-get comment :target-text)))
	(status (or (plist-get comment :status) "OPEN")))
    (string-join
     (delq nil
	   (list (format "[%s]" status)
		 (unless (string-empty-p target)
		   (format "“%s”" target))
		 (unless (string-empty-p body)
		   (format "— %s" body))))
     " ")))

(defun org-comments-panel-render-insert-comment (comment)
  "Insert rich COMMENT row at point."
  (let* ((start (point))
	 (status (or (plist-get comment :status) "OPEN"))
	 (target (or (plist-get comment :target-text) ""))
	 (body (org-comments-panel-render--readable-body comment))
	 (replies (plist-get comment :replies))
	 (stale (org-comments-panel-render--stale-comment-p comment))
	 (remote-missing (org-comments-panel-render--remote-missing-p comment))
	 (page-comment (org-comments-panel-render--page-comment-p comment)))
    (insert (cond (stale "⚠")
		  (remote-missing "⚠")
		  (page-comment "👆")
		  (t "💬"))
	    " [" status "]")
    (cond
     (page-comment
      (insert " PAGE"))
     ((not (string-empty-p target))
      (insert " “" (org-comments-panel-render--truncate
		    target org-comments-panel-target-preview-length) "”")))
    (insert " " (org-comments-panel-render--sync-badge comment) "\n")
    (when stale
      (insert "Anchor no longer matches source text.\n"))
    (when (and remote-missing (plist-get comment :current))
      (insert (format "⚠ remote missing%s\n"
		      (if-let* ((missing-at (plist-get comment :remote-missing-at)))
			  (format " since %s" missing-at)
			""))))
    (org-comments-panel-render--insert-metadata comment)
    (if (plist-get comment :current)
	(progn
	  (org-comments-panel-render--insert-body body)
	  (org-comments-panel-render--insert-replies comment))
      (progn
	(dolist (line (org-comments-panel-render--overview-lines body))
	  (insert line "\n"))
	(when replies
	  (insert (format "↳ %s repl%s\n"
			  (length replies) (if (= (length replies) 1) "y" "ies")))
	  (dolist (reply replies)
	    (insert "  ↳ " (org-comments-panel-render-comment-summary reply) "\n")))))
    (let ((row (copy-sequence comment)))
      (plist-put row :provider 'comments)
      (add-text-properties
       start (point)
       `(org-comments-comment ,row
			      org-context-panel-item ,row
			      mouse-face highlight
			      help-echo "RET: jump, r: reply, e: edit, d: delete")))))

(defun org-comments-panel-render-insert-comments (comments heading)
  "Insert COMMENTS under HEADING."
  (when comments
    (insert (format "\n%s\n" heading))
    (dolist (comment comments)
      (org-comments-panel-render-insert-comment comment))))

(defun org-comments-panel-render-buffer (source-buffer inline-comments page-comments)
  "Render INLINE-COMMENTS and PAGE-COMMENTS for SOURCE-BUFFER."
  (let ((count (+ (length inline-comments) (length page-comments))))
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (format "Org Comments: %s comment%s\n"
		      count (if (= count 1) "" "s")))
      (insert (format "Source: %s\n"
		      (or (buffer-file-name source-buffer)
			  (buffer-name source-buffer))))
      (if (zerop count)
	  (insert "\nNo comments.\n")
	(org-comments-panel-render-insert-comments inline-comments "Inline")
	(org-comments-panel-render-insert-comments page-comments "Page"))
      (goto-char (point-min)))))

(provide 'org-comments-panel-render)
;;; org-comments-panel-render.el ends here
