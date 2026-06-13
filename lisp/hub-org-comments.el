;;; hub-org-comments.el --- Org sidecar comment storage -*- lexical-binding: t; -*-

;;; Commentary:
;; Plain Org sidecar storage for region-targeted comments.  Source Org buffers
;; remain clean; comments are stored next to them as `article.comments.org'.

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'subr-x)

(defgroup hub/org-comments nil
  "Region-targeted Org sidecar comments."
  :group 'org)

(defcustom hub/org-comment-heading-preview-length 60
  "Maximum normalized target text length used in sidecar headings."
  :type 'natnum
  :group 'hub/org-comments)

(defun hub/org-comment-sidecar-path (&optional source-file)
  "Return sidecar comments path for SOURCE-FILE or current buffer file.
For example, article.org maps to article.comments.org."
  (let ((file (or source-file buffer-file-name)))
    (unless file
      (user-error "Current buffer is not visiting a file"))
    (concat (file-name-sans-extension file) ".comments.org")))

(defun hub/org-comment-normalize-target-text (text)
  "Return TEXT trimmed with whitespace runs collapsed to one space."
  (string-trim (replace-regexp-in-string "[[:space:]]+" " " text)))

(defun hub/org-comment-target-hash (target-text)
  "Return sha256 hash string for normalized TARGET-TEXT."
  (concat "sha256:" (secure-hash 'sha256 (hub/org-comment-normalize-target-text target-text))))

(defun hub/org-comment--random-hex ()
  "Return a short random hexadecimal suffix for local comment IDs."
  (format "%06x" (random #x1000000)))

(defun hub/org-comment-generate-id ()
  "Return a stable local comment ID."
  (format "local-%s-%s" (format-time-string "%Y%m%dT%H%M%S") (hub/org-comment--random-hex)))

(defun hub/org-comment--line-column-at (position)
  "Return cons cell of one-based line and zero-based column at POSITION."
  (save-excursion
    (goto-char position)
    (cons (line-number-at-pos position t) (current-column))))

(defun hub/org-comment--heading-preview (target-text)
  "Return sidecar heading preview for normalized TARGET-TEXT."
  (let ((text (hub/org-comment-normalize-target-text target-text)))
    (if (> (length text) hub/org-comment-heading-preview-length)
	(concat (substring text 0 hub/org-comment-heading-preview-length) "…")
      text)))

(defun hub/org-comment-create-record (source-file start end body &optional id)
  "Return a comment plist for SOURCE-FILE region START to END with BODY.
ID defaults to a new local ID."
  (let* ((target-raw (buffer-substring-no-properties start end))
	 (target-text (hub/org-comment-normalize-target-text target-raw))
	 (start-line-column (hub/org-comment--line-column-at start))
	 (end-line-column (hub/org-comment--line-column-at end)))
    (list :id (or id (hub/org-comment-generate-id))
	  :status "open"
	  :source-file source-file
	  :target-text target-text
	  :target-hash (hub/org-comment-target-hash target-text)
	  :target-start start
	  :target-end end
	  :target-start-line (car start-line-column)
	  :target-start-column (cdr start-line-column)
	  :target-end-line (car end-line-column)
	  :target-end-column (cdr end-line-column)
	  :title (concat "Comment: " (hub/org-comment--heading-preview target-text))
	  :body body)))

(defun hub/org-comment--relative-source-file (source-file sidecar-file)
  "Return SOURCE-FILE relative to SIDECAR-FILE directory."
  (file-relative-name source-file (file-name-directory sidecar-file)))

(defun hub/org-comment--ensure-sidecar-header (sidecar-file source-file)
  "Create SIDECAR-FILE with a minimal header for SOURCE-FILE when absent."
  (unless (file-exists-p sidecar-file)
    (make-directory (file-name-directory sidecar-file) t)
    (with-temp-file sidecar-file
      (insert (format "#+title: Comments for %s\n" (file-name-nondirectory source-file)))
      (insert (format "#+source: %s\n\n"
		      (hub/org-comment--relative-source-file source-file sidecar-file))))))

(defun hub/org-comment--property-line (key value)
  "Return an Org property line for KEY and VALUE."
  (format ":%s: %s\n" key value))

(defun hub/org-comment-format-entry (record sidecar-file)
  "Return Org text for comment RECORD stored in SIDECAR-FILE."
  (let ((source-file (plist-get record :source-file)))
    (concat
     (format "* TODO %s\n" (plist-get record :title))
     ":PROPERTIES:\n"
     (hub/org-comment--property-line "HUB_COMMENT_ID" (plist-get record :id))
     (hub/org-comment--property-line "HUB_COMMENT_STATUS" (plist-get record :status))
     (hub/org-comment--property-line
      "HUB_COMMENT_SOURCE_FILE"
      (hub/org-comment--relative-source-file source-file sidecar-file))
     (hub/org-comment--property-line "HUB_COMMENT_TARGET_TEXT" (plist-get record :target-text))
     (hub/org-comment--property-line "HUB_COMMENT_TARGET_HASH" (plist-get record :target-hash))
     (hub/org-comment--property-line "HUB_COMMENT_TARGET_START" (plist-get record :target-start))
     (hub/org-comment--property-line "HUB_COMMENT_TARGET_END" (plist-get record :target-end))
     (hub/org-comment--property-line "HUB_COMMENT_TARGET_START_LINE" (plist-get record :target-start-line))
     (hub/org-comment--property-line "HUB_COMMENT_TARGET_START_COLUMN" (plist-get record :target-start-column))
     (hub/org-comment--property-line "HUB_COMMENT_TARGET_END_LINE" (plist-get record :target-end-line))
     (hub/org-comment--property-line "HUB_COMMENT_TARGET_END_COLUMN" (plist-get record :target-end-column))
     ":END:\n\n"
     (string-trim-right (or (plist-get record :body) ""))
     "\n")))

(defun hub/org-comment-append-to-sidecar (record &optional sidecar-file)
  "Append comment RECORD to SIDECAR-FILE and return SIDECAR-FILE.
SIDECAR-FILE defaults to the sidecar path for RECORD's source file."
  (let* ((source-file (plist-get record :source-file))
	 (target-file (or sidecar-file (hub/org-comment-sidecar-path source-file))))
    (hub/org-comment--ensure-sidecar-header target-file source-file)
    (with-temp-buffer
      (insert-file-contents target-file)
      (goto-char (point-max))
      (unless (bolp) (insert "\n"))
      (unless (save-excursion
		(forward-line -1)
		(looking-at-p "[[:space:]]*$"))
	(insert "\n"))
      (insert (hub/org-comment-format-entry record target-file))
      (write-region (point-min) (point-max) target-file nil 'silent))
    target-file))

(defun hub/org-comment--parse-properties-at-heading ()
  "Return an alist of Org properties at point without inherited values."
  (org-entry-properties nil nil))

(defun hub/org-comment--entry-body (end)
  "Return current heading body text ending before END."
  (save-excursion
    (let ((body-start (progn
			(forward-line 1)
			(when (looking-at-p "[	]*:PROPERTIES:[		]*$")
			  (when (re-search-forward "^[	]*:END:[	]*$" end t)
			    (forward-line 1)))
			(point))))
      (string-trim (buffer-substring-no-properties body-start end)))))

(defun hub/org-comment--number-property (properties key)
  "Return numeric property KEY from PROPERTIES, or nil."
  (when-let* ((value (alist-get key properties nil nil #'equal)))
    (string-to-number value)))

(defun hub/org-comment--record-from-heading (sidecar-file source-buffer)
  "Return a comment record at heading in SIDECAR-FILE for SOURCE-BUFFER."
  (let* ((properties (hub/org-comment--parse-properties-at-heading))
	 (id (alist-get "HUB_COMMENT_ID" properties nil nil #'equal))
	 (target-text (alist-get "HUB_COMMENT_TARGET_TEXT" properties nil nil #'equal))
	 (start (hub/org-comment--number-property properties "HUB_COMMENT_TARGET_START"))
	 (end (hub/org-comment--number-property properties "HUB_COMMENT_TARGET_END"))
	 (entry-end (save-excursion (org-end-of-subtree t t)))
	 (body (hub/org-comment--entry-body entry-end)))
    (when (and id target-text start end)
      (with-current-buffer source-buffer
	(when (and (<= (point-min) start)
		   (<= start end)
		   (<= end (point-max))
		   (equal (hub/org-comment-normalize-target-text
			   (buffer-substring-no-properties start end))
			  target-text))
	  (list :type 'comment
		:id id
		:kind 'comment
		:status (or (alist-get "HUB_COMMENT_STATUS" properties nil nil #'equal) "open")
		:source-file (alist-get "HUB_COMMENT_SOURCE_FILE" properties nil nil #'equal)
		:sidecar-file sidecar-file
		:target-text target-text
		:target-start start
		:target-end end
		:anchor-line (line-number-at-pos start t)
		:anchor-pos start
		:jump-pos start
		:body body
		:height (max 3 (+ 3 (length (split-string body "\n" t))))))))))

(defun hub/org-comment-collect (&optional source-buffer)
  "Collect valid sidecar comments for SOURCE-BUFFER or current buffer."
  (let* ((buffer (or source-buffer (current-buffer)))
	 (source-file (buffer-file-name buffer))
	 (sidecar-file (and source-file (hub/org-comment-sidecar-path source-file))))
    (when (and sidecar-file (file-exists-p sidecar-file))
      (with-temp-buffer
	(insert-file-contents sidecar-file)
	(org-mode)
	(let (comments)
	  (goto-char (point-min))
	  (while (re-search-forward org-heading-regexp nil t)
	    (goto-char (match-beginning 0))
	    (when-let* ((record (hub/org-comment--record-from-heading sidecar-file buffer)))
	      (push record comments))
	    (forward-line 1))
	  (nreverse comments))))))

(provide 'hub-org-comments)
;;; hub-org-comments.el ends here
