;;; org-comments-store.el --- Local sidecar store for Org comments -*- lexical-binding: t; -*-

;;; Commentary:
;; Read-model helpers for local Org sidecar comment storage.  Names remain in
;; the legacy `org-comments-*' namespace until the public API migration
;; slice.

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'org-comments-migrate)
(require 'org-comments-sidecar)
(require 'org-comments-target)
(require 'subr-x)

(defun org-comments--valid-anchor-p (source-buffer properties)
  "Return non-nil when PROPERTIES still identify SOURCE-BUFFER text."
  (let* ((target-text (alist-get "ORG_COMMENTS_TARGET_TEXT" properties nil nil #'equal))
	 (target-bounds (org-comments--target-bounds properties))
	 (start (car target-bounds))
	 (end (cdr target-bounds)))
    (and target-text start end
	 (with-current-buffer source-buffer
	   (and (<= (point-min) start)
		(<= start end)
		(<= end (point-max))
		(equal (org-comments-normalize-target-text
			(buffer-substring-no-properties start end))
		       (org-comments-normalize-target-text target-text)))))))

(defun org-comments--normalized-buffer-with-map ()
  "Return normalized buffer text and source position map for current buffer."
  (let ((position (point-min))
	(text nil)
	(map nil)
	(pending-space nil))
    (while (< position (point-max))
      (let ((char (char-after position)))
	(if (and char (string-match-p "[[:space:]]" (char-to-string char)))
	    (let ((start position))
	      (while (and (< position (point-max))
			  (string-match-p "[[:space:]]" (char-to-string (char-after position))))
		(setq position (1+ position)))
	      (setq pending-space (cons start position)))
	  (when pending-space
	    (unless (null text)
	      (push ?\s text)
	      (push pending-space map))
	    (setq pending-space nil))
	  (push char text)
	  (push (cons position (1+ position)) map)
	  (setq position (1+ position)))))
    (cons (apply #'string (nreverse text)) (nreverse map))))

(defun org-comments--source-match-from-normalized (map start end)
  "Return source buffer cons from normalized MAP START END positions."
  (cons (car (nth start map))
	(cdr (nth (1- end) map))))

(defun org-comments--anchor-matches-for-text (source-buffer target-text)
  "Return source-buffer matches for normalized TARGET-TEXT."
  (let ((target (org-comments-normalize-target-text (or target-text ""))))
    (unless (string-empty-p target)
      (with-current-buffer source-buffer
	(let* ((normalized (org-comments--normalized-buffer-with-map))
	       (source (car normalized))
	       (map (cdr normalized))
	       (start 0)
	       matches)
	  (while (string-match (regexp-quote target) source start)
	    (let ((match-start (match-beginning 0))
		  (match-end (match-end 0)))
	      (push (org-comments--source-match-from-normalized map match-start match-end)
		    matches)
	      (setq start (max (1+ match-start) match-end))))
	  (nreverse matches))))))

(defun org-comments--recovered-anchor-bounds (source-buffer properties)
  "Return recovered source bounds for PROPERTIES in SOURCE-BUFFER, or nil."
  (let* ((target-text (alist-get "ORG_COMMENTS_TARGET_TEXT" properties nil nil #'equal))
	 (matches (org-comments--anchor-matches-for-text source-buffer target-text)))
    (when (= (length matches) 1)
      (car matches))))

(defun org-comments--reply-records-at-heading (sidecar-file)
  "Return direct reply child records under heading at point in SIDECAR-FILE."
  (let ((level (org-outline-level))
	(end (save-excursion (org-end-of-subtree t t)))
	replies)
    (save-excursion
      (forward-line 1)
      (while (re-search-forward org-heading-regexp end t)
	(goto-char (match-beginning 0))
	(cond
	 ((<= (org-outline-level) level)
	  (goto-char end))
	 ((and (= (org-outline-level) (1+ level))
	       (org-comments--reply-heading-p))
	  (let* ((properties (org-comments--parse-properties-at-heading))
		 (entry-end (save-excursion (org-end-of-subtree t t)))
		 (body (org-comments--entry-body entry-end)))
	    (push (org-comments--base-record properties sidecar-file nil nil nil body)
		  replies))))
	(forward-line 1)))
    (nreverse replies)))

(defun org-comments--page-record-from-heading (sidecar-file)
  "Return a page-level comment record at point in SIDECAR-FILE, or nil."
  (let* ((properties (org-comments--parse-properties-at-heading))
	 (id (alist-get "ORG_COMMENTS_ID" properties nil nil #'equal))
	 (sync-kind (alist-get "ORG_COMMENTS_SYNC_KIND" properties nil nil #'equal))
	 (entry-end (save-excursion (org-end-of-subtree t t)))
	 (body (org-comments--entry-body entry-end)))
    (when (and id (equal sync-kind "footer"))
      (append (org-comments--base-record properties sidecar-file nil nil nil body)
	      (list :page-comment t
		    :anchor-state 'page
		    :anchor-line 1
		    :jump-pos nil
		    :replies (org-comments--reply-records-at-heading sidecar-file))))))

(defun org-comments--record-from-heading (sidecar-file source-buffer &optional include-stale)
  "Return a comment record at heading in SIDECAR-FILE for SOURCE-BUFFER.
When INCLUDE-STALE is non-nil, return unanchored stale records for comments
whose stored target no longer validates against the source buffer."
  (let* ((properties (org-comments--parse-properties-at-heading))
	 (id (alist-get "ORG_COMMENTS_ID" properties nil nil #'equal))
	 (target-text (alist-get "ORG_COMMENTS_TARGET_TEXT" properties nil nil #'equal))
	 (target-bounds (org-comments--target-bounds properties))
	 (start (car target-bounds))
	 (end (cdr target-bounds))
	 (entry-end (save-excursion (org-end-of-subtree t t)))
	 (body (org-comments--entry-body entry-end))
	 (sync-kind (alist-get "ORG_COMMENTS_SYNC_KIND" properties nil nil #'equal)))
    (when (and id
	       (not (equal sync-kind "reply"))
	       (or target-text (equal sync-kind "inline")))
      (let ((record (append (org-comments--base-record
			     properties sidecar-file target-text start end body)
			    (list :replies (org-comments--reply-records-at-heading sidecar-file)))))
	(if (org-comments--valid-anchor-p source-buffer properties)
	    (with-current-buffer source-buffer
	      (append record
		      (list :anchor-line (line-number-at-pos start t)
			    :anchor-pos start
			    :jump-pos start)))
	  (if-let* ((recovered (org-comments--recovered-anchor-bounds
				source-buffer properties)))
	      (with-current-buffer source-buffer
		(let ((recovered-record (copy-sequence record)))
		  (plist-put recovered-record :target-start (car recovered))
		  (plist-put recovered-record :target-end (cdr recovered))
		  (append recovered-record
			  (list :anchor-line (line-number-at-pos (car recovered) t)
				:anchor-pos (car recovered)
				:jump-pos (car recovered)
				:anchor-state 'recovered))))
	    (when include-stale
	      (append record
		      (list :anchor-state 'stale
			    :stale t
			    :anchor-line most-positive-fixnum)))))))))

(defun org-comments-goto-id (comment-id)
  "Move point to sidecar heading with COMMENT-ID and return non-nil when found."
  (goto-char (point-min))
  (cl-loop while (re-search-forward org-heading-regexp nil t)
	   do (goto-char (match-beginning 0))
	   when (equal comment-id (org-entry-get nil "ORG_COMMENTS_ID"))
	   return t
	   do (forward-line 1)))

(defun org-comments--ensure-native-sidecar (sidecar-file)
  "Signal when SIDECAR-FILE still uses legacy comment metadata."
  (when (org-comments-legacy-sidecar-p sidecar-file)
    (user-error
     "Comments sidecar uses legacy %s metadata; run M-x org-comments-migrate-sidecar: %s"
     org-comments-migrate-legacy-property-prefix
     sidecar-file)))

(defun org-comments-local-id-for-remote-id (sidecar-file remote-id)
  "Return local comment id in SIDECAR-FILE for Confluence REMOTE-ID."
  (when (and remote-id (file-exists-p sidecar-file))
    (org-comments--ensure-native-sidecar sidecar-file)
    (with-temp-buffer
      (insert-file-contents sidecar-file)
      (delay-mode-hooks
	(org-mode))
      (goto-char (point-min))
      (cl-loop while (re-search-forward org-heading-regexp nil t)
	       do (goto-char (match-beginning 0))
	       when (equal remote-id (org-entry-get nil "ORG_COMMENTS_REMOTE_ID"))
	       return (org-entry-get nil "ORG_COMMENTS_ID")
	       do (forward-line 1)))))

(defun org-comments--collect-from-sidecar (sidecar-file collector)
  "Collect comments from SIDECAR-FILE using heading COLLECTOR."
  (org-comments--ensure-native-sidecar sidecar-file)
  (with-temp-buffer
    (insert-file-contents sidecar-file)
    (delay-mode-hooks
      (org-mode))
    (let (comments)
      (goto-char (point-min))
      (while (re-search-forward org-heading-regexp nil t)
	(goto-char (match-beginning 0))
	(when-let* ((record (funcall collector)))
	  (push record comments))
	(forward-line 1))
      (nreverse comments))))

(defun org-comments-collect (&optional source-buffer include-stale)
  "Collect valid sidecar comments for SOURCE-BUFFER or current buffer.
When INCLUDE-STALE is non-nil, include comments whose stored target no longer
matches the source buffer as unanchored stale records."
  (let* ((buffer (or source-buffer (current-buffer)))
	 (source-file (buffer-file-name buffer))
	 (sidecar-file (and source-file (org-comments-sidecar-path source-file))))
    (when (and sidecar-file (file-exists-p sidecar-file))
      (org-comments--collect-from-sidecar
       sidecar-file
       (lambda ()
	 (org-comments--record-from-heading sidecar-file buffer include-stale))))))

(defun org-comments-collect-page (&optional source-buffer)
  "Collect page-level sidecar comments for SOURCE-BUFFER or current buffer."
  (let* ((buffer (or source-buffer (current-buffer)))
	 (source-file (buffer-file-name buffer))
	 (sidecar-file (and source-file (org-comments-sidecar-path source-file))))
    (when (and sidecar-file (file-exists-p sidecar-file))
      (org-comments--collect-from-sidecar
       sidecar-file
       (lambda ()
	 (org-comments--page-record-from-heading sidecar-file))))))

(provide 'org-comments-store)
;;; org-comments-store.el ends here
