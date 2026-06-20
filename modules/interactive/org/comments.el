;;; comments.el --- Org sidecar comment commands -*- lexical-binding: t; -*-

;;; Commentary:
;; Interactive commands for region-targeted Org comments stored in colocated
;; plain-Org sidecar files.

;;; Code:

(require 'cl-lib)
(require 'hub-confluence-people)
(require 'hub-org-comments)
(require 'org/context-panel)
(require 'org)
(require 'subr-x)

(defun hub/org-comment--region-bounds ()
  "Return active region bounds or signal a user error."
  (unless (use-region-p)
    (user-error "Select a region to comment on"))
  (cons (region-beginning) (region-end)))

(defun hub/org-comment--valid-comments ()
  "Return valid sidecar comments for the current Org buffer."
  (unless (derived-mode-p 'org-mode)
    (user-error "Org comments only work in Org buffers"))
  (or (hub/org-comment-collect (current-buffer))
      (user-error "No comments in this buffer")))

(defun hub/org-comment--target-start (comment)
  "Return COMMENT target start position."
  (or (plist-get comment :target-start)
      (plist-get comment :jump-pos)))

(defun hub/org-comment--stale-comments ()
  "Return stale sidecar comments for the current Org buffer."
  (cl-remove-if-not
   (lambda (comment)
     (eq (plist-get comment :anchor-state) 'stale))
   (or (hub/org-comment-collect (current-buffer) t)
       (user-error "No comments in this buffer"))))

(defvar hub/org-comment-reanchor-history nil
  "Minibuffer history for stale comment reanchoring.")

(defcustom hub/org-comment-picker-preview-length 72
  "Maximum normalized text length in comment picker candidates."
  :type 'natnum
  :group 'hub/org-comments)

(defun hub/org-comment--picker-preview (text)
  "Return TEXT normalized and truncated for picker display."
  (let ((preview (hub/org-comment-normalize-target-text (or text ""))))
    (if (> (length preview) hub/org-comment-picker-preview-length)
	(concat (substring preview 0 hub/org-comment-picker-preview-length) "…")
      preview)))

(defun hub/org-comment--completion-candidate (comment)
  "Return a readable completion candidate for COMMENT."
  (let ((status (or (plist-get comment :status) "OPEN"))
	(target (hub/org-comment--picker-preview (plist-get comment :target-text)))
	(body (hub/org-comment--picker-preview (plist-get comment :body))))
    (string-join
     (delq nil
	   (list status
		 (unless (string-empty-p target)
		   (format "“%s”" target))
		 (unless (string-empty-p body)
		   (format "— %s" body))))
     " ")))

(defun hub/org-comment--completion-table (comments)
  "Return a completion table for stale COMMENTS."
  (let* ((entries (mapcar (lambda (comment)
			    (cons (hub/org-comment--completion-candidate comment) comment))
			  comments))
	 (candidates (mapcar #'car entries)))
    (lambda (string predicate action)
      (if (eq action 'metadata)
	  `(metadata (category . hub-org-comment))
	(complete-with-action action candidates string predicate)))))

(defun hub/org-comment--read-stale-comment ()
  "Prompt for a stale sidecar comment in the current Org buffer."
  (let ((comments (hub/org-comment--stale-comments)))
    (pcase (length comments)
      (0 (user-error "No stale comments in this buffer"))
      (1 (car comments))
      (_ (let* ((entries (mapcar (lambda (comment)
				   (cons (hub/org-comment--completion-candidate comment) comment))
				 comments))
		(table (hub/org-comment--completion-table comments))
		(choice (completing-read
			 "Reanchor stale comment: "
			 table nil t nil 'hub/org-comment-reanchor-history)))
	   (or (alist-get choice entries nil nil #'equal)
	       (user-error "No stale comment selected")))))))

(defun hub/org-comment--defer-region-command (command start end)
  "Run COMMAND with START and END after Evil visual state has unwound."
  (let ((buffer (current-buffer)))
    (when (and (fboundp 'evil-visual-state-p)
	       (evil-visual-state-p)
	       (fboundp 'evil-exit-visual-state))
      (evil-exit-visual-state))
    (deactivate-mark)
    (run-at-time
     0 nil
     (lambda ()
       (when (buffer-live-p buffer)
	 (with-current-buffer buffer
	   (funcall command start end)))))))

(defun hub/org-comment--page-marker-record ()
  "Return a pseudo comment record for the page-comments marker, or nil."
  (when (hub/org-comment-collect-page (current-buffer))
    (when-let* ((position (hub/org-context-panel-page-comment-marker-position)))
      (list :type 'page-marker
	    :target-start position))))

(defun hub/org-comment--navigation-comments ()
  "Return navigable comments including the page-comments pseudo marker."
  (unless (derived-mode-p 'org-mode)
    (user-error "Org comments only work in Org buffers"))
  (let ((comments (sort (delq nil (cons (hub/org-comment--page-marker-record)
					(hub/org-comment-collect (current-buffer))))
			(lambda (left right)
			  (< (hub/org-comment--target-start left)
			     (hub/org-comment--target-start right))))))
    (or comments
	(user-error "No comments in this buffer"))))

(defun hub/org-comment--goto (comment)
  "Move point to COMMENT and open the context panel."
  (goto-char (hub/org-comment--target-start comment))
  (hub/org-comment-overlays-refresh)
  (unless (eq (plist-get comment :type) 'page-marker)
    (hub/org-context-panel-open)))

(defun hub/org-comment--active-at-point ()
  "Return the sidecar comment active at point, or signal a user error."
  (let ((point (point)))
    (or (cl-find-if
	 (lambda (comment)
	   (let ((start (plist-get comment :target-start))
		 (end (plist-get comment :target-end)))
	     (and start end (<= start point) (<= point end))))
	 (hub/org-comment--valid-comments))
	(user-error "Point is not inside a commented region"))))

(defun hub/org-comment--sidecar-comment-at-point ()
  "Return root sidecar comment metadata at point, or nil."
  (when (and (derived-mode-p 'org-mode)
	     buffer-file-name
	     (not (org-before-first-heading-p)))
    (save-excursion
      (org-back-to-heading t)
      (while (and (equal "reply" (org-entry-get nil "HUB_COMMENT_SYNC_KIND"))
		  (org-up-heading-safe)))
      (when-let* ((id (org-entry-get nil "HUB_COMMENT_ID")))
	(list :id id :sidecar-file buffer-file-name)))))

(defun hub/org-comment--comment-to-delete-at-point ()
  "Return comment metadata suitable for deletion at point."
  (or (hub/org-comment--sidecar-comment-at-point)
      (hub/org-comment--active-at-point)))

(defun hub/org-comment--goto-sidecar-heading (comment)
  "Open COMMENT sidecar and move point to its heading."
  (let ((sidecar-file (plist-get comment :sidecar-file))
	(id (plist-get comment :id)))
    (unless (and sidecar-file id)
      (user-error "Comment record is missing sidecar metadata"))
    (find-file sidecar-file)
    (org-mode)
    (hub/org-comment-fold-sidecar-property-drawers)
    (goto-char (point-min))
    (unless (cl-loop while (re-search-forward org-heading-regexp nil t)
		     do (goto-char (match-beginning 0))
		     when (equal id (org-entry-get nil "HUB_COMMENT_ID"))
		     return t
		     do (forward-line 1))
      (user-error "Comment %s not found in sidecar" id))))

(defun hub/org-comment--goto-body-from-heading ()
  "Move point from the current Org heading to its comment body."
  (let ((subtree-end (save-excursion (org-end-of-subtree t t))))
    (forward-line 1)
    (when (looking-at-p "[[:space:]]*:PROPERTIES:[[:space:]]*$")
      (when (re-search-forward "^[[:space:]]*:END:[[:space:]]*$" subtree-end t)
	(forward-line 1)))
    (while (and (< (point) subtree-end)
		(looking-at-p "\n"))
      (forward-char 1))))

(defun hub/org-comment--goto-sidecar-body (comment)
  "Open COMMENT sidecar and move point to its body."
  (hub/org-comment--goto-sidecar-heading comment)
  (hub/org-comment--goto-body-from-heading))

(defun hub/org-comment--set-sidecar-status (comment status)
  "Set COMMENT sidecar heading TODO keyword to STATUS."
  (let ((sidecar-file (plist-get comment :sidecar-file))
	(id (plist-get comment :id)))
    (unless (and sidecar-file id)
      (user-error "Comment record is missing sidecar metadata"))
    (with-temp-buffer
      (insert-file-contents sidecar-file)
      (org-mode)
      (goto-char (point-min))
      (unless (cl-loop while (re-search-forward org-heading-regexp nil t)
		       do (goto-char (match-beginning 0))
		       when (equal id (org-entry-get nil "HUB_COMMENT_ID"))
		       return (progn (org-todo status) t)
		       do (forward-line 1))
	(user-error "Comment %s not found in sidecar" id))
      (write-region (point-min) (point-max) sidecar-file nil 'silent))))

(defun hub/org-comment--status-target-at-point ()
  "Return comment metadata suitable for a status change at point."
  (or (hub/org-comment--sidecar-comment-at-point)
      (hub/org-comment--active-at-point)))

(defun hub/org-comment-set-status (status)
  "Set the source or sidecar comment at point to STATUS."
  (let ((comment (hub/org-comment--status-target-at-point)))
    (hub/org-comment--set-sidecar-status comment status)
    (if (and buffer-file-name
	     (string-suffix-p ".comments.org" buffer-file-name))
	(revert-buffer :ignore-auto :noconfirm)
      (hub/org-comment-overlays-refresh)
      (hub/org-context-panel-open))))

;;;###autoload
(defun hub/org-comment-next ()
  "Jump to the next sidecar comment in the current Org buffer."
  (interactive)
  (let* ((point (point))
	 (comments (hub/org-comment--navigation-comments))
	 (next (or (cl-find-if
		    (lambda (comment)
		      (> (hub/org-comment--target-start comment) point))
		    comments)
		   (car comments))))
    (hub/org-comment--goto next)))

;;;###autoload
(defun hub/org-comment-previous ()
  "Jump to the previous sidecar comment in the current Org buffer."
  (interactive)
  (let* ((point (point))
	 (comments (reverse (hub/org-comment--navigation-comments)))
	 (previous (or (cl-find-if
			(lambda (comment)
			  (< (hub/org-comment--target-start comment) point))
			comments)
		       (car comments))))
    (hub/org-comment--goto previous)))

;;;###autoload
(defun hub/org-comment-jump-to-sidecar ()
  "Jump to the active sidecar comment heading."
  (interactive)
  (hub/org-comment--goto-sidecar-heading (hub/org-comment--active-at-point)))

;;;###autoload
(defun hub/org-comment-open-sidecar ()
  "Open the current Org file's sidecar comments file when it exists."
  (interactive)
  (let ((sidecar-file (hub/org-comment-sidecar-path)))
    (if (file-exists-p sidecar-file)
	(progn
	  (find-file sidecar-file)
	  (hub/org-comment-fold-sidecar-property-drawers))
      (message "No sidecar comments file: %s" sidecar-file))))

;;;###autoload
(defun hub/org-comment-refresh-sidecar-headings-command ()
  "Refresh headings in the current Org sidecar comments file."
  (interactive)
  (let ((sidecar-file (if (and buffer-file-name
			       (string-suffix-p ".comments.org" buffer-file-name))
			  buffer-file-name
			(hub/org-comment-sidecar-path))))
    (unless (file-exists-p sidecar-file)
      (user-error "No sidecar comments file: %s" sidecar-file))
    (let ((count (hub/org-comment-refresh-sidecar-headings sidecar-file)))
      (when (and buffer-file-name (file-equal-p buffer-file-name sidecar-file))
	(revert-buffer :ignore-auto :noconfirm))
      (message "Refreshed %s sidecar comment heading%s"
	       count (if (= count 1) "" "s"))
      count)))

;;;###autoload
(defun hub/org-comment-anchor-imported-inline-comments ()
  "Anchor imported Confluence inline comments for the current Org file."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Org comments only work in Org buffers"))
  (let* ((summary (hub/org-comment--anchor-imported-inline-comments (current-buffer)))
	 (sidecar-file (hub/org-comment-sidecar-path))
	 (anchored (plist-get summary :anchored))
	 (missing (plist-get summary :missing))
	 (ambiguous (plist-get summary :ambiguous)))
    (hub/org-comment-refresh-sidecar-headings sidecar-file)
    (hub/org-comment-overlays-refresh)
    (when (get-buffer-window hub/org-context-panel-buffer-name t)
      (ignore-errors (hub/org-context-panel-open)))
    (message "Anchored %s inline comments; %s missing; %s ambiguous"
	     anchored missing ambiguous)
    summary))

;;;###autoload
(defun hub/org-comment-reanchor-imported-inline-comments ()
  "Batch reanchor imported Confluence inline comments with completion triage."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Org comments only work in Org buffers"))
  (let* ((sidecar-file (hub/org-comment-sidecar-path))
	 (exact (hub/org-comment--anchor-imported-inline-comments (current-buffer) sidecar-file))
	 (triage (hub/org-comment--triage-imported-inline-comments (current-buffer) sidecar-file)))
    (hub/org-comment-refresh-sidecar-headings sidecar-file)
    (hub/org-comment-overlays-refresh)
    (when (get-buffer-window hub/org-context-panel-buffer-name t)
      (ignore-errors (hub/org-context-panel-open)))
    (message "Anchored %s exact, %s selected, %s skipped"
	     (plist-get exact :anchored)
	     (plist-get triage :selected)
	     (plist-get triage :skipped))
    (append exact triage)))

;;;###autoload
(defun hub/org-comment-delete ()
  "Delete the comment at point after confirmation."
  (interactive)
  (let* ((comment (hub/org-comment--comment-to-delete-at-point))
	 (id (plist-get comment :id))
	 (sidecar-file (plist-get comment :sidecar-file)))
    (unless (and id sidecar-file)
      (user-error "No sidecar comment at point"))
    (when (yes-or-no-p (format "Delete comment %s? " id))
      (hub/org-comment-delete-entry sidecar-file id)
      (when (and buffer-file-name
		 (file-equal-p buffer-file-name sidecar-file))
	(revert-buffer :ignore-auto :noconfirm))
      (when (derived-mode-p 'org-mode)
	(ignore-errors (hub/org-comment-overlays-refresh))
	(when (get-buffer-window hub/org-context-panel-buffer-name t)
	  (ignore-errors (hub/org-context-panel-open))))
      (message "Deleted comment %s" id))))

;;;###autoload
(defun hub/org-comment-reanchor (start end &optional comment)
  "Reanchor stale COMMENT to source region START to END."
  (interactive "r")
  (unless (derived-mode-p 'org-mode)
    (user-error "Org comments only work in Org buffers"))
  (let* ((source-buffer (current-buffer))
	 (target-comment (or comment (hub/org-comment--read-stale-comment)))
	 (record (hub/org-comment-create-record
		  buffer-file-name start end "" (plist-get target-comment :id))))
    (hub/org-comment-update-anchor
     (plist-get target-comment :sidecar-file)
     (plist-get target-comment :id)
     record)
    (with-current-buffer source-buffer
      (goto-char start)
      (hub/org-comment-overlays-refresh)
      (hub/org-context-panel-open))))

;;;###autoload
(defun hub/org-comment-reanchor-from-region ()
  "Defer stale comment reanchoring for the active visual region."
  (interactive)
  (let ((bounds (hub/org-comment--region-bounds)))
    (hub/org-comment--defer-region-command
     #'hub/org-comment-reanchor (car bounds) (cdr bounds))))

;;;###autoload
(defun hub/org-comment-edit ()
  "Edit the active sidecar comment body narrowed to its subtree."
  (interactive)
  (let ((comment (hub/org-comment--active-at-point)))
    (hub/org-comment--goto-sidecar-heading comment)
    (org-narrow-to-subtree)
    (goto-char (point-min))
    (hub/org-comment--goto-body-from-heading)))

;;;###autoload
(defun hub/org-comment-mark-open ()
  "Mark the active sidecar comment at point as open."
  (interactive)
  (hub/org-comment-set-status "OPEN"))

;;;###autoload
(defun hub/org-comment-mark-todo ()
  "Mark the active sidecar comment at point as requiring action."
  (interactive)
  (hub/org-comment-set-status "TODO"))

;;;###autoload
(defun hub/org-comment-mark-resolved ()
  "Mark the active sidecar comment at point as resolved."
  (interactive)
  (hub/org-comment-set-status "RESOLVED"))

;;;###autoload
(defun hub/org-comment-cycle-status ()
  "Cycle the active sidecar comment status through OPEN, TODO, and RESOLVED."
  (interactive)
  (let* ((comment (hub/org-comment--status-target-at-point))
	 (status (or (plist-get comment :status)
		     (save-excursion
		       (when (hub/org-comment--sidecar-comment-at-point)
			 (org-back-to-heading t)
			 (org-get-todo-state)))))
	 (next (pcase status
		 ("OPEN" "TODO")
		 ("TODO" "RESOLVED")
		 (_ "OPEN"))))
    (hub/org-comment--set-sidecar-status comment next)
    (if (and buffer-file-name
	     (string-suffix-p ".comments.org" buffer-file-name))
	(revert-buffer :ignore-auto :noconfirm)
      (hub/org-comment-overlays-refresh)
      (hub/org-context-panel-open))))

;;;###autoload
(defun hub/org-comment-create (start end &optional body)
  "Create a sidecar comment for region START to END with BODY."
  (interactive "r")
  (unless (derived-mode-p 'org-mode)
    (user-error "Org comments only work in Org buffers"))
  (unless buffer-file-name
    (user-error "Current buffer is not visiting a file"))
  (let* ((source-buffer (current-buffer))
	 (record (hub/org-comment-create-record buffer-file-name start end (or body "")))
	 (sidecar-file (hub/org-comment-append-to-sidecar record)))
    (with-current-buffer source-buffer
      (goto-char start)
      (hub/org-comment-overlays-refresh))
    (hub/org-comment--goto-sidecar-body
     (list :id (plist-get record :id)
	   :sidecar-file sidecar-file))))

(defun hub/org-page-comment--create-record (source-file &optional body id author created-at)
  "Return a page-level sidecar comment record for SOURCE-FILE.
BODY defaults to an empty string.  ID, AUTHOR, and CREATED-AT default to local
comment metadata."
  (list :id (or id (hub/org-comment-generate-id))
	:status "OPEN"
	:source-file source-file
	:author (or author (hub/org-comment-current-author))
	:created-at (or created-at (hub/org-comment-current-created-at))
	:sync-kind "footer"
	:body (or body "")))

(defun hub/org-page-comment--format-entry (record sidecar-file)
  "Return sidecar Org entry text for page comment RECORD in SIDECAR-FILE."
  (let ((title (hub/org-comment-heading-title record (file-name-directory sidecar-file))))
    (concat
     (format "* %s %s\n" (or (plist-get record :status) "OPEN") title)
     ":PROPERTIES:\n"
     (hub/org-comment--property-line "HUB_COMMENT_ID" (plist-get record :id))
     (hub/org-comment--property-line "HUB_COMMENT_AUTHOR" (plist-get record :author))
     (hub/org-comment--property-line "HUB_COMMENT_CREATED_AT" (plist-get record :created-at))
     (hub/org-comment--property-line "HUB_COMMENT_SYNC_KIND" "footer")
     ":END:\n\n"
     (string-trim-right (or (plist-get record :body) ""))
     "\n")))

(defun hub/org-page-comment--append-to-sidecar (record &optional sidecar-file)
  "Append page comment RECORD to SIDECAR-FILE and return SIDECAR-FILE."
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
      (insert (hub/org-page-comment--format-entry record target-file))
      (write-region (point-min) (point-max) target-file nil 'silent))
    target-file))

;;;###autoload
(defun hub/org-page-comment-create (&optional body)
  "Create a page/footer sidecar comment with BODY and edit it."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Page comments only work in Org buffers"))
  (unless buffer-file-name
    (user-error "Current buffer is not visiting a file"))
  (let* ((source-buffer (current-buffer))
	 (record (hub/org-page-comment--create-record buffer-file-name body))
	 (sidecar-file (hub/org-page-comment--append-to-sidecar record)))
    (with-current-buffer source-buffer
      (hub/org-comment-overlays-refresh))
    (hub/org-comment--goto-sidecar-body
     (list :id (plist-get record :id)
	   :sidecar-file sidecar-file))))

(defun hub/org-comment--reply-root-at-point ()
  "Move to the root sidecar comment for point and return its metadata."
  (unless (and (derived-mode-p 'org-mode)
	       buffer-file-name
	       (string-suffix-p ".comments.org" buffer-file-name)
	       (not (org-before-first-heading-p)))
    (user-error "Point is not on a sidecar comment"))
  (org-back-to-heading t)
  (while (and (> (org-outline-level) 1)
	      (org-up-heading-safe)))
  (let ((id (org-entry-get nil "HUB_COMMENT_ID"))
	(remote-id (org-entry-get nil "HUB_COMMENT_REMOTE_ID")))
    (unless id
      (user-error "Point is not on a sidecar comment"))
    (unless remote-id
      (user-error "Cannot create a Confluence reply before the root comment is remote-linked"))
    (list :id id :remote-id remote-id :sidecar-file buffer-file-name)))

(defun hub/org-comment--reply-root-from-context ()
  "Return root sidecar comment metadata for the current source or sidecar context."
  (if (and buffer-file-name (string-suffix-p ".comments.org" buffer-file-name))
      (save-excursion (hub/org-comment--reply-root-at-point))
    (let ((comment (hub/org-comment--active-at-point)))
      (unless (plist-get comment :remote-id)
	(user-error "Cannot create a Confluence reply before the comment is remote-linked"))
      (list :id (plist-get comment :id)
	    :remote-id (plist-get comment :remote-id)
	    :sidecar-file (plist-get comment :sidecar-file)))))

(defun hub/org-comment--format-reply-entry (record directory)
  "Return sidecar Org text for local reply RECORD using DIRECTORY."
  (concat
   (format "** %s\n" (hub/org-comment-reply-heading-title record directory))
   ":PROPERTIES:\n"
   (hub/org-comment--property-line "HUB_COMMENT_ID" (plist-get record :id))
   (hub/org-comment--property-line "HUB_COMMENT_SYNC_KIND" "reply")
   (hub/org-comment--property-line "HUB_COMMENT_REMOTE_PARENT_ID" (plist-get record :remote-parent-id))
   (hub/org-comment--property-line "HUB_COMMENT_AUTHOR" (plist-get record :author))
   (hub/org-comment--property-line "HUB_COMMENT_CREATED_AT" (plist-get record :created-at))
   ":END:\n\n"
   (string-trim-right (or (plist-get record :body) ""))
   "\n"))

;;;###autoload
(defun hub/org-comment-reply-create (&optional body)
  "Create a local reply under the active remote-linked sidecar comment.
BODY defaults to an empty string.  The reply is not pushed automatically."
  (interactive)
  (unless (or (and buffer-file-name (string-suffix-p ".comments.org" buffer-file-name))
	      (derived-mode-p 'org-mode))
    (user-error "Org comment replies only work in Org buffers or comment sidecars"))
  (let* ((root (hub/org-comment--reply-root-from-context))
	 (sidecar-file (plist-get root :sidecar-file))
	 (record (list :id (hub/org-comment-generate-id)
		       :remote-parent-id (plist-get root :remote-id)
		       :author (hub/org-comment-current-author)
		       :created-at (hub/org-comment-current-created-at)
		       :body (or body "")))
	 (reply-id (plist-get record :id)))
    (hub/org-comment--goto-sidecar-heading root)
    (let ((root-level (org-outline-level))
	  (insert-at (save-excursion (org-end-of-subtree t t))))
      (goto-char insert-at)
      (unless (bolp) (insert "\n"))
      (unless (save-excursion
		(forward-line -1)
		(looking-at-p "[[:space:]]*$"))
	(insert "\n"))
      (insert (hub/org-comment--format-reply-entry
	       record (file-name-directory sidecar-file)))
      (save-buffer)
      (hub/org-comment-refresh-sidecar-headings sidecar-file)
      (hub/org-comment--goto-sidecar-heading
       (list :id reply-id :sidecar-file sidecar-file))
      (unless (= (org-outline-level) (1+ root-level))
	(user-error "Failed to create reply under root comment"))
      (hub/org-comment--goto-body-from-heading))))

;;;###autoload
(defun hub/org-page-comments-open ()
  "Open the bottom page-context window for the current Org buffer."
  (interactive)
  (hub/org-page-context-open t t))

;;;###autoload
(defun hub/org-comment-create-from-region ()
  "Defer sidecar comment creation for the active visual region."
  (interactive)
  (let ((bounds (hub/org-comment--region-bounds)))
    (hub/org-comment--defer-region-command
     #'hub/org-comment-create (car bounds) (cdr bounds))))

(provide 'org/comments)
;;; comments.el ends here
