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

(defcustom hub/org-page-comments-buffer-name "*Org Page Comments*"
  "Buffer name used for the page comments bottom window."
  :type 'string
  :group 'hub/org-comments)

(defvar-local hub/org-page-comments-source-buffer nil
  "Source Org buffer rendered by the current page comments buffer.")

(defvar hub/org-page-comments-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'hub/org-page-comments-jump)
    (define-key map (kbd "e") #'hub/org-page-comments-edit)
    (define-key map (kbd "x") #'hub/org-page-comments-delete)
    (define-key map (kbd "]c") #'hub/org-page-comments-next)
    (define-key map (kbd "[c") #'hub/org-page-comments-previous)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for page comments buffers.")

(define-derived-mode hub/org-page-comments-mode org-mode "Org-Page-Comments"
  "Major mode for read-only page comments buffers."
  (setq-local truncate-lines nil
	      word-wrap t)
  (visual-line-mode 1))

(with-eval-after-load 'evil
  (evil-define-key 'normal hub/org-page-comments-mode-map
		   (kbd "RET") #'hub/org-page-comments-jump
		   (kbd "e") #'hub/org-page-comments-edit
		   (kbd "x") #'hub/org-page-comments-delete
		   (kbd "]c") #'hub/org-page-comments-next
		   (kbd "[c") #'hub/org-page-comments-previous
		   (kbd "q") #'quit-window))

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

(defun hub/org-comment-set-status (status)
  "Set the active comment at point to STATUS."
  (let ((comment (hub/org-comment--active-at-point)))
    (hub/org-comment--set-sidecar-status comment status)
    (hub/org-comment-overlays-refresh)
    (hub/org-context-panel-open)))

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
  (let* ((comment (hub/org-comment--active-at-point))
	 (status (plist-get comment :status))
	 (next (pcase status
		 ("OPEN" "TODO")
		 ("TODO" "RESOLVED")
		 (_ "OPEN"))))
    (hub/org-comment--set-sidecar-status comment next)
    (hub/org-comment-overlays-refresh)
    (hub/org-context-panel-open)))

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

(defun hub/org-page-comments--comment-at-point ()
  "Return page comment item at point in the page comments buffer."
  (or (get-text-property (point) 'hub-org-page-comment)
      (get-text-property (max (point-min) (1- (point))) 'hub-org-page-comment)
      (user-error "No page comment at point")))

(defun hub/org-page-comments--item-starts ()
  "Return positions where page comment items start."
  (let ((position (point-min))
	starts)
    (while (< position (point-max))
      (let ((item (get-text-property position 'hub-org-page-comment)))
	(when (and item
		   (or (= position (point-min))
		       (not (equal item (get-text-property (1- position)
							   'hub-org-page-comment)))))
	  (push position starts))
	(setq position (or (next-single-property-change
			    position 'hub-org-page-comment nil (point-max))
			   (point-max)))))
    (nreverse starts)))

(defun hub/org-page-comments-next ()
  "Move point to the next page comment, wrapping at the end."
  (interactive)
  (let* ((starts (hub/org-page-comments--item-starts))
	 (next (or (cl-find-if (lambda (position) (> position (point))) starts)
		   (car starts))))
    (unless next
      (user-error "No page comments"))
    (goto-char next)))

(defun hub/org-page-comments-previous ()
  "Move point to the previous page comment, wrapping at the beginning."
  (interactive)
  (let* ((starts (reverse (hub/org-page-comments--item-starts)))
	 (previous (or (cl-find-if (lambda (position) (< position (point))) starts)
		       (car starts))))
    (unless previous
      (user-error "No page comments"))
    (goto-char previous)))

(defun hub/org-page-comments--source-directory ()
  "Return directory for the current page comments source buffer, or nil."
  (when-let* ((source hub/org-page-comments-source-buffer)
	      (file (buffer-file-name source)))
    (file-name-directory file)))

(defun hub/org-page-comments--comment-author (comment)
  "Return display author for COMMENT."
  (let ((author (plist-get comment :author))
	(remote-author-id (plist-get comment :remote-author-id)))
    (or (hub/confluence-people-resolve-account-id
	 (or remote-author-id author)
	 (hub/org-page-comments--source-directory))
	author)))

(defun hub/org-page-comments--format-created-at (created-at)
  "Return compact display text for CREATED-AT."
  (if-let* ((time (ignore-errors (date-to-time created-at))))
      (format-time-string "%Y-%m-%d %H:%M" time)
    created-at))

(defun hub/org-page-comments--readable-body (comment)
  "Return readable projection of COMMENT body."
  (let ((body (or (plist-get comment :body) "")))
    (require 'org-confluence-commands nil 'noerror)
    (or (when (fboundp 'hub/confluence-import-storage-to-org)
	  (ignore-errors (hub/confluence-import-storage-to-org body)))
	(hub/org-comment--preview-plain-text body))))

(defun hub/org-page-comments--insert-metadata-line (comment &optional prefix)
  "Insert metadata line for COMMENT with optional PREFIX."
  (let ((author (hub/org-page-comments--comment-author comment))
	(created-at (plist-get comment :created-at)))
    (insert (or prefix ""))
    (when (or author created-at)
      (insert
       (string-join
	(delq nil (list author
			(when created-at
			  (hub/org-page-comments--format-created-at created-at))))
	" · ")))))

(defun hub/org-page-comments--apply-list-wrap-prefix (start end)
  "Indent wrapped list continuation lines between START and END by two spaces."
  (save-excursion
    (goto-char start)
    (while (< (point) end)
      (when (looking-at-p "[[:space:]]*\\([-+*]\\|[0-9]+[.)]\\)[[:space:]]+")
	(add-text-properties (line-beginning-position) (line-end-position)
			     '(wrap-prefix "  ")))
      (forward-line 1))))

(defun hub/org-page-comments--insert-readable-body (body)
  "Insert readable comment BODY with wrapping hints."
  (let ((start (point)))
    (insert body "\n")
    (hub/org-page-comments--apply-list-wrap-prefix start (point))))

(defun hub/org-page-comments--insert-comment (comment)
  "Insert rendered COMMENT in the current page comments buffer."
  (let ((start (point))
	(status (or (plist-get comment :status) "OPEN")))
    (insert (propertize (upcase status) 'face 'bold))
    (when (or (plist-get comment :author) (plist-get comment :created-at))
      (insert " · ")
      (hub/org-page-comments--insert-metadata-line comment))
    (insert "\n\n")
    (hub/org-page-comments--insert-readable-body
     (hub/org-page-comments--readable-body comment))
    (dolist (reply (plist-get comment :replies))
      (insert "\n↳ ")
      (hub/org-page-comments--insert-metadata-line reply)
      (insert "\n")
      (hub/org-page-comments--insert-readable-body
       (hub/org-page-comments--readable-body reply)))
    (add-text-properties start (point) `(hub-org-page-comment ,comment))
    (insert "\n")))

(defun hub/org-page-comments-render-buffer (source-buffer page-buffer)
  "Render SOURCE-BUFFER page comments into PAGE-BUFFER."
  (let ((comments (with-current-buffer source-buffer
		    (hub/org-comment-collect-page source-buffer))))
    (with-current-buffer page-buffer
      (let ((inhibit-read-only t))
	(hub/org-page-comments-mode)
	(setq-local hub/org-page-comments-source-buffer source-buffer)
	(erase-buffer)
	(insert (format "PAGE comments for %s\n\n"
			(or (buffer-file-name source-buffer)
			    (buffer-name source-buffer))))
	(if comments
	    (dolist (comment comments)
	      (hub/org-page-comments--insert-comment comment))
	  (insert "No page comments.\n"))
	(goto-char (point-min))
	(setq buffer-read-only t)))
    page-buffer))

(defun hub/org-page-comments--display-below-source (source-window page-buffer)
  "Display PAGE-BUFFER below SOURCE-WINDOW and return the new window."
  (let* ((source-height (window-total-height source-window))
	 (height (max 8 (floor (* source-height 0.33))))
	 (page-window (split-window source-window (- height) 'below)))
    (set-window-buffer page-window page-buffer)
    page-window))

;;;###autoload
(defun hub/org-page-comments-open ()
  "Open a bottom window listing current Org page comments."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Page comments only work in Org buffers"))
  (let* ((source-buffer (current-buffer))
	 (source-window (or (get-buffer-window source-buffer t)
			    (selected-window)))
	 (page-buffer (get-buffer-create hub/org-page-comments-buffer-name)))
    (hub/org-page-comments-render-buffer source-buffer page-buffer)
    (hub/org-page-comments--display-below-source source-window page-buffer)))

(defun hub/org-page-comments-jump ()
  "Jump to the sidecar heading for the page comment at point."
  (interactive)
  (hub/org-comment--goto-sidecar-heading (hub/org-page-comments--comment-at-point)))

(defun hub/org-page-comments-edit ()
  "Edit the sidecar body for the page comment at point."
  (interactive)
  (let ((comment (hub/org-page-comments--comment-at-point)))
    (hub/org-comment--goto-sidecar-heading comment)
    (org-narrow-to-subtree)
    (goto-char (point-min))
    (hub/org-comment--goto-body-from-heading)))

(defun hub/org-page-comments-delete ()
  "Delete the page comment at point after confirmation."
  (interactive)
  (let* ((comment (hub/org-page-comments--comment-at-point))
	 (id (plist-get comment :id))
	 (sidecar-file (plist-get comment :sidecar-file))
	 (source hub/org-page-comments-source-buffer))
    (unless (and id sidecar-file)
      (user-error "Page comment is missing sidecar metadata"))
    (when (yes-or-no-p (format "Delete comment %s? " id))
      (hub/org-comment-delete-entry sidecar-file id)
      (when (buffer-live-p source)
	(with-current-buffer source
	  (hub/org-comment-overlays-refresh)))
      (hub/org-page-comments-render-buffer source (current-buffer))
      (message "Deleted comment %s" id))))

;;;###autoload
(defun hub/org-comment-create-from-region ()
  "Defer sidecar comment creation for the active visual region."
  (interactive)
  (let ((bounds (hub/org-comment--region-bounds)))
    (hub/org-comment--defer-region-command
     #'hub/org-comment-create (car bounds) (cdr bounds))))

(provide 'org/comments)
;;; comments.el ends here
