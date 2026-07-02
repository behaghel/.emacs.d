;;; org-comments-compose.el --- Simple composition for Org comments -*- lexical-binding: t; -*-

;;; Commentary:
;; Package-owned composition helpers for Org comments.  The default
;; `org-comments-compose-reply' remains minibuffer-based; richer callers can use
;; the compose-buffer commands in this file.

;;; Code:

(require 'org)
(require 'org-comments-core)
(require 'org-comments-model)
(require 'org-comments-sidecar)
(require 'org-comments-store)
(require 'subr-x)

(defvar-local org-comments-compose-context nil
  "Plist describing the active Org comment compose operation.")

(defvar-local org-comments-compose-window nil
  "Window currently displaying this compose buffer.")

(defvar-local org-comments-compose-closing nil
  "Non-nil while closing a compose buffer intentionally.")

(defvar org-comments-compose-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'org-comments-compose-submit)
    (define-key map (kbd "C-c C-s") #'org-comments-compose-save-draft)
    (define-key map (kbd "C-c C-k") #'org-comments-compose-cancel)
    map)
  "Keymap for `org-comments-compose-mode'.")

(define-minor-mode org-comments-compose-mode
  "Minor mode for composing Org sidecar comments without visiting sidecars."
  :lighter " CommentCompose"
  :keymap org-comments-compose-mode-map)

(defun org-comments-compose--reply-record (comment body)
  "Return a reply record for COMMENT with BODY."
  (list :id (org-comments-generate-id)
	:status "OPEN"
	:sidecar-file (plist-get comment :sidecar-file)
	:author (org-comments-current-author)
	:created-at (org-comments-current-created-at)
	:sync-kind "reply"
	:remote-parent-id (plist-get comment :remote-id)
	:body body))

(defun org-comments-compose--format-reply-entry (record directory)
  "Return an Org subtree fragment for reply RECORD using DIRECTORY."
  (concat
   (format "** %s %s\n"
	   (or (plist-get record :status) "OPEN")
	   (org-comments-reply-heading-title record directory))
   ":PROPERTIES:\n"
   (org-comments--property-line "ORG_COMMENTS_ID" (plist-get record :id))
   (org-comments--property-line "ORG_COMMENTS_AUTHOR" (plist-get record :author))
   (org-comments--property-line "ORG_COMMENTS_CREATED_AT" (plist-get record :created-at))
   (org-comments--property-line "ORG_COMMENTS_SYNC_KIND" "reply")
   (org-comments--property-line "ORG_COMMENTS_REMOTE_PARENT_ID"
				(plist-get record :remote-parent-id))
   ":END:\n\n"
   (string-trim-right (or (plist-get record :body) ""))
   "\n"))

(defun org-comments-compose--goto-sidecar-heading (item)
  "Move to sidecar heading for ITEM in the current buffer."
  (unless (org-comments-goto-id (plist-get item :id))
    (user-error "Comment %s not found in sidecar" (plist-get item :id))))

(defun org-comments-compose--root-item (item sidecar)
  "Return root item for ITEM in SIDECAR."
  (with-temp-buffer
    (insert-file-contents sidecar)
    (org-mode)
    (org-comments-compose--goto-sidecar-heading item)
    (while (and (> (org-outline-level) 1) (org-up-heading-safe)))
    (list :id (org-entry-get nil "ORG_COMMENTS_ID")
	  :remote-id (org-entry-get nil "ORG_COMMENTS_REMOTE_ID"))))

(defun org-comments-compose--append-reply (comment record)
  "Append reply RECORD under COMMENT in its sidecar and return RECORD."
  (let ((sidecar-file (plist-get comment :sidecar-file))
	(comment-id (plist-get comment :id)))
    (unless (and sidecar-file comment-id)
      (user-error "Comment has no sidecar location"))
    (with-temp-buffer
      (insert-file-contents sidecar-file)
      (org-mode)
      (goto-char (point-min))
      (unless (org-comments-goto-id comment-id)
	(user-error "Cannot find comment %s" comment-id))
      (when (org-comments--reply-heading-p)
	(user-error "Nested replies are not supported"))
      (goto-char (save-excursion (org-end-of-subtree t t)))
      (unless (bolp)
	(insert "\n"))
      (unless (save-excursion
		(forward-line -1)
		(looking-at-p "[[:space:]]*$"))
	(insert "\n"))
      (insert (org-comments-compose--format-reply-entry
	       record (file-name-directory sidecar-file)))
      (write-region (point-min) (point-max) sidecar-file nil 'silent))
    record))

(defun org-comments-compose--body ()
  "Return editable compose buffer body."
  (string-trim (buffer-substring-no-properties (point-min) (point-max))))

(defun org-comments-compose--display-buffer (buffer origin-window)
  "Display compose BUFFER below ORIGIN-WINDOW without replacing source text."
  (let* ((height (max 8 (floor (* (window-total-height origin-window) 0.33))))
	 (window (if (window-parameter origin-window 'window-side)
		     (display-buffer-in-side-window
		      buffer
		      `((side . bottom)
			(slot . 2)
			(window-height . ,height)))
		   (let ((target (split-window origin-window (- height) 'below)))
		     (set-window-buffer target buffer)
		     target))))
    (select-window window)
    window))

(defun org-comments-compose-open (operation item source-buffer origin-key &optional restore-function)
  "Open a compose buffer for OPERATION on comment ITEM.
SOURCE-BUFFER is the associated Org source buffer.  ORIGIN-KEY identifies the
panel row to restore.  RESTORE-FUNCTION is called as (FUNCTION ORIGIN-WINDOW
ORIGIN-KEY) when the compose buffer closes."
  (let ((origin-window (selected-window))
	(buffer (get-buffer-create (if (eq operation 'reply)
				       "*Org Comment Reply*"
				     "*Org Comment Edit*"))))
    (unless (buffer-live-p source-buffer)
      (user-error "No source buffer for this compose operation"))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
	(erase-buffer)
	(org-mode)
	(org-comments-compose-mode 1)
	(setq-local org-comments-compose-context
		    (list :operation operation
			  :item item
			  :source source-buffer
			  :origin-window origin-window
			  :origin-key origin-key
			  :restore-function restore-function))
	(setq-local header-line-format
		    (if (eq operation 'reply)
			"Reply · C-c C-c submit · C-c C-s save draft · C-c C-k cancel"
		      "Edit · C-c C-c save local edit and close · C-c C-s save and keep open · C-c C-k cancel"))
	(add-hook 'kill-buffer-query-functions
		  #'org-comments-compose--confirm-kill nil t)
	(when (eq operation 'edit)
	  (insert (org-comments-preview-plain-text (or (plist-get item :body) ""))))
	(set-buffer-modified-p nil)
	(goto-char (point-max))))
    (with-current-buffer buffer
      (setq-local org-comments-compose-window
		  (org-comments-compose--display-buffer buffer origin-window)))))

(defun org-comments-compose--restore-origin (context)
  "Restore point and focus to the panel described by CONTEXT."
  (let ((origin-window (plist-get context :origin-window))
	(origin-key (plist-get context :origin-key))
	(restore-function (plist-get context :restore-function)))
    (when (window-live-p origin-window)
      (when restore-function
	(funcall restore-function origin-window origin-key))
      (select-window origin-window))))

(defun org-comments-compose--confirm-discard ()
  "Return non-nil when compose changes may be discarded."
  (or (not (buffer-modified-p))
      (yes-or-no-p "Discard unsaved comment changes? ")))

(defun org-comments-compose--confirm-kill ()
  "Ask before killing a compose buffer with unsaved changes."
  (or org-comments-compose-closing
      (org-comments-compose--confirm-discard)))

(defun org-comments-compose--close ()
  "Close the current compose buffer and restore the invoking panel."
  (let ((buffer (current-buffer))
	(window org-comments-compose-window)
	(context org-comments-compose-context))
    (setq-local org-comments-compose-closing t)
    (when (and (window-live-p window)
	       (not (one-window-p t)))
      (delete-window window))
    (when (buffer-live-p buffer)
      (kill-buffer buffer))
    (org-comments-compose--restore-origin context)))

(defun org-comments-compose-cancel ()
  "Cancel the current Org comment compose buffer."
  (interactive)
  (unless org-comments-compose-context
    (user-error "No active comment compose operation"))
  (unless (org-comments-compose--confirm-discard)
    (user-error "Canceled"))
  (org-comments-compose--close))

(defun org-comments-compose--body-bounds ()
  "Return body bounds for the sidecar heading at point."
  (let* ((subtree-end (save-excursion (org-end-of-subtree t t)))
	 (body-start (save-excursion
		       (forward-line 1)
		       (when (looking-at-p "[[:space:]]*:PROPERTIES:[[:space:]]*$")
			 (re-search-forward "^[[:space:]]*:END:[[:space:]]*$" subtree-end t)
			 (forward-line 1))
		       (point)))
	 (body-end (save-excursion
		     (goto-char body-start)
		     (if (re-search-forward org-heading-regexp subtree-end t)
			 (match-beginning 0)
		       subtree-end))))
    (cons body-start body-end)))

(defun org-comments-compose-save-edit (item body)
  "Save BODY into existing sidecar ITEM."
  (let ((sidecar (plist-get item :sidecar-file)))
    (with-temp-buffer
      (insert-file-contents sidecar)
      (org-mode)
      (org-comments-compose--goto-sidecar-heading item)
      (let* ((bounds (org-comments-compose--body-bounds))
	     (old-body (string-trim
			(buffer-substring-no-properties (car bounds) (cdr bounds)))))
	(delete-region (car bounds) (cdr bounds))
	(goto-char (car bounds))
	(insert body "\n")
	(when (and (plist-get item :remote-id)
		   (not (string= old-body body)))
	  (save-excursion
	    (org-back-to-heading t)
	    (org-entry-put nil "ORG_COMMENTS_LOCAL_UPDATED_AT"
			   (org-comments-current-created-at)))))
      (write-region (point-min) (point-max) sidecar nil 'silent))
    (org-comments-refresh-sidecar-headings sidecar)))

(defun org-comments-compose-save-reply (item body)
  "Create a local reply to ITEM with BODY and return its sidecar record."
  (let* ((sidecar (plist-get item :sidecar-file))
	 (root (org-comments-compose--root-item item sidecar)))
    (unless (plist-get root :remote-id)
      (user-error "Cannot create a reply before the root comment is remote-linked"))
    (with-temp-buffer
      (insert-file-contents sidecar)
      (org-mode)
      (org-comments-compose--goto-sidecar-heading root)
      (let* ((record (org-comments-compose--reply-record root body))
	     (reply-id (plist-get record :id))
	     (insert-at (save-excursion (org-end-of-subtree t t))))
	(goto-char insert-at)
	(unless (bolp) (insert "\n"))
	(insert "\n" (org-comments-compose--format-reply-entry
		      record (file-name-directory sidecar)))
	(write-region (point-min) (point-max) sidecar nil 'silent)
	(org-comments-refresh-sidecar-headings sidecar)
	(list :type 'comment :id reply-id :sidecar-file sidecar)))))

(defun org-comments-compose--save-current (context body)
  "Save compose BODY according to CONTEXT and return the sidecar item."
  (let* ((operation (plist-get context :operation))
	 (item (or (plist-get context :saved-item)
		   (plist-get context :item))))
    (cond
     ((or (eq operation 'edit) (plist-get context :saved-item))
      (org-comments-compose-save-edit item body)
      item)
     (t
      (org-comments-compose-save-reply item body)))))

(defun org-comments-compose-save-draft ()
  "Save the current composed comment as a local draft."
  (interactive)
  (let* ((context org-comments-compose-context)
	 (refresh-function (plist-get context :refresh-function))
	 (body (org-comments-compose--body))
	 saved)
    (unless context
      (user-error "No active comment compose operation"))
    (setq saved (org-comments-compose--save-current context body))
    (setq-local org-comments-compose-context
		(plist-put context :saved-item saved))
    (set-buffer-modified-p nil)
    (when refresh-function
      (funcall refresh-function (plist-get context :source)))
    (message "Saved comment draft")))

(defun org-comments-compose-submit ()
  "Submit the current composed comment by saving it and closing compose."
  (interactive)
  (let* ((context org-comments-compose-context)
	 (refresh-function (plist-get context :refresh-function))
	 (body (org-comments-compose--body)))
    (unless context
      (user-error "No active comment compose operation"))
    (org-comments-compose--save-current context body)
    (when refresh-function
      (funcall refresh-function (plist-get context :source)))
    (org-comments-compose--close)))

;;;###autoload
(defun org-comments-compose-reply (comment &optional body)
  "Create a local reply to COMMENT with BODY.
When BODY is nil, prompt for reply text in the minibuffer."
  (let ((reply-body (or body (read-string "Reply: "))))
    (when (string-empty-p (string-trim reply-body))
      (user-error "Reply body is empty"))
    (org-comments-compose--append-reply
     comment
     (org-comments-compose--reply-record comment reply-body))))

(provide 'org-comments-compose)
;;; org-comments-compose.el ends here
