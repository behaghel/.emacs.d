;;; org-comments-commands.el --- Interactive commands for Org comments -*- lexical-binding: t; -*-

;;; Commentary:
;; Org-native command surface and default keybindings for Org comments.

;;; Code:

(require 'org)
(require 'org-comments-backend)
(require 'org-comments-compose)
(require 'org-comments-context-panel)
(require 'org-comments-core)
(require 'org-comments-model)
(require 'org-comments-overlays)
(require 'org-comments-panel)
(require 'org-comments-page)
(require 'org-comments-sidecar)
(require 'org-comments-store)
(require 'org-comments-ui)
(require 'seq)
(require 'subr-x)

(defcustom org-comments-keymap-prefix "C-c ;"
  "Prefix key for `org-comments-mode' default bindings.
Set this before loading `org-comments-commands' or customize it and reload the
mode map with `org-comments-rebuild-mode-map'."
  :type 'string
  :set (lambda (symbol value)
	 (set-default symbol value)
	 (when (boundp 'org-comments-mode-map)
	   (setq org-comments-mode-map (org-comments-build-mode-map))))
  :group 'org-comments)

(defcustom org-comments-enable-overlays t
  "Whether `org-comments-mode' shows inline and page comment overlays."
  :type 'boolean
  :group 'org-comments)

(defun org-comments-refresh ()
  "Refresh package-owned overlays and any registered rich comments UI."
  (when (and (bound-and-true-p org-comments-mode)
	     (derived-mode-p 'org-mode))
    (org-comments-overlays-refresh))
  (org-comments-ui-refresh))

(defun org-comments--region-bounds ()
  "Return active region bounds or signal a user error."
  (unless (use-region-p)
    (user-error "Select a region first"))
  (cons (region-beginning) (region-end)))

(defun org-comments--target-start (comment)
  "Return source target start for COMMENT, or `point-max' fallback."
  (or (plist-get comment :target-start)
      (plist-get comment :anchor-pos)
      most-positive-fixnum))

(defun org-comments--source-comments (&optional include-stale)
  "Return comments for current source buffer, optionally INCLUDE-STALE."
  (or (org-comments-collect (current-buffer) include-stale)
      (user-error "No comments for current buffer")))

(defun org-comments--active-at-point ()
  "Return source comment at point, or signal a user error."
  (let ((point (point)))
    (or (seq-find (lambda (comment)
		    (let ((start (plist-get comment :target-start))
			  (end (plist-get comment :target-end)))
		      (and start end (<= start point) (<= point end))))
		  (org-comments-collect (current-buffer) t))
	(user-error "No Org comment at point"))))

(defun org-comments--sidecar-comment-at-point ()
  "Return sidecar comment metadata at point, or nil."
  (when (and (derived-mode-p 'org-mode)
	     buffer-file-name
	     (string-suffix-p ".comments.org" buffer-file-name)
	     (not (org-before-first-heading-p)))
    (save-excursion
      (org-back-to-heading t)
      (when-let* ((id (org-entry-get nil "ORG_COMMENTS_ID")))
	(list :id id :sidecar-file buffer-file-name)))))

(defun org-comments--comment-at-point ()
  "Return sidecar or source comment metadata at point."
  (or (org-comments--sidecar-comment-at-point)
      (org-comments--active-at-point)))

(defun org-comments-goto-sidecar-heading (comment)
  "Visit COMMENT sidecar heading and return its buffer."
  (let ((sidecar-file (plist-get comment :sidecar-file))
	(comment-id (plist-get comment :id)))
    (unless (and sidecar-file comment-id)
      (user-error "Comment has no sidecar location"))
    (let ((buffer (find-file-noselect sidecar-file)))
      (pop-to-buffer buffer)
      (org-mode)
      (unless (org-comments-goto-id comment-id)
	(user-error "Cannot find comment %s" comment-id))
      buffer)))

(defun org-comments-goto-sidecar-body (comment)
  "Visit COMMENT sidecar heading and move to its body."
  (org-comments-goto-sidecar-heading comment)
  (org-end-of-meta-data t)
  (when (looking-at-p "[[:space:]]*$")
    (forward-line 1)))

;;;###autoload
(defun org-comments-open-sidecar ()
  "Open the comments sidecar for the current Org source buffer."
  (interactive)
  (let ((sidecar-file (org-comments-sidecar-path)))
    (unless (file-exists-p sidecar-file)
      (user-error "No sidecar comments file: %s" sidecar-file))
    (pop-to-buffer (find-file-noselect sidecar-file))
    (org-mode)
    (org-comments-fold-sidecar-property-drawers)))

;;;###autoload
(defun org-comments-open ()
  "Open the best available comments UI for the current buffer."
  (interactive)
  (org-comments-ui-open #'org-comments-panel-open))

;;;###autoload
(defun org-comments-create (start end &optional body)
  "Create a sidecar comment for region START to END with BODY."
  (interactive "r")
  (unless (derived-mode-p 'org-mode)
    (user-error "Org comments only work in Org buffers"))
  (unless buffer-file-name
    (user-error "Current buffer is not visiting a file"))
  (let* ((record (org-comments-create-record buffer-file-name start end (or body "")))
	 (sidecar-file (org-comments-append-to-sidecar record)))
    (org-comments-refresh)
    (org-comments-goto-sidecar-body
     (list :id (plist-get record :id)
	   :sidecar-file sidecar-file))))

;;;###autoload
(defun org-comments-create-from-region ()
  "Create a sidecar comment for the active region."
  (interactive)
  (pcase-let ((`(,start . ,end) (org-comments--region-bounds)))
    (org-comments-create start end)))

;;;###autoload
(defun org-comments-next ()
  "Move to the next source comment, wrapping at end of buffer."
  (interactive)
  (let* ((point (point))
	 (comments (sort (copy-sequence (org-comments--source-comments))
			 (lambda (left right)
			   (< (org-comments--target-start left)
			      (org-comments--target-start right)))))
	 (next (or (seq-find (lambda (comment)
			       (> (org-comments--target-start comment) point))
			     comments)
		   (car comments))))
    (goto-char (org-comments--target-start next))
    (org-comments-refresh)))

;;;###autoload
(defun org-comments-previous ()
  "Move to the previous source comment, wrapping at beginning of buffer."
  (interactive)
  (let* ((point (point))
	 (comments (sort (copy-sequence (org-comments--source-comments))
			 (lambda (left right)
			   (> (org-comments--target-start left)
			      (org-comments--target-start right)))))
	 (previous (or (seq-find (lambda (comment)
				   (< (org-comments--target-start comment) point))
				 comments)
		       (car comments))))
    (goto-char (org-comments--target-start previous))
    (org-comments-refresh)))

;;;###autoload
(defun org-comments-edit ()
  "Jump to the body of the comment at point."
  (interactive)
  (org-comments-goto-sidecar-body (org-comments--comment-at-point)))

;;;###autoload
(defun org-comments-delete ()
  "Delete the comment at point from its sidecar."
  (interactive)
  (let* ((comment (org-comments--comment-at-point))
	 (sidecar-file (plist-get comment :sidecar-file))
	 (comment-id (plist-get comment :id)))
    (unless (and sidecar-file comment-id)
      (user-error "Comment has no sidecar location"))
    (org-comments-delete-entry sidecar-file comment-id)
    (org-comments-refresh)))

(defun org-comments-set-status (status)
  "Set the sidecar status for the comment at point to STATUS."
  (let* ((comment (org-comments--comment-at-point))
	 (sidecar-file (plist-get comment :sidecar-file))
	 (comment-id (plist-get comment :id)))
    (unless (and sidecar-file comment-id)
      (user-error "Comment has no sidecar location"))
    (with-current-buffer (find-file-noselect sidecar-file)
      (org-mode)
      (unless (org-comments-goto-id comment-id)
	(user-error "Cannot find comment %s" comment-id))
      (org-todo status)
      (save-buffer))
    (org-comments-refresh)))

;;;###autoload
(defun org-comments-mark-open ()
  "Mark the current comment open."
  (interactive)
  (org-comments-set-status "OPEN"))

;;;###autoload
(defun org-comments-mark-todo ()
  "Mark the current comment TODO."
  (interactive)
  (org-comments-set-status "TODO"))

;;;###autoload
(defun org-comments-mark-resolved ()
  "Mark the current comment resolved."
  (interactive)
  (org-comments-set-status "RESOLVED"))

;;;###autoload
(defun org-comments-pull ()
  "Pull remote comments through the detected backend for the current buffer.
The current buffer selects the backend via `org-comments-backend-detect'.  This
command is comments-only and passes only `:source-file' to the backend."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Org comments only work in Org buffers"))
  (unless buffer-file-name
    (user-error "Current buffer is not visiting a file"))
  (let ((result (org-comments-backend-pull
		 (org-comments-backend-detect (current-buffer))
		 (list :source-file buffer-file-name))))
    (org-comments-refresh)
    result))

;;;###autoload
(defun org-comments-push ()
  "Push the current comment through the detected remote backend.
The current buffer selects the backend via `org-comments-backend-detect'.  The
comment record passed to the backend includes `:source-file' so backend adapters
can resolve remote page metadata without generic package coupling."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Org comments only work in Org buffers"))
  (unless buffer-file-name
    (user-error "Current buffer is not visiting a file"))
  (let ((result (org-comments-backend-push
		 (org-comments-backend-detect (current-buffer))
		 (append (org-comments--comment-at-point)
			 (list :source-file buffer-file-name)))))
    (org-comments-refresh)
    result))

;;;###autoload
(defun org-comments-open-remote ()
  "Open the current comment in the detected remote backend.
The current buffer selects the backend via `org-comments-backend-detect'.  The
comment record passed to the backend includes `:source-file' so backend adapters
can resolve remote page metadata without generic package coupling."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Org comments only work in Org buffers"))
  (unless buffer-file-name
    (user-error "Current buffer is not visiting a file"))
  (let ((comment (append (org-comments--comment-at-point)
			 (list :source-file buffer-file-name))))
    (org-comments-backend-open-remote
     (org-comments-backend-detect (current-buffer)) comment)))

;;;###autoload
(defun org-comments-sync ()
  "Synchronize comments for the current Org source buffer.
The selected backend is inferred from the current buffer via
`org-comments-backend-detect'.  This command is comments-only; backends must not
sync page content as a side effect of this operation."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Org comments only work in Org buffers"))
  (unless buffer-file-name
    (user-error "Current buffer is not visiting a file"))
  (let ((result (org-comments-backend-sync
		 (org-comments-backend-detect (current-buffer))
		 (list :source-file buffer-file-name))))
    (org-comments-refresh)
    result))

;;;###autoload
(defun org-comments-reply ()
  "Create a reply using the best available comments UI."
  (interactive)
  (org-comments-ui-compose-reply
   (lambda ()
     (org-comments-compose-reply (org-comments--comment-at-point)))))

(defun org-comments-build-mode-map ()
  "Return a fresh keymap for `org-comments-mode'."
  (let ((map (make-sparse-keymap))
	(prefix (make-sparse-keymap))
	(status (make-sparse-keymap)))
    (define-key map (kbd org-comments-keymap-prefix) prefix)
    (define-key prefix (kbd "c") #'org-comments-create)
    (define-key prefix (kbd "P") #'org-comments-page-create)
    (define-key prefix (kbd "o") #'org-comments-open)
    (define-key prefix (kbd "s") #'org-comments-open-sidecar)
    (define-key prefix (kbd "n") #'org-comments-next)
    (define-key prefix (kbd "p") #'org-comments-previous)
    (define-key prefix (kbd "r") #'org-comments-reply)
    (define-key prefix (kbd "S") #'org-comments-sync)
    (define-key prefix (kbd "O") #'org-comments-open-remote)
    (define-key prefix (kbd "U") #'org-comments-push)
    (define-key prefix (kbd "D") #'org-comments-pull)
    (define-key prefix (kbd "e") #'org-comments-edit)
    (define-key prefix (kbd "d") #'org-comments-delete)
    (define-key prefix (kbd "m") status)
    (define-key status (kbd "o") #'org-comments-mark-open)
    (define-key status (kbd "t") #'org-comments-mark-todo)
    (define-key status (kbd "r") #'org-comments-mark-resolved)
    map))

(defvar org-comments-mode-map)

;;;###autoload
(defun org-comments-rebuild-mode-map ()
  "Rebuild `org-comments-mode-map' after changing `org-comments-keymap-prefix'."
  (interactive)
  (setq org-comments-mode-map (org-comments-build-mode-map)))

(defvar org-comments-mode-map (org-comments-build-mode-map)
  "Keymap for `org-comments-mode'.")

;;;###autoload
(define-minor-mode org-comments-mode
  "Enable Org-native comment commands for the current buffer.
When `org-comments-enable-overlays' is non-nil, also show inline and page
comment overlays owned by this mode.  The default key prefix is controlled by
`org-comments-keymap-prefix'."
  :lighter " OrgC"
  :keymap org-comments-mode-map
  (if org-comments-mode
      (when org-comments-enable-overlays
	(org-comments-context-panel-enable))
    (org-comments-context-panel-disable)))

;;;###autoload
(defun org-comments-setup ()
  "Enable `org-comments-mode' in Org buffers."
  (interactive)
  (add-hook 'org-mode-hook #'org-comments-mode))

(provide 'org-comments-commands)
;;; org-comments-commands.el ends here
