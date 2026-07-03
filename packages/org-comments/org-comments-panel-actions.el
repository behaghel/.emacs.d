;;; org-comments-panel-actions.el --- Actions for Org comments panel -*- lexical-binding: t; -*-

;;; Commentary:
;; Generic row actions for the standalone Org comments panel.

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'org-context-panel)
(require 'org-comments-backend)
(require 'org-comments-compose)
(require 'org-comments-sidecar)
(require 'org-comments-store)

(declare-function org-comments-panel-refresh "org-comments-panel")
(defvar-local org-comments-panel-refresh-function #'org-comments-panel-refresh
  "Function used to refresh the current Org comments panel buffer.")
(defvar-local org-comments-current-comment-function #'org-comments-panel-current-comment
  "Function used to return the comment at point.")
(defvar-local org-comments-current-source-buffer-function
    #'org-context-panel-current-source-buffer
  "Function used to return the source buffer for actions at point.")
(defvar-local org-comments-current-refresh-function
    #'org-comments--refresh-current-context-panel
  "Function used to refresh the UI after actions at point.")
(defvar-local org-comments-current-reply-function #'org-comments-compose-reply
  "Function used to reply to a comment at point.")
(defvar-local org-comments-current-jump-function #'org-comments-panel--jump-current
  "Function used to jump from the current item at point.")
(defvar-local org-comments-current-page-open-function #'org-comments--page-open-current-source
  "Function used to open page-level comments for the current UI.")
(defvar-local org-comments-current-close-function
    #'org-comments--close-current-context-panel
  "Function used to close the current comments UI.")
(defvar-local org-comments-current-help-function #'org-comments--help-current-ui
  "Function used to show help for the current comments UI.")
(defvar-local org-comments-current-item-starts-function #'org-comments-panel--comment-starts
  "Function used to return rendered item start positions in the current UI.")

(defun org-comments--refresh-current-context-panel ()
  "Refresh the current comments UI through `org-context-panel' lifecycle."
  (cond
   ((local-variable-p 'org-comments-panel-refresh-function)
    (funcall org-comments-panel-refresh-function))
   (org-context-panel-view-id
    (org-context-panel-refresh-bottom-view))
   (t
    (let ((org-context-panel-buffer-name (buffer-name)))
      (org-context-panel-refresh)))))

(defun org-comments-panel--refresh ()
  "Refresh the current comments panel using its buffer-local refresh function."
  (funcall org-comments-panel-refresh-function))

(defun org-comments-panel-current-comment ()
  "Return the comment record at point in the current panel buffer."
  (or (get-text-property (point) 'org-comments-comment)
      (get-text-property (line-beginning-position) 'org-comments-comment)
      (get-text-property (max (point-min) (1- (line-end-position))) 'org-comments-comment)
      (user-error "No Org comment at point")))

(defun org-comments-current-comment ()
  "Return the current comment using `org-comments-current-comment-function'."
  (funcall org-comments-current-comment-function))

(defun org-comments-current-source-buffer ()
  "Return the current source buffer using configured source-buffer function."
  (let ((source-buffer (funcall org-comments-current-source-buffer-function)))
    (unless (buffer-live-p source-buffer)
      (user-error "No source buffer associated with this comments UI"))
    source-buffer))

(defun org-comments-refresh-current-ui ()
  "Refresh the current comments UI using `org-comments-current-refresh-function'."
  (when org-comments-current-refresh-function
    (funcall org-comments-current-refresh-function)))

(defun org-comments--close-current-context-panel ()
  "Close the current comments UI through `org-context-panel' lifecycle."
  (if org-context-panel-view-id
      (org-context-panel-close-bottom-view)
    (let ((org-context-panel-buffer-name (buffer-name)))
      (org-context-panel-close))))

(defun org-comments--close-current-buffer-window ()
  "Close the current comments UI buffer and its window."
  (let ((buffer (current-buffer))
	(window (selected-window)))
    (when (window-live-p window)
      (quit-window nil window))
    (when (buffer-live-p buffer)
      (kill-buffer buffer))))

;;;###autoload
(defun org-comments-close-current-ui ()
  "Close the current comments UI using the active close adapter."
  (interactive)
  (funcall org-comments-current-close-function))

(defun org-comments-help-text ()
  "Return generic help text for the current Org comments UI."
  (string-join
   '("Org Comments"
     ""
     "RET  jump to source or sidecar"
     "o/O  open remote comment"
     "r    reply"
     "U    push current row"
     "D    pull remote comments"
     "S    sync remote comments"
     "e    edit"
     "d    delete"
     "m    status actions"
     "z    filters"
     "q    close"
     ""
     "Public commands are DWIM: org-comments-open-remote, org-comments-push,"
     "org-comments-reply, org-comments-pull, org-comments-sync, and mark commands"
     "work from both Org source buffers and comments panel rows.")
   "\n"))

(defun org-comments--help-current-ui ()
  "Show generic help for the current Org comments UI."
  (with-help-window "*Org Comments Help*"
    (princ (org-comments-help-text))))

;;;###autoload
(defun org-comments-help-current-ui ()
  "Show help for the current comments UI using the active help adapter."
  (interactive)
  (funcall org-comments-current-help-function))

(defun org-comments-panel--sidecar-location (comment)
  "Return COMMENT sidecar location as (SIDECAR-FILE . COMMENT-ID)."
  (let ((sidecar-file (plist-get comment :sidecar-file))
	(comment-id (plist-get comment :id)))
    (unless (and sidecar-file comment-id)
      (user-error "Comment has no sidecar location"))
    (cons sidecar-file comment-id)))

(defun org-comments-panel--goto-sidecar-heading (comment)
  "Visit COMMENT sidecar heading and return its buffer."
  (pcase-let ((`(,sidecar-file . ,comment-id)
	       (org-comments-panel--sidecar-location comment)))
    (let ((buffer (find-file-noselect sidecar-file)))
      (with-current-buffer buffer
	(org-mode)
	(goto-char (point-min))
	(unless (org-comments-goto-id comment-id)
	  (user-error "Cannot find comment %s" comment-id)))
      buffer)))

(defun org-comments--source-file (source-buffer)
  "Return the visited file for SOURCE-BUFFER or signal a user error."
  (or (buffer-file-name source-buffer)
      (user-error "Source buffer is not visiting a file")))

(defun org-comments-panel--source-file ()
  "Return the source file associated with the current comments UI."
  (org-comments--source-file (org-comments-current-source-buffer)))

(defun org-comments-panel--source-record ()
  "Return backend source record for the current panel."
  (list :source-file (org-comments-panel--source-file)))

(defun org-comments-comment-with-source-file (comment source-buffer)
  "Return COMMENT annotated with SOURCE-BUFFER's visited file."
  (append comment (list :source-file (org-comments--source-file source-buffer))))

(defun org-comments-panel--comment-with-source-file ()
  "Return current panel comment annotated with its source file."
  (org-comments-comment-with-source-file
   (org-comments-current-comment)
   (org-comments-current-source-buffer)))

(defun org-comments-remote-status-p (comment source-buffer status)
  "Return non-nil when COMMENT STATUS should be handled by a remote backend.
Only resolved-state propagation is enabled initially; local OPEN/TODO state
remains sidecar-only until backends declare broader status semantics."
  (let ((backend (org-comments-backend-detect source-buffer)))
    (and (equal status "RESOLVED")
	 (not (equal (plist-get comment :status) status))
	 (plist-get comment :remote-id)
	 (org-comments-backend-capable-p backend :set-status))))

(defun org-comments-set-remote-status (comment source-buffer status)
  "Set COMMENT STATUS through SOURCE-BUFFER's detected remote backend."
  (let* ((backend (org-comments-backend-detect source-buffer))
	 (record (org-comments-comment-with-source-file comment source-buffer)))
    (org-comments-backend-set-status backend record status)))

(defun org-comments-panel--sidecar-jump-comment-p (comment)
  "Return non-nil when COMMENT should jump to its sidecar heading."
  (or (plist-get comment :page-comment)
      (eq (plist-get comment :anchor-state) 'stale)))

(defun org-comments-panel--jump-current ()
  "Jump from the current panel row to its source or sidecar target."
  (let* ((comment (org-comments-current-comment))
	 (source-buffer (org-comments-current-source-buffer))
	 (position (or (plist-get comment :target-start)
		       (plist-get comment :anchor-pos))))
    (if (org-comments-panel--sidecar-jump-comment-p comment)
	(pop-to-buffer (org-comments-panel--goto-sidecar-heading comment))
      (unless (and (buffer-live-p source-buffer) position)
	(user-error "Comment has no source location"))
      (pop-to-buffer source-buffer)
      (goto-char position))))

;;;###autoload
(defun org-comments-jump-at-point ()
  "Jump from the current item at point using the active comments UI."
  (interactive)
  (funcall org-comments-current-jump-function))

;;;###autoload
(defun org-comments-panel-jump ()
  "Jump from the current panel row to its source comment target."
  (interactive)
  (org-comments-jump-at-point))

(defun org-comments--page-open-current-source ()
  "Open page comments for `org-comments-current-source-buffer'."
  (let ((source-buffer (org-comments-current-source-buffer)))
    (require 'org-comments-page)
    (with-current-buffer source-buffer
      (org-comments-page-open))))

;;;###autoload
(defun org-comments-page-open-at-point ()
  "Open page-level comments for the current comments UI."
  (interactive)
  (funcall org-comments-current-page-open-function))

(defun org-comments-panel--comment-starts ()
  "Return buffer positions that start rendered comments."
  (let (starts)
    (save-excursion
      (goto-char (point-min))
      (while (< (point) (point-max))
	(when (get-text-property (point) 'org-comments-comment)
	  (push (point) starts)
	  (while (and (< (point) (point-max))
		      (get-text-property (point) 'org-comments-comment))
	    (goto-char (next-single-property-change
			(point) 'org-comments-comment nil (point-max)))))
	(unless (eobp)
	  (forward-char 1))))
    (nreverse starts)))

;;;###autoload
(defun org-comments-next-item-at-point ()
  "Move point to the next rendered comment item, wrapping at end."
  (interactive)
  (let* ((starts (funcall org-comments-current-item-starts-function))
	 (next (or (cl-find-if (lambda (position) (> position (point))) starts)
		   (car starts))))
    (unless next
      (user-error "No comment items"))
    (goto-char next)))

;;;###autoload
(defun org-comments-previous-item-at-point ()
  "Move point to the previous rendered comment item, wrapping at beginning."
  (interactive)
  (let* ((starts (reverse (funcall org-comments-current-item-starts-function)))
	 (previous (or (cl-find-if (lambda (position) (< position (point))) starts)
		       (car starts))))
    (unless previous
      (user-error "No comment items"))
    (goto-char previous)))

(defun org-comments-panel-pull ()
  "Pull remote comments through the detected backend for the panel source."
  (let* ((source-buffer (org-comments-current-source-buffer))
	 (result (org-comments-backend-pull
		  (org-comments-backend-detect source-buffer)
		  (org-comments-panel--source-record))))
    (org-comments-refresh-current-ui)
    result))

(defun org-comments-panel-sync ()
  "Synchronize comments through the detected backend for the panel source."
  (let* ((source-buffer (org-comments-current-source-buffer))
	 (result (org-comments-backend-sync
		  (org-comments-backend-detect source-buffer)
		  (org-comments-panel--source-record))))
    (org-comments-refresh-current-ui)
    result))

(defun org-comments-push-at-point ()
  "Push the current comment through the detected remote backend."
  (let* ((source-buffer (org-comments-current-source-buffer))
	 (comment (org-comments-comment-with-source-file
		   (org-comments-current-comment) source-buffer))
	 (result (org-comments-backend-push
		  (org-comments-backend-detect source-buffer)
		  comment)))
    (org-comments-refresh-current-ui)
    result))

(defun org-comments-panel-push ()
  "Push the current panel row through the detected remote backend."
  (org-comments-push-at-point))

(defun org-comments-open-remote-at-point ()
  "Open the current comment through the detected remote backend."
  (let* ((source-buffer (org-comments-current-source-buffer))
	 (comment (org-comments-comment-with-source-file
		   (org-comments-current-comment) source-buffer)))
    (org-comments-backend-open-remote
     (org-comments-backend-detect source-buffer)
     comment)))

(defun org-comments-panel-open-remote ()
  "Open the current panel row through the detected remote backend."
  (org-comments-open-remote-at-point))

(defun org-comments-reply-at-point ()
  "Reply to the current comment at point and refresh the UI."
  (let ((result (funcall org-comments-current-reply-function
			 (org-comments-current-comment))))
    (org-comments-refresh-current-ui)
    result))

(defun org-comments-panel-reply ()
  "Prompt for a reply to the current panel row and refresh."
  (org-comments-reply-at-point))

(defun org-comments-edit-at-point ()
  "Open the current comment at point for editing in its sidecar."
  (let ((buffer (org-comments-panel--goto-sidecar-heading
		 (org-comments-current-comment))))
    (pop-to-buffer buffer)
    (org-end-of-meta-data t)
    (when (looking-at-p "[[:space:]]*$")
      (forward-line 1))))

(defun org-comments-panel-edit ()
  "Open the current panel row's sidecar comment body for editing."
  (org-comments-edit-at-point))

(defun org-comments-delete-at-point ()
  "Delete the current comment at point and refresh the UI."
  (let ((comment (org-comments-current-comment)))
    (pcase-let ((`(,sidecar-file . ,comment-id)
		 (org-comments-panel--sidecar-location comment)))
      (org-comments-delete-entry sidecar-file comment-id)
      (org-comments-refresh-current-ui)
      (message "Deleted comment %s" comment-id))))

(defun org-comments-panel-delete ()
  "Delete the current panel row's sidecar comment and refresh the panel."
  (org-comments-delete-at-point))

(defun org-comments-set-status-at-point (status)
  "Set the current comment at point to STATUS and refresh the UI."
  (let* ((source-buffer (org-comments-current-source-buffer))
	 (comment (org-comments-current-comment))
	 (result (if (org-comments-remote-status-p comment source-buffer status)
		     (org-comments-set-remote-status comment source-buffer status)
		   (with-current-buffer (org-comments-panel--goto-sidecar-heading comment)
		     (org-comments-set-entry-status status)
		     (save-buffer))
		   (message "Marked comment %s %s" (plist-get comment :id) status)
		   status)))
    (org-comments-refresh-current-ui)
    result))

(defun org-comments-panel-set-status (status)
  "Set the current panel row's sidecar comment to STATUS and refresh."
  (org-comments-set-status-at-point status))

(defun org-comments-mark-open-at-point ()
  "Mark the current comment at point OPEN."
  (org-comments-set-status-at-point "OPEN"))

(defun org-comments-mark-todo-at-point ()
  "Mark the current comment at point TODO."
  (org-comments-set-status-at-point "TODO"))

(defun org-comments-mark-resolved-at-point ()
  "Mark the current comment at point RESOLVED."
  (org-comments-set-status-at-point "RESOLVED"))

(defun org-comments-panel-mark-open ()
  "Mark the current panel row's comment open."
  (org-comments-mark-open-at-point))

(defun org-comments-panel-mark-todo ()
  "Mark the current panel row's comment TODO."
  (org-comments-mark-todo-at-point))

(defun org-comments-panel-mark-resolved ()
  "Mark the current panel row's comment resolved."
  (org-comments-mark-resolved-at-point))

(provide 'org-comments-panel-actions)
;;; org-comments-panel-actions.el ends here
