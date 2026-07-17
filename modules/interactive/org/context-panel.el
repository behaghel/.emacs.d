;;; context-panel.el --- Org mode context side panel -*- lexical-binding: t; -*-

;;; Commentary:
;; Read-only right-side panel for contextual Org authoring records.  Native Org
;; footnotes are currently the first data source; sidecar comments will follow.

;;; Code:

(require 'org-comments)
(require 'org-comments-context-panel)
(require 'org-context-panel)
(require 'org-marginalia-context-panel)
(require 'org)
(require 'org-comments-panel-actions)
(require 'org-comments-page)
(require 'org-comments-panel-filter)
(require 'org-comments-ui)
(require 'subr-x)

(autoload 'org-confluence-comments-push-current "org-confluence-comments-push" nil t)

(defgroup hub/org-context-panel nil
  "Interactive Org context panel."
  :group 'org)

(defcustom hub/org-context-panel-buffer-name "*Org Context*"
  "Buffer name used for the Org context side panel."
  :type 'string
  :group 'hub/org-context-panel)

(defcustom hub/org-context-panel-width 38
  "Width of the Org context side panel."
  :type 'natnum
  :group 'hub/org-context-panel)

(defcustom hub/org-context-panel-dock-prose t
  "Whether opening the context panel docks visually filled prose toward it."
  :type 'boolean
  :group 'hub/org-context-panel)

(defcustom hub/org-context-panel-refresh-idle-delay 0.25
  "Idle delay before refreshing a visible Org context panel after commands."
  :type 'number
  :group 'hub/org-context-panel)

(defvar-local hub/org-context-panel--visual-fill-state nil
  "Saved visual-fill-column state while context panel docks prose.")

(defvar-local hub/org-context-panel--refresh-timer nil
  "Pending idle timer for refreshing this buffer's context panel.")

(defvar-local hub/org-context-panel--refresh-signature nil
  "Last viewport/content signature used for context-panel refresh.")

;;;###autoload
(defun hub/org-context-panel--visual-fill-total-margin (source-window)
  "Return total visual-fill-column margin for SOURCE-WINDOW."
  (let* ((width (or (and (boundp 'visual-fill-column-width)
			 (numberp visual-fill-column-width)
			 visual-fill-column-width)
		    fill-column))
	 (total-width (if (fboundp 'visual-fill-column--window-max-text-width)
			  (visual-fill-column--window-max-text-width source-window)
			(window-body-width source-window))))
    (max 0 (- total-width width))))

(defun hub/org-context-panel--dock-prose (&optional source-window)
  "Dock visually filled prose in SOURCE-WINDOW toward the context panel."
  (let ((source-window (or source-window org-context-panel-current-source-window)))
    (when (and hub/org-context-panel-dock-prose
	       (window-live-p source-window)
	       (require 'visual-fill-column nil t))
      (with-current-buffer (window-buffer source-window)
	(when (bound-and-true-p visual-fill-column-mode)
	  (unless hub/org-context-panel--visual-fill-state
	    (setq hub/org-context-panel--visual-fill-state
		  (list :center (and (boundp 'visual-fill-column-center-text)
				     visual-fill-column-center-text)
			:extra (and (boundp 'visual-fill-column-extra-text-width)
				    visual-fill-column-extra-text-width))))
	  (let* ((margin (hub/org-context-panel--visual-fill-total-margin source-window))
		 (half (/ margin 2)))
	    ;; `visual-fill-column' can only left-dock or center text directly.  Keep
	    ;; centering enabled, then shift the centered margins right by expanding the
	    ;; left margin and collapsing the right margin.
	    (setq-local visual-fill-column-center-text t
			visual-fill-column-extra-text-width (cons (- half) half))
	    (when (fboundp 'visual-fill-column--set-margins)
	      (visual-fill-column--set-margins source-window))))))))

(defun hub/org-context-panel--restore-prose-docking (&optional source-buffer)
  "Restore visual-fill-column state saved for SOURCE-BUFFER."
  (let ((source-buffer (or source-buffer org-context-panel-current-source-buffer)))
    (when (and (buffer-live-p source-buffer)
	       (require 'visual-fill-column nil t))
      (with-current-buffer source-buffer
	(when hub/org-context-panel--visual-fill-state
	  (setq-local visual-fill-column-center-text
		      (plist-get hub/org-context-panel--visual-fill-state :center)
		      visual-fill-column-extra-text-width
		      (plist-get hub/org-context-panel--visual-fill-state :extra)
		      hub/org-context-panel--visual-fill-state nil)
	  (when-let* ((window (get-buffer-window source-buffer t)))
	    (when (fboundp 'visual-fill-column--set-margins)
	      (visual-fill-column--set-margins window))))))))

(add-hook 'org-context-panel-after-side-panel-open-hook
	  #'hub/org-context-panel--dock-prose)
(add-hook 'org-context-panel-before-side-panel-close-hook
	  #'hub/org-context-panel--restore-prose-docking)

(defun hub/org-context-panel--cancel-refresh-timer ()
  "Cancel this buffer's pending context-panel refresh timer."
  (when (timerp hub/org-context-panel--refresh-timer)
    (cancel-timer hub/org-context-panel--refresh-timer))
  (setq hub/org-context-panel--refresh-timer nil))

(defun hub/org-context-panel--side-visible-p (&optional source-buffer)
  "Return non-nil when SOURCE-BUFFER has a visible side context panel."
  (let ((source (or source-buffer (current-buffer))))
    (with-current-buffer source
      (and (buffer-live-p org-context-panel-side-panel-buffer)
	   (get-buffer-window org-context-panel-side-panel-buffer t)))))

(defun hub/org-context-panel--bottom-visible-p (&optional source-buffer)
  "Return non-nil when SOURCE-BUFFER has a visible bottom context view."
  (let ((source (or source-buffer (current-buffer))))
    (with-current-buffer source
      (and (buffer-live-p org-context-panel-bottom-panel-buffer)
	   (get-buffer-window org-context-panel-bottom-panel-buffer t)))))

(defun hub/org-context-panel--visible-p (&optional source-buffer)
  "Return non-nil when SOURCE-BUFFER has any visible context panel."
  (or (hub/org-context-panel--side-visible-p source-buffer)
      (hub/org-context-panel--bottom-visible-p source-buffer)))

(defun hub/org-context-panel--refresh-after-idle (source-buffer)
  "Refresh visible context panels for SOURCE-BUFFER after Emacs becomes idle."
  (when (buffer-live-p source-buffer)
    (with-current-buffer source-buffer
      (setq hub/org-context-panel--refresh-timer nil)
      (when (and hub/org-context-panel-mode
		 (hub/org-context-panel--visible-p source-buffer))
	(save-selected-window
	  (hub/org-context-panel--refresh-visible-panels source-buffer))))))

(defun hub/org-context-panel--refresh-signature (&optional source-buffer)
  "Return context-panel refresh signature for SOURCE-BUFFER."
  (let* ((source (or source-buffer (current-buffer)))
	 (window (get-buffer-window source t)))
    (when (window-live-p window)
      (list (window-start window)
	    (window-end window t)
	    (window-body-width window)
	    (window-body-height window)
	    (with-current-buffer source
	      (buffer-chars-modified-tick))))))

(defun hub/org-context-panel--post-command-refresh ()
  "Schedule a visible context panel refresh after source-buffer commands."
  (when (and hub/org-context-panel-mode
	     (hub/org-context-panel--visible-p))
    (let ((signature (hub/org-context-panel--refresh-signature)))
      (unless (equal signature hub/org-context-panel--refresh-signature)
	(setq hub/org-context-panel--refresh-signature signature)
	(hub/org-context-panel--cancel-refresh-timer)
	(setq hub/org-context-panel--refresh-timer
	      (run-with-idle-timer hub/org-context-panel-refresh-idle-delay nil
				   #'hub/org-context-panel--refresh-after-idle
				   (current-buffer)))))))

(defun hub/org-context-panel--enable-comments-provider ()
  "Enable package context providers with personal side-panel naming."
  (let ((org-comments-panel-buffer-name hub/org-context-panel-buffer-name)
	(comments-provider (org-context-panel-registered-provider 'comments)))
    (when (or (not comments-provider)
	      (not (equal (plist-get comments-provider :side-panel-buffer-name)
			  hub/org-context-panel-buffer-name)))
      (org-comments-context-panel-enable))
    (unless (org-context-panel-registered-provider 'marginalia)
      (org-marginalia-context-panel-mode 1))
    (unless org-context-panel-mode
      (org-context-panel-mode 1))))

(defun hub/org-context-panel--page-open-ui ()
  "Open the configured package page comments UI."
  (hub/org-context-panel--open-page-view t t))

(defun hub/org-context-panel--refresh-ui ()
  "Refresh the configured package context panel UI."
  (unless (derived-mode-p 'org-mode)
    (user-error "Org context panel only works in Org buffers"))
  (hub/org-context-panel--enable-comments-provider)
  (org-context-panel-refresh-source-overlays)
  (setq hub/org-context-panel--refresh-signature
	(hub/org-context-panel--refresh-signature))
  (org-context-panel-refresh))

(defun hub/org-context-panel--open-comment-ui (comment-id &optional jump-position)
  "Open COMMENT-ID through the configured package context panel UI."
  (unless (derived-mode-p 'org-mode)
    (user-error "Org context panel only works in Org buffers"))
  (when jump-position
    (goto-char jump-position))
  (let ((panel (hub/org-context-panel--open-ui)))
    (with-current-buffer panel
      (hub/org-context-panel--goto-comment-id comment-id))
    panel))

(defun hub/org-context-panel--page-open-comment-ui (comment-id)
  "Open page COMMENT-ID through the configured package context panel UI."
  (let ((panel (hub/org-context-panel--open-page-view t t)))
    (unless panel
      (user-error "No page context panel available"))
    (with-current-buffer panel
      (hub/org-context-panel--goto-comment-id comment-id))
    panel))

(setq org-comments-ui-open-function #'hub/org-context-panel--open-ui
      org-comments-ui-page-open-function #'hub/org-context-panel--page-open-ui
      org-comments-ui-refresh-function #'hub/org-context-panel--refresh-ui
      org-comments-ui-open-comment-function #'hub/org-context-panel--open-comment-ui
      org-comments-ui-page-open-comment-function #'hub/org-context-panel--page-open-comment-ui)

(defun hub/org-context-panel--visible-window ()
  "Return the visible context panel window, or nil."
  (or (when (buffer-live-p org-context-panel-side-panel-buffer)
	(get-buffer-window org-context-panel-side-panel-buffer t))
      (when-let* ((panel (get-buffer hub/org-context-panel-buffer-name)))
	(get-buffer-window panel t))))

(defun hub/org-context-panel--toggle-filter (property)
  "Toggle package comment filter PROPERTY for the current context panel source."
  (let* ((source (org-comments-filter-current-source-buffer))
	 (state (org-comments-filter-state source)))
    (org-comments-filter-set-state
     (org-comments-toggle-filter property state)
     source)
    (org-comments-refresh-current-ui)
    (message "Comment filters: %s"
	     (org-comments-panel-filter-summary
	      (org-comments-filter-state source)))))

(defun hub/org-context-panel-filter-toggle-actionable ()
  "Toggle actionable-only context filtering."
  (interactive)
  (hub/org-context-panel--toggle-filter :actionable))

(defun hub/org-context-panel-filter-toggle-drafts ()
  "Toggle draft/local-edit-only context filtering."
  (interactive)
  (hub/org-context-panel--toggle-filter :drafts))

(defun hub/org-context-panel-filter-toggle-mine ()
  "Toggle current-user-only context filtering."
  (interactive)
  (hub/org-context-panel--toggle-filter :mine))

(defun hub/org-context-panel-filter-toggle-missing ()
  "Toggle whether remote-missing context items are shown."
  (interactive)
  (hub/org-context-panel--toggle-filter :show-missing))

(with-eval-after-load 'org-comments-panel
  (define-key org-comments-panel-filter-map (kbd "a")
	      #'hub/org-context-panel-filter-toggle-actionable)
  (define-key org-comments-panel-filter-map (kbd "d")
	      #'hub/org-context-panel-filter-toggle-drafts)
  (define-key org-comments-panel-filter-map (kbd "m")
	      #'hub/org-context-panel-filter-toggle-mine)
  (define-key org-comments-panel-filter-map (kbd "x")
	      #'hub/org-context-panel-filter-toggle-missing))

(defun hub/org-context-panel--pulse-current-line ()
  "Briefly highlight the current context panel line when possible."
  (when (fboundp 'pulse-momentary-highlight-one-line)
    (pulse-momentary-highlight-one-line (point))))

(defun hub/org-context-panel--goto-comment-id (comment-id)
  "Move point to COMMENT-ID in the current context panel and highlight it."
  (interactive "sComment ID: ")
  (unless (org-context-panel-goto-item-key comment-id)
    (user-error "Comment %s is not visible in this context panel" comment-id))
  (when-let* ((window (get-buffer-window (current-buffer) t)))
    (set-window-point window (point))
    (with-selected-window window
      (recenter)))
  (hub/org-context-panel--pulse-current-line)
  comment-id)

(defun hub/org-context-panel--close-page-view (&optional source-buffer)
  "Close page-context window for SOURCE-BUFFER or current source."
  (let ((source (or source-buffer
		    (if (derived-mode-p 'org-comments-panel-mode 'org-context-panel-buffer-mode)
			org-context-panel-source-buffer
		      (current-buffer)))))
    (when (buffer-live-p source)
      (with-current-buffer source
	(when (buffer-live-p org-context-panel-bottom-panel-buffer)
	  (org-context-panel-close-bottom-view))))))

(defun hub/org-context-panel--open-page-view (&optional select show-empty)
  "Open page-level context below the current Org source buffer.
When SELECT is non-nil, focus the page-context window.  SHOW-EMPTY controls
whether an empty page-context panel is shown when there are no page comments."
  (interactive (list t t))
  (unless (derived-mode-p 'org-mode)
    (user-error "Page context only works in Org buffers"))
  (let* ((source-buffer (current-buffer))
	 (comments (org-comments-collect-page source-buffer)))
    (org-comments-context-panel-enable)
    (if (or comments show-empty)
	(let ((page-buffer (org-context-panel-open-bottom-view
			    'page-comments source-buffer)))
	  (when-let* ((window (get-buffer-window page-buffer t)))
	    (when select
	      (select-window window)))
	  page-buffer)
      (hub/org-context-panel--close-page-view source-buffer)
      nil)))

(defun hub/org-context-panel--refresh-visible-panels (source)
  "Refresh visible context panels for SOURCE without changing selection."
  (when (buffer-live-p source)
    (with-current-buffer source
      (hub/org-context-panel--enable-comments-provider)
      (when (and (buffer-live-p org-context-panel-side-panel-buffer)
		 (get-buffer-window org-context-panel-side-panel-buffer t))
	(org-context-panel-refresh))
      (when (and (buffer-live-p org-context-panel-bottom-panel-buffer)
		 (get-buffer-window org-context-panel-bottom-panel-buffer t))
	(with-current-buffer org-context-panel-bottom-panel-buffer
	  (org-context-panel-refresh-bottom-view))))))

(defun hub/org-context-panel-revert-buffer (&optional _ignore-auto _noconfirm)
  "Refresh the current context panel buffer for `revert-buffer'."
  (unless (buffer-live-p org-context-panel-source-buffer)
    (user-error "No source buffer for this context panel"))
  (let ((source org-context-panel-source-buffer))
    (if org-context-panel-view-id
	(org-context-panel-refresh-bottom-view)
      (org-context-panel-refresh))
    (hub/org-context-panel--refresh-visible-panels source)
    (message "Refreshed Org context panel")))

;;;###autoload
(defun hub/org-context-panel--close-ui ()
  "Close the context side panel associated with the current buffer."
  (interactive)
  (let* ((panel-window (hub/org-context-panel--visible-window))
	 (source-buffer (cond
			 ((derived-mode-p 'org-comments-panel-mode 'org-context-panel-buffer-mode)
			  org-context-panel-source-buffer)
			 ((and (window-live-p panel-window)
			       (buffer-live-p (window-buffer panel-window)))
			  (with-current-buffer (window-buffer panel-window)
			    org-context-panel-source-buffer))
			 ((derived-mode-p 'org-context-panel-buffer-mode)
			  org-context-panel-source-buffer)
			 (t (current-buffer)))))
    (when (buffer-live-p source-buffer)
      (with-current-buffer source-buffer
	(unless org-comments-mode
	  (org-comments-context-panel-delete-overlays))
	(hub/org-context-panel--close-page-view source-buffer)
	(when (buffer-live-p org-context-panel-side-panel-buffer)
	  (org-context-panel-close))))
    (when (window-live-p panel-window)
      (delete-window panel-window))))

(defun hub/org-context-panel--copilot-chat-session-p ()
  "Return non-nil when current Org buffer has pending Org Copilot chat state."
  (or (and (fboundp 'org-copilot-chat-messages)
	   (org-copilot-chat-messages))
      (and (boundp 'org-copilot-chat-buffer-name)
	   (buffer-live-p (get-buffer org-copilot-chat-buffer-name)))))

(defun hub/org-context-panel--open-ui ()
  "Open side context panel and surface pending bottom views when appropriate."
  (unless (derived-mode-p 'org-mode)
    (user-error "Org context panel only works in Org buffers"))
  (let ((source-buffer (current-buffer))
	(surface-copilot-chat (hub/org-context-panel--copilot-chat-session-p)))
    (hub/org-context-panel--enable-comments-provider)
    (org-context-panel-refresh-source-overlays)
    (setq hub/org-context-panel--refresh-signature
	  (hub/org-context-panel--refresh-signature source-buffer))
    (let ((panel (org-context-panel-open source-buffer)))
      (with-current-buffer source-buffer
	(hub/org-context-panel--open-page-view nil nil)
	(when (and surface-copilot-chat
		   (fboundp 'org-copilot-chat))
	  (org-copilot-chat)))
      panel)))

;;;###autoload
(defun hub/org-context-panel-toggle-open ()
  "Cycle context UI: open side, close all, then reopen all.
When only a bottom chat/view is visible, opening the side panel keeps and
surfaces that pending bottom session instead of closing it."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Org context panel only works in Org buffers"))
  (if (hub/org-context-panel--side-visible-p)
      (hub/org-context-panel--close-ui)
    (hub/org-context-panel--open-ui)))

(defun hub/org-context-panel--comment-at-point ()
  "Return sidecar comment at point in the current source buffer, or nil."
  (get-char-property (point) 'org-comments-comment))

;;;###autoload
(defun hub/org-comments-source-ret-dwim ()
  "Jump from source point to the related item in the context panel.
When point is not inside a commented region, fall back to Evil's normal RET
motion."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Org context panel only works in Org buffers"))
  (cond
   ((org-comments-context-panel-page-marker-at-point-p)
    (hub/org-page-comments-open))
   ((hub/org-context-panel--comment-at-point)
    (let ((comment (hub/org-context-panel--comment-at-point)))
      (org-comments-open)
      (when-let* ((window (hub/org-context-panel--visible-window)))
	(select-window window)
	(org-context-panel-goto-item-key (org-context-panel-item-key comment)))))
   ((fboundp 'evil-ret)
    (call-interactively #'evil-ret))
   (t
    (call-interactively #'newline))))

;;;###autoload
(define-minor-mode hub/org-context-panel-mode
  "Toggle an Org context side panel for the current buffer."
  :lighter " Ctx"
  (if hub/org-context-panel-mode
      (progn
	(add-hook 'post-command-hook #'hub/org-context-panel--post-command-refresh nil t)
	(org-comments-open))
    (remove-hook 'post-command-hook #'hub/org-context-panel--post-command-refresh t)
    (hub/org-context-panel--cancel-refresh-timer)
    (setq hub/org-context-panel--refresh-signature nil)
    (hub/org-context-panel--close-ui)))

(with-eval-after-load 'org-confluence-sync-status
  (setq org-confluence-sync-status-page-context-window-function
	(lambda (source-buffer)
	  (with-current-buffer source-buffer
	    (when (buffer-live-p org-context-panel-bottom-panel-buffer)
	      (get-buffer-window org-context-panel-bottom-panel-buffer t))))
	org-confluence-sync-status-restore-page-context-function
	(lambda (source-buffer window)
	  (with-current-buffer source-buffer
	    (when (buffer-live-p org-context-panel-bottom-panel-buffer)
	      (org-comments-context-panel-enable)
	      (let ((page-buffer (org-context-panel-open-bottom-view
				  'page-comments source-buffer)))
		(when (window-live-p window)
		  (set-window-buffer window page-buffer))
		t))))))

(provide 'org/context-panel)
;;; context-panel.el ends here
