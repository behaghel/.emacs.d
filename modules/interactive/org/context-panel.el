;;; context-panel.el --- Org mode context side panel -*- lexical-binding: t; -*-

;;; Commentary:
;; Read-only right-side panel for contextual Org authoring records.  Native Org
;; footnotes are currently the first data source; sidecar comments will follow.

;;; Code:

(require 'org-comments)
(require 'org-comments-context-panel)
(require 'context-panels)
(require 'org-marginalia-context-panel)
(require 'org)
(require 'org-comments-panel-actions)
(require 'org-comments-page)
(require 'org-comments-panel-filter)
(require 'org-comments-ui)
(require 'subr-x)

(autoload 'org-confluence-comments-push-current "org-confluence-comments-push" nil t)
(autoload 'org-copilot-visualize-at-point "org-copilot-diff" nil t)

(defun hub/org-technical-buffer--evil-normalize-map (map bindings)
  "Apply Evil normal-state BINDINGS to technical buffer MAP."
  (when (fboundp 'evil-define-key)
    (dolist (binding bindings)
      (evil-define-key 'normal map (kbd (car binding)) (cdr binding)))))

(with-eval-after-load 'evil
  (dolist (mode '(org-comments-panel-mode
		  context-panels-buffer-mode
		  org-copilot-panel-mode
		  org-copilot-chat-mode
		  org-copilot-diff-mode))
    (evil-set-initial-state mode 'normal)))

(with-eval-after-load 'org-comments-panel
  (define-key org-comments-panel-mode-map (kbd "v")
	      #'org-copilot-visualize-at-point)
  (with-eval-after-load 'evil
    (hub/org-technical-buffer--evil-normalize-map
     org-comments-panel-mode-map
     '(("RET" . context-panels-jump-at-point)
       ("v" . org-copilot-visualize-at-point)
       ("d" . org-comments-delete)
       ("e" . org-comments-edit)
       ("g" . org-comments-panel-refresh)
       ("m" . org-comments-panel-status-map)
       ("D" . org-comments-pull)
       ("O" . org-comments-open-remote)
       ("o" . org-comments-open-remote)
       ("p" . org-comments-page-open-at-point)
       ("S" . org-comments-sync)
       ("U" . org-comments-push)
       ("q" . org-comments-close-current-ui)
       ("r" . org-comments-reply)
       ("z" . org-comments-panel-filter-map)
       ("]c" . org-comments-next-item-at-point)
       ("[c" . org-comments-previous-item-at-point)))))

(with-eval-after-load 'org-copilot-context-panel
  (with-eval-after-load 'evil
    (hub/org-technical-buffer--evil-normalize-map
     org-copilot-panel-mode-map
     '(("RET" . context-panels-jump-at-point)
       ("d" . org-copilot-view-diff-at-point)
       ("v" . org-copilot-visualize-at-point)
       ("a" . org-copilot-accept-at-point)
       ("x" . org-copilot-dismiss-at-point)
       ("n" . org-copilot-panel-next-item)
       ("p" . org-copilot-panel-previous-item)
       ("]c" . org-copilot-panel-next-item)
       ("[c" . org-copilot-panel-previous-item)
       ("G" . org-copilot-chat-full-document)
       ("g" . org-copilot-refresh)
       ("q" . org-copilot-close)))))

(with-eval-after-load 'org-copilot-chat
  (with-eval-after-load 'evil
    (hub/org-technical-buffer--evil-normalize-map
     org-copilot-chat-mode-map
     '(("RET" . org-copilot-chat-return-dwim)
       ("/" . org-copilot-chat-slash-or-complete)
       ("M-a" . org-copilot-chat-accept-focused-suggestion-at-point)
       ("M-d" . org-copilot-chat-dismiss-focused-comment-at-point)
       ("M-n" . org-copilot-chat-focus-next-comment)
       ("M-p" . org-copilot-chat-focus-previous-comment)
       ("M-u" . org-copilot-chat-undo-focused-comment-at-point)
       ("M-g" . org-copilot-chat-full-document)
       ("M-s" . org-copilot-chat-section)
       ("M-<up>" . org-copilot-chat-recall-last-prompt)))))

(with-eval-after-load 'org-copilot-diff
  (with-eval-after-load 'evil
    (hub/org-technical-buffer--evil-normalize-map
     org-copilot-diff-mode-map
     '(("a" . org-copilot-accept-at-point)
       ("q" . org-copilot-close-diff)))))

(defgroup hub/context-panels nil
  "Interactive Org context panel."
  :group 'org)

(defcustom hub/context-panels-buffer-name "*Org Context*"
  "Buffer name used for the Org context side panel."
  :type 'string
  :group 'hub/context-panels)

(defcustom hub/context-panels-width 38
  "Width of the Org context side panel."
  :type 'natnum
  :group 'hub/context-panels)

(defcustom hub/context-panels-dock-prose t
  "Whether opening the context panel docks visually filled prose toward it."
  :type 'boolean
  :group 'hub/context-panels)

(defcustom hub/context-panels-refresh-idle-delay 0.25
  "Idle delay before refreshing a visible Org context panel after commands."
  :type 'number
  :group 'hub/context-panels)

(defvar-local hub/context-panels--visual-fill-state nil
  "Saved visual-fill-column state while context panel docks prose.")

(defvar-local hub/context-panels--refresh-timer nil
  "Pending idle timer for refreshing this buffer's context panel.")

(defvar-local hub/context-panels--refresh-signature nil
  "Last viewport/content signature used for context-panel refresh.")

;;;###autoload
(defun hub/context-panels--visual-fill-total-margin (source-window)
  "Return total visual-fill-column margin for SOURCE-WINDOW."
  (let* ((width (or (and (boundp 'visual-fill-column-width)
			 (numberp visual-fill-column-width)
			 visual-fill-column-width)
		    fill-column))
	 (total-width (if (fboundp 'visual-fill-column--window-max-text-width)
			  (visual-fill-column--window-max-text-width source-window)
			(window-body-width source-window))))
    (max 0 (- total-width width))))

(defun hub/context-panels--dock-prose (&optional source-window)
  "Dock visually filled prose in SOURCE-WINDOW toward the context panel."
  (let ((source-window (or source-window context-panels-current-source-window)))
    (when (and hub/context-panels-dock-prose
	       (window-live-p source-window)
	       (require 'visual-fill-column nil t))
      (with-current-buffer (window-buffer source-window)
	(when (bound-and-true-p visual-fill-column-mode)
	  (unless hub/context-panels--visual-fill-state
	    (setq hub/context-panels--visual-fill-state
		  (list :center (and (boundp 'visual-fill-column-center-text)
				     visual-fill-column-center-text)
			:extra (and (boundp 'visual-fill-column-extra-text-width)
				    visual-fill-column-extra-text-width))))
	  (let* ((margin (hub/context-panels--visual-fill-total-margin source-window))
		 (half (/ margin 2)))
	    ;; `visual-fill-column' can only left-dock or center text directly.  Keep
	    ;; centering enabled, then shift the centered margins right by expanding the
	    ;; left margin and collapsing the right margin.
	    (setq-local visual-fill-column-center-text t
			visual-fill-column-extra-text-width (cons (- half) half))
	    (when (fboundp 'visual-fill-column--set-margins)
	      (visual-fill-column--set-margins source-window))))))))

(defun hub/context-panels--restore-prose-docking (&optional source-buffer)
  "Restore visual-fill-column state saved for SOURCE-BUFFER."
  (let ((source-buffer (or source-buffer context-panels-current-source-buffer)))
    (when (and (buffer-live-p source-buffer)
	       (require 'visual-fill-column nil t))
      (with-current-buffer source-buffer
	(when hub/context-panels--visual-fill-state
	  (setq-local visual-fill-column-center-text
		      (plist-get hub/context-panels--visual-fill-state :center)
		      visual-fill-column-extra-text-width
		      (plist-get hub/context-panels--visual-fill-state :extra)
		      hub/context-panels--visual-fill-state nil)
	  (when-let* ((window (get-buffer-window source-buffer t)))
	    (when (fboundp 'visual-fill-column--set-margins)
	      (visual-fill-column--set-margins window))))))))

(add-hook 'context-panels-after-side-panel-open-hook
	  #'hub/context-panels--dock-prose)
(add-hook 'context-panels-before-side-panel-close-hook
	  #'hub/context-panels--restore-prose-docking)

(defun hub/context-panels--cancel-refresh-timer ()
  "Cancel this buffer's pending context-panel refresh timer."
  (when (timerp hub/context-panels--refresh-timer)
    (cancel-timer hub/context-panels--refresh-timer))
  (setq hub/context-panels--refresh-timer nil))

(defun hub/context-panels--side-visible-p (&optional source-buffer)
  "Return non-nil when SOURCE-BUFFER has a visible side context panel."
  (let ((source (or source-buffer (current-buffer))))
    (with-current-buffer source
      (and (buffer-live-p context-panels-side-panel-buffer)
	   (get-buffer-window context-panels-side-panel-buffer t)))))

(defun hub/context-panels--bottom-visible-p (&optional source-buffer)
  "Return non-nil when SOURCE-BUFFER has a visible bottom context view."
  (let ((source (or source-buffer (current-buffer))))
    (with-current-buffer source
      (and (buffer-live-p context-panels-bottom-panel-buffer)
	   (get-buffer-window context-panels-bottom-panel-buffer t)))))

(defun hub/context-panels--visible-p (&optional source-buffer)
  "Return non-nil when SOURCE-BUFFER has any visible context panel."
  (or (hub/context-panels--side-visible-p source-buffer)
      (hub/context-panels--bottom-visible-p source-buffer)))

(defun hub/context-panels--refresh-after-idle (source-buffer)
  "Refresh visible context panels for SOURCE-BUFFER after Emacs becomes idle."
  (when (buffer-live-p source-buffer)
    (with-current-buffer source-buffer
      (setq hub/context-panels--refresh-timer nil)
      (when (and hub/context-panels-mode
		 (hub/context-panels--visible-p source-buffer))
	(save-selected-window
	  (hub/context-panels--refresh-visible-panels source-buffer))))))

(defun hub/context-panels--refresh-signature (&optional source-buffer)
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

(defun hub/context-panels--post-command-refresh ()
  "Schedule a visible context panel refresh after source-buffer commands."
  (when (and hub/context-panels-mode
	     (hub/context-panels--visible-p))
    (let ((signature (hub/context-panels--refresh-signature)))
      (unless (equal signature hub/context-panels--refresh-signature)
	(setq hub/context-panels--refresh-signature signature)
	(hub/context-panels--cancel-refresh-timer)
	(setq hub/context-panels--refresh-timer
	      (run-with-idle-timer hub/context-panels-refresh-idle-delay nil
				   #'hub/context-panels--refresh-after-idle
				   (current-buffer)))))))

(defun hub/context-panels--enable-comments-provider ()
  "Enable package context providers with personal side-panel naming."
  (let ((org-comments-panel-buffer-name hub/context-panels-buffer-name)
	(comments-provider (context-panels-registered-provider 'comments)))
    (when (or (not comments-provider)
	      (not (equal (plist-get comments-provider :side-panel-buffer-name)
			  hub/context-panels-buffer-name)))
      (org-comments-context-panel-enable))
    (unless (context-panels-registered-provider 'marginalia)
      (org-marginalia-context-panel-mode 1))
    (unless context-panels-mode
      (context-panels-mode 1))))

(defun hub/context-panels--page-open-ui ()
  "Open the configured package page comments UI."
  (hub/context-panels--open-page-view t t))

(defun hub/context-panels--refresh-ui ()
  "Refresh the configured package context panel UI."
  (unless (derived-mode-p 'org-mode)
    (user-error "Org context panel only works in Org buffers"))
  (hub/context-panels--enable-comments-provider)
  (context-panels-refresh-source-overlays)
  (setq hub/context-panels--refresh-signature
	(hub/context-panels--refresh-signature))
  (context-panels-refresh))

(defun hub/context-panels--open-comment-ui (comment-id &optional jump-position)
  "Open COMMENT-ID through the configured package context panel UI."
  (unless (derived-mode-p 'org-mode)
    (user-error "Org context panel only works in Org buffers"))
  (when jump-position
    (goto-char jump-position))
  (let ((panel (hub/context-panels--open-ui)))
    (with-current-buffer panel
      (hub/context-panels--goto-comment-id comment-id))
    panel))

(defun hub/context-panels--page-open-comment-ui (comment-id)
  "Open page COMMENT-ID through the configured package context panel UI."
  (let ((panel (hub/context-panels--open-page-view t t)))
    (unless panel
      (user-error "No page context panel available"))
    (with-current-buffer panel
      (hub/context-panels--goto-comment-id comment-id))
    panel))

(setq org-comments-ui-open-function #'hub/context-panels--open-ui
      org-comments-ui-page-open-function #'hub/context-panels--page-open-ui
      org-comments-ui-refresh-function #'hub/context-panels--refresh-ui
      org-comments-ui-open-comment-function #'hub/context-panels--open-comment-ui
      org-comments-ui-page-open-comment-function #'hub/context-panels--page-open-comment-ui)

(defun hub/context-panels--visible-window ()
  "Return the visible context panel window, or nil."
  (or (when (buffer-live-p context-panels-side-panel-buffer)
	(get-buffer-window context-panels-side-panel-buffer t))
      (when-let* ((panel (get-buffer hub/context-panels-buffer-name)))
	(get-buffer-window panel t))))

(defun hub/context-panels--toggle-filter (property)
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

(defun hub/context-panels-filter-toggle-actionable ()
  "Toggle actionable-only context filtering."
  (interactive)
  (hub/context-panels--toggle-filter :actionable))

(defun hub/context-panels-filter-toggle-drafts ()
  "Toggle draft/local-edit-only context filtering."
  (interactive)
  (hub/context-panels--toggle-filter :drafts))

(defun hub/context-panels-filter-toggle-mine ()
  "Toggle current-user-only context filtering."
  (interactive)
  (hub/context-panels--toggle-filter :mine))

(defun hub/context-panels-filter-toggle-missing ()
  "Toggle whether remote-missing context items are shown."
  (interactive)
  (hub/context-panels--toggle-filter :show-missing))

(with-eval-after-load 'org-comments-panel
  (define-key org-comments-panel-filter-map (kbd "a")
	      #'hub/context-panels-filter-toggle-actionable)
  (define-key org-comments-panel-filter-map (kbd "d")
	      #'hub/context-panels-filter-toggle-drafts)
  (define-key org-comments-panel-filter-map (kbd "m")
	      #'hub/context-panels-filter-toggle-mine)
  (define-key org-comments-panel-filter-map (kbd "x")
	      #'hub/context-panels-filter-toggle-missing))

(defun hub/context-panels--pulse-current-line ()
  "Briefly highlight the current context panel line when possible."
  (when (fboundp 'pulse-momentary-highlight-one-line)
    (pulse-momentary-highlight-one-line (point))))

(defun hub/context-panels--goto-comment-id (comment-id)
  "Move point to COMMENT-ID in the current context panel and highlight it."
  (interactive "sComment ID: ")
  (unless (context-panels-goto-item-key comment-id)
    (user-error "Comment %s is not visible in this context panel" comment-id))
  (when-let* ((window (get-buffer-window (current-buffer) t)))
    (set-window-point window (point))
    (with-selected-window window
      (recenter)))
  (hub/context-panels--pulse-current-line)
  comment-id)

(defun hub/context-panels--close-page-view (&optional source-buffer)
  "Close page-context window for SOURCE-BUFFER or current source."
  (let ((source (or source-buffer
		    (if (derived-mode-p 'org-comments-panel-mode 'context-panels-buffer-mode)
			context-panels-source-buffer
		      (current-buffer)))))
    (when (buffer-live-p source)
      (with-current-buffer source
	(when (buffer-live-p context-panels-bottom-panel-buffer)
	  (context-panels-close-bottom-view))))))

(defun hub/context-panels--open-page-view (&optional select show-empty)
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
	(let ((page-buffer (context-panels-open-bottom-view
			    'page-comments source-buffer)))
	  (when-let* ((window (get-buffer-window page-buffer t)))
	    (when select
	      (select-window window)))
	  page-buffer)
      (hub/context-panels--close-page-view source-buffer)
      nil)))

(defun hub/context-panels--refresh-visible-panels (source)
  "Refresh visible context panels for SOURCE without changing selection."
  (when (buffer-live-p source)
    (with-current-buffer source
      (hub/context-panels--enable-comments-provider)
      (when (and (buffer-live-p context-panels-side-panel-buffer)
		 (get-buffer-window context-panels-side-panel-buffer t))
	(context-panels-refresh))
      (when (and (buffer-live-p context-panels-bottom-panel-buffer)
		 (get-buffer-window context-panels-bottom-panel-buffer t))
	(with-current-buffer context-panels-bottom-panel-buffer
	  (context-panels-refresh-bottom-view))))))

(defun hub/context-panels-revert-buffer (&optional _ignore-auto _noconfirm)
  "Refresh the current context panel buffer for `revert-buffer'."
  (unless (buffer-live-p context-panels-source-buffer)
    (user-error "No source buffer for this context panel"))
  (let ((source context-panels-source-buffer))
    (if context-panels-view-id
	(context-panels-refresh-bottom-view)
      (context-panels-refresh))
    (hub/context-panels--refresh-visible-panels source)
    (message "Refreshed Org context panel")))

;;;###autoload
(defun hub/context-panels--close-ui ()
  "Close the context side panel associated with the current buffer."
  (interactive)
  (let* ((panel-window (hub/context-panels--visible-window))
	 (source-buffer (cond
			 ((derived-mode-p 'org-comments-panel-mode 'context-panels-buffer-mode)
			  context-panels-source-buffer)
			 ((and (window-live-p panel-window)
			       (buffer-live-p (window-buffer panel-window)))
			  (with-current-buffer (window-buffer panel-window)
			    context-panels-source-buffer))
			 ((derived-mode-p 'context-panels-buffer-mode)
			  context-panels-source-buffer)
			 (t (current-buffer)))))
    (when (buffer-live-p source-buffer)
      (with-current-buffer source-buffer
	(unless org-comments-mode
	  (org-comments-context-panel-delete-overlays))
	(hub/context-panels--close-page-view source-buffer)
	(when (buffer-live-p context-panels-side-panel-buffer)
	  (context-panels-close))))
    (when (window-live-p panel-window)
      (delete-window panel-window))))

(defun hub/context-panels--copilot-chat-session-p ()
  "Return non-nil when current Org buffer has pending Org Copilot chat state."
  (or (and (fboundp 'org-copilot-chat-messages)
	   (org-copilot-chat-messages))
      (and (boundp 'org-copilot-chat-buffer-name)
	   (buffer-live-p (get-buffer org-copilot-chat-buffer-name)))))

(defun hub/context-panels--open-ui ()
  "Open side context panel and surface pending bottom views when appropriate."
  (unless (derived-mode-p 'org-mode)
    (user-error "Org context panel only works in Org buffers"))
  (let ((source-buffer (current-buffer))
	(surface-copilot-chat (hub/context-panels--copilot-chat-session-p)))
    (hub/context-panels--enable-comments-provider)
    (context-panels-refresh-source-overlays)
    (setq hub/context-panels--refresh-signature
	  (hub/context-panels--refresh-signature source-buffer))
    (let ((panel (context-panels-open source-buffer)))
      (with-current-buffer source-buffer
	(hub/context-panels--open-page-view nil nil)
	(when (and surface-copilot-chat
		   (fboundp 'org-copilot-chat))
	  (org-copilot-chat)))
      panel)))

;;;###autoload
(defun hub/context-panels-toggle-open ()
  "Cycle context UI: open side, close all, then reopen all.
When only a bottom chat/view is visible, opening the side panel keeps and
surfaces that pending bottom session instead of closing it."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Org context panel only works in Org buffers"))
  (if (hub/context-panels--side-visible-p)
      (hub/context-panels--close-ui)
    (hub/context-panels--open-ui)))

(defun hub/context-panels--comment-at-point ()
  "Return sidecar comment at point in the current source buffer, or nil."
  (get-char-property (point) 'org-comments-comment))

;;;###autoload
(defun hub/comments-source-ret-dwim ()
  "Jump from source point to the related item in the context panel.
When point is not inside a commented region, fall back to Evil's normal RET
motion."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Org context panel only works in Org buffers"))
  (cond
   ((org-comments-context-panel-page-marker-at-point-p)
    (hub/org-page-comments-open))
   ((hub/context-panels--comment-at-point)
    (let ((comment (hub/context-panels--comment-at-point)))
      (org-comments-open)
      (when-let* ((window (hub/context-panels--visible-window)))
	(select-window window)
	(context-panels-goto-item-key (context-panels-item-key comment)))))
   ((fboundp 'evil-ret)
    (call-interactively #'evil-ret))
   (t
    (call-interactively #'newline))))

;;;###autoload
(define-minor-mode hub/context-panels-mode
  "Toggle an Org context side panel for the current buffer."
  :lighter " Ctx"
  (if hub/context-panels-mode
      (progn
	(add-hook 'post-command-hook #'hub/context-panels--post-command-refresh nil t)
	(org-comments-open))
    (remove-hook 'post-command-hook #'hub/context-panels--post-command-refresh t)
    (hub/context-panels--cancel-refresh-timer)
    (setq hub/context-panels--refresh-signature nil)
    (hub/context-panels--close-ui)))

(with-eval-after-load 'org-confluence-sync-status
  (setq org-confluence-sync-status-page-context-window-function
	(lambda (source-buffer)
	  (with-current-buffer source-buffer
	    (when (buffer-live-p context-panels-bottom-panel-buffer)
	      (get-buffer-window context-panels-bottom-panel-buffer t))))
	org-confluence-sync-status-restore-page-context-function
	(lambda (source-buffer window)
	  (with-current-buffer source-buffer
	    (when (buffer-live-p context-panels-bottom-panel-buffer)
	      (org-comments-context-panel-enable)
	      (let ((page-buffer (context-panels-open-bottom-view
				  'page-comments source-buffer)))
		(when (window-live-p window)
		  (set-window-buffer window page-buffer))
		t))))))

(provide 'org/context-panel)
;;; context-panel.el ends here
