;;; org-copilot-context-panel.el --- Context panel provider for Org Copilot -*- lexical-binding: t; -*-

;; Author: Hubert Behaghel
;; Maintainer: Hubert Behaghel
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (org "9.6"))
;; Keywords: outlines, tools, convenience
;; URL: https://github.com/behaghel/org-copilot

;;; Commentary:
;; Provider glue between Org Copilot ephemeral AI comments and the reusable
;; org-context-panel UI substrate.

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'org-context-panel)
(require 'subr-x)
(require 'org-copilot-model)
(require 'org-copilot-session)
(require 'org-copilot-suggestion)

(defcustom org-copilot-panel-buffer-name "*Org Copilot*"
  "Buffer name used for the Org Copilot side panel."
  :type 'string
  :group 'org-copilot)

(defcustom org-copilot-panel-width 38
  "Width of the Org Copilot side panel."
  :type 'natnum
  :group 'org-copilot)

(defface org-copilot-panel-label-face
  '((t :inherit font-lock-keyword-face :weight bold))
  "Face used for Org Copilot AI labels in panel rows."
  :group 'org-copilot)

(defface org-copilot-panel-status-face
  '((t :inherit shadow))
  "Face used for Org Copilot status labels in panel rows."
  :group 'org-copilot)

(defface org-copilot-panel-focused-face
  '((t :background "#4a3f00" :extend t))
  "Face used for the Org Copilot side-panel row focused by chat.
This face intentionally changes only the background color."
  :group 'org-copilot)

(defface org-copilot-target-face
  '((t :background "#4a3f00" :extend t))
  "Face used to tint the source range focused by Org Copilot chat.
This face intentionally changes only the background color."
  :group 'org-copilot)

(defface org-copilot-target-dim-face
  '((t :background "#2f2a10" :extend t))
  "Face used to tint non-focused Org Copilot source ranges.
This face intentionally changes only the background color."
  :group 'org-copilot)

(defun org-copilot--face-background (light dark)
  "Return LIGHT or DARK depending on the selected frame background mode."
  (if (eq (frame-parameter nil 'background-mode) 'light) light dark))

(defun org-copilot--set-background-only-face (face light dark)
  "Set FACE to a background-only style using LIGHT or DARK color."
  (set-face-attribute face nil
		      :inherit 'unspecified
		      :foreground 'unspecified
		      :background (org-copilot--face-background light dark)
		      :extend t))

(defun org-copilot--apply-background-only-faces ()
  "Ensure Org Copilot highlight faces never alter foreground colors."
  (org-copilot--set-background-only-face
   'org-copilot-target-face "#ffd36a" "#6a4200")
  (org-copilot--set-background-only-face
   'org-copilot-target-dim-face "#fff0c2" "#342414")
  (org-copilot--set-background-only-face
   'org-copilot-panel-focused-face "#ffd36a" "#6a4200"))

(org-copilot--apply-background-only-faces)

(defvar-local org-copilot--overlays nil
  "Source overlays for Org Copilot AI comment targets.")

(defvar-local org-copilot-panel--last-focused-comment-id nil
  "Last Org Copilot comment id focused from this side panel.")

(defvar-local org-copilot-panel--refreshing-focus nil
  "Non-nil while refreshing the side panel after focus changes.")

(defvar org-copilot--workspace-source-buffer nil
  "Last selected source buffer used to retarget Org Copilot panels.")

(defvar org-copilot--workspace-refreshing nil
  "Non-nil while Org Copilot is retargeting auxiliary panels.")

(defvar org-copilot-panel-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'org-context-panel-jump-at-point)
    (define-key map (kbd "d") #'org-copilot-view-diff-at-point)
    (define-key map (kbd "v") #'org-copilot-view-suggestion-at-point)
    (define-key map (kbd "a") #'org-copilot-accept-at-point)
    (define-key map (kbd "x") #'org-copilot-dismiss-at-point)
    (define-key map (kbd "c") #'org-copilot-chat)
    (define-key map (kbd "C-c C-x / a") #'org-copilot-chat-accept-focused-suggestion-at-point)
    (define-key map (kbd "C-c C-x / d") #'org-copilot-chat-dismiss-focused-comment-at-point)
    (define-key map (kbd "C-c C-x / n") #'org-copilot-chat-focus-next-comment)
    (define-key map (kbd "C-c C-x / p") #'org-copilot-chat-focus-previous-comment)
    (define-key map (kbd "C-c C-x / u") #'org-copilot-chat-undo-focused-comment-at-point)
    (define-key map (kbd "C-c C-x / g") #'org-copilot-chat-full-document)
    (define-key map (kbd "C-c C-x / s") #'org-copilot-chat-section)
    (define-key map (kbd "C-c C-x / c") #'org-copilot-chat)
    (define-key map (kbd "C-c C-x / o") #'org-copilot-open-panels)
    (define-key map (kbd "G") #'org-copilot-chat-full-document)
    (define-key map (kbd "g") #'org-copilot-refresh)
    (define-key map (kbd "q") #'org-copilot-close)
    map)
  "Keymap used in Org Copilot side panel buffers.")

(define-derived-mode org-copilot-panel-mode special-mode "Org-Copilot"
  "Major mode for Org Copilot side panel buffers."
  (add-hook 'post-command-hook #'org-copilot-panel-focus-at-point nil t))

(defvar org-copilot-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-x / a") #'org-copilot-chat-accept-focused-suggestion-at-point)
    (define-key map (kbd "C-c C-x / d") #'org-copilot-chat-dismiss-focused-comment-at-point)
    (define-key map (kbd "C-c C-x / n") #'org-copilot-chat-focus-next-comment)
    (define-key map (kbd "C-c C-x / p") #'org-copilot-chat-focus-previous-comment)
    (define-key map (kbd "C-c C-x / u") #'org-copilot-chat-undo-focused-comment-at-point)
    (define-key map (kbd "C-c C-x / g") #'org-copilot-chat-full-document)
    (define-key map (kbd "C-c C-x / s") #'org-copilot-chat-section)
    (define-key map (kbd "C-c C-x / c") #'org-copilot-chat)
    (define-key map (kbd "C-c C-x / o") #'org-copilot-open-panels)
    map)
  "Keymap used by `org-copilot-mode' in source buffers.")

(defun org-copilot-panel-focus-at-point ()
  "Focus source highlighting on the Org Copilot side-panel item at point."
  (when (and (derived-mode-p 'org-copilot-panel-mode)
	     (buffer-live-p org-context-panel-source-buffer)
	     (not org-copilot-panel--refreshing-focus)
	     (not (eq this-command 'org-copilot-chat-full-document)))
    (when-let* ((item (org-context-panel-item-at-point))
		(id (org-copilot-comment-id item)))
      (unless (equal id org-copilot-panel--last-focused-comment-id)
	(let ((org-copilot-panel--refreshing-focus t)
	      (key (org-context-panel-item-key item))
	      (source org-context-panel-source-buffer))
	  (setq org-copilot-panel--last-focused-comment-id id)
	  (with-current-buffer source
	    (org-copilot-chat--set-context
	     source (list :type 'comment :comment-id id))
	    (org-copilot-refresh-overlays))
	  (org-context-panel-render-side-panel source)
	  (org-context-panel-goto-item-key key))))))

(defun org-copilot-context-panel-collect-side-items (_source-buffer)
  "Collect Org Copilot side items for SOURCE-BUFFER."
  (cl-remove-if
   (lambda (comment)
     (eq (org-copilot-comment-status comment) 'dismissed))
   (org-copilot-comments)))

(defun org-copilot-context-panel--status-marker (item)
  "Return compact side-panel marker for AI comment ITEM."
  (pcase (org-copilot-comment-status item)
    ('accepted "✅")
    ('stale "⚠️")
    (_ (cond
	((org-copilot-suggestion-section-comment-p item) "§✏️")
	((plist-get item :suggestion) "✏️")
	(t "💬")))))

(defun org-copilot-context-panel--summary (item)
  "Return compact side-panel summary text for ITEM."
  (let* ((text (or (plist-get item :summary)
		   (plist-get item :body)
		   ""))
	 (single-line (string-trim
		       (replace-regexp-in-string "[\n\t ]+" " " text))))
    (if (> (length single-line) 72)
	(concat (substring single-line 0 71) "…")
      single-line)))

(defun org-copilot-context-panel-render-side-item (source-buffer item)
  "Render one Org Copilot side-panel ITEM."
  (let* ((marker (org-copilot-context-panel--status-marker item))
	 (summary (org-copilot-context-panel--summary item))
	 (prefix (concat marker " "))
	 (focused-p (with-current-buffer source-buffer
		      (equal org-copilot-chat-focus-comment-id
			     (org-copilot-comment-id item))))
	 (start (point))
	 (fill-column 36))
    (insert (propertize prefix 'face 'org-copilot-panel-label-face)
	    summary)
    (when focused-p
      (add-face-text-property start (point) 'org-copilot-panel-focused-face))
    (fill-region (line-beginning-position) (point))
    (insert "\n")))

(defun org-copilot-context-panel-jump-side-item (source-buffer item)
  "Jump from context-panel ITEM to its source location in SOURCE-BUFFER."
  (let ((position (plist-get item :source-start)))
    (unless (and (buffer-live-p source-buffer) position)
      (user-error "AI comment has no source location"))
    (pop-to-buffer source-buffer)
    (goto-char position)))

(defun org-copilot-delete-overlays ()
  "Delete Org Copilot source overlays in the current buffer."
  (mapc #'delete-overlay org-copilot--overlays)
  (setq org-copilot--overlays nil))

(defun org-copilot--overlayable-comment-p (comment)
  "Return non-nil when COMMENT should render a source target overlay."
  (and (not (eq (org-copilot-comment-status comment) 'dismissed))
       (integerp (plist-get comment :source-start))
       (integerp (plist-get comment :source-end))
       (<= (point-min) (plist-get comment :source-start))
       (<= (plist-get comment :source-start) (plist-get comment :source-end))
       (<= (plist-get comment :source-end) (point-max))))

(defun org-copilot--source-overlay-bounds (comment)
  "Return source overlay bounds for COMMENT.
Scope comments may concern a whole subtree, but their source marker should only
highlight the section title line."
  (let ((start (plist-get comment :source-start))
	(end (plist-get comment :source-end)))
    (if (eq (plist-get comment :type) 'scope)
	(save-excursion
	  (goto-char start)
	  (cons start (min (line-end-position) end)))
      (cons start end))))

(defun org-copilot--target-face-for-comment (comment focus-id focused-seen)
  "Return source target face for COMMENT.
FOCUS-ID is the currently focused comment id.  FOCUSED-SEEN is non-nil when a
previous overlay already claimed the focused face."
  (if (and focus-id
	   (not focused-seen)
	   (equal focus-id (org-copilot-comment-id comment)))
      'org-copilot-target-face
    'org-copilot-target-dim-face))

(defun org-copilot-refresh-overlays ()
  "Refresh source overlays for Org Copilot AI comment targets."
  (org-copilot-delete-overlays)
  (let ((focus-id org-copilot-chat-focus-comment-id)
	focused-seen)
    (dolist (comment (org-copilot-comments))
      (when (org-copilot--overlayable-comment-p comment)
	(let* ((face (org-copilot--target-face-for-comment
		      comment focus-id focused-seen))
	       (bounds (org-copilot--source-overlay-bounds comment))
	       (overlay (make-overlay (car bounds)
				      (cdr bounds)
				      nil t nil)))
	  (when (eq face 'org-copilot-target-face)
	    (setq focused-seen t))
	  (overlay-put overlay 'face face)
	  (overlay-put overlay 'org-copilot-comment-id
		       (org-copilot-comment-id comment))
	  (push overlay org-copilot--overlays)))))
  (setq org-copilot--overlays (nreverse org-copilot--overlays)))

(defun org-copilot-context-panel-provider ()
  "Return the Org Copilot context-panel provider descriptor."
  (list :name 'copilot
	:priority 20
	:collect-side-items #'org-copilot-context-panel-collect-side-items
	:render-side-item #'org-copilot-context-panel-render-side-item
	:jump-side-item #'org-copilot-context-panel-jump-side-item
	:side-panel-mode #'org-copilot-panel-mode
	:side-panel-buffer-name org-copilot-panel-buffer-name
	:side-panel-width org-copilot-panel-width))

(defun org-copilot--auxiliary-buffer-p (buffer)
  "Return non-nil when BUFFER is an Org Copilot auxiliary buffer."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (or (derived-mode-p 'org-copilot-panel-mode)
	  (derived-mode-p 'org-copilot-chat-mode)
	  (derived-mode-p 'org-copilot-diff-mode)))))

(defun org-copilot--copilot-source-buffer-p (buffer)
  "Return non-nil when BUFFER has `org-copilot-mode' enabled."
  (and (buffer-live-p buffer)
       (buffer-local-value 'org-copilot-mode buffer)))

(defun org-copilot--close-buffer-window (buffer)
  "Close BUFFER's visible window and kill BUFFER when it is live."
  (when (buffer-live-p buffer)
    (when-let* ((window (get-buffer-window buffer t)))
      (unless (one-window-p t)
	(delete-window window)))
    (when (buffer-live-p buffer)
      (kill-buffer buffer))))

(defun org-copilot--close-auxiliary-panels ()
  "Close Org Copilot auxiliary panels for the previous source."
  (org-copilot--close-buffer-window (get-buffer org-copilot-panel-buffer-name))
  (when (boundp 'org-copilot-chat-buffer-name)
    (org-copilot--close-buffer-window (get-buffer org-copilot-chat-buffer-name)))
  (when (boundp 'org-copilot-diff-buffer-name)
    (org-copilot--close-buffer-window (get-buffer org-copilot-diff-buffer-name))))

(defun org-copilot--retarget-visible-panels (source-buffer)
  "Retarget visible Org Copilot panels to SOURCE-BUFFER."
  (when-let* ((panel (get-buffer org-copilot-panel-buffer-name)))
    (when (get-buffer-window panel t)
      (with-current-buffer panel
	(setq org-context-panel-source-buffer source-buffer)
	(org-context-panel-render-side-panel source-buffer))))
  (when (and (boundp 'org-copilot-chat-buffer-name)
	     (get-buffer org-copilot-chat-buffer-name))
    (let ((chat (get-buffer org-copilot-chat-buffer-name)))
      (when (get-buffer-window chat t)
	(with-current-buffer chat
	  (setq org-copilot-chat-source-buffer source-buffer)
	  (setq org-context-panel-source-buffer source-buffer)
	  (org-copilot-chat-render source-buffer))
	(org-copilot-chat-sync-diff source-buffer)))))

(defun org-copilot--selected-source-buffer ()
  "Return selected buffer when it is a source buffer, or nil."
  (let ((buffer (window-buffer (selected-window))))
    (unless (or (minibufferp buffer)
		(org-copilot--auxiliary-buffer-p buffer))
      buffer)))

(defun org-copilot--window-selection-changed (_frame)
  "Retarget or close Org Copilot panels after selected window changes."
  (unless org-copilot--workspace-refreshing
    (when-let* ((source (org-copilot--selected-source-buffer)))
      (unless (eq source org-copilot--workspace-source-buffer)
	(let ((org-copilot--workspace-refreshing t))
	  (setq org-copilot--workspace-source-buffer source)
	  (if (org-copilot--copilot-source-buffer-p source)
	      (org-copilot--retarget-visible-panels source)
	    (org-copilot--close-auxiliary-panels)))))))

(defun org-copilot--ensure-window-watch ()
  "Install Org Copilot source-window tracking hook."
  (add-hook 'window-selection-change-functions
	    #'org-copilot--window-selection-changed))

(defun org-copilot-context-panel-enable ()
  "Enable Org Copilot as a context-panel provider in the current buffer."
  (org-copilot--ensure-window-watch)
  (setq org-copilot--workspace-source-buffer (current-buffer))
  (org-context-panel-register-provider (org-copilot-context-panel-provider))
  (org-context-panel-mode 1))

(defun org-copilot-context-panel-disable ()
  "Disable Org Copilot as a context-panel provider in the current buffer."
  (org-context-panel-unregister-provider 'copilot)
  (unless (org-context-panel-registered-providers)
    (org-context-panel-mode -1)))

;;;###autoload
(define-minor-mode org-copilot-mode
  "Enable Org Copilot context-panel integration for the current Org buffer."
  :lighter " Copilot"
  (if org-copilot-mode
      (progn
	(org-copilot-context-panel-enable)
	(org-copilot-refresh-overlays))
    (org-copilot-delete-overlays)
    (org-copilot-context-panel-disable)))

;;;###autoload
(defun org-copilot-open ()
  "Open or refresh the Org Copilot side panel for the current Org buffer."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Org Copilot needs an Org source buffer"))
  (org-copilot-mode 1)
  (org-context-panel-open (current-buffer)))

;;;###autoload
(defun org-copilot-open-panels ()
  "Open Org Copilot side and chat panels for the current Org buffer."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Org Copilot needs an Org source buffer"))
  (org-copilot-mode 1)
  (org-copilot-open)
  (org-copilot-chat-full-document))

;;;###autoload
(defun org-copilot-refresh ()
  "Refresh the Org Copilot side panel from current session state."
  (interactive)
  (org-context-panel-refresh))

;;;###autoload
(defun org-copilot-close ()
  "Close the Org Copilot side panel for the current source."
  (interactive)
  (org-context-panel-close))

(provide 'org-copilot-context-panel)
;;; org-copilot-context-panel.el ends here
