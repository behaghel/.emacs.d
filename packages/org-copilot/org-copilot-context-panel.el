;;; org-copilot-context-panel.el --- Context panel provider for Org Copilot -*- lexical-binding: t; -*-

;; Author: Hubert Behaghel
;; Maintainer: Hubert Behaghel
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (org "9.6"))
;; Keywords: outlines, tools, convenience
;; URL: https://github.com/behaghel/org-copilot

;;; Commentary:
;; Provider glue between Org Copilot chat/diff surfaces and context-panels.

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'context-panels)
(require 'subr-x)
(require 'org-copilot-model)
(require 'org-copilot-session)
(require 'org-comments nil 'noerror)

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
  )

(org-copilot--apply-background-only-faces)

(defvar-local org-copilot--overlays nil
  "Source overlays for Org Copilot AI comment targets.")

(defvar org-copilot-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-x / a") #'org-copilot-chat-accept-focused-suggestion-at-point)
    (define-key map (kbd "C-c C-x / d") #'org-copilot-chat-dismiss-focused-comment-at-point)
    (define-key map (kbd "C-c C-x / n") #'context-panels-next-item)
    (define-key map (kbd "C-c C-x / p") #'context-panels-previous-item)
    (define-key map (kbd "C-c C-x / u") #'org-copilot-chat-undo-focused-comment-at-point)
    (define-key map (kbd "C-c C-x / g") #'org-copilot-chat-full-document)
    (define-key map (kbd "C-c C-x / s") #'org-copilot-chat-section)
    (define-key map (kbd "C-c C-x / c") #'org-copilot-chat)
    (define-key map (kbd "C-c C-x / o") #'org-copilot-open-panels)
    map)
  "Keymap used by `org-copilot-mode' in source buffers.")

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
    (dolist (comment (org-copilot-visible-comments))
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
  "Return the Org Copilot context-panel provider descriptor.
Copilot contributes chat and transient auxiliary behavior only; comment side-panel
UI is owned by the unified `org-comments' provider."
  (list :name 'copilot
	:icon "🤖"
	:priority 20
	:collect-bottom-views #'org-copilot-chat-bottom-views
	:cleanup-auxiliary #'org-copilot--cleanup-transient-auxiliary))

(defun org-copilot--auxiliary-buffer-p (buffer)
  "Return non-nil when BUFFER is an Org Copilot auxiliary buffer."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (or (derived-mode-p 'org-copilot-chat-mode)
	  (derived-mode-p 'org-copilot-diff-mode)))))

(defun org-copilot--copilot-source-buffer-p (buffer)
  "Return non-nil when BUFFER has `org-copilot-mode' enabled."
  (and (buffer-live-p buffer)
       (buffer-local-value 'org-copilot-mode buffer)))

(defun org-copilot--close-buffer-window (buffer)
  "Close BUFFER's visible window and bury BUFFER when it is live."
  (when (buffer-live-p buffer)
    (when-let* ((window (get-buffer-window buffer t)))
      (unless (one-window-p t)
	(delete-window window)))
    (when (buffer-live-p buffer)
      (bury-buffer buffer))))

(defun org-copilot--close-auxiliary-panels ()
  "Close Org Copilot auxiliary panels for the previous source."
  (when (boundp 'org-copilot-chat-buffer-name)
    (org-copilot--close-buffer-window (get-buffer org-copilot-chat-buffer-name)))
  (when (boundp 'org-copilot-diff-buffer-name)
    (org-copilot--close-buffer-window (get-buffer org-copilot-diff-buffer-name))))

(defun org-copilot--cleanup-transient-auxiliary (_source-buffer)
  "Close transient Org Copilot auxiliary previews."
  (when (boundp 'org-copilot-diff-buffer-name)
    (org-copilot--close-buffer-window (get-buffer org-copilot-diff-buffer-name))))

(defun org-copilot--retarget-visible-panels (source-buffer)
  "Retarget visible Org Copilot panels to SOURCE-BUFFER."
  (when (and (boundp 'org-copilot-chat-buffer-name)
	     (get-buffer org-copilot-chat-buffer-name))
    (let ((chat (get-buffer org-copilot-chat-buffer-name)))
      (when (get-buffer-window chat t)
	(with-current-buffer chat
	  (setq org-copilot-chat-source-buffer source-buffer)
	  (setq context-panels-source-buffer source-buffer)
	  (org-copilot-chat-render source-buffer))
	(org-copilot-chat-sync-diff source-buffer)))))

(defun org-copilot--selected-source-buffer ()
  "Return selected buffer when it is a source buffer, or nil."
  (let ((buffer (window-buffer (selected-window))))
    (unless (or (minibufferp buffer)
		(org-copilot--auxiliary-buffer-p buffer))
      buffer)))

(defun org-copilot--window-selection-changed (_frame)
  "Schedule generic context-panel reconciliation after selection changes."
  (unless org-copilot--workspace-refreshing
    (when (fboundp 'context-panels--schedule-reconcile)
      (context-panels--schedule-reconcile))))

(defun org-copilot--ensure-window-watch ()
  "Install Org Copilot source-window tracking hook."
  (add-hook 'window-selection-change-functions
	    #'org-copilot--window-selection-changed))

(defun org-copilot-context-panel-enable ()
  "Enable Org Copilot as a context-panel provider in the current buffer."
  (org-copilot--ensure-window-watch)
  (setq org-copilot--workspace-source-buffer (current-buffer))
  (when (fboundp 'org-comments-mode)
    (org-comments-mode 1))
  (context-panels-register-provider (org-copilot-context-panel-provider))
  (context-panels-mode 1))

(defun org-copilot-context-panel-disable ()
  "Disable Org Copilot as a context-panel provider in the current buffer."
  (context-panels-unregister-provider 'copilot)
  (unless (context-panels-registered-providers)
    (context-panels-mode -1)))

;;;###autoload
(define-minor-mode org-copilot-mode
  "Enable Org Copilot context-panel integration for the current Org buffer."
  :lighter " Copilot"
  (if org-copilot-mode
      (progn
	(org-copilot-context-panel-enable)
	(org-copilot-restore-durable-artifacts)
	(org-copilot-refresh-overlays))
    (org-copilot-delete-overlays)
    (org-copilot-context-panel-disable)))

(defun org-copilot--active-source-buffer ()
  "Return the Org source buffer for commands run from source or aux buffers."
  (cond
   ((buffer-live-p context-panels-source-buffer)
    context-panels-source-buffer)
   ((derived-mode-p 'org-mode)
    (current-buffer))
   (t
    (user-error "Org Copilot needs an Org source buffer"))))

;;;###autoload
(defun org-copilot-open ()
  "Open or refresh unified comments for the current Org buffer."
  (interactive)
  (let ((source (org-copilot--active-source-buffer)))
    (with-current-buffer source
      (org-copilot-mode 1)
      (if (fboundp 'org-comments-open)
	  (org-comments-open)
	(context-panels-open source)))))

;;;###autoload
(defun org-copilot-open-panels ()
  "Open Org Copilot side and chat panels for the current Org buffer."
  (interactive)
  (let ((source (org-copilot--active-source-buffer)))
    (with-current-buffer source
      (org-copilot-mode 1))
    (org-copilot-open)
    (with-current-buffer source
      (org-copilot-chat-full-document))))

;;;###autoload
(defun org-copilot-refresh ()
  "Refresh the Org Copilot side panel from current session state."
  (interactive)
  (context-panels-refresh))

;;;###autoload
(defun org-copilot-close ()
  "Close Org Copilot panel windows for the current source or panel."
  (interactive)
  (cond
   ((derived-mode-p 'org-copilot-chat-mode)
    (let ((source context-panels-source-buffer)
	  (chat (current-buffer)))
      (setq context-panels--desired-bottom-view-id nil)
      (when (buffer-live-p source)
	(with-current-buffer source
	  (setq context-panels-bottom-panel-buffer nil)))
      (org-copilot--close-buffer-window chat)))
   (t
    (let ((source (org-copilot--active-source-buffer)))
      (with-current-buffer source
	(when (buffer-live-p context-panels-side-panel-buffer)
	  (context-panels-close))
	(when (buffer-live-p context-panels-bottom-panel-buffer)
	  (context-panels-close-bottom-view)))))))

(provide 'org-copilot-context-panel)
;;; org-copilot-context-panel.el ends here
