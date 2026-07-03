;;; org-comments-panel.el --- Standalone panel for Org comments -*- lexical-binding: t; -*-

;;; Commentary:
;; A small package-owned comments panel.  This is the generic lifecycle and
;; rendering slice; richer actions, filters, and compose support remain separate
;; migration steps.

;;; Code:

(require 'org)
(require 'org-context-panel)
(require 'org-comments-context-panel)
(require 'org-comments-core)
(require 'org-comments-panel-actions)
(require 'org-comments-panel-filter)
(require 'org-comments-panel-render)
(require 'org-comments-store)

(defvar-local org-comments-panel-source-buffer nil
  "Org source buffer rendered by the current comments panel.")

(defvar org-comments-panel-filter-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "z") #'org-comments-filter-reset-current-ui)
    (define-key map (kbd "r") #'org-comments-filter-toggle-resolved-current-ui)
    (define-key map (kbd "?") #'org-comments-filter-status-current-ui)
    map)
  "Prefix keymap for `org-comments-panel-mode' filters.")

(defvar org-comments-panel-status-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "o") #'org-comments-panel-mark-open)
    (define-key map (kbd "t") #'org-comments-panel-mark-todo)
    (define-key map (kbd "r") #'org-comments-panel-mark-resolved)
    map)
  "Prefix keymap for `org-comments-panel-mode' status actions.")

(defvar org-comments-panel-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "?") #'org-comments-help-current-ui)
    (define-key map (kbd "RET") #'org-context-panel-jump-at-point)
    (define-key map (kbd "d") #'org-comments-delete-at-point)
    (define-key map (kbd "e") #'org-comments-edit-at-point)
    (define-key map (kbd "g") #'org-comments-panel-refresh)
    (define-key map (kbd "m") org-comments-panel-status-map)
    (define-key map (kbd "D") #'org-comments-panel-pull)
    (define-key map (kbd "O") #'org-comments-panel-open-remote)
    (define-key map (kbd "o") #'org-comments-panel-open-remote)
    (define-key map (kbd "p") #'org-comments-page-open-at-point)
    (define-key map (kbd "S") #'org-comments-panel-sync)
    (define-key map (kbd "U") #'org-comments-panel-push)
    (define-key map (kbd "q") #'org-comments-close-current-ui)
    (define-key map (kbd "r") #'org-comments-reply-at-point)
    (define-key map (kbd "z") org-comments-panel-filter-map)
    (define-key map (kbd "]c") #'org-comments-next-item-at-point)
    (define-key map (kbd "[c") #'org-comments-previous-item-at-point)
    map)
  "Keymap used in `org-comments-panel-mode'.")

(define-derived-mode org-comments-panel-mode special-mode "Org-Comments"
  "Major mode for the standalone Org comments panel.")

;;;###autoload
(defun org-comments-panel-open ()
  "Open or refresh the standalone Org comments panel for the current source."
  (interactive)
  (let ((source-buffer (org-context-panel-current-source-buffer)))
    (with-current-buffer source-buffer
      (org-comments-context-panel-enable)
      (org-context-panel-open source-buffer))))

;;;###autoload
(defun org-comments-panel-refresh ()
  "Refresh the standalone Org comments panel."
  (interactive)
  (org-context-panel-refresh))

;;;###autoload
(defun org-comments-panel-close ()
  "Close the standalone Org comments panel."
  (interactive)
  (org-context-panel-close))

(provide 'org-comments-panel)
;;; org-comments-panel.el ends here
