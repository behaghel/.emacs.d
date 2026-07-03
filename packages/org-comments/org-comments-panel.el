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
    (define-key map (kbd "o") #'org-comments-mark-open)
    (define-key map (kbd "t") #'org-comments-mark-todo)
    (define-key map (kbd "r") #'org-comments-mark-resolved)
    map)
  "Prefix keymap for `org-comments-panel-mode' status actions.")

(defvar org-comments-panel-mode-map (make-sparse-keymap)
  "Keymap used in `org-comments-panel-mode'.")

(defun org-comments-panel-install-default-keybindings ()
  "Install default keybindings for `org-comments-panel-mode'.
This function is idempotent and intentionally runs on load so reloading this
file updates already-bound maps in long-lived Emacs sessions."
  (define-key org-comments-panel-status-map (kbd "o") #'org-comments-mark-open)
  (define-key org-comments-panel-status-map (kbd "t") #'org-comments-mark-todo)
  (define-key org-comments-panel-status-map (kbd "r") #'org-comments-mark-resolved)
  (define-key org-comments-panel-filter-map (kbd "z") #'org-comments-filter-reset-current-ui)
  (define-key org-comments-panel-filter-map (kbd "r") #'org-comments-filter-toggle-resolved-current-ui)
  (define-key org-comments-panel-filter-map (kbd "?") #'org-comments-filter-status-current-ui)
  (define-key org-comments-panel-mode-map (kbd "?") #'org-comments-help-current-ui)
  (define-key org-comments-panel-mode-map (kbd "RET") #'org-context-panel-jump-at-point)
  (define-key org-comments-panel-mode-map (kbd "d") #'org-comments-delete)
  (define-key org-comments-panel-mode-map (kbd "e") #'org-comments-edit)
  (define-key org-comments-panel-mode-map (kbd "g") #'org-comments-panel-refresh)
  (define-key org-comments-panel-mode-map (kbd "m") org-comments-panel-status-map)
  (define-key org-comments-panel-mode-map (kbd "D") #'org-comments-pull)
  (define-key org-comments-panel-mode-map (kbd "O") #'org-comments-open-remote)
  (define-key org-comments-panel-mode-map (kbd "o") #'org-comments-open-remote)
  (define-key org-comments-panel-mode-map (kbd "p") #'org-comments-page-open-at-point)
  (define-key org-comments-panel-mode-map (kbd "S") #'org-comments-sync)
  (define-key org-comments-panel-mode-map (kbd "U") #'org-comments-push)
  (define-key org-comments-panel-mode-map (kbd "q") #'org-comments-close-current-ui)
  (define-key org-comments-panel-mode-map (kbd "r") #'org-comments-reply)
  (define-key org-comments-panel-mode-map (kbd "z") org-comments-panel-filter-map)
  (define-key org-comments-panel-mode-map (kbd "]c") #'org-comments-next-item-at-point)
  (define-key org-comments-panel-mode-map (kbd "[c") #'org-comments-previous-item-at-point))

(org-comments-panel-install-default-keybindings)

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
