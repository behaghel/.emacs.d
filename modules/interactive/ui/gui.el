;;; gui.el --- UI: GUI-specific extras (themes, icons, dashboard) -*- lexical-binding: t; -*-

;;; Commentary:
;; GUI-only visual enhancements layered on top of ui/core.

;;; Code:

(require 'hub-utils)
(use-package modus-themes
  :demand t
  :bind (:map evil-normal-state-map (",zk" . modus-themes-toggle))
  :config
  (setq modus-themes-slanted-constructs t
	modus-themes-bold-constructs nil
	modus-themes-variable-pitch-ui nil
	modus-themes-headings '((1 . (variable-pitch extrabold 1.15))
				(2 . (variable-pitch 1.1))
				(3 . (variable-pitch semibold 1.05))
				(4 . (variable-pitch 1))
				(5 . (variable-pitch 0.9))
				(agenda-date . (0.8))
				(agenda-structure . (variable-pitch light 1))
				(t . (1)))
	modus-themes-mixed-fonts t
	modus-themes-prompts '(extrabold italic)
	modus-themes-org-blocks 'tinted-background
	modus-themes-mode-line '(borderless))
  (load-theme 'modus-vivendi-tinted :no-confirm))

(use-package all-the-icons :if (display-graphic-p))

;; Emoji and symbol font fallback so headers/subjects render properly
(when (display-graphic-p)
  (setq use-default-font-for-symbols nil)
  (let ((emoji-font
	 (or (and (find-font (font-spec :family "Noto Color Emoji"))
		  (font-spec :family "Noto Color Emoji"))
	     (and (find-font (font-spec :family "Apple Color Emoji"))
		  (font-spec :family "Apple Color Emoji"))
	     (and (find-font (font-spec :family "Segoe UI Emoji"))
		  (font-spec :family "Segoe UI Emoji")))))
    (when emoji-font
      ;; Prefer emoji font for emoji and general symbol ranges
      (set-fontset-font t 'emoji emoji-font nil 'prepend)
      (set-fontset-font t 'symbol emoji-font nil 'prepend)
      ;; Ensure common emoji blocks have a fallback
      (dolist (range '((#x1F300 . #x1FAFF) ; Misc Symbols and Pictographs .. Symbols and Pictographs Extended-A
		       (#x2600  . #x27BF))) ; Misc symbols
	(set-fontset-font t range emoji-font nil 'prepend)))))

(use-package dashboard
  :config
  ;; Ensure recentf is enabled so Recents has data
  (require 'recentf)
  (require 'seq)
  (recentf-mode 1)
  (setq dashboard-set-heading-icons t
	dashboard-set-file-icons t
	dashboard-set-navigator t)
  (dashboard-setup-startup-hook)
  (setq dashboard-projects-switch-function 'project-switch-project
	dashboard-filter-agenda-entry 'dashboard-filter-agenda-by-todo
	dashboard-items '((agenda . 8) (denote . 5) (projects . 5) (recents . 5) (bookmarks . 5)))
  (defun dashboard-insert-denote (list-size)
    (let ((recent-notes (seq-sort-by #'file-name-nondirectory
				     (lambda (x y) (string-lessp y x))
				     (denote-directory-files))))
      (insert (all-the-icons-octicon "repo" :height 1.2 :v-adjust 0.0 :face 'dashboard-heading))
      (dashboard-insert-section "Recent Notes:" recent-notes list-size 'notes "n"
				`(lambda (&rest ignore) (find-file-existing ,el))
				(denote-retrieve-title-value el (denote-filetype-heuristics el)))))
  (add-to-list 'dashboard-item-generators '(denote . dashboard-insert-denote))

  ;; Hide Maildir entries from the Recent Files section while keeping them in recentf.
  (defun hub/dashboard--mail-path-p (path)
    (when (stringp path)
      (let ((mail-root (file-name-as-directory (expand-file-name "Mail" (or (getenv "HOME") "~")))))
	(string-prefix-p mail-root (expand-file-name path)))))

  (defun hub/dashboard--filter-recents (paths)
    (seq-remove #'hub/dashboard--mail-path-p paths))

  (defun hub/dashboard-filter-mail-recents (orig-fn &rest args)
    (let* ((recentf-list (if (and (boundp 'recentf-list) (recentf-enabled-p))
			     (hub/dashboard--filter-recents recentf-list)
			   recentf-list)))
      (apply orig-fn args)))

  (advice-add 'dashboard-insert-startupify-lists :around #'hub/dashboard-filter-mail-recents))

;; Fonts (adjust per system)
(set-face-attribute 'default nil :family "Ubuntu Mono" :height 180)
(set-face-attribute 'fixed-pitch nil :family "Ubuntu Mono" :height 1.0)
(set-face-attribute 'variable-pitch nil :family "ETBembo" :height 1.0)

(provide 'ui/gui)
;;; gui.el ends here
