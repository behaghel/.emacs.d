;;; core.el --- UI core tweaks (frames, windows, themes) -*- lexical-binding: t; -*-

;;; Commentary:
;; Appearance and window behavior customizations for interactive sessions.

;;; Code:

(menu-bar-mode -1)
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(when (fboundp 'horizontal-scroll-bar-mode) (horizontal-scroll-bar-mode -1))

(column-number-mode 1)
(setq frame-title-format '((:eval (if (buffer-file-name)
				      (abbreviate-file-name (buffer-file-name))
				    "%b"))))

(setq view-read-only t)
(setq x-select-enable-clipboard t
      x-select-enable-primary t)
(setq visible-bell t)

(use-package popwin :config (popwin-mode 1))
(save-place-mode 1)
(setq switch-to-buffer-preserve-window-point 'already-displayed
      switch-to-visible-buffer nil
      fit-window-to-buffer-horizontally t
      window-resize-pixelwise t)

(customize-set-variable 'display-buffer-base-action
			'((display-buffer-reuse-window
			   display-buffer-reuse-mode-window
			   display-buffer-in-previous-window
			   display-buffer-same-window)
			  (reusable-frames . t)))
(customize-set-variable 'even-window-sizes nil)

(defvar protected-window '(window-parameters . ((no-other-window . t)
						(no-delete-other-windows . t))))
(defvar adjustable-height '(preserve-size . (nil . t)))
(defvar adjustable-width  '(preserve-size . (t . nil)))
(setq display-buffer-alist
      '(("^magit-diff:" (display-buffer-reuse-window display-buffer-at-bottom)
	 (window-width . 0.5) (reusable-frames . nil))
	("\\*info\\*" (display-buffer-reuse-window display-buffer-in-side-window)
	 (side . right))
	("\\*Help\\*" (display-buffer-reuse-window display-buffer-in-side-window)
	 (window-width . fit-window-to-buffer) (side . right))
	("\\*Org Agenda\\*" display-buffer-in-side-window
	 (side . right) (slot . 0) (window-width . fit-window-to-buffr)
	 ,adjustable-width ,protected-window)
	("\\*Agenda Commands\\*" display-buffer-in-side-window
	 (side . right) (slot . 0) (window-width . 96)
	 ,adjustable-width ,protected-window)
	("\\*pytest\\*.*" (display-buffer-reuse-window display-buffer-at-bottom))
	("mail-sidebar\\.org" (display-buffer-in-side-window)
	 (side . left) (window-width . 36) (dedicated . t)
	 ,protected-window)))

(use-package htmlize :defer t)

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

(use-package dashboard
  :config
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
  (add-to-list 'dashboard-item-generators '(denote . dashboard-insert-denote)))

;; Fonts (adjust per system)
(set-face-attribute 'default nil :family "Ubuntu Mono" :height 180)
(set-face-attribute 'fixed-pitch nil :family "Ubuntu Mono" :height 1.0)
(set-face-attribute 'variable-pitch nil :family "ETBembo" :height 1.0)

(provide 'ui/core)
;;; core.el ends here
