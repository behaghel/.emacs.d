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

(provide 'ui/core)
;;; core.el ends here
