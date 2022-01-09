;;; setup-ui.el --- Where I put all the tweaks for how emacs looks like  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Hubert Behaghel

;; Author: Hubert Behaghel <behaghel@gmail.com>
;; Keywords: convenience, faces

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; TBD

;;; Code:

(menu-bar-mode -1)
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(when (fboundp 'horizontal-scroll-bar-mode)
  (horizontal-scroll-bar-mode -1))

(column-number-mode 1)               ; show column number in mode line
;; more useful frame title, that show either a file or a
;; buffer name (if the buffer isn't visiting a file)
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

;; treat every read-only buffer as a pager
;; navigation: SPC, delete, d, u, etc.
(setq view-read-only t)
;; you can switch any buffer to read-only with C-x C-q

;; Allow Emacs to access content from clipboard.
(setq x-select-enable-clipboard t
      x-select-enable-primary t)

;; No annoying buffer for completion, compilation, help...
(use-package popwin
  :config
  (popwin-mode 1))

;; remember last position last time file was edited
(save-place-mode 1)
; try to stabilize windows and buffers positions
(setq switch-to-buffer-preserve-window-point 'already-displayed
      switch-to-visible-buffer nil)
(setq fit-window-to-buffer-horizontally t)
(setq window-resize-pixelwise t)

;; stolen from https://github.com/nex3/perspective-el#some-musings-on-emacs-window-layouts
(customize-set-variable 'display-buffer-base-action
                        '((display-buffer-reuse-window
                           display-buffer-reuse-mode-window
                           display-buffer-in-previous-window
                           display-buffer-same-window)
                          (reusable-frames . t)))

(customize-set-variable 'even-window-sizes nil)     ; avoid resizing


;; window parameters for windows that shouldn't magically disappear
;; when another window is created
(defvar protected-window
  '(window-parameters . ((no-other-window . t)
                         (no-delete-other-windows . t))))
(defvar adjustable-height '(preserve-size . (nil . t)))
(defvar adjustable-width '(preserve-size . (t . nil)))
(setq display-buffer-alist
      `(
        ("^magit-diff:"
         (display-buffer-reuse-window display-buffer-at-bottom)
         (window-width . 0.5)
         (reusable-frames . nil))
        ;; do not create a new window next to scratch for mu4e
        ("\\*mu4e" display-buffer-use-some-window)
        ("\\*info\\*"
         (display-buffer-reuse-window display-buffer-in-side-window)
         (side . right))
        ("\\*Help\\*"
         (display-buffer-reuse-window display-buffer-in-side-window)
         (window-width . fit-window-to-buffer)
         (side . right))
        ("\\*Org Agenda\\*" display-buffer-in-side-window
         (side . right) (slot . 0)
         (window-width . fit-window-to-buffr)
         ,adjustable-width
         ,protected-window
         )
        ("\\*Agenda Commands\\*" display-buffer-in-side-window
         (side . right) (slot . 0)
         (window-width . 96)
         ,adjustable-width
         ,protected-window
         )
        ("\\*pytest\\*.*" (display-buffer-reuse-window display-buffer-at-bottom))
        ("\\*.*\\*" (display-buffer-reuse-window))
        ("mail-sidebar\\.org"
         (display-buffer-in-side-window)
         (side . left)(window-width . 36)(dedicated . t)
         ,protected-window
         )
        ))


(use-package rainbow-delimiters
  :commands (rainbow-delimiters-mode)
  :config
  ;; (setq rainbow-delimiters-max-face-count 1)
  (set-face-attribute 'rainbow-delimiters-unmatched-face nil
                      :foreground 'unspecified
                      :inherit 'error))

(use-package htmlize
  :defer t)


;;; Smart Mode Line
(use-package smart-mode-line
  :disabled t
  :config
  (setq sml/theme 'respectful)
  (sml/setup))

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :config
  (setq doom-modeline-buffer-file-name-style 'relative-to-project)
  (setq doom-modeline-buffer-encoding nil)
  (setq doom-modeline-mu4e t)

  (setq doom-modeline-height 36)
  ;; FIXME: my hack to make it work for perspective.el isn't working.
  ;; Either I load the modeline before perspective and the whole hack
  ;; create infinite loop on every commands or I load it later but it
  ;; doesn't load well (half loaded?) and require manual fiddling (unload/reload)
  ;; no clue why doom-modeline is so exclusive about perspective.el
  ;; this is all it takes to adapt its persp-mode integration to it
  ;; (defalias 'safe-persp-name #'persp-name)
  ;; (defalias 'get-current-persp #'persp-curr)
  ;; (defun persp-contain-buffer-p (buffer persp)
  ;;   (persp-is-current-buffer buffer))
  ;; (setq persp-nil-name 'main)
  ;; (setq doom-modeline-persp-name t)
)

;; https://gitlab.com/protesilaos/modus-themes
(use-package modus-themes
  :disabled t
  :init
  ;; Add all your customizations prior to loading the themes
  (setq modus-themes-slanted-constructs t
        modus-themes-bold-constructs nil
        modus-themes-variable-pitch-ui nil ; e.g. mode-line, tab-bar
        modus-themes-variable-pitch-headings nil
        modus-themes-scale-headings t
        modus-themes-scale-1 1.05
        modus-themes-scale-2 1.1
        modus-themes-scale-3 1.15
        modus-themes-scale-4 1.3
        modus-themes-scale-5 1.5
        )

  ;; Load the theme files before enabling a theme
  (modus-themes-load-themes)
  :config
  ;; Load the theme of your choice:
  (modus-themes-load-vivendi) ;; dark OR dark (modus-themes-load-operandi)
  :bind (:map evil-normal-state-map
              (",zk" . modus-themes-toggle)))

(use-package doom-themes
  :disabled t
  :pin melpa
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  ;; (load-theme 'doom-one t)
  (load-theme 'doom-material t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)

  ;; Corrects (and improves) org-mode's native fontification.
  (require 'doom-themes-ext-org)
  (doom-themes-org-config)
  )

(use-package nano-theme
  :straight (nano-theme :type git :host github :repo "404cn/nano-theme.el")
  :config
  (setq nano-theme-light/dark 'dark)
  (load-theme 'nano t))

;; introduce contrast between popup buffers and working buffers
(use-package solaire-mode
  :disabled t
  :config
  (solaire-global-mode +1)
  )

(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-projects-switch-function 'projectile-persp-switch-project)
  (setq dashboard-filter-agenda-entry 'dashboard-filter-agenda-by-todo)
  )

;; https://github.com/be5invis/Iosevka
;; https://protesilaos.com/codelog/2020-09-05-emacs-note-mixed-font-heights/
(set-face-attribute 'default nil :family "Iosevka Nerd Font" :height (if is-mac 150 80))
;; (set-face-attribute 'default nil :family "Iosevka Nerd Font" :height 150)
;; (set-face-attribute 'default nil :font (if is-mac "Iosevka-16" "Iosevka-12"))
(set-face-attribute 'fixed-pitch nil :family "Iosevka Nerd Font" :height 1.0)
(set-face-attribute 'variable-pitch nil :family "FiraGO" :height 1.0)
;; (set-face-attribute 'variable-pitch nil :family "ETBembo" :height 1.0)

(provide 'setup-ui)
;;; setup-ui.el ends here
