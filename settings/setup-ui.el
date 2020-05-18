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

;; Keybindings: https://github.com/Alexander-Miller/treemacs#keymap
(use-package treemacs
  :pin melpa
  :ensure t
  :defer nil
  :config
  (progn
    (require 'treemacs-icons)
    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (require 'treemacs-follow-mode)
    (treemacs-follow-mode t)
    (require 'treemacs-filewatch-mode)
    (treemacs-filewatch-mode t)
    (require 'treemacs-fringe-indicator)
    (treemacs-fringe-indicator-mode t)
    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple))))
  :bind (
         :map evil-normal-state-map
         (",tt" . treemacs)
         (",t1" . treemacs-delete-other-windows)
         (",to" . treemacs-select-window)
         (",tb" . treemacs-bookmark)
         (",tf" . treemacs-find-file)
         (",tj" . treemacs-find-tag)
         (",tp" . treemacs-projectile)
         (",td" . treemacs-remove-project-from-workspace)
         (",t+" . treemacs-create-workspace)
         (",tE" . treemacs-edit-workspaces)
         (",ts" . treemacs-switch-workspace)

         :map global-map
         ("M-0"       . treemacs-select-window)
         ("C-x t 1"   . treemacs-delete-other-windows)
         ("C-x t t"   . treemacs)
         ("C-x t B"   . treemacs-bookmark)
         ("C-x t C-t" . treemacs-find-file)
         ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-evil
  :pin melpa
  :after treemacs evil
  :ensure t
  :config
  ;; (define-key evil-treemacs-state-map (kbd "th")  nil)
  ;; (define-key evil-treemacs-state-map (kbd "tw")  nil)
  ;; (define-key evil-treemacs-state-map (kbd "tv")  nil)
  ;; (define-key evil-treemacs-state-map (kbd "tf")  nil)
  ;; (define-key evil-treemacs-state-map (kbd "ta")  nil)
  ;; (define-key evil-treemacs-state-map (kbd "tg")  nil)
  (define-key evil-treemacs-state-map (kbd "z.")  #'treemacs-toggle-show-dotfiles)
  (define-key evil-treemacs-state-map (kbd "zw")  #'treemacs-toggle-fixed-width)
  (define-key evil-treemacs-state-map (kbd "zv")  #'treemacs-fringe-indicator-mode)
  (define-key evil-treemacs-state-map (kbd "zf")  #'treemacs-follow-mode)
  (define-key evil-treemacs-state-map (kbd "za")  #'treemacs-filewatch-mode)
  (define-key evil-treemacs-state-map (kbd "zg")  #'treemacs-git-mode))

(use-package treemacs-projectile
  :pin melpa
  :after treemacs projectile
  :ensure t)

(use-package treemacs-icons-dired
  :pin melpa
  :after treemacs dired
  :ensure t
  :config (treemacs-icons-dired-mode))

(use-package treemacs-magit
  :pin melpa
  :after treemacs magit
  :ensure t)

(use-package treemacs-persp
  :pin melpa
  :disabled t
  :after treemacs persp-projectile
  :ensure t
  :config (treemacs-set-scope-type 'Perspectives))

;; Ligatures
(global-auto-composition-mode)
(let ((alist
       '((33 . ".\\(?:\\(==\\|[!=]\\)[!=]?\\)")
         (35 . ".\\(?:\\(###?\\|_(\\|[(:=?[_{]\\)[#(:=?[_{]?\\)")
         (36 . ".\\(?:\\(>\\)>?\\)")
         (37 . ".\\(?:\\(%\\)%?\\)")
         (38 . ".\\(?:\\(&\\)&?\\)")
         (42 . ".\\(?:\\(\\*\\*\\|[*>]\\)[*>]?\\)")
         ;; (42 . ".\\(?:\\(\\*\\*\\|[*/>]\\).?\\)")
         (43 . ".\\(?:\\([>]\\)>?\\)")
         ;; (43 . ".\\(?:\\(\\+\\+\\|[+>]\\).?\\)")
         (45 . ".\\(?:\\(-[->]\\|<<\\|>>\\|[-<>|~]\\)[-<>|~]?\\)")
         ;; (46 . ".\\(?:\\(\\.[.<]\\|[-.=]\\)[-.<=]?\\)")
         (46 . ".\\(?:\\(\\.<\\|[-=]\\)[-<=]?\\)")
         (47 . ".\\(?:\\(//\\|==\\|[=>]\\)[/=>]?\\)")
         ;; (47 . ".\\(?:\\(//\\|==\\|[*/=>]\\).?\\)")
         (48 . ".\\(?:\\(x[a-fA-F0-9]\\).?\\)")
         (58 . ".\\(?:\\(::\\|[:<=>]\\)[:<=>]?\\)")
         (59 . ".\\(?:\\(;\\);?\\)")
         (60 . ".\\(?:\\(!--\\|\\$>\\|\\*>\\|\\+>\\|-[-<>|]\\|/>\\|<[-<=]\\|=[<>|]\\|==>?\\||>\\||||?\\|~[>~]\\|[$*+/:<=>|~-]\\)[$*+/:<=>|~-]?\\)")
         (61 . ".\\(?:\\(!=\\|/=\\|:=\\|<<\\|=[=>]\\|>>\\|[=>]\\)[=<>]?\\)")
         (62 . ".\\(?:\\(->\\|=>\\|>[-=>]\\|[-:=>]\\)[-:=>]?\\)")
         (63 . ".\\(?:\\([.:=?]\\)[.:=?]?\\)")
         (91 . ".\\(?:\\(|\\)[]|]?\\)")
         ;; (92 . ".\\(?:\\([\\n]\\)[\\]?\\)")
         (94 . ".\\(?:\\(=\\)=?\\)")
         (95 . ".\\(?:\\(|_\\|[_]\\)_?\\)")
         (119 . ".\\(?:\\(ww\\)w?\\)")
         (123 . ".\\(?:\\(|\\)[|}]?\\)")
         (124 . ".\\(?:\\(->\\|=>\\||[-=>]\\||||*>\\|[]=>|}-]\\).?\\)")
         (126 . ".\\(?:\\(~>\\|[-=>@~]\\)[-=>@~]?\\)"))))
  (dolist (char-regexp alist)
    (set-char-table-range composition-function-table (car char-regexp)
                          `([,(cdr char-regexp) 0 font-shape-gstring])))
  )
;; No annoying buffer for completion, compilation, help...
(use-package popwin
  :config
  (popwin-mode 1))

;; remember last position last time file was edited
(save-place-mode 1)
; try to stabilize windows and buffers positions
(setq switch-to-buffer-preserve-window-point 'already-displayed
      switch-to-visible-buffer nil)
;; stolen from https://github.com/nex3/perspective-el#some-musings-on-emacs-window-layouts
(setq display-buffer-alist
      '(("\\*.*\\*" (display-buffer-reuse-window))
        (".*" (display-buffer-reuse-window display-buffer-same-window))))

(setq display-buffer-reuse-frames t)         ; reuse windows in other frames
(setq even-window-sizes nil)                 ; display-buffer: avoid resizing


(use-package rainbow-delimiters
  :commands (rainbow-delimiters-mode)
  :config
  (setq rainbow-delimiters-max-face-count 1)
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
  :pin melpa
  :init (doom-modeline-mode 1))

(column-number-mode 1)               ; show column number in mode line

;; Look / Theme
;; http://pawelbx.github.io/emacs-theme-gallery/
;;
;; (load-theme 'zenburn t)
;; ;; zenburn region face is invisible...
;; (set-face-attribute 'region nil :background "#666")
;; (use-package moe-theme
;;   :config
;;   (moe-dark))
;; (use-package dakrone-theme
;;   :config
;;   (load-theme 'dakrone t))
;; http://chriskempson.github.io/base16/#eighties
;; (load-theme 'base16-eighties-dark t)
(use-package sublime-themes
  :disabled t
  :config
  (load-theme 'odersky t))

(use-package doom-themes
  :pin melpa
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  ;; (load-theme 'doom-one t)
  (load-theme 'doom-material t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)

  ;; or for treemacs users
  (require 'doom-themes-ext-treemacs)
  (setq doom-themes-treemacs-theme "doom-colors") ; use the colorful treemacs theme
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (require 'doom-themes-ext-org)
  (doom-themes-org-config)
  )

;; introduce contrast between popup buffers and working buffers
(use-package solaire-mode
  :hook
  ((change-major-mode after-revert ediff-prepare-buffer) . turn-on-solaire-mode)
  (minibuffer-setup . solaire-mode-in-minibuffer)
  :config
  (solaire-global-mode +1)
  (solaire-mode-swap-bg))

;; more useful frame title, that show either a file or a
;; buffer name (if the buffer isn't visiting a file)
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

;; (set-face-attribute 'default nil :font "Droid Sans Mono-12")
;; (when (member "Source Code Pro-12" (font-family-list))
;; (when (member "mononoki-12" (font-family-list))
;;   (set-face-attribute 'default nil :font "mononoki-12"))
;; https://github.com/be5invis/Iosevka
(set-face-attribute 'default nil :font "Iosevka-12")

(provide 'setup-ui)
;;; setup-ui.el ends here
