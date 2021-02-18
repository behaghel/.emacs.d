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
;; stolen from https://github.com/nex3/perspective-el#some-musings-on-emacs-window-layouts
(setq display-buffer-alist
      '(
        ("^magit-diff:"
         (display-buffer-reuse-window display-buffer-at-bottom)
         (window-width . 0.5)
         (reusable-frames . nil))
        ("\\*.*\\*" (display-buffer-reuse-window))
        ("\\*pytest\\*.*" (display-buffer-reuse-window display-buffer-at-bottom))
        (".*" (display-buffer-reuse-window display-buffer-same-window))))

(setq display-buffer-reuse-frames t)         ; reuse windows in other frames
(setq even-window-sizes nil)                 ; display-buffer: avoid resizing


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
  ;; :pin melpa
  :init (doom-modeline-mode 1))

(use-package sublime-themes
  :disabled t
  :config
  (load-theme 'odersky t))

;; https://gitlab.com/protesilaos/modus-themes
(use-package modus-themes
  :init
  ;; Add all your customizations prior to loading the themes
  (setq modus-themes-slanted-constructs t
        modus-themes-bold-constructs nil
        modus-themes-variable-pitch-ui nil ; e.g. mode-line, tab-bar
        modus-themes-variable-pitch-headings t
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

;; introduce contrast between popup buffers and working buffers
(use-package solaire-mode
  :hook
  ((change-major-mode after-revert ediff-prepare-buffer) . turn-on-solaire-mode)
  (minibuffer-setup . solaire-mode-in-minibuffer)
  :config
  (solaire-global-mode +1)
  (solaire-mode-swap-bg))

;; https://github.com/be5invis/Iosevka
;; (if (eq system-type 'darwin)
;; https://protesilaos.com/codelog/2020-09-05-emacs-note-mixed-font-heights/
(set-face-attribute 'default nil :font "Iosevka-12")
(set-face-attribute 'fixed-pitch nil :family "Iosevka" :height 1.0)
(set-face-attribute 'variable-pitch nil :family "FiraGO" :height 1.0)
;; (set-face-attribute 'variable-pitch nil :family "DejaVu Sans Condensed" :height 1.0)
;; (set-face-attribute 'variable-pitch nil :family "ETBembo" :height 1.0)

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

(provide 'setup-ui)
;;; setup-ui.el ends here
