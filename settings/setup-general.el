;;; setup-general.el --- General settings for my Emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Hubert Behaghel

;; Author: Hubert Behaghel <behaghel@gmail.com>
;; Keywords: lisp

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

;; Where I put better defauls and general config for my Emacs

;;; Code:
; stop cluttering my fs with #file.ext#
(setq backup-directory-alist `((".*" . ,temporary-file-directory))
      auto-save-file-name-transforms
      `((".*" ,(concat user-emacs-directory "backups") t)))

(use-package emacs
  :delight (auto-fill-function))

(use-package better-defaults)

;; https://github.com/emacs-lsp/lsp-mode#performance
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024)) ;; 1mb

; Mac
;; Are we on a mac? Thanks @magnars
(setq is-mac (equal system-type 'darwin))
(setq mac-command-key-is-meta t)
(setq mac-command-modifier 'meta)
(setq mac-option-key-is-meta nil)
(setq mac-option-modifier nil)
(setq ns-use-srgb-colorspace t)
; tweaking to get my proper setup
; OSX
; * iTerm2
; Preferences > Keys (tab) > Remap Left Command to Left Option
; Preferences > Profile > Left Option: Meta + Esc
; * Numpad (for calc): remap keypad-dot to Option+Shift+Keypad-dot


(use-package exec-path-from-shell
  :config
  ;; remove -1 from default value
  ;; this implies PATH and key environment variables are set through
  ;; .profile and not through .zshrc or other interactive-only config files
  (setq exec-path-from-shell-arguments '("-l" "-i"))
  (exec-path-from-shell-initialize))

;; synchronise emacs clipboard with system clipboard
(use-package clipmon
  :defer t
  :ensure t)

;; particularly useful in git repositories to avoid the hassle of
;; manually reloading each buffer when you change branch.
(global-auto-revert-mode t)

(use-package undo-tree
  :diminish undo-tree-mode
  :bind ("C-é" . undo)
  :config
  (global-undo-tree-mode))

(use-package unfill
  :bind (("M-Q" . unfill-toggle))
  :commands (unfill-region unfill-paragraph unfill-toggle))

(use-package expand-region
  :bind ("M-r" . er/expand-region)
  :commands (er/expand-region))

(setq comment-auto-fill-only-comments t) ; auto-fill comments and only them
;; don't wrap for space before French punctuation
(add-to-list 'fill-nobreak-predicate 'fill-french-nobreak-p)

(setq tab-always-indent 'complete)      ; tab try to indent, if indented complete
(setq mode-require-final-newline nil)   ; don't add new line at EOF on save
(setq shift-select-mode nil)            ; no shift selection

;; save all when Emacs loses the focus
(add-hook 'focus-out-hook 'hub/save-all)

;;; TRAMP
(setq tramp-default-method "ssh")

;; General Keybindings
;; recreate what I am familiar with Pharo
(global-set-key (kbd "M-p") 'eval-print-last-sexp)
(global-set-key (kbd "M-ð") 'eval-last-sexp) ; Alt + AltGR + d (do)

;; left cmd + right cmd + csrn in order to jump from window to window
(global-set-key (kbd "M-©") 'evil-window-left)
(global-set-key (kbd "M-®") 'evil-window-right)
(global-set-key (kbd "M-þ") 'evil-window-down)
(global-set-key (kbd "M-ß") 'evil-window-up)
;; on macOS bepo differs
(global-set-key (kbd "M-¸") 'evil-window-left)
(global-set-key (kbd "M-ᵉ") 'evil-window-down)
(global-set-key (kbd "M-˘") 'evil-window-right)

;; stolen from https://github.com/magnars/.emacs.d/blob/master/key-bindings.el
;; Transpose stuff with M-t
(global-unset-key (kbd "M-t")) ;; which used to be transpose-words
(global-set-key (kbd "M-t l") 'transpose-lines)
(global-set-key (kbd "M-t w") 'transpose-words)
(global-set-key (kbd "M-t s") 'transpose-sexps)
(global-set-key (kbd "M-t p") 'hub/transpose-params)

(global-set-key (kbd "C-=") 'align-current)
;; you code you type Enter, you realise you shouldn't have, don't type
;; backspace furiously to repair, just M-Del
(global-set-key (kbd "M-<DEL>") 'delete-indentation)
(define-key key-translation-map (kbd "<f8> <right>") (kbd "→"))
(define-key key-translation-map (kbd "<f8> i") (kbd "∞"))

(global-set-key "\M-;" 'comment-dwim-line)

(provide 'setup-general)
;;; setup-general.el ends here
