;;; setup-treemacs.el --- giving treemacs a go  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Hubert Behaghel

;; Author: Hubert Behaghel <behaghel@gmail.com>
;; Keywords: convenience

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
  :after (doom-themes evil)
  :bind (("M-0"       . treemacs-select-window)
         ("C-x t 1"   . treemacs-delete-other-windows)
         ("C-x t t"   . treemacs)
         ("C-x t B"   . treemacs-bookmark)
         ("C-x t C-t" . treemacs-find-file)
         ("C-x t M-t" . treemacs-find-tag))
  :config
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
     (treemacs-git-mode 'simple)))

  ;; UI
  (require 'doom-themes-ext-treemacs)
  (setq doom-themes-treemacs-theme "doom-colors") ; use the colorful treemacs theme
  (doom-themes-treemacs-config)

  ;; Evil keybindings using evil-collection approach
  ;; ,ot to open treemacs and ,oT to find file in it
  (with-eval-after-load 'evil-collection
    (evil-collection-define-key 'normal 'global
      ",gt" 'treemacs-select-window
      ",mt" 'treemacs-bookmark
      )))

(use-package treemacs-evil
  :after (treemacs evil)
  :config
  (define-key evil-treemacs-state-map (kbd "z.")  #'treemacs-toggle-show-dotfiles)
  (define-key evil-treemacs-state-map (kbd "zw")  #'treemacs-toggle-fixed-width)
  (define-key evil-treemacs-state-map (kbd "zv")  #'treemacs-fringe-indicator-mode)
  (define-key evil-treemacs-state-map (kbd "zf")  #'treemacs-follow-mode)
  (define-key evil-treemacs-state-map (kbd "za")  #'treemacs-filewatch-mode)
  (define-key evil-treemacs-state-map (kbd "zg")  #'treemacs-git-mode))

(use-package treemacs-icons-dired
  :after (treemacs dired)
  :config (treemacs-icons-dired-mode))

(use-package treemacs-magit
  :after (treemacs magit))

(provide 'setup-treemacs)
;;; setup-treemacs.el ends here
