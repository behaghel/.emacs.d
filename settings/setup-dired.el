;;; setup-dired.el --- My way with dired             -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Hubert Behaghel

;; Author: Hubert Behaghel <behaghel@gmail.com>
;; Keywords: files

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

;; I wish dired was more embedded into my flow. This is where I work
;; on it.

;;; Code:

(use-package dired
  :straight nil
  :config
  (setq dired-dwim-target t)
  (setq dired-listing-switches
        "-XGFahlv --group-directories-first --time-style=long-iso")
  (define-key evil-normal-state-map (kbd "g/") 'dired))
(use-package dired-aux
  :straight nil
  :config
  (evil-define-key 'normal dired-mode-map ",vv" 'dired-vc-next-action) ; dwim on marks: add, commit, etc.
  (setq dired-isearch-filenames 'dwim)
  (setq dired-create-destination-dirs 'ask)
  (setq dired-vc-rename-file t))
(use-package dired-subtree
  :config
  (bind-keys :map dired-mode-map
             ("<tab>" . dired-subtree-toggle)
             ("<C-tab>" . dired-subtree-cycle)))
;;narrow dired to match filter
(use-package dired-narrow
  :ensure t
  :bind (:map dired-mode-map
              (">" . dired-narrow)))
(use-package wdired
  :config
  (setq wdired-allow-to-change-permissions t)
  (setq wdired-create-parent-directories t))
;; clipboard-type feature for copying/moving files.
;; Mark files then Y to copy them, add more with C-u Y
;; then go to destination and use X (move) or V (paste)
(use-package dired-ranger
  :ensure t
  :bind (:map dired-mode-map
              ("Y" . dired-ranger-copy)
              ("X" . dired-ranger-move)
              ("V" . dired-ranger-paste)))

(provide 'setup-dired)
;;; setup-dired.el ends here
