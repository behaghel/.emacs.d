;;; setup-python.el --- my preferred flavour of python  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Hubert Behaghel

;; Author: Hubert Behaghel <behaghel@gmail.com>
;; Keywords: languages

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

;; I don't use Python that often and I'd like to change that. This
;; should be designed to lower the barrier to entry to interacting
;; with anything through Python.

;;; Code:

(use-package python
  ;; :ensure nil
  :straight (:type built-in)
  :hook ((python-mode . ensure-eglot)))

(use-package python-docstring
  :hook (python-mode . python-docstring-mode))

(use-package pydoc
  :commands (pydoc pydoc-at-point pydoc-browse)
  :bind (:map evil-normal-state-map
              (",hh" . pydoc-at-point-no-jedi)))

(use-package python-pytest
  :bind (:map evil-normal-state-map
              (",Th" . python-pytest-dispatch)
              (",Tt" . python-pytest)
              (",Tf" . python-pytest-file)
              (",T," . python-pytest-file-dwim)
              (",Tr" . python-pytest-repeat)
              (",Tl" . python-pytest-last-failed)))

(use-package pip-requirements)

(use-package ein
  :after (org)
  :disabled t
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((ein . t))))

(provide 'setup-python)
;;; setup-python.el ends here
