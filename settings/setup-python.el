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
  :ensure nil
  :custom
  ;; (python-shell-interpreter "ipython")
  ;; (python-shell-interpreter-args "-i --simple-prompt")
  ;; (python-shell-interpreter-args "-i")
  ;; (python-indent-guess-indent-offset-verbose nil)
  )

;; I use pyenv to manage python versions
(use-package pyenv-mode
  :pin melpa
  :commands (pyenv-mode pyenv-mode-set)
  :hook (python-mode . pyenv-mode)
  :config
  ;; (let ((my-pyenv-path (expand-file-name "~/.pyenv/bin")))
  ;;   (setenv "PATH" (concat my-pyenv-path path-separator (getenv "PATH")))
  ;;   (add-to-list 'exec-path my-pyenv-path))
  ;; automatically set the right version for `run-python' within
  ;; projectile project
  (defun projectile-pyenv-mode-set ()
    "Set pyenv version matching project name."
    (let ((project (projectile-project-name)))
      (if (member project (pyenv-mode-versions))
          (pyenv-mode-set project)
        (pyenv-mode-unset))))
  (add-hook 'projectile-after-switch-project-hook 'projectile-pyenv-mode-set))

(use-package lsp-pyright
  :ensure t
  :hook (pyenv-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp))))  ; or lsp-deferred

;; useful if you use virtualenvwrapper.sh
;; (defun disable-flycheck ()
;;   "Flycheck doesn't understand virtualenv. And lsp provides the equivalent."
;;   (flycheck-mode -1))
;; (defvar *python-current-env* nil)
;; (defun hub/workon ()
;;   "To load the virtual env with the same name as the root dir."
;;   (interactive)
;;   ;; (message "running hub/workon before: %s"
;;   ;;          *python-current-env*)
;;   (let* ((rootdir (directory-file-name (projectile-project-root)))
;;          (env (file-name-nondirectory rootdir)))
;;     (when (and env
;;                (not (equal env *python-current-env*)))
;;       (progn
;;         (setf *python-current-env* env)
;;         (pyvenv-workon env)
;;         (message "Current python env: %s"
;;                  *python-current-env*))))
;;   (save-some-buffers t))
;; (use-package pyvenv
;;   :commands (pyvenv-activate pyvenv-workon)
;;   :init
;;   (add-to-list 'exec-path "~/.pyenv/shims")
;;   (setenv "WORKON_HOME" "~/.virtualenvs")
;;   (add-hook 'pyvenv-post-activate-hooks 'lsp)
;;   (add-hook 'pyvenv-post-activate-hooks 'pyvenv-restart-python)
;;   :hook (
;;          (python-mode . disable-flycheck)
;;          (python-mode . pyvenv-mode)
;;          ;; (python-mode . hub/workon)
;;          (python-mode . (lambda ()(require 'dap-python)))
;;          (dap-stopped . (lambda (arg) (call-interactively #'dap-hydra)))
;;          )
;;   :config
;;   (evil-define-key 'normal python-mode-map ",gr" 'run-python)
;;   )

;; (use-package pipenv
;;   :hook (python-mode . pipenv-mode)
;;   :after pyvenv
;;   :init
;;   (setq
;;    pipenv-projectile-after-switch-function
;;    #'pipenv-projectile-after-switch-extended))

(use-package py-autopep8
  :hook (python-mode . py-autopep8-enable-on-save))

(use-package blacken
  :hook (python-mode . blacken-mode)
  :config
  (setq blacken-line-length '88))

(use-package pydoc
  :commands (pydoc pydoc-at-point pydoc-browse))

(use-package python-docstring
  :hook (python-mode . python-docstring-mode))

(use-package pip-requirements)

(provide 'setup-python)
;;; setup-python.el ends here
