;;; setup-scala.el --- efficient coding in Scala     -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Hubert Behaghel

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

;; My setup for coding in scala.
;; It uses `metals' as language server for `eglot'.
;; I assume you have nix and direnv installed and configured.
;;
;; In your `.envrc'
;;
;;;; use flake github:devinsideyou/scala-seed
;;
;; learn more: https://github.com/DevInsideYou/scala-seed
;;
;; you can use it in a top directory for all your scala project like
;; `~/ws/scala/...'
;;
;; Bootstrap a scala project:
;;
;;;; cs launch giter8 -- devinsideyou/scala-seed  # Scala 2
;;
;;;; cs launch giter8 -- devinsideyou/scala3-seed # Scala 3
;;
;; To install `metals' in your project
;;
;;;; cs bootstrap \
;;;; --java-opt -XX:+UseG1GC \
;;;; --java-opt -XX:+UseStringDeduplication  \
;;;; --java-opt -Xss4m \
;;;; --java-opt -Xms100m \
;;;; --java-opt -Dmetals.client=emacs \
;;;; org.scalameta:metals_2.13:1.3.2 -o metals -f
;;
;; You may be able to tweak the above -o option to put it straight
;; into your PATH. Otherwise, add it to your `exec-path' in Emacs
;;
;;;; (setq exec-path (append exec-path '("/Users/hub/ws/learning-scala/playground")))

;;; Code:

(use-package scala-ts-mode
  :straight '(scala-ts-mode :type git :host github :repo "KaranAhlawat/scala-ts-mode")
  :bind
  ;; *.worksheet.sc evaluates on save
  ("C-c C-c" . save-buffer)
 )
(use-package sbt-mode
  :commands sbt-start sbt-command
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map)
  ;; compilation-skip-threshold tells the compilation minor-mode
  ;; which type of compiler output can be skipped. 1 = skip info
  ;; 2 = skip info and warnings.
  (setq compilation-skip-threshold 1)

  ;; Bind C-a to 'comint-bol when in sbt-mode. This will move the
  ;; cursor to just after prompt.
  (local-set-key (kbd "C-a") 'comint-bol)

  ;; Bind M-RET to 'comint-accumulate. This will allow you to add
  ;; more than one line to scala console prompt before sending it
  ;; for interpretation. It will keep your command history cleaner.
  (local-set-key (kbd "S-RET") 'comint-accumulate)
  (setq sbt:ansi-support t)
  ;; sbt-supershell kills sbt-mode:
  ;; https://github.com/hvesalai/emacs-sbt-mode/issues/152
  (setq sbt:program-options '("-Dsbt.supershell=false"))
  (add-hook 'sbt-mode-hook
            (lambda ()
              (setq prettify-symbols-alist
                    `((,(expand-file-name (directory-file-name (sbt:find-root))) . ?⌂)
                      (,(expand-file-name "~") . ?~)))
              (prettify-symbols-mode t)))
  ;; default (sbt) is not enough to get ANSI colors as sbt infers that
  ;; it's not supported. Forcing colors in sbt output.
  ;;(setq sbt:program-name "sbt -Dspecs2.color=true -Dsbt.log.format=true")
  (defun hub/sbt-start ()
    "Go to (or start) sbt buffer without affecting the current buffer."
    (interactive)
    (hub/dwim-other-window 'sbt-start))
  (evil-define-key 'normal scala-mode-map ",g." 'sbt-find-definitions)
  (evil-define-key 'normal scala-mode-map ",bB" 'sbt-run-previous-command)
  (evil-define-key 'normal scala-mode-map ",bb" 'sbt-command)
  (evil-define-key 'normal scala-mode-map ",bh" 'sbt-hydra)
  (evil-define-key 'normal scala-mode-map ",e/" 'sbt-grep)
  (evil-define-key 'normal scala-mode-map ",gu" 'sbt-find-usages)
  (evil-define-key 'normal scala-mode-map ",es" 'hub/sbt-start)
  (evil-define-key 'visual scala-mode-map ",el" 'sbt-send-region))

(provide 'setup-scala)
;;; setup-scala.el ends here