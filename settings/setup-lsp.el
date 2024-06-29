;;; setup-lsp.el --- set up LSP                      -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Hubert Behaghel

;; Author: Hubert Behaghel <behaghel@gmail.com>
;; Keywords: tools

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

;; LSP offers all the usual IDE features across pretty much all languages

;;; Code:


;; set prefix for lsp-command-keymap
;; all lsp commands: https://github.com/emacs-lsp/lsp-mode#commands
(setq lsp-keymap-prefix "M-l")
(use-package lsp-mode
  :defer t
  :init
  (setq lsp-prefer-flymake nil
        ;; doesn't work with pyls :(
        lsp-enable-snippet nil)
  :hook (
         (sh-mode . lsp)
         (lsp-mode . lsp-lens-mode)
         )
  :commands lsp)

(use-package lsp-ui
  :defer t
  :commands lsp-ui-mode
  :bind (:map evil-normal-state-map
              (",B?" . netrom/lsp-hydra/body))
  :config
  ;; https://www.mortens.dev/blog/emacs-and-the-language-server-protocol/
  (setq netrom--general-lsp-hydra-heads
        '(;; Xref
          ("d" xref-find-definitions "Definitions" :column "Xref")
          ("D" xref-find-definitions-other-window "-> other win")
          ("r" xref-find-references "References")
          ("s" counsel-ag "Search")

          ;; Peek
          ("C-d" lsp-ui-peek-find-definitions "Definitions" :column "Peek")
          ("C-r" lsp-ui-peek-find-references "References")
          ("C-i" lsp-ui-peek-find-implementation "Implementation")

          ;; LSP
          ("p" lsp-describe-thing-at-point "Describe at point" :column "LSP")
          ("C-a" lsp-execute-code-action "Execute code action")
          ("R" lsp-rename "Rename")
          ("t" lsp-goto-type-definition "Type definition")
          ("i" lsp-goto-implementation "Implementation")
          ("f" lsp-ui-imenu "Filter funcs/classes (Helm)")
          ("C-c" lsp-describe-session "Describe session")

          ;; Flycheck
          ("l" lsp-ui-flycheck-list "List errs/warns/notes" :column "Flycheck"))

        netrom--misc-lsp-hydra-heads
        '(;; Misc
          ("q" nil "Cancel" :column "Misc")
          ("b" pop-tag-mark "Back")))

  ;; Create general hydra.
  (eval `(defhydra netrom/lsp-hydra (:color blue :hint nil)
           ,@(append
              netrom--general-lsp-hydra-heads
              netrom--misc-lsp-hydra-heads)))

  )

(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)

;; Use the Debug Adapter Protocol for running tests and debugging
(use-package posframe
  ;; Posframe is a pop-up tool that must be manually installed for dap-mode
  )

;; Debugging
(use-package dap-mode
  ;; :pin melpa
  :defer t
  :commands (dap-ui-mode dap-mode dap-hydra)
  :hook (
         (lsp-mode . dap-mode)
         (lsp-mode . dap-ui-mode)
         ;; enables mouse hover support
         (lsp-mode . dap-tooltip-mode)
         ;; use tooltips for mouse hover
         ;; if it is not enabled `dap-mode' will use the minibuffer.
         ;; (lsp-mode . tooltip-mode)
         ;; displays floating panel with debug buttons
         ;; requies emacs 26+
         ;; (lsp-mode . dap-ui-controls-mode)
         ;; (dap-server-log-mode . XXX repaint last entry with
         ;; ansi-colorizing, see function colorize-compilation-buffer)
         )
  :bind (:map evil-normal-state-map
              (",dd" . dap-debug)
              (",dl" . dap-debug-last)
              (",de" . dap-eval-thing-at-point)
              (",dD" . dap-debug-recent)
              (",dc" . dap-continue)
              (",dB" . dap-breakpoint-toggle)
              (",dn" . dap-next)
              (",dt" . dap-step-in)
              (",ds" . dap-step-out)
              :map evil-visual-state-map
              (",d:" . dap-eval-region )
              :map dap-server-log-mode-map
              ( "n" . dap-next )
              ( "i" . dap-step-in )
              ( "o" . dap-step-out )
              ( "c" . dap-continue )
              ( "L" . dap-ui-locals )
              ( "S" . dap-ui-sessions )
              ( "E" . dap-ui-expressions )
              ( "B" . dap-ui-breakpoints )
              ( "R" . dap-ui-repl )
              ( "l" . dap-go-to-output-buffer )
              ( "q" . dap-disconnect )
              ;; H : Continue until Point
              ( ":" . dap-eval )
              ( "b" . dap-breakpoint-add )
              ( "u" . dap-breakpoint-delete )
              ( ">" . dap-switch-stack-frame )
              ( "<" . dap-switch-stack-frame )
              ;; g? : Help
              ;; J : Jump to debugger location
              ( "R" . dap-restart-frame )
              :map +dap-running-session-mode-map
              ( ",dn" . dap-next )
              ( ",dt" . dap-step-in )
              ( ",ds" . dap-step-out )
              ( ",dc" . dap-continue )
              ( ",dL" . dap-ui-locals )
              ( ",dS" . dap-ui-sessions )
              ( ",dE" . dap-ui-expressions )
              ( ",dB" . dap-ui-breakpoints )
              ( ",dR" . dap-ui-repl )
              ( ",dt" . dap-go-to-output-buffer )
              ( ",dq" . dap-disconnect )
              ;; H : Continue until Point
              ( ",d:" . dap-eval )
              ( ",dba" . dap-breakpoint-add )
              ( ",dbu" . dap-breakpoint-delete )
              ( ",dbb" . dap-breakpoint-toggle )
              ( ",dbc" . dap-breakpoint-condition )
              ( ",dbC" . dap-breakpoint-hit-condition )
              ( ",dbl" . dap-breakpoint-log-message )
              ( ",d>" . dap-switch-stack-frame )
              ( ",d<" . dap-switch-stack-frame )
              ;; g? : Help
              ;; J : Jump to debugger location
              ( ",dR" . dap-restart-frame )
              )
  :config
  ;; https://github.com/emacs-lsp/dap-mode/wiki/How-to-activate-minor-modes-when-stepping-through-code
  (define-minor-mode +dap-running-session-mode
    "A mode for adding keybindings to running sessions"
    nil
    nil
    (make-sparse-keymap)
    (evil-normalize-keymaps) ;; if you use evil, this is necessary to update the keymaps
    ;; The following code adds to the dap-terminated-hook
    ;; so that this minor mode will be deactivated when the debugger finishes
    (when +dap-running-session-mode
      (let ((session-at-creation (dap--cur-active-session-or-die)))
        (add-hook 'dap-terminated-hook
                  (lambda (session)
                    (when (eq session session-at-creation)
                      (+dap-running-session-mode -1)))))))

  ;; Activate this minor mode when dap is initialized
  (add-hook 'dap-session-created-hook '+dap-running-session-mode)

  ;; Activate this minor mode when hitting a breakpoint in another file
  (add-hook 'dap-stopped-hook '+dap-running-session-mode)

  ;; Activate this minor mode when stepping into code in another file
  (add-hook 'dap-stack-frame-changed-hook (lambda (session)
                                            (when (dap--session-running session)
                                              (+dap-running-session-mode 1))))
)

;; Use the Tree View Protocol for viewing the project structure and triggering compilation
(use-package lsp-treemacs
  :disabled t
  :after (treemacs)
  :defer t
  ;; :pin melpa
  :config
  (lsp-metals-treeview-enable t)
  (setq lsp-metals-treeview-show-when-views-received t)
  (lsp-treemacs-sync-mode 1)
  )

(use-package company-lsp
  :defer t
  ;; :pin melpa
  :config
  (setq company-lsp-cache-candidates 'auto)
  (setq company-minimum-prefix-length 1
        company-idle-delay 0.0)         ;default is 0.2
  ;; https://www.mortens.dev/blog/emacs-and-the-language-server-protocol/
  ;; Disable client-side cache because the LSP server does a better job.
  ;; (setq company-transformers nil
  ;;       company-lsp-async t
  ;;       company-lsp-cache-candidates nil)
)



(provide 'setup-lsp)
;;; setup-lsp.el ends here
