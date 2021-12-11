;;; setup-evil.el --- Hubert's evil settings — otherwise I can't use my fingers  -*- lexical-binding: t; -*-
;; Copyright (C) 2016  Hubert Behaghel
;; Author: Hubert Behaghel <behaghel@gmail.com>
;;; Commentary:
;;  making a glove out of evil
;;; Code:
(use-package evil
  :init
  ;; otherwise evil gives it mappings... not sure what is loading it
  ;; in the first place
  (when (featurep 'tab-bar)
    (unload-feature 'tab-bar))
  (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
  (setq evil-want-keybinding nil)
  (defun hub/setup-elisp-debugging-keybindings ()
    "Install debugging keybindings."
    (evil-collection-define-key 'normal 'debugger-mode-map
      ">" 'backtrace-forward-frame
      "<" 'backtrace-backward-frame
      "C" 'backtrace-toggle-print-circle
      "Y" 'backtrace-toggle-print-gensym
      "…" 'backtrace-expand-ellipses
      "|" 'backtrace-multi-line
      "_" 'backtrace-single-line
      "S" 'backtrace-goto-source
      "H" 'backtrace-help-follow-symbol
      "L" 'backtrace-toggle-locals
      "n" 'debugger-step-through
      "i" 'debugger-step-through
      "c" 'debugger-continue
      "o" 'debugger-jump
      ":" 'debugger-eval-expression
      "x" 'debugger-record-expression
      "D" 'debugger-frame-clear
      ";" 'debugger-return-value
      )
    )
  (advice-add 'evil-collection-debug-setup :after 'hub/setup-elisp-debugging-keybindings)
  :config
  (evil-mode 1)

  (use-package evil-leader)
  (use-package evil-matchit)
  (use-package evil-surround)
  (use-package evil-nerd-commenter)

  (use-package evil-collection
    :after (evil evil-matchit)          ; matchit: to allow reuse of %
    :ensure t
    :custom (evil-collection-company-use-tng nil)
    :config
    (evil-collection-translate-key nil
      '(evil-window-map evil-normal-state-map evil-motion-state-map
                        evil-treemacs-state-map Info-mode-map
                        magit-blame-read-only-mode-map git-rebase-mode-map)
      "c" "h"
      "t" "j"
      "T" "J"
      "s" "k"
      "S" "K"
      "r" "l"
      "l" "c"
      "L" "C"
      "h" "t"
      "H" "T"
      "k" "s"
      "K" "S"
      "j" "r"
      "é" "w"
      "É" "W"
      )

    ;; org-agenda translation are in setup-org.el
    (defun hub/hjkl-rotation (_mode mode-keymaps &rest _rest)
      (evil-collection-translate-key 'normal mode-keymaps
        "c" "h"
        "t" "j"
        "T" "J"
        "s" "k"
        "S" "K"
        "r" "l"
        "l" "c"
        "L" "C"
        "h" "t"
        "H" "T"
        "k" "s"
        "K" "S"
        "j" "r"
        "é" "w"
        "É" "W"
        ))
    ;; called after evil-collection makes its keybindings
    (add-hook 'evil-collection-setup-hook #'hub/hjkl-rotation)

    ;; doc-view default keybindings want s as a prefix which clashes
    ;; with bépo layout
    (defun hub/setup-docview-keybindings ()
      (evil-collection-define-key 'normal 'doc-view-mode-map
        (kbd "z s") 'doc-view-set-slice
        (kbd "z m") 'doc-view-set-slice-using-mouse
        (kbd "z b") 'doc-view-set-slice-from-bounding-box
        (kbd "z r") 'doc-view-reset-slice
        )
      )
    (add-hook 'doc-view-mode-hook #'hub/setup-docview-keybindings)

    (defun hub/setup-magit-keybindings ()
      (dolist (map (list magit-staged-section-map magit-untracked-section-map magit-unstaged-section-map magit-file-section-map magit-hunk-section-map))
        (define-key map "s" nil)
        (evil-define-key 'normal map "s" 'evil-previous-line)
        (evil-define-key 'normal map "à" 'magit-stage)
        (evil-define-key 'normal map "À" 'magit-unstage))
      (evil-collection-define-key 'normal 'magit-status-mode-map
        (kbd "t") 'evil-next-line       ; default tag
        (kbd "s") 'evil-previous-line   ; stage
        (kbd "T") 'magit-tag
        (kbd "à") 'magit-stage
        (kbd "À") 'magit-unstage
        )
      )
    (add-hook 'magit-mode-hook #'hub/setup-magit-keybindings)
    (evil-collection-init))

  ;; surround
  ;; before my config for my config to win
  ;; otherwise in visual s call surround where I want to go on previous line
  (global-evil-surround-mode 1)
  (define-key evil-normal-state-map (kbd "W") 'evil-window-next)
  (define-key evil-window-map "h" 'evil-window-set-height)
  (define-key evil-window-map "_" 'split-window-vertically)
  (define-key evil-window-map "|" 'split-window-horizontally)
  (define-key evil-normal-state-map (kbd "C-à") 'evil-prev-buffer)
  (define-key key-translation-map (kbd "zR") (kbd "zL"))
  (define-key key-translation-map (kbd "zt") (kbd "zk"))
  (define-key evil-normal-state-map (kbd "©") 'backward-sexp)
  (define-key evil-normal-state-map (kbd "®") 'forward-sexp)

;; recreate what I am familiar with Pharo
  (define-key evil-normal-state-map (kbd "M-p") 'eval-print-last-sexp)
  (define-key evil-normal-state-map (kbd "M-d") 'eval-last-sexp)

  (define-key evil-normal-state-map (kbd "M-B") 'eval-buffer)

  ;; stolen from https://github.com/magnars/.emacs.d/blob/master/key-bindings.el
  ;; Transpose stuff with M-t
  (global-unset-key (kbd "M-t")) ;; which used to be transpose-words
  (define-key evil-normal-state-map (kbd "Tl") 'transpose-lines)
  (define-key evil-normal-state-map (kbd "Tw") 'transpose-words)
  (define-key evil-normal-state-map (kbd "Ts") 'transpose-sexps)
  (define-key evil-normal-state-map (kbd "Tp") 'hub/transpose-params)

  ;;;; Other mapping
  ;; (define-key evil-normal-state-map (kbd "C-e") 'evil-end-of-line)
  (define-key evil-insert-state-map (kbd "C-e") 'end-of-line)
  ;; (define-key evil-normal-state-map (kbd "C-y") 'evil-yank)
  (define-key evil-insert-state-map (kbd "C-y") 'yank)
  ;; (define-key evil-insert-state-map (kbd "C-y") 'yank)
  ;; (define-key evil-insert-state-map (kbd "M-y") 'yank-pop)
  (define-key evil-normal-state-map (kbd ",y") 'hub/copy-buffer-file-name)
  (define-key evil-normal-state-map (kbd ",,") 'hub/switch-dwim)
  (define-key evil-normal-state-map (kbd ",'") 'hub/switch-to-other-buffer)
  (define-key evil-visual-state-map (kbd ",|") 'shell-command-on-region)
  (define-key evil-normal-state-map (kbd ",=") 'align-current)
  (define-key evil-normal-state-map (kbd "ç") 'delete-other-windows)
  (define-key evil-insert-state-map (kbd "M-ç") 'delete-other-windows)

  ;; put xref at the front as it's smarter with codebases
  (setq evil-goto-definition-functions
        '(evil-goto-definition-xref evil-goto-definition-imenu
                                    evil-goto-definition-semantic
                                    evil-goto-definition-search)) )
;; stolen from http://www.emacswiki.org/emacs/Evil#toc12
;; Note: lexical-binding must be t in order for this to work correctly.
(defun make-conditional-key-translation (key-from key-to translate-keys-p)
  "Make a Key Translation such that if the translate-keys-p function returns true,
   key-from translates to key-to, else key-from translates to itself.  translate-keys-p
   takes key-from as an argument."
  (define-key key-translation-map key-from
    (lambda (prompt)
      (if (funcall translate-keys-p key-from) key-to key-from))))
(defun not-insert-state-p (key-from)
  "Returns whether conditional key translations should be active.  See make-conditional-key-translation function. "
  (and
   ;; Only allow a non identity translation if we're beginning a Key Sequence.
   (equal key-from (this-command-keys))
   (or (evil-motion-state-p) (evil-normal-state-p) (evil-visual-state-p))))

(make-conditional-key-translation (kbd "è") (kbd "C-x") 'not-insert-state-p)
(make-conditional-key-translation (kbd "È") (kbd "C-u") 'not-insert-state-p)

(require 'evil-matchit)
(global-evil-matchit-mode 1)

;; errors and compilation (*b*uild)
(define-key evil-normal-state-map (kbd "g}") 'next-error)
(define-key evil-normal-state-map (kbd "g{") 'previous-error)
(define-key evil-normal-state-map (kbd ",}") 'next-error)
(define-key evil-normal-state-map (kbd ",{") 'previous-error)
;; evil is crazy
(define-key evil-insert-state-map (kbd "C-d") nil)
(define-key evil-normal-state-map (kbd "M-.") nil)

;; Evil has those but I don't need evil to handle completion
;; (define-key evil-insert-state-map "\C-n" 'evil-complete-next)
;; (define-key evil-insert-state-map "\C-p" 'evil-complete-previous)
(define-key evil-insert-state-map "\C-n" nil)
(define-key evil-insert-state-map "\C-p" nil)
;;;; Default state
;; (evil-set-initial-state 'dired-mode 'emacs)
;; (evil-set-initial-state 'fundamental-mode 'emacs)
(evil-set-initial-state 'ess-help-mode 'emacs)
(evil-set-initial-state 'erc-mode 'emacs)
(evil-set-initial-state 'image-mode 'emacs)
(evil-set-initial-state 'cider-stacktrace-mode 'emacs)
(evil-set-initial-state 'epa-key-list-mode 'emacs)
;; (evil-set-initial-state 'magit-popup-mode 'emacs)
(evil-set-initial-state 'twittering-mode 'normal)
(evil-set-initial-state 'haskell-error-mode 'emacs)
(evil-set-initial-state 'haskell-interactive-mode 'insert)
(evil-set-initial-state 'intero-repl-mode 'insert)
;;; Info & Evil
;; (evil-set-initial-state 'Info 'emacs)
;; (evil-define-key 'motion Info-mode-map "l" nil) ; use l to say last

(provide 'setup-evil)
;;; setup-evil.el ends here
