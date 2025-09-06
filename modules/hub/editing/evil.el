;;; evil.el --- Editing: Evil & friends -*- lexical-binding: t; -*-

;;; Commentary:
;; Extracted from settings/setup-evil.el. Provides Evil configuration,
;; keybindings, and integrations (collection, surround, etc.).

;;; Code:

(use-package evil
  :demand t
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (defun hub/setup-elisp-debugging-keybindings ()
    "Install debugging keybindings for Emacs Lisp debugger."
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
				";" 'debugger-return-value))
  (advice-add 'evil-collection-debug-setup :after 'hub/setup-elisp-debugging-keybindings)
  :config
  (evil-mode 1)

  (use-package evil-leader)
  (use-package evil-matchit)
  (use-package evil-surround)
  (use-package evil-nerd-commenter)

  (use-package evil-collection
    :after (evil evil-matchit)
    :ensure t
    :custom (evil-collection-company-use-tng nil)
    :config
    (evil-collection-translate-key nil
				   '(evil-window-map evil-normal-state-map evil-motion-state-map
						     evil-treemacs-state-map Info-mode-map
						     magit-blame-read-only-mode-map git-rebase-mode-map)
				   "c" "h" "t" "j" "T" "J" "s" "k" "S" "K" "r" "l" "l" "c" "L" "C"
				   "h" "t" "H" "T" "k" "s" "K" "S" "j" "r" "é" "w" "É" "W")

    (defun hub/hjkl-rotation (_mode mode-keymaps &rest _rest)
      (evil-collection-translate-key 'normal mode-keymaps
				     "c" "h" "t" "j" "T" "J" "s" "k" "S" "K" "r" "l" "l" "c" "L" "C"
				     "h" "t" "H" "T" "k" "s" "K" "S" "j" "r" "é" "w" "É" "W"))
    (add-hook 'evil-collection-setup-hook #'hub/hjkl-rotation)

    (defun hub/setup-docview-keybindings ()
      (evil-collection-define-key 'normal 'doc-view-mode-map
				  (kbd "s s") nil
				  (kbd "s m") nil
				  (kbd "s b") nil
				  (kbd "s r") nil
				  (kbd "z s") 'doc-view-set-slice
				  (kbd "z m") 'doc-view-set-slice-using-mouse
				  (kbd "z b") 'doc-view-set-slice-from-bounding-box
				  (kbd "z r") 'doc-view-reset-slice))
    (add-hook 'doc-view-mode-hook #'hub/setup-docview-keybindings)

    (defun hub/setup-magit-keybindings ()
      (dolist (map (list magit-staged-section-map magit-untracked-section-map
			 magit-unstaged-section-map magit-file-section-map
			 magit-hunk-section-map))
	(define-key map "s" nil)
	(evil-define-key 'normal map "s" 'evil-previous-line)
	(evil-define-key 'normal map "à" 'magit-stage)
	(evil-define-key 'normal map "À" 'magit-unstage))
      (evil-collection-define-key 'normal 'magit-status-mode-map
				  (kbd "t") 'evil-next-line
				  (kbd "s") 'evil-previous-line
				  (kbd "T") 'magit-tag
				  (kbd "à") 'magit-stage
				  (kbd "À") 'magit-unstage))
    (add-hook 'magit-mode-hook #'hub/setup-magit-keybindings)
    (evil-collection-init))

  (global-evil-surround-mode 1)
  (define-key evil-visual-state-map (kbd ",s") 'evil-surround-change)
  (define-key evil-normal-state-map (kbd "W") 'evil-window-next)
  (define-key evil-window-map "h" 'evil-window-set-height)
  (define-key evil-window-map "_" 'split-window-vertically)
  (define-key evil-window-map "|" 'split-window-horizontally)
  (define-key evil-normal-state-map (kbd "C-à") 'evil-prev-buffer)
  (define-key key-translation-map (kbd "zR") (kbd "zL"))
  (define-key key-translation-map (kbd "zt") (kbd "zk"))
  (define-key evil-normal-state-map (kbd "©") 'backward-sexp)
  (define-key evil-normal-state-map (kbd "®") 'forward-sexp)

  (define-key evil-normal-state-map (kbd "M-p") 'eval-print-last-sexp)
  (define-key evil-normal-state-map (kbd "M-d") 'eval-last-sexp)
  (define-key evil-normal-state-map (kbd "M-B") 'eval-buffer)

  (global-unset-key (kbd "M-t"))
  (define-key evil-normal-state-map (kbd "M-t l") 'transpose-lines)
  (define-key evil-normal-state-map (kbd "M-t w") 'transpose-words)
  (define-key evil-normal-state-map (kbd "M-t s") 'transpose-sexps)
  (define-key evil-normal-state-map (kbd "M-t p") 'hub/transpose-params)

  (define-key evil-insert-state-map (kbd "C-e") 'end-of-line)
  (define-key evil-insert-state-map (kbd "C-y") 'yank)
  (define-key evil-normal-state-map (kbd ",y") 'hub/copy-buffer-file-name)
  (define-key evil-normal-state-map (kbd ",,") 'hub/switch-dwim)
  (define-key evil-normal-state-map (kbd ",'") 'hub/switch-to-other-buffer)
  (define-key evil-visual-state-map (kbd ",|") 'shell-command-on-region)
  (define-key evil-normal-state-map (kbd ",=") 'align-current)
  (define-key evil-normal-state-map (kbd "ç") 'delete-other-windows)
  (define-key evil-insert-state-map (kbd "M-ç") 'delete-other-windows)
  (define-key evil-normal-state-map (kbd "g}") 'next-error)
  (define-key evil-normal-state-map (kbd "g{") 'previous-error)
  (define-key evil-insert-state-map (kbd "C-d") nil)
  (define-key evil-normal-state-map (kbd "M-.") nil)
  (define-key evil-insert-state-map "\C-n" nil)
  (define-key evil-insert-state-map "\C-p" nil)

  (evil-set-initial-state 'ess-help-mode 'emacs)
  (evil-set-initial-state 'erc-mode 'emacs)
  (evil-set-initial-state 'image-mode 'emacs)
  (evil-set-initial-state 'cider-stacktrace-mode 'emacs)
  (evil-set-initial-state 'epa-key-list-mode 'emacs)
  (evil-set-initial-state 'twittering-mode 'normal)
  (evil-set-initial-state 'haskell-error-mode 'emacs)
  (evil-set-initial-state 'haskell-interactive-mode 'insert)
  (evil-set-initial-state 'prog-mode 'insert)
  (evil-set-initial-state 'calc-mode 'emacs)

  (setq evil-goto-definition-functions
	'(evil-goto-definition-xref evil-goto-definition-imenu
				    evil-goto-definition-semantic evil-goto-definition-search)))

(defun make-conditional-key-translation (key-from key-to translate-keys-p)
  "Conditionally translate KEY-FROM to KEY-TO per TRANSLATE-KEYS-P."
  (define-key key-translation-map key-from
	      (lambda (_prompt)
		(if (funcall translate-keys-p key-from) key-to key-from))))

(defun not-insert-state-p (key-from)
  "Return non-nil when not in insert state for KEY-FROM translation."
  (and (equal key-from (this-command-keys))
       (or (evil-motion-state-p) (evil-normal-state-p) (evil-visual-state-p))))

(make-conditional-key-translation (kbd "è") (kbd "C-x") 'not-insert-state-p)
(make-conditional-key-translation (kbd "È") (kbd "C-u") 'not-insert-state-p)

(provide 'hub/editing/evil)
;;; evil.el ends here
