;;; common.el --- Common programming settings -*- lexical-binding: t; -*-

;;; Commentary:
;; Shared configuration for development across languages.

;;; Code:
;; Navigating
(use-package ggtags
  :commands ggtags-mode
  :diminish ggtags-mode)

(use-package ag
  :commands (ag ag-files ag-regexp ag-project ag-dired)
  :config (setq ag-highlight-search t
                ag-reuse-buffers t))

;; Indenting
(setq-default indent-tabs-mode nil)     ; no tabs, only spaces
;; don't delete tabs one space at a time
(setq backward-delete-char-untabify-method 'hungry)
(use-package editorconfig
  :defer t
  :diminish editorconfig-mode)
(use-package dtrt-indent
  :disabled t
  :defer 3
  :config
  (dtrt-indent-mode)
  (setq dtrt-indent-min-quality 60
        dtrt-indent-verbosity 3))
; automatically indent yanked text in prog-modes
(dolist (command '(yank yank-pop))
  (eval `(defadvice ,command (after indent-region activate)
           (and (not current-prefix-arg)
                (derived-mode-p 'prog-mode)
                (let ((mark-even-if-inactive transient-mark-mode))
                  (indent-region (region-beginning) (region-end) nil))))))

(use-package apheleia
  :ensure t
  :config
  (apheleia-global-mode +1))

;; highlight TODO, FIXME, etc.
(add-hook 'prog-mode-hook 'hub/font-lock-comment-annotations)
;; (show-paren-mode 1)                     ; highlight matching brackets
(setq treesit-font-lock-level 4)

(use-package origami
  :commands origami-mode
  :after (hydra)
  :init
  (defhydra hydra-folding (:color red :hint nil)
    "
_o_pen node    _n_ext fold       toggle forw_a_rd    _u_ndo            _F_ill column: %`fill-column
_c_lose node   _p_revious fold   toggle _A_ll        _r_edo            e_x_it
_z_oom on node
"
    ("o" origami-open-node)
    ("c" origami-close-node)
    ("z" origami-show-only-node)
    ("u" origami-undo)
    ("r" origami-redo)
    ("n" origami-next-fold)
    ("p" origami-previous-fold)
    ("a" origami-forward-toggle-node)
    ("A" origami-toggle-all-nodes)
    ("F" fill-column)
    ("x" nil :color blue))
  :bind (:map evil-normal-state-map
              (",Z" . hydra-folding/body)))


;;; Building
;;; Comint
(setq
 comint-scroll-to-bottom-on-input t
 comint-scroll-to-bottom-on-output t
 comint-show-maximum-output t
 comint-input-ignoredups t
 comint-completion-addsuffix t
 comint-buffer-maximum-size 10000
 )
;; truncate buffers continuously
(add-hook 'comint-output-filter-functions 'comint-truncate-buffer)
(evil-define-key 'normal comint-mode-map ",ee" 'comint-clear-buffer)
(evil-define-key 'insert comint-mode-map (kbd "C-c C-e") 'comint-clear-buffer)

(use-package flycheck
  :commands flycheck-mode
  :init (global-flycheck-mode)
  :defer t
  :disabled t
  :config
  (define-key evil-normal-state-map (kbd ",)") 'flycheck-next-error)
  (define-key evil-normal-state-map (kbd ",(") 'flycheck-previous-error)
)

;; Compilation
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  "Accept coloured output from testing."
  (ansi-color-apply-on-region compilation-filter-start (point)))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)
(setq compilation-scroll-output 'first-error
      compilation-always-kill t
      compilation-auto-jump-to-first-error t
      compilation-context-lines 5)      ; auto scroll in compilation buffer

(add-hook 'prog-mode-hook
          (lambda () (progn
                       ; all code buffers with nlinum
                       ; (so much faster than linum!)
                       ;; (nlinum-mode 1) ; line # are overrated
                       ;; (ggtags-mode 1) ; trial
                       (subword-mode) ; camelcase moves
                       (flyspell-prog-mode)
                       (turn-on-auto-fill)
                       ;; auto-fill comments and only them
                       ;; (setq-local comment-auto-fill-only-comments t)
                       (eldoc-mode)
                       (origami-mode)
                       (editorconfig-mode 1)
                       (electric-indent-local-mode)
                       (define-key evil-normal-state-map (kbd ",bb") 'compile)
                       (define-key evil-normal-state-map (kbd ",br") 'recompile)
                       (define-key evil-normal-state-map (kbd ",bx") 'kill-compilation)
                       (define-key evil-insert-state-map (kbd "M-RET") 'indent-new-comment-line)
                       (define-key evil-normal-state-map (kbd "g)") 'flymake-goto-next-error)
                       (define-key evil-normal-state-map (kbd "g(") 'flymake-goto-previous-error)
                       )))
;; (setq linum-format " %3d ")    ; remove graphical glitches with fringe

;; https://github.com/wbolster/emacs-direnv
(use-package direnv
  :after (exec-path-from-shell)
  :config
  (direnv-mode))

;; Help
(which-function-mode 1)                 ; which function the point is in
(define-key evil-normal-state-map (kbd ",hI") 'info)

(use-package whitespace
  :diminish global-whitespace-mode
  :config
  (setq whitespace-style '(face tabs lines-tail empty trailing))
  (setq whitespace-global-modes '(not org-mode))
  (global-whitespace-mode))
(use-package whitespace-cleanup-mode
  :diminish whitespace-cleanup-mode
  :defer t
  :init (global-whitespace-cleanup-mode))

;; LANGUAGES
(use-package treesit
  :straight (:type built-in)
  :config
  (setq treesit-language-source-alist
        '((bash "https://github.com/tree-sitter/tree-sitter-bash")
          (cmake "https://github.com/uyha/tree-sitter-cmake")
          (css "https://github.com/tree-sitter/tree-sitter-css")
          (elisp "https://github.com/Wilfred/tree-sitter-elisp")
          (go "https://github.com/tree-sitter/tree-sitter-go")
          (html "https://github.com/tree-sitter/tree-sitter-html")
          (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
          (json "https://github.com/tree-sitter/tree-sitter-json")
          (make "https://github.com/alemuller/tree-sitter-make")
          (markdown "https://github.com/ikatyang/tree-sitter-markdown")
          (python "https://github.com/tree-sitter/tree-sitter-python")
          (toml "https://github.com/tree-sitter/tree-sitter-toml")
          (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
          (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
          (yaml "https://github.com/ikatyang/tree-sitter-yaml"))
        treesit-font-lock-settings t
        treesit-simple-indent-rules t
        treesit-defun-type-regexp t
        treesit-defun-name-function t)
  (setq major-mode-remap-alist
        '((yaml-mode . yaml-ts-mode)
          (bash-mode . bash-ts-mode)
          (js2-mode . js-ts-mode)
          (typescript-mode . typescript-ts-mode)
          (typescriptreact-mode . typescriptreact-ts-mode)
          (json-mode . json-ts-mode)
          (css-mode . css-ts-mode)
          (python-mode . python-ts-mode)
          ))
  (treesit-major-mode-setup)
  )

(provide 'dev-common)
;;; common.el ends here
