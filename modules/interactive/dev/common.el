;;; common.el --- Common programming settings -*- lexical-binding: t; -*-

;;; Commentary:
;; Shared configuration for development across languages.

;;; Code:

(require 'hub-utils)

;; Ediff
(setq ediff-diff-options "-w")
(setq ediff-split-window-function 'split-window-horizontally)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; Navigating
(use-package ggtags :commands ggtags-mode :diminish ggtags-mode)
(use-package ag :commands (ag ag-files ag-regexp ag-project ag-dired)
  :config (setq ag-highlight-search t ag-reuse-buffers t))

;; Indenting
(setq-default indent-tabs-mode nil)
(setq backward-delete-char-untabify-method 'hungry)
(use-package editorconfig :defer t :diminish editorconfig-mode)
(use-package dtrt-indent :disabled t :defer 3
  :config (dtrt-indent-mode)
  (setq dtrt-indent-min-quality 60 dtrt-indent-verbosity 3))

;; Automatically indent yanked text in prog-modes
(dolist (command '(yank yank-pop))
  (eval `(defadvice ,command (after indent-region activate)
	   (and (not current-prefix-arg)
		(derived-mode-p 'prog-mode)
		(let ((mark-even-if-inactive transient-mark-mode))
		  (indent-region (region-beginning) (region-end) nil))))))

(use-package apheleia :config (apheleia-global-mode +1))

;; highlight TODO, FIXME, etc.
(add-hook 'prog-mode-hook 'hub/font-lock-comment-annotations)
(setq treesit-font-lock-level 4)

;; Folding (interactive only)
(use-package origami
  :unless noninteractive
  :commands origami-mode
  :after (hydra)
  :init
  (defhydra hydra-folding (:color red :hint nil)
	    "\n _o_pen node    _n_ext fold       toggle forw_a_rd    _u_ndo            _F_ill column: %`fill-column
 _c_lose node   _p_revious fold   toggle _A_ll        _r_edo            e_x_it\n _z_oom on node\n"
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
  :bind (:map evil-normal-state-map (",Z" . hydra-folding/body)))

;; Comint & compilation
(setq comint-scroll-to-bottom-on-input t
      comint-scroll-to-bottom-on-output t
      comint-show-maximum-output t
      comint-input-ignoredups t
      comint-completion-addsuffix t
      comint-buffer-maximum-size 10000)
(add-hook 'comint-output-filter-functions 'comint-truncate-buffer)
(evil-define-key 'normal comint-mode-map ",ee" 'comint-clear-buffer)
(evil-define-key 'insert comint-mode-map (kbd "C-c C-e") 'comint-clear-buffer)

(require 'ansi-color)
(defun colorize-compilation-buffer ()
  "Accept coloured output from testing."
  (ansi-color-apply-on-region compilation-filter-start (point)))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)
(setq compilation-scroll-output 'first-error
      compilation-always-kill t
      compilation-auto-jump-to-first-error t
      compilation-context-lines 5)

(add-hook 'prog-mode-hook
	  (lambda ()
	    (subword-mode)
	    (flyspell-prog-mode)
	    (turn-on-auto-fill)
	    (electric-indent-local-mode)
	    (define-key evil-normal-state-map (kbd ",br") 'recompile)
	    (define-key evil-normal-state-map (kbd ",bx") 'kill-compilation)
	    (define-key evil-insert-state-map (kbd "M-RET") 'indent-new-comment-line)
	    (define-key evil-normal-state-map (kbd "g)") 'flymake-goto-next-error)
	    (define-key evil-normal-state-map (kbd "g(") 'flymake-goto-previous-error)))

;; Env
(use-package direnv :after (exec-path-from-shell) :config (direnv-mode))

;; Help/UI niceties
;; Enable which-function only in programming buffers to avoid mode-line hook
;; errors in special buffers (e.g., Treemacs, magit status, etc.).
(add-hook 'prog-mode-hook (lambda () (which-function-mode 1)))
(define-key evil-normal-state-map (kbd ",hI") 'info)
(use-package whitespace
  :diminish global-whitespace-mode
  :config
  (setq whitespace-style '(face tabs lines-tail empty trailing)
	whitespace-global-modes '(not org-mode))
  (global-whitespace-mode))
(use-package whitespace-cleanup-mode
  :diminish whitespace-cleanup-mode
  :defer t
  :init (global-whitespace-cleanup-mode))

;; Treesit (generic setup only; language-specific sources and remaps live under modules/lang/*)
(use-package treesit :straight (:type built-in)
  :config
  (setq treesit-font-lock-settings t
	treesit-simple-indent-rules t
	treesit-defun-type-regexp t
	treesit-defun-name-function t)
  (run-with-idle-timer 0.1 nil #'treesit-major-mode-setup))

;; Dev helpers for Tree-sitter setup
(defun hub/treesit-install-missing (&optional langs)
  "Install missing Tree-sitter grammars for LANGS or known sources."
  (interactive)
  (unless (featurep 'treesit)
    (user-error "treesit not available in this Emacs"))
  (let* ((known (mapcar #'car treesit-language-source-alist))
	 (targets (or langs known))
	 (missing (seq-filter (lambda (l) (not (treesit-language-available-p l))) targets)))
    (if (null missing)
	(message "All Tree-sitter grammars present: %s" targets)
      (dolist (lang missing)
	(condition-case err
	    (progn (message "Installing grammar: %s" lang)
		   (treesit-install-language-grammar lang))
	  (error (message "Failed installing %s: %S" lang err)))))))

(defun hub/treesit-report (&optional lang)
  "Report Tree-sitter setup; optionally focus on LANG."
  (interactive)
  (let* ((lang (or lang 'scala)))
    (message "treesit available: %s" (treesit-available-p))
    (message "extra load path: %S" treesit-extra-load-path)
    (message "source for %s: %S" lang (alist-get lang treesit-language-source-alist))
    (message "grammar installed for %s: %s" lang (treesit-language-available-p lang))))

;; Lazy auto-install of grammar on first use
(defcustom hub/treesit-auto-install t
  "When non-nil, automatically install missing Tree-sitter grammars on demand."
  :type 'boolean)

(defvar hub/treesit-mode-language-alist
  '((js-ts-mode . javascript)
    (typescriptreact-ts-mode . tsx))
  "Mapping from ts major modes to Tree-sitter language symbols for special cases.")

(defun hub/treesit-language-for-mode (mode)
  "Best-effort mapping from MODE (a symbol) to a Tree-sitter language symbol."
  (or (alist-get mode hub/treesit-mode-language-alist)
      (let* ((name (symbol-name mode)))
	(when (string-match "^\([^-]+\)-ts-mode$" name)
	  (intern (match-string 1 name))))))

(defun hub/treesit-ensure-for-current-mode ()
  "Ensure Tree-sitter grammar for current ts-mode is installed, lazily."
  (when (and hub/treesit-auto-install (not noninteractive) (featurep 'treesit))
    (let* ((lang (hub/treesit-language-for-mode major-mode))
	   (src (and lang (alist-get lang treesit-language-source-alist))))
      (when (and lang src (not (treesit-language-available-p lang)))
	(condition-case err
	    (progn
	      (message "Installing missing Tree-sitter grammar: %s" lang)
	      (treesit-install-language-grammar lang)
	      (message "Installed grammar: %s" lang))
	  (error (message "Failed installing grammar %s: %S" lang err)))))))

(add-hook 'after-change-major-mode-hook #'hub/treesit-ensure-for-current-mode)

;; Nix configuration moved to modules/lang/nix.el

;; Emacs Lisp conveniences (language-agnostic helpers)
(require 'jka-compr)
(evil-define-key 'normal lisp-mode-shared-map
		 ",." 'find-function
		 ",hf" 'describe-function
		 ",hv" 'describe-variable
		 ",hc" 'describe-char
		 ",el" 'eval-last-sexp
		 ",il" 'eval-print-last-sexp)
(eval-after-load 'eldoc '(diminish 'eldoc-mode))

;; Language-specific modules now define their own modes and settings under modules/lang/*

;; Eglot basics (language-specific servers/hooks are configured per language under modules/lang/*)
(use-package eglot)

;; AWK
(add-hook 'awk-mode-hook (lambda () (setq c-basic-offset 2)))

(provide 'dev/common)
;;; common.el ends here
