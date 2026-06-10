;;; common.el --- Common programming settings -*- lexical-binding: t; -*-

;;; Commentary:
;; Shared configuration for development across languages.

;;; Code:

(require 'hub-utils)

;; Ediff
(setq ediff-diff-options "-w")
(setq ediff-split-window-function 'split-window-horizontally)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(defun hub/ediff-use-diff-faces (&rest _)
  "Reuse diff-mode faces for Ediff to keep contrast in TTY sessions."
  (dolist (mapping '((ediff-current-diff-A . diff-removed)
		     (ediff-current-diff-B . diff-added)
		     (ediff-current-diff-C . diff-changed)
		     (ediff-current-diff-Ancestor . diff-changed)
		     (ediff-fine-diff-A . diff-refine-removed)
		     (ediff-fine-diff-B . diff-refine-added)
		     (ediff-fine-diff-C . diff-refine-changed)
		     (ediff-fine-diff-Ancestor . diff-refine-changed)
		     (ediff-merge-current-diff-A . diff-removed)
		     (ediff-merge-current-diff-B . diff-added)
		     (ediff-merge-current-diff-C . diff-changed)
		     (ediff-merge-current-diff-Ancestor . diff-changed)
		     (ediff-merge-diff-A . diff-removed)
		     (ediff-merge-diff-B . diff-added)
		     (ediff-merge-diff-C . diff-changed)
		     (ediff-merge-diff-Ancestor . diff-changed)
		     (ediff-merge-fine-diff-A . diff-refine-removed)
		     (ediff-merge-fine-diff-B . diff-refine-added)
		     (ediff-merge-fine-diff-C . diff-refine-changed)
		     (ediff-merge-fine-diff-Ancestor . diff-refine-changed)))
    (when (and (facep (car mapping)) (facep (cdr mapping)))
      (set-face-attribute (car mapping) nil :inherit (cdr mapping)
			  :foreground nil :background nil)))
  (dolist (face '(ediff-even-diff-A ediff-even-diff-B ediff-even-diff-C
				    ediff-even-diff-Ancestor ediff-odd-diff-A ediff-odd-diff-B
				    ediff-odd-diff-C ediff-odd-diff-Ancestor))
    (when (facep face)
      (set-face-attribute face nil :inherit 'diff-context
			  :foreground nil :background nil))))
(add-hook 'ediff-load-hook #'hub/ediff-use-diff-faces)
(add-hook 'enable-theme-functions #'hub/ediff-use-diff-faces)

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
(defun hub/indent-yanked-region-in-prog-mode (&rest _args)
  "Indent the just-yanked region in programming buffers."
  (and (not current-prefix-arg)
       (derived-mode-p 'prog-mode)
       (let ((mark-even-if-inactive transient-mark-mode))
	 (indent-region (region-beginning) (region-end) nil))))

(advice-add 'yank :after #'hub/indent-yanked-region-in-prog-mode)
(advice-add 'yank-pop :after #'hub/indent-yanked-region-in-prog-mode)

(use-package apheleia :config (apheleia-global-mode +1))

;; highlight TODO, FIXME, etc.
(add-hook 'prog-mode-hook 'hub/font-lock-comment-annotations)

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

(defcustom hub/flyspell-prog-idle-delay 1.0
  "Idle delay before enabling Flyspell in visible programming buffers."
  :type 'number
  :group 'convenience)

(defvar-local hub/flyspell-prog--enable-scheduled nil
  "Non-nil when Flyspell enablement is already scheduled for this buffer.")

(defun hub/flyspell-prog-enable-visible-buffer (window)
  "Schedule Flyspell for WINDOW's visible programming buffer."
  (when (window-live-p window)
    (with-current-buffer (window-buffer window)
      (when (and (derived-mode-p 'prog-mode)
		 (not (bound-and-true-p flyspell-mode))
		 (not hub/flyspell-prog--enable-scheduled))
	(setq hub/flyspell-prog--enable-scheduled t)
	(let ((buffer (current-buffer)))
	  (run-with-idle-timer
	   hub/flyspell-prog-idle-delay nil
	   (lambda ()
	     (when (buffer-live-p buffer)
	       (with-current-buffer buffer
		 (setq hub/flyspell-prog--enable-scheduled nil)
		 (when (and (derived-mode-p 'prog-mode)
			    (not (bound-and-true-p flyspell-mode)))
		   (flyspell-prog-mode)))))))))))

(add-hook 'window-buffer-change-functions #'hub/flyspell-prog-enable-visible-buffer)

(add-hook 'prog-mode-hook
	  (lambda ()
	    (subword-mode)
	    (turn-on-auto-fill)
	    (electric-indent-local-mode)
	    (define-key evil-normal-state-map (kbd ",br") 'recompile)
	    (define-key evil-normal-state-map (kbd ",bx") 'kill-compilation)
	    (define-key evil-insert-state-map (kbd "M-RET") 'indent-new-comment-line)
	    (define-key evil-normal-state-map (kbd "g)") 'flymake-goto-next-error)
	    (define-key evil-normal-state-map (kbd "g(") 'flymake-goto-previous-error)))

;; Env
(use-package direnv
  :commands (direnv-allow direnv-update-directory-environment direnv-update-environment)
  :init
  (defcustom hub/direnv-idle-delay 0.5
    "Idle seconds before applying direnv for the current buffer."
    :type 'number
    :group 'convenience)

  (defvar hub/direnv--pending-timer nil
    "Pending idle timer for deferred direnv updates.")

  (defvar hub/direnv--last-scheduled-directory nil
    "Last directory scheduled for a direnv update.")

  (defun hub/direnv--buffer-directory (&optional buffer)
    "Return local directory relevant for direnv in BUFFER."
    (with-current-buffer (or buffer (current-buffer))
      (let ((dir (or (and buffer-file-name (file-name-directory buffer-file-name))
		     (and (derived-mode-p 'comint-mode 'compilation-mode 'dired-mode 'eshell-mode)
			  default-directory))))
	(when (and dir (not (file-remote-p dir)))
	  (file-name-as-directory (expand-file-name dir))))))

  (defun hub/direnv--project-root-with-envrc (dir)
    "Return nearest ancestor of DIR containing .envrc, or nil."
    (when dir
      (locate-dominating-file dir ".envrc")))

  (defun hub/direnv--apply-directory (dir)
    "Apply direnv environment for DIR."
    (when (and dir (file-directory-p dir) (not (file-remote-p dir)))
      (condition-case err
	  (direnv-update-directory-environment dir)
	(error
	 (message "[direnv] failed for %s: %s" dir (error-message-string err))))))

  (defun hub/direnv-schedule-update (&optional buffer)
    "Schedule a deferred direnv update for BUFFER.
The update runs on an idle timer so file visits and project switches can display
first; environment-sensitive tooling catches up shortly after."
    (let* ((dir (hub/direnv--buffer-directory buffer))
	   (env-root (hub/direnv--project-root-with-envrc dir)))
      (when (and env-root (not (equal env-root hub/direnv--last-scheduled-directory)))
	(setq hub/direnv--last-scheduled-directory env-root)
	(when (timerp hub/direnv--pending-timer)
	  (cancel-timer hub/direnv--pending-timer))
	(setq hub/direnv--pending-timer
	      (run-with-idle-timer hub/direnv-idle-delay nil
				   #'hub/direnv--apply-directory env-root)))))

  (add-hook 'find-file-hook #'hub/direnv-schedule-update)
  (add-hook 'dired-mode-hook #'hub/direnv-schedule-update)
  (add-hook 'eshell-mode-hook #'hub/direnv-schedule-update)
  (add-hook 'compilation-mode-hook #'hub/direnv-schedule-update)
  (add-hook 'comint-mode-hook #'hub/direnv-schedule-update)
  (add-hook 'post-command-hook #'hub/direnv-schedule-update))

;; Help/UI niceties
;; Enable which-function only in programming buffers to avoid mode-line hook
;; errors in special buffers (e.g., Treemacs, magit status, etc.).
(setq which-func-modes '(prog-mode))
(add-hook 'prog-mode-hook (lambda () (unless which-function-mode (which-function-mode 1))))
(define-key evil-normal-state-map (kbd ",hI") 'info)
(defun hub/whitespace-enable-buffer ()
  "Enable whitespace visualization in eligible editing buffers."
  (unless (derived-mode-p 'org-mode 'eve-mode)
    (whitespace-mode 1)))

(use-package whitespace
  :commands (whitespace-mode)
  :init
  (setq whitespace-style '(face tabs lines-tail empty trailing))
  (add-hook 'prog-mode-hook #'hub/whitespace-enable-buffer)
  (add-hook 'text-mode-hook #'hub/whitespace-enable-buffer))

(use-package whitespace-cleanup-mode
  :diminish whitespace-cleanup-mode
  :commands (global-whitespace-cleanup-mode whitespace-cleanup-mode)
  :init (global-whitespace-cleanup-mode))

;; Treesit (generic setup only; language-specific sources and remaps live under modules/lang/*)
(defvar hub/treesit--configured nil
  "Non-nil once generic Tree-sitter defaults have been configured.")

(defun hub/treesit-configure-for-prog-buffer ()
  "Configure generic Tree-sitter defaults for programming buffers."
  (when (and (not hub/treesit--configured)
	     (require 'treesit nil t))
    (setq hub/treesit--configured t)
    (setq treesit-font-lock-level 4
	  treesit-font-lock-settings t
	  treesit-simple-indent-rules t
	  treesit-defun-type-regexp t
	  treesit-defun-name-function t)
    (treesit-major-mode-setup)))

(add-hook 'prog-mode-hook #'hub/treesit-configure-for-prog-buffer)

;; Dev helpers for Tree-sitter setup
(defun hub/treesit-install-missing (&optional langs)
  "Install missing Tree-sitter grammars for LANGS or known sources."
  (interactive)
  (unless (require 'treesit nil t)
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
  (unless (require 'treesit nil t)
    (user-error "treesit not available in this Emacs"))
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

;; Eglot basics (language-specific servers/hooks are configured per language under modules/lang/*).
;; Defer LSP client loading until a language hook or command actually needs it.
(use-package eglot
  :defer t
  :commands (eglot eglot-ensure))

;; AWK
(add-hook 'awk-mode-hook (lambda () (setq c-basic-offset 2)))

(provide 'dev/common)
;;; common.el ends here
