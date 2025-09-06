;;; core.el --- Completion: Vertico/Orderless/Corfu -*- lexical-binding: t; -*-

;;; Commentary:
;; Extracted from settings/setup-completion.el. Provides minibuffer and
;; in-buffer completion stack.

;;; Code:

(use-package marginalia
  :general
  (:keymaps 'minibuffer-local-map
	    "M-A" 'marginalia-cycle)
  :custom
  (marginalia-max-relative-age 0)
  (marginalia-align 'right)
  :init (marginalia-mode))

(use-package all-the-icons-completion
  :after (marginalia all-the-icons)
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :init (all-the-icons-completion-mode))

(use-package vertico
  :demand t
  :straight (vertico :files (:defaults "extensions/*")
		     :includes (vertico-indexed vertico-flat vertico-grid
						vertico-mouse vertico-quick vertico-buffer
						vertico-repeat vertico-reverse
						vertico-directory vertico-multiform
						vertico-unobtrusive))
  :general
  (:keymaps '(normal insert visual motion) "M-," #'vertico-repeat)
  (:keymaps 'vertico-map
	    "<tab>" #'vertico-insert
	    "<escape>" #'minibuffer-keyboard-quit
	    "?" #'minibuffer-completion-help
	    "C-M-n" #'vertico-next-group
	    "C-M-p" #'vertico-previous-group
	    "<backspace>" #'vertico-directory-delete-char
	    "C-w" #'vertico-directory-delete-word
	    "C-<backspace>" #'vertico-directory-delete-word
	    "RET" #'vertico-directory-enter
	    "C-i" #'vertico-quick-insert
	    "C-o" #'vertico-quick-exit
	    "M-o" #'kb/vertico-quick-embark
	    "M-G" #'vertico-multiform-grid
	    "M-F" #'vertico-multiform-flat
	    "M-R" #'vertico-multiform-reverse
	    "M-U" #'vertico-multiform-unobtrusive
	    "C-l" #'kb/vertico-multiform-flat-toggle)
  :hook ((rfn-eshadow-update-overlay . vertico-directory-tidy)
	 (minibuffer-setup . vertico-repeat-save))
  :custom
  (vertico-count 13)
  (vertico-resize t)
  (vertico-cycle nil)
  (vertico-grid-separator "       ")
  (vertico-grid-lookahead 50)
  (vertico-buffer-display-action '(display-buffer-reuse-window))
  (vertico-multiform-categories
   '((file reverse)
     (consult-grep buffer)
     (consult-location)
     (imenu buffer)
     (library reverse indexed)
     (org-roam-node reverse indexed)
     (t reverse)))
  (vertico-multiform-commands
   '(("flyspell-correct-*" grid reverse)
     (org-refile grid reverse indexed)
     (consult-yank-pop indexed)
     (consult-flycheck)
     (consult-lsp-diagnostics)))
  :init
  (defun kb/vertico-multiform-flat-toggle ()
    "Toggle between flat and reverse displays."
    (interactive)
    (vertico-multiform--display-toggle 'vertico-flat-mode)
    (if vertico-flat-mode
	(vertico-multiform--temporary-mode 'vertico-reverse-mode -1)
      (vertico-multiform--temporary-mode 'vertico-reverse-mode 1)))
  (defun kb/vertico-quick-embark (&optional arg)
    "Embark on candidate using quick keys."
    (interactive)
    (when (vertico-quick-jump)
      (embark-act arg)))
  (defun kb/basic-remote-try-completion (string table pred point)
    (and (vertico--remote-p string)
	 (completion-basic-try-completion string table pred point)))
  (defun kb/basic-remote-all-completions (string table pred point)
    (and (vertico--remote-p string)
	 (completion-basic-all-completions string table pred point)))
  (add-to-list 'completion-styles-alist
	       '(basic-remote kb/basic-remote-try-completion
			      kb/basic-remote-all-completions nil))
  :config
  (vertico-mode)
  (vertico-multiform-mode)
  (defvar +vertico-current-arrow t)
  (cl-defmethod vertico--format-candidate :around
    (cand prefix suffix index start &context ((and +vertico-current-arrow
						   (not (bound-and-true-p vertico-flat-mode)))
					      (eql t)))
    (setq cand (cl-call-next-method cand prefix suffix index start))
    (if (bound-and-true-p vertico-grid-mode)
	(if (= vertico--index index)
	    (concat #("▶" 0 1 (face vertico-current)) cand)
	  (concat #("_" 0 1 (display " ")) cand))
      (if (= vertico--index index)
	  (concat #(" " 0 1 (display (left-fringe right-triangle vertico-current))) cand)
	cand))))

(use-package savehist :init (savehist-mode))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles basic-remote orderless))
				   (eglot (styles orderless))))
  (orderless-component-separator 'orderless-escapable-split-on-space)
  (orderless-matching-styles '(orderless-literal orderless-prefixes
						 orderless-initialism orderless-regexp))
  (orderless-style-dispatchers '(prot-orderless-literal-dispatcher
				 prot-orderless-strict-initialism-dispatcher
				 prot-orderless-flex-dispatcher))
  :init
  (defun orderless--strict-*-initialism (component &optional anchored)
    (orderless--separated-by
     '(seq (zero-or-more alpha) word-end (zero-or-more (not alpha)))
     (cl-loop for char across component collect `(seq word-start ,char))
     (when anchored '(seq (group buffer-start) (zero-or-more (not alpha))))
     (when (eq anchored 'both)
       '(seq (zero-or-more alpha) word-end (zero-or-more (not alpha)) eol))))
  (defun orderless-strict-initialism (component)
    (orderless--strict-*-initialism component))
  (defun prot-orderless-literal-dispatcher (pattern _index _total)
    (when (string-suffix-p "=" pattern)
      `(orderless-literal . ,(substring pattern 0 -1))))
  (defun prot-orderless-strict-initialism-dispatcher (pattern _index _total)
    (when (string-suffix-p "," pattern)
      `(orderless-strict-initialism . ,(substring pattern 0 -1))))
  (defun prot-orderless-flex-dispatcher (pattern _index _total)
    (when (string-suffix-p "." pattern)
      `(orderless-flex . ,(substring pattern 0 -1)))))

(use-package corfu
  :after evil
  :hook (lsp-completion-mode . kb/corfu-setup-lsp)
  :general
  (:keymaps 'corfu-map :states 'insert
	    "C-n" #'corfu-next
	    "C-p" #'corfu-previous
	    "<escape>" #'corfu-quit
	    "<return>" #'corfu-insert
	    "H-SPC" #'corfu-insert-separator
	    "M-d" #'corfu-show-documentation
	    "C-g" #'corfu-quit
	    "M-l" #'corfu-show-location)
  :custom
  (tab-always-indent 'complete)
  (completion-cycle-threshold nil)
  (corfu-auto nil)
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0.25)
  (corfu-min-width 80)
  (corfu-max-width corfu-min-width)
  (corfu-count 14)
  (corfu-scroll-margin 4)
  (corfu-cycle nil)
  (corfu-quit-at-boundary nil)
  (corfu-separator ?\s)
  (corfu-quit-no-match 'separator)
  (corfu-preview-current 'insert)
  (corfu-preselect-first t)
  (lsp-completion-provider :none)
  :init
  (global-corfu-mode)
  :config
  (general-add-advice '(corfu--setup corfu--teardown) :after 'evil-normalize-keymaps)
  (evil-make-overriding-map corfu-map)
  (defun corfu-enable-always-in-minibuffer ()
    "Enable Corfu in minibuffer when Vertico is inactive."
    (unless (or (bound-and-true-p mct--active)
		(bound-and-true-p vertico--input))
      (setq-local corfu-auto nil)
      (corfu-mode 1)))
  (add-hook 'minibuffer-setup-hook #'corfu-enable-always-in-minibuffer 1)
  (defun kb/corfu-setup-lsp ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
	  '(orderless))))

(use-package kind-icon
  :after corfu
  :custom
  (kind-icon-use-icons t)
  (kind-icon-default-face 'corfu-default)
  (kind-icon-blend-background nil)
  (kind-icon-blend-frac 0.08)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)
  (add-hook 'kb/themes-hooks #'(lambda () (interactive) (kind-icon-reset-cache))))

(use-package embark
  :ensure t :defer t
  :bind (("C-." . embark-act)
	 ("M-." . embark-dwim)
	 ("C-h B" . embark-bindings))
  :init
  (setq prefix-help-command #'embark-prefix-help-command))

(use-package embark-consult
  :ensure t
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(use-package cape
  :bind (("C-c p p" . completion-at-point)
	 ("C-c p t" . complete-tag)
	 ("C-c p d" . cape-dabbrev)
	 ("C-c p h" . cape-history)
	 ("C-c p f" . cape-file)
	 ("C-c p k" . cape-keyword)
	 ("C-c p s" . cape-elisp-symbol)
	 ("C-c p e" . cape-elisp-block)
	 ("C-c p a" . cape-abbrev)
	 ("C-c p l" . cape-line)
	 ("C-c p w" . cape-dict)
	 ("C-c p :" . cape-emoji)
	 ("C-c p \\" . cape-tex)
	 ("C-c p _" . cape-tex)
	 ("C-c p ^" . cape-tex)
	 ("C-c p &" . cape-sgml)
	 ("C-c p r" . cape-rfc1345)))

(provide 'completion/core)
;;; core.el ends here
