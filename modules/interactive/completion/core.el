;;; core.el --- Completion: Vertico/Orderless/Corfu -*- lexical-binding: t; -*-

;;; Commentary:
;; Extracted from legacy setup-completion.el. Provides minibuffer and
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
  :disabled t
  :demand t
  :config
  ;; Disabled temporarily after repeated `text-read-only' errors in
  ;; `vertico--prompt-selection' during ordinary `find-file' prompts.
  ;; Keep native Emacs completion until the Vertico/minibuffer interaction is
  ;; isolated in a minimal reproduction.
  (vertico-mode -1))

(use-package savehist :init (savehist-mode))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((eglot (styles orderless))))
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
  :commands (kind-icon-margin-formatter kind-icon-reset-cache)
  :init
  (setq kind-icon-use-icons t
	kind-icon-default-face 'corfu-default
	kind-icon-blend-background nil
	kind-icon-blend-frac 0.08)
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)
  (with-eval-after-load 'kind-icon
    (add-hook 'kb/themes-hooks #'kind-icon-reset-cache)))

(use-package embark
  :defer t
  :bind (("C-." . embark-act)
	 ("M-." . embark-dwim)
	 ("C-h B" . embark-bindings))
  :init
  (setq prefix-help-command #'embark-prefix-help-command))

(use-package embark-consult
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
