;; Haskell
;; TODO: next time you get back to haskell, try
;; https://github.com/emacs-lsp/lsp-haskell
;; unicode input doesn't work with eg >= (Not in scope: ≥): disabled
;; (add-hook 'haskell-mode-hook 'turn-on-haskell-unicode-input-method)
(use-package ghc
  :commands (ghc-init ghc-debug)
  :config
  (let ((my-cabal-path (expand-file-name "~/.cabal/bin")))
    (setenv "PATH" (concat my-cabal-path path-separator (getenv "PATH")))
    (add-to-list 'exec-path my-cabal-path))
  (use-package company-ghc
    :config
    (setq company-ghc-show-info t))
  )

(use-package haskell-mode
  :mode "\\.hs\\'"
  :commands haskell-mode
  :config
  (setq haskell-process-args-stack-ghci '("--ghci-options=-ferror-spans"))
  (setq haskell-process-auto-import-loaded-modules t)
  (setq haskell-process-log t)
  (setq haskell-process-suggest-remove-import-lines t)
  ;; (setq haskell-process-type (quote cabal-repl))
  (setq haskell-process-type (quote auto))
  (setq haskell-tags-on-save t)
  ;; (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
  (add-hook 'haskell-mode-hook 'haskell-auto-insert-module-template)
  (require 'haskell-interactive-mode)
  (require 'haskell-process)
  (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
  (add-hook 'haskell-mode-hook 'haskell-indent-mode)
  (add-hook 'haskell-mode-hook 'haskell-doc-mode)
  ;; (add-hook 'flycheck-mode-hook 'flycheck-haskell-setup)

  ;; (use-package flycheck-haskell)
  ;; ghc-init will expect tool ghc-mod to be installed. To have it,
  ;; the process I follow to create a project is:
  ;; - stack new <project-name>
  ;; - cd project-name
  ;; - stack setup
  ;; - stack --resolver=nightly install ghc-mod --no-copy-bins
  (add-hook 'haskell-mode-hook (lambda () (ghc-init)))
  (add-hook 'haskell-mode-hook 'intero-mode)
  (use-package hindent
    :config
    ;; first time: stack install hindent
    (add-hook 'haskell-mode-hook #'hindent-mode)
    (define-key haskell-mode-map (kbd "M-q") 'hindent-reformat-decl-or-fill)
    (evil-define-key 'normal haskell-mode-map ",ff" 'hindent-reformat-decl)
    (evil-define-key 'normal haskell-mode-map ",fF" 'hindent-reformat-buffer)
    (evil-define-key 'visual haskell-mode-map ",f" 'hindent-reformat-region))
  ;; (defun hub/haskell-config ()
  ;;   "Set up my emacs-lisp hacking environment."
  ;;   ;; (hub/set-newline-and-indent-comment)
  ;;   ;; (rainbow-delimiters-mode t)
  ;;   ;; (electric-indent-local-mode)          ; deactivate: weird indent toggle
  ;;   (ghc-init))
  ;; (add-hook 'haskell-mode-hook 'hub/haskell-config)
  (evil-define-key 'normal haskell-cabal-mode-map ",bb" 'haskell-compile)
  (evil-define-key 'normal haskell-mode-map ",bb" 'haskell-compile)
  (evil-define-key 'normal haskell-mode-map ",." 'haskell-mode-jump-to-def-or-tag)
  (evil-define-key 'normal haskell-mode-map ",el" 'haskell-process-load-or-reload)
  (evil-define-key 'normal haskell-mode-map "C-c C-l" 'haskell-process-load-or-reload)
  (evil-define-key 'normal haskell-mode-map ",gh" 'switch-to-haskell)
  (evil-define-key 'normal haskell-mode-map ",gi" 'haskell-navigate-imports)
  (evil-define-key 'normal haskell-mode-map ",gr" 'haskell-interactive-bring)
  (evil-define-key 'normal haskell-mode-map ",hi" 'haskell-process-do-info)
  (evil-define-key 'normal haskell-mode-map "C-c C-i" 'haskell-process-do-info)
  (define-key haskell-interactive-mode-map (kbd "C-c C-i") 'haskell-process-do-info)
  (evil-define-key 'normal haskell-mode-map ",ht" 'haskell-process-do-type)
  (evil-define-key 'normal haskell-mode-map "C-c C-t" 'haskell-process-do-type)
  (define-key haskell-interactive-mode-map (kbd "C-c C-t") 'haskell-process-do-type)
  (evil-define-key 'normal haskell-mode-map ",hh" 'inferior-haskell-find-haddock)
  (define-key haskell-mode-map (kbd "M-<left>") 'haskell-move-nested-left)
  (define-key haskell-mode-map (kbd "M-<right>") 'haskell-move-nested-right)
  (define-key haskell-mode-map (kbd "C-c C-c") 'haskell-compile)
  (define-key haskell-mode-map (kbd  "C-c |") 'haskell-indent-insert-guard)
  (add-hook 'align-load-hook (lambda ()
                               (add-to-list 'align-rules-list
                                            '(haskell-types
                                              (regexp . "\\(\\s-+\\)\\(::\\|∷\\)\\s-+")
                                              (modes quote (haskell-mode literate-haskell-mode))))
                               (add-to-list 'align-rules-list
                                            '(haskell-assignment
                                              (regexp . "\\(\\s-+\\)=\\s-+")
                                              (modes quote (haskell-mode literate-haskell-mode))))
                               (add-to-list 'align-rules-list
                                            '(haskell-arrows
                                              (regexp . "\\(\\s-+\\)\\(->\\|→\\)\\s-+")
                                              (modes quote (haskell-mode literate-haskell-mode))))
                               (add-to-list 'align-rules-list
                                            '(haskell-left-arrows
                                              (regexp . "\\(\\s-+\\)\\(<-\\|←\\)\\s-+")
                                              (modes quote (haskell-mode literate-haskell-mode)))))))

(provide 'setup-haskell)