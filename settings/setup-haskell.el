;; Haskell
;; unicode input doesn't work with eg >= (Not in scope: ≥): disabled
;; (add-hook 'haskell-mode-hook 'turn-on-haskell-unicode-input-method)
(use-package ghc
  :commands (ghc-init ghc-debug)
  :config
  (setq company-ghc-show-info t)
  (let ((my-cabal-path (expand-file-name "~/.cabal/bin")))
    (setenv "PATH" (concat my-cabal-path path-separator (getenv "PATH")))
    (add-to-list 'exec-path my-cabal-path))
  (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
  (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
  (add-hook 'haskell-mode-hook 'haskell-indent-mode)
  (add-hook 'haskell-mode-hook 'haskell-doc-mode)
  (add-hook 'flycheck-mode-hook 'flycheck-haskell-setup)
  )

(use-package haskell-mode
  :mode "\\.hs\\'"
  :commands haskell-mode
  :config
  (setq haskell-process-args-cabal-repl (quote ("--ghc-option=-ferror-spans")))
  (setq haskell-process-auto-import-loaded-modules t)
  (setq haskell-process-log t)
  (setq haskell-process-suggest-remove-import-lines t)
  (setq haskell-process-type (quote cabal-repl))
  (setq haskell-tags-on-save t)
  (add-hook 'haskell-mode-hook (lambda () (ghc-init)))
  (add-hook 'haskell-mode-hook 'intero-mode)
  ;; (add-hook 'haskell-mode-hook #'hindent-mode)
  (defun hub/haskell-config ()
    "Set up my emacs-lisp hacking environment."
    ;; (hub/set-newline-and-indent-comment)
    ;; (rainbow-delimiters-mode t)
    ;; (electric-indent-local-mode)          ; deactivate: weird indent toggle
    (ghc-init))
  (add-hook 'haskell-mode-hook 'hub/haskell-config)
  (evil-define-key 'normal haskell-cabal-mode-map ",bb" 'haskell-compile)
  (evil-define-key 'normal haskell-mode-map ",bb" 'haskell-compile)
  (evil-define-key 'normal haskell-mode-map ",." 'find-tag)
  (evil-define-key 'normal haskell-mode-map ",l" 'inferior-haskell-load-file)
  (evil-define-key 'normal haskell-mode-map ",gr" 'switch-to-haskell)
  (evil-define-key 'normal haskell-mode-map ",ii" 'haskell-process-do-info)
  (evil-define-key 'normal haskell-mode-map ",et" 'haskell-process-do-type)
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
                                              (modes quote (haskell-mode literate-haskell-mode))))))

  (use-package haskell-cabal
    :config
    (define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-compile))
  (use-package intero))

(provide 'setup-haskell)