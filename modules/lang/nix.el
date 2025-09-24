;;; nix.el --- Nix language setup -*- lexical-binding: t; -*-

;;; Code:

(use-package nix-mode :mode "\\.nix\\'"
  :config
  ;; Smartparens pairing convenience in Nix
  (with-eval-after-load 'smartparens
    (sp-local-pair 'nix-mode "{" nil :post-handlers '(("||\n[i]" "RET")))))

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs '(nix-mode . ("nil"))))

(add-hook 'nix-mode-hook #'eglot-ensure)

(provide 'lang/nix)
;;; nix.el ends here
