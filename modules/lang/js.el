;;; js.el --- JavaScript/TypeScript setup -*- lexical-binding: t; -*-

;;; Code:

(use-package typescript-mode
  :after tree-sitter
  :config
  (require 'typescript-ts-mode)
  (define-derived-mode typescriptreact-mode typescript-mode "TypeScript TSX")
  (define-derived-mode typescriptreact-ts-mode typescript-ts-mode "TypeScript TSX")
  (add-to-list 'auto-mode-alist '("\\.tsx?\\'" . typescriptreact-mode))
  (add-to-list 'tree-sitter-major-mode-language-alist '(typescriptreact-mode . tsx)))

(use-package tsi
  :after tree-sitter
  :straight (:type git :host github :repo "orzechowskid/tsi.el")
  :commands (tsi-typescript-mode tsi-json-mode tsi-css-mode)
  :init
  (add-hook 'typescript-mode-hook (lambda () (tsi-typescript-mode 1)))
  (add-hook 'json-mode-hook (lambda () (tsi-json-mode 1)))
  (add-hook 'css-mode-hook (lambda () (tsi-css-mode 1)))
  (add-hook 'scss-mode-hook (lambda () (tsi-scss-mode 1))))

(when (executable-find "eslint_d")
  (setq flycheck-javascript-eslint-executable "eslint_d"))

(use-package nvm
  :commands (nvm-use nvm-use-for nvm--installed-versions)
  :config
  (defvar mjs/previous-node-version nil)
  (defun mjs/choose-node-version ()
    (when mjs/previous-node-version
      (setq exec-path (cl-remove mjs/previous-node-version exec-path :test #'string=)
	    mjs/previous-node-version nil))
    (if (file-exists-p ".nvmrc") (nvm-use-for ".") (nvm-use (caar (last (nvm--installed-versions)))))
    (setq mjs/previous-node-version (getenv "NVM_BIN")
	  exec-path (cl-pushnew mjs/previous-node-version exec-path :test #'string=)))
  (with-eval-after-load 'projectile
    (add-hook 'projectile-after-switch-project-hook 'mjs/choose-node-version)))

(provide 'lang/js)
;;; js.el ends here
