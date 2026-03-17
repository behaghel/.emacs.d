;;; js-config.el --- JavaScript/TypeScript extras -*- lexical-binding: t; -*-

;;; Code:

(require 'js)
(require 'cl-lib)

(use-package typescript-ts-mode
  :straight (:type built-in)
  :mode (("\\.ts\\'" . typescript-ts-mode)
	 ("\\.tsx\\'" . tsx-ts-mode))
  :config
  ;; Compatibility aliases used elsewhere in this config.
  (define-derived-mode typescriptreact-mode tsx-ts-mode "TypeScript TSX")
  (define-derived-mode typescriptreact-ts-mode tsx-ts-mode "TypeScript TSX")
  (when (boundp 'tree-sitter-major-mode-language-alist)
    (add-to-list 'tree-sitter-major-mode-language-alist '(typescriptreact-mode . tsx))
    (add-to-list 'tree-sitter-major-mode-language-alist '(typescriptreact-ts-mode . tsx))))

(use-package tsi
  :after tree-sitter
  :straight (:type git :host github :repo "orzechowskid/tsi.el")
  :commands (tsi-typescript-mode tsi-json-mode tsi-css-mode)
  :init
  (add-hook 'typescript-ts-mode-hook (lambda () (tsi-typescript-mode 1)))
  (add-hook 'tsx-ts-mode-hook (lambda () (tsi-typescript-mode 1)))
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
    (if (file-exists-p ".nvmrc")
	(nvm-use-for ".")
      (nvm-use (caar (last (nvm--installed-versions)))))
    (setq mjs/previous-node-version (getenv "NVM_BIN")
	  exec-path (cl-pushnew mjs/previous-node-version exec-path :test #'string=)))
  (with-eval-after-load 'projectile
    (add-hook 'projectile-after-switch-project-hook #'mjs/choose-node-version)))

(provide 'lang/js)
;;; js-config.el ends here
