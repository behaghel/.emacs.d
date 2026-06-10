;;; lsp.el --- LSP client defaults -*- lexical-binding: t; -*-

;;; Commentary:
;; Eglot basics.  Language-specific servers and hooks are configured per
;; language under modules/lang/*.

;;; Code:

(use-package eglot
  :defer t
  :commands (eglot eglot-ensure))

(provide 'dev/lsp)
;;; lsp.el ends here
