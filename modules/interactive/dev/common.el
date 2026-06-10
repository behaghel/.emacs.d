;;; common.el --- Common programming settings entrypoint -*- lexical-binding: t; -*-

;;; Commentary:
;; Compatibility entrypoint for shared development configuration.  Individual
;; concerns live in focused dev/* modules.

;;; Code:

(require 'dev/diff)
(require 'dev/search)
(require 'dev/indent)
(require 'dev/formatting)
(require 'dev/folding)
(require 'dev/comint)
(require 'dev/prog-mode)
(require 'dev/direnv)
(require 'dev/help)
(require 'dev/whitespace)
(require 'dev/treesit)
(require 'dev/elisp)
(require 'dev/lsp)
(require 'dev/misc)

(provide 'dev/common)
;;; common.el ends here
