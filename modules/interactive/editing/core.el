;;; core.el --- Interactive editing entrypoint -*- lexical-binding: t; -*-

;;; Commentary:
;; Coarse entrypoint for the interactive editing foundation: general defaults,
;; Evil behavior, keybinding infrastructure, and shared keymaps.

;;; Code:

(use-package hydra)
(use-package general)

(require 'editing/general)
(require 'editing/evil)
(require 'editing/keys)

(provide 'editing/core)
;;; core.el ends here
