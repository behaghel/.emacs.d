;;; keybinds.el --- Central keybinding definitions -*- lexical-binding: t; -*-

;;; Commentary:
;; Evil is foundational here; global leader map lives here.
;; Mode-specific bindings should be placed in their respective modules.

;;; Code:

(use-package evil
  :demand t)

(use-package general
  :demand t)

(general-create-definer hub/leader
			:states '(normal visual motion emacs)
			:keymaps 'override
			:prefix ","
			:non-normal-prefix "C-,")

(provide 'core-keybinds)
;;; keybinds.el ends here
