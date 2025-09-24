;;; misc.el --- Misc language modes and associations -*- lexical-binding: t; -*-

;;; Code:

(require 'hub-utils)
(use-package gnuplot :commands (gnuplot-mode gnuplot-make-buffer))

;; Zsh files as shell-script-mode
(add-to-list 'auto-mode-alist '("\\.zsh$" . shell-script-mode))

;; macOS Launchd plist helper
(define-auto-insert "\\.plist\\'" ["template.plist" hub/autoinsert-yas-expand])
(add-to-list 'auto-mode-alist '("\\.plist$" . xml-mode))

(provide 'lang/misc)
;;; misc.el ends here
