;;; formatting.el --- Development formatting helpers -*- lexical-binding: t; -*-

;;; Commentary:
;; Formatter integration and programming annotation highlighting.

;;; Code:

(require 'hub-utils)

(use-package apheleia
  :commands (apheleia-mode apheleia-format-buffer)
  :hook (prog-mode . apheleia-mode))

(add-hook 'prog-mode-hook #'hub/font-lock-comment-annotations)

(provide 'dev/formatting)
;;; formatting.el ends here
