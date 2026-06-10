;;; help.el --- Development help and code context UI -*- lexical-binding: t; -*-

;;; Commentary:
;; Lightweight programming-buffer UI helpers such as which-function and Info
;; bindings.

;;; Code:

(require 'hub-utils)

;; Enable which-function only in programming buffers to avoid mode-line hook
;; errors in special buffers (e.g., Treemacs, magit status, etc.).
(setq which-func-modes '(prog-mode))
(add-hook 'prog-mode-hook (lambda () (unless which-function-mode (which-function-mode 1))))
(define-key evil-normal-state-map (kbd ",hI") #'info)

(provide 'dev/help)
;;; help.el ends here
