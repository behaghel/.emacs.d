;;; whitespace.el --- Development whitespace configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Whitespace visualization and cleanup defaults for editing buffers.

;;; Code:

(require 'hub-utils)

(defun hub/whitespace-enable-buffer ()
  "Enable whitespace visualization in eligible editing buffers."
  (unless (derived-mode-p 'org-mode 'eve-mode)
    (whitespace-mode 1)))

(use-package whitespace
  :commands (whitespace-mode)
  :init
  (setq whitespace-style '(face tabs lines-tail empty trailing))
  (add-hook 'prog-mode-hook #'hub/whitespace-enable-buffer)
  (add-hook 'text-mode-hook #'hub/whitespace-enable-buffer))

(use-package whitespace-cleanup-mode
  :diminish whitespace-cleanup-mode
  :commands (global-whitespace-cleanup-mode whitespace-cleanup-mode)
  :init (global-whitespace-cleanup-mode))

(provide 'dev/whitespace)
;;; whitespace.el ends here
