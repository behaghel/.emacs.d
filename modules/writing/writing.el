;;; writing.el --- Writing-focused helpers -*- lexical-binding: t; -*-

;;; Commentary:
;; Denote, focused writing, and basic quality tools. Inert unless explicitly
;; enabled by user commands or hooks.

;;; Code:

(use-package denote :defer t)
(use-package writeroom-mode :defer t)
(use-package olivetti :defer t)

;;;###autoload
(defun writing/enable-basics ()
  "Enable a minimal writing environment for the current buffer."
  (interactive)
  (when (fboundp 'variable-pitch-mode) (variable-pitch-mode 1))
  (when (fboundp 'visual-line-mode) (visual-line-mode 1))
  (when (require 'olivetti nil t) (olivetti-mode 1))
  (when (require 'writeroom-mode nil t) (writeroom-mode 1)))

(provide 'mod-writing)
;;; writing.el ends here
