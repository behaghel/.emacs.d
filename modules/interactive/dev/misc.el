;;; misc.el --- Miscellaneous development defaults -*- lexical-binding: t; -*-

;;; Commentary:
;; Small programming defaults that do not yet justify a dedicated module.

;;; Code:

(add-hook 'awk-mode-hook (lambda () (setq c-basic-offset 2)))

(provide 'dev/misc)
;;; misc.el ends here
