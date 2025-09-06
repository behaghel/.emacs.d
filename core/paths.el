;;; paths.el --- Standardize paths and state files -*- lexical-binding: t; -*-

;;; Commentary:
;; Configure no-littering to keep configuration/state tidy under etc/ and var/.

;;; Code:

(use-package no-littering
  :demand t
  :init
  (setq no-littering-etc-directory (expand-file-name "etc/" user-emacs-directory)
        no-littering-var-directory (expand-file-name "var/" user-emacs-directory)))

(provide 'core-paths)
;;; paths.el ends here
