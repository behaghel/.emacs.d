;;; comint.el --- Comint and compilation configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Shared comint and compilation behavior for development workflows.

;;; Code:

(require 'ansi-color)

(setq comint-scroll-to-bottom-on-input t
      comint-scroll-to-bottom-on-output t
      comint-show-maximum-output t
      comint-input-ignoredups t
      comint-completion-addsuffix t
      comint-buffer-maximum-size 10000)
(add-hook 'comint-output-filter-functions #'comint-truncate-buffer)
(evil-define-key 'normal comint-mode-map ",ee" #'comint-clear-buffer)
(evil-define-key 'insert comint-mode-map (kbd "C-c C-e") #'comint-clear-buffer)

(defun colorize-compilation-buffer ()
  "Accept coloured output from testing."
  (ansi-color-apply-on-region compilation-filter-start (point)))

(add-hook 'compilation-filter-hook #'colorize-compilation-buffer)
(setq compilation-scroll-output 'first-error
      compilation-always-kill t
      compilation-auto-jump-to-first-error t
      compilation-context-lines 5)

(provide 'dev/comint)
;;; comint.el ends here
