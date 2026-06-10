;;; edit-server.el --- Browser textarea editing app -*- lexical-binding: t; -*-

;;; Commentary:
;; Optional edit-server integration for editing browser textareas in Emacs.

;;; Code:

(use-package edit-server
  :if window-system
  :commands (edit-server-start edit-server-stop)
  :init
  (setq edit-server-default-major-mode 'markdown-mode))

(provide 'apps/edit-server)
;;; edit-server.el ends here
