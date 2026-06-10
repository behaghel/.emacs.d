;;; project.el --- Project navigation configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Built-in project.el configuration for project-as-focus navigation workflows.

;;; Code:

(use-package project
  :straight (:type built-in)
  :init
  (setq project-list-file (expand-file-name "var/project-list.el" user-emacs-directory)))

(provide 'navigation/project)
;;; project.el ends here
