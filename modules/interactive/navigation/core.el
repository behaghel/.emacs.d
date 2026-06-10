;;; core.el --- Interactive navigation entrypoint -*- lexical-binding: t; -*-

;;; Commentary:
;; Coarse entrypoint for project, perspective, tree, and file navigation.

;;; Code:

(require 'navigation/project)
(require 'navigation/treemacs)
(require 'navigation/perspective)
(require 'navigation/perspective-auto)
(require 'navigation/dired)

(provide 'navigation/core)
;;; core.el ends here
