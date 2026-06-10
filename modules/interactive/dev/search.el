;;; search.el --- Development search and tag navigation -*- lexical-binding: t; -*-

;;; Commentary:
;; Project code search and tag navigation packages.

;;; Code:

(use-package ggtags :commands ggtags-mode :diminish ggtags-mode)
(use-package ag :commands (ag ag-files ag-regexp ag-project ag-dired)
  :config (setq ag-highlight-search t ag-reuse-buffers t))

(provide 'dev/search)
;;; search.el ends here
