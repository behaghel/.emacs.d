;;; org-confluence-comments.el --- Confluence comment implementation facade -*- lexical-binding: t; -*-

;;; Commentary:
;; Package-native Confluence comment operation boundary.
;;
;; This facade gives backend adapters and commands a package-native place to
;; depend on focused Confluence comment modules.

;;; Code:

(require 'org-confluence-comments-context)
(require 'org-confluence-comments-diagnostics)
(require 'org-confluence-comments-import)
(require 'org-confluence-comments-open)
(require 'org-confluence-comments-push)
(require 'org-confluence-comments-sync)

(provide 'org-confluence-comments)
;;; org-confluence-comments.el ends here
