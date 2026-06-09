;;; org-confluence.el --- Org Confluence export package facade -*- lexical-binding: t; -*-

;;; Commentary:
;; Package-style entrypoint for the Org -> Confluence exporter.
;;
;; This file intentionally lives next to the implementation files so the whole
;; directory can later become its own Git repository/package with minimal
;; reshuffling.  Requiring `org-confluence' loads the exporter commands and the
;; Org export backend; user/account activation remains outside this package.

;;; Code:

(let ((dir (file-name-directory (or load-file-name buffer-file-name))))
  (unless (featurep 'org-confluence-commands)
    (load (expand-file-name "org-confluence-commands" dir) nil 'nomessage)))

(provide 'org-confluence)
;;; org-confluence.el ends here
