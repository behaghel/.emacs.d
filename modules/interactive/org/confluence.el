;;; confluence.el --- Org: Confluence export integration -*- lexical-binding: t; -*-

;;; Commentary:
;; Activation and user configuration for the local Org -> Confluence exporter.
;;
;; Keep this module thin: the exporter, CLI wrappers, and commands under
;; packages/org-confluence/ is a local package boundary intended to stay
;; self-contained enough to become a standalone package/repository later.

;;; Code:

(require 'hub-utils)

(defgroup hub/confluence nil
  "Personal Confluence integration activation."
  :group 'org)

(defcustom hub/confluence-export-directory
  (expand-file-name "packages/org-confluence" user-emacs-directory)
  "Directory containing the Org Confluence exporter package files."
  :type 'directory
  :group 'hub/confluence)

(defun hub/confluence--export-file (name)
  "Return absolute path for Confluence exporter file NAME."
  (expand-file-name name hub/confluence-export-directory))

(add-to-list 'load-path hub/confluence-export-directory)

(autoload 'hub/confluence-publish "org-confluence-commands" nil t)
(autoload 'hub/confluence-publish-dwim "org-confluence-commands" nil t)
(autoload 'hub/confluence-publish-from-export-dispatch "org-confluence-commands" nil t)
(autoload 'hub/confluence-publish-and-open-from-export-dispatch
  "org-confluence-commands" nil t)
(autoload 'hub/confluence-open-page "org-confluence-commands" nil t)
(autoload 'hub/confluence-pull "org-confluence-commands" nil t)
(autoload 'hub/confluence-comment-list "org-confluence-commands" nil t)
(autoload 'hub/confluence-comment-import "org-confluence-commands" nil t)
(autoload 'hub/confluence-comment-import-footer "org-confluence-commands" nil t)
(autoload 'hub/confluence-comment-import-inline "org-confluence-commands" nil t)
(autoload 'hub/confluence-people-resolve "org-confluence-commands" nil t)

;; Personal Confluence integration defaults.  These are normal Org workflow
;; configuration, so they live in the tracked Org config layer rather than in
;; the reusable exporter package or private/setup.el.  Move only secrets or
;; genuinely uncommittable values to private/setup.el.
(setq hub/confluence-api-default-space "~63cfa80595cff7f585c2f168"
      hub/confluence-api-base-url "https://veriff.atlassian.net")

(with-eval-after-load 'ox
  (load (hub/confluence--export-file "org-confluence-export.el") nil 'nomessage))

(provide 'org/confluence)
;;; confluence.el ends here
