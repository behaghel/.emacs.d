;;; confluence.el --- Org: Confluence export integration -*- lexical-binding: t; -*-

;;; Commentary:
;; Activation and user configuration for the local Org -> Confluence exporter.
;;
;; Keep this module thin: the exporter, CLI wrappers, and commands under
;; packages/org-confluence/ is a local package boundary intended to stay
;; self-contained enough to become a standalone package/repository later.

;;; Code:

(require 'hub-keys)
(require 'hub-utils)
(require 'subr-x)

(defgroup hub/org-confluence nil
  "Personal Confluence integration activation."
  :group 'org)

(defcustom hub/org-confluence-export-directory
  (expand-file-name "packages/org-confluence" user-emacs-directory)
  "Directory containing the Org Confluence exporter package files."
  :type 'directory
  :group 'hub/org-confluence)

(defun hub/org-confluence--export-file (name)
  "Return absolute path for Confluence exporter file NAME."
  (expand-file-name name hub/org-confluence-export-directory))

(add-to-list 'load-path hub/org-confluence-export-directory)

(use-package magit-section
  :demand t)

(use-package transient
  :demand t)

(autoload 'org-confluence-publish "org-confluence-publish" nil t)
(autoload 'org-confluence-publish-dwim "org-confluence-publish" nil t)
(autoload 'org-confluence-publish-from-export-dispatch "org-confluence-publish" nil t)
(autoload 'org-confluence-publish-and-open-from-export-dispatch
  "org-confluence-commands" nil t)
(autoload 'org-confluence-open-page "org-confluence-publish" nil t)
(autoload 'org-confluence-pull "org-confluence-import" nil t)
(autoload 'org-confluence-sync-page-current "org-confluence-sync" nil t)
(autoload 'org-confluence-sync-current "org-confluence-sync" nil t)
(autoload 'org-confluence-pull-child-page "org-confluence-sync" nil t)
(autoload 'org-confluence-sync-status "org-confluence-sync-status" nil t)
(autoload 'org-confluence-comments-list "org-confluence-comments-diagnostics" nil t)
(autoload 'org-confluence-comments-import "org-confluence-comments-import" nil t)
(autoload 'org-confluence-comments-import-footer "org-confluence-comments-import" nil t)
(autoload 'org-confluence-comments-import-inline "org-confluence-comments-import" nil t)
(autoload 'org-confluence-comments-open-current "org-confluence-comments-open" nil t)
(autoload 'org-confluence-comments-push-current "org-confluence-comments-push" nil t)
(autoload 'org-confluence-people-mark-current-user "org-confluence-people" nil t)
(autoload 'org-confluence-people-resolve "org-confluence-people" nil t)

(with-eval-after-load 'general
  (hub/define-leaders)
  (hub/leader
   "c D" #'org-confluence-pull-child-page
   "c g" #'org-confluence-sync-status))

(defun hub/org-confluence-comment--enable-sidecar-submit-key ()
  "Bind `C-c C-c' to push comments in comments sidecar buffers."
  (when (and buffer-file-name
	     (string-suffix-p ".comments.org" buffer-file-name))
    (use-local-map (copy-keymap (current-local-map)))
    (local-set-key (kbd "C-c C-c") #'org-confluence-comments-push-current)))

(add-hook 'org-mode-hook #'hub/org-confluence-comment--enable-sidecar-submit-key)

;; Personal Confluence integration defaults.  These are normal Org workflow
;; configuration, so they live in the tracked Org config layer rather than in
;; the reusable exporter package or private/setup.el.  Move only secrets or
;; genuinely uncommittable values to private/setup.el.
(setq org-confluence-api-default-space "~63cfa80595cff7f585c2f168"
      org-confluence-api-base-url "https://veriff.atlassian.net"
      org-confluence-people-store-directory org-directory)

(with-eval-after-load 'ox
  (load (hub/org-confluence--export-file "org-confluence-export.el") nil 'nomessage))

(provide 'org/confluence)
;;; confluence.el ends here
