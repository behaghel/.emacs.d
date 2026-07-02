;;; org-confluence-entrypoints-test.el --- Entrypoint smoke tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Smoke tests for package facade and canonical entrypoints.

;;; Code:

(require 'ert)
(require 'org-confluence)
(require 'org-confluence-commands)

(ert-deftest org-confluence-entrypoint-provides-facade-and-command-aggregate ()
  "Requiring the package facade provides aggregate command features."
  (should (featurep 'org-confluence))
  (should (featurep 'org-confluence-commands))
  (should (featurep 'org-confluence-api))
  (should (featurep 'org-confluence-export))
  (should (fboundp 'org-confluence-publish))
  (should (fboundp 'org-confluence-pull))
  (should (fboundp 'org-confluence-comments-import))
  (should (fboundp 'org-confluence-comments-push-current)))

(ert-deftest org-confluence-mode-enables-org-comments-mode ()
  "The Confluence integration mode enables generic Org comments by default."
  (with-temp-buffer
    (org-mode)
    (let ((org-confluence-enable-org-comments-mode t))
      (org-confluence-mode 1)
      (should org-confluence-mode)
      (should org-comments-mode)
      (should org-confluence--enabled-org-comments-mode)
      (org-confluence-mode -1)
      (should-not org-confluence-mode)
      (should-not org-comments-mode))))

(ert-deftest org-confluence-mode-does-not-disable-preexisting-org-comments-mode ()
  "Disabling Confluence mode preserves independently enabled Org comments mode."
  (with-temp-buffer
    (org-mode)
    (org-comments-mode 1)
    (let ((org-confluence-enable-org-comments-mode t))
      (org-confluence-mode 1)
      (should org-comments-mode)
      (should-not org-confluence--enabled-org-comments-mode)
      (org-confluence-mode -1)
      (should org-comments-mode))))

(ert-deftest org-confluence-mode-map-binds-dispatch-by-default ()
  "Confluence mode binds one package-native dispatch command by default."
  (let ((org-confluence-keymap-prefix "C-c C-x C"))
    (org-confluence-rebuild-mode-map)
    (should (eq (lookup-key org-confluence-mode-map (kbd "C-c C-x C"))
		#'org-confluence-dispatch))))

(ert-deftest org-confluence-mode-map-can-disable-default-binding ()
  "A nil keymap prefix leaves Confluence mode without a default binding."
  (let ((org-confluence-keymap-prefix nil))
    (org-confluence-rebuild-mode-map)
    (should-not (eq (lookup-key org-confluence-mode-map (kbd "C-c C-x C"))
		    #'org-confluence-dispatch)))
  (let ((org-confluence-keymap-prefix "C-c C-x C"))
    (org-confluence-rebuild-mode-map)))

(ert-deftest org-confluence-mode-installs-account-id-resolver-non-destructively ()
  "Confluence mode installs the generic resolver only when no resolver exists."
  (with-temp-buffer
    (org-mode)
    (setq-local org-comments-resolve-account-id-function nil)
    (org-confluence-mode 1)
    (should (eq org-comments-resolve-account-id-function
		#'org-confluence-people-resolve-account-id))
    (should org-confluence--installed-account-id-resolver)
    (org-confluence-mode -1)
    (should-not org-comments-resolve-account-id-function))
  (with-temp-buffer
    (org-mode)
    (let ((resolver #'ignore))
      (setq-local org-comments-resolve-account-id-function resolver)
      (org-confluence-mode 1)
      (should (eq org-comments-resolve-account-id-function resolver))
      (should-not org-confluence--installed-account-id-resolver)
      (org-confluence-mode -1)
      (should (eq org-comments-resolve-account-id-function resolver)))))

(ert-deftest org-confluence-buffer-p-detects-linked-org-buffer ()
  "Confluence buffers are Org buffers with page metadata."
  (with-temp-buffer
    (org-mode)
    (insert "#+CONFLUENCE_PAGE_ID: 123\n\nBody\n")
    (should (org-confluence-buffer-p)))
  (with-temp-buffer
    (org-mode)
    (insert "#+TITLE: Plain\n\nBody\n")
    (should-not (org-confluence-buffer-p))))

(ert-deftest org-confluence-mode-shows-empty-page-notice ()
  "Confluence mode marks linked buffers whose page body is empty."
  (with-temp-buffer
    (org-mode)
    (insert "#+TITLE: Folder\n#+CONFLUENCE_PAGE_ID: 123\n\n")
    (org-confluence-mode 1)
    (should (overlayp org-confluence--empty-page-overlay))
    (should (string-match-p "no body content"
			    (overlay-get org-confluence--empty-page-overlay 'after-string)))
    (goto-char (point-max))
    (insert "Body\n")
    (org-confluence-refresh-empty-page-notice)
    (should-not (overlayp org-confluence--empty-page-overlay))
    (org-confluence-mode -1)))

(ert-deftest org-confluence-mode-does-not-mark-unlinked-empty-buffer ()
  "The empty-page notice is Confluence-specific."
  (with-temp-buffer
    (org-mode)
    (insert "#+TITLE: Plain\n\n")
    (org-confluence-refresh-empty-page-notice)
    (should-not (overlayp org-confluence--empty-page-overlay))))

(ert-deftest org-confluence-mode-maybe-enables-only-linked-buffers ()
  "Maybe-enable helper enables Confluence mode only for linked Org buffers."
  (with-temp-buffer
    (org-mode)
    (insert "#+CONFLUENCE_PAGE_ID: 123\n\nBody\n")
    (org-confluence-mode-maybe)
    (should org-confluence-mode))
  (with-temp-buffer
    (org-mode)
    (insert "#+TITLE: Plain\n\nBody\n")
    (org-confluence-mode-maybe)
    (should-not org-confluence-mode)))

(ert-deftest org-confluence-sync-status-marker-is-idempotent ()
  "Source sync marker refresh removes persisted duplicates and keeps one overlay."
  (with-temp-buffer
    (org-mode)
    (setq buffer-file-name "/tmp/article.org")
    (insert "#+TITLE: Article\n#+CONFLUENCE_PAGE_ID: 123\n[Sync ? unchecked]\n[Sync ? unchecked]\n\nBody")
    (cl-letf (((symbol-function 'org-confluence-sync-status-marker-string)
	       (lambda (&rest _args) "[Sync ? unchecked]")))
      (org-confluence-sync-status-refresh-source-marker)
      (org-confluence-sync-status-refresh-source-marker)
      (goto-char (point-min))
      (should-not (search-forward "[Sync ? unchecked]" nil t))
      (should (overlayp org-confluence-sync-status--source-marker-overlay))
      (should (equal (substring-no-properties
		      (overlay-get org-confluence-sync-status--source-marker-overlay 'after-string))
		     "[Sync ? unchecked]\n")))))

(ert-deftest org-confluence-mode-removes-sync-status-marker-on-disable ()
  "Disabling Confluence mode removes the source sync marker overlay."
  (with-temp-buffer
    (org-mode)
    (setq buffer-file-name "/tmp/article.org")
    (insert "#+CONFLUENCE_PAGE_ID: 123\n\nBody")
    (cl-letf (((symbol-function 'org-confluence-sync-status-marker-string)
	       (lambda (&rest _args) "[Sync ? unchecked]")))
      (org-confluence-mode 1)
      (should (overlayp org-confluence-sync-status--source-marker-overlay))
      (org-confluence-mode -1)
      (should-not (overlayp org-confluence-sync-status--source-marker-overlay)))))

(ert-deftest org-confluence-drops-short-compatibility-wrapper-files ()
  "Short compatibility wrapper files are not part of the package boundary."
  (dolist (file '("api.el" "commands.el" "export.el"))
    (should-not (file-exists-p (expand-file-name file "packages/org-confluence")))))

(provide 'org-confluence-entrypoints-test)
;;; org-confluence-entrypoints-test.el ends here
