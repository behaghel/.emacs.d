;;; org-google-docs-demo.el --- Offline Google Docs pipeline walkthrough -*- lexical-binding: t; -*-

;;; Commentary:
;; Inspect real conversion and request planning without Google API calls.  The
;; standalone launcher supplies a source-only semantic gdocs dependency closure.

;;; Code:

(require 'org-google-docs)
(require 'gdocs-convert)
(require 'gdocs-diff)

(defvar org-google-docs-demo--source-buffer nil
  "Source explicitly selected for this disposable local pipeline session.")

(defun org-google-docs-demo--source ()
  "Return the live demo source or report how to activate the walkthrough."
  (unless (buffer-live-p org-google-docs-demo--source-buffer)
    (user-error "Start the standalone Google Docs demo first"))
  org-google-docs-demo--source-buffer)

(defun org-google-docs-demo-preview ()
  "Run local preflight and inspect the real IR/request pipeline without fetching."
  (interactive)
  (with-current-buffer (org-google-docs-demo--source)
    (org-google-docs--preflight-footnotes-for-push)
    (org-google-docs--preflight-images-for-push)
    (org-google-docs-debug-pipeline nil)))

(defun org-google-docs-demo-roundtrip ()
  "Render actual local Org-to-IR-to-Org output in a separate read-only buffer."
  (interactive)
  (let* ((text (with-current-buffer (org-google-docs-demo--source)
		 (gdocs-convert-ir-to-org (gdocs-convert-org-buffer-to-ir))))
	 (buffer (get-buffer-create "*Org Google Docs Local Round Trip*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
	(erase-buffer)
	(insert text)
	(org-mode)
	(visual-line-mode 1)
	(goto-char (point-min)))
      (setq buffer-read-only t))
    (pop-to-buffer buffer)
    buffer))

(defun org-google-docs-demo-start ()
  "Activate the local-only pipeline walkthrough for the disposable Org source."
  (interactive)
  (unless (and (derived-mode-p 'org-mode) buffer-file-name)
    (user-error "Visit the disposable Org source before starting the demo"))
  (org-google-docs-check-gdocs-loaded)
  (setq org-google-docs-demo--source-buffer (current-buffer)
	gdocs-auto-push-on-save nil
	gdocs-auto-pull-on-open nil
	org-export-use-babel nil)
  (org-google-docs-mode 1)
  (message "Offline Google Docs: preview and local round-trip commands are ready"))

(provide 'org-google-docs-demo)
;;; org-google-docs-demo.el ends here
