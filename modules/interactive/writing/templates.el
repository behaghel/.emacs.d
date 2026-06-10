;;; templates.el --- Writing file templates -*- lexical-binding: t; -*-

;;; Commentary:
;; Auto-insert templates for writing-oriented files.  Template expansion uses
;; YASnippet when invoked.

;;; Code:

(require 'autoinsert)
(require 'hub-utils)

(auto-insert-mode 0)
(setq auto-insert-directory (expand-file-name "insert/" user-emacs-directory))

(defun hub/autoinsert-yas-expand (&optional expand-env)
  "Replace text in yasnippet template optionally passing EXPAND-ENV."
  (yas-expand-snippet (buffer-string) (point-min) (point-max) expand-env))

(define-auto-insert "\\.org\\'" ["template.org" hub/autoinsert-yas-expand])
(define-auto-insert "\\.veriff\\.org\\'" ["template.veriff.org" hub/autoinsert-yas-expand])
(define-auto-insert "\\(?:nomina\\|.*-nomina\\).*\\.tex\\'" ["template.nomina.tex" hub/autoinsert-yas-expand])
(define-auto-insert "\\.orj\\'" ["template.orj" hub/autoinsert-yas-expand])

(provide 'writing/templates)
;;; templates.el ends here
