;;; org-context-panel.el --- Compatibility shim for context-panels -*- lexical-binding: t; -*-

;; Author: Hubert Behaghel
;; Maintainer: Hubert Behaghel
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (org "9.6"))
;; Keywords: outlines, tools, convenience
;; URL: https://github.com/behaghel/org-comments

;;; Commentary:
;; Backward-compatible `org-context-panel-*' API over the extracted
;; `context-panels' package.

;;; Code:

(require 'context-panels)
(require 'context-panels-org)

(defface org-context-panel-marker-face
  '((t :inherit context-panels-marker-face))
  "Compatibility face for Org context top markers."
  :group 'context-panels-org)

(put 'org-context-panel-marker-face 'face-alias 'context-panels-marker-face)

(defun org-context-panel--compat-symbol (symbol)
  "Return old `org-context-panel' compatibility SYMBOL for context-panels SYMBOL."
  (intern (replace-regexp-in-string
	   "\\`context-panels" "org-context-panel" (symbol-name symbol))))

(defun org-context-panel--install-compat-aliases ()
  "Install aliases from `context-panels-*' to `org-context-panel-*'."
  (mapatoms
   (lambda (symbol)
     (when (string-prefix-p "context-panels" (symbol-name symbol))
       (let ((old (org-context-panel--compat-symbol symbol)))
	 (when (boundp symbol)
	   (defvaralias old symbol))
	 (when (and (fboundp symbol)
		    (not (memq old '(org-context-panel-mode
				     org-context-panel-make-top-marker
				     org-context-panel-marker-position
				     org-context-panel-marker-at-point-p
				     org-context-panel-metadata-end-position))))
	   (defalias old symbol)))))))

(org-context-panel--install-compat-aliases)

(defalias 'org-context-panel-metadata-end-position
  #'context-panels-org-metadata-end-position)
(defalias 'org-context-panel-make-top-marker
  #'context-panels-org-make-top-marker)
(defalias 'org-context-panel-marker-position
  #'context-panels-org-marker-position)
(defalias 'org-context-panel-marker-at-point-p
  #'context-panels-org-marker-at-point-p)

;;;###autoload
(defun org-context-panel-mode (&optional arg)
  "Compatibility wrapper around `context-panels-mode' with Org defaults.
ARG is passed to `context-panels-mode'."
  (interactive (list (or current-prefix-arg 'toggle)))
  (context-panels-org-install-defaults)
  (context-panels-mode arg))

(provide 'org-context-panel)
;;; org-context-panel.el ends here
