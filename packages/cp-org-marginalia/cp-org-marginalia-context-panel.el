;;; cp-org-marginalia-context-panel.el --- Context-panel provider for Org marginalia -*- lexical-binding: t; -*-

;; Author: Hubert Behaghel
;; Maintainer: Hubert Behaghel
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (org "9.6") (context-panels "0.5.0"))
;; Keywords: outlines, convenience
;; URL: https://github.com/behaghel/.emacs.d/tree/master/packages/cp-org-marginalia

;;; Commentary:
;; Collect semantic marginalia items for the standard Org context-panel renderer.
;; Context Panels owns row interaction, placement, and source-anchor navigation.

;;; Code:

(require 'context-panels-org)
(require 'cp-org-marginalia)

(defface cp-org-marginalia-context-panel-icon-face
  '((t :inherit shadow))
  "Face used for compact marginalia icons."
  :group 'cp-org-marginalia)

(defun cp-org-marginalia-context-panel--kind-icon (kind)
  "Return compact display icon for marginalia KIND."
  (pcase kind
    ('footnote "†")
    (_ "✣")))

(defun cp-org-marginalia-context-panel--presentation (_source-buffer item)
  "Describe the icon-led Org body of ITEM without rewriting its record."
  (list :icon (cp-org-marginalia-context-panel--kind-icon (plist-get item :kind))
	:icon-face 'cp-org-marginalia-context-panel-icon-face
	:label nil
	:render-body #'context-panels-org-render-item-body))

(defalias 'cp-org-marginalia-context-panel-collect-side-items
  #'cp-org-marginalia-collect)

;; Preserve callable entrypoints without duplicating framework behavior.
(defalias 'cp-org-marginalia-context-panel-render-side-item
  #'context-panels-org-render-side-item)
(defalias 'cp-org-marginalia-context-panel-jump-side-item
  #'context-panels-jump-to-anchor)
(defalias 'cp-org-marginalia-context-panel-jump-at-point
  #'context-panels-jump-at-point)

(defun cp-org-marginalia-context-panel--item-help (_source-buffer _item)
  "Describe native-note navigation without advertising comment operations."
  (concat (context-panels-command-description
	   #'context-panels-jump-at-point "Jump to source reference")
	  "\n\nEdit the note through Org's native footnote navigation."))

(defun cp-org-marginalia-context-panel-provider ()
  "Return the cp-org-marginalia context-panel provider descriptor."
  (list :name 'marginalia
	:icon "📝"
	:priority 20
	:collect-side-items #'cp-org-marginalia-collect
	:present-side-item #'cp-org-marginalia-context-panel--presentation
	:item-help #'cp-org-marginalia-context-panel--item-help))

;;;###autoload
(define-minor-mode cp-org-marginalia-context-panel-mode
  "Toggle Org marginalia as a `context-panels' provider."
  :lighter " Marginalia"
  (if cp-org-marginalia-context-panel-mode
      (progn
	(context-panels-register-provider (cp-org-marginalia-context-panel-provider))
	(context-panels-mode 1))
    (context-panels-unregister-provider 'marginalia)
    (unless (context-panels-registered-providers)
      (context-panels-mode -1))))

(provide 'cp-org-marginalia-context-panel)
;;; cp-org-marginalia-context-panel.el ends here
