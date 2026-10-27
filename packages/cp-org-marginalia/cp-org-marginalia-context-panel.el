;;; cp-org-marginalia-context-panel.el --- Context-panel provider for Org marginalia -*- lexical-binding: t; -*-

;; Author: Hubert Behaghel
;; Maintainer: Hubert Behaghel
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (org "9.6") (context-panels "0.1.0"))
;; Keywords: outlines, convenience
;; URL: https://github.com/behaghel/.emacs.d/tree/master/packages/cp-org-marginalia

;;; Commentary:
;; Provider glue between `cp-org-marginalia' footnote/sidenote records and the
;; reusable `context-panels' side panel.

;;; Code:

(require 'org)
(require 'context-panels)
(require 'cp-org-marginalia)
(require 'subr-x)

(defface cp-org-marginalia-context-panel-icon-face
  '((t :inherit shadow))
  "Face used for compact marginalia icons."
  :group 'cp-org-marginalia)

(defvar cp-org-marginalia-context-panel-item-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'context-panels-jump-at-point)
    (define-key map (kbd "<return>") #'context-panels-jump-at-point)
    map)
  "Keymap for marginalia rows in standalone or mixed-provider panels.
Row-local bindings preserve the panel's major-mode map and other providers.")

(defun cp-org-marginalia-context-panel--kind-icon (kind)
  "Return compact display icon for marginalia KIND."
  (pcase kind
    ('footnote "†")
    (_ "✣")))

(defun cp-org-marginalia-context-panel--org-fontified-text (text)
  "Return TEXT fontified as Org without changing the current buffer mode."
  (with-temp-buffer
    (org-mode)
    (insert text)
    (font-lock-ensure (point-min) (point-max))
    (buffer-string)))

(defun cp-org-marginalia-context-panel--apply-list-wrap-prefix (start end)
  "Indent wrapped list continuation lines between START and END by two spaces."
  (save-excursion
    (goto-char start)
    (while (< (point) end)
      (when (looking-at-p "[[:space:]]*\\([-+*]\\|[0-9]+[.)]\\)[[:space:]]+")
	(add-text-properties (line-beginning-position) (line-end-position)
			     '(wrap-prefix "  ")))
      (forward-line 1))))

(defun cp-org-marginalia-context-panel--insert-body (body)
  "Insert marginalia BODY with Org fontification and wrapping hints."
  (unless (string-empty-p body)
    (let ((start (point)))
      (insert (cp-org-marginalia-context-panel--org-fontified-text body) "\n")
      (cp-org-marginalia-context-panel--apply-list-wrap-prefix start (point)))))

(defun cp-org-marginalia-context-panel--mark-item-icon (item)
  "Return marginalia ITEM copied with its rendered icon metadata."
  (let ((copy (copy-sequence item)))
    (plist-put copy :icon
	       (cp-org-marginalia-context-panel--kind-icon
		(plist-get copy :kind)))
    copy))

(defun cp-org-marginalia-context-panel-collect-side-items (source-buffer)
  "Collect marginalia side items for SOURCE-BUFFER."
  (with-current-buffer source-buffer
    (mapcar #'cp-org-marginalia-context-panel--mark-item-icon
	    (cp-org-marginalia-layout (cp-org-marginalia-collect)))))

(defun cp-org-marginalia-context-panel-render-side-item (_source-buffer item)
  "Render one marginalia side-panel ITEM."
  (let ((start (point))
	(kind (plist-get item :kind))
	(body (or (plist-get item :body) "")))
    (insert (propertize (cp-org-marginalia-context-panel--kind-icon kind)
			'face 'cp-org-marginalia-context-panel-icon-face))
    (insert (format " %s\n" (plist-get item :id)))
    (cp-org-marginalia-context-panel--insert-body body)
    (add-text-properties start (point)
			 `(cp-org-marginalia-item ,item
						  context-panels-item ,item
						  keymap ,cp-org-marginalia-context-panel-item-map
						  mouse-face highlight
						  help-echo "RET: jump to source reference"))))

(defun cp-org-marginalia-context-panel-jump-side-item (source-buffer item)
  "Jump from marginalia ITEM to its source reference in SOURCE-BUFFER."
  (let ((position (plist-get item :reference-pos)))
    (unless (and (buffer-live-p source-buffer) position)
      (user-error "Marginalia item has no source reference"))
    (pop-to-buffer source-buffer)
    (goto-char position)))

(defun cp-org-marginalia-context-panel-jump-at-point ()
  "Jump from the current marginalia row to its source reference."
  (interactive)
  (let ((item (or (get-text-property (point) 'cp-org-marginalia-item)
		  (get-text-property (line-beginning-position) 'cp-org-marginalia-item)
		  (get-text-property (max (point-min) (1- (line-end-position)))
				     'cp-org-marginalia-item))))
    (unless item
      (user-error "No Org marginalia item at point"))
    (cp-org-marginalia-context-panel-jump-side-item
     (context-panels-current-source-buffer)
     item)))

(defun cp-org-marginalia-context-panel-provider ()
  "Return the cp-org-marginalia context-panel provider descriptor."
  (list :name 'marginalia
	:icon "📝"
	:priority 20
	:collect-side-items #'cp-org-marginalia-context-panel-collect-side-items
	:render-side-item #'cp-org-marginalia-context-panel-render-side-item
	:jump-side-item #'cp-org-marginalia-context-panel-jump-side-item))

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
