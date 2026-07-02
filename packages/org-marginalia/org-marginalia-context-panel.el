;;; org-marginalia-context-panel.el --- Context-panel provider for Org marginalia -*- lexical-binding: t; -*-

;; Author: Hubert Behaghel
;; Maintainer: Hubert Behaghel
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (org "9.6"))
;; Keywords: outlines, convenience
;; URL: https://github.com/behaghel/org-marginalia

;;; Commentary:
;; Provider glue between `org-marginalia' footnote/sidenote records and the
;; reusable `org-context-panel' side panel.

;;; Code:

(require 'org)
(require 'org-context-panel)
(require 'org-marginalia)
(require 'subr-x)

(defface org-marginalia-context-panel-icon-face
  '((t :inherit shadow))
  "Face used for compact marginalia icons."
  :group 'org-marginalia)

(defun org-marginalia-context-panel--kind-icon (kind)
  "Return compact display icon for marginalia KIND."
  (pcase kind
    ('footnote "†")
    (_ "✣")))

(defun org-marginalia-context-panel--org-fontified-text (text)
  "Return TEXT fontified as Org without changing the current buffer mode."
  (with-temp-buffer
    (org-mode)
    (insert text)
    (font-lock-ensure (point-min) (point-max))
    (buffer-string)))

(defun org-marginalia-context-panel--apply-list-wrap-prefix (start end)
  "Indent wrapped list continuation lines between START and END by two spaces."
  (save-excursion
    (goto-char start)
    (while (< (point) end)
      (when (looking-at-p "[[:space:]]*\\([-+*]\\|[0-9]+[.)]\\)[[:space:]]+")
	(add-text-properties (line-beginning-position) (line-end-position)
			     '(wrap-prefix "  ")))
      (forward-line 1))))

(defun org-marginalia-context-panel--insert-body (body)
  "Insert marginalia BODY with Org fontification and wrapping hints."
  (unless (string-empty-p body)
    (let ((start (point)))
      (insert (org-marginalia-context-panel--org-fontified-text body) "\n")
      (org-marginalia-context-panel--apply-list-wrap-prefix start (point)))))

(defun org-marginalia-context-panel-collect-side-items (source-buffer)
  "Collect marginalia side items for SOURCE-BUFFER."
  (with-current-buffer source-buffer
    (org-marginalia-layout (org-marginalia-collect))))

(defun org-marginalia-context-panel-render-side-item (_source-buffer item)
  "Render one marginalia side-panel ITEM."
  (let ((start (point))
	(kind (plist-get item :kind))
	(body (or (plist-get item :body) "")))
    (insert (propertize (org-marginalia-context-panel--kind-icon kind)
			'face 'org-marginalia-context-panel-icon-face))
    (insert (format " %s\n" (plist-get item :id)))
    (org-marginalia-context-panel--insert-body body)
    (add-text-properties start (point)
			 `(org-marginalia-item ,item
					       org-context-panel-item ,item
					       mouse-face highlight
					       help-echo "RET: jump to source reference"))))

(defun org-marginalia-context-panel-jump-side-item (source-buffer item)
  "Jump from marginalia ITEM to its source reference in SOURCE-BUFFER."
  (let ((position (plist-get item :reference-pos)))
    (unless (and (buffer-live-p source-buffer) position)
      (user-error "Marginalia item has no source reference"))
    (pop-to-buffer source-buffer)
    (goto-char position)))

(defun org-marginalia-context-panel-jump-at-point ()
  "Jump from the current marginalia row to its source reference."
  (interactive)
  (let ((item (or (get-text-property (point) 'org-marginalia-item)
		  (get-text-property (line-beginning-position) 'org-marginalia-item)
		  (get-text-property (max (point-min) (1- (line-end-position)))
				     'org-marginalia-item))))
    (unless item
      (user-error "No Org marginalia item at point"))
    (org-marginalia-context-panel-jump-side-item
     (org-context-panel-current-source-buffer)
     item)))

(defun org-marginalia-context-panel-provider ()
  "Return the org-marginalia context-panel provider descriptor."
  (list :name 'marginalia
	:priority 20
	:collect-side-items #'org-marginalia-context-panel-collect-side-items
	:render-side-item #'org-marginalia-context-panel-render-side-item
	:jump-side-item #'org-marginalia-context-panel-jump-side-item))

;;;###autoload
(define-minor-mode org-marginalia-context-panel-mode
  "Toggle Org marginalia as an `org-context-panel' provider."
  :lighter " Marginalia"
  (if org-marginalia-context-panel-mode
      (progn
	(org-context-panel-register-provider (org-marginalia-context-panel-provider))
	(org-context-panel-mode 1))
    (org-context-panel-unregister-provider 'marginalia)
    (unless (org-context-panel-registered-providers)
      (org-context-panel-mode -1))))

(provide 'org-marginalia-context-panel)
;;; org-marginalia-context-panel.el ends here
