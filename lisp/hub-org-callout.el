;;; hub-org-callout.el --- Shared Org callout semantics -*- lexical-binding: t; -*-

;;; Commentary:
;; Helpers for the canonical Org callout syntax shared by exporters and
;; authoring commands.

;;; Code:

(require 'hub-utils)

(declare-function org-export-read-attribute "ox" (attribute element &optional property))

(defconst hub/org-callout-types '("info" "note" "warning" "tip" "important")
  "Supported semantic callout types.")

(defun hub/org-callout--unquote (value)
  "Return VALUE without one layer of Org attribute string quoting."
  (cond
   ((not (stringp value)) value)
   ((string-match-p "\\`\".*\"\\'" value)
    (condition-case nil
	(car (read-from-string value))
      (error (string-trim value "\"" "\""))))
   (t value)))

(defun hub/org-callout-attributes (special-block)
  "Return canonical callout attributes for SPECIAL-BLOCK.
The returned plist currently supports `:type' and `:title', read from
`#+ATTR_CALLOUT:'."
  (let* ((attr (org-export-read-attribute :attr_callout special-block))
	 (type (hub/org-callout--unquote (plist-get attr :type)))
	 (title (hub/org-callout--unquote (plist-get attr :title))))
    (list :type type :title title)))

(defun hub/org-callout-type (special-block &optional default)
  "Return SPECIAL-BLOCK's callout type, or DEFAULT."
  (or (plist-get (hub/org-callout-attributes special-block) :type) default))

(defun hub/org-callout-title (special-block)
  "Return SPECIAL-BLOCK's callout title, or nil."
  (plist-get (hub/org-callout-attributes special-block) :title))

(provide 'hub-org-callout)
;;; hub-org-callout.el ends here
