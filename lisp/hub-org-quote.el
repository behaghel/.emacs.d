;;; hub-org-quote.el --- Shared Org quote semantics -*- lexical-binding: t; -*-

;;; Commentary:
;; Helpers for canonical Org quote-block metadata shared by authoring commands,
;; exporters, and semantic audits.

;;; Code:

(require 'subr-x)

(declare-function org-export-read-attribute "ox" (attribute element &optional property))

(defun hub/org-quote--unquote (value)
  "Return VALUE without one layer of Org attribute string quoting."
  (cond
   ((not (stringp value)) value)
   ((string-match-p "\\`\".*\"\\'" value)
    (condition-case nil
	(car (read-from-string value))
      (error (string-trim value "\"" "\""))))
   (t value)))

(defun hub/org-quote-attributes (quote-block)
  "Return canonical quote attributes for QUOTE-BLOCK.
The returned plist currently supports `:author', read from `#+ATTR_QUOTE:'."
  (let* ((attr (org-export-read-attribute :attr_quote quote-block))
	 (author (hub/org-quote--unquote (plist-get attr :author))))
    (list :author author)))

(defun hub/org-quote-author (quote-block)
  "Return QUOTE-BLOCK's attributed author, or nil."
  (let ((author (plist-get (hub/org-quote-attributes quote-block) :author)))
    (unless (string-empty-p (string-trim (or author "")))
      author)))

(provide 'hub-org-quote)
;;; hub-org-quote.el ends here
