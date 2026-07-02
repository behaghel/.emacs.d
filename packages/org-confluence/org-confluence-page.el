;;; org-confluence-page.el --- Confluence page metadata helpers -*- lexical-binding: t; -*-

;;; Commentary:
;; Generic Org/Confluence page metadata helpers used by publishing and sync.

;;; Code:

(require 'seq)
(require 'subr-x)

(require 'org-confluence-api)

(defun org-confluence-page-number-property-line (key value)
  "Return an Org property line for KEY and numeric VALUE when present."
  (when (numberp value)
    (format ":%s: %s\n" key value)))

(defun org-confluence-page-title-from-buffer ()
  "Return the Org #+TITLE value from the current buffer, or nil."
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search t))
      (when (re-search-forward "^[	]*#\\+TITLE:[	]*\\(.*?\\)[	]*$" nil t)
	(let ((title (string-trim (match-string-no-properties 1))))
	  (unless (string-empty-p title) title))))))

(defun org-confluence-page-created-page-id (output)
  "Return Confluence page ID parsed from cfl create OUTPUT, or nil."
  (cond
   ((string-match "\"id\"[[:space:]]*:[[:space:]]*\"?\\([0-9]+\\)\"?" output)
    (match-string 1 output))
   ((string-match "\\b\\(?:Page[[:space:]]+\\)?ID[[:space:]]*[:=][[:space:]]*\\([0-9]+\\)" output)
    (match-string 1 output))
   ((string-match "/pages/\\([0-9]+\\)" output)
    (match-string 1 output))))

(defun org-confluence-page-metadata-keyword-present-p (keyword)
  "Return non-nil when Org metadata KEYWORD is present in the current buffer."
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search t)
	  (regexp (format "^[ \t]*#\\+%s:" (regexp-quote keyword))))
      (re-search-forward regexp nil t))))

(defun org-confluence-page-insert-metadata-after-keywords (metadata)
  "Insert missing Confluence METADATA after leading Org keyword lines.

METADATA is an alist of Org keyword names to values."
  (let ((missing (seq-filter (lambda (entry)
			       (not (org-confluence-page-metadata-keyword-present-p (car entry))))
			     metadata)))
    (when missing
      (save-excursion
	(goto-char (point-min))
	(while (looking-at "^[ \t]*#\\+[^:\n]+:.*$")
	  (forward-line 1))
	(dolist (entry missing)
	  (insert (format "#+%s: %s\n" (car entry) (cdr entry))))))))

(defun org-confluence-page-id-or-read (page-id)
  "Return PAGE-ID, current buffer page ID, or prompt for one."
  (or page-id
      (org-confluence-api--page-id-from-buffer)
      (read-string "Confluence page ID: ")))

(provide 'org-confluence-page)
;;; org-confluence-page.el ends here
