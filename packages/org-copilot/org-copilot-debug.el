;;; org-copilot-debug.el --- Debug trace buffer for Org Copilot -*- lexical-binding: t; -*-

;; Author: Hubert Behaghel
;; Maintainer: Hubert Behaghel
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: outlines, tools, convenience
;; URL: https://github.com/behaghel/org-copilot

;;; Commentary:
;; Lightweight human-readable diagnostics for Org Copilot request parsing and
;; installation.  The buffer is intentionally local and does not persist raw
;; prompts or responses to disk.

;;; Code:

(require 'pp)
(require 'subr-x)

(defgroup org-copilot-debug nil
  "Debug trace support for Org Copilot."
  :group 'org-copilot)

(defcustom org-copilot-debug-buffer-name "*Org Copilot Debug*"
  "Name of the Org Copilot debug buffer."
  :type 'string
  :group 'org-copilot-debug)

(defcustom org-copilot-debug-enabled t
  "Whether Org Copilot should record diagnostic events."
  :type 'boolean
  :group 'org-copilot-debug)

(defun org-copilot-debug--buffer ()
  "Return the Org Copilot debug buffer."
  (let ((buffer (get-buffer-create org-copilot-debug-buffer-name)))
    (with-current-buffer buffer
      (special-mode))
    buffer))

(defun org-copilot-debug-record (title &rest fields)
  "Append debug event TITLE and FIELDS to `org-copilot-debug-buffer-name'.
FIELDS is a property list.  Values are pretty-printed for inspection."
  (when org-copilot-debug-enabled
    (with-current-buffer (org-copilot-debug--buffer)
      (let ((inhibit-read-only t))
	(goto-char (point-max))
	(unless (bobp)
	  (insert "\n"))
	(insert (format "* %s  %s\n" title (format-time-string "%FT%T%z")))
	(while fields
	  (let ((key (pop fields))
		(value (pop fields)))
	    (insert (format "** %s\n" (string-remove-prefix ":" (symbol-name key))))
	    (insert (pp-to-string value))))))))

;;;###autoload
(defun org-copilot-debug-open ()
  "Open the Org Copilot debug buffer."
  (interactive)
  (pop-to-buffer (org-copilot-debug--buffer)))

(provide 'org-copilot-debug)
;;; org-copilot-debug.el ends here
