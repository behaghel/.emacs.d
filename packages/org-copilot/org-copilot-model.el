;;; org-copilot-model.el --- Data model helpers for Org Copilot -*- lexical-binding: t; -*-

;; Author: Hubert Behaghel
;; Maintainer: Hubert Behaghel
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (org "9.6"))
;; Keywords: outlines, tools, convenience
;; URL: https://github.com/behaghel/org-copilot

;;; Commentary:
;; Plist-shaped model helpers for Org Copilot AI comments.

;;; Code:

(require 'cl-lib)

(defgroup org-copilot nil
  "Org-native AI authoring assistance."
  :group 'org)

(defconst org-copilot-comment-statuses '(active accepted dismissed stale)
  "Valid lifecycle statuses for Org Copilot AI comments.")

(defconst org-copilot-comment-types '(inline scope)
  "Valid AI comment types for Org Copilot.")

(defun org-copilot-valid-comment-status-p (status)
  "Return non-nil when STATUS is a valid AI comment status."
  (memq status org-copilot-comment-statuses))

(defun org-copilot-valid-comment-type-p (type)
  "Return non-nil when TYPE is a valid AI comment type."
  (memq type org-copilot-comment-types))

(defun org-copilot-comment-id (comment)
  "Return COMMENT's stable session-local id."
  (plist-get comment :id))

(defun org-copilot-comment-status (comment)
  "Return COMMENT's lifecycle status."
  (plist-get comment :status))

(defun org-copilot-comment-with-status (comment status)
  "Return a copy of COMMENT with lifecycle STATUS."
  (unless (org-copilot-valid-comment-status-p status)
    (error "Invalid Org Copilot comment status: %S" status))
  (let ((copy (copy-sequence comment)))
    (plist-put copy :status status)))

(defun org-copilot-normalize-comment (comment)
  "Return a normalized copy of AI COMMENT.
COMMENT is a plist.  The normalized copy defaults missing `:status' to
`active' and missing `:type' to `inline'."
  (unless (listp comment)
    (error "Org Copilot comment must be a plist"))
  (let ((copy (copy-sequence comment)))
    (unless (plist-get copy :id)
      (error "Org Copilot comment is missing :id"))
    (unless (plist-member copy :status)
      (setq copy (plist-put copy :status 'active)))
    (unless (plist-member copy :type)
      (setq copy (plist-put copy :type 'inline)))
    (unless (org-copilot-valid-comment-status-p (plist-get copy :status))
      (error "Invalid Org Copilot comment status: %S"
	     (plist-get copy :status)))
    (unless (org-copilot-valid-comment-type-p (plist-get copy :type))
      (error "Invalid Org Copilot comment type: %S"
	     (plist-get copy :type)))
    copy))

(provide 'org-copilot-model)
;;; org-copilot-model.el ends here
