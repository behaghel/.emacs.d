;;; org-copilot-sidecar.el --- Durable sidecars for Org Copilot -*- lexical-binding: t; -*-

;; Author: Hubert Behaghel
;; Maintainer: Hubert Behaghel
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (org "9.6"))
;; Keywords: outlines, tools, convenience
;; URL: https://github.com/behaghel/org-copilot

;;; Commentary:
;; Minimal Org sidecar persistence for Org Copilot chat/session transcript.

;;; Code:

(require 'org)
(require 'subr-x)
(require 'cl-lib)

(defconst org-copilot-sidecar-schema-version 1
  "Current Org Copilot sidecar schema version.")

(defun org-copilot-sidecar-path (&optional source-file)
  "Return Copilot sidecar path for SOURCE-FILE or current buffer file."
  (let ((file (or source-file buffer-file-name)))
    (unless file
      (user-error "Current buffer is not visiting a file"))
    (concat (file-name-sans-extension file) ".copilot.org")))

(defun org-copilot-sidecar--relative-source-file (source-file sidecar-file)
  "Return SOURCE-FILE relative to SIDECAR-FILE directory."
  (file-relative-name source-file (file-name-directory sidecar-file)))

(defun org-copilot-sidecar--insert-session (session-id)
  "Insert an active Copilot session heading for SESSION-ID at point."
  (insert (format "* Session %s\n" session-id))
  (insert ":PROPERTIES:\n")
  (insert (format ":ORG_COPILOT_SESSION_ID: %s\n" session-id))
  (insert ":END:\n\n")
  (insert "** Messages\n"))

(defun org-copilot-sidecar--active-session-exists-p (session-id)
  "Return non-nil when active SESSION-ID exists in current buffer."
  (save-excursion
    (goto-char (point-min))
    (cl-loop while (re-search-forward org-heading-regexp nil t)
	     do (goto-char (match-beginning 0))
	     when (and (= (org-outline-level) 1)
		       (equal session-id
			      (org-entry-get nil "ORG_COPILOT_SESSION_ID")))
	     return t
	     do (forward-line 1))))

(defun org-copilot-sidecar--ensure-header (sidecar-file source-file)
  "Ensure SIDECAR-FILE exists with a header for SOURCE-FILE."
  (if (file-exists-p sidecar-file)
      (with-temp-buffer
	(insert-file-contents sidecar-file)
	(org-mode)
	(unless (org-copilot-sidecar--active-session-exists-p "default")
	  (goto-char (point-max))
	  (unless (bolp) (insert "\n"))
	  (insert "\n")
	  (org-copilot-sidecar--insert-session "default")
	  (write-region (point-min) (point-max) sidecar-file nil 'silent)))
    (make-directory (file-name-directory sidecar-file) t)
    (with-temp-file sidecar-file
      (insert (format "#+title: Copilot for %s\n"
		      (file-name-nondirectory source-file)))
      (insert (format "#+source: %s\n"
		      (org-copilot-sidecar--relative-source-file
		       source-file sidecar-file)))
      (insert (format "#+org_copilot_schema_version: %d\n"
		      org-copilot-sidecar-schema-version))
      (insert "#+org_copilot_current_session: default\n\n")
      (org-copilot-sidecar--insert-session "default"))))

(defun org-copilot-sidecar--entry-body (end)
  "Return current heading body text ending before child headings and END."
  (save-excursion
    (let* ((level (org-outline-level))
	   (body-start (progn
			 (forward-line 1)
			 (when (looking-at-p "[[:space:]]*:PROPERTIES:[[:space:]]*$")
			   (when (re-search-forward "^[[:space:]]*:END:[[:space:]]*$" end t)
			     (forward-line 1)))
			 (point)))
	   (body-end (save-excursion
		       (if (re-search-forward org-heading-regexp end t)
			   (let ((heading-start (match-beginning 0)))
			     (goto-char heading-start)
			     (if (> (org-outline-level) level) heading-start end))
			 end))))
      (string-trim (buffer-substring-no-properties body-start body-end)))))

(defun org-copilot-sidecar-append-message
    (source-file role message &optional properties)
  "Append ROLE MESSAGE for SOURCE-FILE to the Copilot sidecar.
PROPERTIES is a plist of additional Org property names and values."
  (let ((sidecar-file (org-copilot-sidecar-path source-file)))
    (org-copilot-sidecar--ensure-header sidecar-file source-file)
    (with-temp-buffer
      (insert-file-contents sidecar-file)
      (goto-char (point-max))
      (unless (bolp) (insert "\n"))
      (insert (format "*** %s\n" (upcase (symbol-name role))))
      (insert ":PROPERTIES:\n")
      (while properties
	(let ((key (pop properties))
	      (value (pop properties)))
	  (when value
	    (insert (format ":%s: %s\n" key value)))))
      (insert ":END:\n")
      (insert (string-trim-right (or message "")) "\n")
      (write-region (point-min) (point-max) sidecar-file nil 'silent))
    sidecar-file))

(defun org-copilot-sidecar--message-at-heading ()
  "Return chat message plist at current heading, or nil."
  (let* ((role-name (car (split-string (org-get-heading t t t t))))
	 (role (and role-name (intern-soft (downcase role-name))))
	 (end (save-excursion (org-end-of-subtree t t)))
	 (properties (org-entry-properties nil nil))
	 (content (org-copilot-sidecar--entry-body end)))
    (when (memq role '(user assistant))
      (list :role role
	    :content content
	    :context-id (alist-get "ORG_COPILOT_CONTEXT_ID" properties nil nil #'equal)
	    :comment-id (alist-get "ORG_COPILOT_COMMENT_ID" properties nil nil #'equal)
	    :created-at (alist-get "ORG_COPILOT_CREATED_AT" properties nil nil #'equal)))))

(defun org-copilot-sidecar-load-messages (&optional source-file)
  "Return durable chat messages for SOURCE-FILE."
  (let ((sidecar-file (org-copilot-sidecar-path source-file))
	messages)
    (when (file-exists-p sidecar-file)
      (with-temp-buffer
	(insert-file-contents sidecar-file)
	(org-mode)
	(goto-char (point-min))
	(while (re-search-forward org-heading-regexp nil t)
	  (goto-char (match-beginning 0))
	  (when-let* ((message (org-copilot-sidecar--message-at-heading)))
	    (push message messages))
	  (forward-line 1))))
    (nreverse messages)))

(defun org-copilot-sidecar-archive-session (source-file session-id)
  "Archive Copilot SESSION-ID subtree for SOURCE-FILE."
  (let ((sidecar-file (org-copilot-sidecar-path source-file)))
    (when (file-exists-p sidecar-file)
      (with-current-buffer (find-file-noselect sidecar-file)
	(org-mode)
	(save-excursion
	  (goto-char (point-min))
	  (while (re-search-forward org-heading-regexp nil t)
	    (goto-char (match-beginning 0))
	    (if (and (= (org-outline-level) 1)
		     (equal session-id
			    (org-entry-get nil "ORG_COPILOT_SESSION_ID")))
		(org-archive-subtree)
	      (forward-line 1))))
	(save-buffer)))))

(defun org-copilot-sidecar-delete-session (source-file session-id)
  "Delete Copilot SESSION-ID subtree for SOURCE-FILE."
  (let ((sidecar-file (org-copilot-sidecar-path source-file)))
    (when (file-exists-p sidecar-file)
      (with-temp-buffer
	(insert-file-contents sidecar-file)
	(org-mode)
	(goto-char (point-min))
	(while (re-search-forward org-heading-regexp nil t)
	  (goto-char (match-beginning 0))
	  (if (and (= (org-outline-level) 1)
		   (equal session-id
			  (org-entry-get nil "ORG_COPILOT_SESSION_ID")))
	      (delete-region (point) (save-excursion (org-end-of-subtree t t)))
	    (forward-line 1)))
	(write-region (point-min) (point-max) sidecar-file nil 'silent)))))

(provide 'org-copilot-sidecar)
;;; org-copilot-sidecar.el ends here
