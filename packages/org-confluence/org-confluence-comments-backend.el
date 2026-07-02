;;; org-confluence-comments-backend.el --- Org comments backend for Confluence -*- lexical-binding: t; -*-

;;; Commentary:
;; Thin registration layer between org-confluence and the generic org-comments
;; backend registry.  This slice establishes the public integration seam; later
;; slices migrate existing Confluence comment sync/push/pull behavior behind
;; these operations.

;;; Code:

(require 'browse-url)
(require 'org)
(require 'org-comments-backend)
(require 'org-comments-store)
(require 'org-confluence-api)
(require 'org-confluence-comments)

(defun org-confluence-comments-backend--not-migrated (operation)
  "Signal that Confluence comments OPERATION is not migrated yet."
  (user-error "Confluence comments backend operation %s is not migrated yet" operation))

(defun org-confluence-comments-backend-sync (record)
  "Synchronize Confluence comments for RECORD without syncing page content.
RECORD is a plist containing `:source-file'.  Optional `:page-id' and
`:body-format' are passed to the comments-only sync helper."
  (let ((source-file (org-confluence-comments-backend--source-file record "sync")))
    (with-current-buffer (find-file-noselect source-file)
      (org-mode)
      (org-confluence-comments--sync (plist-get record :page-id)
				     (plist-get record :body-format)))))

(defun org-confluence-comments-backend--local-sidecar-location (comment)
  "Return local COMMENT sidecar location as (SIDECAR-FILE . COMMENT-ID)."
  (let ((sidecar-file (plist-get comment :sidecar-file))
	(comment-id (plist-get comment :id)))
    (unless (and sidecar-file comment-id)
      (user-error "Confluence push needs :sidecar-file and :id"))
    (cons sidecar-file comment-id)))

(defun org-confluence-comments-backend-push (comment)
  "Push local COMMENT to Confluence using the existing command implementation.
COMMENT is a plist containing `:sidecar-file' and `:id'.  Optional `:page-id'
overrides page metadata read by the existing push command."
  (pcase-let ((`(,sidecar-file . ,comment-id)
	       (org-confluence-comments-backend--local-sidecar-location comment)))
    (with-current-buffer (find-file-noselect sidecar-file)
      (org-mode)
      (goto-char (point-min))
      (unless (org-comments-goto-id comment-id)
	(user-error "Cannot find comment %s" comment-id))
      (org-confluence-comments-push-current (plist-get comment :page-id)))))

(defun org-confluence-comments-backend--source-file (record operation)
  "Return source file from RECORD for OPERATION or signal a user error."
  (or (plist-get record :source-file)
      (plist-get record :file)
      (user-error "Confluence %s needs :source-file" operation)))

(defun org-confluence-comments-backend-pull (record)
  "Pull remote comments into RECORD's source Org sidecar.
RECORD is a plist containing `:source-file'.  Optional `:page-id' and
`:body-format' are passed to the existing import command."
  (let ((source-file (org-confluence-comments-backend--source-file record "pull")))
    (with-current-buffer (find-file-noselect source-file)
      (org-mode)
      (org-confluence-comments-import (plist-get record :page-id)
				      (plist-get record :body-format)))))

(defun org-confluence-comments-backend--source-metadata (comment)
  "Return Confluence metadata from COMMENT's source file."
  (when-let* ((source-file (plist-get comment :source-file)))
    (with-current-buffer (find-file-noselect source-file)
      (org-mode)
      (list :page-id (org-confluence-api--page-id-from-buffer)
	    :space (org-confluence-api--space-from-buffer)))))

(defun org-confluence-comments-backend--comment-page-id (comment)
  "Return Confluence page id from COMMENT or signal a user error."
  (or (plist-get comment :page-id)
      (plist-get comment :remote-page-id)
      (plist-get comment :confluence-page-id)
      (plist-get (org-confluence-comments-backend--source-metadata comment) :page-id)
      (user-error "Confluence comment has no page id")))

(defun org-confluence-comments-backend--comment-remote-id (comment)
  "Return Confluence remote comment id from COMMENT or signal a user error."
  (or (plist-get comment :remote-id)
      (plist-get comment :comment-id)
      (plist-get comment :remote-comment-id)
      (user-error "Confluence comment has no remote comment id")))

(defun org-confluence-comments-backend-open-remote (comment)
  "Open remote Confluence COMMENT in a browser and return its URL.
COMMENT is a plist containing at least `:page-id' and `:remote-id'.  Optional
`:space' is used when building a space-qualified Confluence URL."
  (let* ((page-id (org-confluence-comments-backend--comment-page-id comment))
	 (remote-id (org-confluence-comments-backend--comment-remote-id comment))
	 (space (or (plist-get comment :space)
		    (plist-get (org-confluence-comments-backend--source-metadata comment) :space)))
	 (url (org-confluence-api--comment-url page-id remote-id space)))
    (browse-url url)
    url))

(defun org-confluence-comments-backend-detect (&optional source-buffer)
  "Return non-nil when SOURCE-BUFFER is linked to a Confluence page."
  (with-current-buffer (or source-buffer (current-buffer))
    (and (derived-mode-p 'org-mode)
	 (org-confluence-api--page-id-from-buffer))))

;;;###autoload
(defun org-confluence-comments-backend-register ()
  "Register the Confluence backend with `org-comments'."
  (org-comments-register-backend
   'confluence
   (list :name "Confluence"
	 :capabilities '(:sync :push :pull :open-remote)
	 :sync #'org-confluence-comments-backend-sync
	 :push #'org-confluence-comments-backend-push
	 :pull #'org-confluence-comments-backend-pull
	 :open-remote #'org-confluence-comments-backend-open-remote))
  (org-comments-register-backend-detector
   'confluence #'org-confluence-comments-backend-detect))

(org-confluence-comments-backend-register)

(provide 'org-confluence-comments-backend)
;;; org-confluence-comments-backend.el ends here
