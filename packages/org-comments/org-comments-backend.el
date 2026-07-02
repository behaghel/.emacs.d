;;; org-comments-backend.el --- Backend protocol for Org comments -*- lexical-binding: t; -*-

;;; Commentary:
;; Plist-based backend registry and dispatch helpers for Org comments.  This is
;; intentionally small while the package still exposes the legacy
;; `org-comments-*' command surface.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defgroup org-comments-backend nil
  "Backend registry for Org comments."
  :group 'org)

(defcustom org-comments-default-backend 'org
  "Default backend id used by backend-neutral Org comments commands."
  :type 'symbol
  :group 'org-comments-backend)

(defvar org-comments-backends (make-hash-table :test #'eq)
  "Registered Org comments backends keyed by backend id.")

(defvar org-comments-backend-detectors nil
  "Registered backend detectors as (ID . FUNCTION) entries.
Each FUNCTION receives a source buffer and returns non-nil when backend ID
should handle remote comment operations for that buffer.")

(defun org-comments-register-backend (id spec)
  "Register backend ID with plist SPEC and return SPEC.
SPEC may contain :name, :capabilities, and operation keys such as :list,
:create, :update, :delete, :reply, :set-status, :open-remote, and :sync."
  (puthash id (plist-put (copy-sequence spec) :id id) org-comments-backends))

(defun org-comments-backend (id)
  "Return registered backend ID's spec, or signal a user error."
  (or (gethash id org-comments-backends)
      (user-error "Unknown org-comments backend: %s" id)))

(defun org-comments-backend-name (id)
  "Return human-readable name for backend ID."
  (or (plist-get (org-comments-backend id) :name)
      (symbol-name id)))

(defun org-comments-backend-capabilities (id)
  "Return capability list for backend ID."
  (plist-get (org-comments-backend id) :capabilities))

(defun org-comments-backend-capable-p (id capability)
  "Return non-nil when backend ID declares CAPABILITY."
  (memq capability (org-comments-backend-capabilities id)))

(defun org-comments-register-backend-detector (id function)
  "Register FUNCTION as a source-buffer detector for backend ID."
  (unless (functionp function)
    (user-error "Backend detector for %s is not callable" id))
  (setq org-comments-backend-detectors
	(cons (cons id function)
	      (assq-delete-all id org-comments-backend-detectors)))
  id)

(defun org-comments-backend-registered-p (id)
  "Return non-nil when backend ID is registered."
  (and (gethash id org-comments-backends) t))

(defun org-comments-backend-detect (&optional source-buffer)
  "Return remote comments backend for SOURCE-BUFFER or current buffer.
Detectors are asked in registration order.  A detector can select a backend only
when that backend is registered; otherwise detection falls back to
`org-comments-default-backend'."
  (let ((buffer (or source-buffer (current-buffer))))
    (or (cl-loop for (id . detector) in org-comments-backend-detectors
		 when (and (org-comments-backend-registered-p id)
			   (with-current-buffer buffer
			     (funcall detector buffer)))
		 return id)
	org-comments-default-backend)))

(defun org-comments-backend-dispatch (id operation &rest args)
  "Dispatch backend ID OPERATION with ARGS.
OPERATION is the plist key naming the backend function, for example :list or
:create."
  (let* ((backend (org-comments-backend id))
	 (function (plist-get backend operation)))
    (unless (functionp function)
      (user-error "Backend %s does not support %s"
		  (org-comments-backend-name id) operation))
    (apply function args)))

(defun org-comments-backend-list (id &optional source-buffer include-stale)
  "List comments from backend ID for SOURCE-BUFFER.
When INCLUDE-STALE is non-nil, include stale comments when the backend supports
that distinction."
  (org-comments-backend-dispatch id :list source-buffer include-stale))

(defun org-comments-backend-create (id record)
  "Create RECORD through backend ID."
  (org-comments-backend-dispatch id :create record))

(defun org-comments-backend-update (id record)
  "Update RECORD through backend ID."
  (org-comments-backend-dispatch id :update record))

(defun org-comments-backend-delete (id comment-id)
  "Delete COMMENT-ID through backend ID."
  (org-comments-backend-dispatch id :delete comment-id))

(defun org-comments-backend-reply (id record)
  "Create reply RECORD through backend ID."
  (org-comments-backend-dispatch id :reply record))

(defun org-comments-backend-set-status (id comment-id status)
  "Set COMMENT-ID to STATUS through backend ID."
  (org-comments-backend-dispatch id :set-status comment-id status))

(defun org-comments-backend-open-remote (id comment)
  "Open remote COMMENT through backend ID."
  (org-comments-backend-dispatch id :open-remote comment))

(defun org-comments-backend-push (id record)
  "Push RECORD through backend ID."
  (org-comments-backend-dispatch id :push record))

(defun org-comments-backend-pull (id record)
  "Pull RECORD through backend ID."
  (org-comments-backend-dispatch id :pull record))

(defun org-comments-backend-sync (id &rest args)
  "Synchronize backend ID with ARGS."
  (apply #'org-comments-backend-dispatch id :sync args))

(provide 'org-comments-backend)
;;; org-comments-backend.el ends here
