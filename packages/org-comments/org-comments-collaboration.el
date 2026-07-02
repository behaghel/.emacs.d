;;; org-comments-collaboration.el --- Collaboration model for Org comments -*- lexical-binding: t; -*-

;;; Commentary:
;; Backend-neutral collaboration helpers for remote-capable Org comments.
;; Backends normalize provider metadata into these fields before filters and
;; renderers consume records.

;;; Code:

(require 'org-comments-core)
(require 'seq)
(require 'subr-x)

(defcustom org-comments-current-user-id-function #'ignore
  "Function returning the current backend-agnostic person id.
The function is called with no arguments and should return a string or nil."
  :type 'function
  :group 'org-comments)

(defcustom org-comments-resolve-person-function #'ignore
  "Function resolving a backend user id to a person plist.
The function is called with BACKEND and RAW-ID.  It should return a plist such
as `(:id ID :name NAME :me BOOL)' or nil."
  :type 'function
  :group 'org-comments)

(defconst org-comments-remote-states '(present missing deleted unconfirmed unknown)
  "Known backend-neutral remote presence states.")

(defconst org-comments-sync-states '(clean dirty conflict error unknown)
  "Known backend-neutral synchronization states.")

(defconst org-comments-local-state-flags
  '(:local-only :draft :edited :pending-push :push-error)
  "Known backend-neutral local lifecycle flags.")

(defun org-comments--normalize-symbol (value allowed)
  "Return VALUE as a symbol when it is a member of ALLOWED.
VALUE may already be a symbol or may be a string naming a symbol.  Unknown
values normalize to nil."
  (let ((symbol (cond
		 ((symbolp value) value)
		 ((stringp value) (intern (downcase value))))))
    (when (memq symbol allowed)
      symbol)))

(defun org-comments--normalize-local-state (state)
  "Return normalized local STATE as a list of known flags."
  (let ((values (cond
		 ((null state) nil)
		 ((listp state) state)
		 ((stringp state) (split-string state "[ ,]+" t))
		 (t (list state)))))
    (seq-filter (lambda (flag)
		  (memq flag org-comments-local-state-flags))
		(seq-map (lambda (value)
			   (if (keywordp value)
			       value
			     (intern (concat ":" (downcase (format "%s" value))))))
			 values))))

(defun org-comments-local-state-p (record flag)
  "Return non-nil when RECORD has local-state FLAG."
  (memq flag (org-comments--normalize-local-state
	      (plist-get record :local-state))))

(defun org-comments-record-resolved-p (record)
  "Return backend-neutral resolved state for RECORD."
  (cond
   ((plist-member record :resolved)
    (and (plist-get record :resolved) t))
   ((equal (plist-get record :status) "RESOLVED") t)
   ((member (plist-get record :status) '("OPEN" "TODO")) nil)
   (t nil)))

(defun org-comments-record-actionable-p (record)
  "Return non-nil when RECORD needs user attention."
  (or (and (plist-get record :actionable) t)
      (memq (plist-get record :sync-state) '(conflict error))
      (memq (plist-get record :remote-state) '(missing deleted))
      (org-comments-local-state-p record :pending-push)
      (org-comments-local-state-p record :push-error)))

(defun org-comments-resolve-person (backend raw-id)
  "Resolve BACKEND RAW-ID to a backend-agnostic person plist."
  (when (and backend raw-id org-comments-resolve-person-function)
    (funcall org-comments-resolve-person-function backend raw-id)))

(defun org-comments-current-user-id ()
  "Return the current backend-agnostic person id, or nil."
  (when org-comments-current-user-id-function
    (funcall org-comments-current-user-id-function)))

(defun org-comments-record-mine-p (record)
  "Return non-nil when RECORD is authored by the current user."
  (let* ((backend (plist-get record :backend))
	 (remote-author-id (plist-get record :remote-author-id))
	 (person (and remote-author-id
		      (org-comments-resolve-person backend remote-author-id)))
	 (person-id (plist-get person :id))
	 (current-user-id (org-comments-current-user-id)))
    (or (and person (plist-get person :me))
	(and person-id current-user-id (equal person-id current-user-id))
	(and (not remote-author-id)
	     current-user-id
	     (equal (plist-get record :author) current-user-id)))))

(defun org-comments-normalize-record (record)
  "Return RECORD with backend-neutral collaboration fields normalized.
Unknown plist keys are preserved."
  (let* ((normalized (copy-sequence record))
	 (remote-state (org-comments--normalize-symbol
			(plist-get normalized :remote-state)
			org-comments-remote-states))
	 (sync-state (org-comments--normalize-symbol
		      (plist-get normalized :sync-state)
		      org-comments-sync-states))
	 (local-state (org-comments--normalize-local-state
		       (plist-get normalized :local-state))))
    (setq normalized (plist-put normalized :remote-state remote-state))
    (setq normalized (plist-put normalized :sync-state sync-state))
    (setq normalized (plist-put normalized :local-state local-state))
    (setq normalized (plist-put normalized :resolved
				(org-comments-record-resolved-p normalized)))
    (setq normalized (plist-put normalized :actionable
				(org-comments-record-actionable-p normalized)))
    normalized))

(provide 'org-comments-collaboration)
;;; org-comments-collaboration.el ends here
