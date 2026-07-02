;;; org-comments-panel-filter.el --- Filters for Org comments panel -*- lexical-binding: t; -*-

;;; Commentary:
;; Generic filter registry, state, and predicates for the standalone Org
;; comments panel.

;;; Code:

(require 'org-comments-collaboration)
(require 'seq)
(require 'subr-x)

(declare-function org-comments-current-source-buffer "org-comments-panel-actions")
(declare-function org-comments-refresh-current-ui "org-comments-panel-actions")

(defvar org-comments-filter-registry nil
  "Registered Org comments filters.
Each entry is a cons cell of the form (ID . SPEC).  SPEC is a plist with at
least `:predicate'.")

(defvar-local org-comments-filter-state nil
  "Source-buffer-scoped Org comments filter state.")

(defvar-local org-comments-panel-filter-state nil
  "Legacy panel-local fallback filter state for Org comments panels.")

(defvar-local org-comments-panel-source-buffer nil
  "Source buffer associated with the current comments panel.")
(defvar-local org-comments-current-filter-reset-function
    #'org-comments-panel-filter--reset-current
  "Function used to reset filters for the current comments UI.")
(defvar-local org-comments-current-filter-toggle-resolved-function
    #'org-comments-panel-filter--toggle-resolved-current
  "Function used to toggle resolved comments for the current comments UI.")
(defvar-local org-comments-current-filter-status-function
    #'org-comments-panel-filter--status-current
  "Function used to show filter status for the current comments UI.")

(defun org-comments-register-filter (id predicate &rest properties)
  "Register comments filter ID using PREDICATE and PROPERTIES.
PREDICATE is called with COMMENT, VALUE, and full filter STATE.  It should
return non-nil when COMMENT is visible.  PROPERTIES may include `:label' and
`:default'."
  (let ((spec (append (list :predicate predicate) properties)))
    (setq org-comments-filter-registry
	  (cons (cons id spec)
		(assq-delete-all id org-comments-filter-registry)))
    id))

(defun org-comments-filter-predicate (id)
  "Return the predicate registered for filter ID, or nil."
  (plist-get (alist-get id org-comments-filter-registry) :predicate))

(defun org-comments-filter--spec (id)
  "Return registered filter spec for ID."
  (alist-get id org-comments-filter-registry))

(defun org-comments-filter--ids ()
  "Return registered filter ids in registration order."
  (mapcar #'car (reverse org-comments-filter-registry)))

(defun org-comments-filter-value (id state)
  "Return filter ID's value in STATE or its default value."
  (if (plist-member state id)
      (plist-get state id)
    (plist-get (org-comments-filter--spec id) :default)))

(defun org-comments-filter-default-state ()
  "Return the default Org comments filter state."
  (let (state)
    (dolist (id (org-comments-filter--ids) state)
      (setq state (plist-put state id
			     (plist-get (org-comments-filter--spec id) :default))))))

(defun org-comments-filter-state (&optional source-buffer)
  "Return source-scoped filter state for SOURCE-BUFFER.
When SOURCE-BUFFER is nil, use the current buffer."
  (with-current-buffer (or source-buffer (current-buffer))
    (or org-comments-filter-state
	(setq-local org-comments-filter-state
		    (org-comments-filter-default-state)))))

(defun org-comments-filter-set-state (state &optional source-buffer)
  "Set source-scoped filter STATE for SOURCE-BUFFER and return it.
When SOURCE-BUFFER is nil, use the current buffer."
  (with-current-buffer (or source-buffer (current-buffer))
    (setq-local org-comments-filter-state state)))

(defun org-comments-toggle-filter (id state)
  "Return STATE with boolean filter ID toggled."
  (plist-put (copy-sequence state)
	     id
	     (not (org-comments-filter-value id state))))

(defun org-comments-filter-current-source-buffer ()
  "Return source buffer for current comments UI filter operations."
  (if (fboundp 'org-comments-current-source-buffer)
      (org-comments-current-source-buffer)
    (or (and (boundp 'org-comments-panel-source-buffer)
	     (buffer-live-p org-comments-panel-source-buffer)
	     org-comments-panel-source-buffer)
	(current-buffer))))

(defun org-comments-filter-include-p (comment state)
  "Return non-nil when COMMENT should be visible under filter STATE."
  (let ((normalized (org-comments-normalize-record comment)))
    (seq-every-p
     (lambda (id)
       (let ((predicate (org-comments-filter-predicate id)))
	 (or (not predicate)
	     (funcall predicate
		      normalized
		      (org-comments-filter-value id state)
		      state))))
     (org-comments-filter--ids))))

(defun org-comments-filter-apply (comments state)
  "Return COMMENTS accepted by filter STATE."
  (seq-filter (lambda (comment)
		(org-comments-filter-include-p comment state))
	      comments))

(defun org-comments-panel-filter-show-resolved-p (state)
  "Return non-nil when STATE allows resolved comments."
  (org-comments-filter-value :show-resolved state))

(defun org-comments-panel-filter-resolved-p (comment)
  "Return non-nil when COMMENT has resolved status."
  (org-comments-record-resolved-p comment))

(defun org-comments-panel-filter-include-p (comment state)
  "Return non-nil when COMMENT should be visible under STATE."
  (org-comments-filter-include-p comment state))

(defun org-comments-panel-filter-apply (comments state)
  "Return COMMENTS accepted by filter STATE."
  (org-comments-filter-apply comments state))

(defun org-comments-panel-filter--active-summary (state)
  "Return a list of human-readable active filter summaries for STATE."
  (delq nil
	(mapcar
	 (lambda (id)
	   (let* ((spec (org-comments-filter--spec id))
		  (value (org-comments-filter-value id state))
		  (default (plist-get spec :default))
		  (label (or (plist-get spec :label) (substring (symbol-name id) 1))))
	     (unless (equal value default)
	       (format "%s %s" label (if value "on" "off")))))
	 (org-comments-filter--ids))))

(defun org-comments-panel-filter-summary (state)
  "Return a human-readable summary for filter STATE."
  (let ((active (org-comments-panel-filter--active-summary state)))
    (if active
	(string-join active ", ")
      "default")))

(defun org-comments-panel-filter--summary (state)
  "Return a human-readable summary for filter STATE."
  (org-comments-panel-filter-summary state))

(defun org-comments-panel-filter--reset-current ()
  "Reset filters in the current Org comments panel."
  (let ((source (org-comments-filter-current-source-buffer)))
    (org-comments-filter-set-state (org-comments-filter-default-state) source)
    (setq org-comments-panel-filter-state nil)
    (when (fboundp 'org-comments-refresh-current-ui)
      (org-comments-refresh-current-ui))
    (message "Comment filters reset: %s"
	     (org-comments-panel-filter--summary
	      (org-comments-filter-state source)))))

(defun org-comments-panel-filter--toggle-resolved-current ()
  "Toggle display of resolved comments in the current panel."
  (let* ((source (org-comments-filter-current-source-buffer))
	 (state (org-comments-filter-state source)))
    (org-comments-filter-set-state
     (org-comments-toggle-filter :show-resolved state)
     source)
    (when (fboundp 'org-comments-refresh-current-ui)
      (org-comments-refresh-current-ui))
    (message "Comment filters: %s"
	     (org-comments-panel-filter--summary
	      (org-comments-filter-state source)))))

(defun org-comments-panel-filter--status-current ()
  "Show current standalone panel filter status."
  (let ((source (org-comments-filter-current-source-buffer)))
    (message "Comment filters: %s"
	     (org-comments-panel-filter--summary
	      (org-comments-filter-state source)))))

;;;###autoload
(defun org-comments-filter-reset-current-ui ()
  "Reset filters in the current comments UI."
  (interactive)
  (funcall org-comments-current-filter-reset-function))

;;;###autoload
(defun org-comments-filter-toggle-resolved-current-ui ()
  "Toggle resolved comments in the current comments UI."
  (interactive)
  (funcall org-comments-current-filter-toggle-resolved-function))

;;;###autoload
(defun org-comments-filter-status-current-ui ()
  "Show filter status for the current comments UI."
  (interactive)
  (funcall org-comments-current-filter-status-function))

(defun org-comments-panel-filter-reset ()
  "Reset filters in the current Org comments panel."
  (interactive)
  (org-comments-filter-reset-current-ui))

(defun org-comments-panel-filter-toggle-resolved ()
  "Toggle display of resolved comments in the current panel."
  (interactive)
  (org-comments-filter-toggle-resolved-current-ui))

(defun org-comments-panel-filter--draft-p (comment)
  "Return non-nil when COMMENT has draft or outbound local state."
  (seq-some (lambda (flag)
	      (org-comments-local-state-p comment flag))
	    '(:local-only :draft :edited :pending-push :push-error)))

(defun org-comments-panel-filter--register-builtins ()
  "Register built-in Org comments filters."
  (setq org-comments-filter-registry nil)
  (org-comments-register-filter
   :show-resolved
   (lambda (comment value _state)
     (or value (not (org-comments-record-resolved-p comment))))
   :label "resolved"
   :default t)
  (org-comments-register-filter
   :show-missing
   (lambda (comment value _state)
     (or value (not (eq (plist-get comment :remote-state) 'missing))))
   :label "missing"
   :default t)
  (org-comments-register-filter
   :show-deleted
   (lambda (comment value _state)
     (or value
	 (and (not (eq (plist-get comment :remote-state) 'deleted))
	      (not (plist-get comment :archived)))))
   :label "deleted"
   :default nil)
  (org-comments-register-filter
   :mine
   (lambda (comment value _state)
     (or (not value) (org-comments-record-mine-p comment)))
   :label "mine"
   :default nil)
  (org-comments-register-filter
   :drafts
   (lambda (comment value _state)
     (or (not value) (org-comments-panel-filter--draft-p comment)))
   :label "drafts"
   :default nil)
  (org-comments-register-filter
   :actionable
   (lambda (comment value _state)
     (or (not value) (org-comments-record-actionable-p comment)))
   :label "actionable"
   :default nil))

(org-comments-panel-filter--register-builtins)

(provide 'org-comments-panel-filter)
;;; org-comments-panel-filter.el ends here
