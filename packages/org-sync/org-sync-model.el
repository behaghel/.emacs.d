;;; org-sync-model.el --- Org sync status model -*- lexical-binding: t; -*-

;;; Commentary:
;; Minimal provider-neutral domain ref and status helpers.

;;; Code:

(require 'subr-x)

(defconst org-sync-domains '(content comments)
  "Domain keys tracked by Org sync v1.")

(defun org-sync--hash-object (object)
  "Return a stable hash string for OBJECT."
  (secure-hash 'sha1 (prin1-to-string object)))

(defun org-sync-content-local-ref (&optional source-buffer)
  "Return current local content ref for SOURCE-BUFFER."
  (with-current-buffer (or source-buffer (current-buffer))
    (list :hash (secure-hash 'sha1 (buffer-substring-no-properties
				    (point-min) (point-max))))))

(defun org-sync-comments-local-ref (&optional _source-buffer)
  "Return current local comments ref.
Iteration 2 tracks the empty normalized comment set; later slices replace this
with provider-filtered `org-comments' records."
  (list :hash (org-sync--hash-object nil) :count 0))

(defun org-sync-local-ref (domain &optional source-buffer)
  "Return current local ref for DOMAIN in SOURCE-BUFFER."
  (pcase domain
    ('content (org-sync-content-local-ref source-buffer))
    ('comments (org-sync-comments-local-ref source-buffer))
    (_ (user-error "Unknown org-sync domain: %s" domain))))

(defun org-sync-domain-entry (tracking domain)
  "Return TRACKING entry plist for DOMAIN."
  (alist-get domain (plist-get tracking :domains)))

(defun org-sync-domain-status (entry current-local-ref)
  "Return status for domain ENTRY against CURRENT-LOCAL-REF."
  (let ((base-local (plist-get entry :base-local-ref))
	(base-remote (plist-get entry :base-remote-ref))
	(fetched-remote (plist-get entry :fetched-remote-ref)))
    (cond
     ((or (not base-local) (not base-remote) (not fetched-remote)) 'unknown)
     ((and (equal current-local-ref base-local)
	   (equal fetched-remote base-remote))
      'clean)
     ((equal fetched-remote base-remote) 'ahead)
     ((equal current-local-ref base-local) 'behind)
     (t 'diverged))))

(defun org-sync-statuses (tracking &optional source-buffer)
  "Return domain status alist for TRACKING and SOURCE-BUFFER."
  (mapcar (lambda (domain)
	    (let ((entry (org-sync-domain-entry tracking domain)))
	      (cons domain
		    (org-sync-domain-status
		     entry (org-sync-local-ref domain source-buffer)))))
	  org-sync-domains))

(defun org-sync-baseline-tracking (tracking source-buffer)
  "Return TRACKING with base refs recorded from SOURCE-BUFFER."
  (let ((copy (copy-sequence tracking))
	domains)
    (dolist (domain org-sync-domains)
      (let* ((entry (copy-sequence (or (org-sync-domain-entry tracking domain) nil)))
	     (remote-ref (plist-get entry :fetched-remote-ref)))
	(unless remote-ref
	  (user-error "Cannot baseline %s without fetched remote ref" domain))
	(setq entry (plist-put entry :base-local-ref
			       (org-sync-local-ref domain source-buffer)))
	(setq entry (plist-put entry :base-remote-ref remote-ref))
	(push (cons domain entry) domains)))
    (plist-put copy :domains (nreverse domains))))

(provide 'org-sync-model)
;;; org-sync-model.el ends here
