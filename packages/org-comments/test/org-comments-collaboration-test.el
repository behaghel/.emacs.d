;;; org-comments-collaboration-test.el --- Collaboration model tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for backend-neutral collaboration state normalization.

;;; Code:

(require 'ert)
(require 'org-comments-collaboration)

(ert-deftest org-comments-collaboration-normalizes-remote-and-sync-states ()
  "Records normalize remote and sync states from strings or symbols."
  (let ((record (org-comments-normalize-record
		 '(:id "c1"
		       :remote-state "MISSING"
		       :sync-state error))))
    (should (eq (plist-get record :remote-state) 'missing))
    (should (eq (plist-get record :sync-state) 'error))))

(ert-deftest org-comments-collaboration-normalizes-local-state-flags ()
  "Records normalize local state to known keyword flags."
  (let ((record (org-comments-normalize-record
		 '(:id "c1"
		       :local-state "local-only edited unknown pending-push"))))
    (should (equal (plist-get record :local-state)
		   '(:local-only :edited :pending-push)))
    (should (org-comments-local-state-p record :edited))
    (should-not (org-comments-local-state-p record :draft))))

(ert-deftest org-comments-collaboration-resolved-prefers-explicit-value ()
  "Explicit resolved state wins over local status."
  (let ((record (org-comments-normalize-record
		 '(:id "c1" :status "RESOLVED" :resolved nil))))
    (should-not (plist-get record :resolved))))

(ert-deftest org-comments-collaboration-resolved-falls-back-to-status ()
  "Records derive resolved state from Org status when needed."
  (should (plist-get (org-comments-normalize-record
		      '(:id "c1" :status "RESOLVED"))
		     :resolved))
  (should-not (plist-get (org-comments-normalize-record
			  '(:id "c2" :status "OPEN"))
			 :resolved)))

(ert-deftest org-comments-collaboration-actionable-is-explicit-or-derived ()
  "Actionability is true when explicit or derived from attention states."
  (should (plist-get (org-comments-normalize-record
		      '(:id "explicit" :actionable t))
		     :actionable))
  (should (plist-get (org-comments-normalize-record
		      '(:id "conflict" :sync-state conflict))
		     :actionable))
  (should (plist-get (org-comments-normalize-record
		      '(:id "missing" :remote-state missing))
		     :actionable))
  (should (plist-get (org-comments-normalize-record
		      '(:id "pending" :local-state (:pending-push)))
		     :actionable))
  (should-not (plist-get (org-comments-normalize-record
			  '(:id "clean" :sync-state clean :remote-state present))
			 :actionable)))

(ert-deftest org-comments-collaboration-mine-uses-identity-resolver ()
  "Mine detection resolves backend raw author ids to people."
  (let ((org-comments-current-user-id-function (lambda () "person-1"))
	(org-comments-resolve-person-function
	 (lambda (backend raw-id)
	   (when (and (eq backend 'confluence)
		      (equal raw-id "abc"))
	     '(:id "person-1" :name "Alice")))))
    (should (org-comments-record-mine-p
	     '(:backend confluence :remote-author-id "abc")))))

(ert-deftest org-comments-collaboration-mine-honors-me-flag ()
  "Mine detection accepts resolver-provided me flags."
  (let ((org-comments-current-user-id-function #'ignore)
	(org-comments-resolve-person-function
	 (lambda (_backend _raw-id)
	   '(:id "person-2" :name "Alice" :me t))))
    (should (org-comments-record-mine-p
	     '(:backend google-docs :remote-author-id "people/1")))))

(ert-deftest org-comments-collaboration-mine-falls-back-to-local-author ()
  "Local-only mine detection can use the local author field."
  (let ((org-comments-current-user-id-function (lambda () "Alice"))
	(org-comments-resolve-person-function #'ignore))
    (should (org-comments-record-mine-p '(:backend org :author "Alice")))
    (should-not (org-comments-record-mine-p '(:backend org :author "Bob")))))

(ert-deftest org-comments-collaboration-preserves-unknown-fields ()
  "Normalization preserves provider-specific or future plist fields."
  (let ((record (org-comments-normalize-record
		 '(:id "c1" :provider-extra "kept" :status "OPEN"))))
    (should (equal (plist-get record :provider-extra) "kept"))))

(provide 'org-comments-collaboration-test)
;;; org-comments-collaboration-test.el ends here
