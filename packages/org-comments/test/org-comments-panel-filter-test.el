;;; org-comments-panel-filter-test.el --- Panel filter tests for org-comments -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for standalone Org comments panel filters.

;;; Code:

(require 'ert)
(require 'org-comments-panel-actions)
(require 'org-comments-panel-filter)

(ert-deftest org-comments-panel-filter-shows-all-by-default ()
  "Default filter state keeps open and resolved comments visible."
  (should (org-comments-panel-filter-include-p '(:status "OPEN") nil))
  (should (org-comments-panel-filter-include-p '(:status "RESOLVED") nil)))

(ert-deftest org-comments-panel-filter-can-hide-resolved-comments ()
  "Filter state can hide resolved comments."
  (let ((state '(:show-resolved nil)))
    (should (org-comments-panel-filter-include-p '(:status "OPEN") state))
    (should-not (org-comments-panel-filter-include-p '(:status "RESOLVED") state))))

(ert-deftest org-comments-panel-filter-apply-filters-comment-list ()
  "Applying filters removes comments rejected by state."
  (should (equal (org-comments-panel-filter-apply
		  '((:id "open" :status "OPEN")
		    (:id "resolved" :status "RESOLVED"))
		  '(:show-resolved nil))
		 '((:id "open" :status "OPEN")))))

(ert-deftest org-comments-panel-filter-registry-exposes-predicates ()
  "Registered filters expose public predicates."
  (should (functionp (org-comments-filter-predicate :show-resolved))))

(ert-deftest org-comments-panel-filter-state-is-source-buffer-scoped ()
  "Filter state is stored on source buffers rather than panel buffers."
  (let ((source-a (generate-new-buffer "source-a"))
	(source-b (generate-new-buffer "source-b")))
    (unwind-protect
	(progn
	  (org-comments-filter-set-state '(:show-resolved nil) source-a)
	  (should-not (org-comments-filter-value
		       :show-resolved
		       (org-comments-filter-state source-a)))
	  (should (org-comments-filter-value
		   :show-resolved
		   (org-comments-filter-state source-b))))
      (kill-buffer source-a)
      (kill-buffer source-b))))

(ert-deftest org-comments-panel-filter-registry-supports-custom-filters ()
  "Custom filters use the same public registry path as built-ins."
  (let ((org-comments-filter-registry org-comments-filter-registry))
    (org-comments-register-filter
     :only-important
     (lambda (comment value _state)
       (or (not value) (plist-get comment :important)))
     :default nil
     :label "important")
    (should (equal (org-comments-filter-apply
		    '((:id "keep" :important t)
		      (:id "drop"))
		    '(:only-important t))
		   '((:id "keep" :important t))))))

(ert-deftest org-comments-panel-filter-can-hide-missing-comments ()
  "Missing remote records can be hidden through generic filters."
  (should-not (org-comments-filter-include-p
	       '(:id "missing" :remote-state missing)
	       '(:show-missing nil)))
  (should (org-comments-filter-include-p
	   '(:id "present" :remote-state present)
	   '(:show-missing nil))))

(ert-deftest org-comments-panel-filter-hides-deleted-by-default ()
  "Deleted and archived records are hidden unless explicitly shown."
  (should-not (org-comments-filter-include-p
	       '(:id "deleted" :remote-state deleted)
	       nil))
  (should-not (org-comments-filter-include-p
	       '(:id "archived" :archived t)
	       nil))
  (should (org-comments-filter-include-p
	   '(:id "deleted" :remote-state deleted)
	   '(:show-deleted t))))

(ert-deftest org-comments-panel-filter-mine-uses-identity-seam ()
  "Mine filter uses backend-neutral identity resolution."
  (let ((org-comments-current-user-id-function (lambda () "person-1"))
	(org-comments-resolve-person-function
	 (lambda (backend raw-id)
	   (when (and (eq backend 'confluence)
		      (equal raw-id "abc"))
	     '(:id "person-1")))))
    (should (org-comments-filter-include-p
	     '(:backend confluence :remote-author-id "abc")
	     '(:mine t)))
    (should-not (org-comments-filter-include-p
		 '(:backend confluence :remote-author-id "def")
		 '(:mine t)))))

(ert-deftest org-comments-panel-filter-drafts-uses-local-state-flags ()
  "Draft filter includes local-only, edited, pending, and error records."
  (should (org-comments-filter-include-p
	   '(:id "draft" :local-state (:draft))
	   '(:drafts t)))
  (should (org-comments-filter-include-p
	   '(:id "pending" :local-state (:pending-push))
	   '(:drafts t)))
  (should-not (org-comments-filter-include-p
	       '(:id "clean" :local-state nil)
	       '(:drafts t))))

(ert-deftest org-comments-panel-filter-actionable-uses-derived-state ()
  "Actionable filter includes explicit and derived actionable records."
  (should (org-comments-filter-include-p
	   '(:id "explicit" :actionable t)
	   '(:actionable t)))
  (should (org-comments-filter-include-p
	   '(:id "conflict" :sync-state conflict)
	   '(:actionable t)))
  (should (org-comments-filter-include-p
	   '(:id "push" :local-state (:push-error))
	   '(:actionable t)))
  (should-not (org-comments-filter-include-p
	       '(:id "clean" :sync-state clean :remote-state present)
	       '(:actionable t))))

(ert-deftest org-comments-panel-filter-source-uses-current-ui-adapter ()
  "Filter operations resolve source buffers through current-UI adapters."
  (let ((source (generate-new-buffer "org-comments-filter-source")))
    (unwind-protect
	(with-temp-buffer
	  (setq-local org-comments-current-source-buffer-function
		      (lambda () source))
	  (should (eq (org-comments-filter-current-source-buffer) source)))
      (kill-buffer source))))

(ert-deftest org-comments-panel-filter-toggle-refreshes-current-ui ()
  "Toggling filters refreshes through the current-UI refresh adapter."
  (let ((source (generate-new-buffer "org-comments-filter-source"))
	refreshed)
    (unwind-protect
	(with-temp-buffer
	  (setq-local org-comments-current-source-buffer-function
		      (lambda () source))
	  (setq-local org-comments-current-refresh-function
		      (lambda () (setq refreshed t)))
	  (org-comments-panel-filter--toggle-resolved-current)
	  (should refreshed)
	  (should-not (org-comments-filter-value
		       :show-resolved
		       (org-comments-filter-state source))))
      (kill-buffer source))))

(provide 'org-comments-panel-filter-test)
;;; org-comments-panel-filter-test.el ends here
