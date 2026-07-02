;;; org-comments-ui-test.el --- UI adapter tests for org-comments -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the explicit UI extension boundary used by package commands.

;;; Code:

(require 'ert)
(require 'org-comments-ui)

(ert-deftest org-comments-ui-open-dispatches-to-registered-function ()
  "The open adapter calls `org-comments-ui-open-function' when configured."
  (let ((org-comments-ui-open-function (lambda () :opened)))
    (should (eq (org-comments-ui-open) :opened))))

(ert-deftest org-comments-ui-open-uses-fallback-when-unconfigured ()
  "The open adapter can fall back when no UI is registered."
  (let ((org-comments-ui-open-function nil))
    (should (eq (org-comments-ui-open (lambda () :fallback)) :fallback))))

(ert-deftest org-comments-ui-refresh-is-noop-when-unconfigured ()
  "Refreshing comments is optional for standalone package use."
  (let ((org-comments-ui-refresh-function nil))
    (should-not (org-comments-ui-refresh))))

(ert-deftest org-comments-ui-compose-reply-errors-when-unconfigured ()
  "Reply composition reports a clear error without a registered UI."
  (let ((org-comments-ui-compose-reply-function nil))
    (should-error (org-comments-ui-compose-reply) :type 'user-error)))

(ert-deftest org-comments-ui-open-comment-dispatches-to-registered-function ()
  "Specific inline comment opening uses the registered UI adapter."
  (let ((previous org-comments-ui-open-comment-function)
	called)
    (unwind-protect
	(progn
	  (setq org-comments-ui-open-comment-function
		(lambda (comment-id position)
		  (setq called (list comment-id position))))
	  (should (org-comments-ui-open-comment "comment-1" 42))
	  (should (equal called '("comment-1" 42))))
      (setq org-comments-ui-open-comment-function previous))))

(ert-deftest org-comments-ui-page-open-comment-dispatches-to-registered-function ()
  "Specific page comment opening uses the registered UI adapter."
  (let ((previous org-comments-ui-page-open-comment-function)
	called)
    (unwind-protect
	(progn
	  (setq org-comments-ui-page-open-comment-function
		(lambda (comment-id)
		  (setq called comment-id)))
	  (should (org-comments-ui-page-open-comment "page-1"))
	  (should (equal called "page-1")))
      (setq org-comments-ui-page-open-comment-function previous))))

(provide 'org-comments-ui-test)
;;; org-comments-ui-test.el ends here
