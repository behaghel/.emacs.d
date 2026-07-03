;;; org-confluence-comments-import-test.el --- Confluence comments import tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for Confluence comment import reporting.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'org-comments-core)
(require 'org-confluence-comments-import)

(ert-deftest org-confluence-comments-import-generic-report-normalizes-counts ()
  "Confluence import reports normalize to provider-neutral counters."
  (should (equal (org-confluence-comments-import-generic-report
		  '(:imported 3
			      :imported-root-ids ("c-2" "c-1")
			      :imported-reply-ids ("r-1")
			      :marked-missing 2
			      :present-again 1
			      :remote-resolved 1
			      :remote-reopened 1))
		 '(:provider "Confluence"
			     :added 2
			     :added-replies 1
			     :updated 0
			     :remote-resolved 1
			     :remote-reopened 1
			     :remote-missing 2
			     :remote-present 1
			     :preserved-local t))))

(ert-deftest org-confluence-comments-import-report-message-uses-generic-summary ()
  "Confluence import messages use the generic org-comments summary format."
  (let (message-text)
    (cl-letf (((symbol-function 'message)
	       (lambda (format-string &rest args)
		 (setq message-text (apply #'format format-string args)))))
      (org-confluence-comments-import-report-message
       '(:imported-root-ids ("c-1")
			    :imported-reply-ids ("r-1")
			    :marked-missing 1)
       "legacy fallback"))
    (should (equal message-text
		   "Confluence comments: added 1, updated 0, added replies 1, remote missing 1; local content preserved"))))

(ert-deftest org-confluence-comments-import-report-keeps-todo-detail-buffer ()
  "Confluence import keeps detailed reports for TODO-impacting changes."
  (when-let* ((buffer (get-buffer "*Confluence Comment Sync Report*")))
    (kill-buffer buffer))
  (cl-letf (((symbol-function 'message) (lambda (&rest _args) nil)))
    (org-confluence-comments-import-report-message
     '(:todo-resolved ("c-1") :todo-missing ("c-2"))
     "legacy fallback"))
  (with-current-buffer "*Confluence Comment Sync Report*"
    (goto-char (point-min))
    (should (search-forward "TODO closed by remote resolution" nil t))
    (should (search-forward "c-1" nil t))
    (should (search-forward "TODO remote-missing" nil t))
    (should (search-forward "c-2" nil t))))

(provide 'org-confluence-comments-import-test)
;;; org-confluence-comments-import-test.el ends here
