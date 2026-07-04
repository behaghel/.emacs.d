;;; org-copilot-session-test.el --- Session tests for org-copilot -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the first org-copilot vertical slice: loadable package and
;; ephemeral in-memory AI comments.

;;; Code:

(require 'ert)
(require 'org)
(require 'org-copilot)

(ert-deftest org-copilot-core-loads-without-gptel ()
  "Core org-copilot loads without the optional gptel adapter."
  (should (featurep 'org-copilot))
  (should (featurep 'org-copilot-model))
  (should (featurep 'org-copilot-session))
  (should-not (featurep 'gptel)))

(ert-deftest org-copilot-session-stores-and-lists-comments ()
  "AI comments are stored in buffer-local session state."
  (with-temp-buffer
    (org-mode)
    (let ((comment (list :id "ai-1"
			 :type 'inline
			 :status 'active
			 :source-start 1
			 :source-end 1
			 :target-text ""
			 :body "Clarify this sentence.")))
      (should (equal (org-copilot-add-comment comment) comment))
      (should (equal (org-copilot-comments) (list comment)))))
  (with-temp-buffer
    (org-mode)
    (should-not (org-copilot-comments))))

(ert-deftest org-copilot-clear-session-removes-comments ()
  "Clearing the current session removes ephemeral AI comments."
  (with-temp-buffer
    (org-mode)
    (org-copilot-add-comment
     (list :id "ai-1"
	   :type 'scope
	   :status 'active
	   :body "Consider adding a conclusion."))
    (should (org-copilot-comments))
    (org-copilot-clear-session)
    (should-not (org-copilot-comments))))

(provide 'org-copilot-session-test)
;;; org-copilot-session-test.el ends here
