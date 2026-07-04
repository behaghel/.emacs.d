;;; org-copilot-llm-test.el --- LLM protocol tests for org-copilot -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for Org Copilot review request construction and fake adapter flow.

;;; Code:

(require 'ert)
(require 'org)
(require 'org-copilot)
(require 'org-copilot-llm)

(ert-deftest org-copilot-review-dwim-prefers-region ()
  "Review DWIM sends the active region when a region is active."
  (with-temp-buffer
    (org-mode)
    (insert "Alpha sentence.\nBeta sentence.\n")
    (let ((org-copilot-review-function
	   (lambda (request)
	     (should (eq (plist-get request :scope) 'region))
	     (should (equal (plist-get request :text) "Alpha sentence."))
	     nil)))
      (goto-char (point-min))
      (push-mark (+ (point-min) (length "Alpha sentence.")) t t)
      (setq mark-active t)
      (org-copilot-review-dwim))))

(ert-deftest org-copilot-review-dwim-falls-back-to-subtree ()
  "Review DWIM sends the current subtree when no region is active."
  (with-temp-buffer
    (org-mode)
    (insert "* Heading\nBody text.\n* Next\nOther text.\n")
    (goto-char (point-min))
    (let ((org-copilot-review-function
	   (lambda (request)
	     (should (eq (plist-get request :scope) 'subtree))
	     (should (string-match-p "Heading" (plist-get request :text)))
	     (should (string-match-p "Body text" (plist-get request :text)))
	     (should-not (string-match-p "Next" (plist-get request :text)))
	     nil)))
      (org-copilot-review-dwim))))

(ert-deftest org-copilot-review-dwim-errors-without-adapter ()
  "Review DWIM fails clearly when no LLM adapter is configured."
  (with-temp-buffer
    (org-mode)
    (insert "* Heading\nBody text.\n")
    (let ((org-copilot-review-function nil))
      (should-error (org-copilot-review-dwim) :type 'user-error))))

(ert-deftest org-copilot-review-dwim-installs-normalized-comments ()
  "Review DWIM installs normalized AI comments returned by the adapter."
  (with-temp-buffer
    (org-mode)
    (insert "* Heading\nAlpha sentence.\n")
    (let ((org-copilot-review-function
	   (lambda (_request)
	     (list (list :id "ai-1"
			 :type 'inline
			 :source-start 11
			 :source-end 26
			 :target-text "Alpha sentence."
			 :body "Tighten this."
			 :suggestion "Alpha.")))))
      (org-copilot-review-dwim)
      (let ((comments (org-copilot-comments)))
	(should (= (length comments) 1))
	(should (equal (plist-get (car comments) :id) "ai-1"))
	(should (eq (plist-get (car comments) :status) 'active))))))

(provide 'org-copilot-llm-test)
;;; org-copilot-llm-test.el ends here
