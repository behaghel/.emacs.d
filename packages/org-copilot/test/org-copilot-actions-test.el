;;; org-copilot-actions-test.el --- Action tests for org-copilot -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for accepting and dismissing durable Org Copilot comments.

;;; Code:

(require 'ert)
(require 'org)
(require 'org-copilot)
(require 'org-copilot-diff)
(require 'org-suggestions)

(ert-deftest org-copilot-accept-at-point-delegates-linked-suggestion ()
  "Accepting a linked comment row delegates source mutation to org-suggestions."
  (let* ((directory (make-temp-file "org-copilot-linked-accept" t))
	 (source-file (expand-file-name "draft.org" directory)))
    (unwind-protect
	(with-current-buffer (find-file-noselect source-file)
	  (erase-buffer)
	  (insert "* Intro\nCurrent body.\n")
	  (save-buffer)
	  (org-mode)
	  (let* ((source (current-buffer))
		 (thread (list :id "ai-thread-1"
			       :candidates
			       (list (list :id "ai-1" :status 'active
					   :hunks
					   (list (list :id "h1"
						       :kind 'section-replace
						       :section-title "Intro"
						       :replacement "New body.")))))))
	    (org-suggestions-write-sidecar source-file (list thread))
	    (with-temp-buffer
	      (setq context-panels-source-buffer source)
	      (insert "💬 Rewrite Intro ✏️ ai-1\n")
	      (add-text-properties
	       (point-min) (point-max)
	       '(context-panels-item
		 (:type comment :id "cmt-1" :suggestion-ids "ai-1")))
	      (goto-char (point-min))
	      (org-copilot-accept-at-point))
	    (should (equal (buffer-string) "* Intro\nNew body.\n"))
	    (let* ((loaded (car (org-suggestions-load-sidecar source-file)))
		   (candidate (car (plist-get loaded :candidates))))
	      (should (eq (plist-get candidate :status) 'accepted)))))
      (when-let* ((buffer (find-buffer-visiting source-file)))
	(kill-buffer buffer))
      (delete-directory directory t))))

(provide 'org-copilot-actions-test)
;;; org-copilot-actions-test.el ends here
