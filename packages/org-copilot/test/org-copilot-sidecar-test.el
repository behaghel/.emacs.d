;;; org-copilot-sidecar-test.el --- Sidecar tests for Org Copilot -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for durable Org Copilot transcript sidecars.

;;; Code:

(require 'ert)
(require 'org-copilot-chat)
(require 'org-copilot-sidecar)
(require 'org-copilot-session)

(ert-deftest org-copilot-sidecar-writes-and-loads-messages ()
  "Copilot sidecars persist and restore chat messages."
  (let* ((directory (make-temp-file "org-copilot-sidecar" t))
	 (source-file (expand-file-name "draft.org" directory)))
    (unwind-protect
	(progn
	  (write-region "* Draft\n" nil source-file nil 'silent)
	  (org-copilot-sidecar-append-message
	   source-file 'user "Rewrite this."
	   '("ORG_COPILOT_CONTEXT_ID" "section:Intro"))
	  (org-copilot-sidecar-append-message
	   source-file 'assistant "Done."
	   '("ORG_COPILOT_MODEL" "gpt-5.5"))
	  (let ((messages (org-copilot-sidecar-load-messages source-file)))
	    (should (= (length messages) 2))
	    (should (eq (plist-get (car messages) :role) 'user))
	    (should (equal (plist-get (car messages) :content) "Rewrite this."))
	    (should (equal (plist-get (car messages) :context-id) "section:Intro"))
	    (should (eq (plist-get (cadr messages) :role) 'assistant))
	    (should (equal (plist-get (cadr messages) :content) "Done."))))
      (delete-directory directory t))))

(ert-deftest org-copilot-chat-open-restores-sidecar-transcript ()
  "Opening chat restores persisted transcript into the source session."
  (let* ((directory (make-temp-file "org-copilot-chat-restore" t))
	 (source-file (expand-file-name "draft.org" directory)))
    (unwind-protect
	(with-current-buffer (find-file-noselect source-file)
	  (erase-buffer)
	  (insert "* Draft\n")
	  (save-buffer)
	  (org-mode)
	  (org-copilot-sidecar-append-message source-file 'user "Question?" nil)
	  (org-copilot-sidecar-append-message source-file 'assistant "Answer." nil)
	  (setq org-copilot--chat-messages nil)
	  (let ((buffer (org-copilot-chat--buffer (current-buffer))))
	    (should (= (length (org-copilot-chat-messages)) 2))
	    (with-current-buffer buffer
	      (goto-char (point-min))
	      (should (search-forward "Question?" nil t))
	      (should (search-forward "Answer." nil t)))))
      (when-let* ((buffer (find-buffer-visiting source-file)))
	(kill-buffer buffer))
      (when (get-buffer org-copilot-chat-buffer-name)
	(kill-buffer org-copilot-chat-buffer-name))
      (delete-directory directory t))))

(provide 'org-copilot-sidecar-test)
;;; org-copilot-sidecar-test.el ends here
