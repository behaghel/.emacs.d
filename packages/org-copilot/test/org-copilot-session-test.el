;;; org-copilot-session-test.el --- Session tests for org-copilot -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the first org-copilot vertical slice: loadable package and
;; ephemeral in-memory AI comments.

;;; Code:

(require 'ert)
(require 'org)
(require 'org-copilot)
(require 'org-comments-store)

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

(ert-deftest org-copilot-clear-session-archives-durable-artifacts ()
  "Clearing archives current-session Copilot sidecar artifacts by default."
  (let* ((directory (make-temp-file "org-copilot-clear" t))
	 (source-file (expand-file-name "draft.org" directory)))
    (unwind-protect
	(with-current-buffer (find-file-noselect source-file)
	  (erase-buffer)
	  (insert "* Intro\nBody.\n")
	  (save-buffer)
	  (org-mode)
	  (org-copilot-add-chat-message 'user "Hello")
	  (org-comments-append-to-sidecar
	   (list :id "c1" :provider "org-copilot" :org-copilot-session-id "default"
		 :source-file source-file :body "Comment" :target-text "Body."
		 :target-start 9 :target-end 14)
	   (org-comments-sidecar-path source-file))
	  (org-suggestions-write-sidecar
	   source-file
	   (list (list :id "t1" :provider "org-copilot" :session-id "default"
		       :candidates nil)))
	  (org-copilot-clear-session)
	  (should-not (org-copilot-chat-messages))
	  (should-not (org-copilot-sidecar-load-messages source-file))
	  (should-not (org-comments-collect (current-buffer) t))
	  (should-not (org-suggestions-load-sidecar source-file))
	  (should (file-exists-p (concat (org-copilot-sidecar-path source-file) "_archive")))
	  (should (file-exists-p (concat (org-comments-sidecar-path source-file) "_archive")))
	  (should (file-exists-p (concat (org-suggestions-sidecar-path source-file) "_archive"))))
      (when-let* ((buffer (find-buffer-visiting source-file)))
	(kill-buffer buffer))
      (delete-directory directory t))))

(ert-deftest org-copilot-clear-session-prefix-preserves-durable-artifacts ()
  "Clearing with prefix preserves durable artifacts and clears UI state only."
  (let* ((directory (make-temp-file "org-copilot-clear-preserve" t))
	 (source-file (expand-file-name "draft.org" directory)))
    (unwind-protect
	(with-current-buffer (find-file-noselect source-file)
	  (erase-buffer)
	  (insert "* Intro\nBody.\n")
	  (save-buffer)
	  (org-mode)
	  (org-copilot-add-chat-message 'user "Hello")
	  (org-copilot-clear-session t)
	  (should-not (org-copilot-chat-messages))
	  (should (org-copilot-sidecar-load-messages source-file)))
      (when-let* ((buffer (find-buffer-visiting source-file)))
	(kill-buffer buffer))
      (delete-directory directory t))))

(ert-deftest org-copilot-erase-session-hard-deletes-durable-artifacts ()
  "Erasing deletes current-session Copilot sidecar artifacts."
  (let* ((directory (make-temp-file "org-copilot-erase" t))
	 (source-file (expand-file-name "draft.org" directory)))
    (unwind-protect
	(with-current-buffer (find-file-noselect source-file)
	  (erase-buffer)
	  (insert "* Intro\nBody.\n")
	  (save-buffer)
	  (org-mode)
	  (org-copilot-add-chat-message 'user "Hello")
	  (org-comments-append-to-sidecar
	   (list :id "c1" :provider "org-copilot" :org-copilot-session-id "default"
		 :source-file source-file :body "Comment" :target-text "Body."
		 :target-start 9 :target-end 14)
	   (org-comments-sidecar-path source-file))
	  (org-suggestions-write-sidecar
	   source-file
	   (list (list :id "t1" :provider "org-copilot" :session-id "default"
		       :candidates nil)))
	  (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
	    (org-copilot-erase-session))
	  (should-not (org-copilot-sidecar-load-messages source-file))
	  (should-not (org-comments-collect (current-buffer) t))
	  (should-not (org-suggestions-load-sidecar source-file))
	  (should-not (file-exists-p (concat (org-copilot-sidecar-path source-file) "_archive"))))
      (when-let* ((buffer (find-buffer-visiting source-file)))
	(kill-buffer buffer))
      (delete-directory directory t))))
