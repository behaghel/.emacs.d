;;; org-copilot-edit-safety-test.el --- Delayed proposal integration -*- lexical-binding: t; -*-

;;; Commentary:
;; Synthetic source documents and a controlled asynchronous transport exercise
;; request construction, response parsing, all three sidecars, preview and accept.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org-copilot)
(require 'org-copilot-gptel)

(defun org-copilot-safety--exercise (timing)
  "Exercise a delayed section proposal with human edit TIMING."
  (let* ((directory (make-temp-file "copilot-edit-safety" t))
	 (file (expand-file-name "draft.org" directory))
	 (org-copilot-gptel-defer-chat-postprocessing nil)
	 (org-copilot-auto-open-panels nil)
	 (org-copilot-gptel-enable-web-tools nil)
	 (org-suggestions--rollback-data nil)
	 source callback)
    (unwind-protect
	(save-window-excursion
	  (with-temp-file file (insert "* Intro\nOriginal.\n* Other\nOther text.\n"))
	  (setq source (find-file-noselect file))
	  (with-current-buffer source
	    (org-copilot-mode 1)
	    (org-copilot-chat--set-context source '(:type full-document))
	    (cl-letf (((symbol-function 'gptel-request)
		       (lambda (_prompt &rest args)
			 (setq callback (plist-get args :callback)))))
	      (org-copilot-gptel-chat (org-copilot-chat--request source "Rewrite Intro.")))
	    (when (eq timing 'during)
	      (goto-char (point-min)) (search-forward "Original.")
	      (replace-match "Human edit during inference." t t)))
	  (with-temp-buffer
	    (funcall callback
		     "{\"intent\":\"edit\",\"message\":\"Here is a rewrite.\",\"suggestion_threads\":[{\"id\":\"thread-1\",\"summary\":\"Rewrite Intro\",\"suggestions\":[{\"id\":\"safe-1\",\"hunks\":[{\"id\":\"h1\",\"kind\":\"section-replace\",\"section_title\":\"Intro\",\"replacement\":\"Proposed.\\n\"}]}]}]}"
		     '(:status success)))
	  (dolist (sidecar (list (org-copilot-sidecar-path file)
				 (org-comments-sidecar-path file)
				 (org-suggestions-sidecar-path file)))
	    (should (file-exists-p sidecar)))
	  (let* ((threads (org-suggestions-load-sidecar file))
		 (thread (car threads))
		 (candidate (car (plist-get thread :candidates))))
	    (org-suggestions-candidate-diff source thread candidate)
	    (with-current-buffer source
	      (pcase timing
		('after (goto-char (point-min)) (search-forward "Original.")
			(replace-match "Human edit after preview." t t))
		('unrelated (goto-char (point-max)) (insert "Unrelated human edit.\n"))))
	    (let ((before (with-current-buffer source (buffer-string))))
	      (if (memq timing '(during after))
		  (progn
		    (should-error (org-suggestions-accept-candidate-id source "safe-1")
				  :type 'user-error)
		    (should (equal before (with-current-buffer source (buffer-string))))
		    (should-not org-suggestions--rollback-data)
		    (should (string-match-p "CONFLICT" (org-suggestions-candidate-diff
							source thread candidate))))
		(let ((accepted (org-suggestions-accept-candidate-id source "safe-1")))
		  (with-current-buffer source
		    (should (string-match-p "Proposed" (buffer-string)))
		    (when (eq timing 'unrelated)
		      (should (string-match-p "Unrelated human edit" (buffer-string)))))
		  (org-suggestions-undo-accepted-candidate source accepted)
		  (should (equal before (with-current-buffer source (buffer-string)))))))))
      (dolist (buffer (buffer-list))
	(when (or (eq buffer source)
		  (and source (eq (buffer-local-value 'context-panels-source-buffer buffer) source))
		  (let ((name (buffer-file-name buffer)))
		    (and name (file-in-directory-p name directory))))
	  (with-current-buffer buffer (set-buffer-modified-p nil))
	  (kill-buffer buffer)))
      (delete-directory directory t))))

(ert-deftest org-copilot-safety-edit-during-inference ()
  "A delayed response cannot overwrite edits made during inference."
  (org-copilot-safety--exercise 'during))

(ert-deftest org-copilot-safety-edit-after-preview ()
  "Acceptance revalidates source changed after a valid preview."
  (org-copilot-safety--exercise 'after))

(ert-deftest org-copilot-safety-unchanged-and-unrelated-edits ()
  "Unchanged targets remain easy to accept, including with unrelated edits."
  (org-copilot-safety--exercise 'unchanged)
  (org-copilot-safety--exercise 'unrelated))

(provide 'org-copilot-edit-safety-test)
;;; org-copilot-edit-safety-test.el ends here
