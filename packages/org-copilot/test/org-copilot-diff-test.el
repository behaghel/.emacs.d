;;; org-copilot-diff-test.el --- Diff tests for org-copilot -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for Org Copilot durable suggestion diff preview buffers.

;;; Code:

(require 'ert)
(require 'org)
(require 'org-copilot)
(require 'org-copilot-diff)
(require 'org-suggestions)

(defun org-copilot-diff-test--with-durable-suggestion (callback)
  "Call CALLBACK with source buffer and linked durable comment."
  (let* ((directory (make-temp-file "org-copilot-diff" t))
	 (source-file (expand-file-name "draft.org" directory)))
    (unwind-protect
	(with-current-buffer (find-file-noselect source-file)
	  (erase-buffer)
	  (insert "Alpha sentence.\n")
	  (save-buffer)
	  (org-mode)
	  (let ((thread (list :id "ai-thread-1"
			      :candidates
			      (list (list :id "ai-1" :status 'active
					  :hunks
					  (list (list :id "h1"
						      :kind 'replace
						      :original "Alpha sentence."
						      :replacement "Alpha."))))))
		(comment (list :id "cmt-1"
			       :source-file source-file
			       :source-start (point-min)
			       :source-end (line-end-position)
			       :target-text "Alpha sentence."
			       :body "Tighten wording."
			       :suggestion-thread-id "ai-thread-1"
			       :suggestion-ids "ai-1"
			       :status 'active)))
	    (org-suggestions-write-sidecar source-file (list thread))
	    (funcall callback (current-buffer) comment)))
      (when-let* ((buffer (find-buffer-visiting source-file)))
	(kill-buffer buffer))
      (delete-directory directory t))))

(ert-deftest org-copilot-diff-mode-defines-action-keys ()
  "Org Copilot diff mode exposes accept and close keys."
  (should (eq (lookup-key org-copilot-diff-mode-map (kbd "a"))
	      #'org-copilot-accept-at-point))
  (should (eq (lookup-key org-copilot-diff-mode-map (kbd "q"))
	      #'org-copilot-close-diff)))

(ert-deftest org-copilot-diff-buffer-is-read-only ()
  "Diff preview buffers are read-only."
  (org-copilot-diff-test--with-durable-suggestion
   (lambda (source comment)
     (with-current-buffer (org-copilot-diff-open source comment)
       (should buffer-read-only)))))

(ert-deftest org-copilot-diff-buffer-shows-old-and-new-text ()
  "Diff preview buffers show durable original text and proposed replacement."
  (org-copilot-diff-test--with-durable-suggestion
   (lambda (source comment)
     (with-current-buffer (org-copilot-diff-open source comment)
       (should (string-match-p "^-Alpha sentence\." (buffer-string)))
       (should (string-match-p "^+Alpha\." (buffer-string)))))))

(ert-deftest org-copilot-diff-buffer-is-context-panel-associated ()
  "Diff preview buffers keep source association for context-panel follow logic."
  (org-copilot-diff-test--with-durable-suggestion
   (lambda (source comment)
     (with-current-buffer (org-copilot-diff-open source comment)
       (should (eq org-context-panel-source-buffer source))
       (should (eq org-context-panel-view-id 'copilot-diff))))))

(ert-deftest org-copilot-diff-rejects-legacy-local-suggestion ()
  "Diff previews reject retired comment-local suggestions."
  (with-temp-buffer
    (org-mode)
    (let ((comment (list :id "ai-1"
			 :target-text "Alpha sentence."
			 :suggestion "Alpha."
			 :status 'active)))
      (should-error (org-copilot-diff-open (current-buffer) comment)
		    :type 'user-error))))

(ert-deftest org-copilot-view-diff-errors-without-suggestion ()
  "Viewing a diff errors when the AI comment has no durable suggestion."
  (with-temp-buffer
    (org-copilot-panel-mode)
    (let ((inhibit-read-only t)
	  (comment (list :id "ai-1"
			 :body "Clarify this."
			 :status 'active)))
      (insert "AI [active] Clarify this.\n")
      (add-text-properties (point-min) (point-max)
			   `(org-context-panel-item ,comment))
      (goto-char (point-min))
      (should-error (org-copilot-view-diff-at-point) :type 'user-error))))

(provide 'org-copilot-diff-test)
;;; org-copilot-diff-test.el ends here
