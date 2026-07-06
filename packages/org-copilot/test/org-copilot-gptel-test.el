;;; org-copilot-gptel-test.el --- gptel adapter tests for org-copilot -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the optional Org Copilot gptel adapter.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'org)
(require 'org-copilot)
(require 'org-copilot-gptel)

(ert-deftest org-copilot-gptel-loads-optionally ()
  "The gptel adapter file loads without requiring gptel eagerly."
  (should (featurep 'org-copilot-gptel)))

(ert-deftest org-copilot-gptel-enable-errors-when-gptel-is-missing ()
  "Enabling the gptel adapter fails clearly when gptel is unavailable."
  (let ((old-fboundp (symbol-function 'fboundp)))
    (cl-letf (((symbol-function 'fboundp)
	       (lambda (symbol)
		 (and (not (eq symbol 'gptel-request))
		      (funcall old-fboundp symbol)))))
      (should-error (org-copilot-gptel-enable) :type 'user-error))))

(ert-deftest org-copilot-gptel-enable-sets-adapters ()
  "Enabling the gptel adapter sets review and chat functions when gptel exists."
  (let ((org-copilot-review-function nil)
	(org-copilot-chat-function nil))
    (cl-letf (((symbol-function 'gptel-request)
	       (lambda (&rest _args) nil)))
      (org-copilot-gptel-enable)
      (should (eq org-copilot-review-function #'org-copilot-gptel-review))
      (should (eq org-copilot-chat-function #'org-copilot-gptel-chat)))))

(ert-deftest org-copilot-gptel-builds-review-prompt ()
  "Review prompts include configurable instructions, JSON schema, and source."
  (let ((prompt (org-copilot-gptel-review-prompt
		 (list :scope 'region
		       :buffer-name "draft.org"
		       :text "Alpha sentence."))))
    (should (string-match-p "JSON" prompt))
    (should (string-match-p "summary" prompt))
    (should (string-match-p "overall review" prompt))
    (should (string-match-p "comments" prompt))
    (should (string-match-p "anchored inline" prompt))
    (should (string-match-p "literal replacement text" prompt))
    (should (string-match-p "For every inline comment" prompt))
    (should (string-match-p "Omit suggestion only" prompt))
    (should (string-match-p "clarity" prompt))
    (should (string-match-p "Alpha sentence" prompt))))

(ert-deftest org-copilot-gptel-review-prompt-uses-custom-instructions ()
  "Review prompts include customized reviewer instructions."
  (let* ((org-copilot-gptel-review-instructions
	  "Custom reviewer policy: prioritize thesis strength.")
	 (prompt (org-copilot-gptel-review-prompt
		  (list :scope 'region
			:buffer-name "draft.org"
			:text "Alpha sentence."))))
    (should (string-match-p "Custom reviewer policy" prompt))
    (should (string-match-p "thesis strength" prompt))
    (should (string-match-p "JSON" prompt))))

(ert-deftest org-copilot-gptel-review-chat-summary-counts-comments-and-suggestions ()
  "Review chat summaries include local comment and suggestion counts."
  (let ((summary (org-copilot-gptel--review-chat-summary
		  (list (list :id "ai-1" :suggestion "Alpha.")
			(list :id "ai-2"))
		  "Overall review.")))
    (should (string-match-p "2 comments, 1 suggestion" summary))
    (should (string-match-p "Overall review" summary))))

(ert-deftest org-copilot-gptel-builds-chat-prompt ()
  "Chat prompts include user message, history, and focused comment context."
  (let ((prompt (org-copilot-gptel-chat-prompt
		 (list :message "Explain this"
		       :messages (list (list :role 'user
					     :content "Earlier question"))
		       :focus-comment-id "ai-1"))))
    (should (string-match-p "Explain this" prompt))
    (should (string-match-p "Earlier question" prompt))
    (should (string-match-p "ai-1" prompt))))

(ert-deftest org-copilot-gptel-full-document-chat-prompt-includes-source-content ()
  "Full-document chat prompts include source content, not focused comments."
  (let ((prompt (org-copilot-gptel-chat-prompt
		 (list :message "Explain this"
		       :messages nil
		       :source-content "* Heading\nBody."
		       :focus-comment-id nil))))
    (should (string-match-p "full-document chat" prompt))
    (should (string-match-p "\\* Heading" prompt))
    (should (string-match-p "Focused comment id: none" prompt))))

(ert-deftest org-copilot-gptel-chat-appends-callback-response ()
  "The gptel chat adapter appends assistant callback text to source chat state."
  (with-temp-buffer
    (org-mode)
    (org-copilot-add-chat-message 'pending "…")
    (cl-letf (((symbol-function 'gptel-request)
	       (lambda (_prompt &rest args)
		 (let ((callback (plist-get args :callback)))
		   (funcall callback "Assistant answer." (list :status 'success))))))
      (org-copilot-gptel-chat
       (list :source-buffer (current-buffer)
	     :buffer-name (buffer-name)
	     :message "Explain this"
	     :messages nil
	     :focus-comment-id nil))
      (let ((messages (org-copilot-chat-messages)))
	(should (= (length messages) 1))
	(should (eq (plist-get (car messages) :role) 'assistant))
	(should (equal (plist-get (car messages) :content) "Assistant answer."))))))

(ert-deftest org-copilot-gptel-chat-installs-structured-comments ()
  "Chat callback comments become normal Org Copilot review artifacts."
  (with-temp-buffer
    (org-mode)
    (insert "Alpha sentence.\n")
    (let ((org-copilot-chat-open-panel-on-comments nil))
      (cl-letf (((symbol-function 'gptel-request)
		 (lambda (_prompt &rest args)
		   (let ((callback (plist-get args :callback)))
		     (funcall callback
			      "{\"message\":\"I added one comment.\",\"comments\":[{\"body\":\"Tighten this.\",\"target_text\":\"Alpha sentence.\",\"suggestion\":\"Alpha.\"}]}"
			      (list :status 'success))))))
	(org-copilot-gptel-chat
	 (list :source-buffer (current-buffer)
	       :buffer-name (buffer-name)
	       :message "Find weak lines"
	       :messages nil
	       :chat-context '(:type full-document)
	       :focus-comment-id nil))
	(let ((comments (org-copilot-comments))
	      (message (car (org-copilot-chat-messages))))
	  (should (= (length comments) 1))
	  (should (equal (plist-get (car comments) :target-text)
			 "Alpha sentence."))
	  (should (equal (plist-get (car comments) :suggestion) "Alpha."))
	  (should (string-match-p "Installed 1 comment"
				  (plist-get message :content))))))))

(ert-deftest org-copilot-gptel-section-chat-keeps-context-id ()
  "Section chat callback messages remain visible in section context."
  (with-temp-buffer
    (org-mode)
    (insert "* Intro\nBody.\n")
    (let ((context-id "section:(Intro)"))
      (org-copilot-add-chat-message 'pending "…" nil context-id)
      (cl-letf (((symbol-function 'gptel-request)
		 (lambda (_prompt &rest args)
		   (let ((callback (plist-get args :callback)))
		     (funcall callback "Section answer."
			      (list :status "HTTP/2 200"))))))
	(org-copilot-gptel-chat
	 (list :source-buffer (current-buffer)
	       :buffer-name (buffer-name)
	       :message "Explain this section"
	       :messages nil
	       :context-id context-id
	       :focus-comment-id nil))
	(let ((messages (org-copilot-chat-messages)))
	  (should (= (length messages) 1))
	  (should (eq (plist-get (car messages) :role) 'assistant))
	  (should (equal (plist-get (car messages) :context-id) context-id))
	  (should (equal (plist-get (car messages) :content)
			 "Section answer.")))))))

(ert-deftest org-copilot-gptel-parses-chat-section-suggestion-fields ()
  "Chat JSON can carry section suggestion anchors."
  (let ((parsed (org-copilot-llm-parse-chat-response
		 "{\"message\":\"Try this.\",\"suggestion\":\"New body.\",\"section_title\":\"Intro\",\"section_path\":[\"Doc\",\"Intro\"]}")))
    (should (equal (plist-get parsed :message) "Try this."))
    (should (equal (plist-get parsed :suggestion) "New body."))
    (should (equal (plist-get parsed :section-title) "Intro"))
    (should (equal (plist-get parsed :section-path) '("Doc" "Intro")))))

(ert-deftest org-copilot-gptel-section-chat-installs-section-suggestion ()
  "Section chat suggestions become AI comments and preview buffers."
  (with-temp-buffer
    (org-mode)
    (insert "* Intro\nOriginal body.\n")
    (goto-char (point-min))
    (let* ((context (org-copilot-chat--section-context-at-point))
	   (context-id (org-copilot-chat--context-id context)))
      (cl-letf (((symbol-function 'gptel-request)
		 (lambda (_prompt &rest args)
		   (let ((callback (plist-get args :callback)))
		     (funcall callback
			      "{\"message\":\"I rewrote it.\",\"suggestion\":\"New body.\"}"
			      (list :status 'success))))))
	(org-copilot-gptel-chat
	 (list :source-buffer (current-buffer)
	       :buffer-name (buffer-name)
	       :message "Rewrite section"
	       :messages nil
	       :chat-context context
	       :context-id context-id
	       :focus-comment-id nil))
	(let ((comment (car (org-copilot-comments))))
	  (should comment)
	  (should (equal (plist-get comment :section-title) "Intro"))
	  (should (equal (plist-get comment :suggestion) "New body.\n"))
	  (should (get-buffer org-copilot-suggestion-buffer-name)))))))

(ert-deftest org-copilot-gptel-focused-chat-updates-suggestion ()
  "Focused chat can update the focused comment's suggestion from JSON response."
  (with-temp-buffer
    (org-mode)
    (org-copilot-add-comment
     (list :id "ai-1"
	   :body "Tighten."
	   :target-text "Alpha sentence."
	   :suggestion "Alpha."))
    (cl-letf (((symbol-function 'gptel-request)
	       (lambda (_prompt &rest args)
		 (let ((callback (plist-get args :callback)))
		   (funcall callback
			    "{\"message\":\"I made it more direct.\",\"suggestion\":\"Direct alpha.\"}"
			    (list :status 'success))))))
      (org-copilot-gptel-chat
       (list :source-buffer (current-buffer)
	     :buffer-name (buffer-name)
	     :message "Make it more direct"
	     :messages nil
	     :focus-comment-id "ai-1"))
      (let ((comment (org-copilot-find-comment "ai-1"))
	    (message (car (org-copilot-chat-messages))))
	(should (equal (plist-get comment :suggestion) "Direct alpha."))
	(should (equal (plist-get message :content) "I made it more direct."))))))

(ert-deftest org-copilot-gptel-review-installs-callback-comments ()
  "The gptel adapter installs parsed callback comments into the source session."
  (with-temp-buffer
    (org-mode)
    (insert "Alpha sentence.\n")
    (setq org-copilot-chat-focus-comment-id "ai-1")
    (cl-letf (((symbol-function 'gptel-request)
	       (lambda (_prompt &rest args)
		 (let ((callback (plist-get args :callback)))
		   (funcall callback
			    "{\"comments\":[{\"id\":\"ai-1\",\"body\":\"Tighten.\",\"target_text\":\"Alpha sentence.\"}]}"
			    (list :status 'success))))))
      (org-copilot-gptel-review
       (list :source-buffer (current-buffer)
	     :scope 'region
	     :buffer-name (buffer-name)
	     :text "Alpha sentence."))
      (let ((comments (org-copilot-comments)))
	(should (= (length comments) 1))
	(should (equal (plist-get (car comments) :id) "ai-1"))
	(should (equal (plist-get (car comments) :source-start) 1))
	(should-not org-copilot-chat-focus-comment-id)))))

(ert-deftest org-copilot-gptel-review-installs-summary-chat-message ()
  "Review summaries are installed as general Copilot chat messages."
  (with-temp-buffer
    (org-mode)
    (insert "Alpha sentence.\n")
    (cl-letf (((symbol-function 'gptel-request)
	       (lambda (_prompt &rest args)
		 (let ((callback (plist-get args :callback)))
		   (funcall callback
			    (concat "{\"summary\":{\"message\":\"Overall review.\","
				    "\"next_steps\":[\"Add evidence\"]},"
				    "\"comments\":[{\"id\":\"ai-1\","
				    "\"body\":\"Tighten.\","
				    "\"target_text\":\"Alpha sentence.\"}]}")
			    (list :status 'success))))))
      (org-copilot-gptel-review
       (list :source-buffer (current-buffer)
	     :scope 'region
	     :buffer-name (buffer-name)
	     :text "Alpha sentence."))
      (let ((messages (org-copilot-chat-messages)))
	(should (= (length messages) 1))
	(should (eq (plist-get (car messages) :role) 'assistant))
	(should-not (plist-get (car messages) :comment-id))
	(should (string-match-p "1 comment, 0 suggestions"
				(plist-get (car messages) :content)))
	(should (string-match-p "Overall review"
				(plist-get (car messages) :content)))
	(should (string-match-p "Add evidence"
				(plist-get (car messages) :content)))))))

(provide 'org-copilot-gptel-test)
;;; org-copilot-gptel-test.el ends here
