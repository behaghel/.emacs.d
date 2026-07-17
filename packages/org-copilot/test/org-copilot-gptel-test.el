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

(ert-deftest org-copilot-gptel-use-openai-oauth-sets-copilot-backend ()
  "OpenAI OAuth setup sets both global and Org Copilot gptel backends."
  (let ((gptel-model nil)
	(gptel-backend nil)
	(org-copilot-gptel-model nil)
	(org-copilot-gptel-backend nil)
	(org-copilot-review-function nil)
	(org-copilot-chat-function nil))
    (cl-letf (((symbol-function 'require)
	       (lambda (feature &optional _filename _noerror)
		 (or (eq feature 'gptel-openai-oauth)
		     (require feature))))
	      ((symbol-function 'gptel-make-openai-oauth)
	       (lambda (name) (list :oauth-backend name)))
	      ((symbol-function 'gptel-request)
	       (lambda (&rest _args) nil)))
      (org-copilot-gptel-use-openai-oauth 'gpt-5.5)
      (should (eq gptel-model 'gpt-5.5))
      (should (eq org-copilot-gptel-model 'gpt-5.5))
      (should (equal gptel-backend '(:oauth-backend "OpenAI-sub")))
      (should (equal org-copilot-gptel-backend
		     '(:oauth-backend "OpenAI-sub")))
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
    (should (string-match-p "insertion" prompt))
    (should (string-match-p "anchor_text" prompt))
    (should (string-match-p "placement" prompt))
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

(ert-deftest org-copilot-gptel-chat-prompt-hardens-references-and-suggestions ()
  "Chat prompts require precise references and replacement-only suggestions."
  (let ((prompt (org-copilot-gptel-chat-prompt
		 (list :message "donne moi des références précises"
		       :messages nil
		       :source-content "Draft"
		       :focus-comment-id nil))))
    (should (string-match-p "author, title, year" prompt))
    (should (string-match-p "anchor_text" prompt))
    (should (string-match-p "type (`inline', `insertion', or `scope')" prompt))
    (should (string-match-p "never put advice" prompt))
    (should (string-match-p "reference lists.*message" prompt))))

(ert-deftest org-copilot-gptel-chat-prompt-announces-web-tools ()
  "Chat prompts tell the model when live web tools are enabled."
  (let ((prompt (org-copilot-gptel-chat-prompt
		 (list :message "donne moi des références précises"
		       :messages nil
		       :source-content "Draft"
		       :web-tools-enabled t
		       :focus-comment-id nil))))
    (should (string-match-p "Live web tools are available" prompt))
    (should (string-match-p "Use WebSearch first" prompt))))

(ert-deftest org-copilot-gptel-chat-prompt-announces-missing-web-tools ()
  "Chat prompts are honest when live web tools are unavailable."
  (let ((prompt (org-copilot-gptel-chat-prompt
		 (list :message "donne moi des références précises"
		       :messages nil
		       :source-content "Draft"
		       :focus-comment-id nil))))
    (should (string-match-p "No live web search tool" prompt))))

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

(ert-deftest org-copilot-gptel-chat-accumulates-streaming-oauth-response ()
  "The gptel chat adapter requests and accumulates streaming OAuth responses."
  (with-temp-buffer
    (org-mode)
    (let (captured-stream)
      (cl-letf (((symbol-function 'gptel-openai-oauth-p)
		 (lambda (_backend) t))
		((symbol-function 'gptel-request)
		 (lambda (_prompt &rest args)
		   (setq captured-stream (plist-get args :stream))
		   (let ((callback (plist-get args :callback)))
		     (funcall callback "{\"message\":" (list :status 'streaming))
		     (funcall callback "\"Assistant answer.\"}" (list :status 'streaming))
		     (funcall callback t (list :status 'success))))))
	(org-copilot-gptel-chat
	 (list :source-buffer (current-buffer)
	       :buffer-name (buffer-name)
	       :message "Explain this"
	       :messages nil
	       :focus-comment-id nil))
	(should captured-stream)
	(should (equal (plist-get (car (org-copilot-chat-messages)) :content)
		       "Assistant answer."))))))

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

(ert-deftest org-copilot-gptel-available-web-tools-loads-provider ()
  "Available web tools attempt to load optional gptel-agent tools."
  (let (loaded)
    (cl-letf (((symbol-function 'require)
	       (lambda (feature &optional _filename _noerror)
		 (when (eq feature 'gptel-agent)
		   (setq loaded t))))
	      ((symbol-function 'gptel-agent-update)
	       #'ignore)
	      ((symbol-function 'gptel-get-tool)
	       (lambda (name) (intern (concat "tool-" name)))))
      (should (equal (org-copilot-gptel--available-web-tools)
		     '(tool-WebSearch tool-WebFetch)))
      (should loaded))))

(ert-deftest org-copilot-gptel-web-tools-require-tool-capable-model ()
  "Web tools are not exposed when the active model lacks tool-use."
  (cl-letf (((symbol-function 'gptel--model-capable-p) #'ignore)
	    ((symbol-function 'gptel-get-tool)
	     (lambda (name) (intern (concat "tool-" name)))))
    (should-not
     (org-copilot-gptel--web-tools-for-request
      (list :message "donne moi des références précises")))))

(ert-deftest org-copilot-gptel-chat-enables-web-tools-for-reference-qa ()
  "Reference-style chat requests expose configured gptel web tools."
  (with-temp-buffer
    (org-mode)
    (let (captured-tools captured-use-tools)
      (cl-letf (((symbol-function 'gptel-get-tool)
		 (lambda (name) (intern (concat "tool-" name))))
		((symbol-function 'gptel-request)
		 (lambda (_prompt &rest args)
		   (setq captured-tools gptel-tools)
		   (setq captured-use-tools gptel-use-tools)
		   (let ((callback (plist-get args :callback)))
		     (funcall callback
			      "{\"message\":\"Found sources.\"}"
			      (list :status 'success))))))
	(org-copilot-gptel-chat
	 (list :source-buffer (current-buffer)
	       :buffer-name (buffer-name)
	       :message "donne moi des références précises"
	       :messages nil
	       :chat-context '(:type full-document)
	       :focus-comment-id nil))
	(should captured-use-tools)
	(should (memq 'tool-WebSearch captured-tools))
	(should (memq 'tool-WebFetch captured-tools))))))

(ert-deftest org-copilot-gptel-chat-ignores-tool-callback-events ()
  "Chat callback ignores intermediate gptel tool events and waits for text."
  (with-temp-buffer
    (org-mode)
    (cl-letf (((symbol-function 'gptel-get-tool)
	       (lambda (name) (intern (concat "tool-" name))))
	      ((symbol-function 'gptel-request)
	       (lambda (_prompt &rest args)
		 (let ((callback (plist-get args :callback)))
		   (funcall callback '(tool-call . nil) (list :status 'tool-call))
		   (funcall callback
			    "{\"message\":\"Final answer.\"}"
			    (list :status 'success))))))
      (org-copilot-gptel-chat
       (list :source-buffer (current-buffer)
	     :buffer-name (buffer-name)
	     :message "references with web search"
	     :messages nil
	     :chat-context '(:type full-document)
	     :focus-comment-id nil))
      (should (= (length (org-copilot-chat-messages)) 1))
      (should (equal (plist-get (car (org-copilot-chat-messages)) :content)
		     "Final answer.")))))

(ert-deftest org-copilot-gptel-chat-drops-section-suggestion-for-reference-qa ()
  "Reference Q&A cannot install section replacement suggestions."
  (with-temp-buffer
    (org-mode)
    (insert "* Préface\nOriginal body.\n")
    (goto-char (point-min))
    (let* ((context (org-copilot-chat--section-context-at-point))
	   (context-id (org-copilot-chat--context-id context)))
      (cl-letf (((symbol-function 'gptel-request)
		 (lambda (_prompt &rest args)
		   (let ((callback (plist-get args :callback)))
		     (funcall callback
			      "{\"message\":\"No web.\",\"suggestion\":\"This is advice, not replacement.\"}"
			      (list :status 'success))))))
	(org-copilot-gptel-chat
	 (list :source-buffer (current-buffer)
	       :buffer-name (buffer-name)
	       :message "Peux-tu me donner des références utiles ? Utilise l'outil de recherche web."
	       :messages nil
	       :chat-context context
	       :context-id context-id
	       :focus-comment-id nil))
	(should-not (org-copilot-comments))
	(should-not (get-buffer org-copilot-suggestion-buffer-name))
	(should (equal (plist-get (car (org-copilot-chat-messages)) :content)
		       "No web."))))))

(ert-deftest org-copilot-gptel-chat-ignores-suggestion-for-reference-qa ()
  "Chat ignores top-level suggestions unless the user asked for a rewrite."
  (with-temp-buffer
    (org-mode)
    (insert "* Intro\nOriginal body.\n")
    (cl-letf (((symbol-function 'gptel-request)
	       (lambda (_prompt &rest args)
		 (let ((callback (plist-get args :callback)))
		   (funcall callback
			    "{\"message\":\"Here are references.\",\"suggestion\":\"I can classify them by usage.\"}"
			    (list :status 'success))))))
      (org-copilot-gptel-chat
       (list :source-buffer (current-buffer)
	     :buffer-name (buffer-name)
	     :message "donne moi des références précises"
	     :messages nil
	     :chat-context '(:type full-document)
	     :focus-comment-id nil))
      (should-not (org-copilot-comments))
      (should-not (get-buffer org-copilot-suggestion-buffer-name))
      (should (equal (plist-get (car (org-copilot-chat-messages)) :content)
		     "Here are references.")))))

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

(ert-deftest org-copilot-gptel-review-accumulates-streaming-oauth-response ()
  "The gptel review adapter requests and accumulates streaming OAuth responses."
  (with-temp-buffer
    (org-mode)
    (insert "Alpha sentence.\n")
    (let (captured-stream)
      (cl-letf (((symbol-function 'gptel-openai-oauth-p)
		 (lambda (_backend) t))
		((symbol-function 'gptel-request)
		 (lambda (_prompt &rest args)
		   (setq captured-stream (plist-get args :stream))
		   (let ((callback (plist-get args :callback)))
		     (funcall callback "{\"comments\":[{" (list :status 'streaming))
		     (funcall callback "\"id\":\"ai-1\",\"body\":\"Tighten.\",\"target_text\":\"Alpha sentence.\"}]}" (list :status 'streaming))
		     (funcall callback t (list :status 'success))))))
	(org-copilot-gptel-review
	 (list :source-buffer (current-buffer)
	       :buffer-name (buffer-name)
	       :scope 'buffer
	       :text (buffer-string)))
	(should captured-stream)
	(should (= (length (org-copilot-comments)) 1))))))

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
