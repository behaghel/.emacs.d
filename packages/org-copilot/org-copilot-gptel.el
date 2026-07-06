;;; org-copilot-gptel.el --- gptel adapter for Org Copilot -*- lexical-binding: t; -*-

;; Author: Hubert Behaghel
;; Maintainer: Hubert Behaghel
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (org "9.6"))
;; Keywords: outlines, tools, convenience
;; URL: https://github.com/behaghel/org-copilot

;;; Commentary:
;; Optional gptel adapter for Org Copilot review requests.  Loading this file
;; does not require gptel; real review requests signal a clear user error when
;; `gptel-request' is unavailable.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'org-copilot-chat)
(require 'org-copilot-llm)
(require 'org-copilot-session)
(require 'org-copilot-suggestion)

(require 'gptel nil 'noerror)

(declare-function gptel-request "gptel")

(defcustom org-copilot-gptel-review-instructions
  (string-join
   '("You are a balanced, high-impact AI reviewer for an Org document."
     "Prefer specific anchored inline comments over broad scope comments."
     "For inline comments, copy the exact reviewed source span into target_text."
     "Use scope comments only for issues that apply to the whole reviewed subtree or document."
     "Prioritize clarity, argument strength, structure, evidence/support, and outcome fit."
     "If the text states a goal, intended audience, or desired outcome, judge feedback against that outcome."
     "Avoid low-value nitpicks; focus on changes likely to improve the document's effectiveness."
     "Use summary for a sharp marginal note, max 72 characters; provocative questions are welcome when useful."
     "Use body for the fuller explanation behind the marginal note."
     "Use suggestion only for literal replacement text that should replace target_text exactly."
     "For every inline comment that recommends a textual change, include suggestion."
     "Do not put advice, rationale, markdown fences, or commentary in suggestion."
     "Omit suggestion only when no precise replacement would be safe or useful.")
   "\n")
  "Reviewer instructions included in Org Copilot gptel review prompts."
  :type 'string
  :group 'org-copilot)

(defconst org-copilot-gptel--review-json-schema-instructions
  (concat "Return strict JSON only, with no Markdown fences.\n"
	  "The JSON shape must be:\n"
	  "{\"summary\":{\"message\":\"overall review of the document or section\","
	  "\"strengths\":[\"specific strength\"],"
	  "\"risks\":[\"specific risk\"],"
	  "\"next_steps\":[\"specific next step\"]},"
	  "\"comments\":[{\"id\":\"ai-1\","
	  "\"type\":\"inline\","
	  "\"summary\":\"sharp marginal note, max 72 chars\","
	  "\"body\":\"full review explanation\","
	  "\"target_text\":\"exact reviewed text\","
	  "\"suggestion\":\"literal replacement text for target_text when changing text\","
	  "\"line_start\":1,\"line_end\":1}]}\n"
	  "Use type \"inline\" for anchored inline comments and \"scope\" "
	  "for whole-scope comments. Include summary for general document-level analysis.")
  "Fixed JSON schema instructions for Org Copilot gptel review prompts.")

(defun org-copilot-gptel-review-prompt (request)
  "Return a strict JSON review prompt for REQUEST."
  (format (concat "%s\n\n"
		  "%s\n\n"
		  "Buffer: %s\n"
		  "Scope: %s\n\n"
		  "Org text:\n%s")
	  org-copilot-gptel-review-instructions
	  org-copilot-gptel--review-json-schema-instructions
	  (or (plist-get request :buffer-name) "Org buffer")
	  (plist-get request :scope)
	  (plist-get request :text)))

(defun org-copilot-gptel--open-general-chat (source-buffer)
  "Open the general Org Copilot chat for SOURCE-BUFFER."
  (with-current-buffer source-buffer
    (org-copilot-chat--set-context source-buffer '(:type full-document))
    (org-copilot-chat--open-bottom-view source-buffer)))

(defun org-copilot-gptel--review-count-summary (comments)
  "Return a local review count summary for COMMENTS."
  (let* ((comment-count (length comments))
	 (suggestion-count (cl-count-if
			    (lambda (comment) (plist-get comment :suggestion))
			    comments)))
    (format "Review complete: %d comment%s, %d suggestion%s."
	    comment-count
	    (if (= comment-count 1) "" "s")
	    suggestion-count
	    (if (= suggestion-count 1) "" "s"))))

(defun org-copilot-gptel--review-chat-summary (comments summary)
  "Return chat summary text for COMMENTS and optional SUMMARY."
  (string-join (delq nil (list (org-copilot-gptel--review-count-summary comments)
			       summary))
	       "\n\n"))

(defun org-copilot-gptel--install-response (source-buffer response &optional request)
  "Install gptel review RESPONSE into SOURCE-BUFFER's session.
REQUEST supplies reviewed source bounds for line-range fallback anchoring."
  (let* ((result (org-copilot-llm-parse-review-result response))
	 (comments (plist-get result :comments))
	 (summary (org-copilot-gptel--review-chat-summary
		   comments
		   (plist-get result :summary)))
	 (anchored (org-copilot-llm-anchor-comments source-buffer comments request)))
    (message "Org Copilot: installing %d gptel comment(s)" (length anchored))
    (with-current-buffer source-buffer
      (org-copilot-chat--set-context source-buffer '(:type full-document))
      (org-copilot-install-review-comments anchored)
      (when summary
	(org-copilot-add-chat-message 'assistant summary nil))
      (org-copilot-open))
    (when summary
      (org-copilot-gptel--open-general-chat source-buffer))))

(defun org-copilot-gptel--format-chat-message (message)
  "Return MESSAGE formatted for an Org Copilot gptel chat prompt."
  (format "%s: %s"
	  (capitalize (symbol-name (plist-get message :role)))
	  (or (plist-get message :content) "")))

(defun org-copilot-gptel--focused-comment-context (request)
  "Return focused comment context for REQUEST, or nil."
  (when-let* ((source (plist-get request :source-buffer))
	      (focus-id (plist-get request :focus-comment-id)))
    (when (buffer-live-p source)
      (with-current-buffer source
	(org-copilot-find-comment focus-id)))))

(defun org-copilot-gptel--format-focused-comment (comment)
  "Return COMMENT formatted for a focused chat prompt."
  (when comment
    (format (concat "Target text:\n%s\n\n"
		    "Current suggestion:\n%s\n\n"
		    "Comment summary:\n%s\n\n"
		    "Comment explanation:\n%s")
	    (or (plist-get comment :target-text) "")
	    (or (plist-get comment :suggestion) "")
	    (or (plist-get comment :summary) "")
	    (or (plist-get comment :body) ""))))

(defun org-copilot-gptel-chat-prompt (request)
  "Return a gptel chat prompt for REQUEST."
  (let* ((history (mapconcat #'org-copilot-gptel--format-chat-message
			     (plist-get request :messages)
			     "\n"))
	 (focus-id (plist-get request :focus-comment-id))
	 (source-content (plist-get request :source-content))
	 (section-content (plist-get request :section-content))
	 (focused-comment (org-copilot-gptel--focused-comment-context request))
	 (focused-context (org-copilot-gptel--format-focused-comment focused-comment)))
    (format (concat "You are an AI writing partner for an Org author.\n"
		    "Answer the user's question concisely and concretely.\n"
		    "Return strict JSON only, with no Markdown fences.\n"
		    "The JSON shape is {\"message\":\"brief answer\",\"suggestion\":\"optional broad rewrite\",\"heading_line\":\"optional exact target heading line\",\"section_title\":\"optional exact target title\",\"section_path\":[\"optional\",\"outline path\"],\"comments\":[...]} .\n"
		    "Use `comments' only when the user clearly asks for review, critique, edits, improvements, suggestions, issues, or targeted comments; otherwise return an empty comments array.\n"
		    "Each comment has optional id, type (`inline' or `scope'), summary, body, target_text, suggestion, line_start, and line_end.\n"
		    "Inline comments must include exact target_text copied from the document; comment suggestions must be literal replacements for target_text only.\n"
		    "At most one scope comment is useful. Prefer no more than 8 comments total.\n"
		    "Do not combine a broad top-level suggestion with targeted comments unless the user explicitly asks for both a rewrite and review comments. This is exceptional because broad rewrites and targeted suggestions can contradict each other. If both seem necessary, ask for confirmation in `message' and return no top-level suggestion.\n"
		    "When a focused comment is present, help iterate on its replacement suggestion. The top-level suggestion must be replacement text for the focused target, not advice.\n"
		    "For full-document chat without a focused comment, use the source content and conversation history; do not infer hidden AI comments.\n"
		    "For section chat, use the full source content as context, but generated comments must target only the focused section unless the user explicitly switches to full-document chat.\n"
		    "For section suggestions, the top-level `suggestion' must be replacement content for the body below the target heading. Do not repeat the target section's own heading line. You may include subsections or lower-level headings inside the replacement body when appropriate. Omit anchor fields when the active section is the target.\n\n"
		    "Buffer: %s\n"
		    "Focused comment id: %s\n\n"
		    "Full source content:\n%s\n\n"
		    "Focused section content, when section chat is active:\n%s\n\n"
		    "Focused comment context:\n%s\n\n"
		    "Conversation so far:\n%s\n\n"
		    "User message:\n%s")
	    (or (plist-get request :buffer-name) "Org buffer")
	    (or focus-id "none")
	    (if focus-id "<focused comment chat>" (or source-content "<none>"))
	    (or section-content "<none>")
	    (or focused-context "<none>")
	    (if (string-empty-p history) "<none>" history)
	    (plist-get request :message))))

(defun org-copilot-gptel--update-focused-suggestion (source-buffer comment-id suggestion)
  "Update COMMENT-ID in SOURCE-BUFFER with revised SUGGESTION."
  (when (and comment-id suggestion)
    (with-current-buffer source-buffer
      (when-let* ((comment (org-copilot-find-comment comment-id)))
	(let ((copy (copy-sequence comment)))
	  (org-copilot-update-comment
	   (plist-put copy :suggestion suggestion)))))))

(defun org-copilot-gptel--install-chat-suggestion (source-buffer parsed request)
  "Install or preview non-comment chat suggestion PARSED for REQUEST."
  (when-let* ((suggestion (plist-get parsed :suggestion)))
    (let ((context (plist-get request :chat-context)))
      (if (org-copilot-suggestion-install-section
	   source-buffer parsed context (plist-get parsed :message))
	  (with-current-buffer source-buffer
	    (when (fboundp 'org-copilot-refresh-overlays)
	      (org-copilot-refresh-overlays))
	    (when (fboundp 'org-context-panel-refresh)
	      (org-context-panel-refresh)))
	(org-copilot-suggestion-open
	 source-buffer "🌐 Full document" suggestion 'full-document nil)))))

(defun org-copilot-gptel--render-chat-buffer (source-buffer &optional scroll-role)
  "Refresh visible Org Copilot chat buffer for SOURCE-BUFFER when present.
When SCROLL-ROLE is non-nil, scroll that role's last message to the top of the
chat viewport."
  (when-let* ((buffer (get-buffer org-copilot-chat-buffer-name)))
    (with-current-buffer buffer
      (when (eq org-copilot-chat-source-buffer source-buffer)
	(org-copilot-chat-render source-buffer)
	(org-copilot-chat-sync-diff source-buffer)
	(when scroll-role
	  (org-copilot-chat-scroll-to-last-message source-buffer scroll-role))))))

(defun org-copilot-gptel-chat (request)
  "Request an Org Copilot chat response for REQUEST using gptel."
  (unless (fboundp 'gptel-request)
    (user-error "Org Copilot gptel chat adapter requires gptel"))
  (let ((source-buffer (plist-get request :source-buffer))
	(comment-id (plist-get request :focus-comment-id))
	(context-id (plist-get request :context-id))
	(prompt (org-copilot-gptel-chat-prompt request)))
    (gptel-request
     prompt
     :callback
     (lambda (response info)
       (condition-case err
	   (progn
	     (message "Org Copilot: gptel chat completed status=%S"
		      (plist-get info :status))
	     (when (and response (buffer-live-p source-buffer))
	       (let* ((parsed (org-copilot-llm-parse-chat-response response))
		      (install-result
		       (unless comment-id
			 (org-copilot-install-chat-comments
			  source-buffer
			  (plist-get parsed :comments)
			  request
			  (plist-get request :message))))
		      (message (org-copilot-llm-append-chat-install-summary
				(plist-get parsed :message)
				install-result)))
		 (if comment-id
		     (org-copilot-gptel--update-focused-suggestion
		      source-buffer comment-id (plist-get parsed :suggestion))
		   (org-copilot-gptel--install-chat-suggestion
		    source-buffer parsed request))
		 (with-current-buffer source-buffer
		   (org-copilot-remove-pending-chat-message comment-id context-id)
		   (org-copilot-add-chat-message
		    'assistant message comment-id context-id)))
	       (org-copilot-gptel--render-chat-buffer source-buffer 'assistant)))
	 (error
	  (message "Org Copilot: gptel chat failed error=%S response=%S"
		   err response)))))
    nil))

(defun org-copilot-gptel-review (request)
  "Request an Org Copilot review for REQUEST using gptel.
The request is asynchronous when using real gptel.  Parsed callback comments are
installed into REQUEST's `:source-buffer' session."
  (unless (fboundp 'gptel-request)
    (user-error "Org Copilot gptel adapter requires gptel"))
  (let ((source-buffer (plist-get request :source-buffer))
	(prompt (org-copilot-gptel-review-prompt request)))
    (gptel-request
     prompt
     :callback
     (lambda (response info)
       (condition-case err
	   (progn
	     (message "Org Copilot: gptel review completed status=%S"
		      (plist-get info :status))
	     (when (and response (buffer-live-p source-buffer))
	       (org-copilot-gptel--install-response source-buffer response request)))
	 (error
	  (message "Org Copilot: gptel review failed error=%S response=%S"
		   err response)))))
    nil))

;;;###autoload
(defun org-copilot-gptel-enable ()
  "Use gptel as the Org Copilot review adapter."
  (interactive)
  (unless (fboundp 'gptel-request)
    (user-error "Org Copilot gptel adapter requires gptel"))
  (setq org-copilot-review-function #'org-copilot-gptel-review)
  (setq org-copilot-chat-function #'org-copilot-gptel-chat))

(provide 'org-copilot-gptel)
;;; org-copilot-gptel.el ends here
