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

(defun org-copilot-gptel--symbol (value fallback)
  "Return VALUE converted to a symbol, or FALLBACK when VALUE is nil."
  (cond
   ((null value) fallback)
   ((symbolp value) value)
   ((stringp value) (intern value))
   (t fallback)))

(defun org-copilot-gptel--optional-string (value)
  "Return VALUE as a non-empty string, or nil."
  (when (and (stringp value)
	     (not (string-empty-p (string-trim value))))
    value))

(defun org-copilot-gptel--comment-from-json (comment)
  "Return normalized AI comment plist from parsed JSON COMMENT."
  (org-copilot-normalize-comment
   (list :id (plist-get comment :id)
	 :type (org-copilot-gptel--symbol (plist-get comment :type) 'inline)
	 :status (org-copilot-gptel--symbol (plist-get comment :status) 'active)
	 :source-start (plist-get comment :source_start)
	 :source-end (plist-get comment :source_end)
	 :target-text (plist-get comment :target_text)
	 :line-start (plist-get comment :line_start)
	 :line-end (plist-get comment :line_end)
	 :summary (plist-get comment :summary)
	 :body (plist-get comment :body)
	 :suggestion (org-copilot-gptel--optional-string
		      (plist-get comment :suggestion))
	 :rationale (plist-get comment :rationale)
	 :metadata (plist-get comment :metadata))))

(defun org-copilot-gptel--summary-list (summary key label)
  "Return formatted SUMMARY list at KEY with LABEL, or nil."
  (when-let* ((items (plist-get summary key)))
    (concat label "\n"
	    (mapconcat (lambda (item) (format "- %s" item)) items "\n"))))

(defun org-copilot-gptel--summary-message (summary)
  "Return a readable summary message from parsed SUMMARY plist."
  (when summary
    (string-join
     (delq nil
	   (list (org-copilot-gptel--optional-string
		  (plist-get summary :message))
		 (org-copilot-gptel--summary-list summary :strengths "Strengths")
		 (org-copilot-gptel--summary-list summary :risks "Risks")
		 (org-copilot-gptel--summary-list summary :next_steps "Next steps")))
     "\n\n")))

(defun org-copilot-gptel-parse-review-result (response)
  "Parse strict JSON review RESPONSE into summary and comments."
  (let* ((parsed (json-parse-string response
				    :object-type 'plist
				    :array-type 'list
				    :null-object nil
				    :false-object nil))
	 (comments (plist-get parsed :comments)))
    (list :summary (org-copilot-gptel--summary-message
		    (plist-get parsed :summary))
	  :comments (mapcar #'org-copilot-gptel--comment-from-json comments))))

(defun org-copilot-gptel-parse-review-response (response)
  "Parse strict JSON review RESPONSE into normalized AI comments."
  (plist-get (org-copilot-gptel-parse-review-result response) :comments))

(defun org-copilot-gptel--anchor-comment-exact (comment)
  "Return COMMENT anchored by exact target text in the current buffer."
  (let ((target-text (plist-get comment :target-text)))
    (when (and target-text (not (string-empty-p target-text)))
      (save-excursion
	(goto-char (point-min))
	(when (search-forward target-text nil t)
	  (let ((copy (copy-sequence comment)))
	    (setq copy (plist-put copy :source-start (match-beginning 0)))
	    (plist-put copy :source-end (match-end 0))))))))

(defun org-copilot-gptel--request-line-range (request line-start line-end)
  "Return source bounds for REQUEST-relative LINE-START and LINE-END."
  (when (and request line-start line-end)
    (let ((request-start (or (plist-get request :start) (point-min)))
	  (request-end (or (plist-get request :end) (point-max))))
      (save-excursion
	(goto-char request-start)
	(forward-line (max 0 (1- line-start)))
	(let ((start (point)))
	  (goto-char request-start)
	  (forward-line (max 0 (1- line-end)))
	  (end-of-line)
	  (let ((end (min (point) request-end)))
	    (when (and (<= request-start start)
		       (<= start end)
		       (<= end request-end))
	      (cons start end))))))))

(defun org-copilot-gptel--anchor-comment-by-lines (comment request)
  "Return COMMENT anchored by REQUEST-relative line hints, or nil."
  (when-let* ((bounds (org-copilot-gptel--request-line-range
		       request
		       (plist-get comment :line-start)
		       (plist-get comment :line-end))))
    (let ((copy (copy-sequence comment)))
      (setq copy (plist-put copy :source-start (car bounds)))
      (plist-put copy :source-end (cdr bounds)))))

(defun org-copilot-gptel--anchored-comment (comment &optional request)
  "Return COMMENT anchored in the current buffer.
Prefer exact `:target-text' matching.  Fall back to REQUEST-relative line hints
when exact matching fails."
  (or (and (plist-get comment :source-start) comment)
      (org-copilot-gptel--anchor-comment-exact comment)
      (org-copilot-gptel--anchor-comment-by-lines comment request)
      comment))

(defun org-copilot-gptel-anchor-comments (source-buffer comments &optional request)
  "Return COMMENTS anchored in SOURCE-BUFFER.
When REQUEST is non-nil, use its `:start' and `:end' bounds for line-range
fallback anchoring."
  (with-current-buffer source-buffer
    (mapcar (lambda (comment)
	      (org-copilot-gptel--anchored-comment comment request))
	    comments)))

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
  (let* ((result (org-copilot-gptel-parse-review-result response))
	 (comments (plist-get result :comments))
	 (summary (org-copilot-gptel--review-chat-summary
		   comments
		   (plist-get result :summary)))
	 (anchored (org-copilot-gptel-anchor-comments source-buffer comments request)))
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
		    "When a focused comment is present, help iterate on its replacement suggestion.\n"
		    "For focused comments, return strict JSON only: "
		    "{\"message\":\"brief explanation\",\"suggestion\":\"revised literal replacement text\"}.\n"
		    "The suggestion must be replacement text for the target, not advice.\n"
		    "For full-document chat without a focused comment, use the source content and conversation history; do not infer hidden AI comments.\n"
		    "For section chat, use the full source content as context, but keep the named section as the focus for any analysis or suggestion.\n"
		    "For full-document and section chat, plain text is acceptable. If you provide a suggestion, return strict JSON: {\"message\":\"brief explanation\",\"suggestion\":\"suggested text\",\"heading_line\":\"optional exact target heading line\",\"section_title\":\"optional exact target title\",\"section_path\":[\"optional\",\"outline path\"]}.\n"
		    "For section suggestions, `suggestion' must be replacement content for the body below the target heading. Do not repeat the target section's own heading line. You may include subsections or lower-level headings inside the replacement body when appropriate. Omit anchor fields when the active section is the target.\n\n"
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

(defun org-copilot-gptel--parse-chat-response (response)
  "Return normalized chat response plist from RESPONSE."
  (condition-case nil
      (let ((parsed (json-parse-string response
				       :object-type 'plist
				       :array-type 'list
				       :null-object nil
				       :false-object nil)))
	(list :message (or (org-copilot-gptel--optional-string
			    (plist-get parsed :message))
			   response)
	      :suggestion (org-copilot-gptel--optional-string
			   (plist-get parsed :suggestion))
	      :summary (org-copilot-gptel--optional-string
			(plist-get parsed :summary))
	      :heading-line (org-copilot-gptel--optional-string
			     (plist-get parsed :heading_line))
	      :section-title (org-copilot-gptel--optional-string
			      (plist-get parsed :section_title))
	      :section-path (plist-get parsed :section_path)))
    (error
     (list :message response :suggestion nil))))

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
	       (let ((parsed (org-copilot-gptel--parse-chat-response response)))
		 (if comment-id
		     (org-copilot-gptel--update-focused-suggestion
		      source-buffer comment-id (plist-get parsed :suggestion))
		   (org-copilot-gptel--install-chat-suggestion
		    source-buffer parsed request))
		 (with-current-buffer source-buffer
		   (org-copilot-remove-pending-chat-message comment-id context-id)
		   (org-copilot-add-chat-message
		    'assistant (plist-get parsed :message) comment-id context-id)))
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
