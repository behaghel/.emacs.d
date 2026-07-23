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
(require 'org-comments-model)
(require 'org-comments-sidecar)
(require 'org-suggestions)
(require 'org-copilot-sidecar)
(require 'org-copilot-chat)
(require 'org-copilot-debug)
(require 'org-copilot-llm)
(require 'org-copilot-session)
(require 'org-copilot-suggestion)

(require 'gptel nil 'noerror)

(declare-function gptel--model-capable-p "gptel-request")
(declare-function gptel-agent-update "gptel-agent")
(declare-function gptel-get-tool "gptel")
(declare-function gptel-openai-oauth-p "gptel-openai-oauth")
(declare-function gptel-request "gptel")

(defvar gptel-backend nil)
(defvar gptel-model nil)
(defvar gptel-tools nil)
(defvar gptel-use-tools nil)

(defcustom org-copilot-gptel-review-instructions
  (string-join
   '("You are a balanced, high-impact AI reviewer for an Org document."
     "Prefer specific anchored inline comments over broad scope comments."
     "For inline comments, copy the exact reviewed source span into target_text."
     "For comments proposing text to add at a specific location, use type insertion with exact anchor_text, placement before or after, and suggestion."
     "Use scope comments only for issues that apply to the whole reviewed subtree or document."
     "Prioritize clarity, argument strength, structure, evidence/support, and outcome fit."
     "If the text states a goal, intended audience, or desired outcome, judge feedback against that outcome."
     "Avoid low-value nitpicks; focus on changes likely to improve the document's effectiveness."
     "Use summary for a sharp marginal note, max 72 characters; provocative questions are welcome when useful."
     "Use body for the fuller explanation behind the marginal note."
     "Use suggestion only for executable text: literal replacement text for inline comments or inserted text for insertion comments."
     "For every inline comment that recommends a textual change, include suggestion."
     "For every insertion comment, include suggestion."
     "Do not put advice, rationale, markdown fences, or commentary in suggestion."
     "Omit suggestion only when no precise replacement would be safe or useful.")
   "\n")
  "Reviewer instructions included in Org Copilot gptel review prompts."
  :type 'string
  :group 'org-copilot)

(defcustom org-copilot-gptel-enable-web-tools t
  "Whether Org Copilot may use available gptel web tools for chat requests."
  :type 'boolean
  :group 'org-copilot)

(defcustom org-copilot-gptel-web-tool-names '("WebSearch" "WebFetch")
  "Names of gptel tools to expose for web-backed Org Copilot chat."
  :type '(repeat string)
  :group 'org-copilot)

(defcustom org-copilot-gptel-backend nil
  "Optional gptel backend used specifically for Org Copilot requests."
  :type 'sexp
  :group 'org-copilot)

(defcustom org-copilot-gptel-model nil
  "Optional gptel model used specifically for Org Copilot requests."
  :type 'sexp
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
	  "\"anchor_text\":\"exact insertion anchor text\","
	  "\"placement\":\"before or after\","
	  "\"suggestion\":\"literal replacement or insertion text\","
	  "\"line_start\":1,\"line_end\":1}]}\n"
	  "Use type \"inline\" for anchored inline comments, \"insertion\" for text to add at a specific location, and \"scope\" "
	  "for whole-scope comments. Inline requires target_text. Insertion requires anchor_text, placement, and suggestion. Include summary for general document-level analysis.")
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

(defun org-copilot-gptel--web-query-p (message)
  "Return non-nil when MESSAGE appears to need live web information."
  (let ((case-fold-search t)
	(text (or message "")))
    (string-match-p
     (rx (or "web" "search" "source" "sources" "citation" "citations"
	     "reference" "references" "bibliography" "bibliographic"
	     "current" "recent" "latest" "today" "verify" "check online"
	     "référence" "références" "bibliographie" "source" "sources"
	     "citation" "citations" "récent" "récente" "actuel"
	     "vérifie" "verifie" "en ligne"))
     text)))

(defun org-copilot-gptel--ensure-web-tools ()
  "Try to load optional gptel web tool providers."
  (when (require 'gptel-agent nil 'noerror)
    (when (fboundp 'gptel-agent-update)
      (gptel-agent-update))))

(defun org-copilot-gptel--available-web-tools ()
  "Return configured gptel web tools that are currently available."
  (org-copilot-gptel--ensure-web-tools)
  (when (fboundp 'gptel-get-tool)
    (delq nil
	  (mapcar (lambda (name)
		    (ignore-errors (gptel-get-tool name)))
		  org-copilot-gptel-web-tool-names))))

(defun org-copilot-gptel--tool-use-supported-p ()
  "Return non-nil when the active gptel model can use tools."
  (or (not (fboundp 'gptel--model-capable-p))
      (ignore-errors (gptel--model-capable-p 'tool-use))))

(defun org-copilot-gptel--web-tools-for-request (request)
  "Return web tools to expose for chat REQUEST."
  (and org-copilot-gptel-enable-web-tools
       (org-copilot-gptel--web-query-p (plist-get request :message))
       (org-copilot-gptel--tool-use-supported-p)
       (org-copilot-gptel--available-web-tools)))

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

(defun org-copilot-gptel--suggestion-thread-history (request)
  "Return compact suggestion thread history for REQUEST."
  (when-let* ((source (plist-get request :source-buffer))
	      (source-file (buffer-file-name source))
	      (thread-id (plist-get request :suggestion-thread-id))
	      (thread (cl-find thread-id
			       (org-suggestions-load-sidecar source-file)
			       :key (lambda (item) (plist-get item :id))
			       :test #'equal)))
    (concat
     (format "Thread %s\n" thread-id)
     (mapconcat
      (lambda (candidate)
	(format "- %s %s"
		(plist-get candidate :id)
		(plist-get candidate :status)))
      (plist-get thread :candidates)
      "\n"))))

(defun org-copilot-gptel-chat-prompt (request)
  "Return a gptel chat prompt for REQUEST."
  (let* ((history (mapconcat #'org-copilot-gptel--format-chat-message
			     (plist-get request :messages)
			     "\n"))
	 (focus-id (plist-get request :focus-comment-id))
	 (source-content (plist-get request :source-content))
	 (section-content (plist-get request :section-content))
	 (focused-comment (org-copilot-gptel--focused-comment-context request))
	 (focused-context (org-copilot-gptel--format-focused-comment focused-comment))
	 (suggestion-history (org-copilot-gptel--suggestion-thread-history request)))
    (format (concat "You are an AI writing partner for an Org author.\n"
		    "Answer the user's question concisely and concretely.\n"
		    "Return strict JSON only, with no Markdown fences.\n"
		    "The JSON shape is {\"message\":\"brief answer\",\"intent\":\"answer|review|edit\",\"suggestion_threads\":[{\"intent\":\"rewrite_section|revise_suggestion|insert_text|mixed_edit\",\"summary\":\"short linked comment text\",\"suggestions\":[{\"id\":\"optional provider id\",\"label\":\"optional candidate label\",\"hunks\":[{\"id\":\"h1\",\"kind\":\"section-replace|replace|insert\",\"primary\":true,\"section_title\":\"optional exact title\",\"section_path\":[\"optional\",\"outline path\"],\"anchor_text\":\"exact insertion anchor\",\"placement\":\"before|after\",\"original\":\"exact replacement target\",\"replacement\":\"executable text\"}]}]}],\"comments\":[...]} .\n"
		    "Choose top-level intent from the user's meaning, not keywords: answer for Q&A, review for critique/comments, and edit for executable edit proposals. Use suggestion_threads only for executable edits. Default to one suggestion thread with one suggestion; use multiple suggestions only for explicit alternatives or coherent multi-hunk edits, and multiple threads only for clearly separate edit intents.\n"
		    "Use `comments' only when the user clearly asks for review, critique, edits, improvements, suggestions, issues, or targeted comments; otherwise return an empty comments array.\n"
		    "Each comment has optional id, type (`inline', `insertion', or `scope'), summary, body, target_text, anchor_text, placement, suggestion, line_start, and line_end.\n"
		    "Inline comments must include exact target_text copied from the document; inline suggestions must be literal replacements for target_text only.\n"
		    "Insertion comments are for adding text at a specific location; they must include exact anchor_text copied from the document, placement `before' or `after', and suggestion containing only the text to insert.\n"
		    "Scope comments are for broad document or section observations without a specific executable edit location; do not attach suggestions to scope comments.\n"
		    "Do not return top-level `suggestion'. Put executable text only in suggestion_threads hunks as `replacement'. Never put advice, follow-up offers, summaries, explanations, questions, classifications, or bibliographic references in replacement text.\n"
		    "For ordinary Q&A, recommendations, explanations, plans, and reference lists, put the complete answer in `message' and omit `suggestion'.\n"
		    "When asked for precise references, provide actual bibliographic entries in `message' with author, title, year, and publisher when known; do not claim to provide precise references unless you list them. If uncertain, say what is uncertain instead of inventing details.\n"
		    (if (plist-get request :web-tools-enabled)
			"Live web tools are available for this request. For current facts, source checks, precise references, bibliographies, or citation-heavy answers, use WebSearch first and WebFetch when a result needs inspection. Cite only sources you actually used.\n"
		      "No live web search tool is available for this request. If web verification is needed, say so plainly, then answer from existing knowledge with uncertainty marked.\n")
		    "At most one scope comment is useful. Prefer no more than 8 comments total.\n"
		    "Do not combine broad replacement hunks with targeted comments unless the user explicitly asks for both a rewrite and review comments. This is exceptional because broad rewrites and targeted suggestions can contradict each other. If both seem necessary, ask for confirmation in `message' and return no suggestion_threads.\n"
		    "When a focused suggestion/comment is present, help iterate on its replacement candidate. Replacement hunks must contain source text for the focused target, not advice.\n"
		    "For full-document chat without a focused comment, use the source content and conversation history; do not infer hidden AI comments.\n"
		    "For section chat, use the full source content as context, but generated comments and suggestion threads must target only the focused section unless the user explicitly switches to full-document chat.\n"
		    "For section-replace hunks, replacement must be content for the body below the target heading. Do not repeat the target section's own heading line. You may include subsections or lower-level headings inside the replacement body when appropriate.\n\n"
		    "Buffer: %s\n"
		    "Focused comment id: %s\n\n"
		    "Full source content:\n%s\n\n"
		    "Focused section content, when section chat is active:\n%s\n\n"
		    "Focused comment context:\n%s\n\n"
		    "Focused suggestion thread history:\n%s\n\n"
		    "Conversation so far:\n%s\n\n"
		    "User message:\n%s")
	    (or (plist-get request :buffer-name) "Org buffer")
	    (or focus-id "none")
	    (if focus-id "<focused comment chat>" (or source-content "<none>"))
	    (or section-content "<none>")
	    (or focused-context "<none>")
	    (or suggestion-history "<none>")
	    (if (string-empty-p history) "<none>" history)
	    (plist-get request :message))))

(defun org-copilot-gptel--next-revision-id (source-buffer root-id)
  "Return next revision id for ROOT-ID in SOURCE-BUFFER."
  (let ((max-revision 0)
	(prefix (concat (regexp-quote root-id) "\\.\\([0-9]+\\)\\'")))
    (with-current-buffer source-buffer
      (dolist (comment (org-copilot-comments))
	(when-let* ((id (org-copilot-comment-id comment)))
	  (when (string-match prefix id)
	    (setq max-revision
		  (max max-revision
		       (string-to-number (match-string 1 id))))))))
    (format "%s.%d" root-id (1+ max-revision))))

(defun org-copilot-gptel--create-section-revision
    (source-buffer comment suggestion message)
  "Create a new active section revision from COMMENT and SUGGESTION."
  (let* ((root-id (or (plist-get comment :thread-root-id)
		      (org-copilot-comment-id comment)))
	 (revision-id (org-copilot-gptel--next-revision-id source-buffer root-id))
	 (normalized (org-copilot-suggestion-normalize-section-body suggestion))
	 (section (org-copilot-suggestion-resolve-comment-section
		   source-buffer comment))
	 (copy (copy-sequence comment))
	 (metadata (copy-sequence (or (plist-get copy :metadata) nil))))
    (setq metadata (plist-put metadata :parent-comment-id
			      (org-copilot-comment-id comment)))
    (setq copy (plist-put copy :id revision-id))
    (setq copy (plist-put copy :status 'active))
    (setq copy (plist-put copy :thread-root-id root-id))
    (setq copy (plist-put copy :parent-comment-id
			  (org-copilot-comment-id comment)))
    (setq copy (plist-put copy :revision
			  (string-to-number
			   (car (last (split-string revision-id "\\."))))))
    (setq copy (plist-put copy :metadata metadata))
    (setq copy (plist-put copy :body (or message (plist-get copy :body))))
    (setq copy (plist-put copy :suggestion normalized))
    (when section
      (setq copy (plist-put copy :source-start (plist-get section :body-start)))
      (setq copy (plist-put copy :source-end (plist-get section :end)))
      (setq copy (plist-put copy :target-text
			    (with-current-buffer source-buffer
			      (buffer-substring-no-properties
			       (plist-get section :body-start)
			       (plist-get section :end)))))
      (setq copy (plist-put copy :heading-line (plist-get section :heading-line)))
      (setq copy (plist-put copy :section-title (plist-get section :section-title)))
      (setq copy (plist-put copy :section-path (plist-get section :section-path))))
    (with-current-buffer source-buffer
      (let ((revision (org-copilot-add-comment copy)))
	(org-copilot-chat--set-context
	 source-buffer (list :type 'comment :comment-id revision-id))
	(org-copilot-suggestion-open-comment source-buffer revision nil)
	revision))))

(defun org-copilot-gptel--update-focused-suggestion
    (source-buffer comment-id suggestion &optional message)
  "Update COMMENT-ID in SOURCE-BUFFER with revised SUGGESTION."
  (when (and comment-id suggestion)
    (with-current-buffer source-buffer
      (when-let* ((comment (org-copilot-find-comment comment-id)))
	(if (org-copilot-suggestion-section-comment-p comment)
	    (org-copilot-gptel--create-section-revision
	     source-buffer comment suggestion message)
	  (let ((copy (copy-sequence comment)))
	    (org-copilot-update-comment
	     (plist-put copy :suggestion suggestion))))))))

(defun org-copilot-gptel--chat-suggestions-allowed-p (parsed request)
  "Return non-nil when PARSED may install top-level suggestions for REQUEST."
  (pcase (plist-get parsed :intent)
    ('rewrite_section (eq (plist-get (plist-get request :chat-context) :type)
			  'section))
    ('rewrite_document t)
    ('draft_document t)
    ('revise_comment (plist-get request :focus-comment-id))
    (_ nil)))

(defun org-copilot-gptel--drop-disallowed-chat-suggestion (parsed request)
  "Return PARSED with unsafe top-level suggestions removed for REQUEST."
  (if (or (not (plist-get parsed :suggestion))
	  (org-copilot-gptel--chat-suggestions-allowed-p parsed request))
      parsed
    (plist-put (copy-sequence parsed) :suggestion nil)))

(defun org-copilot-gptel--next-thread-id (source-file)
  "Return next suggestion thread id for SOURCE-FILE."
  (format "ai-thread-%d"
	  (1+ (length (org-suggestions-load-sidecar source-file)))))

(defun org-copilot-gptel--ensure-candidate-ids (candidates thread-id)
  "Return CANDIDATES with stable ids derived from THREAD-ID when missing."
  (cl-loop for candidate in candidates
	   for index from 1
	   collect (if (org-copilot-llm-optional-string
			(plist-get candidate :id))
		       candidate
		     (plist-put (copy-sequence candidate) :id
				(format "%s.%d" thread-id index)))))

(defun org-copilot-gptel--normalize-section-title (title)
  "Return TITLE normalized for matching against Org headings."
  (when (stringp title)
    (let* ((trimmed (string-trim title))
	   (todo-keywords (and (boundp 'org-todo-keywords-1)
			       org-todo-keywords-1))
	   (todo-regexp (and todo-keywords
			     (concat "\\`" (regexp-opt todo-keywords 'words) "[ \\t]+"))))
      (if (and todo-regexp (string-match-p todo-regexp trimmed))
	  (string-trim (replace-regexp-in-string todo-regexp "" trimmed nil nil 1))
	trimmed))))

(defun org-copilot-gptel--section-heading-bounds (source-buffer title)
  "Return cons bounds for section heading TITLE in SOURCE-BUFFER."
  (let ((normalized-title (org-copilot-gptel--normalize-section-title title)))
    (with-current-buffer source-buffer
      (save-excursion
	(goto-char (point-min))
	(catch 'found
	  (while (re-search-forward org-heading-regexp nil t)
	    (beginning-of-line)
	    (when (string= normalized-title (org-get-heading t t t t))
	      (throw 'found (cons (line-beginning-position)
				  (line-end-position))))
	    (org-end-of-subtree t t)))))))

(defun org-copilot-gptel--primary-hunk (thread)
  "Return primary hunk from THREAD, or first hunk."
  (let ((hunks (cl-mapcan (lambda (candidate)
			    (copy-sequence (plist-get candidate :hunks)))
			  (plist-get thread :candidates))))
    (or (cl-find-if (lambda (hunk) (plist-get hunk :primary)) hunks)
	(car hunks))))

(defun org-copilot-gptel--persist-thread-comment
    (source-buffer source-file thread thread-id suggestion-ids)
  "Persist linked comment for THREAD in SOURCE-BUFFER/SOURCE-FILE."
  (when-let* ((hunk (org-copilot-gptel--primary-hunk thread))
	      (section-title (plist-get hunk :section-title))
	      (bounds (org-copilot-gptel--section-heading-bounds
		       source-buffer section-title)))
    (with-current-buffer source-buffer
      (let ((record (org-comments-create-record
		     source-file (car bounds) (cdr bounds)
		     (or (plist-get thread :summary) "AI suggestion")
		     nil "org-copilot" (format-time-string "%FT%T%z"))))
	(setq record (plist-put record :provider "org-copilot"))
	(setq record (plist-put record :org-copilot-session-id "default"))
	(setq record (plist-put record :suggestion-thread-id thread-id))
	(setq record (plist-put record :suggestion-ids
				(string-join suggestion-ids " ")))
	(org-comments-append-to-sidecar record)
	(plist-get record :id)))))

(defun org-copilot-gptel--install-suggestion-threads
    (source-buffer parsed request)
  "Install durable suggestion threads from PARSED for REQUEST."
  (when-let* ((source-file (buffer-file-name source-buffer))
	      (threads (plist-get parsed :suggestion-threads)))
    (let ((stored (org-suggestions-load-sidecar source-file))
	  installed-thread-ids
	  installed-suggestion-ids)
      (dolist (thread threads)
	(let* ((requested-thread-id (plist-get request :suggestion-thread-id))
	       (thread-id (or (plist-get thread :id)
			      requested-thread-id
			      (org-copilot-gptel--next-thread-id source-file)))
	       (existing (cl-find thread-id stored
				  :key (lambda (item) (plist-get item :id))
				  :test #'equal))
	       (candidates (org-copilot-gptel--ensure-candidate-ids
			    (plist-get thread :candidates) thread-id))
	       (suggestion-ids (mapcar (lambda (candidate)
					 (plist-get candidate :id))
				       candidates))
	       (focus-suggestion-id (plist-get request :focus-suggestion-id)))
	  (when focus-suggestion-id
	    (setq candidates
		  (mapcar (lambda (candidate)
			    (plist-put candidate :parent-id focus-suggestion-id))
			  candidates)))
	  (if existing
	      (plist-put existing :candidates
			 (append (plist-get existing :candidates) candidates))
	    (let* ((new-thread (copy-sequence thread))
		   (comment-id nil))
	      (setq new-thread (plist-put new-thread :id thread-id))
	      (setq new-thread (plist-put new-thread :provider "org-copilot"))
	      (setq new-thread (plist-put new-thread :session-id "default"))
	      (setq new-thread (plist-put new-thread :candidates candidates))
	      (setq comment-id
		    (org-copilot-gptel--persist-thread-comment
		     source-buffer source-file new-thread thread-id suggestion-ids))
	      (setq new-thread (plist-put new-thread :comment-id comment-id))
	      (unless comment-id
		(org-copilot-debug-record
		 "Suggestion thread comment not anchored"
		 :source-file source-file
		 :thread-id thread-id
		 :primary-hunk (org-copilot-gptel--primary-hunk new-thread)))
	      (push new-thread stored)))
	  (push thread-id installed-thread-ids)
	  (setq installed-suggestion-ids
		(append suggestion-ids installed-suggestion-ids))))
      (org-suggestions-write-sidecar source-file (nreverse stored))
      (let ((result (list :thread-ids (nreverse installed-thread-ids)
			  :suggestion-ids (nreverse installed-suggestion-ids))))
	(org-copilot-debug-record
	 "Suggestion threads installed"
	 :source-file source-file
	 :result result
	 :parsed-threads threads)
	result))))

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

(defun org-copilot-gptel--stream-required-p ()
  "Return non-nil when the active gptel backend requires streaming."
  (and (fboundp 'gptel-openai-oauth-p)
       (gptel-openai-oauth-p gptel-backend)))

(defun org-copilot-gptel--chat-response-complete-p (response stream)
  "Return non-nil when RESPONSE is complete for STREAM mode."
  (if stream (eq response t) (stringp response)))

(defun org-copilot-gptel--chat-response-text (response chunks stream)
  "Return complete response text from RESPONSE, CHUNKS, and STREAM mode."
  (if stream
      (apply #'concat (nreverse chunks))
    response))

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

(defun org-copilot-gptel--missing-executable-warning (parsed suggestions-result)
  "Return warning when PARSED promised an edit but installed no artifact."
  (when (and (memq (plist-get parsed :intent)
		   '(rewrite_section rewrite_document draft_document revise_comment edit))
	     (not (plist-get parsed :suggestion))
	     (not (plist-get suggestions-result :suggestion-ids)))
    "\n\n⚠ No executable suggestion artifact was returned or installed. Open `*Org Copilot Debug*` for the raw response and parse trace."))

(defun org-copilot-gptel--handle-chat-response
    (text source-buffer comment-id context-id request)
  "Handle complete chat response TEXT for SOURCE-BUFFER."
  (when (and (stringp text) (buffer-live-p source-buffer))
    (let* ((parsed-raw (org-copilot-llm-parse-chat-response text))
	   (parsed (org-copilot-gptel--drop-disallowed-chat-suggestion
		    parsed-raw request))
	   (suggestions-result
	    (org-copilot-gptel--install-suggestion-threads
	     source-buffer parsed request))
	   (install-result
	    (unless comment-id
	      (org-copilot-install-chat-comments
	       source-buffer
	       (plist-get parsed :comments)
	       request
	       (plist-get request :message))))
	   (message (concat
		     (org-copilot-llm-append-chat-install-summary
		      (plist-get parsed :message)
		      install-result)
		     (or (org-copilot-gptel--missing-executable-warning
			  parsed suggestions-result)
			 ""))))
      (org-copilot-debug-record
       "Chat response handled"
       :source-file (buffer-file-name source-buffer)
       :request request
       :raw-response text
       :parsed parsed
       :dropped-fields (and (not (equal parsed parsed-raw))
			    (list :before parsed-raw :after parsed))
       :comment-install-result install-result
       :suggestion-install-result suggestions-result)
      (if comment-id
	  (org-copilot-gptel--update-focused-suggestion
	   source-buffer comment-id (plist-get parsed :suggestion)
	   (plist-get parsed :message))
	(org-copilot-gptel--install-chat-suggestion
	 source-buffer parsed request))
      (with-current-buffer source-buffer
	(org-copilot-remove-pending-chat-message comment-id context-id)
	(org-copilot-add-chat-message
	 'assistant message comment-id context-id)))
    (org-copilot-gptel--render-chat-buffer source-buffer 'assistant)))

(defun org-copilot-gptel--handle-review-response (text source-buffer request)
  "Handle complete review response TEXT for SOURCE-BUFFER."
  (when (and (stringp text) (buffer-live-p source-buffer))
    (org-copilot-gptel--install-response source-buffer text request)))

(defun org-copilot-gptel-chat (request)
  "Request an Org Copilot chat response for REQUEST using gptel."
  (unless (fboundp 'gptel-request)
    (user-error "Org Copilot gptel chat adapter requires gptel"))
  (let* ((source-buffer (plist-get request :source-buffer))
	 (comment-id (plist-get request :focus-comment-id))
	 (context-id (plist-get request :context-id))
	 (web-tools (org-copilot-gptel--web-tools-for-request request))
	 (request (if web-tools
		      (plist-put (copy-sequence request) :web-tools-enabled t)
		    request))
	 (prompt (org-copilot-gptel-chat-prompt request))
	 chunks)
    (let* ((gptel-backend (or org-copilot-gptel-backend gptel-backend))
	   (gptel-model (or org-copilot-gptel-model gptel-model))
	   (stream (org-copilot-gptel--stream-required-p))
	   (gptel-use-tools (or gptel-use-tools (and web-tools t)))
	   (gptel-tools (append web-tools gptel-tools)))
      (gptel-request
       prompt
       :stream stream
       :callback
       (lambda (response info)
	 (condition-case err
	     (progn
	       (message "Org Copilot: gptel chat completed status=%S"
			(plist-get info :status))
	       (when (and stream (stringp response))
		 (push response chunks))
	       (when (org-copilot-gptel--chat-response-complete-p response stream)
		 (org-copilot-gptel--handle-chat-response
		  (org-copilot-gptel--chat-response-text response chunks stream)
		  source-buffer comment-id context-id request)))
	   (error
	    (message "Org Copilot: gptel chat failed error=%S response=%S"
		     err response))))))
    nil))

(defun org-copilot-gptel-review (request)
  "Request an Org Copilot review for REQUEST using gptel.
The request is asynchronous when using real gptel.  Parsed callback comments are
installed into REQUEST's `:source-buffer' session."
  (unless (fboundp 'gptel-request)
    (user-error "Org Copilot gptel adapter requires gptel"))
  (let* ((source-buffer (plist-get request :source-buffer))
	 (prompt (org-copilot-gptel-review-prompt request))
	 (gptel-backend (or org-copilot-gptel-backend gptel-backend))
	 (gptel-model (or org-copilot-gptel-model gptel-model))
	 (stream (org-copilot-gptel--stream-required-p))
	 chunks)
    (gptel-request
     prompt
     :stream stream
     :callback
     (lambda (response info)
       (condition-case err
	   (progn
	     (message "Org Copilot: gptel review completed status=%S"
		      (plist-get info :status))
	     (when (and stream (stringp response))
	       (push response chunks))
	     (when (org-copilot-gptel--chat-response-complete-p response stream)
	       (org-copilot-gptel--handle-review-response
		(org-copilot-gptel--chat-response-text response chunks stream)
		source-buffer request)))
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

;;;###autoload
(defun org-copilot-gptel-use-openai-oauth (&optional model)
  "Use ChatGPT subscription OAuth for gptel and Org Copilot.
MODEL defaults to `gpt-5.5'."
  (interactive)
  (unless (require 'gptel-openai-oauth nil 'noerror)
    (user-error "gptel-openai-oauth is unavailable"))
  (let ((backend (gptel-make-openai-oauth "OpenAI-sub"))
	(model (or model 'gpt-5.5)))
    (setq gptel-model model)
    (setq gptel-backend backend)
    (setq org-copilot-gptel-model model)
    (setq org-copilot-gptel-backend backend)
    (org-copilot-gptel-enable)
    (message "Org Copilot: using OpenAI OAuth backend with %s" model)
    backend))

(provide 'org-copilot-gptel)
;;; org-copilot-gptel.el ends here
