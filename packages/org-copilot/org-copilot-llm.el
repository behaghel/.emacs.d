;;; org-copilot-llm.el --- LLM adapter protocol for Org Copilot -*- lexical-binding: t; -*-

;; Author: Hubert Behaghel
;; Maintainer: Hubert Behaghel
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (org "9.6"))
;; Keywords: outlines, tools, convenience
;; URL: https://github.com/behaghel/org-copilot

;;; Commentary:
;; Adapter-neutral review request construction for Org Copilot.  The core
;; package calls `org-copilot-review-function' and installs the normalized AI
;; comments it returns.  Concrete adapters such as gptel live in separate files.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'org)
(require 'org-copilot-context-panel)
(require 'org-copilot-model)
(require 'org-copilot-session)
(require 'seq)
(require 'subr-x)

(defcustom org-copilot-review-function nil
  "Function used to request an AI review.
The function receives one plist request and returns a list of normalized or
normalizable AI comment plists.  Adapter packages set this variable."
  :type '(choice (const :tag "No adapter" nil)
		 function)
  :group 'org-copilot)

(defcustom org-copilot-chat-max-comments 8
  "Maximum number of comments a single chat response may install."
  :type 'natnum
  :group 'org-copilot)

(defcustom org-copilot-chat-open-panel-on-comments t
  "Whether chat-generated comments open the Org Copilot side panel."
  :type 'boolean
  :group 'org-copilot)

(defun org-copilot--region-active-p ()
  "Return non-nil when the current buffer has an active region."
  (and mark-active
       (mark t)))

(defun org-copilot--subtree-bounds ()
  "Return current Org subtree bounds, or whole buffer bounds outside headings."
  (save-excursion
    (if (org-before-first-heading-p)
	(cons (point-min) (point-max))
      (org-back-to-heading t)
      (let ((start (point)))
	(org-end-of-subtree t t)
	(cons start (point))))))

(defun org-copilot-review-bounds-dwim ()
  "Return review bounds as a plist for the active region or current subtree."
  (if (org-copilot--region-active-p)
      (let ((start (region-beginning))
	    (end (region-end)))
	(list :scope 'region :start start :end end))
    (pcase-let ((`(,start . ,end) (org-copilot--subtree-bounds)))
      (list :scope 'subtree :start start :end end))))

(defun org-copilot-review-request-dwim ()
  "Return a review request plist for the current Org buffer context."
  (unless (derived-mode-p 'org-mode)
    (user-error "Org Copilot review needs an Org source buffer"))
  (let* ((bounds (org-copilot-review-bounds-dwim))
	 (start (plist-get bounds :start))
	 (end (plist-get bounds :end)))
    (append bounds
	    (list :source-buffer (current-buffer)
		  :buffer-name (buffer-name)
		  :text (buffer-substring-no-properties start end)))))

(defun org-copilot--request-review (request)
  "Request AI review for REQUEST through `org-copilot-review-function'."
  (unless org-copilot-review-function
    (user-error "No Org Copilot review adapter configured"))
  (funcall org-copilot-review-function request))

(defun org-copilot-llm--symbol (value fallback)
  "Return VALUE converted to a symbol, or FALLBACK when VALUE is nil."
  (cond
   ((null value) fallback)
   ((symbolp value) value)
   ((stringp value) (intern value))
   (t fallback)))

(defun org-copilot-llm-optional-string (value)
  "Return VALUE as a non-empty string, or nil."
  (when (and (stringp value)
	     (not (string-empty-p (string-trim value))))
    value))

(defun org-copilot-llm--comment-from-json (comment &optional fallback-id)
  "Return normalized AI comment plist from parsed JSON COMMENT.
When FALLBACK-ID is non-nil, use it if COMMENT has no JSON id."
  (org-copilot-normalize-comment
   (list :id (or (plist-get comment :id) fallback-id)
	 :type (org-copilot-llm--symbol (plist-get comment :type) 'inline)
	 :status (org-copilot-llm--symbol (plist-get comment :status) 'active)
	 :source-start (plist-get comment :source_start)
	 :source-end (plist-get comment :source_end)
	 :target-text (plist-get comment :target_text)
	 :anchor-text (plist-get comment :anchor_text)
	 :placement (org-copilot-llm--symbol
		     (plist-get comment :placement) 'after)
	 :line-start (plist-get comment :line_start)
	 :line-end (plist-get comment :line_end)
	 :summary (plist-get comment :summary)
	 :body (plist-get comment :body)
	 :suggestion (org-copilot-llm-optional-string
		      (plist-get comment :suggestion))
	 :rationale (plist-get comment :rationale)
	 :metadata (plist-get comment :metadata))))

(defun org-copilot-llm-comment-from-json (comment)
  "Return normalized AI comment plist from parsed JSON COMMENT."
  (org-copilot-llm--comment-from-json comment))

(defun org-copilot-llm--summary-list (summary key label)
  "Return formatted SUMMARY list at KEY with LABEL, or nil."
  (when-let* ((items (plist-get summary key)))
    (concat label "\n"
	    (mapconcat (lambda (item) (format "- %s" item)) items "\n"))))

(defun org-copilot-llm-summary-message (summary)
  "Return a readable summary message from parsed SUMMARY plist."
  (when summary
    (string-join
     (delq nil
	   (list (org-copilot-llm-optional-string
		  (plist-get summary :message))
		 (org-copilot-llm--summary-list summary :strengths "Strengths")
		 (org-copilot-llm--summary-list summary :risks "Risks")
		 (org-copilot-llm--summary-list summary :next_steps "Next steps")))
     "\n\n")))

(defun org-copilot-llm-parse-review-result (response)
  "Parse strict JSON review RESPONSE into summary and comments."
  (let* ((parsed (json-parse-string response
				    :object-type 'plist
				    :array-type 'list
				    :null-object nil
				    :false-object nil))
	 (comments (plist-get parsed :comments)))
    (list :summary (org-copilot-llm-summary-message
		    (plist-get parsed :summary))
	  :comments (mapcar #'org-copilot-llm-comment-from-json comments))))

(defun org-copilot-llm-chat-comment-from-json (comment)
  "Return normalized chat AI comment from parsed JSON COMMENT.
The model id is kept only as metadata; callers rewrite `:id' before install."
  (let* ((model-id (plist-get comment :id))
	 (normalized (org-copilot-llm--comment-from-json
		      comment (or model-id "model-comment")))
	 (metadata (copy-sequence (or (plist-get normalized :metadata) nil))))
    (when model-id
      (setq metadata (plist-put metadata :model-id model-id)))
    (plist-put normalized :metadata metadata)))

(defun org-copilot-llm--hunk-from-json (hunk)
  "Return suggestion hunk plist from parsed JSON HUNK."
  (list :id (plist-get hunk :id)
	:kind (org-copilot-llm--symbol (plist-get hunk :kind) nil)
	:primary (plist-get hunk :primary)
	:section-title (org-copilot-llm-optional-string
			(plist-get hunk :section_title))
	:section-path (plist-get hunk :section_path)
	:anchor-text (org-copilot-llm-optional-string
		      (plist-get hunk :anchor_text))
	:placement (org-copilot-llm--symbol (plist-get hunk :placement) nil)
	:original (org-copilot-llm-optional-string
		   (plist-get hunk :original))
	:replacement (org-copilot-llm-optional-string
		      (plist-get hunk :replacement))))

(defun org-copilot-llm--candidate-from-json (candidate)
  "Return suggestion candidate plist from parsed JSON CANDIDATE."
  (list :id (plist-get candidate :id)
	:status 'active
	:body (org-copilot-llm-optional-string (plist-get candidate :label))
	:hunks (mapcar #'org-copilot-llm--hunk-from-json
		       (plist-get candidate :hunks))))

(defun org-copilot-llm--suggestion-thread-from-json (thread)
  "Return suggestion thread plist from parsed JSON THREAD."
  (list :intent (org-copilot-llm--symbol (plist-get thread :intent) nil)
	:summary (org-copilot-llm-optional-string (plist-get thread :summary))
	:candidates (mapcar #'org-copilot-llm--candidate-from-json
			    (plist-get thread :suggestions))))

(defun org-copilot-llm-parse-chat-response (response)
  "Parse Org Copilot structured chat RESPONSE.
If RESPONSE is not valid JSON, return it as a plain `:message'."
  (condition-case nil
      (let ((parsed (json-parse-string response
				       :object-type 'plist
				       :array-type 'list
				       :null-object nil
				       :false-object nil)))
	(list :message (or (org-copilot-llm-optional-string
			    (plist-get parsed :message))
			   response)
	      :intent (org-copilot-llm--symbol (plist-get parsed :intent) nil)
	      :suggestion (org-copilot-llm-optional-string
			   (plist-get parsed :suggestion))
	      :summary (org-copilot-llm-optional-string
			(plist-get parsed :summary))
	      :suggestion-threads
	      (mapcar #'org-copilot-llm--suggestion-thread-from-json
		      (plist-get parsed :suggestion_threads))
	      :heading-line (org-copilot-llm-optional-string
			     (plist-get parsed :heading_line))
	      :section-title (org-copilot-llm-optional-string
			      (plist-get parsed :section_title))
	      :section-path (plist-get parsed :section_path)
	      :comments (mapcar #'org-copilot-llm-chat-comment-from-json
				(plist-get parsed :comments))))
    (error
     (list :message response :suggestion nil :comments nil))))

(defun org-copilot-llm-parse-review-response (response)
  "Parse strict JSON review RESPONSE into normalized AI comments."
  (plist-get (org-copilot-llm-parse-review-result response) :comments))

(defun org-copilot-llm--anchor-comment-exact (comment)
  "Return COMMENT anchored by exact target text in the current buffer."
  (let ((target-text (plist-get comment :target-text)))
    (when (and target-text (not (string-empty-p target-text)))
      (save-excursion
	(goto-char (point-min))
	(when (search-forward target-text nil t)
	  (let ((copy (copy-sequence comment)))
	    (setq copy (plist-put copy :source-start (match-beginning 0)))
	    (plist-put copy :source-end (match-end 0))))))))

(defun org-copilot-llm--request-line-range (request line-start line-end)
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

(defun org-copilot-llm--anchor-comment-by-lines (comment request)
  "Return COMMENT anchored by REQUEST-relative line hints, or nil."
  (when-let* ((bounds (org-copilot-llm--request-line-range
		       request
		       (plist-get comment :line-start)
		       (plist-get comment :line-end))))
    (let ((copy (copy-sequence comment)))
      (setq copy (plist-put copy :source-start (car bounds)))
      (plist-put copy :source-end (cdr bounds)))))

(defun org-copilot-llm--anchor-insertion-comment (comment)
  "Return insertion COMMENT anchored by exact anchor text."
  (let ((anchor-text (plist-get comment :anchor-text)))
    (when (and (eq (plist-get comment :type) 'insertion)
	       (org-copilot-llm-optional-string anchor-text))
      (save-excursion
	(goto-char (point-min))
	(when (search-forward anchor-text nil t)
	  (let* ((placement (or (plist-get comment :placement) 'after))
		 (position (if (eq placement 'before)
			       (match-beginning 0)
			     (match-end 0)))
		 (copy (copy-sequence comment)))
	    (setq copy (plist-put copy :source-start position))
	    (plist-put copy :source-end position)))))))

(defun org-copilot-llm-anchored-comment (comment &optional request)
  "Return COMMENT anchored in the current buffer.
Prefer exact `:target-text' matching.  Fall back to REQUEST-relative line hints
when exact matching fails."
  (or (and (plist-get comment :source-start) comment)
      (org-copilot-llm--anchor-insertion-comment comment)
      (org-copilot-llm--anchor-comment-exact comment)
      (org-copilot-llm--anchor-comment-by-lines comment request)
      comment))

(defun org-copilot-llm-anchor-comments (source-buffer comments &optional request)
  "Return COMMENTS anchored in SOURCE-BUFFER.
When REQUEST is non-nil, use its `:start' and `:end' bounds for line-range
fallback anchoring."
  (with-current-buffer source-buffer
    (mapcar (lambda (comment)
	      (org-copilot-llm-anchored-comment comment request))
	    comments)))

(defun org-copilot-llm--anchored-inline-comment-p (comment)
  "Return non-nil when inline COMMENT has an anchored target."
  (and (eq (plist-get comment :type) 'inline)
       (org-copilot-llm-optional-string (plist-get comment :target-text))
       (integerp (plist-get comment :source-start))
       (integerp (plist-get comment :source-end))))

(defun org-copilot-llm--comment-in-section-p (comment context)
  "Return non-nil when COMMENT is allowed by chat CONTEXT."
  (or (not (eq (plist-get context :type) 'section))
      (not (eq (plist-get comment :type) 'inline))
      (let ((start (plist-get context :heading-marker))
	    (end (plist-get context :end-marker)))
	(and (markerp start)
	     (markerp end)
	     (<= (marker-position start) (plist-get comment :source-start))
	     (<= (plist-get comment :source-end) (marker-position end))))))

(defun org-copilot-llm--chat-anchor-request (request)
  "Return anchoring request derived from chat REQUEST."
  (let* ((context (plist-get request :chat-context))
	 (start (plist-get context :heading-marker))
	 (end (plist-get context :end-marker)))
    (if (and (eq (plist-get context :type) 'section)
	     (markerp start)
	     (markerp end))
	(append request (list :start (marker-position start)
			      :end (marker-position end)))
      request)))

(defun org-copilot-llm--anchored-insertion-comment-p (comment)
  "Return non-nil when insertion COMMENT has an executable anchor."
  (and (eq (plist-get comment :type) 'insertion)
       (org-copilot-llm-optional-string (plist-get comment :anchor-text))
       (org-copilot-llm-optional-string (plist-get comment :suggestion))
       (memq (plist-get comment :placement) '(before after))
       (integerp (plist-get comment :source-start))
       (integerp (plist-get comment :source-end))))

(defun org-copilot-llm--comment-installable-p (comment context)
  "Return non-nil when chat COMMENT may be installed in CONTEXT."
  (and (memq (plist-get comment :type) '(inline scope insertion))
       (or (eq (plist-get comment :type) 'scope)
	   (org-copilot-llm--anchored-inline-comment-p comment)
	   (org-copilot-llm--anchored-insertion-comment-p comment))
       (org-copilot-llm--comment-in-section-p comment context)))

(defun org-copilot-llm--review-comment-installable-p (comment)
  "Return non-nil when review COMMENT may be installed."
  (and (memq (plist-get comment :type) '(inline scope insertion))
       (or (eq (plist-get comment :type) 'scope)
	   (org-copilot-llm--anchored-inline-comment-p comment)
	   (org-copilot-llm--anchored-insertion-comment-p comment))))

(defun org-copilot-llm--next-ai-comment-id ()
  "Return the next available `ai-N' id in the current buffer session."
  (let ((max-id 0))
    (dolist (comment (org-copilot-comments))
      (when-let* ((id (plist-get comment :id)))
	(when (string-match "\\`ai-\\([0-9]+\\)\\'" id)
	  (setq max-id (max max-id (string-to-number (match-string 1 id)))))))
    (format "ai-%d" (1+ max-id))))

(defun org-copilot-llm--rewrite-chat-comment-id (comment prompt)
  "Return COMMENT with a locally generated id and chat PROMPT metadata."
  (let* ((copy (copy-sequence comment))
	 (metadata (copy-sequence (or (plist-get copy :metadata) nil))))
    (setq metadata (plist-put metadata :source 'chat))
    (setq metadata (plist-put metadata :prompt prompt))
    (setq copy (plist-put copy :metadata metadata))
    (plist-put copy :id (org-copilot-llm--next-ai-comment-id))))

(defun org-copilot-llm--chat-install-summary
    (installed skipped-unanchored skipped-limit &optional reachable hidden)
  "Return factual summary for chat comment installation counts."
  (let ((reachable (or reachable installed))
	(hidden (or hidden 0)))
    (when (or (> installed 0) (> skipped-unanchored 0) (> skipped-limit 0))
      (string-join
       (delq nil
	     (list (when (> installed 0)
		     (if (> hidden 0)
			 (format "Installed %d reachable comment%s; %d hidden."
				 reachable (if (= reachable 1) "" "s") hidden)
		       (format "Installed %d comment%s."
			       installed (if (= installed 1) "" "s"))))
		   (when (> skipped-unanchored 0)
		     (format "Skipped %d unanchored comment%s."
			     skipped-unanchored
			     (if (= skipped-unanchored 1) "" "s")))
		   (when (> skipped-limit 0)
		     (format "Skipped %d comment%s over the per-response limit."
			     skipped-limit (if (= skipped-limit 1) "" "s")))))
       " "))))

(defun org-copilot-llm-append-chat-install-summary (message result)
  "Append chat comment install RESULT footer to MESSAGE when needed."
  (let ((summary (and result
		      (org-copilot-llm--chat-install-summary
		       (or (plist-get result :installed) 0)
		       (or (plist-get result :skipped-unanchored) 0)
		       (or (plist-get result :skipped-limit) 0)
		       (plist-get result :reachable)
		       (plist-get result :hidden)))))
    (if summary
	(string-join (delq nil (list (org-copilot-llm-optional-string message)
				     summary))
		     "\n\n")
      message)))

(defun org-copilot-install-chat-comments (source-buffer comments request prompt)
  "Install chat-generated COMMENTS for SOURCE-BUFFER.
REQUEST is the originating chat request, and PROMPT is the user's message.
Return a plist with install counts and a reachability receipt."
  (let* ((limit org-copilot-chat-max-comments)
	 (limited (seq-take comments limit))
	 (skipped-limit (max 0 (- (length comments) (length limited))))
	 (anchor-request (org-copilot-llm--chat-anchor-request request))
	 (context (plist-get request :chat-context))
	 (anchored (org-copilot-llm-anchor-comments
		    source-buffer limited anchor-request))
	 (installable (cl-remove-if-not
		       (lambda (comment)
			 (org-copilot-llm--comment-installable-p comment context))
		       anchored))
	 (skipped-unanchored (- (length anchored) (length installable)))
	 (installed 0)
	 installed-ids)
    (when installable
      (with-current-buffer source-buffer
	(dolist (comment installable)
	  (let ((installed-comment
		 (org-copilot-add-comment
		  (org-copilot-llm--rewrite-chat-comment-id comment prompt))))
	    (push (org-copilot-comment-id installed-comment) installed-ids)
	    (setq installed (1+ installed))))
	(org-copilot-mode 1)
	(when (fboundp 'org-copilot-refresh-overlays)
	  (org-copilot-refresh-overlays))
	(if org-copilot-chat-open-panel-on-comments
	    (save-selected-window
	      (org-copilot-open))
	  (when (fboundp 'org-context-panel-refresh)
	    (org-context-panel-refresh)))
	(when (and (boundp 'org-copilot-chat-buffer-name)
		   (get-buffer org-copilot-chat-buffer-name))
	  (with-current-buffer org-copilot-chat-buffer-name
	    (force-mode-line-update)))))
    (let ((reachable (with-current-buffer source-buffer
		       (cl-count-if
			(lambda (comment)
			  (member (org-copilot-comment-id comment) installed-ids))
			(org-copilot-comments)))))
      (list :installed installed
	    :reachable reachable
	    :hidden (max 0 (- installed reachable))
	    :skipped-unanchored skipped-unanchored
	    :skipped-limit skipped-limit))))

(defun org-copilot-install-review-comments (comments)
  "Install normalized review COMMENTS into the current Org Copilot session."
  (dolist (comment (cl-remove-if-not
		    #'org-copilot-llm--review-comment-installable-p
		    comments))
    (org-copilot-add-comment comment))
  (org-copilot-mode 1)
  (when (fboundp 'org-copilot-refresh-overlays)
    (org-copilot-refresh-overlays))
  (org-copilot-comments))

;;;###autoload
(defun org-copilot-review-dwim ()
  "Review the active region or current Org subtree with the configured adapter."
  (interactive)
  (let* ((request (org-copilot-review-request-dwim))
	 (comments (org-copilot--request-review request)))
    (org-copilot-install-review-comments comments)))

(provide 'org-copilot-llm)
;;; org-copilot-llm.el ends here
