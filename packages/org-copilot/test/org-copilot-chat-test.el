;;; org-copilot-chat-test.el --- Chat tests for org-copilot -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for Org Copilot bottom chat skeleton and session transcript state.

;;; Code:

(require 'ert)
(require 'org)
(require 'org-comments-store)
(require 'org-suggestions)
(require 'org-copilot)
(require 'org-copilot-chat)

(ert-deftest org-copilot-chat-submits-captured-review-without-another-request-or-focus-change ()
  "Review uses ordinary turn handling while retaining the original request fence."
  (with-temp-buffer
    (org-mode) (insert "* Draft\nClaim.\n")
    (let* ((source (current-buffer))
	   (request (org-copilot-start-request
		     (list :source-buffer source :message "Review the selected text."
			   :source-content (buffer-string) :context-id "review:subtree")))
	   captured)
      (setq-local org-copilot-chat-focus-comment-id "existing-focus")
      (setq-local org-copilot-chat-function (lambda (value) (setq captured value) nil))
      (cl-letf (((symbol-function 'org-copilot-chat--buffer) #'ignore)
		((symbol-function 'org-copilot-chat-scroll-to-last-message) #'ignore))
	(org-copilot-submit-chat-request request))
      (should (eq (plist-get captured :request-token) (plist-get request :request-token)))
      (should (= (length org-copilot--requests) 1))
      (should (equal org-copilot-chat-focus-comment-id "existing-focus"))
      (should (equal (plist-get captured :source-content) "* Draft\nClaim.\n"))
      (org-copilot-cancel-request captured))))

(ert-deftest org-copilot-chat-obsolete-captured-review-cannot-submit-another-turn ()
  "A cancelled captured review cannot append messages or dispatch an adapter."
  (with-temp-buffer
    (org-mode)
    (let ((request (org-copilot-start-request (list :source-buffer (current-buffer) :message "Review."))))
      (org-copilot-cancel-request request)
      (cl-letf (((symbol-function 'org-copilot-chat-send)
		 (lambda (&rest _) (error "Obsolete review was submitted"))))
	(should-not (org-copilot-submit-chat-request request))))))

(ert-deftest org-copilot-chat-opens-bottom-view-for-source ()
  "Opening chat creates a bottom-view buffer associated with the source."
  (let ((source (generate-new-buffer " *org copilot chat source*")))
    (unwind-protect
	(with-current-buffer source
	  (org-mode)
	  (insert "* Heading\nBody.\n")
	  (let ((buffer (org-copilot-chat)))
	    (should (buffer-live-p buffer))
	    (with-current-buffer source
	      (should (equal (buffer-string) "* Heading\nBody.\n")))
	    (with-current-buffer buffer
	      (should (derived-mode-p 'org-copilot-chat-mode))
	      (should (eq context-panels-source-buffer source))
	      (should (eq context-panels-view-id 'copilot-chat))
	      (should (string-match-p "Org Copilot Chat" (buffer-string)))
	      (should (string-match-p "Ask Copilot about the full document"
				      (buffer-string))))))
      (when (buffer-live-p source)
	(kill-buffer source)))))

(ert-deftest org-copilot-chat-fixed-input-keeps-draft-through-a-reply ()
  "Sending and receiving use real chat state without hiding or erasing a draft."
  (save-window-excursion
    (let ((source (generate-new-buffer " *copilot fixed input source*"))
	  input request)
      (unwind-protect
	  (with-current-buffer source
	    (org-mode)
	    (insert "* Draft\nSelected paragraph.\n")
	    (setq-local org-copilot-chat-buffer-name "*Copilot fixed input test*")
	    (setq-local org-copilot-chat-function
			(lambda (captured) (setq request captured) nil))
	    (org-copilot-chat)
	    (setq input (org-copilot-chat-input-buffer source))
	    (should (eq (window-buffer (selected-window)) input))
	    (with-current-buffer input
	      (org-copilot-chat--set-prompt-text "Improve the paragraph.")
	      (org-copilot-chat-send-current-input)
	      (should (equal "" (org-copilot-chat--prompt-text)))
	      (org-copilot-chat--set-prompt-text "My next unsent thought."))
	    (should (equal "Improve the paragraph." (plist-get request :message)))
	    (with-current-buffer source
	      (should (org-copilot-begin-completion request))
	      (org-copilot-add-chat-message
	       'assistant (mapconcat (lambda (_) "A long answer line.") (number-sequence 1 60) "\n")
	       nil nil request)
	      (org-copilot-chat--buffer source)
	      (org-copilot-chat-scroll-to-last-message source 'assistant)
	      (org-copilot-finish-request request))
	    (with-current-buffer input
	      (should (equal "My next unsent thought." (org-copilot-chat--prompt-text)))
	      (let ((window (get-buffer-window input)))
		(should window)
		(should (<= (window-start window)
			    (marker-position org-copilot-chat--prompt-start)
			    (window-end window t)))))
	    (with-current-buffer source
	      (should (equal "* Draft\nSelected paragraph.\n" (buffer-string)))
	      (should (= 1 (cl-count 'user (org-copilot-chat-messages)
				     :key (lambda (message) (plist-get message :role)))))
	      (context-panels-close-bottom-view)
	      (org-copilot-chat))
	    (should (eq input (org-copilot-chat-input-buffer source)))
	    (with-current-buffer input
	      (should (equal "My next unsent thought." (org-copilot-chat--prompt-text)))))
	(when (buffer-live-p source) (kill-buffer source))
	(when (buffer-live-p input) (kill-buffer input))))))

(ert-deftest org-copilot-chat-submit-error-keeps-the-unsent-text ()
  "A synchronous adapter failure cannot erase the text the user was typing."
  (let ((source (generate-new-buffer " *failed input source*")) input)
    (unwind-protect
	(progn
	  (with-current-buffer source
	    (org-mode)
	    (setq-local org-copilot-chat-function (lambda (_) (error "Fixture adapter failed"))))
	  (setq input (org-copilot-chat-input-buffer source))
	  (with-current-buffer input
	    (org-copilot-chat--set-prompt-text "Keep this text.")
	    (should-error (org-copilot-chat-send-current-input))
	    (should (equal "Keep this text." (org-copilot-chat--prompt-text)))))
      (dolist (buffer (list source input))
	(when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest org-copilot-chat-input-drafts-belong-to-their-source ()
  "Moving between documents cannot move an unsent draft to another target."
  (let ((first (generate-new-buffer " *copilot first draft source*"))
	(second (generate-new-buffer " *copilot second draft source*"))
	a b)
    (unwind-protect
	(save-window-excursion
	  (with-current-buffer first (org-mode))
	  (with-current-buffer second (org-mode))
	  (setq a (org-copilot-chat-input-buffer first)
		b (org-copilot-chat-input-buffer second))
	  (with-current-buffer a (org-copilot-chat--set-prompt-text "First draft."))
	  (with-current-buffer b (org-copilot-chat--set-prompt-text "Second draft."))
	  (with-current-buffer first (org-copilot-chat))
	  (should (eq a (window-buffer (selected-window))))
	  (with-current-buffer second (org-copilot-chat))
	  (should (eq b (window-buffer (selected-window))))
	  (with-current-buffer a
	    (should (equal "First draft." (org-copilot-chat--prompt-text)))
	    (should (eq first org-copilot-chat-source-buffer)))
	  (with-current-buffer b
	    (should (equal "Second draft." (org-copilot-chat--prompt-text)))
	    (should (eq second org-copilot-chat-source-buffer))))
      (dolist (buffer (list first second a b))
	(when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest org-copilot-chat-status-line-summarizes-comments ()
  "Chat status line compactly summarizes active source comments."
  (let ((source (generate-new-buffer " *org copilot chat status source*")))
    (unwind-protect
	(progn
	  (with-current-buffer source
	    (org-mode)
	    (org-copilot-add-comment
	     (list :id "ai-1" :type 'scope :status 'active :body "Scope."))
	    (org-copilot-add-comment
	     (list :id "ai-2" :type 'inline :status 'active :body "Inline."))
	    (setq org-copilot-chat-focus-comment-id "ai-1"))
	  (with-temp-buffer
	    (org-copilot-chat-mode)
	    (setq context-panels-source-buffer source)
	    (should (equal (org-copilot-chat--status-line)
			   "2 comments · 1 scope · ai-1"))))
      (when (buffer-live-p source)
	(kill-buffer source)))))

(ert-deftest org-copilot-chat-focuses-current-comment ()
  "Opening chat from a context-panel row focuses that AI comment."
  (let ((source (generate-new-buffer " *org copilot chat source*")))
    (unwind-protect
	(let ((comment nil))
	  (with-current-buffer source
	    (org-mode)
	    (insert "Alpha sentence.\n")
	    (org-copilot-add-chat-message 'user "Earlier general chat")
	    (setq comment
		  (org-copilot-add-comment
		   (list :id "ai-1"
			 :source-start (point-min)
			 :source-end (+ (point-min) 5)
			 :target-text "Alpha"
			 :body "Clarify this sentence."
			 :suggestion "Alpha."
			 :status 'active))))
	  (with-temp-buffer
	    (context-panels-buffer-mode)
	    (setq context-panels-source-buffer source)
	    (let ((inhibit-read-only t))
	      (insert "AI [active] Clarify this sentence.\n"))
	    (let ((inhibit-read-only t))
	      (add-text-properties (point-min) (point-max)
				   `(context-panels-item ,comment)))
	    (goto-char (point-min))
	    (let ((buffer (org-copilot-chat)))
	      (with-current-buffer source
		(should (equal org-copilot-chat-focus-comment-id "ai-1")))
	      (with-current-buffer buffer
		(should (string-match-p "Org Copilot Chat — .* · 💬 Comment ai-1 · active"
					(buffer-string)))
		(should-not (string-match-p "^Context" (buffer-string)))
		(should-not (string-match-p "Clarify this sentence" (buffer-string)))
		(should-not (string-match-p "Comment: Clarify" (buffer-string)))
		(should-not (string-match-p "Suggestion:" (buffer-string)))
		(should (string-match-p "Earlier general chat" (buffer-string)))
		(should-not (string-match-p "Ask Copilot to revise"
					    (buffer-string)))))))
      (when (buffer-live-p source)
	(kill-buffer source)))))

(ert-deftest org-copilot-chat-keeps-one-session-per-source-buffer ()
  "Chat messages are stored per source buffer session."
  (let ((source-a (generate-new-buffer " *org copilot chat source a*"))
	(source-b (generate-new-buffer " *org copilot chat source b*")))
    (unwind-protect
	(progn
	  (with-current-buffer source-a
	    (org-mode)
	    (org-copilot-add-chat-message 'user "Message A"))
	  (with-current-buffer source-b
	    (org-mode)
	    (org-copilot-add-chat-message 'user "Message B"))
	  (with-current-buffer source-a
	    (should (equal (plist-get (car (org-copilot-chat-messages)) :content)
			   "Message A")))
	  (with-current-buffer source-b
	    (should (equal (plist-get (car (org-copilot-chat-messages)) :content)
			   "Message B"))))
      (mapc (lambda (buffer)
	      (when (buffer-live-p buffer)
		(kill-buffer buffer)))
	    (list source-a source-b)))))

(ert-deftest org-copilot-chat-opens-at-editable-prompt ()
  "Opening chat selects the chat buffer and places point at an editable prompt."
  (let ((source (generate-new-buffer " *org copilot chat source*")))
    (unwind-protect
	(progn
	  (let ((buffer nil))
	    (with-current-buffer source
	      (org-mode)
	      (insert "* Heading\nBody.\n")
	      (org-copilot-chat)
	      (setq buffer (org-copilot-chat-input-buffer source)))
	    (with-current-buffer buffer
	      (should (derived-mode-p 'org-copilot-chat-mode))
	      (should-not buffer-read-only)
	      (should (= (point) org-copilot-chat--prompt-start))
	      (should-not (get-text-property (point) 'read-only))
	      (should (get-text-property (1- (point)) 'read-only))
	      (insert "Can I type here?")
	      (should (string-suffix-p "You: Can I type here?"
				       (buffer-string))))))
      (when (buffer-live-p source)
	(kill-buffer source)))))

(ert-deftest org-copilot-chat-renders-distinct-faces ()
  "Chat rendering visually distinguishes metadata, dialogue, and prompt."
  (with-temp-buffer
    (org-mode)
    (let ((source (current-buffer)))
      (org-copilot-add-chat-message 'user "Hello")
      (org-copilot-add-chat-message 'assistant "Hi")
      (with-current-buffer (org-copilot-chat--buffer source)
	(goto-char (point-min))
	(should (eq (get-text-property (point) 'face)
		    'org-copilot-chat-metadata-face))
	(search-forward "You")
	(should (eq (get-text-property (match-beginning 0) 'face)
		    'org-copilot-chat-you-face))
	(search-forward "Copilot")
	(should (eq (get-text-property (match-beginning 0) 'face)
		    'org-copilot-chat-copilot-face))
	(should buffer-read-only))
      (with-current-buffer (org-copilot-chat-input-buffer source)
	(should (eq (get-text-property (point-min) 'face)
		    'org-copilot-chat-you-face))))))

(ert-deftest org-copilot-chat-prompt-field-sends-and-clears ()
  "RET-style prompt sending records input and clears the prompt field."
  (with-temp-buffer
    (org-mode)
    (let ((source (current-buffer)))
      (with-current-buffer (org-copilot-chat-input-buffer source)
	(insert "Explain this")
	(org-copilot-chat-send-current-input)
	(should (equal "" (org-copilot-chat--prompt-text))))
      (with-current-buffer (org-copilot-chat--buffer source)
	(should (string-match-p "You:\n  Explain this" (buffer-string))))
      (should (equal (plist-get (car (org-copilot-chat-messages)) :content)
		     "Explain this")))))

(ert-deftest org-copilot-chat-ret-outside-prompt-moves-to-prompt ()
  "RET outside the prompt field moves point to the editable prompt."
  (with-temp-buffer
    (org-mode)
    (let ((source (current-buffer)))
      (org-copilot-chat)
      (with-current-buffer (org-copilot-chat--buffer source)
	(goto-char (point-min))
	(org-copilot-chat-return-dwim)
	(should (eq (window-buffer (selected-window)) (org-copilot-chat-input-buffer source))))
      (should-not (org-copilot-chat-messages)))))

(ert-deftest org-copilot-chat-ret-inside-prompt-sends-input ()
  "RET inside the prompt field sends the current input."
  (with-temp-buffer
    (org-mode)
    (let ((source (current-buffer)))
      (with-current-buffer (org-copilot-chat-input-buffer source)
	(insert "Explain this")
	(org-copilot-chat-return-dwim))
      (with-current-buffer (org-copilot-chat--buffer source)
	(should (string-match-p "You:\n  Explain this" (buffer-string))))
      (should (equal (plist-get (car (org-copilot-chat-messages)) :content)
		     "Explain this")))))

(ert-deftest org-copilot-chat-full-document-clears-comment-focus ()
  "Full-document chat clears comment focus and leaves source targets dim."
  (with-temp-buffer
    (org-mode)
    (insert "Alpha sentence.\n")
    (org-copilot-add-comment
     (list :id "ai-1"
	   :status 'active
	   :source-start (point-min)
	   :source-end (+ (point-min) 5)
	   :target-text "Alpha"
	   :suggestion "Alpha."))
    (setq org-copilot-chat-focus-comment-id "ai-1")
    (let ((source (current-buffer)))
      (org-copilot-chat-full-document)
      (with-current-buffer source
	(should-not org-copilot-chat-focus-comment-id)
	(should (eq (overlay-get (car org-copilot--overlays) 'face)
		    'org-copilot-target-dim-face))))
    (with-current-buffer (get-buffer org-copilot-chat-buffer-name)
      (should (string-match-p "🌐 Full document" (buffer-string)))
      (should buffer-read-only))
    (should (string-prefix-p "You: " (buffer-string)))))

(ert-deftest org-copilot-chat-section-context-uses-current-subtree ()
  "Section chat context uses the current Org subtree and section prompt marker."
  (with-temp-buffer
    (org-mode)
    (insert "* Intro\nIntro body.\n* Methods\nMethod body.\n")
    (goto-char (point-min))
    (search-forward "Method body")
    (let ((source (current-buffer)))
      (org-copilot-chat-section)
      (with-current-buffer source
	(should (eq (plist-get org-copilot-chat-context :type) 'section))
	(should (equal (plist-get org-copilot-chat-context :section-title)
		       "Methods"))))
    (with-current-buffer (get-buffer org-copilot-chat-buffer-name)
      (should (string-match-p "§ Section: Methods" (buffer-string)))
      (should buffer-read-only))
    (should (string-prefix-p "You: " (buffer-string)))))

(ert-deftest org-copilot-chat-section-request-uses-full-document-and-focused-subtree ()
  "Section chat requests include full source plus focused subtree content."
  (with-temp-buffer
    (org-mode)
    (insert "* Intro\nIntro body.\n* Methods\nMethod body.\n")
    (goto-char (point-min))
    (search-forward "Method body")
    (org-copilot-chat--set-context
     (current-buffer)
     (org-copilot-chat--section-context-at-point))
    (org-copilot-add-chat-message
     'user "Earlier section" nil
     (org-copilot-chat--context-id org-copilot-chat-context))
    (org-copilot-add-chat-message 'user "Earlier full document" nil nil)
    (org-copilot-add-chat-message 'pending "…")
    (let ((request (org-copilot-chat--request (current-buffer) "Explain")))
      (should (eq (plist-get (plist-get request :chat-context) :type)
		  'section))
      (should (string-match-p "Intro body" (plist-get request :source-content)))
      (should (string-match-p "\\* Methods" (plist-get request :section-content)))
      (should-not (string-match-p "Intro body"
				  (plist-get request :section-content)))
      (should (equal (mapcar (lambda (message) (plist-get message :content))
			     (plist-get request :messages))
		     '("Earlier section" "Earlier full document"))))))

(ert-deftest org-copilot-chat-request-full-document-keeps-conversation-history ()
  "Removing comment focus preserves its discussion in model history."
  (with-temp-buffer
    (org-mode)
    (insert "Alpha sentence.\n")
    (org-copilot-add-comment (list :id "ai-1" :status 'active))
    (org-copilot-add-chat-message 'user "General" nil)
    (org-copilot-add-chat-message 'user "Focused" "ai-1")
    (let ((request (org-copilot-chat--request (current-buffer) "Explain")))
      (should-not (plist-get request :focus-comment-id))
      (should (string-match-p "Alpha sentence" (plist-get request :source-content)))
      (should (equal (mapcar (lambda (message) (plist-get message :content))
			     (plist-get request :messages))
		     '("General" "Focused"))))))

(ert-deftest org-copilot-chat-navigation-scrolls-source-to-target ()
  "Focusing comments scrolls source windows and moves point to target."
  (let ((source (generate-new-buffer " *org copilot scroll source*")))
    (unwind-protect
	(progn
	  (delete-other-windows)
	  (set-window-buffer (selected-window) source)
	  (with-current-buffer source
	    (org-mode)
	    (dotimes (index 80)
	      (insert (format "Line %02d\n" index)))
	    (goto-char (point-min))
	    (let ((target (save-excursion
			    (goto-char (point-min))
			    (forward-line 70)
			    (point))))
	      (org-copilot-add-comment
	       (list :id "ai-1"
		     :status 'active
		     :source-start target
		     :source-end target))))
	  (let ((target (plist-get (car (with-current-buffer source
					  (org-copilot-visible-comments)))
				   :source-start))
		(original-start (window-start (selected-window))))
	    (with-current-buffer source
	      (org-copilot-chat-focus-next-comment))
	    (should (= (with-current-buffer source (point)) target))
	    (should (> (window-start (get-buffer-window source t)) original-start))))
      (delete-other-windows)
      (when (buffer-live-p source)
	(kill-buffer source)))))

(ert-deftest org-copilot-chat-navigation-focuses-next-and-previous-comments ()
  "Chat navigation cycles through non-dismissed AI comments."
  (with-temp-buffer
    (org-mode)
    (org-copilot-add-comment (list :id "ai-1" :status 'active))
    (org-copilot-add-comment (list :id "ai-2" :status 'accepted))
    (org-copilot-add-comment (list :id "ai-3" :status 'dismissed))
    (org-copilot-chat-focus-next-comment)
    (should (equal org-copilot-chat-focus-comment-id "ai-1"))
    (org-copilot-chat-focus-next-comment)
    (should (equal org-copilot-chat-focus-comment-id "ai-2"))
    (org-copilot-chat-focus-next-comment)
    (should (equal org-copilot-chat-focus-comment-id "ai-1"))
    (org-copilot-chat-focus-previous-comment)
    (should (equal org-copilot-chat-focus-comment-id "ai-2"))))

(ert-deftest org-copilot-chat-navigation-syncs-comments-panel ()
  "Chat navigation propagates focus to comments side-panel sync."
  (with-temp-buffer
    (org-mode)
    (org-copilot-add-comment
     (list :id "ai-1" :status 'active :source-start 1 :source-end 1))
    (let (synced)
      (cl-letf (((symbol-function 'org-comments-context-panel-focus-comment)
		 (lambda (source comment &optional skip-copilot)
		   (setq synced (list source (plist-get comment :id)
				      skip-copilot)))))
	(org-copilot-chat-focus-next-comment)
	(should (equal synced (list (current-buffer) "ai-1" t)))))))

(ert-deftest org-copilot-chat-next-prev-commands-change-focus ()
  "The /next and /prev chat commands navigate focused comments."
  (with-temp-buffer
    (org-mode)
    (org-copilot-add-comment (list :id "ai-1" :status 'active))
    (org-copilot-add-comment (list :id "ai-2" :status 'active))
    (org-copilot-chat-send "/next")
    (should (equal org-copilot-chat-focus-comment-id "ai-1"))
    (org-copilot-chat-send "/next")
    (should (equal org-copilot-chat-focus-comment-id "ai-2"))
    (org-copilot-chat-send "/prev")
    (should (equal org-copilot-chat-focus-comment-id "ai-1"))))

(ert-deftest org-copilot-chat-next-navigates-durable-sidecar-suggestions ()
  "The /next command navigates restored durable linked suggestions."
  (let* ((directory (make-temp-file "org-copilot-chat-durable-next" t))
	 (source-file (expand-file-name "draft.org" directory))
	 (alpha-candidate (list :id "ai-1"
				:status 'active
				:hunks (list (list :id "h1"
						   :kind 'replace
						   :primary t
						   :original "Alpha typo"
						   :replacement "Alpha fixed"))))
	 (beta-candidate (list :id "ai-2"
			       :status 'active
			       :hunks (list (list :id "h2"
						  :kind 'replace
						  :primary t
						  :original "Beta typo"
						  :replacement "Beta fixed"))))
	 (threads (list (list :id "ai-thread-1"
			      :provider "org-copilot"
			      :summary "Fix Alpha"
			      :candidates (list alpha-candidate))
			(list :id "ai-thread-2"
			      :provider "org-copilot"
			      :summary "Fix Beta"
			      :candidates (list beta-candidate)))))
    (unwind-protect
	(with-current-buffer (find-file-noselect source-file)
	  (erase-buffer)
	  (insert "Alpha typo.\nBeta typo.\n")
	  (save-buffer)
	  (org-mode)
	  (org-suggestions-write-sidecar source-file threads)
	  (org-copilot-restore-suggestion-comments)
	  (setq org-copilot--comments nil)
	  (should (assoc "/next" (org-copilot-chat--available-slash-commands
				  (current-buffer))))
	  (let ((navigable (org-copilot-chat--navigable-comments (current-buffer))))
	    (org-copilot-chat-send "/next")
	    (should (equal org-copilot-chat-focus-comment-id
			   (plist-get (car navigable) :id)))
	    (org-copilot-chat-send "/next")
	    (should (equal org-copilot-chat-focus-comment-id
			   (plist-get (cadr navigable) :id)))))
      (when-let* ((buffer (find-buffer-visiting source-file)))
	(kill-buffer buffer))
      (delete-directory directory t))))

(ert-deftest org-copilot-chat-accept-mutates-durable-sidecars ()
  "The /accept command accepts durable linked suggestions."
  (let* ((directory (make-temp-file "org-copilot-chat-durable-accept" t))
	 (source-file (expand-file-name "draft.org" directory))
	 (candidate (list :id "ai-1"
			  :status 'active
			  :hunks (list (list :id "h1"
					     :kind 'replace
					     :primary t
					     :original "Alpha typo."
					     :replacement "Alpha fixed."))))
	 (thread (list :id "ai-thread-1"
		       :provider "org-copilot"
		       :summary "Fix Alpha"
		       :candidates (list candidate))))
    (unwind-protect
	(with-current-buffer (find-file-noselect source-file)
	  (erase-buffer)
	  (insert "Alpha typo.\n")
	  (save-buffer)
	  (org-mode)
	  (org-suggestions-write-sidecar source-file (list thread))
	  (org-copilot-restore-suggestion-comments)
	  (let ((comment (car (org-copilot-visible-comments))))
	    (setq org-copilot-chat-focus-comment-id (plist-get comment :id))
	    (org-copilot-chat-send "/accept")
	    (let* ((loaded-thread (car (org-suggestions-load-sidecar source-file)))
		   (loaded-candidate (car (plist-get loaded-thread :candidates))))
	      (should (equal (buffer-string) "Alpha fixed.\n"))
	      (should (eq (plist-get loaded-candidate :status) 'accepted)))))
      (when-let* ((buffer (find-buffer-visiting source-file)))
	(kill-buffer buffer))
      (delete-directory directory t))))

(ert-deftest org-copilot-chat-dismiss-mutates-durable-sidecars ()
  "The /dismiss command marks durable linked suggestions dismissed."
  (let* ((directory (make-temp-file "org-copilot-chat-durable-dismiss" t))
	 (source-file (expand-file-name "draft.org" directory))
	 (candidate (list :id "ai-1"
			  :status 'active
			  :hunks (list (list :id "h1"
					     :kind 'replace
					     :primary t
					     :original "Alpha typo."
					     :replacement "Alpha fixed."))))
	 (thread (list :id "ai-thread-1"
		       :provider "org-copilot"
		       :summary "Fix Alpha"
		       :candidates (list candidate))))
    (unwind-protect
	(with-current-buffer (find-file-noselect source-file)
	  (erase-buffer)
	  (insert "Alpha typo.\n")
	  (save-buffer)
	  (org-mode)
	  (org-suggestions-write-sidecar source-file (list thread))
	  (org-copilot-restore-suggestion-comments)
	  (let ((comment (car (org-copilot-visible-comments))))
	    (setq org-copilot-chat-focus-comment-id (plist-get comment :id))
	    (org-copilot-chat-send "/dismiss")
	    (let* ((visible (org-copilot-visible-comments))
		   (loaded-thread (car (org-suggestions-load-sidecar source-file)))
		   (loaded-candidate (car (plist-get loaded-thread :candidates))))
	      (should (eq (plist-get loaded-candidate :status) 'dismissed))
	      (should (eq (plist-get (car visible) :status) 'dismissed)))))
      (when-let* ((buffer (find-buffer-visiting source-file)))
	(kill-buffer buffer))
      (delete-directory directory t))))

(ert-deftest org-copilot-chat-available-slash-commands-are-context-aware ()
  "Slash command completion only exposes commands valid for current focus."
  (with-temp-buffer
    (org-mode)
    (should (assoc "/doctor" (org-copilot-chat--available-slash-commands
			      (current-buffer))))
    (org-copilot-add-comment (list :id "ai-1" :status 'active))
    (should (assoc "/next" (org-copilot-chat--available-slash-commands
			    (current-buffer))))
    (should-not (assoc "/accept" (org-copilot-chat--available-slash-commands
				  (current-buffer))))
    (setq org-copilot-chat-focus-comment-id "ai-1")
    (should (assoc "/dismiss" (org-copilot-chat--available-slash-commands
			       (current-buffer))))
    (should (assoc "/resolve" (org-copilot-chat--available-slash-commands
			       (current-buffer))))
    (should-not (assoc "/accept" (org-copilot-chat--available-slash-commands
				  (current-buffer))))
    (org-copilot-update-comment
     (plist-put (org-copilot-find-comment "ai-1") :suggestion "Alpha."))
    (should-not (assoc "/accept" (org-copilot-chat--available-slash-commands
				  (current-buffer))))
    (org-copilot-update-comment
     (org-copilot-comment-with-status (org-copilot-find-comment "ai-1") 'stale))
    (should-not (assoc "/accept" (org-copilot-chat--available-slash-commands
				  (current-buffer))))))

(ert-deftest org-copilot-chat-available-slash-commands-include-undo-for-accepted-rollback ()
  "Slash command completion exposes undo only for accepted rollback comments."
  (with-temp-buffer
    (org-mode)
    (org-copilot-add-comment
     (list :id "ai-1"
	   :status 'accepted
	   :target-text "Alpha."
	   :accepted-text "Alpha."
	   :original-target-text "Alpha sentence."))
    (setq org-copilot-chat-focus-comment-id "ai-1")
    (should (assoc "/undo" (org-copilot-chat--available-slash-commands
			    (current-buffer))))))

(ert-deftest org-copilot-chat-resolve-command-resolves-and-advances ()
  "The /resolve command resolves focused comment and focuses the next one."
  (with-temp-buffer
    (org-mode)
    (org-copilot-add-comment
     (list :id "ai-1" :status 'active :sidecar-file "/tmp/comments.org"))
    (org-copilot-add-comment
     (list :id "ai-2" :status 'active :sidecar-file "/tmp/comments.org"))
    (setq org-copilot-chat-focus-comment-id "ai-1")
    (let (resolved)
      (cl-letf (((symbol-function 'org-copilot-set-durable-comment-status)
		 (lambda (comment status)
		   (setq resolved (list (plist-get comment :id) status))))
		((symbol-function 'org-copilot-chat--refresh-source-ui) #'ignore)
		((symbol-function 'org-copilot-chat--close-stale-diff) #'ignore))
	(org-copilot-chat-send "/resolve")
	(should (equal resolved '("ai-1" "RESOLVED")))
	(should (equal org-copilot-chat-focus-comment-id "ai-2"))))))

(ert-deftest org-copilot-chat-doctor-command-appends-health-report ()
  "The /doctor command appends a local Org Copilot health report."
  (with-temp-buffer
    (org-mode)
    (let ((source (current-buffer)))
      (with-current-buffer (org-copilot-chat--buffer source)
	(org-copilot-chat-send "/doctor"))
      (let ((messages (org-copilot-chat-messages)))
	(should (= (length messages) 1))
	(should (eq (plist-get (car messages) :role) 'assistant))
	(should (string-match-p "Org Copilot Doctor"
				(plist-get (car messages) :content)))
	(should (string-match-p "chat adapter configured"
				(plist-get (car messages) :content)))))))

(ert-deftest org-copilot-chat-slash-completion-executes-selected-command ()
  "Typing slash in an empty prompt completes and executes a slash command."
  (with-temp-buffer
    (org-mode)
    (org-copilot-add-comment (list :id "ai-1" :status 'active))
    (org-copilot-add-comment (list :id "ai-2" :status 'active))
    (let ((source (current-buffer)))
      (with-current-buffer (org-copilot-chat--buffer source)
	(cl-letf (((symbol-function 'completing-read)
		   (lambda (&rest _args) "/next")))
	  (org-copilot-chat-slash-or-complete)))
      (should (equal org-copilot-chat-focus-comment-id "ai-1")))))

(ert-deftest org-copilot-chat-slash-inserts-when-prompt-is-not-empty ()
  "Typing slash in a non-empty prompt inserts a literal slash."
  (with-temp-buffer
    (org-mode)
    (let ((source (current-buffer)))
      (with-current-buffer (org-copilot-chat-input-buffer source)
	(insert "look ")
	(org-copilot-chat-slash-or-complete)
	(should (string-suffix-p "You: look /" (buffer-string)))))))

(ert-deftest org-copilot-chat-slash-commands-have-annotations ()
  "Implemented slash commands expose completion annotations."
  (should (equal (org-copilot-chat--slash-command-annotation "/accept")
		 "  Accept focused suggestion"))
  (should (assoc "/dismiss" org-copilot-chat-slash-commands))
  (should (equal (org-copilot-chat--slash-command-annotation "/doctor")
		 "  Run Org Copilot health check"))
  (should-not (assoc "/help" org-copilot-chat-slash-commands)))

(ert-deftest org-copilot-chat-slash-commands-have-key-coverage ()
  "Every implemented slash command documents a live chat-mode key."
  (dolist (entry org-copilot-chat-slash-commands)
    (let* ((command (car entry))
	   (keys (cdr (assoc command org-copilot-chat-slash-command-keys))))
      (should keys)
      (should (cl-some (lambda (key)
			 (lookup-key org-copilot-chat-mode-map (kbd key)))
		       keys)))))

(ert-deftest org-copilot-chat-mode-defines-action-keys ()
  "Org Copilot chat mode defines send, focus, and action keys."
  (should (eq (lookup-key org-copilot-chat-mode-map (kbd "RET"))
	      #'org-copilot-chat-return-dwim))
  (should (eq (lookup-key org-copilot-chat-mode-map (kbd "C-c C-c"))
	      #'org-copilot-chat-send-current-input))
  (should (eq (lookup-key org-copilot-chat-mode-map (kbd "/"))
	      #'org-copilot-chat-slash-or-complete))
  (should (eq (lookup-key org-copilot-chat-mode-map (kbd "C-c C-f"))
	      #'org-copilot-chat-goto-prompt))
  (should (eq (lookup-key org-copilot-chat-mode-map (kbd "C-c C-a"))
	      #'org-copilot-chat-accept-focused-suggestion-at-point))
  (should (eq (lookup-key org-copilot-chat-mode-map (kbd "C-c C-d"))
	      #'org-copilot-chat-dismiss-focused-comment-at-point))
  (should (eq (lookup-key org-copilot-chat-mode-map (kbd "C-c C-r"))
	      #'org-copilot-chat-resolve-focused-comment-at-point))
  (should (eq (lookup-key org-copilot-chat-mode-map (kbd "C-c C-n"))
	      #'org-copilot-chat-focus-next-comment))
  (should (eq (lookup-key org-copilot-chat-mode-map (kbd "C-c C-p"))
	      #'org-copilot-chat-focus-previous-comment))
  (should (eq (lookup-key org-copilot-chat-mode-map (kbd "C-c C-u"))
	      #'org-copilot-chat-undo-focused-comment-at-point))
  (should (eq (lookup-key org-copilot-chat-mode-map (kbd "C-c C-g"))
	      #'org-copilot-chat-full-document))
  (should (eq (lookup-key org-copilot-chat-mode-map (kbd "C-c C-x / g"))
	      #'org-copilot-chat-full-document))
  (should (eq (lookup-key org-copilot-chat-mode-map (kbd "C-c C-x / s"))
	      #'org-copilot-chat-section))
  (should (eq (lookup-key org-copilot-chat-mode-map (kbd "C-c C-x / o"))
	      #'org-copilot-open-panels))
  (should (eq (lookup-key org-copilot-chat-mode-map (kbd "C-c C-x / c"))
	      #'org-copilot-chat-clear-session-at-point))
  (should (eq (lookup-key org-copilot-chat-mode-map (kbd "C-c C-x / u"))
	      #'org-copilot-chat-clear-ui-at-point))
  (should (eq (lookup-key org-copilot-chat-mode-map (kbd "C-c C-x / d"))
	      #'org-copilot-chat-doctor-at-point))
  (should (eq (lookup-key org-copilot-chat-mode-map (kbd "C-c C-x / e"))
	      #'org-copilot-chat-erase-session-at-point))
  (should (eq (lookup-key org-copilot-chat-mode-map (kbd "M-a"))
	      #'org-copilot-chat-accept-focused-suggestion-at-point))
  (should (eq (lookup-key org-copilot-chat-mode-map (kbd "M-d"))
	      #'org-copilot-chat-dismiss-focused-comment-at-point))
  (should (eq (lookup-key org-copilot-chat-mode-map (kbd "M-z"))
	      #'org-copilot-chat-resolve-focused-comment-at-point))
  (should (eq (lookup-key org-copilot-chat-mode-map (kbd "M-n"))
	      #'org-copilot-chat-focus-next-comment))
  (should (eq (lookup-key org-copilot-chat-mode-map (kbd "M-p"))
	      #'org-copilot-chat-focus-previous-comment))
  (should (eq (lookup-key org-copilot-chat-mode-map (kbd "M-u"))
	      #'org-copilot-chat-undo-focused-comment-at-point))
  (should (eq (lookup-key org-copilot-chat-mode-map (kbd "M-g"))
	      #'org-copilot-chat-full-document))
  (should (eq (lookup-key org-copilot-chat-mode-map (kbd "M-s"))
	      #'org-copilot-chat-section))
  (should (eq (lookup-key org-copilot-chat-mode-map (kbd "M-<up>"))
	      #'org-copilot-chat-recall-last-prompt)))

(ert-deftest org-copilot-chat-recall-last-prompt-follows-conversation ()
  "M-up recalls the last conversation prompt regardless of editing focus."
  (with-temp-buffer
    (org-mode)
    (let ((source (current-buffer)))
      (org-copilot-add-chat-message 'user "Full document prompt" nil nil)
      (org-copilot-add-chat-message 'user "Other comment prompt" "ai-1" nil)
      (with-current-buffer (org-copilot-chat-input-buffer source)
	(insert "draft")
	(org-copilot-chat-recall-last-prompt)
	(should (equal (org-copilot-chat--prompt-text)
		       "Other comment prompt"))))))

(ert-deftest org-copilot-chat-recall-last-prompt-errors-without-history ()
  "Prompt recall signals clearly when the conversation has no history."
  (with-temp-buffer
    (org-mode)
    (let ((source (current-buffer)))
      (with-current-buffer (org-copilot-chat--buffer source)
	(should-error (org-copilot-chat-recall-last-prompt)
		      :type 'user-error)))))

(ert-deftest org-copilot-chat-send-scrolls-pending-message-to-window-top ()
  "Sending to an async adapter scrolls the pending response to the viewport top."
  (let ((source (generate-new-buffer " *org copilot chat scroll source*"))
	(chat-window nil))
    (unwind-protect
	(with-current-buffer source
	  (org-mode)
	  (let ((org-copilot-chat-function (lambda (_request) nil)))
	    (setq chat-window
		  (display-buffer (org-copilot-chat--buffer source)))
	    (select-window chat-window)
	    (org-copilot-chat-send "Explain this")
	    (with-current-buffer (window-buffer chat-window)
	      (should (eq (get-text-property (window-start chat-window)
					     'org-copilot-chat-message-role)
			  'pending)))))
      (when (window-live-p chat-window)
	(delete-window chat-window))
      (when (buffer-live-p source)
	(kill-buffer source)))))

(ert-deftest org-copilot-chat-scrolls-assistant-header-to-window-top ()
  "Assistant response scrolling keeps its timestamp and speaker visible."
  (let ((source (generate-new-buffer " *org copilot assistant scroll source*"))
	(chat-window nil))
    (unwind-protect
	(with-current-buffer source
	  (org-mode)
	  (org-copilot-add-chat-message 'user "Explain this")
	  (org-copilot-add-chat-message 'assistant "First answer line.\nSecond answer line.")
	  (setq chat-window
		(display-buffer (org-copilot-chat--buffer source)))
	  (org-copilot-chat-scroll-to-last-message source 'assistant)
	  (with-current-buffer (window-buffer chat-window)
	    (goto-char (window-start chat-window))
	    (should (eq (get-text-property (point)
					   'org-copilot-chat-message-role)
			'assistant))
	    (should (looking-at-p "\\[[0-9][0-9]:[0-9][0-9]\\] Copilot:"))))
      (when (window-live-p chat-window)
	(delete-window chat-window))
      (when (buffer-live-p source)
	(kill-buffer source)))))

(ert-deftest org-copilot-chat-send-adds-pending-message-for-async-adapter ()
  "Async chat adapters produce a pending Copilot message."
  (with-temp-buffer
    (org-mode)
    (let ((org-copilot-chat-function (lambda (_request) nil)))
      (org-copilot-chat-send "Explain this")
      (let ((messages (org-copilot-chat-messages)))
	(should (= (length messages) 2))
	(should (eq (plist-get (nth 1 messages) :role) 'pending))
	(should (equal (plist-get (nth 1 messages) :content) "…"))))))

(ert-deftest org-copilot-chat-send-appends-user-and-assistant-messages ()
  "Sending chat appends user text and optional adapter assistant text."
  (with-temp-buffer
    (org-mode)
    (let ((org-copilot-chat-function
	   (lambda (request)
	     (should-not (plist-get request :allow-comment-updates))
	     (should (equal (plist-get request :message) "Explain this"))
	     (list :message "Here is an explanation."
		   :comments (list (list :id "ignored" :body "Ignored."))))))
      (cl-letf (((symbol-function 'read-string)
		 (lambda (&rest _args) "Explain this")))
	(call-interactively #'org-copilot-chat-send))
      (let ((messages (org-copilot-chat-messages)))
	(should (= (length messages) 2))
	(should (eq (plist-get (nth 0 messages) :role) 'user))
	(should (equal (plist-get (nth 0 messages) :content) "Explain this"))
	(should (eq (plist-get (nth 1 messages) :role) 'assistant))
	(should (equal (plist-get (nth 1 messages) :content)
		       "Here is an explanation.")))
      (should-not (org-copilot-comments)))))

(ert-deftest org-copilot-chat-card-preview-keeps-full-rationale-and-source-action ()
  "A compact card retains the original body and opens its exact source comment."
  (let ((source (generate-new-buffer " *compact card source*")) opened)
    (unwind-protect
	(with-current-buffer source
	  (org-mode)
	  (org-copilot-mode 1)
	  (let* ((org-copilot-comment-preview-length 80)
		 (body (concat "Keep ownership distinct from compensation. " (make-string 300 ?x)))
		 (comment (list :id "durable-comment" :provider "org-copilot"
				:type 'comment :current t :body body)))
	    (with-temp-buffer
	      (setq-local org-comments-panel-source-buffer source)
	      (org-comments-panel-render-insert-comment-body comment)
	      (should (< (buffer-size) 140))
	      (should-not (string-match-p (regexp-quote body) (buffer-string)))
	      (goto-char (point-min))
	      (let ((button (next-button (point-min))))
		(should button)
		(should (equal (button-label button) "→"))
		(should (= (line-number-at-pos (button-start button)) 1))
		(should-not (string-match-p "…" (buffer-string)))
		(cl-letf (((symbol-function 'org-copilot-chat-show-explanation)
			   (lambda (actual-source id) (setq opened (list actual-source id)))))
		  (button-activate button))))
	    (should (equal opened (list source "durable-comment")))
	    (should (equal (plist-get comment :body) body))))
      (kill-buffer source))))

(ert-deftest org-copilot-chat-renders-org-and-opens-links-in-source ()
  "Org typography and link activation keep the source without running hooks."
  (let ((source (generate-new-buffer " *Org chat link source*"))
	(hooks 0) opened)
    (unwind-protect
	(with-temp-buffer
	  (org-copilot-chat-mode)
	  (setq-local org-copilot-chat-source-buffer source)
	  (let ((org-mode-hook (list (lambda () (cl-incf hooks)))))
	    (org-copilot-chat--insert-message
	     '(:role assistant :content "*Ownership*, not currency. [[https://example.invalid/evidence][Evidence]]")))
	  (should (= hooks 0))
	  (goto-char (point-min))
	  (search-forward "Ownership")
	  (should (get-text-property (1- (point)) 'face))
	  (let ((button (next-button (point-min))))
	    (should button)
	    (should (equal (button-label button) "Evidence"))
	    (cl-letf (((symbol-function 'org-link-open-from-string)
		       (lambda (link &rest _) (setq opened (list (current-buffer) link)))))
	      (button-activate button)))
	  (should (equal opened (list source "[[https://example.invalid/evidence][Evidence]]"))))
      (kill-buffer source))))

(ert-deftest org-copilot-chat-focus-does-not-repeat-card-body-in-conversation ()
  "Focus retains the conversation without automatically prepending a rationale."
  (let ((source (generate-new-buffer " *nonredundant chat source*")))
    (unwind-protect
	(with-temp-buffer
	  (org-copilot-chat-mode)
	  (cl-letf (((symbol-function 'org-copilot-chat--visible-messages)
		     (lambda (_) '((:role assistant :content "The conversation answer."))))
		    ((symbol-function 'org-copilot-find-visible-comment)
		     (lambda (_) '(:body "The full card rationale."))))
	    (org-copilot-chat-render source)
	    (should (string-match-p "The conversation answer" (buffer-string)))
	    (should-not (string-match-p "The full card rationale" (buffer-string))))
	  )
      (kill-buffer source))))

(ert-deftest org-copilot-chat-finding-is-already-visible-at-its-producing-response ()
  "The full finding is present before any arrow is clicked."
  (let ((source (generate-new-buffer " *finding source*")))
    (unwind-protect
	(with-temp-buffer
	  (org-copilot-chat-mode)
	  (cl-letf (((symbol-function 'org-copilot-chat--visible-messages)
		     (lambda (_) '((:role assistant :content "Review response." :finding-ids ("finding"))
				   (:role user :content "Later question."))))
		    ((symbol-function 'org-copilot-find-visible-comment)
		     (lambda (_) '(:id "finding" :body "Full finding rationale." :provider "org-copilot"))))
	    (org-copilot-chat-render source))
	  (goto-char (point-min))
	  (search-forward "Review response.")
	  (search-forward "Full finding rationale.")
	  (should (equal (get-text-property (1- (point)) 'org-copilot-chat-explanation) "finding"))
	  (search-forward "Later question.")
	  (should-not (string-match-p "Selected comment — explanation" (buffer-string))))
      (kill-buffer source))))

(ert-deftest org-copilot-chat-legacy-finding-uses-shared-preview-without-retargeting ()
  "Unlinked findings reuse the preview area without filtering or changing focus."
  (let ((org-copilot-diff-buffer-name " *legacy finding details*") previews)
    (unwind-protect
	(with-temp-buffer
	  (org-mode)
	  (setq org-copilot--chat-messages '((:role assistant :content "Document answer.")))
	  (org-copilot-chat--set-context (current-buffer) '(:type full-document))
	  (cl-letf (((symbol-function 'org-copilot-find-visible-comment)
		     (lambda (_) '(:id "old" :body "Original rationale.")))
		    ((symbol-function 'org-copilot--display-diff-buffer)
		     (lambda (_source buffer preserve) (should preserve) (push buffer previews))))
	    (org-copilot-chat-show-explanation (current-buffer) "old")
	    (org-copilot-chat-show-explanation (current-buffer) "old"))
	  (should (eq (car previews) (cadr previews)))
	  (should (equal org-copilot-chat-context '(:type full-document)))
	  (should (= 1 (length (org-copilot-chat-messages))))
	  (with-current-buffer (car previews)
	    (should (derived-mode-p 'org-copilot-details-mode))
	    (should buffer-read-only)
	    (should (string-match-p "Original rationale" (buffer-string)))))
      (when (get-buffer org-copilot-diff-buffer-name) (kill-buffer org-copilot-diff-buffer-name)))))

(ert-deftest org-copilot-chat-missing-explanation-never-falls-through ()
  "A stale explanation identity must not focus a neighbouring comment."
  (with-temp-buffer
    (cl-letf (((symbol-function 'org-copilot-find-visible-comment) (lambda (_) nil)))
      (should-error (org-copilot-chat-show-explanation (current-buffer) "missing")
		    :type 'user-error))))

(ert-deftest org-copilot-chat-explanation-reopens-from-durable-comment ()
  "A recreated source opens its exact persisted rationale without a new history."
  (let* ((directory (make-temp-file "copilot-explanation-reopen" t))
	 (file (expand-file-name "draft.org" directory))
	 (chat "*Reopened explanation test*") source)
    (unwind-protect
	(save-window-excursion
	  (with-temp-file file (insert "Original paragraph.\n"))
	  (setq source (find-file-noselect file))
	  (with-current-buffer source
	    (org-mode)
	    (let ((record (org-comments-create-record
			   file 1 9 "*Keep ownership distinct.* [[https://example.invalid/evidence][Evidence]]"
			   "persisted-reason" "Herbert" "now")))
	      (org-comments-append-to-sidecar (plist-put record :provider "org-copilot")))
	    (org-copilot-add-chat-message 'assistant "The retained conversation." nil nil nil '("persisted-reason"))
	    (org-copilot-add-chat-message 'user "A later turn."))
	  (kill-buffer source)
	  (setq source (find-file-noselect file))
	  (switch-to-buffer source)
	  (with-current-buffer source
	    (setq-local org-copilot-chat-buffer-name chat)
	    (org-copilot-mode 1)
	    (org-copilot-chat--buffer source)
	    (with-current-buffer chat (org-copilot-chat--set-prompt-text "My unsent reply."))
	    (let* ((comment (org-copilot-find-visible-comment "persisted-reason"))
		   (presentation (org-copilot-chat-comment-presentation source comment))
		   (before (with-current-buffer chat (buffer-string)))
		   (source-point (point)))
	      (should comment)
	      (with-current-buffer chat (should (string-match-p "Keep ownership distinct" before)))
	      (funcall (plist-get presentation :open))
	      (should (= source-point (point)))
	      (with-current-buffer chat
		(should (equal before (buffer-string)))
		(let ((window (get-buffer-window chat t)))
		  (should window)
		  (should (equal "persisted-reason"
				 (get-text-property (window-start window) 'org-copilot-chat-explanation)))))
	      (should-not org-copilot-chat-focus-comment-id)
	      (should (equal (buffer-string) "Original paragraph.\n"))
	      (should-not (buffer-modified-p))
	      (should (= 2 (length (org-copilot-chat-messages))))
	      (with-current-buffer chat
		(should (equal "My unsent reply." (org-copilot-chat--prompt-text)))
		(goto-char (point-min))
		(search-forward "The retained conversation.")
		(search-forward "Keep ownership distinct")
		(search-forward "A later turn.")
		(should (string-match-p "Keep ownership distinct" (buffer-string)))
		(should (string-match-p "The retained conversation" (buffer-string)))
		(should (next-button (point-min)))))))
      (when (buffer-live-p source) (kill-buffer source))
      (when (get-buffer chat) (kill-buffer chat))
      (dolist (buffer (buffer-list))
	(when (and (buffer-file-name buffer)
		   (string-prefix-p directory (buffer-file-name buffer)))
	  (kill-buffer buffer)))
      (delete-directory directory t))))

(ert-deftest org-copilot-chat-displays-recorded-times-with-finding-at-original-turn ()
  "A finding expands at its original response, before later conversation."
  (let* ((source (generate-new-buffer " *chronological chat source*"))
	 (messages (list
		    (list :role 'user :content "First question." :created-at
			  (format-time-string "%FT%T%z" (encode-time 0 11 9 20 11 2026)))
		    (list :role 'assistant :content "First reply." :finding-ids '("selected-note") :created-at
			  (format-time-string "%FT%T%z" (encode-time 0 12 9 20 11 2026)))
		    (list :role 'user :content "Latest question." :created-at
			  (format-time-string "%FT%T%z" (encode-time 0 13 9 20 11 2026))))))
    (unwind-protect
	(with-temp-buffer
	  (org-copilot-chat-mode)
	  (cl-letf (((symbol-function 'org-copilot-chat--visible-messages) (lambda (_) messages))
		    ((symbol-function 'org-copilot-find-visible-comment)
		     (lambda (_) '(:body "Full rationale." :id "selected-note"))))
	    (org-copilot-chat-render source))
	  (goto-char (point-min))
	  (search-forward "[09:11] You:")
	  (search-forward "First question.")
	  (search-forward "[09:12] Copilot:")
	  (search-forward "First reply.")
	  (search-forward "Full rationale.")
	  (search-forward "[09:13] You:")
	  (search-forward "Latest question.")
	  (should-not (string-match-p "Selected comment — explanation" (buffer-string))))
      (kill-buffer source))))

(ert-deftest org-copilot-chat-never-invents-a-legacy-message-time ()
  "Legacy and invalid timestamps are visibly unknown, not the current clock."
  (with-temp-buffer
    (org-copilot-chat-mode)
    (org-copilot-chat--insert-message '(:role user :content "Undated."))
    (org-copilot-chat--insert-message '(:role assistant :content "Malformed." :created-at "not-a-time"))
    (should (string-match-p (regexp-quote "[time unknown] You:") (buffer-string)))
    (should (string-match-p (regexp-quote "[time unknown] Copilot:") (buffer-string)))))

(provide 'org-copilot-chat-test)
;;; org-copilot-chat-test.el ends here

(ert-deftest org-copilot-chat-slash-commands-include-session-management ()
  "Slash completion includes session management commands."
  (with-temp-buffer
    (org-mode)
    (let ((commands (mapcar #'car
			    (org-copilot-chat--available-slash-commands
			     (current-buffer)))))
      (should (member "/clear" commands))
      (should (member "/clear-ui" commands))
      (should (member "/erase" commands)))))

(ert-deftest org-copilot-chat-clear-ui-command-preserves-sidecar ()
  "The /clear-ui command clears ephemeral chat while preserving transcript."
  (let* ((directory (make-temp-file "org-copilot-chat-clear-ui" t))
	 (source-file (expand-file-name "draft.org" directory)))
    (unwind-protect
	(with-current-buffer (find-file-noselect source-file)
	  (erase-buffer)
	  (insert "* Intro\nBody.\n")
	  (save-buffer)
	  (org-mode)
	  (org-copilot-add-chat-message 'user "Hello")
	  (org-copilot-chat-send "/clear-ui")
	  (should-not (org-copilot-chat-messages))
	  (should (org-copilot-sidecar-load-messages source-file)))
      (when-let* ((buffer (find-buffer-visiting source-file)))
	(kill-buffer buffer))
      (delete-directory directory t))))
