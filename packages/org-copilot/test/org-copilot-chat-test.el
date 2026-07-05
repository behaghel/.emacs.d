;;; org-copilot-chat-test.el --- Chat tests for org-copilot -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for Org Copilot bottom chat skeleton and session transcript state.

;;; Code:

(require 'ert)
(require 'org)
(require 'org-copilot)
(require 'org-copilot-chat)

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
	      (should (eq org-context-panel-source-buffer source))
	      (should (eq org-context-panel-view-id 'copilot-chat))
	      (should (string-match-p "Org Copilot Chat" (buffer-string)))
	      (should (string-match-p "Ask Copilot about the full document"
				      (buffer-string))))))
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
	    (org-copilot-panel-mode)
	    (setq org-context-panel-source-buffer source)
	    (let ((inhibit-read-only t))
	      (insert "AI [active] Clarify this sentence.\n"))
	    (let ((inhibit-read-only t))
	      (add-text-properties (point-min) (point-max)
				   `(org-context-panel-item ,comment)))
	    (goto-char (point-min))
	    (let ((buffer (org-copilot-chat)))
	      (with-current-buffer source
		(should (equal org-copilot-chat-focus-comment-id "ai-1")))
	      (with-current-buffer buffer
		(should (string-match-p "Org Copilot Chat — .* · ✏️ Comment ai-1 · active"
					(buffer-string)))
		(should-not (string-match-p "^Context" (buffer-string)))
		(should (string-match-p "Copilot\n  Clarify this sentence" (buffer-string)))
		(should-not (string-match-p "Comment: Clarify" (buffer-string)))
		(should-not (string-match-p "Suggestion:" (buffer-string)))
		(should-not (string-match-p "Earlier general chat" (buffer-string)))
		(should-not (string-match-p "Ask Copilot to revise"
					    (buffer-string)))))))
      (when (buffer-live-p source)
	(kill-buffer source)))))

(ert-deftest org-copilot-chat-opens-diff-for-focused-suggestion ()
  "Focused chat opens a diff buffer for the focused comment suggestion."
  (let ((source (generate-new-buffer " *org copilot chat source*")))
    (unwind-protect
	(let ((comment nil))
	  (with-current-buffer source
	    (org-mode)
	    (insert "Alpha sentence.\n")
	    (setq comment
		  (org-copilot-add-comment
		   (list :id "ai-1"
			 :source-start (point-min)
			 :source-end (+ (point-min) (length "Alpha sentence."))
			 :target-text "Alpha sentence."
			 :body "Tighten this."
			 :suggestion "Alpha."
			 :status 'active))))
	  (with-temp-buffer
	    (org-copilot-panel-mode)
	    (setq org-context-panel-source-buffer source)
	    (let ((inhibit-read-only t))
	      (insert "💬 Tighten this.\n")
	      (add-text-properties (point-min) (point-max)
				   `(org-context-panel-item ,comment)))
	    (goto-char (point-min))
	    (org-copilot-chat)
	    (with-current-buffer (get-buffer org-copilot-diff-buffer-name)
	      (should (string-match-p "^-Alpha sentence\\." (buffer-string)))
	      (should (string-match-p "^+Alpha\\." (buffer-string))))))
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

(ert-deftest org-copilot-chat-accept-command-accepts-focused-suggestion ()
  "The /accept chat command accepts the focused comment suggestion."
  (with-temp-buffer
    (org-mode)
    (insert "Alpha sentence.\n")
    (org-copilot-add-comment
     (list :id "ai-1"
	   :source-start (point-min)
	   :source-end (+ (point-min) (length "Alpha sentence."))
	   :target-text "Alpha sentence."
	   :suggestion "Alpha."
	   :status 'active))
    (setq org-copilot-chat-focus-comment-id "ai-1")
    (org-copilot-chat-send "/accept")
    (should (equal (buffer-string) "Alpha.\n"))
    (should (eq (plist-get (org-copilot-find-comment "ai-1") :status)
		'accepted))))

(ert-deftest org-copilot-chat-undo-command-restores-accepted-suggestion ()
  "The /undo chat command restores an accepted focused suggestion."
  (with-temp-buffer
    (org-mode)
    (insert "Alpha sentence.\n")
    (let ((comment (org-copilot-add-comment
		    (list :id "ai-1"
			  :source-start (point-min)
			  :source-end (+ (point-min) (length "Alpha sentence."))
			  :target-text "Alpha sentence."
			  :suggestion "Alpha."
			  :status 'active))))
      (org-copilot-accept-comment comment (current-buffer))
      (setq org-copilot-chat-focus-comment-id "ai-1")
      (org-copilot-chat-send "/undo")
      (should (equal (buffer-string) "Alpha sentence.\n"))
      (should (eq (plist-get (org-copilot-find-comment "ai-1") :status)
		  'active)))))

(ert-deftest org-copilot-chat-accept-key-accepts-focused-suggestion ()
  "Direct chat accept command accepts the focused comment suggestion."
  (with-temp-buffer
    (org-mode)
    (insert "Alpha sentence.\n")
    (org-copilot-add-comment
     (list :id "ai-1"
	   :source-start (point-min)
	   :source-end (+ (point-min) (length "Alpha sentence."))
	   :target-text "Alpha sentence."
	   :suggestion "Alpha."
	   :status 'active))
    (setq org-copilot-chat-focus-comment-id "ai-1")
    (with-current-buffer (org-copilot-chat--buffer (current-buffer))
      (org-copilot-chat-accept-focused-suggestion-at-point))
    (should (equal (buffer-string) "Alpha.\n"))
    (should (eq (plist-get (org-copilot-find-comment "ai-1") :status)
		'accepted))))

(ert-deftest org-copilot-chat-opens-at-editable-prompt ()
  "Opening chat selects the chat buffer and places point at an editable prompt."
  (let ((source (generate-new-buffer " *org copilot chat source*")))
    (unwind-protect
	(progn
	  (let ((buffer nil))
	    (with-current-buffer source
	      (org-mode)
	      (insert "* Heading\nBody.\n")
	      (setq buffer (org-copilot-chat)))
	    (with-current-buffer buffer
	      (should (derived-mode-p 'org-copilot-chat-mode))
	      (should-not buffer-read-only)
	      (should (= (point) org-copilot-chat--prompt-start))
	      (should-not (get-text-property (point) 'read-only))
	      (should (get-text-property (1- (point)) 'read-only))
	      (insert "Can I type here?")
	      (should (string-suffix-p "🌐 You: Can I type here?"
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
	(search-forward "🌐 You: ")
	(should (eq (get-text-property (match-beginning 0) 'face)
		    'org-copilot-chat-prompt-face))))))

(ert-deftest org-copilot-chat-prompt-field-sends-and-clears ()
  "RET-style prompt sending records input and clears the prompt field."
  (with-temp-buffer
    (org-mode)
    (let ((source (current-buffer)))
      (with-current-buffer (org-copilot-chat--buffer source)
	(insert "Explain this")
	(org-copilot-chat-send-current-input)
	(should (string-match-p "You\n  Explain this" (buffer-string)))
	(should (string-suffix-p "🌐 You: " (buffer-string))))
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
      (should (string-suffix-p "🌐 You: " (buffer-string))))))

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
      (should (string-suffix-p "§ You: " (buffer-string))))))

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
    (let ((request (org-copilot-chat--request (current-buffer) "Explain")))
      (should (eq (plist-get (plist-get request :chat-context) :type)
		  'section))
      (should (string-match-p "Intro body" (plist-get request :source-content)))
      (should (string-match-p "\\* Methods" (plist-get request :section-content)))
      (should-not (string-match-p "Intro body"
				  (plist-get request :section-content)))
      (should (= (length (plist-get request :messages)) 1))
      (should (equal (plist-get (car (plist-get request :messages)) :content)
		     "Earlier section")))))

(ert-deftest org-copilot-chat-request-full-document-uses-source-content-only ()
  "Full-document chat request includes source content without comment focus."
  (with-temp-buffer
    (org-mode)
    (insert "Alpha sentence.\n")
    (org-copilot-add-comment (list :id "ai-1" :status 'active))
    (org-copilot-add-chat-message 'user "General" nil)
    (org-copilot-add-chat-message 'user "Focused" "ai-1")
    (let ((request (org-copilot-chat--request (current-buffer) "Explain")))
      (should-not (plist-get request :focus-comment-id))
      (should (string-match-p "Alpha sentence" (plist-get request :source-content)))
      (should (= (length (plist-get request :messages)) 1))
      (should (equal (plist-get (car (plist-get request :messages)) :content)
		     "General")))))

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

(ert-deftest org-copilot-chat-navigation-syncs-diff ()
  "Chat navigation opens suggestion diffs for newly focused comments."
  (with-temp-buffer
    (org-mode)
    (insert "Alpha sentence.\nBeta sentence.\n")
    (org-copilot-add-comment
     (list :id "ai-1"
	   :status 'active
	   :source-start (point-min)
	   :source-end (+ (point-min) (length "Alpha sentence."))
	   :target-text "Alpha sentence."
	   :suggestion "Alpha."))
    (org-copilot-chat-focus-next-comment)
    (should (get-buffer org-copilot-diff-buffer-name))))

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

(ert-deftest org-copilot-chat-dismiss-command-dismisses-focused-comment ()
  "The /dismiss chat command dismisses the focused comment."
  (with-temp-buffer
    (org-mode)
    (org-copilot-add-comment (list :id "ai-1" :status 'active))
    (setq org-copilot-chat-focus-comment-id "ai-1")
    (org-copilot-chat-send "/dismiss")
    (should-not (org-copilot-find-comment "ai-1"))
    (should-not org-copilot-chat-focus-comment-id)))

(ert-deftest org-copilot-chat-dismiss-refreshes-side-panel ()
  "Dismissing from chat refreshes the visible side panel."
  (with-temp-buffer
    (org-mode)
    (org-copilot-mode 1)
    (org-copilot-add-comment
     (list :id "ai-1" :status 'active :summary "Remove me"))
    (setq org-copilot-chat-focus-comment-id "ai-1")
    (let ((source (current-buffer))
	  (panel-buffer (get-buffer-create org-copilot-panel-buffer-name)))
      (unwind-protect
	  (progn
	    (with-current-buffer panel-buffer
	      (org-copilot-panel-mode)
	      (setq org-context-panel-source-buffer source))
	    (org-context-panel-refresh)
	    (with-current-buffer panel-buffer
	      (should (string-match-p "Remove me" (buffer-string))))
	    (org-copilot-chat-send "/dismiss")
	    (with-current-buffer panel-buffer
	      (should-not (string-match-p "Remove me" (buffer-string)))))
	(when (buffer-live-p panel-buffer)
	  (kill-buffer panel-buffer))))))

(ert-deftest org-copilot-chat-advance-after-dismiss-focuses-next-comment ()
  "Dismiss can automatically advance chat focus to the next comment."
  (with-temp-buffer
    (org-mode)
    (let ((org-copilot-chat-advance-after-action t))
      (org-copilot-add-comment (list :id "ai-1" :status 'active))
      (org-copilot-add-comment (list :id "ai-2" :status 'active))
      (setq org-copilot-chat-focus-comment-id "ai-1")
      (org-copilot-chat-dismiss-focused-comment (current-buffer))
      (should (equal org-copilot-chat-focus-comment-id "ai-2")))))

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
    (should-not (assoc "/accept" (org-copilot-chat--available-slash-commands
				  (current-buffer))))
    (org-copilot-update-comment
     (plist-put (org-copilot-find-comment "ai-1") :suggestion "Alpha."))
    (should (assoc "/accept" (org-copilot-chat--available-slash-commands
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
      (with-current-buffer (org-copilot-chat--buffer source)
	(insert "look ")
	(org-copilot-chat-slash-or-complete)
	(should (string-suffix-p "🌐 You: look /" (buffer-string)))))))

(ert-deftest org-copilot-chat-slash-commands-have-annotations ()
  "Implemented slash commands expose completion annotations."
  (should (equal (org-copilot-chat--slash-command-annotation "/accept")
		 "  Accept focused suggestion"))
  (should (assoc "/dismiss" org-copilot-chat-slash-commands))
  (should (equal (org-copilot-chat--slash-command-annotation "/doctor")
		 "  Run Org Copilot health check"))
  (should-not (assoc "/help" org-copilot-chat-slash-commands)))

(ert-deftest org-copilot-chat-mode-defines-action-keys ()
  "Org Copilot chat mode defines send, focus, and action keys."
  (should (eq (lookup-key org-copilot-chat-mode-map (kbd "RET"))
	      #'org-copilot-chat-send-current-input))
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
  (should (eq (lookup-key org-copilot-chat-mode-map (kbd "M-a"))
	      #'org-copilot-chat-accept-focused-suggestion-at-point))
  (should (eq (lookup-key org-copilot-chat-mode-map (kbd "M-d"))
	      #'org-copilot-chat-dismiss-focused-comment-at-point))
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

(ert-deftest org-copilot-chat-recall-last-prompt-uses-current-context ()
  "M-up recalls the last submitted prompt in the current chat context."
  (with-temp-buffer
    (org-mode)
    (let ((source (current-buffer)))
      (org-copilot-add-chat-message 'user "Full document prompt" nil nil)
      (org-copilot-add-chat-message 'user "Other comment prompt" "ai-1" nil)
      (with-current-buffer (org-copilot-chat--buffer source)
	(insert "draft")
	(org-copilot-chat-recall-last-prompt)
	(should (equal (org-copilot-chat--prompt-text)
		       "Full document prompt"))))))

(ert-deftest org-copilot-chat-recall-last-prompt-errors-without-history ()
  "Prompt recall signals clearly when there is no current-context history."
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

(provide 'org-copilot-chat-test)
;;; org-copilot-chat-test.el ends here
