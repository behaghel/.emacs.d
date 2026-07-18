;;; org-copilot-llm-test.el --- LLM protocol tests for org-copilot -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for Org Copilot review request construction and fake adapter flow.

;;; Code:

(require 'ert)
(require 'org)
(require 'org-copilot)
(require 'org-copilot-llm)

(ert-deftest org-copilot-review-dwim-prefers-region ()
  "Review DWIM sends the active region when a region is active."
  (with-temp-buffer
    (org-mode)
    (insert "Alpha sentence.\nBeta sentence.\n")
    (let ((org-copilot-review-function
	   (lambda (request)
	     (should (eq (plist-get request :scope) 'region))
	     (should (equal (plist-get request :text) "Alpha sentence."))
	     nil)))
      (goto-char (point-min))
      (push-mark (+ (point-min) (length "Alpha sentence.")) t t)
      (setq mark-active t)
      (org-copilot-review-dwim))))

(ert-deftest org-copilot-review-dwim-falls-back-to-subtree ()
  "Review DWIM sends the current subtree when no region is active."
  (with-temp-buffer
    (org-mode)
    (insert "* Heading\nBody text.\n* Next\nOther text.\n")
    (goto-char (point-min))
    (let ((org-copilot-review-function
	   (lambda (request)
	     (should (eq (plist-get request :scope) 'subtree))
	     (should (string-match-p "Heading" (plist-get request :text)))
	     (should (string-match-p "Body text" (plist-get request :text)))
	     (should-not (string-match-p "Next" (plist-get request :text)))
	     nil)))
      (org-copilot-review-dwim))))

(ert-deftest org-copilot-review-dwim-errors-without-adapter ()
  "Review DWIM fails clearly when no LLM adapter is configured."
  (with-temp-buffer
    (org-mode)
    (insert "* Heading\nBody text.\n")
    (let ((org-copilot-review-function nil))
      (should-error (org-copilot-review-dwim) :type 'user-error))))

(ert-deftest org-copilot-review-dwim-installs-normalized-comments ()
  "Review DWIM installs normalized AI comments returned by the adapter."
  (with-temp-buffer
    (org-mode)
    (insert "* Heading\nAlpha sentence.\n")
    (let ((org-copilot-review-function
	   (lambda (_request)
	     (list (list :id "ai-1"
			 :type 'inline
			 :source-start 11
			 :source-end 26
			 :target-text "Alpha sentence."
			 :body "Tighten this."
			 :suggestion "Alpha.")))))
      (org-copilot-review-dwim)
      (let ((comments (org-copilot-comments)))
	(should (= (length comments) 1))
	(should (equal (plist-get (car comments) :id) "ai-1"))
	(should (eq (plist-get (car comments) :status) 'active))))))

(ert-deftest org-copilot-llm-parses-review-summary ()
  "Strict JSON review responses may include a general summary."
  (let* ((result (org-copilot-llm-parse-review-result
		  (concat "{\"summary\":{\"message\":\"Overall: promising.\","
			  "\"strengths\":[\"Clear goal\"],"
			  "\"risks\":[\"Weak evidence\"],"
			  "\"next_steps\":[\"Add examples\"]},"
			  "\"comments\":[{\"id\":\"ai-1\",\"body\":\"Tighten.\"}]}")))
	 (summary (plist-get result :summary)))
    (should (string-match-p "Overall: promising" summary))
    (should (string-match-p "Strengths" summary))
    (should (string-match-p "Clear goal" summary))
    (should (= (length (plist-get result :comments)) 1))))

(ert-deftest org-copilot-llm-empty-suggestion-normalizes-to-nil ()
  "Empty suggestion strings are treated as absent suggestions."
  (let ((comments (org-copilot-llm-parse-review-response
		   "{\"comments\":[{\"id\":\"ai-1\",\"body\":\"Tighten.\",\"suggestion\":\"   \"}]}")))
    (should-not (plist-get (car comments) :suggestion))))

(ert-deftest org-copilot-llm-parses-json-comments ()
  "Strict JSON responses parse to normalized AI comments."
  (let ((comments (org-copilot-llm-parse-review-response
		   "{\"comments\":[{\"id\":\"ai-1\",\"type\":\"inline\",\"summary\":\"Too soft?\",\"body\":\"Tighten.\",\"target_text\":\"Alpha sentence.\",\"suggestion\":\"Alpha.\",\"line_start\":1,\"line_end\":1}]}")))
    (should (= (length comments) 1))
    (should (equal (plist-get (car comments) :id) "ai-1"))
    (should (eq (plist-get (car comments) :type) 'inline))
    (should (eq (plist-get (car comments) :status) 'active))
    (should (equal (plist-get (car comments) :summary) "Too soft?"))
    (should (equal (plist-get (car comments) :target-text) "Alpha sentence."))
    (should (equal (plist-get (car comments) :line-start) 1))))

(ert-deftest org-copilot-llm-anchors-comments-by-target-text ()
  "Parsed comments are anchored by exact target text in the source buffer."
  (with-temp-buffer
    (org-mode)
    (insert "* Draft\n\nAlpha sentence.\n")
    (let* ((comments (org-copilot-llm-parse-review-response
		      "{\"comments\":[{\"id\":\"ai-1\",\"body\":\"Tighten.\",\"target_text\":\"Alpha sentence.\"}]}"))
	   (anchored (org-copilot-llm-anchor-comments (current-buffer) comments))
	   (comment (car anchored)))
      (should (equal (plist-get comment :source-start) 10))
      (should (equal (plist-get comment :source-end) 25)))))

(ert-deftest org-copilot-llm-anchors-comments-by-line-range-fallback ()
  "Comments fall back to request-relative line ranges when target text fails."
  (with-temp-buffer
    (org-mode)
    (insert "* Draft\n\nAlpha sentence.\nBeta sentence.\n")
    (let* ((request (list :start (point-min)
			  :end (point-max)
			  :text (buffer-string)))
	   (comments (org-copilot-llm-parse-review-response
		      "{\"comments\":[{\"id\":\"ai-1\",\"body\":\"Tighten.\",\"target_text\":\"Missing text\",\"line_start\":3,\"line_end\":3}]}"))
	   (anchored (org-copilot-llm-anchor-comments
		      (current-buffer) comments request))
	   (comment (car anchored)))
      (should (equal (buffer-substring-no-properties
		      (plist-get comment :source-start)
		      (plist-get comment :source-end))
		     "Alpha sentence.")))))

(ert-deftest org-copilot-llm-parses-chat-comments-with-fallback-message ()
  "Structured chat responses parse comments while plain text remains usable."
  (let ((parsed (org-copilot-llm-parse-chat-response
		 "{\"message\":\"I found one issue.\",\"intent\":\"review\",\"comments\":[{\"body\":\"Tighten.\",\"target_text\":\"Alpha sentence.\",\"suggestion\":\"Alpha.\"}]}")))
    (should (equal (plist-get parsed :message) "I found one issue."))
    (should (eq (plist-get parsed :intent) 'review))
    (should (= (length (plist-get parsed :comments)) 1))
    (should (equal (plist-get (car (plist-get parsed :comments)) :target-text)
		   "Alpha sentence.")))
  (should (equal (plist-get (org-copilot-llm-parse-chat-response "Plain answer.")
			    :message)
		 "Plain answer.")))

(ert-deftest org-copilot-llm-installs-insertion-chat-comments ()
  "Insertion comments anchor by anchor text and install as review artifacts."
  (with-temp-buffer
    (org-mode)
    (insert "Intro.\nConclusion.\n")
    (let* ((org-copilot-chat-open-panel-on-comments nil)
	   (parsed (org-copilot-llm-parse-chat-response
		    "{\"comments\":[{\"type\":\"insertion\",\"body\":\"Add bridge.\",\"anchor_text\":\"Intro.\",\"placement\":\"after\",\"suggestion\":\"Bridge.\"}]}"))
	   (result (org-copilot-install-chat-comments
		    (current-buffer)
		    (plist-get parsed :comments)
		    (list :chat-context '(:type full-document))
		    "Add missing bridge"))
	   (comment (org-copilot-find-comment "ai-1")))
      (should (= (plist-get result :installed) 1))
      (should comment)
      (should (eq (plist-get comment :type) 'insertion))
      (should (equal (plist-get comment :anchor-text) "Intro."))
      (should (eq (plist-get comment :placement) 'after))
      (should (= (plist-get comment :source-start) 7)))))

(ert-deftest org-copilot-llm-installs-chat-comments-with-local-ids ()
  "Chat comments are anchored, installed, and assigned local ids."
  (with-temp-buffer
    (org-mode)
    (insert "* Draft\nAlpha sentence.\n")
    (org-copilot-add-comment (list :id "ai-3" :type 'scope :body "Existing."))
    (let* ((org-copilot-chat-open-panel-on-comments nil)
	   (parsed (org-copilot-llm-parse-chat-response
		    "{\"message\":\"Done.\",\"comments\":[{\"id\":\"model-1\",\"body\":\"Tighten.\",\"target_text\":\"Alpha sentence.\",\"suggestion\":\"Alpha.\"}]}"))
	   (result (org-copilot-install-chat-comments
		    (current-buffer)
		    (plist-get parsed :comments)
		    (list :chat-context '(:type full-document))
		    "Please improve weak lines"))
	   (comment (org-copilot-find-comment "ai-4")))
      (should (= (plist-get result :installed) 1))
      (should comment)
      (should (equal (plist-get comment :target-text) "Alpha sentence."))
      (should (equal (plist-get (plist-get comment :metadata) :model-id)
		     "model-1"))
      (should (eq (plist-get (plist-get comment :metadata) :source) 'chat)))))

(ert-deftest org-copilot-llm-skips-invalid-inline-review-comments ()
  "Review install rejects inline comments without resolved target anchors."
  (with-temp-buffer
    (org-mode)
    (insert "* Draft\nAlpha sentence.\n")
    (org-copilot-install-review-comments
     (list (list :id "ai-1"
		 :type 'inline
		 :status 'active
		 :body "Missing target.")
	   (list :id "ai-2"
		 :type 'scope
		 :status 'active
		 :body "Scope note.")))
    (let ((comments (org-copilot-comments)))
      (should (= (length comments) 1))
      (should (equal (plist-get (car comments) :id) "ai-2")))))

(ert-deftest org-copilot-llm-skips-unanchored-chat-comments ()
  "Chat comments without reliable inline anchors are skipped."
  (with-temp-buffer
    (org-mode)
    (insert "* Draft\nAlpha sentence.\n")
    (let* ((org-copilot-chat-open-panel-on-comments nil)
	   (parsed (org-copilot-llm-parse-chat-response
		    "{\"comments\":[{\"body\":\"Missing target.\"},{\"body\":\"Wrong target.\",\"target_text\":\"Missing text\"}]}"))
	   (result (org-copilot-install-chat-comments
		    (current-buffer)
		    (plist-get parsed :comments)
		    (list :chat-context '(:type full-document))
		    "Review this")))
      (should (= (plist-get result :installed) 0))
      (should (= (plist-get result :skipped-unanchored) 2))
      (should-not (org-copilot-comments)))))

(ert-deftest org-copilot-llm-chat-install-result-reports-reachability ()
  "Chat install results include a receipt for session-reachable comments."
  (with-temp-buffer
    (org-mode)
    (insert "Alpha sentence.\n")
    (let* ((org-copilot-chat-open-panel-on-comments nil)
	   (parsed (org-copilot-llm-parse-chat-response
		    "{\"comments\":[{\"body\":\"Tighten.\",\"target_text\":\"Alpha sentence.\",\"suggestion\":\"Alpha.\"}]}"))
	   (result (org-copilot-install-chat-comments
		    (current-buffer)
		    (plist-get parsed :comments)
		    (list :chat-context '(:type full-document))
		    "Review this")))
      (should (= (plist-get result :installed) 1))
      (should (= (plist-get result :reachable) 1))
      (should (= (plist-get result :hidden) 0)))))

(ert-deftest org-copilot-llm-chat-install-summary-reports-hidden-comments ()
  "Chat install summaries do not present hidden comments as plain success."
  (let ((summary (org-copilot-llm-append-chat-install-summary
		  "Done."
		  '(:installed 2 :reachable 1 :hidden 1
			       :skipped-unanchored 0 :skipped-limit 0))))
    (should (string-match-p "Installed 1 reachable comment" summary))
    (should (string-match-p "1 hidden" summary))))

(ert-deftest org-copilot-llm-limits-chat-comments ()
  "Chat comment installation enforces the configured per-response limit."
  (with-temp-buffer
    (org-mode)
    (insert "Alpha. Beta. Gamma.\n")
    (let* ((org-copilot-chat-open-panel-on-comments nil)
	   (org-copilot-chat-max-comments 1)
	   (parsed (org-copilot-llm-parse-chat-response
		    "{\"comments\":[{\"body\":\"A\",\"target_text\":\"Alpha.\"},{\"body\":\"B\",\"target_text\":\"Beta.\"}]}"))
	   (result (org-copilot-install-chat-comments
		    (current-buffer)
		    (plist-get parsed :comments)
		    (list :chat-context '(:type full-document))
		    "Review this")))
      (should (= (plist-get result :installed) 1))
      (should (= (plist-get result :skipped-limit) 1))
      (should (= (length (org-copilot-comments)) 1)))))

(ert-deftest org-copilot-llm-section-chat-comments-stay-in-section ()
  "Section chat installs only comments anchored inside the active section."
  (with-temp-buffer
    (org-mode)
    (insert "* First\nAlpha sentence.\n* Second\nBeta sentence.\n")
    (goto-char (point-min))
    (let* ((org-copilot-chat-open-panel-on-comments nil)
	   (context (org-copilot-chat--section-context-at-point))
	   (parsed (org-copilot-llm-parse-chat-response
		    "{\"comments\":[{\"body\":\"Keep.\",\"target_text\":\"Alpha sentence.\"},{\"body\":\"Skip.\",\"target_text\":\"Beta sentence.\"}]}"))
	   (result (org-copilot-install-chat-comments
		    (current-buffer)
		    (plist-get parsed :comments)
		    (list :chat-context context)
		    "Review this section")))
      (should (= (plist-get result :installed) 1))
      (should (= (plist-get result :skipped-unanchored) 1))
      (should (= (length (org-copilot-comments)) 1))
      (should (equal (plist-get (car (org-copilot-comments)) :target-text)
		     "Alpha sentence.")))))

(provide 'org-copilot-llm-test)
;;; org-copilot-llm-test.el ends here
