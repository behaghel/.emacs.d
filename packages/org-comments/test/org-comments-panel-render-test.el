;;; org-comments-panel-render-test.el --- Panel render tests for org-comments -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for standalone Org comments panel rendering helpers.

;;; Code:

(require 'ert)
(require 'org-comments-panel-render)

(defun org-comments-panel-render-test--with-backend (record backend)
  "Return RECORD fixture annotated recursively with BACKEND."
  (let ((copy (copy-sequence record)))
    (plist-put copy :backend backend)
    (when-let* ((replies (plist-get copy :replies)))
      (plist-put copy :replies
		 (mapcar (lambda (reply)
			   (org-comments-panel-render-test--with-backend reply backend))
			 replies)))
    copy))

(defun org-comments-panel-render-test--visual-fixture (backend)
  "Return provider-neutral visual panel fixture for BACKEND."
  (list
   :inline
   (mapcar
    (lambda (record)
      (org-comments-panel-render-test--with-backend record backend))
    '((:type comment :status "OPEN" :target-text "selected paragraph"
	     :remote-id "root-open" :remote-author-display-name "Alice"
	     :created-at "2026-07-02T10:00:00Z" :body "Remote linked root."
	     :current t
	     :replies ((:type comment :status "OPEN" :remote-id "reply-remote"
			      :remote-author-display-name "Bob"
			      :created-at "2026-07-02T10:05:00Z"
			      :body "Remote reply.")
		       (:type comment :status "OPEN" :author "Carol"
			      :local-updated-at "2026-07-02T10:10:00Z"
			      :body "Pending local reply.")))
      (:type comment :status "RESOLVED" :target-text "done paragraph"
	     :remote-id "root-resolved" :body "Resolved remote-linked root.")
      (:type comment :status "TODO" :target-text "stale paragraph"
	     :remote-id "root-missing" :remote-state missing
	     :remote-missing-at "2026-07-02T12:00:00Z"
	     :body "Missing remote root.")
      (:type comment :status "OPEN" :target-text "unconfirmed paragraph"
	     :remote-anchor-state "unconfirmed" :body "Unconfirmed anchor.")))
   :page
   (mapcar
    (lambda (record)
      (org-comments-panel-render-test--with-backend record backend))
    '((:type comment :status "OPEN" :page-comment t :remote-id "page-1"
	     :body "Page/footer provider note.")))))

(defun org-comments-panel-render-test--fixture-output (backend)
  "Return rendered visual fixture output for BACKEND without text properties."
  (let* ((fixture (org-comments-panel-render-test--visual-fixture backend))
	 (source-buffer (current-buffer)))
    (with-temp-buffer
      (org-comments-panel-render-buffer
       source-buffer (plist-get fixture :inline) (plist-get fixture :page))
      (buffer-substring-no-properties (point-min) (point-max)))))

(ert-deftest org-comments-panel-render-summary-includes-status-target-and-body ()
  "Comment summaries include the useful standalone panel fields."
  (should (equal (org-comments-panel-render-comment-summary
		  '(:status "OPEN" :target-text " selected text " :body " Review this. "))
		 "[OPEN] “selected text” — Review this.")))

(ert-deftest org-comments-panel-render-buffer-includes-count-and-sections ()
  "Rendering inserts the count and comment sections."
  (with-temp-buffer
    (org-comments-panel-render-buffer
     (current-buffer)
     '((:type comment :status "OPEN" :target-text "Alpha" :body "Inline note"))
     '((:type comment :status "TODO" :body "Page note" :page-comment t)))
    (should (string-match-p "2 comments" (buffer-string)))
    (should (string-match-p "Inline" (buffer-string)))
    (should (string-match-p "Page" (buffer-string)))
    (should (string-match-p "Inline note" (buffer-string)))
    (should (string-match-p "Page note" (buffer-string)))))

(ert-deftest org-comments-panel-render-decodes-html-body ()
  "Rendering shows readable text for HTML-ish comment bodies."
  (with-temp-buffer
    (org-comments-panel-render-buffer
     (current-buffer)
     '((:type comment :status "OPEN" :body "<p>It&rsquo;s useful &amp; readable.</p>"))
     nil)
    (should (search-forward "It’s useful & readable." nil t))))

(ert-deftest org-comments-panel-render-shows-stale-warning ()
  "Rendering marks stale overview comments with a compact warning badge."
  (with-temp-buffer
    (org-comments-panel-render-buffer
     (current-buffer)
     '((:type comment :status "OPEN" :body "Please revisit." :anchor-state stale))
     nil)
    (let ((output (buffer-substring-no-properties (point-min) (point-max))))
      (should (string-match-p "⚠️" output))
      (should-not (string-match-p "Anchor no longer matches source text." output)))))

(ert-deftest org-comments-panel-render-shows-page-badge ()
  "Rendering marks page comments in their row header."
  (with-temp-buffer
    (org-comments-panel-render-buffer
     (current-buffer)
     nil
     '((:type comment :status "OPEN" :body "Page note" :page-comment t)))
    (should (search-forward "PAGE" nil t))))


(ert-deftest org-comments-panel-render-uses-provider-icon-on-first-line ()
  "Rendering identifies the comment provider by first-line icon."
  (with-temp-buffer
    (org-comments-panel-render-buffer
     (current-buffer)
     '((:type comment :status "OPEN" :target-text "Intro"
	      :provider "org-copilot" :body "Rewrite intro.")
       (:type comment :status "OPEN" :target-text "Cloud"
	      :remote-id "remote-1" :body "Remote note.")
       (:type comment :status "OPEN" :target-text "Local"
	      :body "Local note."))
     nil)
    (let ((output (buffer-substring-no-properties (point-min) (point-max))))
      (should (string-match-p "🤖 \\[OPEN\\] “Intro”" output))
      (should (string-match-p "☁️ \\[OPEN\\] “Cloud”" output))
      (should (string-match-p "✍️ \\[OPEN\\] “Local”" output))
      (should-not (string-match-p "💬 \\[OPEN\\]" output)))))

(ert-deftest org-comments-panel-render-shows-suggestion-badge-on-metadata-line ()
  "Rendering shows compact linked suggestion state on the metadata line."
  (with-temp-buffer
    (org-comments-panel-render-buffer
     (current-buffer)
     '((:type comment :status "OPEN" :body "Rewrite intro."
	      :created-at "2026-08-06T20:22:00+0200"
	      :suggestion-thread-id "ai-thread-1"
	      :suggestion-ids "ai-1 ai-1.1"))
     nil)
    (let ((output (buffer-substring-no-properties (point-min) (point-max))))
      (should (string-match-p "2026-08-06 20:22 ✏️" output))
      (should-not (search-forward "ai-1 ai-1.1" nil t)))))

(ert-deftest org-comments-panel-render-overview-body-is-one-truncated-line ()
  "Overview cards show one compact body line without autofill expansion."
  (let ((org-comments-panel-overview-comment-line-length 18))
    (with-temp-buffer
      (org-comments-panel-render-buffer
       (current-buffer)
       '((:type comment :status "OPEN" :target-text "target"
		:body "one two three four five six seven"))
       nil)
      (let ((output (buffer-substring-no-properties (point-min) (point-max))))
	(should (string-match-p "one two three fou…" output))
	(should-not (string-match-p "five" output))))))

(ert-deftest org-comments-panel-render-current-comment-shows-replies ()
  "Current comments render full body and reply conversation."
  (with-temp-buffer
    (org-comments-panel-render-buffer
     (current-buffer)
     '((:type comment :status "OPEN" :body "Root body" :current t
	      :replies ((:type comment :status "OPEN" :body "Reply body"))))
     nil)
    (should (search-forward "Root body" nil t))
    (should (search-forward "↳ Reply" nil t))
    (should (search-forward "Reply body" nil t))))

(ert-deftest org-comments-panel-render-focused-reply-thread-is-provider-neutral ()
  "Focused reply threads render identically for normalized remote providers."
  (let* ((thread '(:type comment :status "OPEN" :target-text "Alpha"
			 :remote-id "root-1" :remote-author-display-name "Alice"
			 :created-at "2026-07-02T10:00:00Z"
			 :body "Root body" :current t
			 :replies ((:type comment :status "OPEN" :remote-id "reply-1"
					  :remote-author-display-name "Bob"
					  :created-at "2026-07-02T10:05:00Z"
					  :body "Remote reply")
				   (:type comment :status "OPEN"
					  :author "Carol"
					  :created-at "2026-07-02T10:10:00Z"
					  :body "Pending local reply"))))
	 (source-buffer (current-buffer))
	 (confluence-output
	  (with-temp-buffer
	    (org-comments-panel-render-buffer
	     source-buffer (list (append thread '(:backend confluence))) nil)
	    (buffer-substring-no-properties (point-min) (point-max))))
	 (google-output
	  (with-temp-buffer
	    (org-comments-panel-render-buffer
	     source-buffer (list (append thread '(:backend google-docs))) nil)
	    (buffer-substring-no-properties (point-min) (point-max)))))
    (should (equal confluence-output google-output))
    (should (string-match-p "↳ Bob · " confluence-output))
    (should (string-match-p "↳ Carol · " confluence-output))
    (should (string-match-p "Remote reply" confluence-output))
    (should (string-match-p "Pending local reply" confluence-output))))

(ert-deftest org-comments-panel-render-visual-fixture-is-provider-neutral ()
  "Shared visual fixture covers provider-neutral panel parity states."
  (let ((confluence-output
	 (org-comments-panel-render-test--fixture-output 'confluence))
	(google-output
	 (org-comments-panel-render-test--fixture-output 'google-docs)))
    (should (equal confluence-output google-output))
    (dolist (expected '("☁️ [OPEN] “selected paragraph”"
			"Alice · "
			"↳ Bob · "
			"↳ Carol"
			"☁️ [RESOLVED] “done paragraph”"
			"☁️ [TODO] “stale paragraph”"
			"⚠️"
			"✍️ [OPEN] “unconfirmed paragrap…”"
			"❓"
			"☁️ [OPEN] PAGE"
			"Page/footer provider note."))
      (should (string-match-p (regexp-quote expected) confluence-output)))))

(ert-deftest org-comments-panel-render-labels-synced-replies ()
  "Current comments render remote replies without noisy sync prefixes."
  (with-temp-buffer
    (org-comments-panel-render-buffer
     (current-buffer)
     '((:type comment :status "OPEN" :body "Root body" :current t
	      :replies ((:type comment :status "OPEN" :remote-id "r-1" :body "Reply body"))))
     nil)
    (should (search-forward "↳ Reply" nil t))
    (should-not (search-forward "synced" nil t))))

(ert-deftest org-comments-panel-render-overview-replies-omit-root-status ()
  "Overview reply summaries show sync state rather than root comment status."
  (with-temp-buffer
    (org-comments-panel-render-buffer
     (current-buffer)
     '((:type comment :id "root" :status "OPEN" :body "Root body"
	      :replies ((:type comment :id "reply" :status "OPEN" :body "Reply body"))))
     nil)
    (should (search-forward "↳ 1 reply" nil t))
    (should (search-forward "↳ Reply — Reply body" nil t))
    (should-not (search-forward "↳ [OPEN]" nil t))))

(ert-deftest org-comments-panel-render-overview-replies-carry-reply-property ()
  "Overview reply rows expose the reply record, not the root comment record."
  (with-temp-buffer
    (org-comments-panel-render-buffer
     (current-buffer)
     '((:type comment :id "root" :status "OPEN" :body "Root body"
	      :replies ((:type comment :id "reply" :sync-kind "reply" :body "Reply body"))))
     nil)
    (goto-char (point-min))
    (search-forward "Reply body")
    (let ((row (get-text-property (line-beginning-position)
				  'org-comments-comment)))
      (should (equal "reply" (plist-get row :id)))
      (should (equal "reply" (plist-get row :sync-kind))))))

(ert-deftest org-comments-panel-render-list-body-gets-wrap-prefix ()
  "Rendered list bodies set wrap-prefix on list lines."
  (with-temp-buffer
    (org-comments-panel-render-buffer
     (current-buffer)
     '((:type comment :status "OPEN" :body "- first item wraps" :current t))
     nil)
    (goto-char (point-min))
    (should (search-forward "- first item wraps" nil t))
    (should (equal "  " (get-text-property (line-beginning-position) 'wrap-prefix)))))

(ert-deftest org-comments-panel-render-rows-carry-comment-property ()
  "Rendered comments expose their records through text properties."
  (with-temp-buffer
    (let ((comment '(:type comment :id "c1" :status "OPEN" :body "Body")))
      (org-comments-panel-render-buffer (current-buffer) (list comment) nil)
      (goto-char (point-min))
      (search-forward "Body")
      (let ((row (get-text-property (line-beginning-position)
				    'org-comments-comment)))
	(should (equal "c1" (plist-get row :id)))
	(should (eq 'comments (plist-get row :provider)))
	(should (equal row (get-text-property (line-beginning-position)
					      'context-panels-item)))))))

(ert-deftest org-comments-panel-render-truncates-target-preview ()
  "Comment target previews stay compact on the row header line."
  (let ((org-comments-panel-target-preview-length 12))
    (with-temp-buffer
      (org-comments-panel-render-buffer
       (current-buffer)
       '((:type comment :status "OPEN"
		:target-text "a very long commented region preview"
		:body "Body."))
       nil)
      (should (search-forward "“a very long …”" nil t))
      (should-not (search-forward "commented region" nil t)))))

(ert-deftest org-comments-panel-render-clamps-overview-body ()
  "Overview comments show one compact body line."
  (let ((org-comments-panel-overview-comment-line-length 10))
    (with-temp-buffer
      (org-comments-panel-render-buffer
       (current-buffer)
       '((:type comment :status "OPEN" :target-text "target"
		:body "one two three four five six seven eight nine"))
       nil)
      (should (search-forward "one two t…" nil t))
      (should-not (search-forward "three" nil t)))))

(ert-deftest org-comments-panel-render-remote-missing-overview-is-compact ()
  "Remote-missing overview cards show only a warning icon."
  (with-temp-buffer
    (org-comments-panel-render-buffer
     (current-buffer)
     '((:type comment :status "OPEN" :remote-state missing :body "Gone"))
     nil)
    (should (search-forward "⚠" nil t))
    (should-not (search-forward "remote missing" nil t))))

(ert-deftest org-comments-panel-render-remote-missing-current-shows-detail ()
  "Focused remote-missing comments show explicit missing-since detail."
  (with-temp-buffer
    (org-comments-panel-render-buffer
     (current-buffer)
     '((:type comment :status "TODO" :current t
	      :remote-state missing
	      :remote-missing-at "2026-01-01T00:00:00+0000"
	      :body "Gone"))
     nil)
    (should (search-forward "remote missing since 2026-01-01" nil t))))

(provide 'org-comments-panel-render-test)
;;; org-comments-panel-render-test.el ends here
