;;; org-comments-test.el --- Org sidecar comment tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for region-targeted Org comments stored in colocated sidecar files.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'test-helpers)
(require 'hub-org-comments)
(require 'org/comments)
(require 'org/confluence)

(defmacro hub/org-comments-test--with-file-buffer (name contents &rest body)
  "Visit temp file NAME with CONTENTS, then run BODY."
  (declare (indent 2))
  `(let* ((dir (make-temp-file "hub-org-comments-" t))
	  (file (expand-file-name ,name dir)))
     (unwind-protect
	 (with-current-buffer (find-file-noselect file)
	   (erase-buffer)
	   (insert ,contents)
	   (save-buffer)
	   (org-mode)
	   ,@body)
       (delete-directory dir t))))

(ert-deftest hub/org-comment-sidecar-path-preserves-org-extension ()
  "Sidecar paths keep an Org extension for comments."
  (should (string-suffix-p "article.comments.org"
			   (hub/org-comment-sidecar-path "/tmp/article.org")))
  (should (string-suffix-p "article.en.comments.org"
			   (hub/org-comment-sidecar-path "/tmp/article.en.org"))))

(ert-deftest hub/org-comment-append-creates-readable-sidecar-entry ()
  "Appending a comment creates a plain Org sidecar with target metadata."
  (hub/org-comments-test--with-file-buffer "article.org" "Alpha selected text omega"
					   (let* ((start (progn (goto-char (point-min)) (search-forward "selected") (match-beginning 0)))
						  (end (match-end 0))
						  (record (hub/org-comment-create-record
							   buffer-file-name start end "Please clarify." "local-test"
							   "Ada" "2026-06-15T19:42:00+0200"))
						  (sidecar (hub/org-comment-append-to-sidecar record)))
					     (with-temp-buffer
					       (insert-file-contents sidecar)
					       (should (search-forward "#+title: Comments for article.org" nil t))
					       (should (search-forward "#+source: article.org" nil t))
					       (should (search-forward "#+todo: OPEN TODO | RESOLVED" nil t))
					       (should (search-forward "* OPEN Ada · “selected” — Please clarify." nil t))
					       (should (search-forward ":HUB_COMMENT_ID: local-test" nil t))
					       (should (search-forward ":HUB_COMMENT_AUTHOR: Ada" nil t))
					       (should (search-forward ":HUB_COMMENT_CREATED_AT: 2026-06-15T19:42:00+0200" nil t))
					       (should (search-forward ":HUB_COMMENT_TARGET:" nil t))
					       (should (search-forward ":HUB_COMMENT_TARGET_LINES:" nil t))
					       (should-not (search-forward ":HUB_COMMENT_SOURCE_FILE:" nil t))
					       (should-not (search-forward ":HUB_COMMENT_STATUS:" nil t))
					       (should-not (search-forward ":HUB_COMMENT_TARGET_START:" nil t))
					       (should (search-forward ":HUB_COMMENT_TARGET_TEXT: selected" nil t))
					       (should-not (search-forward ":HUB_COMMENT_TARGET_HASH:" nil t))
					       (should (search-forward "Please clarify." nil t))))))

(ert-deftest hub/org-comment-current-author-prefers-org-metadata ()
  "Local comment authors prefer Org metadata before Emacs user identity."
  (hub/org-comments-test--with-file-buffer "article.org" "#+AUTHOR: Document Author\n#+EMAIL: doc@example.com\n\nAlpha selected"
					   (let ((hub/org-comment-author nil)
						 (user-full-name "Emacs User"))
					     (should (equal (hub/org-comment-current-author) "Document Author")))))

(ert-deftest hub/org-comment-heading-title-decodes-html-entities ()
  "Readable headings decode Confluence storage HTML entities."
  (let ((hub/org-comment-heading-body-preview-length 60))
    (should (equal
	     "Page · Alice · It’s useful & clear"
	     (hub/org-comment-heading-title
	      '(:author "Alice" :sync-kind "footer" :body "<p>It&rsquo;s useful &amp; clear</p>"))))))

(ert-deftest hub/org-comment-anchor-imported-inline-comments-anchors-unique-match ()
  "Imported Confluence inline comments anchor when target text has one match."
  (hub/org-comments-test--with-file-buffer "article.org" "Alpha imported target omega"
					   (let ((sidecar (hub/org-comment-sidecar-path buffer-file-name)))
					     (with-temp-file sidecar
					       (insert "#+title: Comments for article.org\n")
					       (insert "#+source: article.org\n")
					       (insert "#+todo: OPEN TODO | RESOLVED\n\n")
					       (insert "* OPEN Remote inline\n")
					       (insert ":PROPERTIES:\n")
					       (insert ":HUB_COMMENT_ID: remote-confluence-i1\n")
					       (insert ":HUB_COMMENT_REMOTE_ID: i1\n")
					       (insert ":HUB_COMMENT_SOURCE: confluence\n")
					       (insert ":HUB_COMMENT_SYNC_KIND: inline\n")
					       (insert ":HUB_COMMENT_TARGET_TEXT: imported target\n")
					       (insert ":END:\n\n")
					       (insert "Body\n"))
					     (should (equal '(:anchored 1 :missing 0 :ambiguous 0)
							    (hub/org-comment--anchor-imported-inline-comments (current-buffer))))
					     (with-temp-buffer
					       (insert-file-contents sidecar)
					       (should (search-forward ":HUB_COMMENT_TARGET:" nil t))
					       (should (search-forward ":HUB_COMMENT_TARGET_LINES:" nil t))
					       (should-not (search-forward ":HUB_COMMENT_TARGET_HASH:" nil t))
					       (should-not (search-forward ":HUB_COMMENT_ANCHOR_STATE:" nil t))))))

(ert-deftest hub/org-comment-anchor-imported-inline-comments-shrinks-flattened-target ()
  "Imported Confluence inline comments anchor shrunk windows when fuzzy has none."
  (hub/org-comments-test--with-file-buffer "article.org" "| Whole Face domain | Strategically Network-shaped but not mature enough yet |\n| Face fraud intelligence / biometric attack response | Good candidate later |"
					   (let ((sidecar (hub/org-comment-sidecar-path buffer-file-name)))
					     (with-temp-file sidecar
					       (insert "#+title: Comments for article.org\n")
					       (insert "#+source: article.org\n")
					       (insert "#+todo: OPEN TODO | RESOLVED\n\n")
					       (insert "* OPEN Remote inline\n")
					       (insert ":PROPERTIES:\n")
					       (insert ":HUB_COMMENT_ID: remote-confluence-i1\n")
					       (insert ":HUB_COMMENT_SOURCE: confluence\n")
					       (insert ":HUB_COMMENT_SYNC_KIND: inline\n")
					       (insert ":HUB_COMMENT_TARGET_TEXT: Whole Face domainStrategically Network-shaped but not mature enough yetFace fraud intelligence / biometric attack responseGood candidate later\n")
					       (insert ":END:\n\n")
					       (insert "Body\n"))
					     (cl-letf (((symbol-function 'hub/org-comment--fuzzy-anchor-candidates)
							(lambda (&rest _args) nil)))
					       (should (equal '(:anchored 1 :missing 0 :ambiguous 0)
							      (hub/org-comment--anchor-imported-inline-comments (current-buffer)))))
					     (with-temp-buffer
					       (insert-file-contents sidecar)
					       (should (search-forward ":HUB_COMMENT_TARGET_TEXT: Strategically Network-shaped but not mature enough yet" nil t))
					       (should-not (search-forward ":HUB_COMMENT_ANCHOR_STATE:" nil t))))))

(ert-deftest hub/org-comment-anchor-imported-inline-comments-records-ambiguous ()
  "Imported Confluence inline comments record ambiguity for multiple matches."
  (hub/org-comments-test--with-file-buffer "article.org" "Alpha repeated beta repeated omega"
					   (let ((sidecar (hub/org-comment-sidecar-path buffer-file-name)))
					     (with-temp-file sidecar
					       (insert "#+title: Comments for article.org\n")
					       (insert "#+source: article.org\n")
					       (insert "#+todo: OPEN TODO | RESOLVED\n\n")
					       (insert "* OPEN Remote inline\n")
					       (insert ":PROPERTIES:\n")
					       (insert ":HUB_COMMENT_ID: remote-confluence-i1\n")
					       (insert ":HUB_COMMENT_SOURCE: confluence\n")
					       (insert ":HUB_COMMENT_SYNC_KIND: inline\n")
					       (insert ":HUB_COMMENT_TARGET_TEXT: repeated\n")
					       (insert ":END:\n\n")
					       (insert "Body\n"))
					     (should (equal '(:anchored 0 :missing 0 :ambiguous 1)
							    (hub/org-comment--anchor-imported-inline-comments (current-buffer))))
					     (with-temp-buffer
					       (insert-file-contents sidecar)
					       (should (search-forward ":HUB_COMMENT_ANCHOR_STATE: ambiguous" nil t))
					       (should (search-forward ":HUB_COMMENT_ANCHOR_MATCH_COUNT: 2" nil t))))))

(ert-deftest hub/org-comment-triage-imported-inline-comments-anchors-completion-choice ()
  "Batch triage anchors the completion-selected occurrence immediately."
  (hub/org-comments-test--with-file-buffer "article.org" "First repeated target.\n\nSecond repeated target."
					   (let ((sidecar (hub/org-comment-sidecar-path buffer-file-name)))
					     (with-temp-file sidecar
					       (insert "#+title: Comments for article.org\n")
					       (insert "#+source: article.org\n")
					       (insert "#+todo: OPEN TODO | RESOLVED\n\n")
					       (insert "* OPEN Remote inline\n")
					       (insert ":PROPERTIES:\n")
					       (insert ":HUB_COMMENT_ID: remote-confluence-i1\n")
					       (insert ":HUB_COMMENT_SOURCE: confluence\n")
					       (insert ":HUB_COMMENT_SYNC_KIND: inline\n")
					       (insert ":HUB_COMMENT_TARGET_TEXT: repeated target\n")
					       (insert ":HUB_COMMENT_ANCHOR_STATE: ambiguous\n")
					       (insert ":HUB_COMMENT_ANCHOR_MATCH_COUNT: 2\n")
					       (insert ":END:\n\n")
					       (insert "Please place me.\n"))
					     (cl-letf (((symbol-function 'completing-read)
							(lambda (_prompt candidates &rest _args)
							  (cl-find-if (lambda (candidate)
									(string-match-p "L3" candidate))
								      candidates))))
					       (should (equal '(:selected 1 :skipped 0)
							      (hub/org-comment--triage-imported-inline-comments
							       (current-buffer) sidecar))))
					     (with-temp-buffer
					       (insert-file-contents sidecar)
					       (should (search-forward ":HUB_COMMENT_TARGET:" nil t))
					       (should-not (search-forward ":HUB_COMMENT_ANCHOR_STATE:" nil t))))))

(ert-deftest hub/org-comment-compact-sidecar-metadata-removes-obsolete-properties ()
  "Compact metadata removes derivable properties without touching local author metadata."
  (let* ((dir (make-temp-file "hub-org-comments-" t))
	 (sidecar (expand-file-name "article.comments.org" dir)))
    (unwind-protect
	(progn
	  (with-temp-file sidecar
	    (insert "#+title: Comments for article.org\n#+source: article.org\n#+todo: OPEN TODO | RESOLVED\n\n")
	    (insert "* OPEN Remote\n:PROPERTIES:\n:HUB_COMMENT_ID: remote-confluence-r1\n:HUB_COMMENT_REMOTE_ID: r1\n:HUB_COMMENT_AUTHOR: Alice\n:HUB_COMMENT_CREATED_AT: 2026-01-01\n:HUB_COMMENT_TARGET_HASH: sha256:abc\n:HUB_COMMENT_PARENT_ID: remote-confluence-p1\n:HUB_COMMENT_BODY_FORMAT: storage\n:HUB_COMMENT_REMOTE_TARGET_JSON: {}\n:HUB_COMMENT_REMOTE_STATE: present\n:END:\n\nBody\n")
	    (insert "* OPEN Local\n:PROPERTIES:\n:HUB_COMMENT_ID: local-1\n:HUB_COMMENT_AUTHOR: Local Author\n:HUB_COMMENT_CREATED_AT: 2026-02-02\n:HUB_COMMENT_TARGET_HASH: sha256:def\n:END:\n\nBody\n"))
	  (should (= 8 (hub/org-comment-compact-sidecar-metadata sidecar)))
	  (with-temp-buffer
	    (insert-file-contents sidecar)
	    (should-not (search-forward ":HUB_COMMENT_TARGET_HASH:" nil t))
	    (should-not (search-forward ":HUB_COMMENT_PARENT_ID:" nil t))
	    (should-not (search-forward ":HUB_COMMENT_BODY_FORMAT: storage" nil t))
	    (should-not (search-forward ":HUB_COMMENT_REMOTE_TARGET_JSON:" nil t))
	    (should-not (search-forward ":HUB_COMMENT_REMOTE_STATE: present" nil t))
	    (should-not (search-forward ":HUB_COMMENT_AUTHOR: Alice" nil t))
	    (should (search-forward ":HUB_COMMENT_AUTHOR: Local Author" nil t))
	    (should (search-forward ":HUB_COMMENT_CREATED_AT: 2026-02-02" nil t))))
      (delete-directory dir t))))

(ert-deftest hub/org-comment-refresh-sidecar-headings-renames-existing-entry ()
  "Refreshing sidecar headings recomputes readable titles from metadata and body."
  (let* ((dir (make-temp-file "hub-org-comments-" t))
	 (sidecar (expand-file-name "article.comments.org" dir)))
    (unwind-protect
	(progn
	  (with-temp-file sidecar
	    (insert "#+title: Comments for article.org\n")
	    (insert "#+source: article.org\n")
	    (insert "#+todo: OPEN TODO | RESOLVED\n\n")
	    (insert "* TODO Old machine title\n")
	    (insert ":PROPERTIES:\n")
	    (insert ":HUB_COMMENT_ID: remote-confluence-1\n")
	    (insert ":HUB_COMMENT_AUTHOR: Alice Example\n")
	    (insert ":HUB_COMMENT_SOURCE: confluence\n")
	    (insert ":HUB_COMMENT_SYNC_KIND: inline\n")
	    (insert ":HUB_COMMENT_TARGET_TEXT: selected target text\n")
	    (insert ":END:\n\n")
	    (insert "<p>Please clarify this part.</p>\n"))
	  (should (= 1 (hub/org-comment-refresh-sidecar-headings sidecar)))
	  (with-temp-buffer
	    (insert-file-contents sidecar)
	    (should (search-forward
		     "* TODO Alice Example · “selected target text” — Please clarify this part."
		     nil t))
	    (should (search-forward ":HUB_COMMENT_ID: remote-confluence-1" nil t))
	    (should (search-forward "<p>Please clarify this part.</p>" nil t))))
      (delete-directory dir t))))

(ert-deftest hub/org-comment-collects-author-and-created-at ()
  "Comment collection includes author and creation metadata."
  (hub/org-comments-test--with-file-buffer "article.org" "Alpha selected text omega"
					   (let* ((start (progn (goto-char (point-min)) (search-forward "selected") (match-beginning 0)))
						  (end (match-end 0))
						  (record (hub/org-comment-create-record
							   buffer-file-name start end "Body." "local-meta"
							   "Ada" "2026-06-15T19:42:00+0200")))
					     (hub/org-comment-append-to-sidecar record)
					     (let ((comment (car (hub/org-comment-collect (current-buffer)))))
					       (should (equal (plist-get comment :author) "Ada"))
					       (should (equal (plist-get comment :created-at)
							      "2026-06-15T19:42:00+0200"))))))

(ert-deftest hub/org-comment-collects-prefers-remote-created-at ()
  "Comment collection prefers remote timestamps for display when available."
  (hub/org-comments-test--with-file-buffer "article.org" "Alpha selected text omega"
					   (let* ((start (progn
							   (goto-char (point-min))
							   (search-forward "selected")
							   (match-beginning 0)))
						  (end (match-end 0))
						  (sidecar (hub/org-comment-append-to-sidecar
							    (hub/org-comment-create-record
							     buffer-file-name start end "Body." "local-remote-time"
							     "Ada" "2026-06-15T19:42:00+0200"))))
					     (with-temp-buffer
					       (insert-file-contents sidecar)
					       (goto-char (point-min))
					       (search-forward ":HUB_COMMENT_CREATED_AT:")
					       (forward-line 1)
					       (insert ":HUB_COMMENT_REMOTE_CREATED_AT: 2026-06-16T10:00:00.000Z\n")
					       (write-region (point-min) (point-max) sidecar nil 'silent))
					     (let ((comment (car (hub/org-comment-collect (current-buffer)))))
					       (should (equal (plist-get comment :created-at)
							      "2026-06-16T10:00:00.000Z"))))))

(ert-deftest hub/org-comment-collects-stale-comments-when-requested ()
  "Comment collection can include unanchored records whose targets drifted."
  (hub/org-comments-test--with-file-buffer "article.org" "Alpha selected text omega"
					   (let* ((start (progn (goto-char (point-min)) (search-forward "selected") (match-beginning 0)))
						  (end (match-end 0))
						  (record (hub/org-comment-create-record buffer-file-name start end "Drifted." "local-stale")))
					     (hub/org-comment-append-to-sidecar record)
					     (save-excursion
					       (goto-char start)
					       (delete-char 1)
					       (insert "S"))
					     (should-not (hub/org-comment-collect (current-buffer)))
					     (let ((comments (hub/org-comment-collect (current-buffer) t)))
					       (should (= 1 (length comments)))
					       (should (eq 'stale (plist-get (car comments) :anchor-state)))
					       (should-not (plist-get (car comments) :jump-pos))
					       (should (equal "local-stale" (plist-get (car comments) :id)))))))

(ert-deftest hub/org-comment-collects-only-matching-offset-comments ()
  "Comment collection ignores sidecar records whose target offsets drifted."
  (hub/org-comments-test--with-file-buffer "article.org" "Alpha selected text omega"
					   (let* ((start (progn (goto-char (point-min)) (search-forward "selected") (match-beginning 0)))
						  (end (match-end 0))
						  (record (hub/org-comment-create-record buffer-file-name start end "Good target." "local-good")))
					     (hub/org-comment-append-to-sidecar record)
					     (let ((comments (hub/org-comment-collect (current-buffer))))
					       (should (= 1 (length comments)))
					       (should (equal "local-good" (plist-get (car comments) :id)))
					       (should (= start (plist-get (car comments) :jump-pos))))
					     (save-excursion
					       (goto-char start)
					       (delete-char 1)
					       (insert "S"))
					     (should-not (hub/org-comment-collect (current-buffer))))))

(ert-deftest hub/org-comment-create-command-writes-sidecar-and-opens-body ()
  "The interactive command writes a sidecar comment and opens its body."
  (hub/org-comments-test--with-file-buffer "article.org" "Alpha selected text omega"
					   (let ((start (progn (goto-char (point-min)) (search-forward "selected") (match-beginning 0)))
						 (end (match-end 0)))
					     (let ((sidecar (hub/org-comment-sidecar-path buffer-file-name)))
					       (cl-letf (((symbol-function 'hub/org-comment-generate-id)
							  (lambda () "local-command")))
						 (hub/org-comment-create start end "Please revise."))
					       (should (equal sidecar buffer-file-name))
					       (should (looking-at-p "Please revise\."))
					       (with-temp-buffer
						 (insert-file-contents sidecar)
						 (should (search-forward ":HUB_COMMENT_ID: local-command" nil t))
						 (should (search-forward "Please revise." nil t)))))))

(ert-deftest hub/org-comment-reanchor-uses-only-stale-comment-without-prompt ()
  "Reanchoring one stale comment does not require a picker."
  (hub/org-comments-test--with-file-buffer "article.org" "Alpha selected text omega"
					   (let* ((old-start (progn
							       (goto-char (point-min))
							       (search-forward "selected")
							       (match-beginning 0)))
						  (old-end (match-end 0)))
					     (hub/org-comment-append-to-sidecar
					      (hub/org-comment-create-record buffer-file-name old-start old-end "Repair." "local-single"))
					     (save-excursion
					       (goto-char old-start)
					       (delete-char 1)
					       (insert "S"))
					     (let ((new-start (progn
								(goto-char (point-min))
								(search-forward "text")
								(match-beginning 0)))
						   (new-end (match-end 0)))
					       (cl-letf (((symbol-function 'hub/org-context-panel-open) #'ignore)
							 ((symbol-function 'completing-read)
							  (lambda (&rest _)
							    (error "Reanchor should not prompt while hardening visual commands"))))
						 (hub/org-comment-reanchor new-start new-end))
					       (should (equal "text" (plist-get (car (hub/org-comment-collect (current-buffer)))
										:target-text)))))))

(ert-deftest hub/org-comment-reanchor-prompts-for-multiple-stale-comments ()
  "Reanchoring multiple stale comments uses a completion picker."
  (hub/org-comments-test--with-file-buffer "article.org" "Alpha selected text omega tail"
					   (let* ((selected-start (progn
								    (goto-char (point-min))
								    (search-forward "selected")
								    (match-beginning 0)))
						  (selected-end (match-end 0))
						  (omega-start (progn
								 (search-forward "omega")
								 (match-beginning 0)))
						  (omega-end (match-end 0)))
					     (hub/org-comment-append-to-sidecar
					      (hub/org-comment-create-record buffer-file-name selected-start selected-end "First." "local-first"))
					     (hub/org-comment-append-to-sidecar
					      (hub/org-comment-create-record buffer-file-name omega-start omega-end "Second." "local-second"))
					     (save-excursion
					       (goto-char selected-start)
					       (delete-char 1)
					       (insert "S")
					       (goto-char omega-start)
					       (delete-char 1)
					       (insert "O"))
					     (let ((new-start (progn
								(goto-char (point-min))
								(search-forward "tail")
								(match-beginning 0)))
						   (new-end (match-end 0))
						   seen-category
						   seen-candidates)
					       (cl-letf (((symbol-function 'hub/org-context-panel-open) #'ignore)
							 ((symbol-function 'completing-read)
							  (lambda (_prompt collection &rest _args)
							    (let ((metadata (funcall collection "" nil 'metadata)))
							      (setq seen-category (alist-get 'category (cdr metadata))
								    seen-candidates (all-completions "" collection)))
							    "OPEN “omega” — Second.")))
						 (hub/org-comment-reanchor new-start new-end))
					       (should (eq 'hub-org-comment seen-category))
					       (should (member "OPEN “selected” — First." seen-candidates))
					       (should (member "OPEN “omega” — Second." seen-candidates))
					       (let ((comments (hub/org-comment-collect (current-buffer) t)))
						 (should (equal "tail" (plist-get
									(cl-find "local-second" comments
										 :key (lambda (comment)
											(plist-get comment :id))
										 :test #'equal)
									:target-text))))))))

(ert-deftest hub/org-comment-reanchor-updates-stale-comment-target ()
  "Reanchoring updates stale comment metadata to the selected source region."
  (hub/org-comments-test--with-file-buffer "article.org" "Alpha selected text omega"
					   (let* ((old-start (progn
							       (goto-char (point-min))
							       (search-forward "selected")
							       (match-beginning 0)))
						  (old-end (match-end 0))
						  (sidecar (hub/org-comment-append-to-sidecar
							    (hub/org-comment-create-record buffer-file-name old-start old-end "Repair." "local-reanchor"))))
					     (save-excursion
					       (goto-char old-start)
					       (delete-char 1)
					       (insert "S"))
					     (let* ((new-start (progn
								 (goto-char (point-min))
								 (search-forward "text")
								 (match-beginning 0)))
						    (new-end (match-end 0))
						    (stale (car (hub/org-comment-collect (current-buffer) t))))
					       (cl-letf (((symbol-function 'hub/org-context-panel-open) #'ignore))
						 (hub/org-comment-reanchor new-start new-end stale))
					       (should (= new-start (point)))
					       (let ((comments (hub/org-comment-collect (current-buffer))))
						 (should (= 1 (length comments)))
						 (should (equal "local-reanchor" (plist-get (car comments) :id)))
						 (should (= new-start (plist-get (car comments) :target-start)))
						 (should (equal "text" (plist-get (car comments) :target-text))))
					       (with-temp-buffer
						 (insert-file-contents sidecar)
						 (should (search-forward ":HUB_COMMENT_TARGET_TEXT: text" nil t))
						 (should-not (search-forward ":HUB_COMMENT_TARGET_HASH:" nil t)))))))

(ert-deftest hub/org-comment-edit-jumps-to-body-and-narrows-subtree ()
  "Editing an active comment opens its sidecar body narrowed to the subtree."
  (hub/org-comments-test--with-file-buffer "article.org" "Alpha selected text omega"
					   (let* ((start (progn
							   (goto-char (point-min))
							   (search-forward "selected")
							   (match-beginning 0)))
						  (end (match-end 0))
						  (sidecar (hub/org-comment-append-to-sidecar
							    (hub/org-comment-create-record buffer-file-name start end "Edit this." "local-edit"))))
					     (unwind-protect
						 (progn
						   (goto-char start)
						   (hub/org-comment-edit)
						   (should (equal sidecar buffer-file-name))
						   (should (buffer-narrowed-p))
						   (should (looking-at-p "Edit this\\."))
						   (should (save-excursion
							     (goto-char (point-min))
							     (search-forward "“selected”" nil t))))
					       (when (get-file-buffer sidecar)
						 (kill-buffer (get-file-buffer sidecar)))))))

(ert-deftest hub/org-comment-edit-empty-body-does-not-stay-on-heading ()
  "Editing an empty sidecar comment lands in the body area, not on the title."
  (hub/org-comments-test--with-file-buffer "article.org" "Alpha selected text omega"
					   (let* ((start (progn
							   (goto-char (point-min))
							   (search-forward "selected")
							   (match-beginning 0)))
						  (end (match-end 0))
						  (sidecar (hub/org-comment-append-to-sidecar
							    (hub/org-comment-create-record buffer-file-name start end "" "local-empty"))))
					     (unwind-protect
						 (progn
						   (goto-char start)
						   (hub/org-comment-edit)
						   (should (equal sidecar buffer-file-name))
						   (should (buffer-narrowed-p))
						   (should-not (looking-at-p "\\* OPEN .*“selected”"))
						   (let ((body-point (point)))
						     (should (save-excursion
							       (goto-char (point-min))
							       (search-forward ":END:" body-point t)))))
					       (when (get-file-buffer sidecar)
						 (kill-buffer (get-file-buffer sidecar)))))))

(ert-deftest hub/org-comment-jump-to-sidecar-opens-heading ()
  "Jumping to sidecar moves to the active comment heading."
  (hub/org-comments-test--with-file-buffer "article.org" "Alpha selected text omega"
					   (let* ((start (progn
							   (goto-char (point-min))
							   (search-forward "selected")
							   (match-beginning 0)))
						  (end (match-end 0))
						  (sidecar (hub/org-comment-append-to-sidecar
							    (hub/org-comment-create-record buffer-file-name start end "Jump." "local-jump"))))
					     (unwind-protect
						 (progn
						   (goto-char start)
						   (hub/org-comment-jump-to-sidecar)
						   (should (equal sidecar buffer-file-name))
						   (should (looking-at-p "\\* OPEN .*“selected”")))
					       (when (get-file-buffer sidecar)
						 (kill-buffer (get-file-buffer sidecar)))))))

(ert-deftest hub/org-comment-open-sidecar-folds-property-drawers ()
  "Opening the sidecar folds verbose comment property drawers by default."
  (let* ((dir (make-temp-file "hub-org-comments-" t))
	 (source (expand-file-name "article.org" dir))
	 (sidecar (expand-file-name "article.comments.org" dir)))
    (unwind-protect
	(progn
	  (with-temp-file source (insert "Body\n"))
	  (with-temp-file sidecar
	    (insert "#+title: Comments for article.org\n#+source: article.org\n#+todo: OPEN TODO | RESOLVED\n\n")
	    (insert "* OPEN Comment\n:PROPERTIES:\n:HUB_COMMENT_ID: local-fold\n:END:\n\nBody\n"))
	  (with-current-buffer (find-file-noselect source)
	    (hub/org-comment-open-sidecar))
	  (with-current-buffer (find-buffer-visiting sidecar)
	    (goto-char (point-min))
	    (search-forward ":HUB_COMMENT_ID:")
	    (should (invisible-p (point)))))
      (when (get-file-buffer source)
	(kill-buffer (get-file-buffer source)))
      (when (get-file-buffer sidecar)
	(kill-buffer (get-file-buffer sidecar)))
      (delete-directory dir t))))

(ert-deftest hub/org-comment-open-sidecar-opens-existing-file ()
  "Opening the sidecar visits the comments file when it exists."
  (hub/org-comments-test--with-file-buffer "article.org" "Alpha selected text omega"
					   (let* ((start (progn
							   (goto-char (point-min))
							   (search-forward "selected")
							   (match-beginning 0)))
						  (end (match-end 0))
						  (sidecar (hub/org-comment-append-to-sidecar
							    (hub/org-comment-create-record buffer-file-name start end "Open." "local-open"))))
					     (unwind-protect
						 (progn
						   (hub/org-comment-open-sidecar)
						   (should (equal sidecar buffer-file-name)))
					       (when (get-file-buffer sidecar)
						 (kill-buffer (get-file-buffer sidecar)))))))

(ert-deftest hub/confluence-comment-sidecar-submit-key-is-local ()
  "Bind C-c C-c to Confluence push only in comments sidecar buffers."
  (hub/org-comments-test--with-file-buffer "article.org" "#+TITLE: Article\n\nBody"
					   (let ((source-buffer (current-buffer))
						 (sidecar (hub/org-comment-sidecar-path buffer-file-name)))
					     (should-not (eq (local-key-binding (kbd "C-c C-c"))
							     #'hub/confluence-comment-push-current))
					     (with-temp-file sidecar
					       (insert "#+source: article.org\n#+todo: OPEN TODO | RESOLVED\n\n"))
					     (with-current-buffer (find-file-noselect sidecar)
					       (org-mode)
					       (should (eq (local-key-binding (kbd "C-c C-c"))
							   #'hub/confluence-comment-push-current)))
					     (when (get-file-buffer sidecar)
					       (kill-buffer (get-file-buffer sidecar)))
					     (should (buffer-live-p source-buffer)))))

(ert-deftest hub/org-comment-open-sidecar-messages-when-absent ()
  "Opening a missing sidecar only reports in the minibuffer."
  (hub/org-comments-test--with-file-buffer "article.org" "Alpha"
					   (let ((opened nil)
						 (reported nil))
					     (cl-letf (((symbol-function 'find-file)
							(lambda (&rest _args) (setq opened t)))
						       ((symbol-function 'message)
							(lambda (format-string &rest args)
							  (setq reported (apply #'format format-string args)))))
					       (hub/org-comment-open-sidecar))
					     (should-not opened)
					     (should (string-match-p "No sidecar comments file" reported)))))

(ert-deftest hub/org-comment-delete-removes-active-source-comment ()
  "Deleting from the source removes the active sidecar comment after confirmation."
  (hub/org-comments-test--with-file-buffer "article.org" "Alpha selected text omega"
					   (let* ((start (progn
							   (goto-char (point-min))
							   (search-forward "selected")
							   (match-beginning 0)))
						  (end (match-end 0))
						  (sidecar (hub/org-comment-append-to-sidecar
							    (hub/org-comment-create-record buffer-file-name start end "Delete." "local-delete"))))
					     (goto-char start)
					     (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _args) t))
						       ((symbol-function 'hub/org-context-panel-open) #'ignore))
					       (hub/org-comment-delete))
					     (with-temp-buffer
					       (insert-file-contents sidecar)
					       (should-not (search-forward "local-delete" nil t))))))

(ert-deftest hub/org-comment-delete-removes-sidecar-heading ()
  "Deleting from the sidecar removes the current comment subtree."
  (hub/org-comments-test--with-file-buffer "article.org" "Alpha selected text omega"
					   (let* ((start (progn
							   (goto-char (point-min))
							   (search-forward "selected")
							   (match-beginning 0)))
						  (end (match-end 0))
						  (sidecar (hub/org-comment-append-to-sidecar
							    (hub/org-comment-create-record buffer-file-name start end "Delete sidecar." "local-sidecar-delete"))))
					     (unwind-protect
						 (progn
						   (find-file sidecar)
						   (goto-char (point-min))
						   (search-forward "local-sidecar-delete")
						   (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _args) t)))
						     (hub/org-comment-delete))
						   (should-not (search-forward "local-sidecar-delete" nil t)))
					       (when (get-file-buffer sidecar)
						 (kill-buffer (get-file-buffer sidecar)))))))

(ert-deftest hub/org-comment-status-commands-update-active-sidecar-heading ()
  "Comment status commands update the active sidecar heading TODO keyword."
  (hub/org-comments-test--with-file-buffer "article.org" "Alpha selected text omega"
					   (let* ((start (progn
							   (goto-char (point-min))
							   (search-forward "selected")
							   (match-beginning 0)))
						  (end (match-end 0))
						  (sidecar (hub/org-comment-append-to-sidecar
							    (hub/org-comment-create-record buffer-file-name start end "Act." "local-status"))))
					     (cl-letf (((symbol-function 'hub/org-context-panel-open) #'ignore))
					       (goto-char start)
					       (hub/org-comment-mark-todo)
					       (with-temp-buffer
						 (insert-file-contents sidecar)
						 (should (search-forward "* TODO " nil t))
						 (should (search-forward "“selected”" nil t)))
					       (hub/org-comment-cycle-status)
					       (with-temp-buffer
						 (insert-file-contents sidecar)
						 (should (search-forward "* RESOLVED " nil t))
						 (should (search-forward "“selected”" nil t)))
					       (hub/org-comment-mark-open)
					       (with-temp-buffer
						 (insert-file-contents sidecar)
						 (should (search-forward "* OPEN " nil t))
						 (should (search-forward "“selected”" nil t)))))))

(ert-deftest hub/org-comment-navigation-wraps-and-opens-panel ()
  "Comment navigation jumps by target position and refreshes context UI."
  (hub/org-comments-test--with-file-buffer "article.org" "Alpha first beta second omega"
					   (let* ((first-start (progn
								 (goto-char (point-min))
								 (search-forward "first")
								 (match-beginning 0)))
						  (first-end (match-end 0))
						  (second-start (progn
								  (search-forward "second")
								  (match-beginning 0)))
						  (second-end (match-end 0))
						  (opened 0))
					     (hub/org-comment-append-to-sidecar
					      (hub/org-comment-create-record buffer-file-name first-start first-end "First." "local-first"))
					     (hub/org-comment-append-to-sidecar
					      (hub/org-comment-create-record buffer-file-name second-start second-end "Second." "local-second"))
					     (cl-letf (((symbol-function 'hub/org-context-panel-open)
							(lambda () (setq opened (1+ opened)))))
					       (goto-char (point-min))
					       (hub/org-comment-next)
					       (should (= first-start (point)))
					       (hub/org-comment-next)
					       (should (= second-start (point)))
					       (hub/org-comment-next)
					       (should (= first-start (point)))
					       (hub/org-comment-previous)
					       (should (= second-start (point)))
					       (should (= 4 opened))))))

(ert-deftest hub/org-comment-navigation-stops-at-page-marker ()
  "Comment navigation includes a page-comments stop at the metadata boundary."
  (hub/org-comments-test--with-file-buffer "article.org" "#+TITLE: Article\n\nAlpha selected text omega"
					   (let* ((start (progn
							   (goto-char (point-min))
							   (search-forward "selected")
							   (match-beginning 0)))
						  (end (match-end 0))
						  (sidecar (hub/org-comment-append-to-sidecar
							    (hub/org-comment-create-record buffer-file-name start end "Anchored." "local-anchored"))))
					     (with-temp-buffer
					       (insert-file-contents sidecar)
					       (goto-char (point-max))
					       (insert "\n* OPEN Confluence footer comment f1\n")
					       (insert ":PROPERTIES:\n")
					       (insert ":HUB_COMMENT_ID: remote-confluence-f1\n")
					       (insert ":HUB_COMMENT_REMOTE_ID: f1\n")
					       (insert ":HUB_COMMENT_SOURCE: confluence\n")
					       (insert ":HUB_COMMENT_SYNC_KIND: footer\n")
					       (insert ":HUB_COMMENT_BODY_FORMAT: storage\n")
					       (insert ":END:\n\nFooter\n")
					       (write-region (point-min) (point-max) sidecar nil 'silent))
					     (hub/org-comment-overlays-refresh)
					     (goto-char (point-min))
					     (hub/org-comment-next)
					     (should (= (point) (hub/org-context-panel-page-comment-marker-position)))
					     (hub/org-comment-next)
					     (should (= (point) start))
					     (hub/org-comment-previous)
					     (should (= (point) (hub/org-context-panel-page-comment-marker-position))))))

(ert-deftest hub/org-context-panel-ret-at-page-marker-opens-page-comments ()
  "RET at the page marker opens the bottom page-context panel."
  (hub/org-comments-test--with-file-buffer "article.org" "#+TITLE: Article\n\nBody"
					   (let ((sidecar (hub/org-comment-sidecar-path buffer-file-name))
						 (opened nil))
					     (with-temp-file sidecar
					       (insert "#+title: Comments for article.org\n")
					       (insert "#+todo: OPEN TODO | RESOLVED\n\n")
					       (insert "* OPEN Confluence footer comment f1\n")
					       (insert ":PROPERTIES:\n")
					       (insert ":HUB_COMMENT_ID: remote-confluence-f1\n")
					       (insert ":HUB_COMMENT_REMOTE_ID: f1\n")
					       (insert ":HUB_COMMENT_SOURCE: confluence\n")
					       (insert ":HUB_COMMENT_SYNC_KIND: footer\n")
					       (insert ":HUB_COMMENT_BODY_FORMAT: storage\n")
					       (insert ":END:\n\nFooter\n"))
					     (hub/org-comment-overlays-refresh)
					     (goto-char (hub/org-context-panel-page-comment-marker-position))
					     (cl-letf (((symbol-function 'hub/org-page-comments-open)
							(lambda () (setq opened t))))
					       (hub/org-context-panel-jump-to-item-at-point))
					     (should opened))))

(ert-deftest hub/org-page-context-navigation-wraps ()
  "Page context uses context-panel ]c/[c navigation."
  (let ((buffer (generate-new-buffer " *page context navigation test*")))
    (unwind-protect
	(with-current-buffer buffer
	  (hub/org-context-panel-buffer-mode)
	  (let ((inhibit-read-only t))
	    (let ((first-start (point)))
	      (insert "First\n")
	      (add-text-properties first-start (point) '(hub-org-context-panel-item (:id "first")))
	      (insert "\n")
	      (let ((second-start (point)))
		(insert "Second\n")
		(add-text-properties second-start (point) '(hub-org-context-panel-item (:id "second")))
		(goto-char (point-min))
		(hub/org-context-panel-next-item)
		(should (= (point) second-start))
		(hub/org-context-panel-next-item)
		(should (= (point) first-start))
		(hub/org-context-panel-previous-item)
		(should (= (point) second-start))))))
      (kill-buffer buffer))))

(ert-deftest hub/org-context-panel-overview-height-counts-reply-summary ()
  "Overview card height reserves space for the reply summary line."
  (let* ((item '(:type comment
		       :id "c1"
		       :author "Alice"
		       :created-at "2026-06-19T10:00:00+0000"
		       :body "Short body."
		       :replies ((:id "r1" :body "Reply."))))
	 (prepared (hub/org-context-panel--item-with-overview-height item)))
    (should (= (plist-get prepared :height) 4))))

(ert-deftest hub/org-context-panel-highlights-current-user-author ()
  "Context panel highlights only the current user's author segment."
  (let* ((dir (make-temp-file "hub-context-me-" t))
	 (global-dir (make-temp-file "hub-context-me-global-" t))
	 (org-directory global-dir)
	 (source (expand-file-name "article.org" dir))
	 (people (hub/confluence-people-local-file dir)))
    (unwind-protect
	(progn
	  (with-temp-file source
	    (insert "#+TITLE: Article\n\nBody\n"))
	  (with-temp-file people
	    (insert "* Hubert\n:PROPERTIES:\n:HUB_CONFLUENCE_ACCOUNT_ID: acct-me\n:HUB_PERSON_DISPLAY_NAME: Hubert\n:HUB_PERSON_ME: t\n:END:\n"))
	  (with-current-buffer (find-file-noselect source)
	    (org-mode)
	    (let ((hub/org-context-panel-source-buffer (current-buffer)))
	      (with-temp-buffer
		(hub/org-context-panel--insert-comment-metadata
		 '(:remote-author-id "acct-me" :remote-author-display-name "Hubert"
				     :created-at "2026-06-19T20:00:00+0000"))
		(goto-char (point-min))
		(should (equal (get-text-property (point) 'face)
			       'hub/org-context-panel-current-author-face))
		(search-forward " · ")
		(should (equal (get-text-property (point) 'face)
			       'hub/org-context-panel-metadata-face))))))
      (when-let* ((buffer (find-buffer-visiting source)))
	(kill-buffer buffer))
      (delete-directory dir t)
      (delete-directory global-dir t))))

(ert-deftest hub/org-context-panel-renders-comment-sync-badges ()
  "Context panel comments show draft, remote, and degraded sync badges."
  (hub/org-comments-test--with-file-buffer "article.org" "Alpha selected text omega.\nBeta other text done.\nGamma third text done.\nDelta fourth text done."
					   (let* ((source-buffer (current-buffer))
						  (sidecar (hub/org-comment-sidecar-path buffer-file-name))
						  (panel (generate-new-buffer " *org context sync badges test*")))
					     (unwind-protect
						 (progn
						   (with-temp-file sidecar
						     (insert "#+source: article.org\n#+todo: OPEN TODO | RESOLVED\n\n")
						     (insert "* OPEN Draft\n:PROPERTIES:\n:HUB_COMMENT_ID: draft\n:HUB_COMMENT_SYNC_KIND: inline\n:HUB_COMMENT_TARGET: 7 20\n:HUB_COMMENT_TARGET_TEXT: selected text\n:END:\n\nDraft body.\n")
						     (insert "* OPEN Remote\n:PROPERTIES:\n:HUB_COMMENT_ID: remote\n:HUB_COMMENT_SYNC_KIND: inline\n:HUB_COMMENT_SOURCE: confluence\n:HUB_COMMENT_REMOTE_ID: i123\n:HUB_COMMENT_TARGET: 33 43\n:HUB_COMMENT_TARGET_TEXT: other text\n:END:\n\nRemote body.\n")
						     (insert "* OPEN Missing\n:PROPERTIES:\n:HUB_COMMENT_ID: missing\n:HUB_COMMENT_SYNC_KIND: inline\n:HUB_COMMENT_SOURCE: confluence\n:HUB_COMMENT_REMOTE_ID: i456\n:HUB_COMMENT_REMOTE_STATE: missing\n:HUB_COMMENT_TARGET: 57 67\n:HUB_COMMENT_TARGET_TEXT: third text\n:END:\n\nMissing body.\n")
						     (insert "* OPEN Dangling\n:PROPERTIES:\n:HUB_COMMENT_ID: dangling\n:HUB_COMMENT_SYNC_KIND: inline\n:HUB_COMMENT_SOURCE: confluence\n:HUB_COMMENT_REMOTE_ID: i789\n:HUB_COMMENT_REMOTE_ANCHOR_STATE: dangling\n:HUB_COMMENT_TARGET: 81 92\n:HUB_COMMENT_TARGET_TEXT: fourth text\n:END:\n\nDangling body.\n")
						     (insert "* OPEN Page\n:PROPERTIES:\n:HUB_COMMENT_ID: page\n:HUB_COMMENT_SYNC_KIND: footer\n:END:\n\nPage body.\n"))
						   (hub/org-context-panel-render-buffer source-buffer panel)
						   (with-current-buffer panel
						     (should (search-forward "✍️" nil t))
						     (should (search-forward "🔗" nil t))
						     (should (search-forward "⚠" nil t))
						     (should-not (search-forward "✍️ draft" nil t))
						     (should-not (search-forward "🔗 confluence" nil t))
						     (should-not (search-forward "⚠ dangling" nil t))
						     (should (equal (hub/org-context-panel--comment-sync-badge
								     '(:remote-anchor-state "unconfirmed"))
								    "❓"))
						     (goto-char (point-min))
						     (should-not (search-forward "Page body." nil t))
						     (should-not (search-forward "open remote" nil t)))
						   (hub/org-page-context-render-buffer source-buffer panel t)
						   (with-current-buffer panel
						     (should (search-forward "👆" nil t))
						     (should (search-forward "Page body." nil t))
						     (should-not (search-forward "PAGE" nil t))))
					       (when (buffer-live-p panel)
						 (kill-buffer panel))))))

(ert-deftest hub/org-context-panel-filters-show-missing-by-default ()
  "Context filters show remote-missing comments by default and zx hides them."
  (hub/org-comments-test--with-file-buffer "article.org" "Alpha selected text omega."
					   (let* ((source-buffer (current-buffer))
						  (sidecar (hub/org-comment-sidecar-path buffer-file-name))
						  (panel (generate-new-buffer " *org context filter missing test*")))
					     (unwind-protect
						 (progn
						   (with-temp-file sidecar
						     (insert "#+source: article.org\n#+todo: OPEN TODO | RESOLVED\n\n")
						     (insert "* OPEN Missing\n:PROPERTIES:\n:HUB_COMMENT_ID: missing\n:HUB_COMMENT_SYNC_KIND: inline\n:HUB_COMMENT_REMOTE_ID: i1\n:HUB_COMMENT_REMOTE_STATE: missing\n:HUB_COMMENT_TARGET: 7 20\n:HUB_COMMENT_TARGET_TEXT: selected text\n:END:\n\nMissing body.\n"))
						   (hub/org-context-panel-render-buffer source-buffer panel)
						   (with-current-buffer panel
						     (should (search-forward "Missing body." nil t))
						     (hub/org-context-panel-filter-toggle-missing))
						   (hub/org-context-panel-render-buffer source-buffer panel)
						   (with-current-buffer panel
						     (goto-char (point-min))
						     (should-not (search-forward "Missing body." nil t))))
					       (when (buffer-live-p panel)
						 (kill-buffer panel))))))

(ert-deftest hub/org-context-panel-filter-matching-reply-shows-root-context ()
  "Reply filters keep the parent root visible as conversation context."
  (let* ((dir (make-temp-file "hub-context-filter-reply-" t))
	 (global-dir (make-temp-file "hub-context-filter-reply-global-" t))
	 (org-directory global-dir)
	 (source (expand-file-name "article.org" dir))
	 (sidecar (expand-file-name "article.comments.org" dir))
	 (people (hub/confluence-people-local-file dir))
	 (panel (generate-new-buffer " *org context filter reply wrapper test*")))
    (unwind-protect
	(progn
	  (with-temp-file source
	    (insert "#+TITLE: Article\n\nBody\n"))
	  (with-temp-file people
	    (insert "* Hubert\n:PROPERTIES:\n:HUB_CONFLUENCE_ACCOUNT_ID: acct-me\n:HUB_PERSON_DISPLAY_NAME: Hubert\n:HUB_PERSON_ME: t\n:END:\n"))
	  (with-temp-file sidecar
	    (insert "#+source: article.org\n#+todo: OPEN TODO | RESOLVED\n\n")
	    (insert "* OPEN Page\n:PROPERTIES:\n:HUB_COMMENT_ID: page-1\n:HUB_COMMENT_SYNC_KIND: footer\n:HUB_COMMENT_REMOTE_ID: f1\n:HUB_COMMENT_REMOTE_AUTHOR_ID: acct-alice\n:HUB_COMMENT_REMOTE_AUTHOR_DISPLAY_NAME: Alice\n:END:\n\nRoot context body.\n")
	    (insert "** Reply · Hubert — Mine\n:PROPERTIES:\n:HUB_COMMENT_ID: reply-me\n:HUB_COMMENT_SYNC_KIND: reply\n:HUB_COMMENT_REMOTE_ID: r-me\n:HUB_COMMENT_REMOTE_AUTHOR_ID: acct-me\n:HUB_COMMENT_REMOTE_AUTHOR_DISPLAY_NAME: Hubert\n:END:\n\nMy reply body.\n")
	    (insert "** Reply · Bob — Other\n:PROPERTIES:\n:HUB_COMMENT_ID: reply-other\n:HUB_COMMENT_SYNC_KIND: reply\n:HUB_COMMENT_REMOTE_ID: r-other\n:HUB_COMMENT_REMOTE_AUTHOR_ID: acct-other\n:HUB_COMMENT_REMOTE_AUTHOR_DISPLAY_NAME: Bob\n:END:\n\nOther reply body.\n"))
	  (with-current-buffer (find-file-noselect source)
	    (org-mode)
	    (setq-local hub/org-context-panel-filter-state
			'(:actionable nil :drafts nil :mine t
				      :show-resolved t :show-missing nil))
	    (hub/org-page-context-render-buffer (current-buffer) panel t))
	  (with-current-buffer panel
	    (should (search-forward "Root context body." nil t))
	    (should (search-forward "My reply body." nil t))
	    (should-not (search-forward "Other reply body." nil t))))
      (when (buffer-live-p panel)
	(kill-buffer panel))
      (when-let* ((buffer (find-buffer-visiting source)))
	(kill-buffer buffer))
      (delete-directory dir t)
      (delete-directory global-dir t))))

(ert-deftest hub/org-context-panel-filters-show-missing-replies-by-default ()
  "Context filters show remote-missing replies by default and zx hides them."
  (hub/org-comments-test--with-file-buffer "article.org" "#+TITLE: Article\n\nBody"
					   (let* ((source-buffer (current-buffer))
						  (sidecar (hub/org-comment-sidecar-path buffer-file-name))
						  (panel (generate-new-buffer " *org context filter missing reply test*")))
					     (unwind-protect
						 (progn
						   (with-temp-file sidecar
						     (insert "#+source: article.org\n#+todo: OPEN TODO | RESOLVED\n\n")
						     (insert "* OPEN Page\n:PROPERTIES:\n:HUB_COMMENT_ID: page-1\n:HUB_COMMENT_SYNC_KIND: footer\n:HUB_COMMENT_REMOTE_ID: f1\n:END:\n\nPage body.\n")
						     (insert "** Reply · Bob — Gone\n:PROPERTIES:\n:HUB_COMMENT_ID: reply-1\n:HUB_COMMENT_SYNC_KIND: reply\n:HUB_COMMENT_REMOTE_ID: r1\n:HUB_COMMENT_REMOTE_STATE: missing\n:END:\n\nMissing reply body.\n"))
						   (hub/org-page-context-render-buffer source-buffer panel t)
						   (with-current-buffer panel
						     (should (search-forward "Page body." nil t))
						     (should (search-forward "Missing reply body." nil t))
						     (hub/org-context-panel-filter-toggle-missing))
						   (hub/org-page-context-render-buffer source-buffer panel t)
						   (with-current-buffer panel
						     (goto-char (point-min))
						     (should-not (search-forward "Missing reply body." nil t))
						     (hub/org-context-panel-filter-toggle-missing))
						   (hub/org-page-context-render-buffer source-buffer panel t)
						   (with-current-buffer panel
						     (goto-char (point-min))
						     (should (search-forward "Missing reply body." nil t))))
					       (when (buffer-live-p panel)
						 (kill-buffer panel))))))

(ert-deftest hub/org-context-panel-filter-keys-use-z-prefix ()
  "Context filters live under the z prefix and zz resets filters."
  (with-temp-buffer
    (hub/org-context-panel-buffer-mode)
    (should (eq (local-key-binding (kbd "zx"))
		#'hub/org-context-panel-filter-toggle-missing))
    (should (eq (local-key-binding (kbd "zz"))
		#'hub/org-context-panel-filter-reset))))

(ert-deftest hub/org-context-panel-filter-header-shows-state-and-counts ()
  "Active non-default filters show compact state and item counts."
  (hub/org-comments-test--with-file-buffer "article.org" "Alpha selected text omega. Beta other text."
					   (let* ((source-buffer (current-buffer))
						  (sidecar (hub/org-comment-sidecar-path buffer-file-name))
						  (panel (generate-new-buffer " *org context filter header test*")))
					     (unwind-protect
						 (progn
						   (with-temp-file sidecar
						     (insert "#+source: article.org\n#+todo: OPEN TODO | RESOLVED\n\n")
						     (insert "* OPEN Draft\n:PROPERTIES:\n:HUB_COMMENT_ID: draft\n:HUB_COMMENT_SYNC_KIND: inline\n:HUB_COMMENT_TARGET: 7 20\n:HUB_COMMENT_TARGET_TEXT: selected text\n:END:\n\nDraft body.\n")
						     (insert "* OPEN Remote\n:PROPERTIES:\n:HUB_COMMENT_ID: remote\n:HUB_COMMENT_SYNC_KIND: inline\n:HUB_COMMENT_REMOTE_ID: i1\n:HUB_COMMENT_TARGET: 33 43\n:HUB_COMMENT_TARGET_TEXT: other text\n:END:\n\nRemote body.\n"))
						   (hub/org-context-panel-render-buffer source-buffer panel)
						   (with-current-buffer panel
						     (should-not header-line-format))
						   (with-current-buffer source-buffer
						     (setq-local hub/org-context-panel-filter-state
								 '(:actionable nil :drafts t :mine nil
									       :show-resolved t :show-missing nil)))
						   (hub/org-context-panel-render-buffer source-buffer panel)
						   (with-current-buffer panel
						     (should (string-match-p "drafts" (format "%s" header-line-format)))
						     (should (string-match-p "showing 1/2" (format "%s" header-line-format))))
						   (with-current-buffer panel
						     (hub/org-context-panel-filter-reset))
						   (hub/org-context-panel-render-buffer source-buffer panel)
						   (with-current-buffer panel
						     (should-not header-line-format)))
					       (when (buffer-live-p panel)
						 (kill-buffer panel))))))

(ert-deftest hub/org-context-panel-help-toggles-bottom-window ()
  "Question mark toggles an aligned help window below the context panel."
  (hub/org-comments-test--with-file-buffer "article.org" "Alpha selected text omega"
					   (let* ((source-buffer (current-buffer))
						  (start (progn
							   (goto-char (point-min))
							   (search-forward "selected")
							   (match-beginning 0)))
						  (end (match-end 0))
						  (record (hub/org-comment-create-record
							   buffer-file-name start end "Needs help." "root-1"
							   "Alice" "2026-06-18T09:00:00+0000"))
						  (sidecar (hub/org-comment-append-to-sidecar record))
						  (panel (generate-new-buffer " *org context help test*")))
					     (unwind-protect
						 (progn
						   (delete-other-windows)
						   (hub/org-context-panel-render-buffer source-buffer panel)
						   (switch-to-buffer panel)
						   (set-window-point (selected-window) 2)
						   (hub/org-context-panel-toggle-help)
						   (should (window-live-p hub/org-context-panel--help-window))
						   (with-current-buffer (window-buffer hub/org-context-panel--help-window)
						     (should (derived-mode-p 'hub/org-context-panel-help-mode))
						     (goto-char (point-min))
						     (should (search-forward "+    reply" nil t))
						     (should (search-forward "o    open remote" nil t))
						     (should-not (search-forward "q    close" nil t))
						     (goto-char (point-min))
						     (search-forward "open remote")
						     (should (eq (get-text-property (match-beginning 0) 'face)
								 'hub/org-context-panel-help-description-face))
						     (search-backward "\no    ")
						     (forward-char 1)
						     (should (eq (get-text-property (point) 'face)
								 'hub/org-context-panel-help-key-face))
						     (should-not (get-text-property (point) 'hub-org-context-panel-item)))
						   (let ((panel-window (get-buffer-window panel t))
							 (help-window hub/org-context-panel--help-window))
						     (select-window help-window)
						     (hub/org-context-panel-help-close)
						     (should-not (window-live-p help-window))
						     (should (eq (selected-window) panel-window))
						     (should (= (window-point panel-window) 2))
						     (with-current-buffer panel
						       (should-not (window-live-p hub/org-context-panel--help-window)))))
					       (delete-other-windows)
					       (when (buffer-live-p panel)
						 (kill-buffer panel))
					       (when (get-file-buffer sidecar)
						 (kill-buffer (get-file-buffer sidecar)))))))

(ert-deftest hub/org-comment-compose-opens-below-invoking-panel ()
  "Compose buffers open below the invoking panel without replacing source text."
  (let* ((source (generate-new-buffer " *compose source*"))
	 (panel (generate-new-buffer " *compose panel*"))
	 (source-window (selected-window))
	 compose-window)
    (unwind-protect
	(progn
	  (switch-to-buffer source)
	  (split-window-right)
	  (other-window 1)
	  (switch-to-buffer panel)
	  (with-current-buffer panel
	    (hub/org-context-panel-buffer-mode)
	    (setq-local hub/org-context-panel-source-buffer source))
	  (setq compose-window
		(with-current-buffer panel
		  (hub/org-comment-compose-open
		   'reply '(:type comment :id "root" :remote-id "r1" :sidecar-file "/tmp/root.comments.org"))))
	  (should (window-live-p compose-window))
	  (should (eq (window-buffer source-window) source))
	  (should (equal (buffer-name (window-buffer compose-window)) "*Org Comment Reply*"))
	  (with-current-buffer (window-buffer compose-window)
	    (should (equal (buffer-string) ""))
	    (should (string-match-p "C-c C-c submit" (format "%s" header-line-format)))))
      (delete-other-windows)
      (dolist (buffer (list source panel (get-buffer "*Org Comment Reply*")))
	(when (buffer-live-p buffer)
	  (kill-buffer buffer))))))

(ert-deftest hub/org-comment-compose-cancel-prompts-before-discarding ()
  "Canceling a modified compose buffer asks before discarding text."
  (let ((prompted nil)
	(buffer (generate-new-buffer " *compose cancel prompt*")))
    (unwind-protect
	(with-current-buffer buffer
	  (org-mode)
	  (hub/org-comment-compose-mode 1)
	  (setq-local hub/org-comment-compose--context '(:operation reply))
	  (insert "unsaved")
	  (cl-letf (((symbol-function 'yes-or-no-p)
		     (lambda (&rest _args)
		       (setq prompted t)
		       nil)))
	    (should-error (hub/org-comment-compose-cancel) :type 'user-error))
	  (should prompted)
	  (should (buffer-live-p buffer)))
      (when (buffer-live-p buffer)
	(with-current-buffer buffer
	  (setq-local hub/org-comment-compose--closing t))
	(kill-buffer buffer)))))

(ert-deftest hub/org-comment-compose-kill-prompts-before-discarding ()
  "Killing a modified compose buffer asks before discarding text."
  (let ((prompted nil)
	(buffer (generate-new-buffer " *compose kill prompt*")))
    (unwind-protect
	(with-current-buffer buffer
	  (org-mode)
	  (hub/org-comment-compose-mode 1)
	  (setq-local hub/org-comment-compose--context '(:operation reply))
	  (add-hook 'kill-buffer-query-functions
		    #'hub/org-comment-compose--confirm-kill nil t)
	  (insert "unsaved")
	  (cl-letf (((symbol-function 'yes-or-no-p)
		     (lambda (&rest _args)
		       (setq prompted t)
		       nil)))
	    (should-not (kill-buffer buffer)))
	  (should prompted)
	  (should (buffer-live-p buffer)))
      (when (buffer-live-p buffer)
	(with-current-buffer buffer
	  (setq-local hub/org-comment-compose--closing t))
	(kill-buffer buffer)))))

(ert-deftest hub/org-comment-compose-close-restores-invoking-panel ()
  "Closing compose deletes its window and restores the edited panel item."
  (let* ((source (generate-new-buffer " *compose restore source*"))
	 (panel (generate-new-buffer " *compose restore panel*"))
	 (source-window (selected-window))
	 panel-window
	 compose-window)
    (unwind-protect
	(progn
	  (switch-to-buffer source)
	  (setq panel-window (split-window-right))
	  (set-window-buffer panel-window panel)
	  (with-current-buffer panel
	    (hub/org-context-panel-buffer-mode)
	    (setq-local hub/org-context-panel-source-buffer source)
	    (let ((inhibit-read-only t)
		  (first-start (point))
		  (first '(:type comment :id "first"))
		  (second '(:type comment :id "second")))
	      (insert "First\n")
	      (add-text-properties first-start (point) `(hub-org-context-panel-item ,first))
	      (let ((second-start (point)))
		(insert "Second\n")
		(add-text-properties second-start (point) `(hub-org-context-panel-item ,second))
		(goto-char first-start))))
	  (select-window panel-window)
	  (setq compose-window
		(with-current-buffer panel
		  (hub/org-comment-compose-open 'edit '(:type comment :id "second"))))
	  (with-current-buffer (window-buffer compose-window)
	    (hub/org-comment-compose--close))
	  (should-not (window-live-p compose-window))
	  (should (eq (selected-window) panel-window))
	  (with-current-buffer panel
	    (should (equal (plist-get (hub/org-context-panel--item-at-point) :id) "second")))
	  (should (eq (window-buffer source-window) source)))
      (delete-other-windows)
      (dolist (buffer (list source panel (get-buffer "*Org Comment Edit*")))
	(when (buffer-live-p buffer)
	  (kill-buffer buffer))))))

(ert-deftest hub/org-context-panel-reply-lines-are-actionable-items ()
  "Reply lines in context panels expose their own comment item."
  (hub/org-comments-test--with-file-buffer "article.org" "#+TITLE: Article\n\nBody"
					   (let* ((source-buffer (current-buffer))
						  (sidecar (hub/org-comment-sidecar-path buffer-file-name))
						  (panel (generate-new-buffer " *org context reply item test*")))
					     (unwind-protect
						 (progn
						   (with-temp-file sidecar
						     (insert "#+source: article.org\n#+todo: OPEN TODO | RESOLVED\n\n")
						     (insert "* OPEN Page\n:PROPERTIES:\n:HUB_COMMENT_ID: page-1\n:HUB_COMMENT_SYNC_KIND: footer\n:HUB_COMMENT_REMOTE_ID: f1\n:END:\n\nPage body.\n")
						     (insert "** Reply · Bob — Draft\n:PROPERTIES:\n:HUB_COMMENT_ID: reply-1\n:HUB_COMMENT_SYNC_KIND: reply\n:HUB_COMMENT_AUTHOR: Bob\n:END:\n\nDraft reply.\n"))
						   (hub/org-page-context-render-buffer source-buffer panel t)
						   (with-current-buffer panel
						     (search-forward "Draft reply")
						     (should (equal (plist-get (hub/org-context-panel--item-at-point) :id)
								    "reply-1"))))
					       (when (buffer-live-p panel)
						 (kill-buffer panel))))))

(ert-deftest hub/org-comment-compose-edit-stamps-local-update ()
  "Editing a remote-linked comment stamps local update metadata and draft badge."
  (hub/org-comments-test--with-file-buffer "article.org" "#+TITLE: Article\n\nBody"
					   (let* ((source-buffer (current-buffer))
						  (sidecar (hub/org-comment-sidecar-path buffer-file-name)))
					     (with-temp-file sidecar
					       (insert "#+source: article.org\n#+todo: OPEN TODO | RESOLVED\n\n")
					       (insert "* OPEN Page\n:PROPERTIES:\n:HUB_COMMENT_ID: page-1\n:HUB_COMMENT_SYNC_KIND: footer\n:HUB_COMMENT_REMOTE_ID: f1\n:END:\n\nOld body.\n"))
					     (cl-letf (((symbol-function 'hub/org-comment-current-created-at)
							(lambda () "2026-06-19T20:00:00+0000")))
					       (hub/org-comment-compose--save-edit
						(list :type 'comment :id "page-1" :sidecar-file sidecar :remote-id "f1")
						"New body."))
					     (with-temp-buffer
					       (insert-file-contents sidecar)
					       (should (search-forward ":HUB_COMMENT_LOCAL_UPDATED_AT: 2026-06-19T20:00:00+0000" nil t)))
					     (let ((comment (car (hub/org-comment-collect-page source-buffer))))
					       (should (equal (plist-get comment :local-updated-at)
							      "2026-06-19T20:00:00+0000"))
					       (should (equal (hub/org-context-panel--comment-sync-badge comment) "✍️"))))))

(ert-deftest hub/org-context-panel-action-keys-avoid-core-s-and-r ()
  "Panel actions leave s and r unbound for core navigation."
  (with-temp-buffer
    (hub/org-context-panel-buffer-mode)
    (should (eq (local-key-binding (kbd "C-c C-c")) #'hub/org-context-panel-push-item))
    (should (eq (local-key-binding (kbd "+")) #'hub/org-context-panel-reply-to-item))
    (should-not (local-key-binding (kbd "s")))
    (should-not (local-key-binding (kbd "r")))))

(ert-deftest hub/org-context-panel-push-item-pushes-reply-from-panel ()
  "Panel push dispatches through the sidecar heading for the item at point."
  (hub/org-comments-test--with-file-buffer "article.org" "#+TITLE: Article\n\nBody"
					   (let* ((source-buffer (current-buffer))
						  (sidecar (hub/org-comment-sidecar-path buffer-file-name))
						  (panel (generate-new-buffer " *org context push item test*"))
						  pushed-id)
					     (unwind-protect
						 (progn
						   (with-temp-file sidecar
						     (insert "#+source: article.org\n#+todo: OPEN TODO | RESOLVED\n\n")
						     (insert "* OPEN Page\n:PROPERTIES:\n:HUB_COMMENT_ID: page-1\n:HUB_COMMENT_SYNC_KIND: footer\n:HUB_COMMENT_REMOTE_ID: f1\n:END:\n\nPage body.\n")
						     (insert "** Reply · Bob — Draft\n:PROPERTIES:\n:HUB_COMMENT_ID: reply-1\n:HUB_COMMENT_SYNC_KIND: reply\n:HUB_COMMENT_AUTHOR: Bob\n:END:\n\nDraft reply.\n"))
						   (hub/org-page-context-render-buffer source-buffer panel t)
						   (with-current-buffer panel
						     (search-forward "Draft reply")
						     (cl-letf (((symbol-function 'hub/confluence-comment-push-current)
								(lambda (&rest _args)
								  (setq pushed-id (org-entry-get nil "HUB_COMMENT_ID")))))
						       (hub/org-context-panel-push-item)))
						   (should (equal pushed-id "reply-1")))
					       (when (buffer-live-p panel)
						 (kill-buffer panel))
					       (when (get-file-buffer sidecar)
						 (kill-buffer (get-file-buffer sidecar)))))))

(ert-deftest hub/org-context-panel-open-remote-item-opens-comment-url ()
  "Pressing open remote in the context panel opens the remote comment."
  (hub/org-comments-test--with-file-buffer "article.org" "#+CONFLUENCE_PAGE_ID: 123\n\nAlpha selected text omega"
					   (let* ((source-buffer (current-buffer))
						  (start (progn
							   (goto-char (point-min))
							   (search-forward "selected")
							   (match-beginning 0)))
						  (end (match-end 0))
						  (record (hub/org-comment-create-record
							   buffer-file-name start end "Remote comment." "root-1"
							   "Alice" "2026-06-18T09:00:00+0000"))
						  (sidecar (hub/org-comment-append-to-sidecar record))
						  (panel (generate-new-buffer " *org context open remote test*"))
						  opened)
					     (unwind-protect
						 (progn
						   (with-temp-buffer
						     (insert-file-contents sidecar)
						     (goto-char (point-min))
						     (search-forward ":HUB_COMMENT_ID: root-1")
						     (forward-line 1)
						     (insert ":HUB_COMMENT_REMOTE_ID: i123\n")
						     (write-region (point-min) (point-max) sidecar nil 'silent))
						   (hub/org-context-panel-render-buffer source-buffer panel)
						   (with-current-buffer panel
						     (goto-char (point-min))
						     (search-forward "Remote comment")
						     (cl-letf (((symbol-function 'hub/confluence-comment-open-current)
								(lambda (page-id comment-id)
								  (setq opened (list page-id comment-id)))))
						       (hub/org-context-panel-open-remote-item)))
						   (should (equal opened '(nil "i123"))))
					       (when (buffer-live-p panel)
						 (kill-buffer panel))))))

(ert-deftest hub/org-context-panel-delete-reply-keeps-page-context-renderer ()
  "Deleting from page context re-renders page context, not the side panel."
  (hub/org-comments-test--with-file-buffer "article.org" "#+TITLE: Article\n\nBody"
					   (let* ((source-buffer (current-buffer))
						  (sidecar (hub/org-comment-sidecar-path buffer-file-name))
						  (panel (generate-new-buffer " *org page context delete reply test*")))
					     (unwind-protect
						 (progn
						   (with-temp-file sidecar
						     (insert "#+source: article.org\n#+todo: OPEN TODO | RESOLVED\n\n")
						     (insert "* OPEN Page root\n:PROPERTIES:\n:HUB_COMMENT_ID: root-1\n:HUB_COMMENT_SYNC_KIND: footer\n:HUB_COMMENT_REMOTE_ID: f1\n:END:\n\nRoot body.\n")
						     (insert "** Reply · Bob — First reply\n:PROPERTIES:\n:HUB_COMMENT_ID: reply-1\n:HUB_COMMENT_SYNC_KIND: reply\n:HUB_COMMENT_AUTHOR: Bob\n:END:\n\nFirst reply body.\n"))
						   (hub/org-page-context-render-buffer source-buffer panel t)
						   (with-current-buffer panel
						     (search-forward "First reply body")
						     (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _args) t)))
						       (hub/org-context-panel-delete-item))
						     (should hub/org-context-panel-page-context-p)
						     (goto-char (point-min))
						     (should (search-forward "👆" nil t))
						     (should (search-forward "Root body." nil t))
						     (should-not (search-forward "First reply body" nil t))))
					       (when (buffer-live-p panel)
						 (kill-buffer panel))
					       (when (get-file-buffer sidecar)
						 (kill-buffer (get-file-buffer sidecar)))))))

(ert-deftest hub/org-context-panel-reply-to-reply-preserves-root ()
  "Replying from a reply item creates a sibling without deleting the root."
  (hub/org-comments-test--with-file-buffer "article.org" "#+TITLE: Article\n\nBody"
					   (let* ((source-buffer (current-buffer))
						  (sidecar (hub/org-comment-sidecar-path buffer-file-name))
						  (panel (generate-new-buffer " *org context reply sibling test*")))
					     (unwind-protect
						 (progn
						   (with-temp-file sidecar
						     (insert "#+source: article.org\n#+todo: OPEN TODO | RESOLVED\n\n")
						     (insert "* OPEN Page root\n:PROPERTIES:\n:HUB_COMMENT_ID: root-1\n:HUB_COMMENT_SYNC_KIND: footer\n:HUB_COMMENT_REMOTE_ID: f1\n:END:\n\nRoot body.\n")
						     (insert "** Reply · Bob — First reply\n:PROPERTIES:\n:HUB_COMMENT_ID: reply-1\n:HUB_COMMENT_SYNC_KIND: reply\n:HUB_COMMENT_AUTHOR: Bob\n:END:\n\nFirst reply body.\n"))
						   (hub/org-page-context-render-buffer source-buffer panel t)
						   (with-current-buffer panel
						     (search-forward "First reply body")
						     (cl-letf (((symbol-function 'hub/org-comment-current-created-at)
								(lambda () "2026-06-19T10:00:00+0000"))
							       ((symbol-function 'hub/org-comment-generate-id)
								(lambda () "reply-2")))
						       (hub/org-context-panel-reply-to-item)
						       (with-current-buffer "*Org Comment Reply*"
							 (goto-char (point-max))
							 (insert "Second reply body.")
							 (hub/org-comment-compose-save-draft))))
						   (with-temp-buffer
						     (insert-file-contents sidecar)
						     (should (search-forward "* OPEN [2 replies]" nil t))
						     (should (search-forward ":HUB_COMMENT_ID: root-1" nil t))
						     (should (search-forward "** Reply" nil t))
						     (should (search-forward ":HUB_COMMENT_ID: reply-1" nil t))
						     (should (search-forward ":HUB_COMMENT_ID: reply-2" nil t))))
					       (when (buffer-live-p panel)
						 (kill-buffer panel))
					       (when (get-file-buffer sidecar)
						 (kill-buffer (get-file-buffer sidecar)))))))

(ert-deftest hub/org-context-panel-reply-to-item-creates-reply ()
  "Pressing reply in the context panel creates a local sidecar reply."
  (hub/org-comments-test--with-file-buffer "article.org" "Alpha selected text omega"
					   (let* ((source-buffer (current-buffer))
						  (start (progn
							   (goto-char (point-min))
							   (search-forward "selected")
							   (match-beginning 0)))
						  (end (match-end 0))
						  (record (hub/org-comment-create-record
							   buffer-file-name start end "Needs reply." "root-1"
							   "Alice" "2026-06-18T09:00:00+0000"))
						  (sidecar (hub/org-comment-append-to-sidecar record))
						  (panel (generate-new-buffer " *org context reply test*")))
					     (unwind-protect
						 (progn
						   (with-temp-buffer
						     (insert-file-contents sidecar)
						     (goto-char (point-min))
						     (search-forward ":HUB_COMMENT_ID: root-1")
						     (forward-line 1)
						     (insert ":HUB_COMMENT_REMOTE_ID: i123\n")
						     (write-region (point-min) (point-max) sidecar nil 'silent))
						   (hub/org-context-panel-render-buffer source-buffer panel)
						   (with-current-buffer panel
						     (goto-char (point-min))
						     (search-forward "Needs reply")
						     (cl-letf (((symbol-function 'hub/org-comment-current-created-at)
								(lambda () "2026-06-18T10:00:00+0000"))
							       ((symbol-function 'hub/org-comment-generate-id)
								(lambda () "local-reply-1")))
						       (hub/org-context-panel-reply-to-item)
						       (with-current-buffer "*Org Comment Reply*"
							 (goto-char (point-max))
							 (insert "Reply body.")
							 (hub/org-comment-compose-save-draft))))
						   (with-temp-buffer
						     (insert-file-contents sidecar)
						     (should (search-forward "** Reply ·" nil t))
						     (should (search-forward ":HUB_COMMENT_ID: local-reply-1" nil t))
						     (should (search-forward ":HUB_COMMENT_REMOTE_PARENT_ID: i123" nil t))))
					       (when (buffer-live-p panel)
						 (kill-buffer panel))))))

(ert-deftest hub/org-page-context-display-splits-source-window ()
  "Page context display splits the source window directly."
  (let ((source-window 'source-window)
	(page-window 'page-window)
	(page-buffer (generate-new-buffer " *page comments display test*"))
	(called nil))
    (unwind-protect
	(cl-letf (((symbol-function 'window-total-height)
		   (lambda (window)
		     (should (eq window source-window))
		     30))
		  ((symbol-function 'split-window)
		   (lambda (window size side)
		     (setq called (list window size side))
		     page-window))
		  ((symbol-function 'set-window-buffer)
		   (lambda (window buffer)
		     (should (eq window page-window))
		     (should (eq buffer page-buffer)))))
	  (should (eq (hub/org-page-context--display-below-source source-window page-buffer)
		      page-window))
	  (should (equal called '(source-window -9 below))))
      (kill-buffer page-buffer))))

(ert-deftest hub/org-context-panel-open-auto-opens-page-context ()
  "Opening context panel auto-opens page context when page comments exist."
  (hub/org-comments-test--with-file-buffer "article.org" "#+TITLE: Article\n\nBody"
					   (let* ((source-buffer (current-buffer))
						  (sidecar (hub/org-comment-sidecar-path buffer-file-name))
						  opened)
					     (with-temp-file sidecar
					       (insert "#+source: article.org\n#+todo: OPEN TODO | RESOLVED\n\n")
					       (insert "* OPEN Page\n:PROPERTIES:\n:HUB_COMMENT_ID: page-1\n:HUB_COMMENT_SYNC_KIND: footer\n:END:\n\nPage body.\n"))
					     (cl-letf (((symbol-function 'display-buffer-in-side-window)
							(lambda (buffer _alist) (display-buffer buffer)))
						       ((symbol-function 'hub/org-context-panel--dock-prose) #'ignore)
						       ((symbol-function 'hub/org-page-context--display-below-source)
							(lambda (_source-window buffer &optional select)
							  (setq opened (list buffer select)))))
					       (hub/org-context-panel-open))
					     (should (buffer-live-p (car opened)))
					     (should-not (cadr opened))
					     (with-current-buffer (car opened)
					       (should (equal source-buffer hub/org-context-panel-source-buffer))
					       (should hub/org-context-panel-page-context-p)
					       (should (search-forward "Page body." nil t))))))

(ert-deftest hub/org-context-panel-revert-refreshes-sidecar-state ()
  "Reverting a context panel rereads sidecar state instead of needing a file."
  (hub/org-comments-test--with-file-buffer "article.org" "#+TITLE: Article\n\nBody"
					   (let* ((source-buffer (current-buffer))
						  (sidecar (hub/org-comment-sidecar-path buffer-file-name))
						  (panel (generate-new-buffer " *org context revert test*")))
					     (unwind-protect
						 (progn
						   (with-temp-file sidecar
						     (insert "#+source: article.org\n#+todo: OPEN TODO | RESOLVED\n\n")
						     (insert "* OPEN Page\n:PROPERTIES:\n:HUB_COMMENT_ID: page-1\n:HUB_COMMENT_SYNC_KIND: footer\n:HUB_COMMENT_REMOTE_ID: f1\n:HUB_COMMENT_SOURCE: confluence\n:END:\n\nPage body.\n")
						     (insert "** Reply · Bob — Draft\n:PROPERTIES:\n:HUB_COMMENT_ID: reply-1\n:HUB_COMMENT_SYNC_KIND: reply\n:HUB_COMMENT_AUTHOR: Bob\n:END:\n\nDraft reply.\n"))
						   (hub/org-page-context-render-buffer source-buffer panel t)
						   (with-current-buffer panel
						     (should (search-forward "↳ ✍️ Bob" nil t)))
						   (with-temp-buffer
						     (insert-file-contents sidecar)
						     (goto-char (point-min))
						     (search-forward ":HUB_COMMENT_ID: reply-1")
						     (forward-line 1)
						     (insert ":HUB_COMMENT_REMOTE_ID: r1\n:HUB_COMMENT_SOURCE: confluence\n")
						     (write-region (point-min) (point-max) sidecar nil 'silent))
						   (with-current-buffer panel
						     (revert-buffer)
						     (goto-char (point-min))
						     (should (search-forward "↳ 🔗 Bob" nil t))))
					       (when (buffer-live-p panel)
						 (kill-buffer panel))))))

(ert-deftest hub/org-page-comments-open-renders-footer-comments ()
  "Page comments open in a bottom page-context panel without touching source text."
  (hub/org-comments-test--with-file-buffer "article.org" "#+TITLE: Article\n\nBody"
					   (let* ((source-buffer (current-buffer))
						  (sidecar (hub/org-comment-sidecar-path buffer-file-name))
						  (opened nil))
					     (with-temp-file sidecar
					       (insert "#+title: Comments for article.org\n")
					       (insert "#+source: article.org\n")
					       (insert "#+todo: OPEN TODO | RESOLVED\n\n")
					       (insert "* OPEN Confluence footer comment f1\n")
					       (insert ":PROPERTIES:\n")
					       (insert ":HUB_COMMENT_ID: remote-confluence-f1\n")
					       (insert ":HUB_COMMENT_AUTHOR: Alice\n")
					       (insert ":HUB_COMMENT_CREATED_AT: 2026-06-15T17:42:00.000Z\n")
					       (insert ":HUB_COMMENT_REMOTE_ID: f1\n")
					       (insert ":HUB_COMMENT_SOURCE: confluence\n")
					       (insert ":HUB_COMMENT_SYNC_KIND: footer\n")
					       (insert ":HUB_COMMENT_BODY_FORMAT: storage\n")
					       (insert ":END:\n\n")
					       (insert "<p>Footer body with enough words to avoid the narrow side-panel overview truncation.</p>\n")
					       (insert "** Reply · Bob — Draft reply\n")
					       (insert ":PROPERTIES:\n")
					       (insert ":HUB_COMMENT_ID: local-reply-1\n")
					       (insert ":HUB_COMMENT_SYNC_KIND: reply\n")
					       (insert ":HUB_COMMENT_AUTHOR: Bob\n")
					       (insert ":HUB_COMMENT_CREATED_AT: 2026-06-15T18:00:00.000Z\n")
					       (insert ":END:\n\n")
					       (insert "Draft reply body.\n"))
					     (cl-letf (((symbol-function 'hub/org-page-context--display-below-source)
							(lambda (_source-window buffer &optional _select)
							  (setq opened buffer))))
					       (hub/org-page-comments-open))
					     (should (equal (buffer-string) "#+TITLE: Article\n\nBody\n"))
					     (with-current-buffer opened
					       (should (derived-mode-p 'hub/org-context-panel-buffer-mode))
					       (should hub/org-context-panel-page-context-p)
					       (should (equal source-buffer hub/org-context-panel-source-buffer))
					       (goto-char (point-min))
					       (should-not (search-forward "PAGE comments for" nil t))
					       (should (search-forward "👆" nil t))
					       (should (search-forward "OPEN" nil t))
					       (should (search-forward "Alice · 2026-06-15 19:42" nil t))
					       (should (search-forward "Footer body with enough words to avoid the narrow side-panel overview truncation." nil t))
					       (should (search-forward "↳ ✍️ Bob · 2026-06-15 20:00" nil t))
					       (should (search-forward "Draft reply body." nil t))
					       (should-not (search-forward "<p>Footer body" nil t)))
					     (when (buffer-live-p opened)
					       (kill-buffer opened)))))

(ert-deftest hub/org-comment-overlays-mode-shows-page-comment-marker ()
  "Comment overlays display a top marker for page comments."
  (hub/org-comments-test--with-file-buffer "article.org" "#+TITLE: Article\n\nBody"
					   (let ((sidecar (hub/org-comment-sidecar-path buffer-file-name)))
					     (with-temp-file sidecar
					       (insert "#+title: Comments for article.org\n")
					       (insert "#+source: article.org\n")
					       (insert "#+todo: OPEN TODO | RESOLVED\n\n")
					       (insert "* OPEN Confluence footer comment f1\n")
					       (insert ":PROPERTIES:\n")
					       (insert ":HUB_COMMENT_ID: remote-confluence-f1\n")
					       (insert ":HUB_COMMENT_REMOTE_ID: f1\n")
					       (insert ":HUB_COMMENT_SOURCE: confluence\n")
					       (insert ":HUB_COMMENT_SYNC_KIND: footer\n")
					       (insert ":HUB_COMMENT_BODY_FORMAT: storage\n")
					       (insert ":END:\n\nFooter\n"))
					     (hub/org-comment-overlays-refresh)
					     (should (overlayp hub/org-context-panel--page-comment-overlay))
					     (should (string-match-p
						      "\\[1 PAGE comment\\]"
						      (overlay-get hub/org-context-panel--page-comment-overlay 'after-string))))))

(ert-deftest hub/org-comment-overlays-mode-persists-overlays-after-panel-close ()
  "Persistent comment overlays survive closing the context panel."
  (hub/org-comments-test--with-file-buffer "article.org" "Alpha selected text omega"
					   (let* ((start (progn
							   (goto-char (point-min))
							   (search-forward "selected")
							   (match-beginning 0)))
						  (end (match-end 0)))
					     (hub/org-comment-append-to-sidecar
					      (hub/org-comment-create-record buffer-file-name start end "Keep visible." "local-visible"))
					     (hub/org-comment-overlays-mode 1)
					     (should (cl-some
						      (lambda (overlay)
							(eq (overlay-get overlay 'face) 'hub/org-context-panel-comment-region-face))
						      (overlays-at start)))
					     (hub/org-context-panel-close)
					     (should (cl-some
						      (lambda (overlay)
							(eq (overlay-get overlay 'face) 'hub/org-context-panel-comment-region-face))
						      (overlays-at start)))
					     (hub/org-comment-overlays-mode -1)
					     (should-not (cl-some
							  (lambda (overlay)
							    (eq (overlay-get overlay 'face) 'hub/org-context-panel-comment-region-face))
							  (overlays-at start))))))

(ert-deftest hub/org-page-comment-create-appends-local-footer-entry ()
  "Creating a page comment writes explicit footer metadata to the sidecar."
  (hub/org-comments-test--with-file-buffer "article.org" "#+TITLE: Article\n\nBody"
					   (let* ((source-file buffer-file-name)
						  (sidecar (hub/org-comment-sidecar-path source-file))
						  (hub/org-comment-author "Alice"))
					     (cl-letf (((symbol-function 'hub/org-comment-current-created-at)
							(lambda () "2026-06-17T10:00:00+0000"))
						       ((symbol-function 'hub/org-comment-generate-id)
							(lambda () "local-page-1")))
					       (hub/org-page-comment-create "Review the whole page."))
					     (should (equal (buffer-file-name) sidecar))
					     (should (looking-at-p (regexp-quote "Review the whole page.")))
					     (with-temp-buffer
					       (insert-file-contents sidecar)
					       (should (search-forward "#+source: article.org" nil t))
					       (should (search-forward "* OPEN Page · Alice · Review the whole page." nil t))
					       (should (search-forward ":HUB_COMMENT_ID: local-page-1" nil t))
					       (should (search-forward ":HUB_COMMENT_AUTHOR: Alice" nil t))
					       (should (search-forward ":HUB_COMMENT_CREATED_AT: 2026-06-17T10:00:00+0000" nil t))
					       (should (search-forward ":HUB_COMMENT_SYNC_KIND: footer" nil t))
					       (should-not (search-forward ":HUB_COMMENT_TARGET:" nil t))
					       (should-not (search-forward ":HUB_COMMENT_TARGET_TEXT:" nil t))
					       (should (search-forward "Review the whole page." nil t)))
					     (with-temp-buffer
					       (insert-file-contents source-file)
					       (should (equal (buffer-string) "#+TITLE: Article\n\nBody\n"))
					       (should-not (search-forward "HUB_COMMENT" nil t))))))


(ert-deftest hub/org-comment-reply-create-adds-child-under-remote-root ()
  "Creating a reply writes a local child heading under a remote-linked root."
  (hub/org-comments-test--with-file-buffer "article.org" "#+TITLE: Article\n\nBody"
					   (let* ((source-file buffer-file-name)
						  (sidecar (hub/org-comment-sidecar-path source-file))
						  (hub/org-comment-author "Alice"))
					     (with-temp-file sidecar
					       (insert "#+source: article.org\n#+todo: OPEN TODO | RESOLVED\n\n")
					       (insert "* OPEN Remote root\n")
					       (insert ":PROPERTIES:\n")
					       (insert ":HUB_COMMENT_ID: root-1\n")
					       (insert ":HUB_COMMENT_REMOTE_ID: i123\n")
					       (insert ":HUB_COMMENT_SOURCE: confluence\n")
					       (insert ":HUB_COMMENT_SYNC_KIND: inline\n")
					       (insert ":END:\n\nRoot body\n"))
					     (find-file sidecar)
					     (org-mode)
					     (goto-char (point-min))
					     (re-search-forward "Remote root")
					     (cl-letf (((symbol-function 'hub/org-comment-current-created-at)
							(lambda () "2026-06-18T10:00:00+0000"))
						       ((symbol-function 'hub/org-comment-generate-id)
							(lambda () "local-reply-1")))
					       (hub/org-comment-reply-create "Reply body."))
					     (should (equal (buffer-file-name) sidecar))
					     (should (looking-at-p (regexp-quote "Reply body.")))
					     (with-temp-buffer
					       (insert-file-contents sidecar)
					       (should (search-forward "* OPEN [1 reply]" nil t))
					       (should (search-forward "** Reply · Alice" nil t))
					       (should-not (search-forward "** OPEN" nil t))
					       (should (search-forward ":HUB_COMMENT_ID: local-reply-1" nil t))
					       (should (search-forward ":HUB_COMMENT_SYNC_KIND: reply" nil t))
					       (should (search-forward ":HUB_COMMENT_REMOTE_PARENT_ID: i123" nil t))
					       (should (search-forward ":HUB_COMMENT_AUTHOR: Alice" nil t))
					       (should (search-forward ":HUB_COMMENT_CREATED_AT: 2026-06-18T10:00:00+0000" nil t))
					       (should (search-forward "Reply body." nil t))))))

(ert-deftest hub/org-comment-reply-create-refuses-local-only-root ()
  "Creating a Confluence reply requires a remote-linked root comment."
  (hub/org-comments-test--with-file-buffer "article.org" "#+TITLE: Article\n\nBody"
					   (let* ((sidecar (hub/org-comment-sidecar-path buffer-file-name)))
					     (with-temp-file sidecar
					       (insert "#+source: article.org\n#+todo: OPEN TODO | RESOLVED\n\n")
					       (insert "* OPEN Local root\n:PROPERTIES:\n:HUB_COMMENT_ID: root-1\n:HUB_COMMENT_SYNC_KIND: inline\n:END:\n\nBody\n"))
					     (find-file sidecar)
					     (org-mode)
					     (goto-char (point-min))
					     (re-search-forward "Local root")
					     (should-error (hub/org-comment-reply-create "Reply") :type 'user-error))))

(provide 'org-comments-test)
;;; org-comments-test.el ends here
