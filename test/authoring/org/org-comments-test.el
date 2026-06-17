;;; org-comments-test.el --- Org sidecar comment tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for region-targeted Org comments stored in colocated sidecar files.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'test-helpers)
(require 'hub-org-comments)
(require 'org/comments)

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
  "RET at the page marker opens the bottom page-comments reader."
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

(ert-deftest hub/org-page-comments-navigation-wraps ()
  "Page comments reader supports ]c/[c navigation."
  (let ((buffer (generate-new-buffer " *page comments navigation test*")))
    (unwind-protect
	(with-current-buffer buffer
	  (hub/org-page-comments-mode)
	  (let ((inhibit-read-only t))
	    (insert "Header\n\n")
	    (let ((first-start (point)))
	      (insert "First\n")
	      (add-text-properties first-start (point) '(hub-org-page-comment (:id "first")))
	      (insert "\n")
	      (let ((second-start (point)))
		(insert "Second\n")
		(add-text-properties second-start (point) '(hub-org-page-comment (:id "second")))
		(goto-char (point-min))
		(hub/org-page-comments-next)
		(should (= (point) first-start))
		(hub/org-page-comments-next)
		(should (= (point) second-start))
		(hub/org-page-comments-next)
		(should (= (point) first-start))
		(hub/org-page-comments-previous)
		(should (= (point) second-start))))))
      (kill-buffer buffer))))

(ert-deftest hub/org-page-comments-display-splits-source-window ()
  "Page comments display splits the source window directly."
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
	  (should (eq (hub/org-page-comments--display-below-source source-window page-buffer)
		      page-window))
	  (should (equal called '(source-window -9 below))))
      (kill-buffer page-buffer))))

(ert-deftest hub/org-page-comments-open-renders-footer-comments ()
  "Page comments open in a bottom reader without touching source text."
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
					       (insert "<p>Footer body</p>\n"))
					     (cl-letf (((symbol-function 'hub/org-page-comments--display-below-source)
							(lambda (_source-window buffer)
							  (setq opened buffer))))
					       (hub/org-page-comments-open))
					     (should (equal (buffer-string) "#+TITLE: Article\n\nBody\n"))
					     (with-current-buffer opened
					       (should (derived-mode-p 'hub/org-page-comments-mode))
					       (should (equal source-buffer hub/org-page-comments-source-buffer))
					       (should (search-forward "PAGE comments for" nil t))
					       (should (search-forward "OPEN · Alice · 2026-06-15 19:42" nil t))
					       (should (search-forward "Footer body" nil t))
					       (should-not (search-forward "<p>Footer body</p>" nil t)))
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

(provide 'org-comments-test)
;;; org-comments-test.el ends here
