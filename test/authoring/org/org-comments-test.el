;;; org-comments-test.el --- Org sidecar comment tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for region-targeted Org comments stored in colocated sidecar files.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'test-helpers)
(require 'org-comments)
(require 'org-comments-context-panel)
(require 'org-comments-panel)
(require 'org/comments)
(require 'org/confluence)

(defmacro hub/org-comments-test--with-file-buffer (name contents &rest body)
  "Visit temp file NAME with CONTENTS, then run BODY."
  (declare (indent 2))
  `(let* ((dir (make-temp-file "org-comments-" t))
	  (file (expand-file-name ,name dir)))
     (unwind-protect
	 (with-current-buffer (find-file-noselect file)
	   (erase-buffer)
	   (insert ,contents)
	   (save-buffer)
	   (org-mode)
	   ,@body)
       (delete-directory dir t))))

(defun hub/org-comments-test--render-package-panel (source-buffer panel-buffer)
  "Render package comments for SOURCE-BUFFER into PANEL-BUFFER."
  (with-current-buffer source-buffer
    (org-comments-context-panel-refresh-source-overlays))
  (with-current-buffer panel-buffer
    (org-comments-panel-mode)
    (setq-local org-context-panel-source-buffer source-buffer)
    (setq-local org-comments-panel-source-buffer source-buffer)
    (setq-local org-comments-current-source-buffer-function
		(lambda () org-comments-panel-source-buffer))
    (setq-local org-comments-panel-refresh-function
		(lambda ()
		  (let ((inhibit-read-only t))
		    (org-comments-context-panel-render-side-panel
		     org-comments-panel-source-buffer nil))))
    (funcall org-comments-panel-refresh-function)))

(ert-deftest org-comments-sidecar-path-preserves-org-extension ()
  "Sidecar paths keep an Org extension for comments."
  (should (string-suffix-p "article.comments.org"
			   (org-comments-sidecar-path "/tmp/article.org")))
  (should (string-suffix-p "article.en.comments.org"
			   (org-comments-sidecar-path "/tmp/article.en.org"))))

(ert-deftest org-comments-append-creates-readable-sidecar-entry ()
  "Appending a comment creates a plain Org sidecar with target metadata."
  (hub/org-comments-test--with-file-buffer "article.org" "Alpha selected text omega"
					   (let* ((start (progn (goto-char (point-min)) (search-forward "selected") (match-beginning 0)))
						  (end (match-end 0))
						  (record (org-comments-create-record
							   buffer-file-name start end "Please clarify." "local-test"
							   "Ada" "2026-06-15T19:42:00+0200"))
						  (sidecar (org-comments-append-to-sidecar record)))
					     (with-temp-buffer
					       (insert-file-contents sidecar)
					       (should (search-forward "#+title: Comments for article.org" nil t))
					       (should (search-forward "#+source: article.org" nil t))
					       (should (search-forward "#+todo: OPEN TODO | RESOLVED" nil t))
					       (should (search-forward "* OPEN Ada · “selected” — Please clarify." nil t))
					       (should (search-forward ":ORG_COMMENTS_ID: local-test" nil t))
					       (should (search-forward ":ORG_COMMENTS_AUTHOR: Ada" nil t))
					       (should (search-forward ":ORG_COMMENTS_CREATED_AT: 2026-06-15T19:42:00+0200" nil t))
					       (should (search-forward ":ORG_COMMENTS_TARGET:" nil t))
					       (should (search-forward ":ORG_COMMENTS_TARGET_LINES:" nil t))
					       (should-not (search-forward ":ORG_COMMENTS_SOURCE_FILE:" nil t))
					       (should-not (search-forward ":ORG_COMMENTS_STATUS:" nil t))
					       (should-not (search-forward ":ORG_COMMENTS_TARGET_START:" nil t))
					       (should (search-forward ":ORG_COMMENTS_TARGET_TEXT: selected" nil t))
					       (should-not (search-forward ":ORG_COMMENTS_TARGET_HASH:" nil t))
					       (should (search-forward "Please clarify." nil t))))))

(ert-deftest org-comments-current-author-prefers-org-metadata ()
  "Local comment authors prefer Org metadata before Emacs user identity."
  (hub/org-comments-test--with-file-buffer "article.org" "#+AUTHOR: Document Author\n#+EMAIL: doc@example.com\n\nAlpha selected"
					   (let ((org-comments-author nil)
						 (user-full-name "Emacs User"))
					     (should (equal (org-comments-current-author) "Document Author")))))

(ert-deftest org-comments-heading-title-decodes-html-entities ()
  "Readable headings decode Confluence storage HTML entities."
  (let ((org-comments-heading-body-preview-length 60))
    (should (equal
	     "Page · Alice · It’s useful & clear"
	     (org-comments-heading-title
	      '(:author "Alice" :sync-kind "footer" :body "<p>It&rsquo;s useful &amp; clear</p>"))))))

(ert-deftest org-comments-anchor-imported-inline-comments-anchors-unique-match ()
  "Imported Confluence inline comments anchor when target text has one match."
  (hub/org-comments-test--with-file-buffer "article.org" "Alpha imported target omega"
					   (let ((sidecar (org-comments-sidecar-path buffer-file-name)))
					     (with-temp-file sidecar
					       (insert "#+title: Comments for article.org\n")
					       (insert "#+source: article.org\n")
					       (insert "#+todo: OPEN TODO | RESOLVED\n\n")
					       (insert "* OPEN Remote inline\n")
					       (insert ":PROPERTIES:\n")
					       (insert ":ORG_COMMENTS_ID: remote-confluence-i1\n")
					       (insert ":ORG_COMMENTS_REMOTE_ID: i1\n")
					       (insert ":ORG_COMMENTS_SOURCE: confluence\n")
					       (insert ":ORG_COMMENTS_SYNC_KIND: inline\n")
					       (insert ":ORG_COMMENTS_TARGET_TEXT: imported target\n")
					       (insert ":END:\n\n")
					       (insert "Body\n"))
					     (should (equal '(:anchored 1 :missing 0 :ambiguous 0)
							    (org-comments-anchor-imported-inline-comments (current-buffer))))
					     (with-temp-buffer
					       (insert-file-contents sidecar)
					       (should (search-forward ":ORG_COMMENTS_TARGET:" nil t))
					       (should (search-forward ":ORG_COMMENTS_TARGET_LINES:" nil t))
					       (should-not (search-forward ":ORG_COMMENTS_TARGET_HASH:" nil t))
					       (should-not (search-forward ":ORG_COMMENTS_ANCHOR_STATE:" nil t))))))

(ert-deftest org-comments-anchor-imported-inline-comments-shrinks-flattened-target ()
  "Imported Confluence inline comments anchor shrunk windows when fuzzy has none."
  (hub/org-comments-test--with-file-buffer "article.org" "| Whole Face domain | Strategically Network-shaped but not mature enough yet |\n| Face fraud intelligence / biometric attack response | Good candidate later |"
					   (let ((sidecar (org-comments-sidecar-path buffer-file-name)))
					     (with-temp-file sidecar
					       (insert "#+title: Comments for article.org\n")
					       (insert "#+source: article.org\n")
					       (insert "#+todo: OPEN TODO | RESOLVED\n\n")
					       (insert "* OPEN Remote inline\n")
					       (insert ":PROPERTIES:\n")
					       (insert ":ORG_COMMENTS_ID: remote-confluence-i1\n")
					       (insert ":ORG_COMMENTS_SOURCE: confluence\n")
					       (insert ":ORG_COMMENTS_SYNC_KIND: inline\n")
					       (insert ":ORG_COMMENTS_TARGET_TEXT: Whole Face domainStrategically Network-shaped but not mature enough yetFace fraud intelligence / biometric attack responseGood candidate later\n")
					       (insert ":END:\n\n")
					       (insert "Body\n"))
					     (cl-letf (((symbol-function 'org-comments--fuzzy-anchor-candidates)
							(lambda (&rest _args) nil)))
					       (should (equal '(:anchored 1 :missing 0 :ambiguous 0)
							      (org-comments-anchor-imported-inline-comments (current-buffer)))))
					     (with-temp-buffer
					       (insert-file-contents sidecar)
					       (should (search-forward ":ORG_COMMENTS_TARGET_TEXT: Strategically Network-shaped but not mature enough yet" nil t))
					       (should-not (search-forward ":ORG_COMMENTS_ANCHOR_STATE:" nil t))))))

(ert-deftest org-comments-anchor-imported-inline-comments-records-ambiguous ()
  "Imported Confluence inline comments record ambiguity for multiple matches."
  (hub/org-comments-test--with-file-buffer "article.org" "Alpha repeated beta repeated omega"
					   (let ((sidecar (org-comments-sidecar-path buffer-file-name)))
					     (with-temp-file sidecar
					       (insert "#+title: Comments for article.org\n")
					       (insert "#+source: article.org\n")
					       (insert "#+todo: OPEN TODO | RESOLVED\n\n")
					       (insert "* OPEN Remote inline\n")
					       (insert ":PROPERTIES:\n")
					       (insert ":ORG_COMMENTS_ID: remote-confluence-i1\n")
					       (insert ":ORG_COMMENTS_SOURCE: confluence\n")
					       (insert ":ORG_COMMENTS_SYNC_KIND: inline\n")
					       (insert ":ORG_COMMENTS_TARGET_TEXT: repeated\n")
					       (insert ":END:\n\n")
					       (insert "Body\n"))
					     (should (equal '(:anchored 0 :missing 0 :ambiguous 1)
							    (org-comments-anchor-imported-inline-comments (current-buffer))))
					     (with-temp-buffer
					       (insert-file-contents sidecar)
					       (should (search-forward ":ORG_COMMENTS_ANCHOR_STATE: ambiguous" nil t))
					       (should (search-forward ":ORG_COMMENTS_ANCHOR_MATCH_COUNT: 2" nil t))))))

(ert-deftest org-comments-triage-imported-inline-comments-anchors-completion-choice ()
  "Batch triage anchors the completion-selected occurrence immediately."
  (hub/org-comments-test--with-file-buffer "article.org" "First repeated target.\n\nSecond repeated target."
					   (let ((sidecar (org-comments-sidecar-path buffer-file-name)))
					     (with-temp-file sidecar
					       (insert "#+title: Comments for article.org\n")
					       (insert "#+source: article.org\n")
					       (insert "#+todo: OPEN TODO | RESOLVED\n\n")
					       (insert "* OPEN Remote inline\n")
					       (insert ":PROPERTIES:\n")
					       (insert ":ORG_COMMENTS_ID: remote-confluence-i1\n")
					       (insert ":ORG_COMMENTS_SOURCE: confluence\n")
					       (insert ":ORG_COMMENTS_SYNC_KIND: inline\n")
					       (insert ":ORG_COMMENTS_TARGET_TEXT: repeated target\n")
					       (insert ":ORG_COMMENTS_ANCHOR_STATE: ambiguous\n")
					       (insert ":ORG_COMMENTS_ANCHOR_MATCH_COUNT: 2\n")
					       (insert ":END:\n\n")
					       (insert "Please place me.\n"))
					     (cl-letf (((symbol-function 'completing-read)
							(lambda (_prompt candidates &rest _args)
							  (cl-find-if (lambda (candidate)
									(string-match-p "L3" candidate))
								      candidates))))
					       (should (equal '(:selected 1 :skipped 0)
							      (org-comments--triage-imported-inline-comments
							       (current-buffer) sidecar))))
					     (with-temp-buffer
					       (insert-file-contents sidecar)
					       (should (search-forward ":ORG_COMMENTS_TARGET:" nil t))
					       (should-not (search-forward ":ORG_COMMENTS_ANCHOR_STATE:" nil t))))))

(ert-deftest org-comments-compact-sidecar-metadata-removes-obsolete-properties ()
  "Compact metadata removes derivable properties without touching local author metadata."
  (let* ((dir (make-temp-file "org-comments-" t))
	 (sidecar (expand-file-name "article.comments.org" dir)))
    (unwind-protect
	(progn
	  (with-temp-file sidecar
	    (insert "#+title: Comments for article.org\n#+source: article.org\n#+todo: OPEN TODO | RESOLVED\n\n")
	    (insert "* OPEN Remote\n:PROPERTIES:\n:ORG_COMMENTS_ID: remote-confluence-r1\n:ORG_COMMENTS_REMOTE_ID: r1\n:ORG_COMMENTS_AUTHOR: Alice\n:ORG_COMMENTS_CREATED_AT: 2026-01-01\n:ORG_COMMENTS_TARGET_HASH: sha256:abc\n:ORG_COMMENTS_PARENT_ID: remote-confluence-p1\n:ORG_COMMENTS_BODY_FORMAT: storage\n:ORG_COMMENTS_REMOTE_TARGET_JSON: {}\n:ORG_COMMENTS_REMOTE_STATE: present\n:END:\n\nBody\n")
	    (insert "* OPEN Local\n:PROPERTIES:\n:ORG_COMMENTS_ID: local-1\n:ORG_COMMENTS_AUTHOR: Local Author\n:ORG_COMMENTS_CREATED_AT: 2026-02-02\n:ORG_COMMENTS_TARGET_HASH: sha256:def\n:END:\n\nBody\n"))
	  (should (= 8 (org-comments-compact-sidecar-metadata sidecar)))
	  (with-temp-buffer
	    (insert-file-contents sidecar)
	    (should-not (search-forward ":ORG_COMMENTS_TARGET_HASH:" nil t))
	    (should-not (search-forward ":ORG_COMMENTS_PARENT_ID:" nil t))
	    (should-not (search-forward ":ORG_COMMENTS_BODY_FORMAT: storage" nil t))
	    (should-not (search-forward ":ORG_COMMENTS_REMOTE_TARGET_JSON:" nil t))
	    (should-not (search-forward ":ORG_COMMENTS_REMOTE_STATE: present" nil t))
	    (should-not (search-forward ":ORG_COMMENTS_AUTHOR: Alice" nil t))
	    (should (search-forward ":ORG_COMMENTS_AUTHOR: Local Author" nil t))
	    (should (search-forward ":ORG_COMMENTS_CREATED_AT: 2026-02-02" nil t))))
      (delete-directory dir t))))

(ert-deftest org-comments-refresh-sidecar-headings-renames-existing-entry ()
  "Refreshing sidecar headings recomputes readable titles from metadata and body."
  (let* ((dir (make-temp-file "org-comments-" t))
	 (sidecar (expand-file-name "article.comments.org" dir)))
    (unwind-protect
	(progn
	  (with-temp-file sidecar
	    (insert "#+title: Comments for article.org\n")
	    (insert "#+source: article.org\n")
	    (insert "#+todo: OPEN TODO | RESOLVED\n\n")
	    (insert "* TODO Old machine title\n")
	    (insert ":PROPERTIES:\n")
	    (insert ":ORG_COMMENTS_ID: remote-confluence-1\n")
	    (insert ":ORG_COMMENTS_AUTHOR: Alice Example\n")
	    (insert ":ORG_COMMENTS_SOURCE: confluence\n")
	    (insert ":ORG_COMMENTS_SYNC_KIND: inline\n")
	    (insert ":ORG_COMMENTS_TARGET_TEXT: selected target text\n")
	    (insert ":END:\n\n")
	    (insert "<p>Please clarify this part.</p>\n"))
	  (should (= 1 (org-comments-refresh-sidecar-headings sidecar)))
	  (with-temp-buffer
	    (insert-file-contents sidecar)
	    (should (search-forward
		     "* TODO Alice Example · “selected target text” — Please clarify this part."
		     nil t))
	    (should (search-forward ":ORG_COMMENTS_ID: remote-confluence-1" nil t))
	    (should (search-forward "<p>Please clarify this part.</p>" nil t))))
      (delete-directory dir t))))

(ert-deftest org-comments-collects-author-and-created-at ()
  "Comment collection includes author and creation metadata."
  (hub/org-comments-test--with-file-buffer "article.org" "Alpha selected text omega"
					   (let* ((start (progn (goto-char (point-min)) (search-forward "selected") (match-beginning 0)))
						  (end (match-end 0))
						  (record (org-comments-create-record
							   buffer-file-name start end "Body." "local-meta"
							   "Ada" "2026-06-15T19:42:00+0200")))
					     (org-comments-append-to-sidecar record)
					     (let ((comment (car (org-comments-collect (current-buffer)))))
					       (should (equal (plist-get comment :author) "Ada"))
					       (should (equal (plist-get comment :created-at)
							      "2026-06-15T19:42:00+0200"))))))

(ert-deftest org-comments-collects-prefers-remote-created-at ()
  "Comment collection prefers remote timestamps for display when available."
  (hub/org-comments-test--with-file-buffer "article.org" "Alpha selected text omega"
					   (let* ((start (progn
							   (goto-char (point-min))
							   (search-forward "selected")
							   (match-beginning 0)))
						  (end (match-end 0))
						  (sidecar (org-comments-append-to-sidecar
							    (org-comments-create-record
							     buffer-file-name start end "Body." "local-remote-time"
							     "Ada" "2026-06-15T19:42:00+0200"))))
					     (with-temp-buffer
					       (insert-file-contents sidecar)
					       (goto-char (point-min))
					       (search-forward ":ORG_COMMENTS_CREATED_AT:")
					       (forward-line 1)
					       (insert ":ORG_COMMENTS_REMOTE_CREATED_AT: 2026-06-16T10:00:00.000Z\n")
					       (write-region (point-min) (point-max) sidecar nil 'silent))
					     (let ((comment (car (org-comments-collect (current-buffer)))))
					       (should (equal (plist-get comment :created-at)
							      "2026-06-16T10:00:00.000Z"))))))

(ert-deftest org-comments-collects-stale-comments-when-requested ()
  "Comment collection can include unanchored records whose targets drifted."
  (hub/org-comments-test--with-file-buffer "article.org" "Alpha selected text omega"
					   (let* ((start (progn (goto-char (point-min)) (search-forward "selected") (match-beginning 0)))
						  (end (match-end 0))
						  (record (org-comments-create-record buffer-file-name start end "Drifted." "local-stale")))
					     (org-comments-append-to-sidecar record)
					     (save-excursion
					       (goto-char start)
					       (delete-char 1)
					       (insert "S"))
					     (should-not (org-comments-collect (current-buffer)))
					     (let ((comments (org-comments-collect (current-buffer) t)))
					       (should (= 1 (length comments)))
					       (should (eq 'stale (plist-get (car comments) :anchor-state)))
					       (should-not (plist-get (car comments) :jump-pos))
					       (should (equal "local-stale" (plist-get (car comments) :id)))))))

(ert-deftest org-comments-collects-only-matching-offset-comments ()
  "Comment collection ignores sidecar records whose target offsets drifted."
  (hub/org-comments-test--with-file-buffer "article.org" "Alpha selected text omega"
					   (let* ((start (progn (goto-char (point-min)) (search-forward "selected") (match-beginning 0)))
						  (end (match-end 0))
						  (record (org-comments-create-record buffer-file-name start end "Good target." "local-good")))
					     (org-comments-append-to-sidecar record)
					     (let ((comments (org-comments-collect (current-buffer))))
					       (should (= 1 (length comments)))
					       (should (equal "local-good" (plist-get (car comments) :id)))
					       (should (= start (plist-get (car comments) :jump-pos))))
					     (save-excursion
					       (goto-char start)
					       (delete-char 1)
					       (insert "S"))
					     (should-not (org-comments-collect (current-buffer))))))

(ert-deftest org-comments-create-command-writes-sidecar-and-opens-body ()
  "The interactive command writes a sidecar comment and opens its body."
  (hub/org-comments-test--with-file-buffer "article.org" "Alpha selected text omega"
					   (let ((start (progn (goto-char (point-min)) (search-forward "selected") (match-beginning 0)))
						 (end (match-end 0)))
					     (let ((sidecar (org-comments-sidecar-path buffer-file-name)))
					       (cl-letf (((symbol-function 'org-comments-generate-id)
							  (lambda () "local-command")))
						 (hub/org-comment-create start end "Please revise."))
					       (should (equal sidecar buffer-file-name))
					       (should (looking-at-p "Please revise\."))
					       (with-temp-buffer
						 (insert-file-contents sidecar)
						 (should (search-forward ":ORG_COMMENTS_ID: local-command" nil t))
						 (should (search-forward "Please revise." nil t)))))))

(ert-deftest org-comments-reanchor-uses-only-stale-comment-without-prompt ()
  "Reanchoring one stale comment does not require a picker."
  (hub/org-comments-test--with-file-buffer "article.org" "Alpha selected text omega"
					   (let* ((old-start (progn
							       (goto-char (point-min))
							       (search-forward "selected")
							       (match-beginning 0)))
						  (old-end (match-end 0)))
					     (org-comments-append-to-sidecar
					      (org-comments-create-record buffer-file-name old-start old-end "Repair." "local-single"))
					     (save-excursion
					       (goto-char old-start)
					       (delete-char 1)
					       (insert "S"))
					     (let ((new-start (progn
								(goto-char (point-min))
								(search-forward "text")
								(match-beginning 0)))
						   (new-end (match-end 0)))
					       (cl-letf ((org-comments-ui-open-function #'ignore)
							 ((symbol-function 'completing-read)
							  (lambda (&rest _)
							    (error "Reanchor should not prompt while hardening visual commands"))))
						 (hub/org-comment-reanchor new-start new-end))
					       (should (equal "text" (plist-get (car (org-comments-collect (current-buffer)))
										:target-text)))))))

(ert-deftest org-comments-reanchor-prompts-for-multiple-stale-comments ()
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
					     (org-comments-append-to-sidecar
					      (org-comments-create-record buffer-file-name selected-start selected-end "First." "local-first"))
					     (org-comments-append-to-sidecar
					      (org-comments-create-record buffer-file-name omega-start omega-end "Second." "local-second"))
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
					       (cl-letf ((org-comments-ui-open-function #'ignore)
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
					       (let ((comments (org-comments-collect (current-buffer) t)))
						 (should (equal "tail" (plist-get
									(cl-find "local-second" comments
										 :key (lambda (comment)
											(plist-get comment :id))
										 :test #'equal)
									:target-text))))))))

(ert-deftest org-comments-reanchor-updates-stale-comment-target ()
  "Reanchoring updates stale comment metadata to the selected source region."
  (hub/org-comments-test--with-file-buffer "article.org" "Alpha selected text omega"
					   (let* ((old-start (progn
							       (goto-char (point-min))
							       (search-forward "selected")
							       (match-beginning 0)))
						  (old-end (match-end 0))
						  (sidecar (org-comments-append-to-sidecar
							    (org-comments-create-record buffer-file-name old-start old-end "Repair." "local-reanchor"))))
					     (save-excursion
					       (goto-char old-start)
					       (delete-char 1)
					       (insert "S"))
					     (let* ((new-start (progn
								 (goto-char (point-min))
								 (search-forward "text")
								 (match-beginning 0)))
						    (new-end (match-end 0))
						    (stale (car (org-comments-collect (current-buffer) t))))
					       (cl-letf ((org-comments-ui-open-function #'ignore))
						 (hub/org-comment-reanchor new-start new-end stale))
					       (should (= new-start (point)))
					       (let ((comments (org-comments-collect (current-buffer))))
						 (should (= 1 (length comments)))
						 (should (equal "local-reanchor" (plist-get (car comments) :id)))
						 (should (= new-start (plist-get (car comments) :target-start)))
						 (should (equal "text" (plist-get (car comments) :target-text))))
					       (with-temp-buffer
						 (insert-file-contents sidecar)
						 (should (search-forward ":ORG_COMMENTS_TARGET_TEXT: text" nil t))
						 (should-not (search-forward ":ORG_COMMENTS_TARGET_HASH:" nil t)))))))

(ert-deftest org-comments-edit-jumps-to-body-and-narrows-subtree ()
  "Editing an active comment opens its sidecar body narrowed to the subtree."
  (hub/org-comments-test--with-file-buffer "article.org" "Alpha selected text omega"
					   (let* ((start (progn
							   (goto-char (point-min))
							   (search-forward "selected")
							   (match-beginning 0)))
						  (end (match-end 0))
						  (sidecar (org-comments-append-to-sidecar
							    (org-comments-create-record buffer-file-name start end "Edit this." "local-edit"))))
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

(ert-deftest org-comments-edit-empty-body-does-not-stay-on-heading ()
  "Editing an empty sidecar comment lands in the body area, not on the title."
  (hub/org-comments-test--with-file-buffer "article.org" "Alpha selected text omega"
					   (let* ((start (progn
							   (goto-char (point-min))
							   (search-forward "selected")
							   (match-beginning 0)))
						  (end (match-end 0))
						  (sidecar (org-comments-append-to-sidecar
							    (org-comments-create-record buffer-file-name start end "" "local-empty"))))
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

(ert-deftest org-comments-jump-to-sidecar-opens-heading ()
  "Jumping to sidecar moves to the active comment heading."
  (hub/org-comments-test--with-file-buffer "article.org" "Alpha selected text omega"
					   (let* ((start (progn
							   (goto-char (point-min))
							   (search-forward "selected")
							   (match-beginning 0)))
						  (end (match-end 0))
						  (sidecar (org-comments-append-to-sidecar
							    (org-comments-create-record buffer-file-name start end "Jump." "local-jump"))))
					     (unwind-protect
						 (progn
						   (goto-char start)
						   (hub/org-comment-jump-to-sidecar)
						   (should (equal sidecar buffer-file-name))
						   (should (looking-at-p "\\* OPEN .*“selected”")))
					       (when (get-file-buffer sidecar)
						 (kill-buffer (get-file-buffer sidecar)))))))

(ert-deftest org-comments-open-sidecar-folds-property-drawers ()
  "Opening the sidecar folds verbose comment property drawers by default."
  (let* ((dir (make-temp-file "org-comments-" t))
	 (source (expand-file-name "article.org" dir))
	 (sidecar (expand-file-name "article.comments.org" dir)))
    (unwind-protect
	(progn
	  (with-temp-file source (insert "Body\n"))
	  (with-temp-file sidecar
	    (insert "#+title: Comments for article.org\n#+source: article.org\n#+todo: OPEN TODO | RESOLVED\n\n")
	    (insert "* OPEN Comment\n:PROPERTIES:\n:ORG_COMMENTS_ID: local-fold\n:END:\n\nBody\n"))
	  (with-current-buffer (find-file-noselect source)
	    (hub/org-comment-open-sidecar))
	  (with-current-buffer (find-buffer-visiting sidecar)
	    (goto-char (point-min))
	    (search-forward ":ORG_COMMENTS_ID:")
	    (should (invisible-p (point)))))
      (when (get-file-buffer source)
	(kill-buffer (get-file-buffer source)))
      (when (get-file-buffer sidecar)
	(kill-buffer (get-file-buffer sidecar)))
      (delete-directory dir t))))

(ert-deftest org-comments-open-sidecar-opens-existing-file ()
  "Opening the sidecar visits the comments file when it exists."
  (hub/org-comments-test--with-file-buffer "article.org" "Alpha selected text omega"
					   (let* ((start (progn
							   (goto-char (point-min))
							   (search-forward "selected")
							   (match-beginning 0)))
						  (end (match-end 0))
						  (sidecar (org-comments-append-to-sidecar
							    (org-comments-create-record buffer-file-name start end "Open." "local-open"))))
					     (unwind-protect
						 (progn
						   (hub/org-comment-open-sidecar)
						   (should (equal sidecar buffer-file-name)))
					       (when (get-file-buffer sidecar)
						 (kill-buffer (get-file-buffer sidecar)))))))

(ert-deftest org-confluence-comments-sidecar-submit-key-is-local ()
  "Bind C-c C-c to Confluence push only in comments sidecar buffers."
  (hub/org-comments-test--with-file-buffer "article.org" "#+TITLE: Article\n\nBody"
					   (let ((source-buffer (current-buffer))
						 (sidecar (org-comments-sidecar-path buffer-file-name)))
					     (should-not (eq (local-key-binding (kbd "C-c C-c"))
							     #'org-confluence-comments-push-current))
					     (with-temp-file sidecar
					       (insert "#+source: article.org\n#+todo: OPEN TODO | RESOLVED\n\n"))
					     (with-current-buffer (find-file-noselect sidecar)
					       (org-mode)
					       (should (eq (local-key-binding (kbd "C-c C-c"))
							   #'org-confluence-comments-push-current)))
					     (when (get-file-buffer sidecar)
					       (kill-buffer (get-file-buffer sidecar)))
					     (should (buffer-live-p source-buffer)))))

(ert-deftest org-comments-open-sidecar-messages-when-absent ()
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

(ert-deftest org-comments-delete-removes-active-source-comment ()
  "Deleting from the source removes the active sidecar comment after confirmation."
  (hub/org-comments-test--with-file-buffer "article.org" "Alpha selected text omega"
					   (let* ((start (progn
							   (goto-char (point-min))
							   (search-forward "selected")
							   (match-beginning 0)))
						  (end (match-end 0))
						  (sidecar (org-comments-append-to-sidecar
							    (org-comments-create-record buffer-file-name start end "Delete." "local-delete"))))
					     (goto-char start)
					     (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _args) t))
						       (org-comments-ui-open-function #'ignore))
					       (hub/org-comment-delete))
					     (with-temp-buffer
					       (insert-file-contents sidecar)
					       (should-not (search-forward "local-delete" nil t))))))

(ert-deftest org-comments-delete-removes-sidecar-heading ()
  "Deleting from the sidecar removes the current comment subtree."
  (hub/org-comments-test--with-file-buffer "article.org" "Alpha selected text omega"
					   (let* ((start (progn
							   (goto-char (point-min))
							   (search-forward "selected")
							   (match-beginning 0)))
						  (end (match-end 0))
						  (sidecar (org-comments-append-to-sidecar
							    (org-comments-create-record buffer-file-name start end "Delete sidecar." "local-sidecar-delete"))))
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

(ert-deftest org-comments-status-commands-update-active-sidecar-heading ()
  "Comment status commands update the active sidecar heading TODO keyword."
  (hub/org-comments-test--with-file-buffer "article.org" "Alpha selected text omega"
					   (let* ((start (progn
							   (goto-char (point-min))
							   (search-forward "selected")
							   (match-beginning 0)))
						  (end (match-end 0))
						  (sidecar (org-comments-append-to-sidecar
							    (org-comments-create-record buffer-file-name start end "Act." "local-status"))))
					     (cl-letf ((org-comments-ui-open-function #'ignore))
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

(ert-deftest org-comments-navigation-wraps-and-opens-panel ()
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
					     (org-comments-append-to-sidecar
					      (org-comments-create-record buffer-file-name first-start first-end "First." "local-first"))
					     (org-comments-append-to-sidecar
					      (org-comments-create-record buffer-file-name second-start second-end "Second." "local-second"))
					     (let ((org-comments-ui-open-function
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

(ert-deftest org-comments-navigation-stops-at-page-marker ()
  "Comment navigation includes a page-comments stop at the metadata boundary."
  (hub/org-comments-test--with-file-buffer "article.org" "#+TITLE: Article\n\nAlpha selected text omega"
					   (let* ((start (progn
							   (goto-char (point-min))
							   (search-forward "selected")
							   (match-beginning 0)))
						  (end (match-end 0))
						  (sidecar (org-comments-append-to-sidecar
							    (org-comments-create-record buffer-file-name start end "Anchored." "local-anchored"))))
					     (with-temp-buffer
					       (insert-file-contents sidecar)
					       (goto-char (point-max))
					       (insert "\n* OPEN Confluence footer comment f1\n")
					       (insert ":PROPERTIES:\n")
					       (insert ":ORG_COMMENTS_ID: remote-confluence-f1\n")
					       (insert ":ORG_COMMENTS_REMOTE_ID: f1\n")
					       (insert ":ORG_COMMENTS_SOURCE: confluence\n")
					       (insert ":ORG_COMMENTS_SYNC_KIND: footer\n")
					       (insert ":ORG_COMMENTS_BODY_FORMAT: storage\n")
					       (insert ":END:\n\nFooter\n")
					       (write-region (point-min) (point-max) sidecar nil 'silent))
					     (org-comments-overlays-refresh)
					     (goto-char (point-min))
					     (hub/org-comment-next)
					     (should (= (point) (org-comments-context-panel-page-marker-position)))
					     (hub/org-comment-next)
					     (should (= (point) start))
					     (hub/org-comment-previous)
					     (should (= (point) (org-comments-context-panel-page-marker-position))))))

(ert-deftest org-context-panel-ret-at-page-marker-opens-page-comments ()
  "RET at the page marker opens the bottom page-context panel."
  (hub/org-comments-test--with-file-buffer "article.org" "#+TITLE: Article\n\nBody"
					   (let ((sidecar (org-comments-sidecar-path buffer-file-name))
						 (opened nil))
					     (with-temp-file sidecar
					       (insert "#+title: Comments for article.org\n")
					       (insert "#+todo: OPEN TODO | RESOLVED\n\n")
					       (insert "* OPEN Confluence footer comment f1\n")
					       (insert ":PROPERTIES:\n")
					       (insert ":ORG_COMMENTS_ID: remote-confluence-f1\n")
					       (insert ":ORG_COMMENTS_REMOTE_ID: f1\n")
					       (insert ":ORG_COMMENTS_SOURCE: confluence\n")
					       (insert ":ORG_COMMENTS_SYNC_KIND: footer\n")
					       (insert ":ORG_COMMENTS_BODY_FORMAT: storage\n")
					       (insert ":END:\n\nFooter\n"))
					     (org-comments-overlays-refresh)
					     (goto-char (org-comments-context-panel-page-marker-position))
					     (cl-letf (((symbol-function 'hub/org-page-comments-open)
							(lambda () (setq opened t))))
					       (hub/org-comments-source-ret-dwim))
					     (should opened))))

(ert-deftest org-context-panel-page-view-navigation-wraps ()
  "Page context uses context-panel ]c/[c navigation."
  (let ((buffer (generate-new-buffer " *page context navigation test*")))
    (unwind-protect
	(with-current-buffer buffer
	  (org-comments-panel-mode)
	  (let ((inhibit-read-only t))
	    (let ((first-start (point)))
	      (insert "First\n")
	      (add-text-properties first-start (point) '(org-context-panel-item (:id "first")))
	      (insert "\n")
	      (let ((second-start (point)))
		(insert "Second\n")
		(add-text-properties second-start (point) '(org-context-panel-item (:id "second")))
		(goto-char (point-min))
		(setq-local org-comments-current-item-starts-function
			    #'org-context-panel-item-starts)
		(org-comments-next-item-at-point)
		(should (= (point) second-start))
		(org-comments-next-item-at-point)
		(should (= (point) first-start))
		(org-comments-previous-item-at-point)
		(should (= (point) second-start))))))
      (kill-buffer buffer))))

(ert-deftest org-context-panel-filter-keys-use-z-prefix ()
  "Context filters live under the z prefix and zz resets filters."
  (with-temp-buffer
    (org-comments-panel-mode)
    (should (eq (local-key-binding (kbd "zx"))
		#'hub/org-context-panel-filter-toggle-missing))
    (should (eq (local-key-binding (kbd "zr"))
		#'org-comments-filter-toggle-resolved-current-ui))
    (should (eq (local-key-binding (kbd "z?"))
		#'org-comments-filter-status-current-ui))
    (should (eq (local-key-binding (kbd "zz"))
		#'org-comments-filter-reset-current-ui))))

(ert-deftest org-comments-compose-cancel-prompts-before-discarding ()
  "Canceling a modified compose buffer asks before discarding text."
  (let ((prompted nil)
	(buffer (generate-new-buffer " *compose cancel prompt*")))
    (unwind-protect
	(with-current-buffer buffer
	  (org-mode)
	  (org-comments-compose-mode 1)
	  (setq-local org-comments-compose-context '(:operation reply))
	  (insert "unsaved")
	  (cl-letf (((symbol-function 'yes-or-no-p)
		     (lambda (&rest _args)
		       (setq prompted t)
		       nil)))
	    (should-error (org-comments-compose-cancel) :type 'user-error))
	  (should prompted)
	  (should (buffer-live-p buffer)))
      (when (buffer-live-p buffer)
	(with-current-buffer buffer
	  (setq-local org-comments-compose-closing t))
	(kill-buffer buffer)))))

(ert-deftest org-comments-compose-kill-prompts-before-discarding ()
  "Killing a modified compose buffer asks before discarding text."
  (let ((prompted nil)
	(buffer (generate-new-buffer " *compose kill prompt*")))
    (unwind-protect
	(with-current-buffer buffer
	  (org-mode)
	  (org-comments-compose-mode 1)
	  (setq-local org-comments-compose-context '(:operation reply))
	  (add-hook 'kill-buffer-query-functions
		    #'org-comments-compose--confirm-kill nil t)
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
	  (setq-local org-comments-compose-closing t))
	(kill-buffer buffer)))))

(ert-deftest org-comments-compose-edit-stamps-local-update ()
  "Editing a remote-linked comment stamps local update metadata and draft badge."
  (hub/org-comments-test--with-file-buffer "article.org" "#+TITLE: Article\n\nBody"
					   (let* ((source-buffer (current-buffer))
						  (sidecar (org-comments-sidecar-path buffer-file-name)))
					     (with-temp-file sidecar
					       (insert "#+source: article.org\n#+todo: OPEN TODO | RESOLVED\n\n")
					       (insert "* OPEN Page\n:PROPERTIES:\n:ORG_COMMENTS_ID: page-1\n:ORG_COMMENTS_SYNC_KIND: footer\n:ORG_COMMENTS_REMOTE_ID: f1\n:END:\n\nOld body.\n"))
					     (cl-letf (((symbol-function 'org-comments-current-created-at)
							(lambda () "2026-06-19T20:00:00+0000")))
					       (org-comments-compose-save-edit
						(list :type 'comment :id "page-1" :sidecar-file sidecar :remote-id "f1")
						"New body."))
					     (with-temp-buffer
					       (insert-file-contents sidecar)
					       (should (search-forward ":ORG_COMMENTS_LOCAL_UPDATED_AT: 2026-06-19T20:00:00+0000" nil t)))
					     (let ((comment (car (org-comments-collect-page source-buffer))))
					       (should (equal (plist-get comment :local-updated-at)
							      "2026-06-19T20:00:00+0000"))))))

(ert-deftest org-context-panel-action-keys-live-in-package-panel-mode ()
  "Panel actions are owned by `org-comments-panel-mode-map'."
  (with-temp-buffer
    (org-comments-panel-mode)
    (should (eq (local-key-binding (kbd "?")) #'org-comments-help-current-ui))
    (should (eq (local-key-binding (kbd "U")) #'org-comments-push))
    (should (eq (local-key-binding (kbd "RET")) #'org-context-panel-jump-at-point))
    (should (eq (local-key-binding (kbd "o")) #'org-comments-open-remote))
    (should (eq (local-key-binding (kbd "p")) #'org-comments-page-open-at-point))
    (should (eq (local-key-binding (kbd "q")) #'org-comments-close-current-ui))
    (should (eq (local-key-binding (kbd "d")) #'org-comments-delete-at-point))
    (should (eq (local-key-binding (kbd "e")) #'org-comments-edit-at-point))
    (should (eq (lookup-key org-comments-panel-status-map (kbd "o"))
		#'org-comments-mark-open))
    (should (eq (lookup-key org-comments-panel-status-map (kbd "t"))
		#'org-comments-mark-todo))
    (should (eq (lookup-key org-comments-panel-status-map (kbd "r"))
		#'org-comments-mark-resolved))
    (should (eq (local-key-binding (kbd "r")) #'org-comments-reply))
    (should-not (local-key-binding (kbd "s")))))

(ert-deftest org-context-panel-uses-package-open-remote-at-point ()
  "Pressing open remote in the context panel opens the remote comment."
  (hub/org-comments-test--with-file-buffer "article.org" "#+CONFLUENCE_PAGE_ID: 123\n\nAlpha selected text omega"
					   (let* ((source-buffer (current-buffer))
						  (start (progn
							   (goto-char (point-min))
							   (search-forward "selected")
							   (match-beginning 0)))
						  (end (match-end 0))
						  (record (org-comments-create-record
							   buffer-file-name start end "Remote comment." "root-1"
							   "Alice" "2026-06-18T09:00:00+0000"))
						  (sidecar (org-comments-append-to-sidecar record))
						  (panel (generate-new-buffer " *org context open remote test*"))
						  opened)
					     (unwind-protect
						 (progn
						   (with-temp-buffer
						     (insert-file-contents sidecar)
						     (goto-char (point-min))
						     (search-forward ":ORG_COMMENTS_ID: root-1")
						     (forward-line 1)
						     (insert ":ORG_COMMENTS_REMOTE_ID: i123\n")
						     (write-region (point-min) (point-max) sidecar nil 'silent))
						   (cl-letf (((symbol-function 'org-confluence-sync-status-marker-string)
							      (lambda (&rest _args) "")))
						     (hub/org-comments-test--render-package-panel source-buffer panel))
						   (with-current-buffer panel
						     (goto-char (point-min))
						     (search-forward "Remote comment")
						     (cl-letf (((symbol-function 'org-comments-backend-detect)
								(lambda (&optional source-buffer)
								  (setq opened (list :detect-buffer source-buffer))
								  'confluence))
							       ((symbol-function 'org-comments-backend-open-remote)
								(lambda (backend comment)
								  (setq opened (append opened
										       (list :backend backend :comment comment)))
								  "remote-url")))
						       (should (equal (org-comments-open-remote-at-point)
								      "remote-url"))))
						   (should (eq (plist-get opened :detect-buffer) source-buffer))
						   (should (eq (plist-get opened :backend) 'confluence))
						   (should (equal (plist-get (plist-get opened :comment) :remote-id) "i123"))
						   (should (equal (plist-get (plist-get opened :comment) :source-file)
								  buffer-file-name)))
					       (when (buffer-live-p panel)
						 (kill-buffer panel))))))

(ert-deftest org-comments-overlays-mode-shows-page-comment-marker ()
  "Comment overlays display a top marker for page comments."
  (hub/org-comments-test--with-file-buffer "article.org" "#+TITLE: Article\n\nBody"
					   (let ((sidecar (org-comments-sidecar-path buffer-file-name)))
					     (with-temp-file sidecar
					       (insert "#+title: Comments for article.org\n")
					       (insert "#+source: article.org\n")
					       (insert "#+todo: OPEN TODO | RESOLVED\n\n")
					       (insert "* OPEN Confluence footer comment f1\n")
					       (insert ":PROPERTIES:\n")
					       (insert ":ORG_COMMENTS_ID: remote-confluence-f1\n")
					       (insert ":ORG_COMMENTS_REMOTE_ID: f1\n")
					       (insert ":ORG_COMMENTS_SOURCE: confluence\n")
					       (insert ":ORG_COMMENTS_SYNC_KIND: footer\n")
					       (insert ":ORG_COMMENTS_BODY_FORMAT: storage\n")
					       (insert ":END:\n\nFooter\n"))
					     (org-comments-overlays-refresh)
					     (should (overlayp org-comments-page-comment-overlay))
					     (should (string-match-p
						      "\\[1 PAGE comment\\]"
						      (overlay-get org-comments-page-comment-overlay 'after-string))))))

(ert-deftest org-comments-overlays-mode-persists-overlays-after-panel-close ()
  "Persistent comment overlays survive closing the context panel."
  (hub/org-comments-test--with-file-buffer "article.org" "Alpha selected text omega"
					   (let* ((start (progn
							   (goto-char (point-min))
							   (search-forward "selected")
							   (match-beginning 0)))
						  (end (match-end 0)))
					     (org-comments-append-to-sidecar
					      (org-comments-create-record buffer-file-name start end "Keep visible." "local-visible"))
					     (org-comments-mode 1)
					     (should (cl-some
						      (lambda (overlay)
							(eq (overlay-get overlay 'face) 'org-comments-region-face))
						      (overlays-at start)))
					     (hub/org-context-panel--close-ui)
					     (should (cl-some
						      (lambda (overlay)
							(eq (overlay-get overlay 'face) 'org-comments-region-face))
						      (overlays-at start)))
					     (org-comments-mode -1)
					     (should-not (cl-some
							  (lambda (overlay)
							    (eq (overlay-get overlay 'face) 'org-comments-region-face))
							  (overlays-at start))))))

(ert-deftest hub/org-page-comment-create-appends-local-footer-entry ()
  "Creating a page comment writes explicit footer metadata to the sidecar."
  (hub/org-comments-test--with-file-buffer "article.org" "#+TITLE: Article\n\nBody"
					   (let* ((source-file buffer-file-name)
						  (sidecar (org-comments-sidecar-path source-file))
						  (org-comments-author "Alice"))
					     (cl-letf (((symbol-function 'org-comments-current-created-at)
							(lambda () "2026-06-17T10:00:00+0000"))
						       ((symbol-function 'org-comments-generate-id)
							(lambda () "local-page-1")))
					       (hub/org-page-comment-create "Review the whole page."))
					     (should (equal (buffer-file-name) sidecar))
					     (should (looking-at-p (regexp-quote "Review the whole page.")))
					     (with-temp-buffer
					       (insert-file-contents sidecar)
					       (should (search-forward "#+source: article.org" nil t))
					       (should (search-forward "* OPEN Page · Alice · Review the whole page." nil t))
					       (should (search-forward ":ORG_COMMENTS_ID: local-page-1" nil t))
					       (should (search-forward ":ORG_COMMENTS_AUTHOR: Alice" nil t))
					       (should (search-forward ":ORG_COMMENTS_CREATED_AT: 2026-06-17T10:00:00+0000" nil t))
					       (should (search-forward ":ORG_COMMENTS_SYNC_KIND: footer" nil t))
					       (should-not (search-forward ":ORG_COMMENTS_TARGET:" nil t))
					       (should-not (search-forward ":ORG_COMMENTS_TARGET_TEXT:" nil t))
					       (should (search-forward "Review the whole page." nil t)))
					     (with-temp-buffer
					       (insert-file-contents source-file)
					       (should (equal (buffer-string) "#+TITLE: Article\n\nBody\n"))
					       (should-not (search-forward "HUB_COMMENT" nil t))))))


(ert-deftest org-comments-reply-create-adds-child-under-remote-root ()
  "Creating a reply writes a local child heading under a remote-linked root."
  (hub/org-comments-test--with-file-buffer "article.org" "#+TITLE: Article\n\nBody"
					   (let* ((source-file buffer-file-name)
						  (sidecar (org-comments-sidecar-path source-file))
						  (org-comments-author "Alice"))
					     (with-temp-file sidecar
					       (insert "#+source: article.org\n#+todo: OPEN TODO | RESOLVED\n\n")
					       (insert "* OPEN Remote root\n")
					       (insert ":PROPERTIES:\n")
					       (insert ":ORG_COMMENTS_ID: root-1\n")
					       (insert ":ORG_COMMENTS_REMOTE_ID: i123\n")
					       (insert ":ORG_COMMENTS_SOURCE: confluence\n")
					       (insert ":ORG_COMMENTS_SYNC_KIND: inline\n")
					       (insert ":END:\n\nRoot body\n"))
					     (find-file sidecar)
					     (org-mode)
					     (goto-char (point-min))
					     (re-search-forward "Remote root")
					     (cl-letf (((symbol-function 'org-comments-current-created-at)
							(lambda () "2026-06-18T10:00:00+0000"))
						       ((symbol-function 'org-comments-generate-id)
							(lambda () "local-reply-1")))
					       (hub/org-comment-reply-create "Reply body."))
					     (should (equal (buffer-file-name) sidecar))
					     (should (looking-at-p (regexp-quote "Reply body.")))
					     (with-temp-buffer
					       (insert-file-contents sidecar)
					       (should (search-forward "* OPEN [1 reply]" nil t))
					       (should (search-forward "** Reply · Alice" nil t))
					       (should-not (search-forward "** OPEN" nil t))
					       (should (search-forward ":ORG_COMMENTS_ID: local-reply-1" nil t))
					       (should (search-forward ":ORG_COMMENTS_SYNC_KIND: reply" nil t))
					       (should (search-forward ":ORG_COMMENTS_REMOTE_PARENT_ID: i123" nil t))
					       (should (search-forward ":ORG_COMMENTS_AUTHOR: Alice" nil t))
					       (should (search-forward ":ORG_COMMENTS_CREATED_AT: 2026-06-18T10:00:00+0000" nil t))
					       (should (search-forward "Reply body." nil t))))))

(ert-deftest org-comments-reply-create-refuses-local-only-root ()
  "Creating a Confluence reply requires a remote-linked root comment."
  (hub/org-comments-test--with-file-buffer "article.org" "#+TITLE: Article\n\nBody"
					   (let* ((sidecar (org-comments-sidecar-path buffer-file-name)))
					     (with-temp-file sidecar
					       (insert "#+source: article.org\n#+todo: OPEN TODO | RESOLVED\n\n")
					       (insert "* OPEN Local root\n:PROPERTIES:\n:ORG_COMMENTS_ID: root-1\n:ORG_COMMENTS_SYNC_KIND: inline\n:END:\n\nBody\n"))
					     (find-file sidecar)
					     (org-mode)
					     (goto-char (point-min))
					     (re-search-forward "Local root")
					     (should-error (hub/org-comment-reply-create "Reply") :type 'user-error))))

(provide 'org-comments-test)
;;; org-comments-test.el ends here
