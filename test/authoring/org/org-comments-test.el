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
						  (record (hub/org-comment-create-record buffer-file-name start end "Please clarify." "local-test"))
						  (sidecar (hub/org-comment-append-to-sidecar record)))
					     (with-temp-buffer
					       (insert-file-contents sidecar)
					       (should (search-forward "#+title: Comments for article.org" nil t))
					       (should (search-forward "#+source: article.org" nil t))
					       (should (search-forward "#+todo: OPEN TODO | RESOLVED" nil t))
					       (should (search-forward "* OPEN Comment: selected" nil t))
					       (should (search-forward ":HUB_COMMENT_ID: local-test" nil t))
					       (should (search-forward ":HUB_COMMENT_TARGET:" nil t))
					       (should (search-forward ":HUB_COMMENT_TARGET_LINES:" nil t))
					       (should-not (search-forward ":HUB_COMMENT_SOURCE_FILE:" nil t))
					       (should-not (search-forward ":HUB_COMMENT_STATUS:" nil t))
					       (should-not (search-forward ":HUB_COMMENT_TARGET_START:" nil t))
					       (should (search-forward ":HUB_COMMENT_TARGET_TEXT: selected" nil t))
					       (should (search-forward ":HUB_COMMENT_TARGET_HASH: sha256:" nil t))
					       (should (search-forward "Please clarify." nil t))))))

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
						   seen-annotation)
					       (cl-letf (((symbol-function 'hub/org-context-panel-open) #'ignore)
							 ((symbol-function 'completing-read)
							  (lambda (_prompt collection &rest _args)
							    (let ((metadata (funcall collection "" nil 'metadata)))
							      (setq seen-category (alist-get 'category (cdr metadata))
								    seen-annotation (funcall
										     (alist-get 'annotation-function (cdr metadata))
										     "local-second")))
							    "local-second")))
						 (hub/org-comment-reanchor new-start new-end))
					       (should (eq 'hub-org-comment seen-category))
					       (should (string-match-p "OPEN — omega" seen-annotation))
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
						 (should (search-forward ":HUB_COMMENT_TARGET_HASH: sha256:" nil t)))))))

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
							     (search-forward "* OPEN Comment: selected" nil t))))
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
						   (should (looking-at-p "\\* OPEN Comment: selected")))
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
						 (should (search-forward "* TODO Comment: selected" nil t)))
					       (hub/org-comment-cycle-status)
					       (with-temp-buffer
						 (insert-file-contents sidecar)
						 (should (search-forward "* RESOLVED Comment: selected" nil t)))
					       (hub/org-comment-mark-open)
					       (with-temp-buffer
						 (insert-file-contents sidecar)
						 (should (search-forward "* OPEN Comment: selected" nil t)))))))

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
