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

(ert-deftest hub/org-comment-create-command-writes-sidecar-and-refreshes-panel ()
  "The interactive command writes a sidecar comment and refreshes context UI."
  (hub/org-comments-test--with-file-buffer "article.org" "Alpha selected text omega"
					   (let ((start (progn (goto-char (point-min)) (search-forward "selected") (match-beginning 0)))
						 (end (match-end 0))
						 (opened nil))
					     (let ((sidecar (hub/org-comment-sidecar-path buffer-file-name)))
					       (cl-letf (((symbol-function 'hub/org-context-panel-open)
							  (lambda () (setq opened t)))
							 ((symbol-function 'hub/org-comment-generate-id)
							  (lambda () "local-command")))
						 (hub/org-comment-create start end "Please revise."))
					       (should opened)
					       (should (= start (point)))
					       (with-temp-buffer
						 (insert-file-contents sidecar)
						 (should (search-forward ":HUB_COMMENT_ID: local-command" nil t))
						 (should (search-forward "Please revise." nil t)))))))

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
