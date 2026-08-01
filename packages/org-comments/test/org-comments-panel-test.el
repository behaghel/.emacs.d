;;; org-comments-panel-test.el --- Panel tests for org-comments -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the standalone package comments panel lifecycle.

;;; Code:

(require 'ert)
(require 'org-comments)

(defmacro org-comments-panel-test--with-source (contents &rest body)
  "Visit a temporary Org source buffer with CONTENTS, then run BODY."
  (declare (indent 1))
  `(let* ((directory (make-temp-file "org-comments-panel" t))
	  (source-file (expand-file-name "source.org" directory)))
     (unwind-protect
	 (with-current-buffer (find-file-noselect source-file)
	   (erase-buffer)
	   (insert ,contents)
	   (save-buffer)
	   (org-mode)
	   (prog1 (progn ,@body)
	     (when (get-buffer org-comments-panel-buffer-name)
	       (kill-buffer org-comments-panel-buffer-name))))
       (delete-directory directory t))))

(ert-deftest org-comments-panel-open-creates-panel-mode-buffer ()
  "Opening the package panel creates a panel buffer tied to the source buffer."
  (org-comments-panel-test--with-source "Alpha selected text omega"
					(let ((source-buffer (current-buffer)))
					  (org-comments-panel-open)
					  (with-current-buffer org-comments-panel-buffer-name
					    (should (derived-mode-p 'org-comments-panel-mode))
					    (should (eq context-panels-source-buffer source-buffer))))))

(ert-deftest org-comments-panel-open-renders-comments-from-source ()
  "The package panel renders visible source comments."
  (org-comments-panel-test--with-source "Alpha selected text omega"
					(let ((record (org-comments-create-record buffer-file-name 7 20 "Review this." "c1" "Alice" "now")))
					  (org-comments-append-to-sidecar record)
					  (org-comments-panel-open)
					  (with-current-buffer org-comments-panel-buffer-name
					    (should (derived-mode-p 'org-comments-panel-mode))
					    (should (string-match-p "Review this" (buffer-string)))
					    (should (string-match-p "selected text" (buffer-string)))))))

(ert-deftest org-comments-panel-renders-reply-author-metadata-without-sync-prefix ()
  "Reply rows show author metadata instead of low-value sync labels."
  (with-temp-buffer
    (let ((comment (list :type 'comment
			 :status "OPEN"
			 :current t
			 :target-text "target"
			 :body "Root body"
			 :remote-author-display-name "Romain Moisescot"
			 :created-at "2026-07-29T09:37:00+0200"
			 :remote-id "root-1"
			 :replies (list (list :type 'comment
					      :body "Reply body"
					      :remote-author-display-name "Romain Moisescot"
					      :created-at "2026-07-29T10:00:00+0200"
					      :remote-id "reply-1")))))
      (org-comments-panel-render-insert-comment comment)
      (let ((text (buffer-string))
	    (expected-face (org-comments-panel-render--author-face "Romain Moisescot")))
	(should (string-match-p "Romain Moisescot · 2026-07-29 09:37" text))
	(should (string-match-p "↳ Romain Moisescot · 2026-07-29 10:00" text))
	(should-not (string-match-p "synced —" text))
	(goto-char (point-min))
	(search-forward "Romain Moisescot")
	(should (eq (get-text-property (match-beginning 0) 'face) expected-face))
	(search-forward "2026-07-29 09:37")
	(should (eq (get-text-property (match-beginning 0) 'face)
		    'org-comments-panel-timestamp))
	(search-forward "Romain Moisescot")
	(should (eq (get-text-property (match-beginning 0) 'face) expected-face))
	(search-forward "2026-07-29 10:00")
	(should (eq (get-text-property (match-beginning 0) 'face)
		    'org-comments-panel-timestamp))))))

(ert-deftest org-comments-panel-colors-reply-authors-in-overview ()
  "Collapsed reply summaries reuse the same stable author color."
  (with-temp-buffer
    (let ((comment (list :type 'comment
			 :status "OPEN"
			 :target-text "target"
			 :body "Root body"
			 :remote-author-display-name "Romain Moisescot"
			 :created-at "2026-07-29T09:37:00+0200"
			 :remote-id "root-1"
			 :replies (list (list :type 'comment
					      :body "Reply body"
					      :remote-author-display-name "Romain Moisescot"
					      :created-at "2026-07-29T10:00:00+0200"
					      :remote-id "reply-1")))))
      (org-comments-panel-render-insert-comment comment)
      (let ((expected-face (org-comments-panel-render--author-face "Romain Moisescot")))
	(goto-char (point-min))
	(search-forward "Romain Moisescot")
	(should (eq (get-text-property (match-beginning 0) 'face) expected-face))
	(search-forward "Romain Moisescot")
	(should (eq (get-text-property (match-beginning 0) 'face) expected-face))))))

(ert-deftest org-comments-panel-linkifies-raw-http-urls ()
  "Panel rendering makes raw HTTP links actionable."
  (with-temp-buffer
    (org-comments-panel-render--insert-body "See https://example.com/path for detail")
    (goto-char (point-min))
    (search-forward "https://example.com/path")
    (should (button-at (match-beginning 0)))))

(ert-deftest org-comments-panel-filters-resolved-comments ()
  "The package panel applies source-buffer-scoped filter state while rendering."
  (org-comments-panel-test--with-source "Alpha selected text omega"
					(org-comments-append-to-sidecar
					 (org-comments-create-record buffer-file-name 7 20 "Open note" "open" "Alice" "now"))
					(let ((resolved (org-comments-create-record
							 buffer-file-name 7 20 "Resolved note" "resolved" "Alice" "now")))
					  (setq resolved (plist-put resolved :status "RESOLVED"))
					  (org-comments-append-to-sidecar resolved))
					(org-comments-panel-open)
					(org-comments-filter-set-state '(:show-resolved nil) (current-buffer))
					(with-current-buffer org-comments-panel-buffer-name
					  (org-comments-panel-refresh)
					  (should (string-match-p "Open note" (buffer-string)))
					  (should-not (string-match-p "Resolved note" (buffer-string))))))

(ert-deftest org-comments-panel-close-clears-source-panel-reference ()
  "Closing the package panel clears the source buffer panel reference."
  (org-comments-panel-test--with-source "Alpha"
					(org-comments-panel-open)
					(with-current-buffer org-comments-panel-buffer-name
					  (org-comments-panel-close))
					(should-not context-panels-side-panel-buffer)))

(provide 'org-comments-panel-test)
;;; org-comments-panel-test.el ends here
