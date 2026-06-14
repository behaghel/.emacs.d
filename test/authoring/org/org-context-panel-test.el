;;; org-context-panel-test.el --- Org context panel tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the interactive Org context panel renderer.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'test-helpers)
(require 'hub-org-comments)
(require 'org/context-panel)

(defmacro hub/org-context-panel-test--with-source (contents &rest body)
  "Run BODY in a temporary Org source buffer containing CONTENTS."
  (declare (indent 1))
  `(with-temp-buffer
     (org-mode)
     (insert ,contents)
     (goto-char (point-min))
     ,@body))

(ert-deftest hub/org-context-panel-renders-side-notes ()
  "The panel renderer inserts note bodies in display-line order."
  (hub/org-context-panel-test--with-source "Text[fn:one]\n\n[fn:one] Note body.\n"
					   (let ((panel (generate-new-buffer " *hub context panel test*")))
					     (unwind-protect
						 (let ((source (current-buffer)))
						   (hub/org-context-panel-render-buffer source panel)
						   (with-current-buffer panel
						     (should (equal source hub/org-context-panel-source-buffer))
						     (should buffer-read-only)
						     (should-not truncate-lines)
						     (should word-wrap)
						     (should visual-line-mode)
						     (should (search-forward "Note body." nil t))))
					       (kill-buffer panel)))))

(ert-deftest hub/org-context-panel-marginalia-with-viewport-anchor-replaces-anchor ()
  "Viewport anchoring must replace, not append after, the original anchor line."
  (let ((note (hub/org-context-panel--marginalia-with-viewport-anchor
	       '(:id "one" :anchor-line 486 :body "x") 21)))
    (should (= 21 (plist-get note :anchor-line)))
    (should (= 486 (plist-get note :logical-anchor-line)))))

(ert-deftest hub/org-context-panel-renders-sidecar-comments ()
  "The panel renderer includes valid sidecar comments."
  (let* ((dir (make-temp-file "hub-context-panel-" t))
	 (source-file (expand-file-name "article.org" dir))
	 (panel (generate-new-buffer " *hub context comment test*")))
    (unwind-protect
	(with-current-buffer (find-file-noselect source-file)
	  (erase-buffer)
	  (insert "Alpha selected text omega")
	  (save-buffer)
	  (org-mode)
	  (let* ((start (progn
			  (goto-char (point-min))
			  (search-forward "selected text")
			  (match-beginning 0)))
		 (end (match-end 0))
		 (record (hub/org-comment-create-record
			  buffer-file-name start end "Please clarify." "local-panel")))
	    (hub/org-comment-append-to-sidecar record)
	    (hub/org-comment-overlays-mode -1)
	    (goto-char start)
	    (hub/org-context-panel-render-buffer (current-buffer) panel)
	    (should (cl-some
		     (lambda (overlay)
		       (eq (overlay-get overlay 'face) 'hub/org-context-panel-comment-region-face))
		     (overlays-at start)))
	    (with-current-buffer panel
	      (should (search-forward "💬" nil t))
	      (should (search-forward "OPEN" nil t))
	      (let ((face (get-text-property (match-beginning 0) 'face)))
		(should (memq 'hub/org-context-panel-status-open-face
			      (if (listp face) face (list face))))
		(should (memq 'hub/org-context-panel-current-item-face
			      (if (listp face) face (list face)))))
	      (should (search-forward "“selected text”" nil t))
	      (should (search-forward "Please clarify." nil t))
	      (let ((item (get-text-property (point) 'hub-org-context-panel-item)))
		(should (eq 'comment (plist-get item :type)))
		(should (= start (plist-get item :jump-pos)))))
	    (with-current-buffer (current-buffer)
	      (hub/org-context-panel-close)
	      (should-not (cl-some
			   (lambda (overlay)
			     (eq (overlay-get overlay 'face) 'hub/org-context-panel-comment-region-face))
			   (overlays-at start))))))
      (when (get-file-buffer source-file)
	(kill-buffer (get-file-buffer source-file)))
      (kill-buffer panel)
      (delete-directory dir t))))

(ert-deftest hub/org-context-panel-truncates-comment-target-preview ()
  "Comment target previews stay on the card header line."
  (let ((hub/org-context-panel-target-preview-length 12))
    (hub/org-context-panel-test--with-source "Alpha[fn:one]\n\n[fn:one] Note body.\n"
					     (let ((panel (generate-new-buffer " *hub context target preview test*")))
					       (unwind-protect
						   (with-current-buffer panel
						     (hub/org-context-panel-buffer-mode)
						     (let ((inhibit-read-only t))
						       (hub/org-context-panel--insert-comment
							'(:status "open"
								  :target-text "a very long commented region preview"
								  :body "Body.")))
						     (goto-char (point-min))
						     (should (search-forward "“a very long …”" nil t))
						     (should-not (search-forward "commented region" nil t)))
						 (kill-buffer panel))))))

(ert-deftest hub/org-context-panel-clamps-overview-comment-body ()
  "Overview comments show at most two compact body lines."
  (let ((hub/org-context-panel-overview-comment-line-length 10)
	(hub/org-context-panel-overview-comment-lines 2))
    (hub/org-context-panel-test--with-source "Alpha[fn:one]\n\n[fn:one] Note body.\n"
					     (let ((panel (generate-new-buffer " *hub context overview clamp test*")))
					       (unwind-protect
						   (with-current-buffer panel
						     (hub/org-context-panel-buffer-mode)
						     (let ((inhibit-read-only t))
						       (hub/org-context-panel--insert-comment
							'(:status "open"
								  :target-text "target"
								  :body "one two three four five six seven eight nine")))
						     (goto-char (point-min))
						     (should (search-forward "one two th" nil t))
						     (should (search-forward "ree four …" nil t))
						     (should-not (search-forward "five" nil t)))
						 (kill-buffer panel))))))

(ert-deftest hub/org-context-panel-focuses-current-comment ()
  "When point is inside a comment target, the panel shows only that comment."
  (let* ((dir (make-temp-file "hub-context-panel-focus-" t))
	 (source-file (expand-file-name "article.org" dir))
	 (panel (generate-new-buffer " *hub context focus test*")))
    (unwind-protect
	(with-current-buffer (find-file-noselect source-file)
	  (erase-buffer)
	  (insert "Alpha first beta second omega")
	  (save-buffer)
	  (org-mode)
	  (let* ((first-start (progn
				(goto-char (point-min))
				(search-forward "first")
				(match-beginning 0)))
		 (first-end (match-end 0))
		 (second-start (progn
				 (search-forward "second")
				 (match-beginning 0)))
		 (second-end (match-end 0)))
	    (hub/org-comment-append-to-sidecar
	     (hub/org-comment-create-record buffer-file-name first-start first-end "First body." "local-first"))
	    (hub/org-comment-append-to-sidecar
	     (hub/org-comment-create-record buffer-file-name second-start second-end "Second body." "local-second"))
	    (goto-char second-end)
	    (hub/org-context-panel-render-buffer (current-buffer) panel)
	    (with-current-buffer panel
	      (should (search-forward "Second body." nil t))
	      (should-not (search-forward "First body." nil t)))))
      (when (get-file-buffer source-file)
	(kill-buffer (get-file-buffer source-file)))
      (kill-buffer panel)
      (delete-directory dir t))))

(ert-deftest hub/org-context-panel-docks-and-restores-visual-fill-column ()
  "Opening the context panel can dock visual-fill-column prose toward the panel."
  (hub/org-context-panel-test--with-source "Text"
					   (let ((source (current-buffer))
						 (set-margins-called nil)
						 (require-function (symbol-function 'require)))
					     (setq-local visual-fill-column-mode t
							 visual-fill-column-width 80
							 visual-fill-column-center-text t
							 visual-fill-column-extra-text-width nil)
					     (cl-letf (((symbol-function 'visual-fill-column--window-max-text-width)
							(lambda (_window) 120))
						       ((symbol-function 'visual-fill-column--set-margins)
							(lambda (_window) (setq set-margins-called t)))
						       ((symbol-function 'require)
							(lambda (feature &optional filename noerror)
							  (or (eq feature 'visual-fill-column)
							      (funcall require-function feature filename noerror)))))
					       (switch-to-buffer source)
					       (hub/org-context-panel--dock-prose (selected-window))
					       (should visual-fill-column-center-text)
					       (should (equal '(-20 . 20) visual-fill-column-extra-text-width))
					       (should set-margins-called)
					       (setq set-margins-called nil)
					       (hub/org-context-panel--restore-prose-docking source)
					       (should visual-fill-column-center-text)
					       (should-not visual-fill-column-extra-text-width)
					       (should set-margins-called)))))

(ert-deftest hub/org-context-panel-records-jump-targets ()
  "Rendered notes carry their source footnote definition position."
  (hub/org-context-panel-test--with-source "Text[fn:one]\n\n[fn:one] Note body.\n"
					   (let ((panel (generate-new-buffer " *hub context panel target test*")))
					     (unwind-protect
						 (progn
						   (hub/org-context-panel-render-buffer (current-buffer) panel)
						   (with-current-buffer panel
						     (goto-char (point-min))
						     (search-forward "Note body.")
						     (let ((note (get-text-property (point) 'hub-org-context-panel-item)))
						       (should (equal "one" (plist-get note :id)))
						       (should (integerp (plist-get note :definition-pos))))))
					       (kill-buffer panel)))))

(provide 'org-context-panel-test)
;;; org-context-panel-test.el ends here
