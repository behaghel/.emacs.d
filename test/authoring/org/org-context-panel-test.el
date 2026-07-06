;;; org-context-panel-test.el --- Org context panel tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the interactive Org context panel renderer.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'test-helpers)
(require 'org-confluence-people-store)
(require 'org-comments)
(require 'org-comments-context-panel)
(require 'org-comments-panel)
(require 'org/context-panel)

(defmacro org-context-panel-test--with-source (contents &rest body)
  "Run BODY in a temporary Org source buffer containing CONTENTS."
  (declare (indent 1))
  `(with-temp-buffer
     (org-mode)
     (insert ,contents)
     (goto-char (point-min))
     ,@body))

(defun org-context-panel-test--render-comments (source-buffer panel-buffer)
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

(ert-deftest org-context-panel-goto-item-key-selects-row ()
  "Jump to a context panel row by comment ID."
  (with-temp-buffer
    (org-comments-panel-mode)
    (let ((inhibit-read-only t)
	  first-start second-start)
      (setq first-start (point))
      (insert "First\n")
      (add-text-properties first-start (point)
			   '(org-context-panel-item (:id "first")))
      (setq second-start (point))
      (insert "Second\n")
      (add-text-properties second-start (point)
			   '(org-context-panel-item (:id "second")))
      (goto-char (point-min))
      (org-context-panel-goto-item-key "second")
      (should (= (point) second-start)))))

(ert-deftest org-comments-link-to-reply-opens-parent-panel-context ()
  "Reply links open their parent context and select the reply row."
  (let* ((dir (make-temp-file "hub-org-comment-reply-link-" t))
	 (source (expand-file-name "article.org" dir))
	 (sidecar (expand-file-name "article.comments.org" dir))
	 opened selected)
    (unwind-protect
	(progn
	  (with-temp-file source
	    (insert "Body.\n"))
	  (with-temp-file sidecar
	    (insert "#+source: article.org\n#+todo: OPEN TODO | RESOLVED\n\n")
	    (insert "* OPEN Page\n:PROPERTIES:\n:ORG_COMMENTS_ID: root\n:ORG_COMMENTS_SYNC_KIND: footer\n:END:\n\nPage body.\n")
	    (insert "** Reply · Alice\n:PROPERTIES:\n:ORG_COMMENTS_ID: reply-1\n:ORG_COMMENTS_SYNC_KIND: reply\n:END:\n\nReply body.\n"))
	  (let ((org-comments-ui-page-open-comment-function
		 (lambda (comment-id) (setq selected comment-id))))
	    (cl-letf (((symbol-function 'pop-to-buffer)
		       (lambda (buffer &rest _args) (setq opened buffer))))
	      (org-comments-open-link (format "%s::reply-1" source))))
	  (should (equal (buffer-file-name opened) source))
	  (should (equal selected "reply-1")))
      (dolist (buffer (buffer-list))
	(when (member (buffer-file-name buffer) (list source sidecar))
	  (kill-buffer buffer)))
      (delete-directory dir t))))

(ert-deftest org-context-panel-open-refreshes-source-comment-overlays ()
  "Opening the context panel highlights comment targets in the source buffer."
  (let* ((dir (make-temp-file "hub-context-panel-open-overlays-" t))
	 (source-file (expand-file-name "article.org" dir))
	 (source-buffer (find-file-noselect source-file)))
    (unwind-protect
	(progn
	  (delete-other-windows)
	  (switch-to-buffer source-buffer)
	  (erase-buffer)
	  (insert "Alpha selected text omega")
	  (save-buffer)
	  (org-mode)
	  (let* ((start (progn
			  (goto-char (point-min))
			  (search-forward "selected text")
			  (match-beginning 0)))
		 (end (match-end 0)))
	    (org-comments-append-to-sidecar
	     (org-comments-create-record buffer-file-name start end
					 "Please clarify." "local-open-overlay"))
	    (org-comments-mode -1)
	    (setq-local org-context-panel-providers
			(list (org-comments-context-panel-provider)))
	    (hub/org-context-panel--open-ui)
	    (should (cl-some
		     (lambda (overlay)
		       (eq (overlay-get overlay 'face) 'org-comments-region-face))
		     (overlays-at start)))))
      (when-let* ((panel (get-buffer hub/org-context-panel-buffer-name)))
	(kill-buffer panel))
      (when (buffer-live-p source-buffer)
	(kill-buffer source-buffer))
      (delete-directory dir t))))

(ert-deftest org-context-panel-renders-sidecar-comments ()
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
		 (record (org-comments-create-record
			  buffer-file-name start end "Please clarify." "local-panel"
			  "Ada" "2026-06-15T19:42:00+0200")))
	    (org-comments-append-to-sidecar record)
	    (org-comments-mode -1)
	    (goto-char start)
	    (org-context-panel-test--render-comments (current-buffer) panel)
	    (should (cl-some
		     (lambda (overlay)
		       (eq (overlay-get overlay 'face) 'org-comments-region-face))
		     (overlays-at start)))
	    (with-current-buffer panel
	      (should (search-forward "💬" nil t))
	      (should (search-forward "OPEN" nil t))
	      (should (search-forward "“selected text”" nil t))
	      (should (search-forward "Ada · 2026-06-15" nil t))
	      (should (search-forward "Please clarify." nil t))
	      (let ((item (get-text-property (point) 'org-comments-comment)))
		(should (eq 'comment (plist-get item :type)))
		(should (= start (plist-get item :target-start)))))))
      (when (get-file-buffer source-file)
	(kill-buffer (get-file-buffer source-file)))
      (kill-buffer panel)
      (delete-directory dir t))))

(ert-deftest org-context-panel-edits-sidecar-comment-entry ()
  "Panel edit opens the backing sidecar comment body through package command."
  (let* ((dir (make-temp-file "hub-context-panel-edit-" t))
	 (source-file (expand-file-name "article.org" dir))
	 (panel (generate-new-buffer " *hub context edit test*")))
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
		 (record (org-comments-create-record
			  buffer-file-name start end "Please clarify." "local-panel-edit"))
		 (sidecar (org-comments-append-to-sidecar record)))
	    (org-context-panel-test--render-comments (current-buffer) panel)
	    (with-current-buffer panel
	      (goto-char (point-min))
	      (should (search-forward "Please clarify." nil t))
	      (org-comments-edit-at-point)
	      (should (equal (buffer-file-name) sidecar))
	      (should (looking-at-p "Please clarify\\.")))))
      (when (get-file-buffer source-file)
	(kill-buffer (get-file-buffer source-file)))
      (when-let* ((sidecar-buffer (get-file-buffer (org-comments-sidecar-path source-file))))
	(kill-buffer sidecar-buffer))
      (when (buffer-live-p panel)
	(kill-buffer panel))
      (delete-directory dir t))))

(ert-deftest org-context-panel-deletes-sidecar-comment-entry ()
  "Panel delete removes the backing sidecar comment after confirmation."
  (let* ((dir (make-temp-file "hub-context-panel-delete-" t))
	 (source-file (expand-file-name "article.org" dir))
	 (panel (generate-new-buffer " *hub context delete test*")))
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
		 (record (org-comments-create-record
			  buffer-file-name start end "Please delete." "local-panel-delete"))
		 (sidecar (org-comments-append-to-sidecar record)))
	    (org-context-panel-test--render-comments (current-buffer) panel)
	    (with-current-buffer panel
	      (goto-char (point-min))
	      (should (search-forward "Please delete." nil t))
	      (org-comments-delete-at-point))
	    (with-temp-buffer
	      (insert-file-contents sidecar)
	      (should-not (search-forward "local-panel-delete" nil t)))))
      (when (get-file-buffer source-file)
	(kill-buffer (get-file-buffer source-file)))
      (when (buffer-live-p panel)
	(kill-buffer panel))
      (delete-directory dir t))))

(ert-deftest org-context-panel-marks-sidecar-comment-status ()
  "Panel status commands update the backing sidecar comment TODO keyword."
  (let* ((dir (make-temp-file "hub-context-panel-status-" t))
	 (source-file (expand-file-name "article.org" dir))
	 (panel (generate-new-buffer " *hub context status test*")))
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
		 (record (org-comments-create-record
			  buffer-file-name start end "Please triage." "local-panel-status"))
		 (sidecar (org-comments-append-to-sidecar record)))
	    (org-context-panel-test--render-comments (current-buffer) panel)
	    (with-current-buffer panel
	      (goto-char (point-min))
	      (should (search-forward "Please triage." nil t))
	      (org-comments-mark-todo-at-point)
	      (goto-char (point-min))
	      (search-forward "Please triage.")
	      (org-comments-mark-resolved-at-point)
	      (goto-char (point-min))
	      (search-forward "Please triage.")
	      (org-comments-mark-open-at-point))
	    (with-temp-buffer
	      (insert-file-contents sidecar)
	      (should (search-forward "* OPEN " nil t)))))
      (when (get-file-buffer source-file)
	(kill-buffer (get-file-buffer source-file)))
      (when (buffer-live-p panel)
	(kill-buffer panel))
      (delete-directory dir t))))

(ert-deftest org-context-panel-renders-stale-comments-as-unanchored-warnings ()
  "The panel renderer shows stale sidecar comments without source overlays."
  (let* ((dir (make-temp-file "hub-context-panel-stale-" t))
	 (source-file (expand-file-name "article.org" dir))
	 (panel (generate-new-buffer " *hub context stale test*")))
    (unwind-protect
	(with-current-buffer (find-file-noselect source-file)
	  (erase-buffer)
	  (insert "Alpha selected text omega")
	  (save-buffer)
	  (org-mode)
	  (let* ((start (progn
			  (goto-char (point-min))
			  (search-forward "selected")
			  (match-beginning 0)))
		 (end (match-end 0))
		 (record (org-comments-create-record
			  buffer-file-name start end "Please revisit." "local-stale-panel")))
	    (org-comments-append-to-sidecar record)
	    (save-excursion
	      (goto-char start)
	      (delete-region start end)
	      (insert "removed"))
	    (org-context-panel-test--render-comments (current-buffer) panel)
	    (should-not (cl-some
			 (lambda (overlay)
			   (eq (overlay-get overlay 'face) 'org-comments-region-face))
			 (overlays-at start)))
	    (with-current-buffer panel
	      (should (search-forward "⚠" nil t))
	      (should (search-forward "OPEN" nil t))
	      (should (search-forward "Anchor no longer matches source text." nil t))
	      (should (search-forward "Please revisit." nil t))
	      (let ((item (get-text-property (point) 'org-comments-comment)))
		(should (eq 'stale (plist-get item :anchor-state))))
	      (org-comments-jump-at-point)
	      (should (string-suffix-p "article.comments.org" buffer-file-name))
	      (should (looking-at-p "\\* OPEN .*“selected”")))))
      (when (get-file-buffer source-file)
	(kill-buffer (get-file-buffer source-file)))
      (when (buffer-live-p panel)
	(kill-buffer panel))
      (delete-directory dir t))))

(ert-deftest org-context-panel-composed-render-keeps-stale-comments ()
  "Composed context panels keep stale comment warnings visible."
  (let* ((dir (make-temp-file "hub-context-panel-composed-stale-" t))
	 (source-file (expand-file-name "article.org" dir)))
    (unwind-protect
	(with-current-buffer (find-file-noselect source-file)
	  (erase-buffer)
	  (insert "Alpha selected text omega")
	  (save-buffer)
	  (org-mode)
	  (let* ((start (progn
			  (goto-char (point-min))
			  (search-forward "selected")
			  (match-beginning 0)))
		 (end (match-end 0)))
	    (org-comments-append-to-sidecar
	     (org-comments-create-record
	      buffer-file-name start end "Please revisit." "local-composed-stale"))
	    (save-excursion
	      (goto-char start)
	      (delete-region start end)
	      (insert "removed"))
	    (let ((panel (hub/org-context-panel--open-ui)))
	      (with-current-buffer panel
		(should (search-forward "⚠" nil t))
		(should (search-forward "Anchor no longer matches source text." nil t))
		(should (search-forward "Please revisit." nil t))))))
      (when-let* ((panel (get-buffer hub/org-context-panel-buffer-name)))
	(kill-buffer panel))
      (when (get-file-buffer source-file)
	(kill-buffer (get-file-buffer source-file)))
      (delete-directory dir t))))

(ert-deftest org-context-panel-source-ret-falls-back-outside-comments ()
  "Source RET uses Evil fallback when point is not in a commented region."
  (org-context-panel-test--with-source "Plain text"
				       (let ((called nil))
					 (let ((org-comments-ui-open-function
						(lambda () (error "Panel should not open without a comment"))))
					   (cl-letf (((symbol-function 'evil-ret)
						      (lambda (&rest _) (interactive) (setq called t))))
					     (hub/org-comments-source-ret-dwim)
					     (should called))))))

(ert-deftest org-context-panel-focuses-current-comment ()
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
	    (org-comments-append-to-sidecar
	     (org-comments-create-record buffer-file-name first-start first-end "First body." "local-first"))
	    (org-comments-append-to-sidecar
	     (org-comments-create-record buffer-file-name second-start second-end "Second body." "local-second"))
	    (goto-char second-end)
	    (org-context-panel-test--render-comments (current-buffer) panel)
	    (with-current-buffer panel
	      (should (search-forward "Second body." nil t))
	      (should-not (search-forward "First body." nil t)))))
      (when (get-file-buffer source-file)
	(kill-buffer (get-file-buffer source-file)))
      (kill-buffer panel)
      (delete-directory dir t))))

(ert-deftest org-context-panel-closes-on-selected-non-org-buffer ()
  "A visible context panel closes when selection moves to a non-Org buffer."
  (let* ((dir (make-temp-file "hub-context-panel-non-org-" t))
	 (source-file (expand-file-name "source.org" dir))
	 (source-buffer (find-file-noselect source-file))
	 (other-buffer (generate-new-buffer " *hub context non-org*")))
    (unwind-protect
	(progn
	  (with-current-buffer source-buffer
	    (erase-buffer)
	    (insert "Text[fn:one]\n\n[fn:one] Body.\n")
	    (save-buffer)
	    (org-mode))
	  (with-current-buffer other-buffer
	    (emacs-lisp-mode))
	  (delete-other-windows)
	  (switch-to-buffer source-buffer)
	  (org-comments-open)
	  (should (hub/org-context-panel--visible-window))
	  (switch-to-buffer other-buffer)
	  (org-context-panel--follow-selected-buffer)
	  (should-not (hub/org-context-panel--visible-window)))
      (when-let* ((panel (get-buffer hub/org-context-panel-buffer-name)))
	(kill-buffer panel))
      (when (buffer-live-p source-buffer) (kill-buffer source-buffer))
      (when (buffer-live-p other-buffer) (kill-buffer other-buffer))
      (delete-directory dir t))))

(ert-deftest org-context-panel-ignores-selection-while-minibuffer-active ()
  "A visible context panel must not follow or close during minibuffer prompts."
  (let* ((dir (make-temp-file "hub-context-panel-minibuffer-" t))
	 (source-file (expand-file-name "source.org" dir))
	 (source-buffer (find-file-noselect source-file))
	 (other-buffer (generate-new-buffer " *hub context minibuffer*")))
    (unwind-protect
	(progn
	  (with-current-buffer source-buffer
	    (erase-buffer)
	    (insert "Text[fn:one]\n\n[fn:one] Body.\n")
	    (save-buffer)
	    (org-mode))
	  (with-current-buffer other-buffer
	    (emacs-lisp-mode))
	  (delete-other-windows)
	  (switch-to-buffer source-buffer)
	  (org-comments-open)
	  (let ((panel-window (hub/org-context-panel--visible-window)))
	    (switch-to-buffer other-buffer)
	    (cl-letf (((symbol-function 'active-minibuffer-window)
		       (lambda () (selected-window))))
	      (org-context-panel--follow-selected-buffer))
	    (should (window-live-p panel-window))
	    (should (eq panel-window (hub/org-context-panel--visible-window)))
	    (with-current-buffer (window-buffer panel-window)
	      (should (equal source-buffer org-context-panel-source-buffer)))))
      (when-let* ((panel (get-buffer hub/org-context-panel-buffer-name)))
	(kill-buffer panel))
      (when (buffer-live-p source-buffer) (kill-buffer source-buffer))
      (when (buffer-live-p other-buffer) (kill-buffer other-buffer))
      (delete-directory dir t))))

(ert-deftest org-context-panel-follows-selected-org-buffer ()
  "A visible context panel rebinds to the selected Org buffer."
  (let* ((dir (make-temp-file "hub-context-panel-follow-" t))
	 (first-file (expand-file-name "first.org" dir))
	 (second-file (expand-file-name "second.org" dir))
	 (first-buffer (find-file-noselect first-file))
	 (second-buffer (find-file-noselect second-file)))
    (unwind-protect
	(progn
	  (with-current-buffer first-buffer
	    (erase-buffer)
	    (insert "Alpha first omega")
	    (save-buffer)
	    (org-mode)
	    (let ((start (progn
			   (goto-char (point-min))
			   (search-forward "first")
			   (match-beginning 0)))
		  (end (match-end 0)))
	      (org-comments-append-to-sidecar
	       (org-comments-create-record buffer-file-name start end "First body." "local-first"))))
	  (with-current-buffer second-buffer
	    (erase-buffer)
	    (insert "Beta second omega")
	    (save-buffer)
	    (org-mode)
	    (let ((start (progn
			   (goto-char (point-min))
			   (search-forward "second")
			   (match-beginning 0)))
		  (end (match-end 0)))
	      (org-comments-append-to-sidecar
	       (org-comments-create-record buffer-file-name start end "Second body." "local-second"))))
	  (delete-other-windows)
	  (switch-to-buffer first-buffer)
	  (org-comments-open)
	  (with-current-buffer second-buffer
	    (let ((org-comments-panel-buffer-name hub/org-context-panel-buffer-name))
	      (org-comments-context-panel-enable)))
	  (switch-to-buffer second-buffer)
	  (org-context-panel--follow-selected-buffer)
	  (with-current-buffer (get-buffer hub/org-context-panel-buffer-name)
	    (should (equal second-buffer org-context-panel-source-buffer))
	    (should (search-forward "Second body." nil t))
	    (should-not (search-forward "First body." nil t))))
      (when-let* ((panel (get-buffer hub/org-context-panel-buffer-name)))
	(kill-buffer panel))
      (when (buffer-live-p first-buffer) (kill-buffer first-buffer))
      (when (buffer-live-p second-buffer) (kill-buffer second-buffer))
      (delete-directory dir t))))

(ert-deftest org-context-panel-docks-and-restores-visual-fill-column ()
  "Opening the context panel can dock visual-fill-column prose toward the panel."
  (org-context-panel-test--with-source "Text"
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

(provide 'org-context-panel-test)
;;; org-context-panel-test.el ends here
