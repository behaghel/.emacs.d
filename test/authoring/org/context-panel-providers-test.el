;;; context-panel-providers-test.el --- Mixed provider integration -*- lexical-binding: t; -*-

;;; Commentary:
;; Exercise real Marginalia and comments providers without sidecars or accounts.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org-comments)
(require 'cp-org-marginalia-context-panel)
(require 'test-helpers)
(require 'org/context-panel)

(ert-deftest hub/context-panel-marginalia-and-comments-preserve-row-actions ()
  "Standard Org note rows coexist with rich comments and their custom actions."
  (save-window-excursion
    (with-temp-buffer
      (org-mode)
      (insert "Alpha[fn:one] beta.\n\n[fn:one] *Native note*.\n")
      (set-buffer-modified-p nil)
      (let* ((source (current-buffer))
	     (content (buffer-string))
	     (reference (plist-get (car (cp-org-marginalia-collect)) :reference-pos))
	     (comment '(:type comment :id "c1" :status "OPEN" :author "Alice"
			      :target-start 1 :target-end 6 :target-text "Alpha"
			      :body "Comment body.")))
	(cl-letf (((symbol-function 'org-comments-collect)
		   (lambda (&rest _) (list comment))))
	  (context-panels-register-provider (org-comments-context-panel-provider))
	  (cp-org-marginalia-context-panel-mode 1)
	  (with-temp-buffer
	    (org-comments-panel-mode)
	    (setq-local context-panels-source-buffer source)
	    (let ((panel (current-buffer))
		  (reply-binding (key-binding (kbd "r")))
		  (edit-binding (key-binding (kbd "e"))))
	      (context-panels-render-side-panel source)
	      (search-forward "Native note")
	      (should (eq (plist-get (context-panels-item-at-point) :provider)
			  'marginalia))
	      (should (eq (key-binding (kbd "<return>"))
			  #'context-panels-jump-at-point))
	      (context-panels-jump-at-point)
	      (should (eq (current-buffer) source))
	      (should (= (point) reference))
	      (with-current-buffer panel
		(goto-char (point-min))
		(search-forward "Comment body.")
		(should (get-text-property (1- (point)) 'org-comments-comment))
		(should (eq (lookup-key (current-local-map) (kbd "r"))
			    #'org-comments-reply))
		(should (eq (lookup-key (current-local-map) (kbd "e"))
			    #'org-comments-edit))
		(should (eq (key-binding (kbd "r")) reply-binding))
		(should (eq (key-binding (kbd "e")) edit-binding))
		(should (string-match-p "[Rr]eply" (context-panels-item-help-text)))
		(context-panels-jump-at-point)
		(should (eq (current-buffer) source))
		(should (= (point) 1)))))
	  (cp-org-marginalia-context-panel-mode -1)
	  (should (context-panels-registered-provider 'comments))
	  (should context-panels-mode)
	  (context-panels-unregister-provider 'comments)
	  (context-panels-mode -1)
	  (should (equal (buffer-string) content))
	  (should-not (buffer-modified-p)))))))

(ert-deftest hub/context-panel-reopened-source-is-readable-through-both-openers ()
  "Ordinary reopen prepares both providers and preserves mixed navigation."
  (let* ((directory (make-temp-file "context-panel-reading-" t))
	 (file (expand-file-name "source.org" directory))
	 (old-width (frame-width))
	 source)
    (unwind-protect
	(save-window-excursion
	  (set-frame-width (selected-frame) 160)
	  (with-temp-file file
	    (insert "Alpha[fn:side]\nBeta[fn:foot]\n\n"
		    "[fn:side] *Sidenote body*.\n\n"
		    "[fn:foot]\n:PROPERTIES:\n:HUB_NOTE_KIND: footnote\n:END:\n\n"
		    "Footnote body.\n"))
	  (setq source (find-file-noselect file))
	  (with-current-buffer source
	    (org-comments-append-to-sidecar
	     (org-comments-create-record file 1 6
					 "Comment body. Additional words make this paragraph exceed the panel width."
					 "reading"
					 "Demo" "2026-11-14")))
	  (kill-buffer source)
	  (dolist (opener '(context-panels-open hub/context-panels-toggle-open))
	    (setq source (find-file-noselect file))
	    (switch-to-buffer source)
	    (let ((content (buffer-string)))
	      (call-interactively opener)
	      (should (context-panels-registered-provider 'comments))
	      (should (context-panels-registered-provider 'marginalia))
	      (let ((panel context-panels-side-panel-buffer))
		(should (get-buffer-window panel))
		(with-current-buffer panel
		  (should-not truncate-lines)
		  (should word-wrap)
		  (goto-char (point-min))
		  (search-forward "Comment body.")
		  (should (> (count-screen-lines (line-beginning-position)
						 (line-end-position) nil
						 (get-buffer-window panel))
			     1))
		  (should (string-match-p "\n\n✣ "  (buffer-string))))
		(dolist (case '(("Comment body." . "Alpha")
				("Sidenote body" . "[fn:side]")
				("Footnote body." . "[fn:foot]")))
		  (dolist (key '("RET" "<return>"))
		    (with-current-buffer panel
		      (select-window (get-buffer-window panel))
		      (goto-char (point-min))
		      (search-forward (car case))
		      (should (equal (get-text-property (1- (point)) 'wrap-prefix) "  "))
		      (execute-kbd-macro (kbd key)))
		    (should (eq (window-buffer (selected-window)) source))
		    (with-current-buffer source
		      (should (looking-at-p (regexp-quote (cdr case))))))))
	      (with-current-buffer source
		(should (equal (buffer-string) content))
		(should-not (buffer-modified-p))
		(context-panels-close)))
	    (kill-buffer source)))
      (when (buffer-live-p source) (kill-buffer source))
      (dolist (buffer (buffer-list))
	(when-let* ((path (buffer-file-name buffer)))
	  (when (string-prefix-p directory path) (kill-buffer buffer))))
      (set-frame-width (selected-frame) old-width)
      (delete-directory directory t))))

(ert-deftest hub/context-panel-preparation-excludes-auxiliary-and-temporary-buffers ()
  "Personal preparation does not activate marginalia in sidecars or parsers."
  (dolist (file '(nil "/tmp/reading.comments.org" "/tmp/reading.copilot.org"))
    (with-temp-buffer
      (setq-local buffer-file-name file)
      (org-mode)
      (should-not (context-panels-registered-provider 'marginalia))
      (should-not (local-variable-p 'context-panels-side-item-body-indent)))))

(ert-deftest hub/context-panel-icon-led-content-and-thread-state ()
  "Notes are icon-led bodies; state marks the thread, not message authors."
  (dolist (state '("OPEN" "TODO" "RESOLVED"))
    (with-temp-buffer
      (org-mode)
      (insert "Alpha[fn:side] beta[fn:foot].\n\n[fn:side] Quiet note.\n\n"
	      "[fn:foot]\n:PROPERTIES:\n:HUB_NOTE_KIND: footnote\n:END:\n\nBottom note.\n")
      (let* ((source (current-buffer))
	     (original (buffer-string))
	     (comment (list :type 'comment :id "thread" :status state
			    :author "Demo" :created-at "2026-11-14"
			    :target-start 1 :target-end 6 :target-text "Alpha"
			    :body "Could we make this claim more specific?"
			    :replies '((:id "reply-alice" :author "Alice"
					    :body "I will add the supporting figure.")
				       (:id "reply-demo" :author "Demo"
					    :body "Thanks, that addresses it.")))))
	(setq-local org-comments-active-comment-key "thread")
	(cl-letf (((symbol-function 'org-comments-collect)
		   (lambda (&rest _) (list comment))))
	  (context-panels-register-provider (org-comments-context-panel-provider))
	  (cp-org-marginalia-context-panel-mode 1)
	  (unwind-protect
	      (with-temp-buffer
		(org-comments-panel-mode)
		(setq-local context-panels-source-buffer source)
		(context-panels-render-side-panel source)
		(let ((text (buffer-string)))
		  (should (string-match-p "✣ Quiet note\\.\n" text))
		  (should (string-match-p "† Bottom note\\.\n" text))
		  (should-not (string-match-p "Sidenote ·\\|Footnote ·\\|Comment ·" text))
		  (should-not (string-match-p "\\[OPEN\\]\\|\\[TODO\\]\\|\\[RESOLVED\\]" text))
		  (should (string-match-p
			   (concat "✍️? " (pcase state
					   ("TODO" "◇ ")
					   ("RESOLVED" "✓ ")
					   (_ ""))
				   "“Alpha”\n") text)))
		(unless (equal state "OPEN")
		  (goto-char (point-min))
		  (search-forward (if (equal state "TODO") "◇" "✓"))
		  (should (equal (get-text-property (1- (point)) 'help-echo)
				 (if (equal state "TODO") "To do" "Resolved")))
		  (should (memq (if (equal state "TODO")
				    'org-comments-panel-state-todo
				  'org-comments-panel-state-resolved)
				(ensure-list (get-text-property (1- (point)) 'face)))))
		(dolist (reply '(("supporting figure" . "reply-alice")
				 ("Thanks, that addresses it" . "reply-demo")))
		  (goto-char (point-min))
		  (search-forward (car reply))
		  (should (equal (plist-get (context-panels-item-at-point) :id)
				 (cdr reply)))))
	    (with-current-buffer source
	      (cp-org-marginalia-context-panel-mode -1)
	      (context-panels-unregister-provider 'comments)
	      (context-panels-mode -1))))
	(should (equal (buffer-string) original))
	(should (equal (plist-get comment :status) state))))))

(ert-deftest hub/context-panel-movement-and-help-follow-the-selected-kind ()
  "Existing panel keys keep source position and describe only the selected kind."
  (save-window-excursion
    (with-temp-buffer
      (org-mode)
      (insert "Alpha[fn:one] beta.\n\n[fn:one] A useful note.\n")
      (goto-char 3)
      (let* ((source (current-buffer))
	     (content (buffer-string))
	     (comment '(:type comment :id "review" :status "OPEN" :author "Demo"
			      :target-start 1 :target-end 6 :target-text "Alpha"
			      :body "Could we clarify this?")))
	(switch-to-buffer source)
	(cl-letf (((symbol-function 'org-comments-collect)
		   (lambda (&optional buffer &rest _)
		     (when (eq (or buffer (current-buffer)) source) (list comment)))))
	  (context-panels-register-provider (org-comments-context-panel-provider))
	  (cp-org-marginalia-context-panel-mode 1)
	  (unwind-protect
	      (let* ((panel (context-panels-open source))
		     (panel-window (get-buffer-window panel))
		     (source-window (get-buffer-window source))
		     (source-start (window-start source-window))
		     (source-point (with-current-buffer source (point))))
		(select-window panel-window)
		(with-current-buffer panel
		  (goto-char (point-min))
		  (search-forward "A useful note")
		  (execute-kbd-macro (kbd "]c"))
		  (should (eq (selected-window) panel-window))
		  (should (eq (plist-get (context-panels-item-at-point) :provider) 'comments))
		  (execute-kbd-macro (kbd "[c"))
		  (should (eq (plist-get (context-panels-item-at-point) :provider) 'marginalia))
		  (should (= (with-current-buffer source (point)) source-point))
		  (should (= (window-start source-window) source-start))
		  (let (help)
		    (cl-letf (((symbol-function 'context-panels-describe-item)
			       (lambda () (interactive) (setq help (context-panels-item-help-text)))))
		      (call-interactively (key-binding (kbd "?"))))
		    (should (string-match-p "A useful note" help))
		    (should-not (string-match-p "[Rr]eply\\|[Dd]elete\\|[Pp]ush" help)))
		  (let ((error (should-error (org-comments-edit) :type 'user-error)))
		    (should (string-match-p "not a comment" (error-message-string error))))
		  (execute-kbd-macro (kbd "]c"))
		  (let ((help (context-panels-item-help-text)))
		    (should (string-match-p "[Rr]eply" help))
		    (should-not (string-match-p "r: reply\\|r    reply" help)))))
	    (with-current-buffer source
	      (context-panels-close)
	      (cp-org-marginalia-context-panel-mode -1)
	      (context-panels-unregister-provider 'comments)
	      (context-panels-mode -1))))
	(with-current-buffer source
	  (should (equal (buffer-string) content)))))))

(ert-deftest hub/context-panel-interaction-keeps-bottom-view-adapters ()
  "Shared side-row commands do not migrate existing bottom-view behavior."
  (with-temp-buffer
    (let ((source (current-buffer)) called)
      (with-temp-buffer
	(setq-local context-panels-source-buffer source context-panels-view-id 'page-comments)
	(cl-letf (((symbol-function 'org-comments-next-item-at-point)
		   (lambda () (setq called 'next)))
		  ((symbol-function 'org-comments-previous-item-at-point)
		   (lambda () (setq called 'previous)))
		  ((symbol-function 'org-comments-help-current-ui)
		   (lambda () (setq called 'help))))
	  (hub/context-panels-next-row)
	  (should (eq called 'next))
	  (hub/context-panels-previous-row)
	  (should (eq called 'previous))
	  (hub/context-panels-describe-item)
	  (should (eq called 'help)))))))

(provide 'context-panel-providers-test)
;;; context-panel-providers-test.el ends here
