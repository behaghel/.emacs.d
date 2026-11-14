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
		(should (string-match-p "r: reply"
					(get-text-property (point) 'help-echo)))
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
	     (org-comments-create-record file 1 6 "Comment body." "reading"
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
		  (should (string-match-p "\n\n✣ Sidenote · side" (buffer-string))))
		(dolist (case '(("Comment body." . "Alpha")
				("Sidenote body" . "[fn:side]")
				("Footnote body." . "[fn:foot]")))
		  (dolist (key '("RET" "<return>"))
		    (with-current-buffer panel
		      (select-window (get-buffer-window panel))
		      (goto-char (point-min))
		      (search-forward (car case))
		      (should (equal (get-text-property (1- (point)) 'line-prefix) "  "))
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

(provide 'context-panel-providers-test)
;;; context-panel-providers-test.el ends here
