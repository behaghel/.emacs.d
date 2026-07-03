;;; org-comments-commands-test.el --- Command tests for org-comments package -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for Org-native comments command keybindings and setup.

;;; Code:

(require 'ert)
(require 'org-comments-commands)

(ert-deftest org-comments-commands-default-prefix-is-c-c-semicolon ()
  "The default minor-mode prefix follows Emacs package key conventions."
  (should (equal org-comments-keymap-prefix "C-c ;"))
  (should (eq (lookup-key org-comments-mode-map (kbd "C-c ; c"))
	      #'org-comments-create))
  (should (eq (lookup-key org-comments-mode-map (kbd "C-c ; P"))
	      #'org-comments-page-create))
  (should (eq (lookup-key org-comments-mode-map (kbd "C-c ; n"))
	      #'org-comments-next))
  (should (eq (lookup-key org-comments-mode-map (kbd "C-c ; p"))
	      #'org-comments-previous))
  (should (eq (lookup-key org-comments-mode-map (kbd "C-c ; S"))
	      #'org-comments-sync))
  (should (eq (lookup-key org-comments-mode-map (kbd "C-c ; O"))
	      #'org-comments-open-remote))
  (should (eq (lookup-key org-comments-mode-map (kbd "C-c ; U"))
	      #'org-comments-push))
  (should (eq (lookup-key org-comments-mode-map (kbd "C-c ; D"))
	      #'org-comments-pull))
  (should (eq (lookup-key org-comments-mode-map (kbd "C-c ; m r"))
	      #'org-comments-mark-resolved)))

(ert-deftest org-comments-commands-rebuilds-custom-prefix-map ()
  "The minor-mode keymap can be rebuilt after prefix customization."
  (let ((org-comments-keymap-prefix "C-c '"))
    (org-comments-rebuild-mode-map)
    (unwind-protect
	(progn
	  (should (eq (lookup-key org-comments-mode-map (kbd "C-c ' c"))
		      #'org-comments-create))
	  (should-not (eq (lookup-key org-comments-mode-map (kbd "C-c ; c"))
			  #'org-comments-create)))
      (let ((org-comments-keymap-prefix "C-c ;"))
	(org-comments-rebuild-mode-map)))))

(ert-deftest org-comments-commands-setup-adds-org-mode-hook ()
  "Setup enables `org-comments-mode' through `org-mode-hook'."
  (let (org-mode-hook)
    (org-comments-setup)
    (should (memq #'org-comments-mode org-mode-hook))))

(ert-deftest org-comments-commands-open-prefers-registered-ui-when-available ()
  "Open dispatches to the registered rich comments UI."
  (let ((org-comments-ui-open-function (lambda () :panel)))
    (should (eq (org-comments-open) :panel))))

(ert-deftest org-comments-commands-sync-detects-backend-from-source-buffer ()
  "Sync detects the comments backend and passes source-buffer context."
  (let* ((directory (make-temp-file "org-comments-sync" t))
	 (source-file (expand-file-name "source.org" directory))
	 called)
    (unwind-protect
	(progn
	  (with-temp-file source-file
	    (insert "#+title: Source\n\nBody\n"))
	  (with-current-buffer (find-file-noselect source-file)
	    (org-mode)
	    (cl-letf (((symbol-function 'org-comments-backend-detect)
		       (lambda (&optional source-buffer)
			 (setq called (list :detect-buffer source-buffer))
			 'fake))
		      ((symbol-function 'org-comments-backend-sync)
		       (lambda (id record)
			 (setq called (append called
					      (list :sync-id id :record record)))
			 '(:comments-imported 1 :comments-pushed 2))))
	      (should (equal (org-comments-sync)
			     '(:comments-imported 1 :comments-pushed 2))))))
      (should (eq (plist-get called :detect-buffer)
		  (find-buffer-visiting source-file)))
      (should (eq (plist-get called :sync-id) 'fake))
      (should (equal (plist-get called :record)
		     (list :source-file source-file))))
    (when-let* ((buffer (find-buffer-visiting source-file)))
      (kill-buffer buffer))
    (delete-directory directory t)))

(ert-deftest org-comments-commands-sync-requires-file-backed-org-buffer ()
  "Sync is only available from file-backed Org buffers."
  (with-temp-buffer
    (org-mode)
    (should-error (org-comments-sync) :type 'user-error)))

(ert-deftest org-comments-commands-open-remote-detects-backend-from-source-buffer ()
  "Open remote detects the backend and passes source-file context."
  (let* ((directory (make-temp-file "org-comments-open-remote" t))
	 (source-file (expand-file-name "source.org" directory))
	 called)
    (unwind-protect
	(progn
	  (with-temp-file source-file
	    (insert "#+title: Source\n\nBody\n"))
	  (with-current-buffer (find-file-noselect source-file)
	    (org-mode)
	    (cl-letf (((symbol-function 'org-comments--comment-at-point)
		       (lambda () '(:id "local-1" :remote-id "c456")))
		      ((symbol-function 'org-comments-backend-detect)
		       (lambda (&optional source-buffer)
			 (setq called (list :detect-buffer source-buffer))
			 'fake))
		      ((symbol-function 'org-comments-backend-open-remote)
		       (lambda (id comment)
			 (setq called (append called
					      (list :open-id id :comment comment)))
			 "https://example.test/comment")))
	      (should (equal (org-comments-open-remote)
			     "https://example.test/comment")))))
      (should (eq (plist-get called :detect-buffer)
		  (find-buffer-visiting source-file)))
      (should (eq (plist-get called :open-id) 'fake))
      (should (equal (plist-get called :comment)
		     (list :id "local-1"
			   :remote-id "c456"
			   :source-file source-file))))
    (when-let* ((buffer (find-buffer-visiting source-file)))
      (kill-buffer buffer))
    (delete-directory directory t)))

(ert-deftest org-comments-commands-open-remote-dwim-opens-panel-row ()
  "Open remote delegates to the panel action when point is on a rendered row."
  (with-temp-buffer
    (insert "row")
    (add-text-properties (point-min) (point-max)
			 '(org-comments-comment (:id "c1")))
    (goto-char (point-min))
    (cl-letf (((symbol-function 'org-comments-open-remote-at-point)
	       (lambda () "https://example.test/panel")))
      (should (equal (org-comments-open-remote)
		     "https://example.test/panel")))))

(ert-deftest org-comments-commands-open-remote-requires-file-backed-org-buffer ()
  "Open remote is only available from file-backed Org buffers or panel rows."
  (with-temp-buffer
    (org-mode)
    (should-error (org-comments-open-remote) :type 'user-error)))

(ert-deftest org-comments-commands-push-detects-backend-from-source-buffer ()
  "Push detects the backend and passes current comment context."
  (let* ((directory (make-temp-file "org-comments-push" t))
	 (source-file (expand-file-name "source.org" directory))
	 (sidecar-file (expand-file-name "source.comments.org" directory))
	 called)
    (unwind-protect
	(progn
	  (with-temp-file source-file
	    (insert "#+title: Source\n\nBody\n"))
	  (with-current-buffer (find-file-noselect source-file)
	    (org-mode)
	    (cl-letf (((symbol-function 'org-comments--comment-at-point)
		       (lambda () (list :id "local-1" :sidecar-file sidecar-file)))
		      ((symbol-function 'org-comments-backend-detect)
		       (lambda (&optional source-buffer)
			 (setq called (list :detect-buffer source-buffer))
			 'fake))
		      ((symbol-function 'org-comments-backend-push)
		       (lambda (id comment)
			 (setq called (append called
					      (list :push-id id :comment comment)))
			 '(:pushed t))))
	      (should (equal (org-comments-push) '(:pushed t))))))
      (should (eq (plist-get called :detect-buffer)
		  (find-buffer-visiting source-file)))
      (should (eq (plist-get called :push-id) 'fake))
      (should (equal (plist-get called :comment)
		     (list :id "local-1"
			   :sidecar-file sidecar-file
			   :source-file source-file))))
    (when-let* ((buffer (find-buffer-visiting source-file)))
      (kill-buffer buffer))
    (delete-directory directory t)))

(ert-deftest org-comments-commands-push-requires-file-backed-org-buffer ()
  "Push is only available from file-backed Org buffers."
  (with-temp-buffer
    (org-mode)
    (should-error (org-comments-push) :type 'user-error)))

(ert-deftest org-comments-commands-pull-detects-backend-from-source-buffer ()
  "Pull detects the backend and passes source-file context."
  (let* ((directory (make-temp-file "org-comments-pull" t))
	 (source-file (expand-file-name "source.org" directory))
	 called)
    (unwind-protect
	(progn
	  (with-temp-file source-file
	    (insert "#+title: Source\n\nBody\n"))
	  (with-current-buffer (find-file-noselect source-file)
	    (org-mode)
	    (cl-letf (((symbol-function 'org-comments-backend-detect)
		       (lambda (&optional source-buffer)
			 (setq called (list :detect-buffer source-buffer))
			 'fake))
		      ((symbol-function 'org-comments-backend-pull)
		       (lambda (id record)
			 (setq called (append called
					      (list :pull-id id :record record)))
			 '(:comments-imported 2))))
	      (should (equal (org-comments-pull) '(:comments-imported 2))))))
      (should (eq (plist-get called :detect-buffer)
		  (find-buffer-visiting source-file)))
      (should (eq (plist-get called :pull-id) 'fake))
      (should (equal (plist-get called :record)
		     (list :source-file source-file))))
    (when-let* ((buffer (find-buffer-visiting source-file)))
      (kill-buffer buffer))
    (delete-directory directory t)))

(ert-deftest org-comments-commands-pull-requires-file-backed-org-buffer ()
  "Pull is only available from file-backed Org buffers."
  (with-temp-buffer
    (org-mode)
    (should-error (org-comments-pull) :type 'user-error)))

(ert-deftest org-comments-commands-open-falls-back-to-package-panel ()
  "Open uses the standalone package panel when no richer UI is registered."
  (let* ((directory (make-temp-file "org-comments-open-panel" t))
	 (source-file (expand-file-name "source.org" directory))
	 (org-comments-ui-open-function nil))
    (unwind-protect
	(progn
	  (with-temp-file source-file
	    (insert "Alpha\n"))
	  (with-current-buffer (find-file-noselect source-file)
	    (org-mode)
	    (org-comments-open)
	    (with-current-buffer org-comments-panel-buffer-name
	      (should (derived-mode-p 'org-comments-panel-mode)))))
      (when (get-buffer org-comments-panel-buffer-name)
	(kill-buffer org-comments-panel-buffer-name))
      (delete-directory directory t))))

(provide 'org-comments-commands-test)
;;; org-comments-commands-test.el ends here
