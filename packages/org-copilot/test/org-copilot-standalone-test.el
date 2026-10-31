;;; org-copilot-standalone-test.el --- Standalone Copilot contracts -*- lexical-binding: t; -*-

;;; Commentary:
;; Exercise package-owned behavior without personal configuration or live models.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'package)
(require 'org-copilot)

(ert-deftest org-copilot-standalone-declares-runtime-dependencies ()
  "The facade declares its actual reusable package dependency closure."
  (with-temp-buffer
    (insert-file-contents (locate-library "org-copilot"))
    (let ((requirements (package-desc-reqs (package-buffer-info))))
      (dolist (dependency '(context-panels org-comments org-suggestions))
	(should (assq dependency requirements))))))

(ert-deftest org-copilot-standalone-chat-keeps-modal-policy-outside-package ()
  "A reusable chat buffer does not switch the caller's Evil state."
  (let ((org-copilot-chat-mode-hook nil)
	selected)
    (cl-letf (((symbol-function 'evil-insert-state) (lambda () (setq selected t))))
      (with-temp-buffer (org-copilot-chat-mode)))
    (should-not selected)))

(ert-deftest org-copilot-standalone-missing-review-adapter-is-actionable ()
  "Unconfigured review fails clearly without activating a provider."
  (let ((org-copilot-review-function nil))
    (with-temp-buffer
      (org-mode)
      (insert "* Intro\nAlpha draft.\n")
      (should (string-match-p
	       "No Org Copilot review adapter configured"
	       (error-message-string
		(should-error (org-copilot-review-dwim) :type 'user-error)))))))

(defvar org-copilot-demo--pending)

(ert-deftest org-copilot-standalone-scripted-chat-proposal-accept-undo-and-restore ()
  "The offline chat path persists artifacts and changes source only on acceptance."
  (let* ((directory (make-temp-file "copilot-offline-flow-" t))
	 (file (expand-file-name "draft.org" directory))
	 (examples (expand-file-name "examples" (file-name-directory (locate-library "org-copilot"))))
	 (org-copilot-demo--pending nil)
	 source)
    (unwind-protect
	(save-window-excursion
	  (load (expand-file-name "org-copilot-demo.el" examples) nil t)
	  (copy-file (expand-file-name "standalone.org" examples) file)
	  (find-file file)
	  (setq source (current-buffer))
	  (let ((before (buffer-string)))
	    (org-copilot-demo-start)
	    (with-current-buffer source (org-copilot-chat-send "answer"))
	    (with-temp-buffer
	      ;; Delivery must use the captured source, not the current buffer.
	      (org-copilot-demo-respond))
	    (should (equal before (with-current-buffer source (buffer-string))))
	    (should (file-exists-p (org-copilot-sidecar-path file)))
	    (should-not (file-exists-p (org-suggestions-sidecar-path file)))
	    (should-not (file-exists-p (org-comments-sidecar-path file)))
	    (with-selected-window (get-buffer-window source)
	      (goto-char (point-min))
	      (re-search-forward "^\\* Intro$")
	      (beginning-of-line)
	      (set-window-start (selected-window) (point)))
	    (with-current-buffer source (org-copilot-chat-send "edit"))
	    (with-temp-buffer (org-copilot-demo-respond))
	    (with-current-buffer source
	      (should org-copilot--overlays)
	      (should org-comments-overlays)
	      (with-current-buffer context-panels-side-panel-buffer
		(should (string-match-p "Make the Intro explicitly reviewable" (buffer-string)))))
	    (should (equal before (with-current-buffer source (buffer-string))))
	    (let* ((threads (org-suggestions-load-sidecar file))
		   (thread (car threads))
		   (candidate (car (plist-get thread :candidates)))
		   (comments (with-current-buffer source (org-copilot-visible-comments))))
	      (should (= 1 (length threads)))
	      (should (= 1 (length comments)))
	      (should (string-match-p "Bépo" (org-suggestions-candidate-diff source thread candidate)))
	      (with-current-buffer source
		(goto-char (plist-get (car comments) :source-start))
		(call-interactively #'org-copilot-chat-focus-next-comment)
		(call-interactively #'org-copilot-view-diff-at-point))
	      (with-current-buffer org-copilot-diff-buffer-name
		(should (string-match-p "-Alpha draft\\." (buffer-string)))
		(should-not (string-match-p "-\\* Intro" (buffer-string))))
	      (should (equal before (with-current-buffer source (buffer-string))))
	      (with-selected-window (get-buffer-window org-copilot-diff-buffer-name)
		(execute-kbd-macro (kbd "a")))
	      (with-current-buffer source
		(should (string-match-p "Bépo keeps edits reviewable" (buffer-string))))
	      (with-current-buffer source (org-copilot-demo-undo))
	      (should (equal before (with-current-buffer source (buffer-string)))))
	    (with-temp-buffer
	      (insert-file-contents file)
	      (setq buffer-file-name file)
	      (org-mode)
	      (should-not org-copilot-mode)
	      (org-copilot-restore-chat-messages)
	      (should (= 4 (length (org-copilot-chat-messages)))))))
      (dolist (buffer (buffer-list))
	(when (or (eq buffer source)
		  (and source
		       (eq (buffer-local-value 'context-panels-source-buffer buffer) source))
		  (let ((name (buffer-file-name buffer)))
		    (and name (file-in-directory-p name directory))))
	  (with-current-buffer buffer (set-buffer-modified-p nil))
	  (kill-buffer buffer)))
      (delete-directory directory t))))

(ert-deftest org-copilot-standalone-killing-source-closes-owned-views-not-sidecars ()
  "Visible sidecars do not keep a killed source's comments, chat, or diff alive."
  (let* ((directory (make-temp-file "copilot-source-lifetime-" t))
	 (file (expand-file-name "draft.org" directory))
	 (examples (expand-file-name "examples" (file-name-directory (locate-library "org-copilot"))))
	 (org-copilot-demo--pending nil)
	 source)
    (unwind-protect
	(save-window-excursion
	  (load (expand-file-name "org-copilot-demo.el" examples) nil t)
	  (copy-file (expand-file-name "standalone.org" examples) file)
	  (find-file file)
	  (setq source (current-buffer))
	  (org-copilot-demo-start)
	  (with-current-buffer source (org-copilot-chat-send "edit"))
	  (org-copilot-demo-respond)
	  (with-current-buffer source
	    (org-copilot-chat-focus-next-comment)
	    (org-copilot-view-diff-at-point))
	  (let* ((side (buffer-local-value 'context-panels-side-panel-buffer source))
		 (chat (buffer-local-value 'context-panels-bottom-panel-buffer source))
		 (diff (get-buffer org-copilot-diff-buffer-name))
		 (comments (find-file-noselect (org-comments-sidecar-path file)))
		 (transcript (find-file-noselect (org-copilot-sidecar-path file)))
		 (window (get-buffer-window source)))
	    (should (get-buffer-window diff))
	    (set-window-buffer window comments)
	    (set-window-buffer (split-window window nil 'right) transcript)
	    (kill-buffer source)
	    (context-panels-reconcile-windows)
	    (dolist (buffer (list side chat diff))
	      (should-not (get-buffer-window buffer t)))
	    (should (get-buffer-window comments))
	    (should (get-buffer-window transcript))
	    (should (file-exists-p (org-comments-sidecar-path file)))
	    (should (file-exists-p (org-copilot-sidecar-path file)))))
      (dolist (buffer (buffer-list))
	(when (or (eq buffer source)
		  (and source (eq (buffer-local-value 'context-panels-source-buffer buffer) source))
		  (let ((name (buffer-file-name buffer)))
		    (and name (file-in-directory-p name directory))))
	  (with-current-buffer buffer (set-buffer-modified-p nil))
	  (kill-buffer buffer)))
      (delete-directory directory t))))

(ert-deftest org-copilot-standalone-source-finalizer-leaves-retargeted-views-alone ()
  "Source cleanup must not close shared view buffers now owned by another source."
  (let ((other (generate-new-buffer " *copilot-other-owner*"))
	(views (list (generate-new-buffer " *copilot-owned-side*" )
		     (generate-new-buffer " *copilot-owned-chat*" )
		     (generate-new-buffer " *copilot-owned-diff*" )))
	closed)
    (unwind-protect
	(progn
	  (dolist (view views)
	    (with-current-buffer view (setq context-panels-source-buffer other)))
	  (with-temp-buffer
	    (org-mode)
	    (setq context-panels-side-panel-buffer (nth 0 views)
		  context-panels-bottom-panel-buffer (nth 1 views))
	    (let ((org-copilot-diff-buffer-name (buffer-name (nth 2 views))))
	      (cl-letf (((symbol-function 'context-panels-close)
			 (lambda () (push 'side closed)))
			((symbol-function 'context-panels-close-bottom-view)
			 (lambda () (push 'chat closed)))
			((symbol-function 'org-copilot--close-buffer-window)
			 (lambda (&rest _) (push 'diff closed))))
		(org-copilot--close-source-views))))
	  (should-not closed))
      (mapc #'kill-buffer (cons other views)))))

(provide 'org-copilot-standalone-test)
;;; org-copilot-standalone-test.el ends here
