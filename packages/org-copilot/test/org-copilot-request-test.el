;;; org-copilot-request-test.el --- Request lifecycle regressions -*- lexical-binding: t; -*-

;;; Commentary:
;; Synthetic documents exercise real chat/review callbacks and durable stores.
;; Only the model transport, idle scheduling and panel display are controlled.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org-copilot)
(require 'org-copilot-gptel)

(defconst org-copilot-request-test--edit
  "{\"intent\":\"edit\",\"message\":\"Proposal ready.\",\"suggestion_threads\":[{\"summary\":\"Rewrite Intro\",\"suggestions\":[{\"hunks\":[{\"kind\":\"section-replace\",\"primary\":true,\"section_title\":\"Intro\",\"replacement\":\"Proposed body.\"}]}]}],\"comments\":[{\"body\":\"Clarify.\",\"target_text\":\"Original body.\"}]}"
  "Controlled reply containing both a comment and an executable proposal.")

(defconst org-copilot-request-test--review
  "{\"summary\":{\"message\":\"Review ready.\"},\"comments\":[{\"id\":\"review-1\",\"body\":\"Clarify.\",\"target_text\":\"Original body.\"}]}"
  "Controlled review with a durably anchored comment.")

(defmacro org-copilot-request-test--with-source (&rest body)
  "Run BODY with a synthetic source and captured transport callbacks."
  (declare (indent 0) (debug t))
  `(let* ((directory (make-temp-file "copilot-request-test" t))
	  (source-file (expand-file-name "draft.org" directory))
	  (source (find-file-noselect source-file))
	  (org-copilot-chat-function #'org-copilot-gptel-chat)
	  (org-copilot-review-function #'org-copilot-gptel-review)
	  (org-copilot-chat-open-panel-on-comments nil)
	  (org-copilot-gptel-defer-chat-postprocessing nil)
	  (real-idle (symbol-function 'run-with-idle-timer))
	  (real-write (symbol-function 'write-region))
	  callbacks deferred transports scheduled)
     (unwind-protect
	 (save-window-excursion
	   (cl-letf (((symbol-function 'write-region)
		      (lambda (&rest args)
			;; Exercise CI's native supersession refusal even on
			;; builds that do not check other visiting buffers here.
			(let* ((file (nth 2 args))
			       (visitor (and (equal file (org-copilot-sidecar-path source-file))
					     (find-buffer-visiting file))))
			  (when (and visitor (not (verify-visited-file-modtime visitor)))
			    (ask-user-about-supersession-threat file)))
			(apply real-write args)))
		     ((symbol-function 'gptel-request)
		      (lambda (_prompt &rest args)
			(setq callbacks (append callbacks (list (plist-get args :callback))))
			(push (plist-get args :buffer) transports)))
		     ((symbol-function 'org-copilot-gptel--stream-required-p) #'ignore)
		     ((symbol-function 'org-copilot-open) #'ignore)
		     ((symbol-function 'org-copilot-gptel--open-general-chat) #'ignore)
		     ((symbol-function 'run-with-idle-timer)
		      (lambda (seconds repeat function &rest args)
			(if (equal seconds 0)
			    (let ((timer (apply real-idle 3600 repeat function args)))
			      (push (lambda () (apply function args)) deferred)
			      (push timer scheduled)
			      timer)
			  (apply real-idle seconds repeat function args)))))
	     (with-current-buffer source
	       (org-mode)
	       (insert "* Intro\nOriginal body.\n* Other\nOther body.\n")
	       (save-buffer)
	       (goto-char (point-min))
	       ,@body)))
       (mapc #'cancel-timer scheduled)
       (dolist (buffer (buffer-list))
	 (when (or (eq buffer (get-buffer org-copilot-chat-buffer-name))
		   (and (buffer-file-name buffer)
			(string-prefix-p directory (buffer-file-name buffer))))
	   (with-current-buffer buffer (set-buffer-modified-p nil))
	   (kill-buffer buffer)))
       (delete-directory directory t))))

(defun org-copilot-request-test--stamp-transcript (source-file)
  "Give SOURCE-FILE's visited transcript a distinct, consistent old timestamp.
The next real write must update both the file and its visiting buffer."
  (let ((file (org-copilot-sidecar-path source-file)))
    (when (file-exists-p file)
      (with-current-buffer (find-file-noselect file)
	(set-file-times file (seconds-to-time 1000000000))
	(set-visited-file-modtime)))))

(defun org-copilot-request-test--assert-empty (source-file)
  "Assert no conversation or artifacts were recreated for SOURCE-FILE."
  (should-not (org-copilot-chat-messages))
  (should-not (org-copilot-sidecar-load-messages source-file))
  (should-not (org-comments-collect (current-buffer) t))
  (should-not (org-suggestions-load-sidecar source-file)))

(ert-deftest org-copilot-request-reset-fences-delayed-chat ()
  "Archive, UI clear and erase all fence delayed chat artifacts."
  (dolist (reset '(archive ui erase replace))
    (org-copilot-request-test--with-source
     (org-copilot-chat-send "Rewrite Intro")
     (pcase reset
       ('archive (org-copilot-clear-session))
       ('ui (org-copilot-clear-session t))
       ('erase (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
		 (org-copilot-erase-session)))
       ('replace (org-copilot-set-chat-messages nil)))
     (let ((before (buffer-string))
	   (durable-before (org-copilot-sidecar-load-messages source-file)))
       (funcall (car callbacks) org-copilot-request-test--edit '(:status success))
       (should (equal (buffer-string) before))
       (should-not (org-copilot-chat-messages))
       (should (equal (org-copilot-sidecar-load-messages source-file) durable-before))
       (should-not (org-comments-collect source t))
       (should-not (org-suggestions-load-sidecar source-file))))))

(ert-deftest org-copilot-request-reset-fences-delayed-review ()
  "Review completion cannot recreate erased conversation or comments."
  (dolist (erase '(nil t))
    (org-copilot-request-test--with-source
     (org-copilot-review-dwim)
     (if erase
	 (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
	   (org-copilot-erase-session))
       (org-copilot-clear-session))
     (funcall (car callbacks) org-copilot-request-test--review '(:status success))
     (org-copilot-request-test--assert-empty source-file))))

(ert-deftest org-copilot-request-current-controls ()
  "Current chat/review complete durably without mutating the Org source."
  (dolist (review '(nil t))
    (org-copilot-request-test--with-source
     (let ((before (buffer-string)))
       (if review (org-copilot-review-dwim)
	 (org-copilot-chat-send "Rewrite Intro"))
       (funcall (car callbacks)
		(if review org-copilot-request-test--review org-copilot-request-test--edit)
		'(:status success))
       (should (equal (buffer-string) before))
       (should (org-copilot-sidecar-load-messages source-file))
       (should (org-comments-collect source t))
       (unless review (should (org-suggestions-load-sidecar source-file)))
       (should-not (cl-find 'pending (org-copilot-chat-messages)
			    :key (lambda (message) (plist-get message :role))))))))

(ert-deftest org-copilot-request-new-session-survives-old-errors ()
  "An obsolete chat/review error cannot remove a new session's pending state."
  (dolist (review '(nil t))
    (org-copilot-request-test--with-source
     (if review (org-copilot-review-dwim) (org-copilot-chat-send "Old request"))
     (org-copilot-clear-session)
     (org-copilot-request-test--stamp-transcript source-file)
     (org-copilot-chat-send "New request")
     (let ((before (copy-tree (org-copilot-chat-messages))))
       (funcall (car callbacks) nil '(:status "HTTP/2 503" :error "Old failure"))
       (should (equal before (org-copilot-chat-messages))))
     (funcall (cadr callbacks) "New answer" '(:status success))
     (should (equal (mapcar (lambda (m) (plist-get m :content))
			    (org-copilot-chat-messages))
		    '("New request" "New answer")))
     (should-not org-copilot--requests))))

(ert-deftest org-copilot-request-overlap-completes-only-own-pending ()
  "Same-context overlap handles completion/error out of order and exactly once."
  (org-copilot-request-test--with-source
   (org-copilot-chat-send "First")
   (org-copilot-chat-send "Second")
   (let* ((pending (cl-remove-if-not
		    (lambda (m) (eq (plist-get m :role) 'pending))
		    (org-copilot-chat-messages)))
	  (second (plist-get (cadr pending) :request-id)))
     (should second)
     (should-not (eq (plist-get (car pending) :request-id) second))
     (funcall (car callbacks) "First answer" '(:status success))
     (should (eq (plist-get
		  (cl-find 'pending (org-copilot-chat-messages)
			   :key (lambda (m) (plist-get m :role))) :request-id)
		 second))
     (funcall (cadr callbacks) nil '(:status "HTTP/2 503" :error "Second failure"))
     (let ((messages (copy-tree (org-copilot-chat-messages))))
       (funcall (car callbacks) "Duplicate" '(:status success))
       (funcall (cadr callbacks) "Too late" '(:status success))
       (should (equal messages (org-copilot-chat-messages)))))
   (should-not org-copilot--requests)
   (should-not (cl-find 'pending (org-copilot-chat-messages)
			:key (lambda (m) (plist-get m :role))))))

(ert-deftest org-copilot-request-deferred-reset-fences-artifacts ()
  "Reset after initial display fences the already-scheduled installation."
  (dolist (reset '(archive erase replace retarget close cancel))
    (org-copilot-request-test--with-source
     (let ((org-copilot-gptel-defer-chat-postprocessing t))
       (org-copilot-chat-send "Rewrite Intro")
       (funcall (car callbacks) org-copilot-request-test--edit '(:status success))
       (should deferred)
       (let ((token (car org-copilot--requests)))
	 (pcase reset
	   ('archive (org-copilot-clear-session))
	   ('erase (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
		     (org-copilot-erase-session)))
	   ('replace (org-copilot-set-chat-messages nil))
	   ('retarget (set-visited-file-name (expand-file-name "other.org" directory) t))
	   ('close (kill-buffer source))
	   ('cancel (org-copilot-cancel-request (list :request-token token))))
	 (let ((before (org-copilot-sidecar-load-messages source-file)))
	   (funcall (pop deferred))
	   (should (equal before (org-copilot-sidecar-load-messages source-file)))
	   (should-not (org-suggestions-load-sidecar source-file))
	   (should-not (file-exists-p (org-comments-sidecar-path source-file)))))))))

(ert-deftest org-copilot-request-deferred-overlap-updates-origin-message ()
  "Out-of-order deferred footers update their own messages, not the last one."
  (org-copilot-request-test--with-source
   (let ((org-copilot-gptel-defer-chat-postprocessing t))
     (org-copilot-chat-send "First")
     (org-copilot-chat-send "Second")
     (funcall (car callbacks)
	      "{\"message\":\"First answer\",\"comments\":[{\"body\":\"Clarify\",\"target_text\":\"Original body.\"}]}"
	      '(:status success))
     (funcall (cadr callbacks) "Second answer" '(:status success))
     ;; Second postprocess first, then the first response's installation footer.
     (mapc #'funcall deferred)
     (let ((answers (cl-remove-if-not
		     (lambda (m) (eq (plist-get m :role) 'assistant))
		     (org-copilot-chat-messages))))
       (should (string-match-p "First answer.*" (plist-get (car answers) :content)))
       (should (string-match-p "Installed 1 comment" (plist-get (car answers) :content)))
       (should (equal "Second answer" (plist-get (cadr answers) :content))))
     (should-not org-copilot--requests))))

(ert-deftest org-copilot-request-source-lifecycle-fences-completion ()
  "Closing or retargeting a source fences both chat and review completions."
  (dolist (review '(nil t))
    (dolist (close '(nil t))
      (org-copilot-request-test--with-source
       (if review (org-copilot-review-dwim) (org-copilot-chat-send "Old request"))
       (let ((before (org-copilot-sidecar-load-messages source-file)))
	 (if close (kill-buffer source)
	   (set-visited-file-name (expand-file-name "other.org" directory) t))
	 (funcall (car callbacks)
		  (if review org-copilot-request-test--review org-copilot-request-test--edit)
		  '(:status success))
	 (funcall (car callbacks) nil '(:status failure :error "Obsolete error"))
	 (should (equal before (org-copilot-sidecar-load-messages source-file)))
	 (should-not (file-exists-p (org-comments-sidecar-path source-file)))
	 (should-not (file-exists-p (org-suggestions-sidecar-path source-file)))
	 (unless close
	   (should-not (file-exists-p (org-comments-sidecar-path buffer-file-name)))
	   (org-copilot-chat-send "Retargeted request")
	   (funcall (cadr callbacks) "Retargeted answer" '(:status success))
	   (should (org-copilot-sidecar-load-messages buffer-file-name))))))))

(ert-deftest org-copilot-request-focus-navigation-keeps-captured-context ()
  "Navigating to another section does not redirect a valid pending reply."
  (org-copilot-request-test--with-source
   (let ((intro (org-copilot-chat--section-context-at-point)))
     (org-copilot-chat--set-context source intro)
     (org-copilot-chat-send "Explain Intro")
     (goto-char (point-max))
     (org-copilot-chat--set-context source (org-copilot-chat--section-context-at-point))
     (org-copilot-chat-send "Explain Other")
     (funcall (cadr callbacks) "Other answer" '(:status success))
     (funcall (car callbacks) "Intro answer" '(:status success))
     (should (equal (plist-get org-copilot-chat-context :section-title) "Other"))
     (let ((intro-id (org-copilot-chat--context-id intro))
	   (other-id (org-copilot-chat--context-id org-copilot-chat-context)))
       ;; Request ownership is independent of how the UI filters conversation.
       (should (equal (mapcar (lambda (m)
				(cons (plist-get m :content) (plist-get m :context-id)))
			      (org-copilot-chat-messages))
		      (list (cons "Explain Intro" intro-id)
			    (cons "Explain Other" other-id)
			    (cons "Other answer" other-id)
			    (cons "Intro answer" intro-id))))))))

(ert-deftest org-copilot-request-cancel-isolated-and-independent-of-provider ()
  "Cancellation uses the supported buffer API and fences even failed aborts."
  (org-copilot-request-test--with-source
   (org-copilot-chat-send "First")
   (let ((first (car org-copilot--requests)))
     (org-copilot-chat-send "Second")
     (should-not (eq (car transports) (cadr transports)))
     (should-not (memq source transports))
     (cl-letf (((symbol-function 'gptel-abort)
		(lambda (buffer)
		  (should (eq buffer (cadr transports)))
		  (funcall (car callbacks) "Abort raced with completion" '(:status success))
		  (error "Synthetic abort failure"))))
       (org-copilot-cancel-request (list :request-token first)))
     (should-not (buffer-live-p (cadr transports)))
     (should (buffer-live-p (car transports)))
     (funcall (car callbacks) org-copilot-request-test--edit '(:status success))
     (funcall (cadr callbacks) "Second answer" '(:status success))
     (should-not (buffer-live-p (car transports)))
     (should (equal (mapcar (lambda (m) (plist-get m :content))
			    (org-copilot-chat-messages))
		    '("First" "Second" "Second answer")))
     (should-not (org-comments-collect source t))
     (should-not (org-suggestions-load-sidecar source-file)))))

(ert-deftest org-copilot-request-synchronous-callback-has-no-ghost-pending ()
  "Immediate adapters cannot complete before their pending state exists."
  (org-copilot-request-test--with-source
   (cl-letf (((symbol-function 'gptel-request)
	      (lambda (_prompt &rest args)
		(funcall (plist-get args :callback) "Immediate answer" '(:status success)))))
     (org-copilot-chat-send "Immediate question"))
   (should (equal (mapcar (lambda (m) (plist-get m :content))
			  (org-copilot-chat-messages))
		  '("Immediate question" "Immediate answer")))
   (should-not org-copilot--requests)))

(ert-deftest org-copilot-request-transport-keeps-effective-provider-options ()
  "The cancellation target retains source-local and adapter-overridden options."
  (org-copilot-request-test--with-source
   (setq-local gptel-model 'source-model gptel-backend 'source-backend
	       gptel-temperature 0.42)
   (let ((org-copilot-gptel-model 'override-model)
	 (org-copilot-gptel-backend 'override-backend))
     (cl-letf (((symbol-function 'gptel-request)
		(lambda (_prompt &rest args)
		  (let ((buffer (plist-get args :buffer)))
		    (should (eq (buffer-local-value 'gptel-model buffer) 'override-model))
		    (should (eq (buffer-local-value 'gptel-backend buffer) 'override-backend))
		    (should (= (buffer-local-value 'gptel-temperature buffer) 0.42))))))
       (org-copilot-chat-send "Provider control")))))

(ert-deftest org-copilot-request-review-does-not-steal-focused-context ()
  "A completed review does not change a section selected while waiting."
  (org-copilot-request-test--with-source
   (org-copilot-review-dwim)
   (goto-char (point-max))
   (org-copilot-chat--set-context source (org-copilot-chat--section-context-at-point))
   (funcall (car callbacks) org-copilot-request-test--review '(:status success))
   (should (equal (plist-get org-copilot-chat-context :section-title) "Other"))))

(ert-deftest org-copilot-request-retarget-keeps-transcripts-separate ()
  "Retargeting the source does not carry its old conversation into another file."
  (org-copilot-request-test--with-source
   (org-copilot-chat-send "Old request")
   (set-visited-file-name (expand-file-name "other.org" directory) t)
   (org-copilot-chat-send "New request")
   (funcall (cadr callbacks) "New answer" '(:status success))
   (should (equal (mapcar (lambda (m) (plist-get m :content))
			  (org-copilot-chat-messages))
		  '("New request" "New answer")))))

(ert-deftest org-copilot-request-overlap-reverse-completion ()
  "A later request may finish first without consuming the older pending state."
  (org-copilot-request-test--with-source
   (org-copilot-chat-send "First")
   (let ((first (car org-copilot--requests)))
     (org-copilot-chat-send "Second")
     (funcall (cadr callbacks) "Second answer" '(:status success))
     (should (eq first
		 (plist-get (cl-find 'pending (org-copilot-chat-messages)
				     :key (lambda (m) (plist-get m :role))) :request-id)))
     (funcall (car callbacks) "First answer" '(:status success))
     (should (equal (mapcar (lambda (m) (plist-get m :content))
			    (org-copilot-chat-messages))
		    '("First" "Second" "Second answer" "First answer"))))))

(ert-deftest org-copilot-request-source-reset-leaves-independent-source-working ()
  "A reset cannot cancel another source's valid request or retarget its chat."
  (org-copilot-request-test--with-source
   (org-copilot-chat-send "Source A")
   (let ((other (find-file-noselect (expand-file-name "other.org" directory))))
     (with-current-buffer other
       (org-mode)
       (insert "* Other\nIndependent source.\n")
       (save-buffer)
       (org-copilot-chat-send "Source B"))
     (org-copilot-clear-session)
     (funcall (car callbacks) org-copilot-request-test--edit '(:status success))
     (funcall (cadr callbacks) "Independent answer" '(:status success))
     (org-copilot-request-test--assert-empty source-file)
     (with-current-buffer other
       (should (equal (mapcar (lambda (m) (plist-get m :content))
			      (org-copilot-chat-messages))
		      '("Source B" "Independent answer"))))
     (with-current-buffer (get-buffer org-copilot-chat-buffer-name)
       (should (eq org-copilot-chat-source-buffer other))))))

(ert-deftest org-copilot-request-stream-reset-fences-final-and-error ()
  "Partial streaming text cannot install after reset, even on late final/error."
  (org-copilot-request-test--with-source
   (cl-letf (((symbol-function 'org-copilot-gptel--stream-required-p) (lambda (&rest _) t)))
     (org-copilot-chat-send "Old stream")
     (funcall (car callbacks) "{\"message\":\"Old " '(:status streaming))
     (org-copilot-clear-session)
     (org-copilot-request-test--stamp-transcript source-file)
     (org-copilot-chat-send "New stream")
     (funcall (car callbacks) "answer\"}" '(:status streaming))
     (funcall (car callbacks) t '(:status success))
     (funcall (car callbacks) nil '(:status failure :error "Late failure"))
     (funcall (cadr callbacks) "{\"message\":\"New answer\"}" '(:status streaming))
     (funcall (cadr callbacks) t '(:status success))
     (should (equal (mapcar (lambda (m) (plist-get m :content))
			    (org-copilot-chat-messages))
		    '("New stream" "New answer"))))))

(ert-deftest org-copilot-request-cancel-command-preserves-artifacts ()
  "The source/chat cancellation command fences work without erasing history."
  (org-copilot-request-test--with-source
   (org-copilot-chat-send "Keep this question")
   (with-current-buffer (get-buffer org-copilot-chat-buffer-name)
     (org-copilot-cancel-requests))
   (funcall (car callbacks) org-copilot-request-test--edit '(:status success))
   (should (equal (mapcar (lambda (m) (plist-get m :content))
			  (org-copilot-chat-messages))
		  '("Keep this question")))
   (should (org-copilot-sidecar-load-messages source-file))
   (should-not org-copilot--requests)))

(provide 'org-copilot-request-test)
;;; org-copilot-request-test.el ends here
