;;; org-comments-panel-actions-test.el --- Panel action tests for org-comments -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for standalone Org comments panel actions.

;;; Code:

(require 'ert)
(require 'org-comments)

(defmacro org-comments-panel-actions-test--with-comment (&rest body)
  "Create a temporary source with one comment, then run BODY in its panel."
  (declare (indent 0))
  `(let* ((directory (make-temp-file "org-comments-panel-actions" t))
	  (source-file (expand-file-name "source.org" directory)))
     (unwind-protect
	 (progn
	   (with-temp-file source-file
	     (insert "Alpha selected text omega"))
	   (with-current-buffer (find-file-noselect source-file)
	     (org-mode)
	     (org-comments-append-to-sidecar
	      (org-comments-create-record buffer-file-name 7 20 "Review this." "c1" "Alice" "now"))
	     (org-comments-panel-open))
	   (with-current-buffer org-comments-panel-buffer-name
	     (goto-char (point-min))
	     (search-forward "Review this")
	     ,@body))
       (when (get-buffer org-comments-panel-buffer-name)
	 (kill-buffer org-comments-panel-buffer-name))
       (when-let* ((source-buffer (find-buffer-visiting source-file)))
	 (kill-buffer source-buffer))
       (delete-directory directory t))))

(ert-deftest org-comments-panel-actions-keymap-binds-help ()
  "Panel binds ? directly to package help-current-ui."
  (should (eq (lookup-key org-comments-panel-mode-map (kbd "?"))
	      #'org-comments-help-current-ui)))

(ert-deftest org-comments-panel-actions-keymap-binds-filters ()
  "Panel filter map binds generic filters to package current-UI commands."
  (should (eq (lookup-key org-comments-panel-filter-map (kbd "z"))
	      #'org-comments-filter-reset-current-ui))
  (should (eq (lookup-key org-comments-panel-filter-map (kbd "r"))
	      #'org-comments-filter-toggle-resolved-current-ui))
  (should (eq (lookup-key org-comments-panel-filter-map (kbd "?"))
	      #'org-comments-filter-status-current-ui)))

(ert-deftest org-comments-panel-actions-keymap-binds-close ()
  "Panel binds q directly to package close-current-ui."
  (should (eq (lookup-key org-comments-panel-mode-map (kbd "q"))
	      #'org-comments-close-current-ui)))

(ert-deftest org-comments-panel-actions-keymap-binds-jump ()
  "Panel binds RET to generic context-panel jump dispatch."
  (should (eq (lookup-key org-comments-panel-mode-map (kbd "RET"))
	      #'org-context-panel-jump-at-point)))

(ert-deftest org-comments-panel-actions-keymap-binds-delete ()
  "Panel binds d directly to package delete-at-point."
  (should (eq (lookup-key org-comments-panel-mode-map (kbd "d"))
	      #'org-comments-delete-at-point)))

(ert-deftest org-comments-panel-actions-keymap-binds-edit ()
  "Panel binds e directly to package edit-at-point."
  (should (eq (lookup-key org-comments-panel-mode-map (kbd "e"))
	      #'org-comments-edit-at-point)))

(ert-deftest org-comments-panel-actions-keymap-binds-reply ()
  "Panel binds r to the public DWIM reply command."
  (should (eq (lookup-key org-comments-panel-mode-map (kbd "r"))
	      #'org-comments-reply)))

(ert-deftest org-comments-panel-actions-keymap-binds-remote-open ()
  "Panel binds o/O to the public DWIM remote-open command."
  (should (eq (lookup-key org-comments-panel-mode-map (kbd "O"))
	      #'org-comments-open-remote))
  (should (eq (lookup-key org-comments-panel-mode-map (kbd "o"))
	      #'org-comments-open-remote)))

(ert-deftest org-comments-panel-actions-keymap-binds-push ()
  "Panel binds capital U to the public DWIM push command."
  (should (eq (lookup-key org-comments-panel-mode-map (kbd "U"))
	      #'org-comments-push)))

(ert-deftest org-comments-panel-actions-keymap-binds-navigation ()
  "Panel binds ]c/[c directly to package item navigation."
  (should (eq (lookup-key org-comments-panel-mode-map (kbd "]c"))
	      #'org-comments-next-item-at-point))
  (should (eq (lookup-key org-comments-panel-mode-map (kbd "[c"))
	      #'org-comments-previous-item-at-point)))

(ert-deftest org-comments-panel-actions-keymap-binds-page-open ()
  "Panel binds p directly to package page-open-at-point."
  (should (eq (lookup-key org-comments-panel-mode-map (kbd "p"))
	      #'org-comments-page-open-at-point)))

(ert-deftest org-comments-panel-actions-keymap-binds-pull-and-sync ()
  "Panel binds capital D and S to public DWIM remote actions."
  (should (eq (lookup-key org-comments-panel-mode-map (kbd "D"))
	      #'org-comments-pull))
  (should (eq (lookup-key org-comments-panel-mode-map (kbd "S"))
	      #'org-comments-sync)))

(ert-deftest org-comments-panel-actions-rendered-rows-carry-comment ()
  "Rendered panel rows expose the underlying comment record."
  (org-comments-panel-actions-test--with-comment
   (let ((comment (org-comments-panel-current-comment)))
     (should (equal (plist-get comment :id) "c1"))
     (should (plist-get comment :sidecar-file)))))

(ert-deftest org-comments-panel-actions-jump-opens-source-position ()
  "Jumping from a panel row opens the source at the comment target."
  (org-comments-panel-actions-test--with-comment
   (org-comments-jump-at-point)
   (should (derived-mode-p 'org-mode))
   (should (= (point) 7))))

(ert-deftest org-comments-panel-actions-jump-opens-stale-comment-sidecar ()
  "Jumping to a stale comment opens its sidecar heading."
  (org-comments-panel-actions-test--with-comment
   (let* ((comment (plist-put (copy-sequence (org-comments-panel-current-comment))
			      :anchor-state 'stale))
	  (org-comments-current-comment-function (lambda () comment)))
     (org-comments-jump-at-point)
     (should (string-suffix-p "source.comments.org" buffer-file-name))
     (should (equal "c1" (org-entry-get nil "ORG_COMMENTS_ID"))))))

(ert-deftest org-comments-panel-actions-jump-opens-page-comment-sidecar ()
  "Jumping to a page comment opens its sidecar heading."
  (org-comments-panel-actions-test--with-comment
   (let* ((comment (plist-put (copy-sequence (org-comments-panel-current-comment))
			      :page-comment t))
	  (org-comments-current-comment-function (lambda () comment)))
     (org-comments-jump-at-point)
     (should (string-suffix-p "source.comments.org" buffer-file-name))
     (should (equal "c1" (org-entry-get nil "ORG_COMMENTS_ID"))))))

(ert-deftest org-comments-panel-actions-navigation-uses-item-starts-adapter ()
  "Panel navigation delegates through current item starts adapter."
  (with-temp-buffer
    (insert "First\n\nSecond\n")
    (let ((first (point-min))
	  (second (save-excursion
		    (goto-char (point-min))
		    (search-forward "Second")
		    (match-beginning 0))))
      (setq-local org-comments-current-item-starts-function
		  (lambda () (list first second)))
      (goto-char first)
      (org-comments-next-item-at-point)
      (should (= (point) second))
      (org-comments-next-item-at-point)
      (should (= (point) first))
      (org-comments-previous-item-at-point)
      (should (= (point) second)))))

(ert-deftest org-comments-panel-actions-jump-at-point-uses-adapter ()
  "Jumping at point delegates through the current jump adapter."
  (org-comments-panel-actions-test--with-comment
   (let (jumped)
     (setq-local org-comments-current-jump-function
		 (lambda () (setq jumped t)))
     (org-comments-jump-at-point)
     (should jumped))))

(ert-deftest org-comments-panel-actions-edit-at-point-opens-sidecar-body ()
  "Editing at point opens the sidecar body for the current comment."
  (org-comments-panel-actions-test--with-comment
   (let ((comment (org-comments-panel-current-comment)))
     (setq-local org-comments-current-comment-function (lambda () comment))
     (org-comments-edit-at-point)
     (should (equal (buffer-file-name) (plist-get comment :sidecar-file)))
     (should (looking-at-p "Review this\\.")))))

(ert-deftest org-comments-panel-actions-help-current-ui-uses-adapter ()
  "Help command delegates through the current help adapter."
  (with-temp-buffer
    (let (shown)
      (setq-local org-comments-current-help-function
		  (lambda () (setq shown t)))
      (org-comments-help-current-ui)
      (should shown))))

(ert-deftest org-comments-panel-actions-filter-commands-use-adapters ()
  "Current-UI filter commands delegate through filter adapters."
  (with-temp-buffer
    (let (calls)
      (setq-local org-comments-current-filter-reset-function
		  (lambda () (push 'reset calls)))
      (setq-local org-comments-current-filter-toggle-resolved-function
		  (lambda () (push 'resolved calls)))
      (setq-local org-comments-current-filter-status-function
		  (lambda () (push 'status calls)))
      (org-comments-filter-reset-current-ui)
      (org-comments-filter-toggle-resolved-current-ui)
      (org-comments-filter-status-current-ui)
      (should (equal calls '(status resolved reset))))))

(ert-deftest org-comments-panel-actions-close-current-ui-uses-adapter ()
  "Closing current UI delegates through the current close adapter."
  (with-temp-buffer
    (let (closed)
      (setq-local org-comments-current-close-function
		  (lambda () (setq closed t)))
      (org-comments-close-current-ui)
      (should closed))))

(ert-deftest org-comments-panel-actions-page-open-at-point-uses-adapter ()
  "Page open at point delegates through the current page-open adapter."
  (org-comments-panel-actions-test--with-comment
   (let (opened)
     (setq-local org-comments-current-page-open-function
		 (lambda () (setq opened t)))
     (org-comments-page-open-at-point)
     (should opened))))

(ert-deftest org-comments-panel-actions-reply-at-point-uses-adapters ()
  "Replying at point delegates through the current reply adapter."
  (org-comments-panel-actions-test--with-comment
   (let ((comment (org-comments-panel-current-comment))
	 replied refreshed)
     (setq-local org-comments-current-comment-function (lambda () comment))
     (setq-local org-comments-current-reply-function
		 (lambda (current)
		   (setq replied current)
		   'reply-result))
     (setq-local org-comments-current-refresh-function
		 (lambda () (setq refreshed t)))
     (should (eq (org-comments-reply-at-point) 'reply-result))
     (should (eq replied comment))
     (should refreshed))))

(ert-deftest org-comments-panel-actions-current-refresh-uses-buffer-refresh-function ()
  "Default current-UI refresh honors buffer-local panel refresh functions."
  (with-temp-buffer
    (let (refreshed)
      (setq-local org-comments-panel-refresh-function
		  (lambda () (setq refreshed t)))
      (org-comments-refresh-current-ui)
      (should refreshed))))

(ert-deftest org-comments-panel-actions-delete-removes-comment ()
  "Deleting from a panel row removes the sidecar entry and refreshes the panel."
  (org-comments-panel-actions-test--with-comment
   (org-comments-panel-delete)
   (should-not (string-match-p "Review this" (buffer-string)))))

(ert-deftest org-comments-panel-actions-delete-at-point-uses-adapters ()
  "Generic delete command removes comments through action adapters."
  (org-comments-panel-actions-test--with-comment
   (let ((comment (org-comments-panel-current-comment))
	 refreshed)
     (setq-local org-comments-current-comment-function (lambda () comment))
     (setq-local org-comments-current-refresh-function
		 (lambda ()
		   (setq refreshed t)
		   (org-comments-panel-refresh)))
     (org-comments-delete-at-point)
     (should refreshed)
     (should-not (string-match-p "Review this" (buffer-string))))))

(ert-deftest org-comments-panel-actions-status-updates-comment ()
  "Status actions update the sidecar heading and refresh the panel."
  (org-comments-panel-actions-test--with-comment
   (org-comments-panel-mark-resolved)
   (should (string-match-p "\\[RESOLVED\\]" (buffer-string)))))

(ert-deftest org-comments-panel-actions-status-at-point-uses-adapters ()
  "Generic status command updates comments through action adapters."
  (org-comments-panel-actions-test--with-comment
   (let ((comment (org-comments-panel-current-comment))
	 refreshed)
     (setq-local org-comments-current-comment-function (lambda () comment))
     (setq-local org-comments-current-refresh-function
		 (lambda ()
		   (setq refreshed t)
		   (org-comments-panel-refresh)))
     (org-comments-mark-resolved-at-point)
     (should refreshed)
     (should (string-match-p "\\[RESOLVED\\]" (buffer-string))))))

(ert-deftest org-comments-panel-actions-pull-dispatches-for-source ()
  "Pull dispatches through the detected backend for the panel source."
  (org-comments-panel-actions-test--with-comment
   (let ((source-file (buffer-file-name org-comments-panel-source-buffer))
	 called)
     (cl-letf (((symbol-function 'org-comments-backend-detect)
		(lambda (&optional source-buffer)
		  (setq called (list :detect-buffer source-buffer))
		  'fake))
	       ((symbol-function 'org-comments-backend-pull)
		(lambda (id record)
		  (setq called (append called
				       (list :pull-id id :record record)))
		  '(:comments-imported 2))))
       (should (equal (org-comments-panel-pull) '(:comments-imported 2))))
     (should (eq (plist-get called :detect-buffer)
		 org-comments-panel-source-buffer))
     (should (eq (plist-get called :pull-id) 'fake))
     (should (equal (plist-get called :record)
		    (list :source-file source-file))))))

(ert-deftest org-comments-panel-actions-sync-dispatches-for-source ()
  "Sync dispatches through the detected backend for the panel source."
  (org-comments-panel-actions-test--with-comment
   (let ((source-file (buffer-file-name org-comments-panel-source-buffer))
	 called)
     (cl-letf (((symbol-function 'org-comments-backend-detect)
		(lambda (&optional source-buffer)
		  (setq called (list :detect-buffer source-buffer))
		  'fake))
	       ((symbol-function 'org-comments-backend-sync)
		(lambda (id record)
		  (setq called (append called
				       (list :sync-id id :record record)))
		  '(:comments-imported 2 :comments-pushed 1))))
       (should (equal (org-comments-panel-sync)
		      '(:comments-imported 2 :comments-pushed 1))))
     (should (eq (plist-get called :detect-buffer)
		 org-comments-panel-source-buffer))
     (should (eq (plist-get called :sync-id) 'fake))
     (should (equal (plist-get called :record)
		    (list :source-file source-file))))))

(ert-deftest org-comments-panel-actions-push-at-point-uses-adapters ()
  "Generic push command dispatches through buffer-local action adapters."
  (org-comments-panel-actions-test--with-comment
   (let ((source-file (buffer-file-name org-comments-panel-source-buffer))
	 called refreshed)
     (setq-local org-comments-current-comment-function
		 (lambda () '(:id "adapter-c1" :sidecar-file "comments.org")))
     (setq-local org-comments-current-source-buffer-function
		 (lambda () org-comments-panel-source-buffer))
     (setq-local org-comments-current-refresh-function
		 (lambda () (setq refreshed t)))
     (cl-letf (((symbol-function 'org-comments-backend-detect)
		(lambda (&optional source-buffer)
		  (setq called (list :detect-buffer source-buffer))
		  'fake))
	       ((symbol-function 'org-comments-backend-push)
		(lambda (id comment)
		  (setq called (append called
				       (list :push-id id :comment comment)))
		  '(:pushed t))))
       (should (equal (org-comments-push-at-point) '(:pushed t))))
     (should refreshed)
     (should (eq (plist-get called :detect-buffer)
		 org-comments-panel-source-buffer))
     (should (eq (plist-get called :push-id) 'fake))
     (should (equal (plist-get (plist-get called :comment) :id) "adapter-c1"))
     (should (equal (plist-get (plist-get called :comment) :source-file)
		    source-file)))))

(ert-deftest org-comments-panel-actions-push-dispatches-for-row ()
  "Push dispatches through the detected backend for the current row."
  (org-comments-panel-actions-test--with-comment
   (let ((source-file (buffer-file-name org-comments-panel-source-buffer))
	 called)
     (cl-letf (((symbol-function 'org-comments-backend-detect)
		(lambda (&optional source-buffer)
		  (setq called (list :detect-buffer source-buffer))
		  'fake))
	       ((symbol-function 'org-comments-backend-push)
		(lambda (id comment)
		  (setq called (append called
				       (list :push-id id :comment comment)))
		  '(:pushed t))))
       (should (equal (org-comments-panel-push) '(:pushed t))))
     (should (eq (plist-get called :detect-buffer)
		 org-comments-panel-source-buffer))
     (should (eq (plist-get called :push-id) 'fake))
     (should (equal (plist-get (plist-get called :comment) :id) "c1"))
     (should (equal (plist-get (plist-get called :comment) :source-file)
		    source-file)))))

(ert-deftest org-comments-panel-actions-open-remote-at-point-uses-adapters ()
  "Generic remote-open command dispatches through action adapters."
  (org-comments-panel-actions-test--with-comment
   (let ((source-file (buffer-file-name org-comments-panel-source-buffer))
	 called)
     (setq-local org-comments-current-comment-function
		 (lambda () '(:id "adapter-c1" :remote-id "r1")))
     (setq-local org-comments-current-source-buffer-function
		 (lambda () org-comments-panel-source-buffer))
     (cl-letf (((symbol-function 'org-comments-backend-detect)
		(lambda (&optional source-buffer)
		  (setq called (list :detect-buffer source-buffer))
		  'fake))
	       ((symbol-function 'org-comments-backend-open-remote)
		(lambda (id comment)
		  (setq called (append called
				       (list :open-id id :comment comment)))
		  "remote-url")))
       (should (equal (org-comments-open-remote-at-point) "remote-url")))
     (should (eq (plist-get called :detect-buffer)
		 org-comments-panel-source-buffer))
     (should (eq (plist-get called :open-id) 'fake))
     (should (equal (plist-get (plist-get called :comment) :id) "adapter-c1"))
     (should (equal (plist-get (plist-get called :comment) :source-file)
		    source-file)))))

(ert-deftest org-comments-panel-actions-open-remote-dispatches-for-row ()
  "Remote-open dispatches through the detected backend for the current row."
  (org-comments-panel-actions-test--with-comment
   (let ((source-file (buffer-file-name org-comments-panel-source-buffer))
	 called)
     (cl-letf (((symbol-function 'org-comments-backend-detect)
		(lambda (&optional source-buffer)
		  (setq called (list :detect-buffer source-buffer))
		  'fake))
	       ((symbol-function 'org-comments-backend-open-remote)
		(lambda (id comment)
		  (setq called (append called
				       (list :open-id id :comment comment)))
		  "https://example.test/comment")))
       (should (equal (org-comments-panel-open-remote)
		      "https://example.test/comment")))
     (should (eq (plist-get called :detect-buffer)
		 org-comments-panel-source-buffer))
     (should (eq (plist-get called :open-id) 'fake))
     (should (equal (plist-get (plist-get called :comment) :id) "c1"))
     (should (equal (plist-get (plist-get called :comment) :source-file)
		    source-file)))))

(provide 'org-comments-panel-actions-test)
;;; org-comments-panel-actions-test.el ends here
