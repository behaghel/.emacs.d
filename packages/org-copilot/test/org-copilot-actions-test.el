;;; org-copilot-actions-test.el --- Action tests for org-copilot -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for accepting and dismissing Org Copilot AI comments.

;;; Code:

(require 'ert)
(require 'org)
(require 'org-copilot)
(require 'org-copilot-diff)

(ert-deftest org-copilot-accept-replaces-valid-target-text ()
  "Accepting a valid suggestion replaces the reviewed source text."
  (with-temp-buffer
    (org-mode)
    (insert "Alpha sentence.\n")
    (let ((comment (org-copilot-add-comment
		    (list :id "ai-1"
			  :source-start (point-min)
			  :source-end (+ (point-min) (length "Alpha sentence."))
			  :target-text "Alpha sentence."
			  :suggestion "Alpha."
			  :status 'active))))
      (org-copilot-accept-comment comment (current-buffer))
      (should (equal (buffer-string) "Alpha.\n")))))

(ert-deftest org-copilot-accept-stores-rollback-metadata ()
  "Accepting stores enough metadata to undo the change."
  (with-temp-buffer
    (org-mode)
    (insert "Alpha sentence.\n")
    (let ((comment (org-copilot-add-comment
		    (list :id "ai-1"
			  :source-start (point-min)
			  :source-end (+ (point-min) (length "Alpha sentence."))
			  :target-text "Alpha sentence."
			  :suggestion "Alpha."
			  :status 'active))))
      (org-copilot-accept-comment comment (current-buffer))
      (let ((accepted (org-copilot-find-comment "ai-1")))
	(should (equal (plist-get accepted :original-target-text)
		       "Alpha sentence."))
	(should (equal (plist-get accepted :accepted-text) "Alpha."))))))

(ert-deftest org-copilot-accept-recovers-shifted-target-text ()
  "Accepting recovers unique target text when stored positions drift."
  (with-temp-buffer
    (org-mode)
    (insert "Prefix. Alpha sentence.\n")
    (let ((comment (org-copilot-add-comment
		    (list :id "ai-1"
			  :source-start (point-min)
			  :source-end (+ (point-min) (length "Alpha sentence."))
			  :target-text "Alpha sentence."
			  :suggestion "Alpha."
			  :status 'active))))
      (org-copilot-accept-comment comment (current-buffer))
      (should (equal (buffer-string) "Prefix. Alpha.\n"))
      (let ((accepted (org-copilot-find-comment "ai-1")))
	(should (eq (plist-get accepted :status) 'accepted))
	(should (equal (plist-get accepted :source-start)
		       (+ (point-min) (length "Prefix. "))))))))

(ert-deftest org-copilot-accept-recovers-unanchored-target-text ()
  "Accepting can anchor an unpositioned comment by unique target text."
  (with-temp-buffer
    (org-mode)
    (insert "Alpha sentence.\n")
    (let ((comment (org-copilot-add-comment
		    (list :id "ai-1"
			  :target-text "Alpha sentence."
			  :suggestion "Alpha."
			  :status 'active))))
      (org-copilot-accept-comment comment (current-buffer))
      (should (equal (buffer-string) "Alpha.\n"))
      (should (eq (plist-get (org-copilot-find-comment "ai-1") :status)
		  'accepted)))))

(ert-deftest org-copilot-accept-section-suggestion-replaces-current-body ()
  "Accepting a section suggestion replaces the current body despite edits."
  (with-temp-buffer
    (org-mode)
    (insert "* Intro\nEdited body.\n")
    (let ((comment (org-copilot-add-comment
		    (list :id "ai-section-1"
			  :type 'scope
			  :source-start 9
			  :source-end 20
			  :target-text "Original body.\n"
			  :suggestion "New body.\n"
			  :section-title "Intro"))))
      (org-copilot-accept-comment comment (current-buffer))
      (should (equal (buffer-string) "* Intro\nNew body.\n"))
      (should (eq (plist-get (org-copilot-find-comment "ai-section-1")
			     :status)
		  'accepted)))))

(ert-deftest org-copilot-accept-insertion-inserts-suggestion-at-anchor ()
  "Accepting an insertion comment inserts its suggestion at the anchor point."
  (with-temp-buffer
    (org-mode)
    (insert "Intro.\nConclusion.\n")
    (let ((comment (org-copilot-add-comment
		    (list :id "ai-1"
			  :type 'insertion
			  :source-start 7
			  :source-end 7
			  :anchor-text "Intro."
			  :placement 'after
			  :suggestion "\nBridge."))))
      (org-copilot-accept-comment comment (current-buffer))
      (should (equal (buffer-string) "Intro.\nBridge.\nConclusion.\n"))
      (should (eq (plist-get (org-copilot-find-comment "ai-1") :status)
		  'accepted)))))

(ert-deftest org-copilot-accept-marks-comment-accepted ()
  "Accepting a valid suggestion marks the AI comment accepted."
  (with-temp-buffer
    (org-mode)
    (insert "Alpha sentence.\n")
    (let ((comment (org-copilot-add-comment
		    (list :id "ai-1"
			  :source-start (point-min)
			  :source-end (+ (point-min) (length "Alpha sentence."))
			  :target-text "Alpha sentence."
			  :suggestion "Alpha."
			  :status 'active))))
      (org-copilot-accept-comment comment (current-buffer))
      (should (eq (plist-get (car (org-copilot-comments)) :status) 'accepted)))))

(ert-deftest org-copilot-accept-refuses-and-stales-on-target-mismatch ()
  "Accepting a mismatched suggestion marks it stale without changing source."
  (with-temp-buffer
    (org-mode)
    (insert "Changed sentence.\n")
    (let ((comment (org-copilot-add-comment
		    (list :id "ai-1"
			  :source-start (point-min)
			  :source-end (line-end-position)
			  :target-text "Alpha sentence."
			  :suggestion "Alpha."
			  :status 'active))))
      (should-error (org-copilot-accept-comment comment (current-buffer))
		    :type 'user-error)
      (should (equal (buffer-string) "Changed sentence.\n"))
      (should (eq (plist-get (car (org-copilot-comments)) :status) 'stale)))))

(ert-deftest org-copilot-accept-at-point-uses-latest-model-suggestion ()
  "Accepting from stale panel metadata uses the latest session suggestion."
  (let ((source (generate-new-buffer " *org copilot action source*")))
    (unwind-protect
	(with-current-buffer source
	  (org-mode)
	  (insert "Alpha sentence.\n")
	  (org-copilot-add-comment
	   (list :id "ai-1"
		 :source-start (point-min)
		 :source-end (+ (point-min) (length "Alpha sentence."))
		 :target-text "Alpha sentence."
		 :suggestion "New suggestion."
		 :status 'active))
	  (with-temp-buffer
	    (org-copilot-panel-mode)
	    (setq org-context-panel-source-buffer source)
	    (let ((stale-item (list :id "ai-1"
				    :source-start 1
				    :source-end 16
				    :target-text "Alpha sentence."
				    :suggestion "Old suggestion."
				    :status 'active)))
	      (let ((inhibit-read-only t))
		(insert "💬 stale row\n")
		(add-text-properties (point-min) (point-max)
				     `(org-context-panel-item ,stale-item)))
	      (goto-char (point-min))
	      (org-copilot-accept-at-point)))
	  (should (equal (buffer-string) "New suggestion.\n")))
      (when (buffer-live-p source)
	(kill-buffer source)))))

(ert-deftest org-copilot-accept-from-diff-uses-latest-model-suggestion ()
  "Accepting from stale diff metadata uses the latest session suggestion."
  (let ((source (generate-new-buffer " *org copilot action source*")))
    (unwind-protect
	(with-current-buffer source
	  (org-mode)
	  (insert "Alpha sentence.\n")
	  (let ((old-comment (org-copilot-add-comment
			      (list :id "ai-1"
				    :source-start (point-min)
				    :source-end (+ (point-min) (length "Alpha sentence."))
				    :target-text "Alpha sentence."
				    :suggestion "Old suggestion."
				    :status 'active))))
	    (org-copilot-update-comment
	     (plist-put (copy-sequence old-comment) :suggestion "New suggestion."))
	    (with-current-buffer (org-copilot-diff-open source old-comment)
	      (org-copilot-accept-at-point)))
	  (should (equal (buffer-string) "New suggestion.\n")))
      (when (buffer-live-p source)
	(kill-buffer source)))))

(ert-deftest org-copilot-undo-accepted-restores-original-text ()
  "Undoing an accepted suggestion restores original source text."
  (with-temp-buffer
    (org-mode)
    (insert "Alpha sentence.\n")
    (let ((comment (org-copilot-add-comment
		    (list :id "ai-1"
			  :source-start (point-min)
			  :source-end (+ (point-min) (length "Alpha sentence."))
			  :target-text "Alpha sentence."
			  :suggestion "Alpha."
			  :status 'active))))
      (org-copilot-accept-comment comment (current-buffer))
      (org-copilot-undo-accepted-comment
       (org-copilot-find-comment "ai-1")
       (current-buffer))
      (should (equal (buffer-string) "Alpha sentence.\n"))
      (let ((active (org-copilot-find-comment "ai-1")))
	(should (eq (plist-get active :status) 'active))
	(should (equal (plist-get active :target-text) "Alpha sentence."))))))

(ert-deftest org-copilot-undo-refuses-stale-accepted-text ()
  "Undo refuses when accepted source text was edited later."
  (with-temp-buffer
    (org-mode)
    (insert "Alpha sentence.\n")
    (let ((comment (org-copilot-add-comment
		    (list :id "ai-1"
			  :source-start (point-min)
			  :source-end (+ (point-min) (length "Alpha sentence."))
			  :target-text "Alpha sentence."
			  :suggestion "Alpha."
			  :status 'active))))
      (org-copilot-accept-comment comment (current-buffer))
      (goto-char (point-min))
      (insert "Changed ")
      (should-error
       (org-copilot-undo-accepted-comment
	(org-copilot-find-comment "ai-1")
	(current-buffer))
       :type 'user-error)
      (should (eq (plist-get (org-copilot-find-comment "ai-1") :status)
		  'accepted)))))

(ert-deftest org-copilot-dismiss-at-point-refreshes-panel ()
  "Dismissing from the side panel refreshes visible panel contents."
  (let ((source (generate-new-buffer " *org copilot action source*")))
    (unwind-protect
	(with-current-buffer source
	  (org-mode)
	  (insert "Alpha sentence.\n")
	  (org-copilot-mode 1)
	  (org-copilot-add-comment
	   (list :id "ai-1"
		 :source-start (point-min)
		 :source-end (+ (point-min) 5)
		 :target-text "Alpha"
		 :body "Dismiss me."
		 :status 'active))
	  (with-temp-buffer
	    (org-copilot-panel-mode)
	    (setq org-context-panel-source-buffer source)
	    (let ((inhibit-read-only t))
	      (org-context-panel-render-side-panel source)
	      (goto-char (point-min)))
	    (should (string-match-p "Dismiss me" (buffer-string)))
	    (org-copilot-dismiss-at-point)
	    (should-not (string-match-p "Dismiss me" (buffer-string)))))
      (when (buffer-live-p source)
	(kill-buffer source)))))

(ert-deftest org-copilot-dismiss-removes-comment ()
  "Dismissing an AI comment removes it from the current session."
  (with-temp-buffer
    (org-mode)
    (let ((comment (org-copilot-add-comment
		    (list :id "ai-1"
			  :body "Clarify this."
			  :status 'active))))
      (org-copilot-dismiss-comment comment (current-buffer))
      (should-not (org-copilot-comments)))))

(provide 'org-copilot-actions-test)
;;; org-copilot-actions-test.el ends here

(ert-deftest org-copilot-accept-at-point-delegates-linked-suggestion ()
  "Accepting a linked comment row delegates source mutation to org-suggestions."
  (let* ((directory (make-temp-file "org-copilot-linked-accept" t))
	 (source-file (expand-file-name "draft.org" directory)))
    (unwind-protect
	(with-current-buffer (find-file-noselect source-file)
	  (erase-buffer)
	  (insert "* Intro\nCurrent body.\n")
	  (save-buffer)
	  (org-mode)
	  (let* ((source (current-buffer))
		 (thread (list :id "ai-thread-1"
			       :candidates
			       (list (list :id "ai-1" :status 'active
					   :hunks
					   (list (list :id "h1"
						       :kind 'section-replace
						       :section-title "Intro"
						       :replacement "New body.")))))))
	    (org-suggestions-write-sidecar source-file (list thread))
	    (with-temp-buffer
	      (setq org-context-panel-source-buffer source)
	      (insert "💬 Rewrite Intro ✏️ ai-1\n")
	      (add-text-properties
	       (point-min) (point-max)
	       '(org-context-panel-item
		 (:type comment :id "cmt-1" :suggestion-ids "ai-1")))
	      (goto-char (point-min))
	      (org-copilot-accept-at-point))
	    (should (equal (buffer-string) "* Intro\nNew body.\n"))
	    (let* ((loaded (car (org-suggestions-load-sidecar source-file)))
		   (candidate (car (plist-get loaded :candidates))))
	      (should (eq (plist-get candidate :status) 'accepted)))))
      (when-let* ((buffer (find-buffer-visiting source-file)))
	(kill-buffer buffer))
      (delete-directory directory t))))
