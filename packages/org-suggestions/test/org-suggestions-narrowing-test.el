;;; org-suggestions-narrowing-test.el --- Restricted undo tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Public acceptance and undo preserve the author's narrowed source view.

;;; Code:

(require 'ert)
(require 'org-suggestions)

(defconst org-suggestions-narrowing-test--source
  "* Intro\nOriginal.\n* Middle\nKeep this.\n* Other\nOther.\n* Tail\nTail.\n"
  "Source with targets on both sides of an unrelated visible subtree.")

(defun org-suggestions-narrowing-test--candidate (replacement &optional multi)
  "Bind a candidate to the full source, using REPLACEMENT and optional MULTI."
  (org-suggestions-candidate-with-source-context
   (list :id "narrow-undo" :status 'active
	 :hunks (append
		 (list (list :id "intro" :kind 'section-replace
			     :section-title "Intro" :replacement replacement))
		 (when multi
		   (list (list :id "tail" :kind 'section-replace
			       :section-title "Tail" :replacement "Longer tail.\n")))))
   (org-suggestions-capture-source-context)))

(defun org-suggestions-narrowing-test--narrow ()
  "Narrow to Other and leave point inside its unchanged body."
  (goto-char (point-min))
  (search-forward "* Other")
  (beginning-of-line)
  (org-narrow-to-subtree)
  (search-forward "Other."))

(defun org-suggestions-narrowing-test--text ()
  "Return the complete source without changing its restriction or point."
  (org-with-wide-buffer (buffer-string)))

(ert-deftest org-suggestions-narrowing-accept-then-immediate-undo ()
  "Undo a full-snapshot proposal outside the current view without widening."
  (dolist (replacement '("Proposed.\n" "A much longer proposal.\n" "X\n"))
    (with-temp-buffer
      (org-mode)
      (insert org-suggestions-narrowing-test--source)
      (let* ((org-suggestions--rollback-data nil)
	     (candidate (org-suggestions-narrowing-test--candidate replacement)))
	(org-suggestions-narrowing-test--narrow)
	(let ((view (buffer-string))
	      (start (point-min)) (end (point-max)) (position (point)))
	  (org-suggestions-accept-candidate (current-buffer) nil candidate)
	  (should (eq (plist-get candidate :status) 'accepted))
	  (should (equal (org-suggestions-narrowing-test--text)
			 (concat "* Intro\n" replacement
				 "* Middle\nKeep this.\n* Other\nOther.\n* Tail\nTail.\n")))
	  (should (buffer-narrowed-p))
	  (should (equal (buffer-string) view))
	  (should (= (- (point) (point-min)) (- position start)))
	  (org-suggestions-undo-accepted-candidate (current-buffer) candidate)
	  (should (equal (org-suggestions-narrowing-test--text)
			 org-suggestions-narrowing-test--source))
	  (should (eq (plist-get candidate :status) 'active))
	  (should (buffer-narrowed-p))
	  (should (equal (buffer-string) view))
	  (should (= (point-min) start))
	  (should (= (point-max) end))
	  (should (= (point) position)))))))

(ert-deftest org-suggestions-narrowing-adjacent-target-preserves-view ()
  "Replacing text immediately before the view does not expose hidden text."
  (dolist (replacement '("Proposed.\n" "A much longer proposal.\n" "X\n"))
    (with-temp-buffer
      (org-mode)
      (insert "* Intro\nOriginal.\n* Other\nOther.\n")
      (let* ((before (buffer-string))
	     (org-suggestions--rollback-data nil)
	     (candidate (org-suggestions-narrowing-test--candidate replacement)))
	(org-suggestions-narrowing-test--narrow)
	(let ((view (buffer-string)) (start (point-min)) (position (point)))
	  (org-suggestions-accept-candidate (current-buffer) nil candidate)
	  (should (equal (buffer-string) view))
	  (should (= (- (point) (point-min)) (- position start)))
	  (org-suggestions-undo-accepted-candidate (current-buffer) candidate)
	  (should (equal (buffer-string) view))
	  (should (= (point) position))
	  (should (equal (org-suggestions-narrowing-test--text) before)))))))

(ert-deftest org-suggestions-narrowing-multi-hunk-undo ()
  "Reverse length-changing hunks on both sides of the restricted view."
  (dolist (replacement '("A much longer proposal.\n" "X\n"))
    (with-temp-buffer
      (org-mode)
      (insert org-suggestions-narrowing-test--source)
      (let* ((org-suggestions--rollback-data nil)
	     (candidate (org-suggestions-narrowing-test--candidate replacement t)))
	(org-suggestions-narrowing-test--narrow)
	(let ((view (buffer-string))
	      (start (point-min)) (end (point-max)) (position (point)))
	  (org-suggestions-accept-candidate (current-buffer) nil candidate)
	  (should (string-suffix-p "* Tail\nLonger tail.\n"
				   (org-suggestions-narrowing-test--text)))
	  (org-suggestions-undo-accepted-candidate (current-buffer) candidate)
	  (should (equal (org-suggestions-narrowing-test--text)
			 org-suggestions-narrowing-test--source))
	  (should (eq (plist-get candidate :status) 'active))
	  (should (equal (buffer-string) view))
	  (should (= (point-min) start))
	  (should (= (point-max) end))
	  (should (= (point) position)))))))

(ert-deftest org-suggestions-narrowing-visible-and-hidden-hunks-undo ()
  "Undo restores hunks before, inside and after the restricted subtree."
  (with-temp-buffer
    (org-mode)
    (insert org-suggestions-narrowing-test--source)
    (let* ((org-suggestions--rollback-data nil)
	   (candidate (org-suggestions-narrowing-test--candidate "X\n" t)))
      (plist-put candidate :hunks
		 (append (plist-get candidate :hunks)
			 (list (list :id "visible" :kind 'replace :original "Other."
				     :replacement "An expanded visible body."))))
      (org-suggestions-narrowing-test--narrow)
      (let ((view (buffer-string)) (start (point-min)) (end (point-max)))
	(org-suggestions-accept-candidate (current-buffer) nil candidate)
	(should (equal (buffer-string) "* Other\nAn expanded visible body."))
	(org-suggestions-undo-accepted-candidate (current-buffer) candidate)
	(should (equal (buffer-string) view))
	(should (= (point-min) start))
	(should (= (point-max) end))
	(should (equal (org-suggestions-narrowing-test--text)
		       org-suggestions-narrowing-test--source))
	(should (eq (plist-get candidate :status) 'active))))))

(ert-deftest org-suggestions-narrowing-undo-failures-are-atomic ()
  "Hidden changed text, read-only text and hook errors never partially undo."
  (dolist (failure '(changed-text read-only buffer-read-only hook))
    (with-temp-buffer
      (org-mode)
      (insert "* Intro\nOriginal.\n* Other\nOther.\n* Tail\nTail.\n")
      (let* ((original (buffer-string))
	     (org-suggestions--rollback-data nil)
	     (candidate (org-suggestions-narrowing-test--candidate "X\n" t)))
	(org-suggestions-narrowing-test--narrow)
	(org-suggestions-accept-candidate (current-buffer) nil candidate)
	;; Inject failures in the hidden hunks, including after one is restored.
	(org-with-wide-buffer
	 (goto-char (point-max))
	 (search-backward "Longer tail.")
	 (pcase failure
	   ('changed-text (replace-match "Human edits." t t))
	   ('read-only (put-text-property (point) (+ (point) 12) 'read-only t))))
	(let ((before (org-suggestions-narrowing-test--text))
	      (candidate-before (copy-tree candidate))
	      (rollback-before (copy-tree org-suggestions--rollback-data))
	      (view (buffer-string))
	      (start (point-min)) (end (point-max)) (position (point))
	      (buffer-read-only (eq failure 'buffer-read-only))
	      (before-change-functions
	       (when (eq failure 'hook)
		 (list (lambda (_start _end)
			 (when (string-suffix-p "* Tail\nTail.\n" (buffer-string))
			   (error "Simulated narrowed undo failure")))))))
	  (pcase failure
	    ('changed-text
	     (should (equal (should-error (org-suggestions-undo-accepted-candidate
					   (current-buffer) candidate) :type 'user-error)
			    '(user-error "Accepted suggestion text has changed"))))
	    ('buffer-read-only
	     (should-error (org-suggestions-undo-accepted-candidate
			    (current-buffer) candidate) :type 'buffer-read-only))
	    ('read-only
	     (should-error (org-suggestions-undo-accepted-candidate
			    (current-buffer) candidate) :type 'text-read-only))
	    ('hook
	     (should (equal (should-error (org-suggestions-undo-accepted-candidate
					   (current-buffer) candidate))
			    '(error "Simulated narrowed undo failure")))))
	  (should (equal-including-properties
		   (org-suggestions-narrowing-test--text) before))
	  (should (equal candidate candidate-before))
	  (should (equal org-suggestions--rollback-data rollback-before))
	  (should (equal (buffer-string) view))
	  (should (= (point-min) start))
	  (should (= (point-max) end))
	  (should (= (point) position)))
	(org-with-wide-buffer
	 (let ((inhibit-read-only t))
	   (remove-text-properties (point-min) (point-max) '(read-only nil)))
	 (when (eq failure 'changed-text)
	   (goto-char (point-max))
	   (search-backward "Human edits.")
	   (replace-match "Longer tail." t t)))
	(org-suggestions-undo-accepted-candidate (current-buffer) candidate)
	(should (equal (org-suggestions-narrowing-test--text) original))
	(should (eq (plist-get candidate :status) 'active))))))

(ert-deftest org-suggestions-narrowing-durable-comment-undo ()
  "Linked public actions persist accepted then active status while narrowed."
  (let* ((directory (make-temp-file "suggestions-narrowed" t))
	 (file (expand-file-name "draft.org" directory))
	 (comment '(:id "comment" :suggestion-thread-id "thread"
			:suggestion-ids "narrow-undo"))
	 (org-suggestions--rollback-data nil))
    (unwind-protect
	(with-temp-buffer
	  (org-mode)
	  (insert org-suggestions-narrowing-test--source)
	  (write-region (point-min) (point-max) file nil 'silent)
	  (setq buffer-file-name file)
	  (org-suggestions-write-sidecar
	   file (list (list :id "thread" :comment-id "comment"
			    :candidates (list (org-suggestions-narrowing-test--candidate
					       "Longer proposed body.\n" t)))))
	  (org-suggestions-narrowing-test--narrow)
	  (let ((view (buffer-string)) (position (point)))
	    (org-suggestions-accept-comment-suggestion (current-buffer) comment)
	    (should (eq (plist-get (cdr (org-suggestions-resolve-comment-suggestion
					 (current-buffer) comment)) :status) 'accepted))
	    (org-with-wide-buffer
	     (goto-char (point-max)) (search-backward "Longer tail.")
	     (replace-match "Human edits." t t))
	    (let ((stored (org-suggestions-load-sidecar file))
		  (before (org-suggestions-narrowing-test--text)))
	      (should-error (org-suggestions-undo-comment-suggestion
			     (current-buffer) comment) :type 'user-error)
	      (should (equal (org-suggestions-load-sidecar file) stored))
	      (should (equal (org-suggestions-narrowing-test--text) before)))
	    (org-with-wide-buffer
	     (goto-char (point-max)) (search-backward "Human edits.")
	     (replace-match "Longer tail." t t))
	    (org-suggestions-undo-comment-suggestion (current-buffer) comment)
	    (should (eq (plist-get (cdr (org-suggestions-resolve-comment-suggestion
					 (current-buffer) comment)) :status) 'active))
	    (should (equal (org-suggestions-narrowing-test--text)
			   org-suggestions-narrowing-test--source))
	    (should (buffer-narrowed-p))
	    (should (equal (buffer-string) view))
	    (should (= (point) position)))
	  (should (equal (with-temp-buffer (insert-file-contents file) (buffer-string))
			 org-suggestions-narrowing-test--source)))
      (delete-directory directory t))))

(provide 'org-suggestions-narrowing-test)
;;; org-suggestions-narrowing-test.el ends here
