;;; org-suggestions-edit-safety-test.el --- Concurrent authoring tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Request-time evidence, deliberate reconciliation, and transactional acceptance.

;;; Code:

(require 'ert)
(require 'org-suggestions)

(defun org-suggestions-safety--candidate (source &optional hunks)
  "Return a candidate bound to SOURCE with optional HUNKS."
  (org-suggestions-candidate-with-source-context
   (list :id "safe-1" :status 'active
	 :hunks (or hunks (list (list :id "h1" :kind 'section-replace
				      :section-title "Intro"
				      :section-path '("Intro")
				      :replacement "Proposed.\n"))))
   source))

(ert-deftest org-suggestions-safety-edits-after-preview-conflict ()
  "A preview never authorizes overwriting a later human edit."
  (with-temp-buffer
    (org-mode)
    (insert "* Intro\nOriginal.\n")
    (let* ((candidate (org-suggestions-safety--candidate (buffer-string)))
	   (thread (list :candidates (list candidate)))
	   (org-suggestions--rollback-data nil))
      (should (string-match-p "-Original" (org-suggestions-candidate-diff
					   (current-buffer) thread candidate)))
      (goto-char (point-max))
      (insert "Human addition.\n")
      (let ((before (buffer-string)))
	(should-error (org-suggestions-accept-candidate
		       (current-buffer) thread candidate) :type 'user-error)
	(should (equal before (buffer-string)))
	(should-not org-suggestions--rollback-data)
	(should (eq (plist-get candidate :status) 'stale))
	(let ((comparison (org-suggestions-candidate-diff
			   (current-buffer) thread candidate)))
	  (should (string-match-p "CONFLICT.*h1" comparison))
	  (should (string-match-p "Original" comparison))
	  (should (string-match-p "Human addition" comparison))
	  (should (string-match-p "Proposed" comparison)))))))

(ert-deftest org-suggestions-safety-unrelated-edits-and-heading-recovery ()
  "Moving a unique target and changing unrelated text do not invalidate it."
  (with-temp-buffer
    (org-mode)
    (let ((candidate (org-suggestions-safety--candidate "* Intro\nOriginal.\n")))
      (insert "* New parent\n** Intro\nOriginal.\n* Elsewhere\nHuman change.\n")
      (org-suggestions-accept-candidate (current-buffer) nil candidate)
      (should (equal (buffer-string)
		     "* New parent\n** Intro\nProposed.\n* Elsewhere\nHuman change.\n")))))

(ert-deftest org-suggestions-safety-missing-and-ambiguous-targets ()
  "Neither duplicate paths nor missing headings may select an arbitrary target."
  (dolist (source '("* Intro\nOriginal.\n* Intro\nOriginal.\n"
		    "* Other\nOriginal.\n"))
    (with-temp-buffer
      (org-mode)
      (insert source)
      (let ((candidate (org-suggestions-safety--candidate "* Intro\nOriginal.\n")))
	(should-error (org-suggestions-accept-candidate
		       (current-buffer) nil candidate) :type 'user-error)
	(should (equal source (buffer-string)))))))

(ert-deftest org-suggestions-safety-multi-hunk-conflict-preserves-alternatives ()
  "A conflict prevents all edits, rollback records, and alternative transitions."
  (with-temp-buffer
    (org-mode)
    (let* ((baseline "* Intro\nOriginal.\n* Other\nOther body.\n")
	   (candidate (org-suggestions-safety--candidate
		       baseline
		       (list (list :id "h1" :kind 'section-replace
				   :section-title "Intro" :replacement "Proposed.\n")
			     (list :id "h2" :kind 'section-replace
				   :section-title "Other" :replacement "Other proposal.\n"))))
	   (alternative (list :id "safe-2" :status 'active :alternative-group-id "a"))
	   (thread (list :candidates (list candidate alternative)))
	   (org-suggestions--rollback-data nil))
      (plist-put candidate :alternative-group-id "a")
      (insert baseline "Human change.\n")
      (let ((before (buffer-string)))
	(should-error (org-suggestions-accept-candidate
		       (current-buffer) thread candidate) :type 'user-error)
	(should (equal before (buffer-string)))
	(should (eq (plist-get alternative :status) 'active))
	(should-not org-suggestions--rollback-data)))))

(ert-deftest org-suggestions-safety-restoration-retains-baseline-and-identity ()
  "Round-tripping nested Org source preserves baseline text and heading ID."
  (let* ((directory (make-temp-file "suggestions-safety" t))
	 (file (expand-file-name "draft.org" directory))
	 (baseline "* Intro\n:PROPERTIES:\n:ID: stable-intro\n:END:\nOriginal.\n** Child\n#+begin_src org\n,* Literal heading\n#+end_src\n"))
    (unwind-protect
	(let ((candidate (org-suggestions-safety--candidate baseline)))
	  (org-suggestions-write-sidecar file (list (list :id "t" :candidates (list candidate))))
	  (let* ((thread (car (org-suggestions-load-sidecar file)))
		 (restored (car (plist-get thread :candidates))))
	    (should (= 1 (length (plist-get restored :hunks))))
	    (let ((hunk (car (plist-get restored :hunks))))
	      (should (equal (plist-get hunk :original)
			     (plist-get (car (plist-get candidate :hunks)) :original)))
	      (should (equal (plist-get hunk :section-id) "stable-intro")))
	    (with-temp-buffer
	      (org-mode)
	      (insert baseline)
	      (goto-char (point-max))
	      (insert "Human change.\n")
	      (should-error (org-suggestions-accept-candidate
			     (current-buffer) thread restored) :type 'user-error))))
      (delete-directory directory t))))

(ert-deftest org-suggestions-safety-legacy-without-baseline-is-not-safe ()
  "A restored legacy section proposal requires deliberate reconciliation."
  (with-temp-buffer
    (org-mode)
    (insert "* Intro\nHuman text.\n")
    (let ((candidate '(:id "legacy" :status active
			   :hunks ((:id "h1" :kind section-replace
					:section-title "Intro" :replacement "Old proposal.\n")))))
      (should-error (org-suggestions-accept-candidate
		     (current-buffer) nil candidate) :type 'user-error)
      (should (equal (buffer-string) "* Intro\nHuman text.\n")))))

(ert-deftest org-suggestions-safety-heading-id-survives-accept-and-undo ()
  "Stable heading metadata survives a rename, acceptance and atomic undo."
  (with-temp-buffer
    (org-mode)
    (let* ((candidate (org-suggestions-safety--candidate
		       "* Intro\n:PROPERTIES:\n:ID: stable-intro\n:END:\nOriginal.\n"))
	   (org-suggestions--rollback-data nil)
	   (before "* Renamed\n:PROPERTIES:\n:ID: stable-intro\n:END:\nOriginal.\n"))
      (insert before)
      (org-suggestions-accept-candidate (current-buffer) nil candidate)
      (should (equal (buffer-string)
		     "* Renamed\n:PROPERTIES:\n:ID: stable-intro\n:END:\nProposed.\n"))
      (org-suggestions-undo-accepted-candidate (current-buffer) candidate)
      (should (equal before (buffer-string))))))

(ert-deftest org-suggestions-safety-reconciliation-revalidates-and-undoes ()
  "Reconciliation is deliberate, persists a new baseline, and revalidates it."
  (let* ((directory (make-temp-file "suggestions-reconcile" t))
	 (file (expand-file-name "draft.org" directory))
	 (org-suggestions--rollback-data nil)
	 source review)
    (unwind-protect
	(save-window-excursion
	  (with-temp-file file (insert "* Intro\nHuman text.\n"))
	  (org-suggestions-write-sidecar
	   file (list (list :id "t" :candidates
			    (list (org-suggestions-safety--candidate "* Intro\nOriginal.\n")))))
	  (setq source (find-file-noselect file))
	  (setq review (org-suggestions-reconcile-candidate source "safe-1"))
	  (with-current-buffer review
	    (should (string-match-p "Original" (buffer-string)))
	    (should (string-match-p "Human text" (buffer-string)))
	    (goto-char (point-min))
	    (search-forward "Proposed.")
	    (replace-match "Human text, reconciled." t t)
	    (call-interactively #'org-suggestions-finish-reconciliation))
	  (with-current-buffer source
	    (should (equal (buffer-string) "* Intro\nHuman text.\n"))
	    (goto-char (point-max)) (insert "More human text.\n"))
	  (should-error (org-suggestions-accept-candidate-id source "safe-1") :type 'user-error)
	  (setq review (org-suggestions-reconcile-candidate source "safe-1"))
	  (with-current-buffer review
	    (call-interactively #'org-suggestions-finish-reconciliation))
	  (let ((before (with-current-buffer source (buffer-string)))
		(accepted (org-suggestions-accept-candidate-id source "safe-1")))
	    (should (equal (with-current-buffer source (buffer-string))
			   "* Intro\nHuman text, reconciled.\n"))
	    (org-suggestions-undo-accepted-candidate source accepted)
	    (should (equal before (with-current-buffer source (buffer-string))))))
      (when (buffer-live-p review) (kill-buffer review))
      (when (buffer-live-p source)
	(with-current-buffer source (set-buffer-modified-p nil)) (kill-buffer source))
      (delete-directory directory t))))

(ert-deftest org-suggestions-safety-overlapping-hunks-refuse-atomically ()
  "Overlapping edits are a candidate conflict, not sequential overwrites."
  (with-temp-buffer
    (org-mode)
    (insert "* Intro\nOriginal.\n")
    (let ((candidate (org-suggestions-safety--candidate
		      (buffer-string)
		      '((:id "h1" :kind section-replace :section-title "Intro"
			     :replacement "Section proposal.\n")
			(:id "h2" :kind replace :original "Original."
			     :replacement "Paragraph proposal."))))
	  (org-suggestions--rollback-data nil))
      (should-error (org-suggestions-accept-candidate
		     (current-buffer) nil candidate) :type 'user-error)
      (should (equal (buffer-string) "* Intro\nOriginal.\n"))
      (should-not org-suggestions--rollback-data))))

(ert-deftest org-suggestions-safety-empty-and-unknown-baselines-round-trip ()
  "Known empty text is distinct from missing evidence after restoration."
  (let* ((directory (make-temp-file "suggestions-empty" t))
	 (file (expand-file-name "draft.org" directory)))
    (unwind-protect
	(dolist (context '("* Intro\n" nil))
	  (let ((candidate (org-suggestions-safety--candidate context)))
	    (org-suggestions-write-sidecar file (list (list :id "t" :candidates (list candidate))))
	    (let ((restored (cdr (org-suggestions-find-candidate
				  (org-suggestions-load-sidecar file) "safe-1"))))
	      (with-temp-buffer
		(org-mode) (insert "* Intro\n")
		(if context
		    (progn (org-suggestions-accept-candidate (current-buffer) nil restored)
			   (should (equal (buffer-string) "* Intro\nProposed.\n")))
		  (should-error (org-suggestions-accept-candidate
				 (current-buffer) nil restored) :type 'user-error))))))
      (delete-directory directory t))))

(ert-deftest org-suggestions-safety-comparison-preserves-concurrent-proposal ()
  "Saving an old comparison cannot replace a concurrently revised candidate."
  (let* ((directory (make-temp-file "suggestions-concurrent" t))
	 (file (expand-file-name "draft.org" directory))
	 source review)
    (unwind-protect
	(save-window-excursion
	  (with-temp-file file (insert "* Intro\nHuman text.\n"))
	  (org-suggestions-write-sidecar
	   file (list (list :id "t" :candidates
			    (list (org-suggestions-safety--candidate "* Intro\nOriginal.\n")))))
	  (setq source (find-file-noselect file)
		review (org-suggestions-reconcile-candidate source "safe-1"))
	  (with-current-buffer review
	    (goto-char (point-min))
	    (search-forward "Baseline:\n")
	    (forward-char 1)
	    (should-error (insert "Accidental baseline edit") :type 'text-read-only))
	  (let* ((threads (org-suggestions-load-sidecar file))
		 (candidate (cdr (org-suggestions-find-candidate threads "safe-1"))))
	    (plist-put candidate :status 'dismissed)
	    (org-suggestions-write-sidecar file threads)
	    (with-current-buffer review
	      (should-error (org-suggestions-finish-reconciliation) :type 'user-error))
	    (should (equal threads (org-suggestions-load-sidecar file))))
	  (should (equal (with-current-buffer source (buffer-string)) "* Intro\nHuman text.\n")))
      (when (buffer-live-p review)
	(with-current-buffer review (set-buffer-modified-p nil)) (kill-buffer review))
      (when (buffer-live-p source) (kill-buffer source))
      (delete-directory directory t))))

(ert-deftest org-suggestions-safety-stable-id-never-falls-back ()
  "Missing or duplicated stable IDs cannot resolve through a matching title."
  (dolist (text '("* Intro\nOriginal.\n"
		  "* Intro\n:PROPERTIES:\n:ID: stable\n:END:\nOriginal.\n* Copy\n:PROPERTIES:\n:ID: stable\n:END:\nOriginal.\n"))
    (with-temp-buffer
      (org-mode) (insert text)
      (let ((candidate (org-suggestions-safety--candidate
			"* Intro\n:PROPERTIES:\n:ID: stable\n:END:\nOriginal.\n")))
	(should-error (org-suggestions-accept-candidate
		       (current-buffer) nil candidate) :type 'user-error)
	(should (equal text (buffer-string)))))))

(provide 'org-suggestions-edit-safety-test)
;;; org-suggestions-edit-safety-test.el ends here
