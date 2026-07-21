;;; org-suggestions-test.el --- Tests for Org Suggestions -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for executable Org suggestion threads, candidates, and hunks.

;;; Code:

(require 'ert)
(require 'org)
(require 'org-suggestions)

(defun org-suggestions-test--with-source-file (content thunk)
  "Create a temporary Org source with CONTENT and call THUNK with file path."
  (let* ((directory (make-temp-file "org-suggestions-test" t))
	 (file (expand-file-name "draft.org" directory)))
    (unwind-protect
	(progn
	  (with-temp-file file
	    (insert content))
	  (funcall thunk file))
      (delete-directory directory t))))

(defun org-suggestions-test--sample-thread ()
  "Return a sample section-replace suggestion thread."
  (list :id "ai-thread-1"
	:provider "org-copilot"
	:summary "Rewrite Intro"
	:candidates
	(list
	 (list :id "ai-1"
	       :status 'active
	       :hunks
	       (list
		(list :id "h1"
		      :kind 'section-replace
		      :primary t
		      :section-title "Intro"
		      :section-path '("Intro")
		      :replacement "Replacement body.\n"))))))

(ert-deftest org-suggestions-sidecar-writes-section-replace-thread ()
  "Writing a section replacement thread creates the documented sidecar shape."
  (org-suggestions-test--with-source-file
   "* Intro\nOld body.\n"
   (lambda (file)
     (let ((sidecar (org-suggestions-sidecar-path file)))
       (org-suggestions-write-sidecar file (list (org-suggestions-test--sample-thread)))
       (should (file-exists-p sidecar))
       (with-temp-buffer
	 (insert-file-contents sidecar)
	 (should (string-match-p "#\\+org_suggestions_schema_version: 1"
				 (buffer-string)))
	 (should (string-match-p "\\* Thread ai-thread-1" (buffer-string)))
	 (should (string-match-p "\\*\\* ACTIVE ai-1" (buffer-string)))
	 (should (string-match-p "\\*\\*\\* Hunk h1" (buffer-string)))
	 (should (string-match-p ":ORG_SUGGESTIONS_HUNK_KIND: section-replace"
				 (buffer-string)))
	 (should (string-match-p "#\\+begin_src org\nReplacement body\."
				 (buffer-string))))))))

(ert-deftest org-suggestions-sidecar-loads-thread-candidate-hunk ()
  "Loading a sidecar restores thread, candidate, and hunk metadata."
  (org-suggestions-test--with-source-file
   "* Intro\nOld body.\n"
   (lambda (file)
     (org-suggestions-write-sidecar file (list (org-suggestions-test--sample-thread)))
     (let* ((threads (org-suggestions-load-sidecar file))
	    (thread (car threads))
	    (candidate (car (plist-get thread :candidates)))
	    (hunk (car (plist-get candidate :hunks))))
       (should (equal (plist-get thread :id) "ai-thread-1"))
       (should (equal (plist-get thread :provider) "org-copilot"))
       (should (equal (plist-get candidate :id) "ai-1"))
       (should (eq (plist-get candidate :status) 'active))
       (should (eq (plist-get hunk :kind) 'section-replace))
       (should (equal (plist-get hunk :section-title) "Intro"))
       (should (equal (plist-get hunk :replacement) "Replacement body.\n"))))))

(ert-deftest org-suggestions-accept-section-replace-uses-current-body ()
  "Accepting section replacement replaces the current body, not old text."
  (with-temp-buffer
    (org-mode)
    (insert "* Intro\nEdited after suggestion.\n")
    (let* ((thread (org-suggestions-test--sample-thread))
	   (candidate (car (plist-get thread :candidates))))
      (org-suggestions-accept-candidate (current-buffer) thread candidate)
      (should (equal (buffer-string) "* Intro\nReplacement body.\n"))
      (should (eq (plist-get candidate :status) 'accepted)))))

(ert-deftest org-suggestions-preview-diff-uses-live-section-body ()
  "Section replacement previews compare replacement against live source body."
  (with-temp-buffer
    (org-mode)
    (insert "* Intro\nLive edited body.\n")
    (let* ((thread (org-suggestions-test--sample-thread))
	   (candidate (car (plist-get thread :candidates)))
	   (diff (org-suggestions-candidate-diff (current-buffer) thread candidate)))
      (should (string-match-p "-Live edited body\." diff))
      (should (string-match-p "+Replacement body\." diff)))))



(ert-deftest org-suggestions-sidecar-round-trips-replace-and-insert-hunks ()
  "Sidecar storage preserves replace and insert hunk anchors."
  (org-suggestions-test--with-source-file
   "Alpha.
"
   (lambda (file)
     (let ((thread
	    (list :id "ai-thread-1"
		  :candidates
		  (list
		   (list :id "ai-1"
			 :status 'active
			 :hunks
			 (list
			  (list :id "h1" :kind 'replace
				:original "Alpha" :replacement "Beta")
			  (list :id "h2" :kind 'insert
				:anchor-text "Beta" :placement 'after
				:replacement "Gamma")))))))
       (org-suggestions-write-sidecar file (list thread))
       (let* ((loaded (car (org-suggestions-load-sidecar file)))
	      (candidate (car (plist-get loaded :candidates)))
	      (hunks (plist-get candidate :hunks))
	      (replace (car hunks))
	      (insert (cadr hunks)))
	 (should (eq (plist-get replace :kind) 'replace))
	 (should (equal (plist-get replace :original) "Alpha
"))
	 (should (equal (plist-get replace :replacement) "Beta
"))
	 (should (eq (plist-get insert :kind) 'insert))
	 (should (equal (plist-get insert :anchor-text) "Beta"))
	 (should (eq (plist-get insert :placement) 'after))
	 (should (equal (plist-get insert :replacement) "Gamma
")))))))

(defun org-suggestions-test--candidate (id status hunks &rest props)
  "Return candidate ID with STATUS, HUNKS, and PROPS."
  (append (list :id id :status status :hunks hunks) props))

(ert-deftest org-suggestions-accept-replace-requires-resolved-original ()
  "Replacement hunks accept only when original text resolves."
  (with-temp-buffer
    (org-mode)
    (insert "Alpha beta gamma.
")
    (let* ((candidate (org-suggestions-test--candidate
		       "ai-1" 'active
		       (list (list :id "h1"
				   :kind 'replace
				   :original "beta"
				   :replacement "BETA"))))
	   (thread (list :id "ai-thread-1" :candidates (list candidate))))
      (org-suggestions-accept-candidate (current-buffer) thread candidate)
      (should (equal (buffer-string) "Alpha BETA gamma.
"))
      (should (eq (plist-get candidate :status) 'accepted))
      (plist-put candidate :status 'active)
      (plist-put (car (plist-get candidate :hunks)) :original "missing")
      (should-error (org-suggestions-accept-candidate
		     (current-buffer) thread candidate)
		    :type 'user-error)
      (should (eq (plist-get candidate :status) 'stale)))))

(ert-deftest org-suggestions-accept-insert-uses-anchor-placement ()
  "Insertion hunks insert replacement before or after resolved anchors."
  (with-temp-buffer
    (org-mode)
    (insert "Alpha.
Omega.
")
    (let* ((candidate (org-suggestions-test--candidate
		       "ai-1" 'active
		       (list (list :id "h1"
				   :kind 'insert
				   :anchor-text "Alpha."
				   :placement 'after
				   :replacement "Middle.
"))))
	   (thread (list :id "ai-thread-1" :candidates (list candidate))))
      (org-suggestions-accept-candidate (current-buffer) thread candidate)
      (should (equal (buffer-string) "Alpha.Middle.

Omega.
"))
      (should (eq (plist-get candidate :status) 'accepted)))))

(ert-deftest org-suggestions-accept-multi-hunk-is-all-or-nothing ()
  "If any hunk is stale, no hunks in the candidate apply."
  (with-temp-buffer
    (org-mode)
    (insert "Alpha beta.
")
    (let* ((candidate (org-suggestions-test--candidate
		       "ai-1" 'active
		       (list (list :id "h1"
				   :kind 'replace
				   :original "Alpha"
				   :replacement "ALPHA")
			     (list :id "h2"
				   :kind 'replace
				   :original "missing"
				   :replacement "MISSING"))))
	   (thread (list :id "ai-thread-1" :candidates (list candidate))))
      (should-error (org-suggestions-accept-candidate
		     (current-buffer) thread candidate)
		    :type 'user-error)
      (should (equal (buffer-string) "Alpha beta.
"))
      (should (eq (plist-get candidate :status) 'stale)))))

(ert-deftest org-suggestions-alternative-accept-supersedes-siblings ()
  "Accepting one alternative supersedes active siblings in the same group."
  (with-temp-buffer
    (org-mode)
    (insert "Alpha.
")
    (let* ((left (org-suggestions-test--candidate
		  "ai-1" 'active
		  (list (list :id "h1" :kind 'replace
			      :original "Alpha" :replacement "Left"))
		  :alternative-group-id "alt-1"))
	   (right (org-suggestions-test--candidate
		   "ai-2" 'active
		   (list (list :id "h1" :kind 'replace
			       :original "Alpha" :replacement "Right"))
		   :alternative-group-id "alt-1"))
	   (thread (list :id "ai-thread-1" :candidates (list left right))))
      (org-suggestions-accept-candidate (current-buffer) thread right)
      (should (eq (plist-get right :status) 'accepted))
      (should (eq (plist-get left :status) 'superseded)))))

(ert-deftest org-suggestions-undo-accepted-session-local ()
  "Undo restores source text for an accepted candidate in the same session."
  (with-temp-buffer
    (org-mode)
    (insert "Alpha beta.
")
    (let* ((candidate (org-suggestions-test--candidate
		       "ai-1" 'active
		       (list (list :id "h1" :kind 'replace
				   :original "beta" :replacement "BETA"))))
	   (thread (list :id "ai-thread-1" :candidates (list candidate))))
      (org-suggestions-accept-candidate (current-buffer) thread candidate)
      (should (equal (buffer-string) "Alpha BETA.
"))
      (org-suggestions-undo-accepted-candidate (current-buffer) candidate)
      (should (equal (buffer-string) "Alpha beta.
"))
      (should (eq (plist-get candidate :status) 'active)))))

(provide 'org-suggestions-test)
;;; org-suggestions-test.el ends here
