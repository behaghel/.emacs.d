;;; org-suggestions-source-context-test.el --- Scoped source evidence -*- lexical-binding: t; -*-

;;; Commentary:
;; Provider-neutral request capture must preserve identity without granting
;; hidden or partially visible sections an executable baseline.

;;; Code:

(require 'ert)
(require 'org-suggestions)

(defun org-suggestions-context-test--candidate (context &rest identity)
  "Bind a section candidate with IDENTITY to immutable CONTEXT."
  (org-suggestions-candidate-with-source-context
   (list :id "context-1" :status 'active
	 :hunks (list (append (list :id "h1" :kind 'section-replace
				    :original "Forged baseline"
				    :replacement "Proposed\n")
			      identity)))
   context))

(ert-deftest org-suggestions-context-rejects-hidden-and-partial-sections ()
  "Neither hidden targets nor partial bodies acquire executable baselines."
  (dolist (restriction '(subtree partial-body partial-heading hidden-child))
    (with-temp-buffer
      (org-mode)
      (insert "* Parent\n** Intro\nOriginal\n*** Child\nChild body\n* Hidden\nSecret\n")
      (goto-char (point-min))
      (search-forward "** Intro")
      (beginning-of-line)
      (org-narrow-to-subtree)
      (pcase restriction
	('partial-body (narrow-to-region (point-min) (- (point-max) 2)))
	('partial-heading (narrow-to-region (+ (point-min) 2) (point-max)))
	('hidden-child
	 (goto-char (point-min))
	 (search-forward "*** Child")
	 (narrow-to-region (point-min) (match-beginning 0))))
      (let* ((context (org-suggestions-capture-source-context))
	     (candidate (org-suggestions-context-test--candidate
			 context :section-title "Hidden"))
	     (partial (unless (eq restriction 'subtree)
			(org-suggestions-context-test--candidate
			 context :section-title "Intro"))))
	(should (buffer-narrowed-p))
	(widen)
	(dolist (proposal (delq nil (list candidate partial)))
	  (should-not (plist-get (car (plist-get proposal :hunks)) :original))
	  (let ((before (buffer-string)))
	    (should-error (org-suggestions-accept-candidate
			   (current-buffer) nil proposal) :type 'user-error)
	    (should (equal before (buffer-string)))))))))

(ert-deftest org-suggestions-context-preserves-full-path-and-newline ()
  "Scope capture supplies full identity and an exact canonical section body."
  (dolist (include-newline '(nil t))
    (with-temp-buffer
      (org-mode)
      (insert "* Parent\n** Intro\nOriginal\n* Intro\nOriginal\n")
      (goto-char (point-min))
      (search-forward "** Intro")
      (beginning-of-line)
      (org-narrow-to-subtree)
      (when include-newline
	(let ((start (point-min)) (end (1+ (point-max))))
	  (widen) (narrow-to-region start end)))
      (let* ((bounds (cons (point-min) (point-max)))
	     (point-before (point))
	     (context (org-suggestions-capture-source-context))
	     (candidate (org-suggestions-context-test--candidate
			 context :section-title "Intro"))
	     (hunk (car (plist-get candidate :hunks))))
	(should (equal bounds (cons (point-min) (point-max))))
	(should (= point-before (point)))
	(should (equal (plist-get hunk :section-path) '("Parent" "Intro")))
	(should (equal (plist-get hunk :original) "Original\n"))
	(widen)
	(org-suggestions-accept-candidate (current-buffer) nil candidate)
	(should (equal (buffer-string)
		       "* Parent\n** Intro\nProposed\n* Intro\nOriginal\n"))))))

(ert-deftest org-suggestions-context-ambiguous-visible-titles-stay-unsafe ()
  "Widened title-only requests cannot guess between identical visible sections."
  (with-temp-buffer
    (org-mode)
    (insert "* Parent\n** Intro\nOriginal\n* Intro\nOriginal\n")
    (let* ((context (org-suggestions-capture-source-context))
	   (ambiguous (org-suggestions-context-test--candidate
		       context :section-title "Intro"))
	   (explicit (org-suggestions-context-test--candidate
		      context :section-path '("Parent" "Intro")
		      :section-title "Intro")))
      (should-error (org-suggestions-accept-candidate
		     (current-buffer) nil ambiguous) :type 'user-error)
      (org-suggestions-accept-candidate (current-buffer) nil explicit)
      (should (equal (buffer-string)
		     "* Parent\n** Intro\nProposed\n* Intro\nOriginal\n")))))

(ert-deftest org-suggestions-context-stable-ids-still-take-precedence ()
  "Captured ID and CUSTOM_ID evidence defeats misleading path/title hints."
  (dolist (property '("ID" "CUSTOM_ID"))
    (with-temp-buffer
      (org-mode)
      (insert (format "* Parent\n** Intro\n:PROPERTIES:\n:%s: stable\n:END:\nOriginal\n* Intro\nOriginal\n"
		      property))
      (goto-char (point-min))
      (search-forward "** Intro") (beginning-of-line)
      (org-narrow-to-subtree)
      (let* ((context (org-suggestions-capture-source-context))
	     (candidate (org-suggestions-context-test--candidate
			 context :section-title "Intro"))
	     (hunk (car (plist-get candidate :hunks))))
	(plist-put hunk :section-path '("Intro"))
	(widen)
	(org-suggestions-accept-candidate (current-buffer) nil candidate)
	(should (string-match-p "Proposed\n\\* Intro\nOriginal" (buffer-string)))
	(goto-char (point-min))
	(search-forward "** Intro")
	(should (equal (org-entry-get nil property) "stable"))))))

(provide 'org-suggestions-source-context-test)
;;; org-suggestions-source-context-test.el ends here
