;;; org-suggestions.el --- Executable Org edit suggestions -*- lexical-binding: t; -*-

;; Author: Hubert Behaghel
;; Maintainer: Hubert Behaghel
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (org "9.6"))
;; Keywords: outlines, convenience, tools
;; URL: https://github.com/behaghel/org-suggestions

;;; Commentary:
;; Structured patch-like suggestion threads for Org buffers.  A thread groups
;; candidate edit proposals; each candidate contains executable hunks.

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'subr-x)

(defconst org-suggestions-schema-version 1
  "Current Org Suggestions sidecar schema version.")

(defgroup org-suggestions nil
  "Executable suggestions for Org buffers."
  :group 'org)

(defvar org-suggestions--rollback-data nil
  "Session-local rollback data keyed by suggestion candidate id.")

(defun org-suggestions-sidecar-path (&optional source-file)
  "Return suggestions sidecar path for SOURCE-FILE or current buffer file."
  (let ((file (or source-file buffer-file-name)))
    (unless file
      (user-error "Current buffer is not visiting a file"))
    (concat (file-name-sans-extension file) ".suggestions.org")))

(defun org-suggestions--relative-source-file (source-file sidecar-file)
  "Return SOURCE-FILE relative to SIDECAR-FILE directory."
  (file-relative-name source-file (file-name-directory sidecar-file)))

(defun org-suggestions--property-line (key value)
  "Return an Org property line for KEY and VALUE when VALUE is non-nil."
  (when value
    (format ":%s: %s\n" key value)))

(defun org-suggestions--symbol-name (value)
  "Return lowercase string name for symbol or string VALUE."
  (cond
   ((symbolp value) (symbol-name value))
   ((stringp value) value)))

(defun org-suggestions--format-path (path)
  "Return serialized outline PATH."
  (when path
    (if (listp path)
	(string-join path "/")
      path)))

(defun org-suggestions--parse-path (path)
  "Return outline path list from serialized PATH."
  (when (and path (not (string-empty-p path)))
    (split-string path "/" t)))

(defun org-suggestions--normalize-replacement (text)
  "Normalize replacement TEXT for source insertion."
  (concat (string-trim-right (or text "")) "\n"))

(defun org-suggestions--insert-properties (properties)
  "Insert Org PROPERTIES drawer from alternating key/value list."
  (insert ":PROPERTIES:\n")
  (while properties
    (insert (or (org-suggestions--property-line (pop properties) (pop properties))
		"")))
  (insert ":END:\n"))

(defun org-suggestions--insert-org-block (text)
  "Insert TEXT as an Org source block."
  (insert "#+begin_src org\n")
  (insert (or text ""))
  (unless (or (string-empty-p (or text ""))
	      (string-suffix-p "\n" text))
    (insert "\n"))
  (insert "#+end_src\n"))

(defun org-suggestions--insert-hunk (hunk)
  "Insert HUNK as Org sidecar text."
  (insert (format "*** Hunk %s\n" (plist-get hunk :id)))
  (org-suggestions--insert-properties
   (list "ORG_SUGGESTIONS_HUNK_ID" (plist-get hunk :id)
	 "ORG_SUGGESTIONS_HUNK_KIND" (org-suggestions--symbol-name
				      (plist-get hunk :kind))
	 "ORG_SUGGESTIONS_PRIMARY" (when (plist-get hunk :primary) "t")
	 "ORG_SUGGESTIONS_SECTION_TITLE" (plist-get hunk :section-title)
	 "ORG_SUGGESTIONS_SECTION_PATH" (org-suggestions--format-path
					 (plist-get hunk :section-path))
	 "ORG_SUGGESTIONS_ANCHOR_TEXT" (plist-get hunk :anchor-text)
	 "ORG_SUGGESTIONS_PLACEMENT" (org-suggestions--symbol-name
				      (plist-get hunk :placement))))
  (when (plist-member hunk :original)
    (insert "\n**** Original\n")
    (org-suggestions--insert-org-block (plist-get hunk :original)))
  (insert "\n**** Replacement\n")
  (org-suggestions--insert-org-block (plist-get hunk :replacement)))

(defun org-suggestions--insert-candidate (candidate)
  "Insert CANDIDATE as Org sidecar text."
  (insert (format "** %s %s\n"
		  (upcase (org-suggestions--symbol-name
			   (plist-get candidate :status)))
		  (plist-get candidate :id)))
  (org-suggestions--insert-properties
   (list "ORG_SUGGESTIONS_ID" (plist-get candidate :id)
	 "ORG_SUGGESTIONS_PARENT_ID" (plist-get candidate :parent-id)
	 "ORG_SUGGESTIONS_ALTERNATIVE_GROUP_ID"
	 (plist-get candidate :alternative-group-id)
	 "ORG_SUGGESTIONS_CREATED_BY" (plist-get candidate :created-by)
	 "ORG_SUGGESTIONS_CREATED_BY_MESSAGE_ID"
	 (plist-get candidate :created-by-message-id)))
  (when-let* ((body (plist-get candidate :body)))
    (insert body)
    (unless (string-suffix-p "\n" body)
      (insert "\n")))
  (insert "\n")
  (dolist (hunk (plist-get candidate :hunks))
    (org-suggestions--insert-hunk hunk)
    (insert "\n")))

(defun org-suggestions--insert-thread (thread)
  "Insert THREAD as Org sidecar text."
  (insert (format "* Thread %s\n" (plist-get thread :id)))
  (org-suggestions--insert-properties
   (list "ORG_SUGGESTIONS_THREAD_ID" (plist-get thread :id)
	 "ORG_SUGGESTIONS_PROVIDER" (plist-get thread :provider)
	 "ORG_SUGGESTIONS_SESSION_ID" (plist-get thread :session-id)
	 "ORG_SUGGESTIONS_SUMMARY" (plist-get thread :summary)
	 "ORG_SUGGESTIONS_COMMENT_ID" (plist-get thread :comment-id)))
  (insert "\n")
  (dolist (candidate (plist-get thread :candidates))
    (org-suggestions--insert-candidate candidate)))

(defun org-suggestions--sidecar-text (source-file threads sidecar-file)
  "Return sidecar text for SOURCE-FILE and THREADS at SIDECAR-FILE."
  (with-temp-buffer
    (insert (format "#+title: Suggestions for %s\n"
		    (file-name-nondirectory source-file)))
    (insert (format "#+source: %s\n"
		    (org-suggestions--relative-source-file
		     source-file sidecar-file)))
    (insert (format "#+org_suggestions_schema_version: %d\n"
		    org-suggestions-schema-version))
    (insert "#+todo: ACTIVE ACCEPTED SUPERSEDED DISMISSED STALE | ARCHIVED\n\n")
    (dolist (thread threads)
      (org-suggestions--insert-thread thread))
    (buffer-string)))

(defun org-suggestions-write-sidecar (source-file threads &optional sidecar-file)
  "Write THREADS for SOURCE-FILE to SIDEcar and return sidecar path.
SIDECAR-FILE defaults to `org-suggestions-sidecar-path'."
  (let* ((target (or sidecar-file (org-suggestions-sidecar-path source-file)))
	 (temporary (make-temp-file
		     (expand-file-name ".org-suggestions"
				       (file-name-directory target)))))
    (make-directory (file-name-directory target) t)
    (unwind-protect
	(progn
	  (with-temp-file temporary
	    (insert (org-suggestions--sidecar-text source-file threads target)))
	  (rename-file temporary target t))
      (when (file-exists-p temporary)
	(delete-file temporary)))
    target))

(defun org-suggestions--heading-level ()
  "Return current heading level."
  (org-outline-level))

(defun org-suggestions--heading-title ()
  "Return current heading title without TODO keyword."
  (org-get-heading t t t t))

(defun org-suggestions--subtree-end ()
  "Return end of current Org subtree."
  (save-excursion
    (org-end-of-subtree t t)))

(defun org-suggestions--source-block-body-in-subtree ()
  "Return first source block body in current subtree."
  (let ((end (org-suggestions--subtree-end))
	body)
    (save-excursion
      (while (and (not body)
		  (re-search-forward "^[[:space:]]*#[+]begin_src\\b.*$" end t))
	(forward-line 1)
	(let ((start (point)))
	  (when (re-search-forward "^[[:space:]]*#[+]end_src[[:space:]]*$" nil t)
	    (setq body (buffer-substring-no-properties start (match-beginning 0)))))))
    body))

(defun org-suggestions--parse-hunk ()
  "Parse hunk at current heading."
  (let ((end (org-suggestions--subtree-end))
	(hunk (list :id (org-entry-get nil "ORG_SUGGESTIONS_HUNK_ID")
		    :kind (intern (org-entry-get nil "ORG_SUGGESTIONS_HUNK_KIND"))
		    :primary (equal (org-entry-get nil "ORG_SUGGESTIONS_PRIMARY") "t")
		    :section-title (org-entry-get nil "ORG_SUGGESTIONS_SECTION_TITLE")
		    :section-path (org-suggestions--parse-path
				   (org-entry-get nil "ORG_SUGGESTIONS_SECTION_PATH"))
		    :anchor-text (org-entry-get nil "ORG_SUGGESTIONS_ANCHOR_TEXT")
		    :placement (when-let* ((placement (org-entry-get nil "ORG_SUGGESTIONS_PLACEMENT")))
				 (intern placement)))))
    (save-excursion
      (while (re-search-forward org-heading-regexp end t)
	(when (= (org-suggestions--heading-level) 4)
	  (pcase (org-suggestions--heading-title)
	    ("Original"
	     (setq hunk (plist-put hunk :original
				   (org-suggestions--source-block-body-in-subtree))))
	    ("Replacement"
	     (setq hunk (plist-put hunk :replacement
				   (org-suggestions--source-block-body-in-subtree))))))))
    hunk))

(defun org-suggestions--parse-candidate ()
  "Parse candidate at current heading."
  (let ((end (org-suggestions--subtree-end))
	(candidate (list :id (org-entry-get nil "ORG_SUGGESTIONS_ID")
			 :status (intern (downcase (or (org-get-todo-state)
						       "active")))
			 :parent-id (org-entry-get nil "ORG_SUGGESTIONS_PARENT_ID")
			 :alternative-group-id
			 (org-entry-get nil "ORG_SUGGESTIONS_ALTERNATIVE_GROUP_ID")
			 :created-by (org-entry-get nil "ORG_SUGGESTIONS_CREATED_BY")
			 :created-by-message-id
			 (org-entry-get nil "ORG_SUGGESTIONS_CREATED_BY_MESSAGE_ID")))
	hunks)
    (save-excursion
      (while (re-search-forward org-heading-regexp end t)
	(when (= (org-suggestions--heading-level) 3)
	  (push (org-suggestions--parse-hunk) hunks))))
    (plist-put candidate :hunks (nreverse hunks))))

(defun org-suggestions--parse-thread ()
  "Parse thread at current heading."
  (let ((end (org-suggestions--subtree-end))
	(thread (list :id (org-entry-get nil "ORG_SUGGESTIONS_THREAD_ID")
		      :provider (org-entry-get nil "ORG_SUGGESTIONS_PROVIDER")
		      :session-id (org-entry-get nil "ORG_SUGGESTIONS_SESSION_ID")
		      :summary (org-entry-get nil "ORG_SUGGESTIONS_SUMMARY")
		      :comment-id (org-entry-get nil "ORG_SUGGESTIONS_COMMENT_ID")))
	candidates)
    (save-excursion
      (while (re-search-forward org-heading-regexp end t)
	(when (= (org-suggestions--heading-level) 2)
	  (push (org-suggestions--parse-candidate) candidates))))
    (plist-put thread :candidates (nreverse candidates))))

(defun org-suggestions-load-sidecar (source-file &optional sidecar-file)
  "Load suggestions threads for SOURCE-FILE from SIDEcar.
SIDECAR-FILE defaults to `org-suggestions-sidecar-path'."
  (let ((target (or sidecar-file (org-suggestions-sidecar-path source-file)))
	threads)
    (when (file-exists-p target)
      (with-temp-buffer
	(insert-file-contents target)
	(org-mode)
	(goto-char (point-min))
	(while (re-search-forward org-heading-regexp nil t)
	  (when (= (org-suggestions--heading-level) 1)
	    (push (org-suggestions--parse-thread) threads)))))
    (nreverse threads)))

(defun org-suggestions--heading-line ()
  "Return current Org heading line without text properties."
  (buffer-substring-no-properties (line-beginning-position) (line-end-position)))

(defun org-suggestions--section-bounds-at-heading ()
  "Return section bounds at current heading."
  (let* ((heading-start (point))
	 (heading-line (org-suggestions--heading-line))
	 (section-title (org-get-heading t t t t))
	 (section-path (org-get-outline-path t t))
	 (body-start (save-excursion
		       (forward-line 1)
		       (point)))
	 (end (save-excursion
		(org-end-of-subtree t t)
		(point))))
    (list :heading-start heading-start
	  :heading-line heading-line
	  :section-title section-title
	  :section-path section-path
	  :body-start body-start
	  :end end)))

(defun org-suggestions--all-section-bounds ()
  "Return all Org section bounds in the current buffer."
  (let (sections)
    (org-with-wide-buffer
     (save-excursion
       (goto-char (point-min))
       (while (re-search-forward org-heading-regexp nil t)
	 (beginning-of-line)
	 (push (org-suggestions--section-bounds-at-heading) sections)
	 (org-end-of-subtree t t))))
    (nreverse sections)))

(defun org-suggestions--find-section-by-path (section-path)
  "Return section bounds for SECTION-PATH, or nil."
  (org-with-wide-buffer
   (save-excursion
     (goto-char (point-min))
     (catch 'found
       (while (re-search-forward org-heading-regexp nil t)
	 (beginning-of-line)
	 (when (equal section-path (org-get-outline-path t t))
	   (throw 'found (org-suggestions--section-bounds-at-heading)))
	 (forward-line 1))))))

(defun org-suggestions--find-unique-section-by-title (section-title)
  "Return unique section bounds for SECTION-TITLE, or nil."
  (org-with-wide-buffer
   (save-excursion
     (goto-char (point-min))
     (let (matches)
       (while (re-search-forward org-heading-regexp nil t)
	 (beginning-of-line)
	 (when (string= section-title (org-get-heading t t t t))
	   (push (org-suggestions--section-bounds-at-heading) matches))
	 (forward-line 1))
       (and (= (length matches) 1) (car matches))))))

(defun org-suggestions--resolve-section-hunk (hunk)
  "Resolve section target for HUNK in current buffer."
  (let ((section-title (plist-get hunk :section-title))
	(section-path (plist-get hunk :section-path)))
    (or (and section-path
	     (org-suggestions--find-section-by-path section-path))
	(and section-title
	     (org-suggestions--find-unique-section-by-title section-title)))))

(defun org-suggestions--unique-text-match (text)
  "Return unique cons bounds for TEXT in current buffer, or nil."
  (let (matches)
    (save-excursion
      (goto-char (point-min))
      (while (search-forward text nil t)
	(push (cons (match-beginning 0) (match-end 0)) matches)))
    (and (= (length matches) 1) (car matches))))

(defun org-suggestions--resolve-replace-hunk (hunk)
  "Resolve replacement HUNK in current buffer."
  (when-let* ((original (plist-get hunk :original))
	      (match (org-suggestions--unique-text-match original)))
    (list :hunk hunk
	  :start (car match)
	  :end (cdr match)
	  :replacement (or (plist-get hunk :replacement) "")
	  :original original)))

(defun org-suggestions--resolve-insert-hunk (hunk)
  "Resolve insertion HUNK in current buffer."
  (when-let* ((anchor (plist-get hunk :anchor-text))
	      (match (org-suggestions--unique-text-match anchor)))
    (let ((position (pcase (plist-get hunk :placement)
		      ('before (car match))
		      ("before" (car match))
		      (_ (cdr match)))))
      (list :hunk hunk
	    :start position
	    :end position
	    :replacement (org-suggestions--normalize-replacement
			  (plist-get hunk :replacement))
	    :original ""))))

(defun org-suggestions--resolve-candidate (source-buffer candidate)
  "Return resolved hunk operations for CANDIDATE in SOURCE-BUFFER."
  (with-current-buffer source-buffer
    (cl-loop for hunk in (plist-get candidate :hunks)
	     collect
	     (pcase (plist-get hunk :kind)
	       ('section-replace
		(when-let* ((section (org-suggestions--resolve-section-hunk hunk)))
		  (list :hunk hunk
			:start (plist-get section :body-start)
			:end (plist-get section :end)
			:replacement (org-suggestions--normalize-replacement
				      (plist-get hunk :replacement))
			:original (buffer-substring-no-properties
				   (plist-get section :body-start)
				   (plist-get section :end)))))
	       ('replace (org-suggestions--resolve-replace-hunk hunk))
	       ('insert (org-suggestions--resolve-insert-hunk hunk))
	       (_ nil)))))

(defun org-suggestions--supersede-alternatives (thread candidate)
  "Mark active alternatives to CANDIDATE in THREAD as superseded."
  (when-let* ((group (plist-get candidate :alternative-group-id)))
    (dolist (sibling (plist-get thread :candidates))
      (when (and (not (eq sibling candidate))
		 (equal group (plist-get sibling :alternative-group-id))
		 (eq (plist-get sibling :status) 'active))
	(plist-put sibling :status 'superseded)))))

(defun org-suggestions-find-candidate (threads candidate-id)
  "Return cons cell (THREAD . CANDIDATE) for CANDIDATE-ID in THREADS."
  (catch 'found
    (dolist (thread threads)
      (dolist (candidate (plist-get thread :candidates))
	(when (equal candidate-id (plist-get candidate :id))
	  (throw 'found (cons thread candidate)))))))

(defun org-suggestions-accept-candidate (source-buffer thread candidate)
  "Accept CANDIDATE in SOURCE-BUFFER.
All candidate hunks apply as a single all-or-nothing operation."
  (let ((operations (org-suggestions--resolve-candidate source-buffer candidate)))
    (unless (and operations (cl-every #'identity operations))
      (plist-put candidate :status 'stale)
      (user-error "Suggestion candidate target is stale"))
    (setq operations
	  (sort operations
		(lambda (left right)
		  (> (plist-get left :start) (plist-get right :start)))))
    (with-current-buffer source-buffer
      (save-excursion
	(dolist (operation operations)
	  (let ((start (plist-get operation :start))
		(end (plist-get operation :end))
		(replacement (plist-get operation :replacement)))
	    (goto-char start)
	    (delete-region start end)
	    (insert replacement)))))
    (push (cons (plist-get candidate :id) operations)
	  org-suggestions--rollback-data)
    (plist-put candidate :status 'accepted)
    (org-suggestions--supersede-alternatives thread candidate)))

(defun org-suggestions-undo-accepted-candidate (source-buffer candidate)
  "Undo accepted CANDIDATE in SOURCE-BUFFER using session rollback data."
  (let* ((candidate-id (plist-get candidate :id))
	 (operations (alist-get candidate-id org-suggestions--rollback-data
				nil nil #'equal)))
    (unless operations
      (user-error "No rollback data for suggestion candidate"))
    (with-current-buffer source-buffer
      (save-excursion
	(dolist (operation operations)
	  (let* ((start (plist-get operation :start))
		 (replacement (plist-get operation :replacement))
		 (end (+ start (length replacement)))
		 (original (plist-get operation :original)))
	    (unless (equal (buffer-substring-no-properties start end)
			   replacement)
	      (user-error "Accepted suggestion text has changed"))
	    (goto-char start)
	    (delete-region start end)
	    (insert original))))))
  (plist-put candidate :status 'active))

(defun org-suggestions-accept-candidate-id (source-buffer candidate-id)
  "Accept persisted suggestion CANDIDATE-ID for SOURCE-BUFFER."
  (let* ((source-file (buffer-file-name source-buffer))
	 (threads (org-suggestions-load-sidecar source-file))
	 (match (org-suggestions-find-candidate threads candidate-id)))
    (unless match
      (user-error "No suggestion candidate: %s" candidate-id))
    (org-suggestions-accept-candidate source-buffer (car match) (cdr match))
    (org-suggestions-write-sidecar source-file threads)
    (cdr match)))

(defun org-suggestions--matching-session-thread-p (provider session-id)
  "Return non-nil when current thread belongs to PROVIDER and SESSION-ID."
  (and (= (org-outline-level) 1)
       (equal provider (org-entry-get nil "ORG_SUGGESTIONS_PROVIDER"))
       (equal session-id (org-entry-get nil "ORG_SUGGESTIONS_SESSION_ID"))))

(defun org-suggestions-archive-provider-session-threads
    (sidecar-file provider session-id)
  "Archive suggestion threads in SIDECAR-FILE for PROVIDER SESSION-ID."
  (when (file-exists-p sidecar-file)
    (with-current-buffer (find-file-noselect sidecar-file)
      (org-mode)
      (save-excursion
	(goto-char (point-min))
	(while (re-search-forward org-heading-regexp nil t)
	  (goto-char (match-beginning 0))
	  (if (org-suggestions--matching-session-thread-p provider session-id)
	      (org-archive-subtree)
	    (forward-line 1))))
      (save-buffer))))

(defun org-suggestions-delete-provider-session-threads
    (sidecar-file provider session-id)
  "Delete suggestion threads in SIDECAR-FILE for PROVIDER SESSION-ID."
  (when (file-exists-p sidecar-file)
    (with-temp-buffer
      (insert-file-contents sidecar-file)
      (org-mode)
      (goto-char (point-min))
      (while (re-search-forward org-heading-regexp nil t)
	(goto-char (match-beginning 0))
	(if (org-suggestions--matching-session-thread-p provider session-id)
	    (delete-region (point) (save-excursion (org-end-of-subtree t t)))
	  (forward-line 1)))
      (write-region (point-min) (point-max) sidecar-file nil 'silent))))

(defun org-suggestions--line-diff (old new)
  "Return a simple line diff from OLD to NEW."
  (concat
   (mapconcat (lambda (line) (concat "-" line))
	      (split-string (string-trim-right old) "\n")
	      "\n")
   "\n"
   (mapconcat (lambda (line) (concat "+" line))
	      (split-string (string-trim-right new) "\n")
	      "\n")
   "\n"))

(defun org-suggestions-candidate-diff (source-buffer _thread candidate)
  "Return a textual diff preview for CANDIDATE against live SOURCE-BUFFER."
  (with-current-buffer source-buffer
    (mapconcat
     (lambda (hunk)
       (pcase (plist-get hunk :kind)
	 ('section-replace
	  (let* ((section (or (org-suggestions--resolve-section-hunk hunk)
			      (user-error "Suggestion candidate target is stale")))
		 (old (buffer-substring-no-properties
		       (plist-get section :body-start)
		       (plist-get section :end)))
		 (new (org-suggestions--normalize-replacement
		       (plist-get hunk :replacement))))
	    (org-suggestions--line-diff old new)))
	 (_ "")))
     (plist-get candidate :hunks)
     "\n")))

(provide 'org-suggestions)
;;; org-suggestions.el ends here
