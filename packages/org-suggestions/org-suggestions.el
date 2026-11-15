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
  (let ((start (point)))
    (insert (or text ""))
    (org-escape-code-in-region start (point)))
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
	 "ORG_SUGGESTIONS_SECTION_ID" (plist-get hunk :section-id)
	 "ORG_SUGGESTIONS_SECTION_CUSTOM_ID" (plist-get hunk :section-custom-id)
	 "ORG_SUGGESTIONS_ESCAPED" "t"
	 "ORG_SUGGESTIONS_SECTION_PATH" (org-suggestions--format-path
					 (plist-get hunk :section-path))
	 "ORG_SUGGESTIONS_ANCHOR_TEXT" (plist-get hunk :anchor-text)
	 "ORG_SUGGESTIONS_PLACEMENT" (org-suggestions--symbol-name
				      (plist-get hunk :placement))
	 "ORG_SUGGESTIONS_ORIGINAL_NO_FINAL_NEWLINE"
	 (when (and (plist-member hunk :original)
		    (not (string-suffix-p "\n" (or (plist-get hunk :original) ""))))
	   "t")
	 "ORG_SUGGESTIONS_REPLACEMENT_NO_FINAL_NEWLINE"
	 (unless (string-suffix-p "\n" (or (plist-get hunk :replacement) ""))
	   "t")))
  (when (stringp (plist-get hunk :original))
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

(defun org-suggestions--source-block-body-in-subtree (&optional no-final-newline)
  "Return first source block body in current subtree.
When NO-FINAL-NEWLINE is non-nil, remove the synthetic delimiter newline."
  (let ((end (org-suggestions--subtree-end))
	body)
    (save-excursion
      (while (and (not body)
		  (re-search-forward "^[ \t]*#[+]begin_src\\b.*$" end t))
	(forward-line 1)
	(let ((start (point)))
	  (when (re-search-forward "^[ \t]*#[+]end_src[ \t]*$" end t)
	    (setq body (buffer-substring-no-properties start (match-beginning 0)))))))
    (if (and body no-final-newline)
	(string-remove-suffix "\n" body)
      body)))

(defun org-suggestions--parse-hunk ()
  "Parse hunk at current heading."
  (let ((end (org-suggestions--subtree-end))
	(escaped (equal (org-entry-get nil "ORG_SUGGESTIONS_ESCAPED") "t"))
	(original-no-final-newline
	 (equal (org-entry-get nil "ORG_SUGGESTIONS_ORIGINAL_NO_FINAL_NEWLINE") "t"))
	(replacement-no-final-newline
	 (equal (org-entry-get nil "ORG_SUGGESTIONS_REPLACEMENT_NO_FINAL_NEWLINE") "t"))
	(hunk (list :id (org-entry-get nil "ORG_SUGGESTIONS_HUNK_ID")
		    :kind (intern (org-entry-get nil "ORG_SUGGESTIONS_HUNK_KIND"))
		    :primary (equal (org-entry-get nil "ORG_SUGGESTIONS_PRIMARY") "t")
		    :section-title (org-entry-get nil "ORG_SUGGESTIONS_SECTION_TITLE")
		    :section-id (org-entry-get nil "ORG_SUGGESTIONS_SECTION_ID")
		    :section-custom-id (org-entry-get nil "ORG_SUGGESTIONS_SECTION_CUSTOM_ID")
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
				   (org-suggestions--source-block-body-in-subtree
				    original-no-final-newline))))
	    ("Replacement"
	     (setq hunk (plist-put hunk :replacement
				   (org-suggestions--source-block-body-in-subtree
				    replacement-no-final-newline))))))))
    (when escaped
      (dolist (key '(:original :replacement))
	(when (stringp (plist-get hunk key))
	  (plist-put hunk key (org-unescape-code-in-string (plist-get hunk key))))))
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
		       (org-end-of-meta-data)
		       (point)))
	 (end (save-excursion
		(org-end-of-subtree t t)
		(point))))
    (list :heading-start heading-start
	  :heading-line heading-line
	  :section-title section-title
	  :section-path section-path
	  :section-id (org-entry-get nil "ID")
	  :section-custom-id (org-entry-get nil "CUSTOM_ID")
	  :body-start body-start
	  :end end)))

(defun org-suggestions-capture-source-context ()
  "Capture immutable section-binding evidence from the current Org buffer.
The widened text preserves full outline identity and canonical body boundaries.
The accessible bounds preserve the author's narrowing; hidden sections cannot
supply baselines.  This is local evidence, not additional producer prompt text."
  (let ((bounds (cons (point-min) (point-max))))
    (org-with-wide-buffer
     (list :text (buffer-substring-no-properties (point-min) (point-max))
	   :bounds bounds))))

(defun org-suggestions--section-visible-p (section bounds)
  "Return non-nil when SECTION is completely accessible within BOUNDS.
Allow only a final newline just outside the accessible end, as excluded by
`org-narrow-to-subtree'; do not authorize a truncated body or hidden children."
  (and (>= (plist-get section :heading-start) (car bounds))
       (or (<= (plist-get section :end) (cdr bounds))
	   (and (= (plist-get section :end) (1+ (cdr bounds)))
		(eq (char-after (cdr bounds)) ?\n)))))

(defun org-suggestions--matching-sections (predicate &optional bounds)
  "Return sections satisfying PREDICATE and optional accessible BOUNDS."
  (org-with-wide-buffer
   (save-excursion
     (goto-char (point-min))
     (let ((org-outline-path-cache nil) matches)
       (while (re-search-forward org-heading-regexp nil t)
	 (beginning-of-line)
	 (when (and (not (org-in-block-p '("src" "example")))
		    (funcall predicate))
	   (let ((section (org-suggestions--section-bounds-at-heading)))
	     (when (or (not bounds)
		       (org-suggestions--section-visible-p section bounds))
	       (push section matches))))
	 (forward-line 1))
       matches))))

(defun org-suggestions--resolve-section-hunk (hunk &optional bounds)
  "Resolve HUNK uniquely within optional BOUNDS, preferring stable identity."
  (let* ((id (plist-get hunk :section-id))
	 (custom-id (plist-get hunk :section-custom-id))
	 (path (plist-get hunk :section-path))
	 (title (plist-get hunk :section-title))
	 (matches
	  (cond
	   (id (org-suggestions--matching-sections
		(lambda () (equal id (org-entry-get nil "ID"))) bounds))
	   (custom-id (org-suggestions--matching-sections
		       (lambda () (equal custom-id (org-entry-get nil "CUSTOM_ID"))) bounds))
	   (t (or (and path (org-suggestions--matching-sections
			     (lambda () (equal path (org-get-outline-path t t))) bounds))
		  (and title (org-suggestions--matching-sections
			      (lambda () (equal title (org-get-heading t t t t))) bounds)))))))
    (and (= (length matches) 1) (car matches))))

(defun org-suggestions-candidate-with-source-context (candidate source-context)
  "Return a copy of CANDIDATE bound to request-time Org SOURCE-CONTEXT.
SOURCE-CONTEXT is evidence from `org-suggestions-capture-source-context',
or a complete Org document string for existing producers.  Do not pass
narrowed fragments as complete documents.  Section baselines and identities
come only from this snapshot; missing, partial or ambiguous evidence remains
unsafe.  No source or sidecar is changed."
  (let ((copy (copy-tree candidate))
	(text (if (stringp source-context) source-context
		(plist-get source-context :text)))
	(bounds (and (listp source-context) (plist-get source-context :bounds))))
    (with-temp-buffer
      (when (stringp text) (insert text))
      (let ((org-mode-hook nil) (org-inhibit-startup t)) (org-mode))
      (dolist (hunk (plist-get copy :hunks))
	(when (eq (plist-get hunk :kind) 'section-replace)
	  ;; Never trust model-supplied original text as baseline evidence.
	  (setq hunk (plist-put hunk :original nil))
	  (when-let* ((section (org-suggestions--resolve-section-hunk hunk bounds)))
	    (dolist (key '(:section-title :section-path :section-id :section-custom-id))
	      (setq hunk (plist-put hunk key (plist-get section key))))
	    (plist-put hunk :original
		       (buffer-substring-no-properties
			(plist-get section :body-start) (plist-get section :end)))))))
    copy))

(defun org-suggestions--unique-text-match (text)
  "Return unique cons bounds for TEXT in current buffer, or nil."
  (let (matches)
    (save-excursion
      (goto-char (point-min))
      (while (and (stringp text) (not (string-empty-p text))
		  (search-forward text nil t))
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

(defun org-suggestions--mark-overlapping-operations (operations)
  "Mark overlapping OPERATIONS as conflicts and return the original list."
  (let ((ordered (sort (delq nil (copy-sequence operations))
		       (lambda (left right)
			 (< (plist-get left :start) (plist-get right :start)))))
	previous)
    (dolist (operation ordered)
      (when (and previous
		 (or (< (plist-get operation :start) (plist-get previous :end))
		     (= (plist-get operation :start) (plist-get previous :start))))
	(plist-put operation :conflict "Overlapping candidate hunks"))
      (when (or (not previous) (> (plist-get operation :end) (plist-get previous :end)))
	(setq previous operation))))
  operations)

(defun org-suggestions--resolve-candidate (source-buffer candidate)
  "Return resolved hunk operations and conflicts for CANDIDATE in SOURCE-BUFFER."
  (with-current-buffer source-buffer
    (org-with-wide-buffer
     (org-suggestions--mark-overlapping-operations
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
			  :conflict
			  (cond
			   ((not (stringp (plist-get hunk :original)))
			    "Request-time baseline unavailable")
			   ((not (equal (plist-get hunk :original)
					(buffer-substring-no-properties
					 (plist-get section :body-start)
					 (plist-get section :end))))
			    "Source changed since baseline"))
			  :original (buffer-substring-no-properties
				     (plist-get section :body-start)
				     (plist-get section :end)))))
		 ('replace (org-suggestions--resolve-replace-hunk hunk))
		 ('insert (org-suggestions--resolve-insert-hunk hunk))
		 (_ nil)))))))

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
    (unless (and operations (cl-every #'identity operations)
		 (not (cl-some (lambda (operation) (plist-get operation :conflict))
			       operations)))
      (plist-put candidate :status 'stale)
      (user-error "Suggestion candidate is stale; preview to compare, then M-x org-suggestions-reconcile-candidate"))
    (setq operations
	  (sort operations
		(lambda (left right)
		  (> (plist-get left :start) (plist-get right :start)))))
    (with-current-buffer source-buffer
      (org-with-wide-buffer
       (save-excursion
	 (atomic-change-group
	   (dolist (operation operations)
	     (let ((start (plist-get operation :start))
		   (end (plist-get operation :end))
		   (replacement (plist-get operation :replacement)))
	       (goto-char start)
	       (delete-region start end)
	       (insert replacement)))))))
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
	(atomic-change-group
	  ;; Undo in reverse application order so earlier length changes
	  ;; restore the original coordinates of later hunks.
	  (dolist (operation (reverse operations))
	    (let* ((start (plist-get operation :start))
		   (replacement (plist-get operation :replacement))
		   (end (+ start (length replacement)))
		   (original (plist-get operation :original)))
	      (unless (equal (buffer-substring-no-properties start end)
			     replacement)
		(user-error "Accepted suggestion text has changed"))
	      (goto-char start)
	      (delete-region start end)
	      (insert original)))))))
  (plist-put candidate :status 'active))

(defvar-local org-suggestions--reconciliation nil
  "Source, original candidate, revised candidate and editable hunk regions.")

(defvar org-suggestions-reconciliation-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map text-mode-map)
    (define-key map (kbd "C-c C-c") #'org-suggestions-finish-reconciliation)
    map)
  "Local map for deliberate suggestion reconciliation.")

(define-derived-mode org-suggestions-reconciliation-mode text-mode "Reconcile"
  "Compare baseline, current source and editable proposed replacements.
Only replacement fields are editable.  C-c C-c saves the reconciled proposal,
not the source.  Acceptance always revalidates its captured current baseline.")

(defun org-suggestions--insert-comparison (text)
  "Insert protected comparison TEXT, leaving following fields editable."
  (let ((inhibit-read-only t))
    (insert (propertize text 'read-only t))
    (unless (string-empty-p text)
      (put-text-property (1- (point)) (point) 'rear-nonsticky t))))

;;;###autoload
(defun org-suggestions-reconcile-candidate (source-buffer candidate-id)
  "Compare and edit persisted CANDIDATE-ID for SOURCE-BUFFER.
Return an editable review buffer.  Saving with C-c C-c records the source
baseline seen here; it neither accepts nor changes the source document."
  (interactive
   (let* ((source (current-buffer))
	  (threads (org-suggestions-load-sidecar (org-suggestions--source-file source))))
     (list source (completing-read "Reconcile candidate: "
				   (cl-loop for thread in threads append
					    (mapcar (lambda (candidate) (plist-get candidate :id))
						    (plist-get thread :candidates))) nil t))))
  (let* ((file (org-suggestions--source-file source-buffer))
	 (match (org-suggestions-find-candidate (org-suggestions-load-sidecar file) candidate-id))
	 (original (cdr match))
	 (operations (and original (org-suggestions--resolve-candidate source-buffer original))))
    (unless (memq (plist-get original :status) '(active stale))
      (user-error "Choose an active or stale candidate"))
    (unless (and operations (cl-every #'identity operations))
      (user-error "Missing or ambiguous target; restore its identity before reconciliation"))
    (let* ((revised (org-suggestions-candidate-with-source-context
		     original (with-current-buffer source-buffer
				(org-with-wide-buffer
				 (buffer-substring-no-properties (point-min) (point-max))))))
	   (buffer (generate-new-buffer (format "*Reconcile %s*" candidate-id))))
      (with-current-buffer buffer
	(org-suggestions-reconciliation-mode)
	(let (regions)
	  (org-suggestions--insert-comparison
	   "Compare all three versions. Edit Replacement fields only.\nC-c C-c saves the proposal; accept separately. Killing this buffer cancels.\n\n")
	  (cl-loop for operation in operations
		   for hunk in (plist-get revised :hunks)
		   do
		   (org-suggestions--insert-comparison
		    (format "Hunk %s\nBaseline:\n%s\nCurrent source:\n%s\nReplacement (editable):\n"
			    (plist-get hunk :id)
			    (or (plist-get (plist-get operation :hunk) :original)
				(plist-get (plist-get operation :hunk) :anchor-text)
				"<unavailable>")
			    (plist-get operation :original)))
		   (let ((start (copy-marker (point))))
		     (insert (or (plist-get hunk :replacement) ""))
		     (push (list hunk start (copy-marker (point))) regions))
		   (org-suggestions--insert-comparison "\n\n"))
	  (dolist (region regions)
	    (set-marker-insertion-type (nth 2 region) t))
	  (setq org-suggestions--reconciliation
		(list :source source-buffer :original original :revised revised
		      :regions (nreverse regions))))
	(goto-char (point-min)))
      (pop-to-buffer buffer)
      buffer)))

(defun org-suggestions-finish-reconciliation ()
  "Persist edited replacements and the reviewed baseline, without accepting."
  (interactive)
  (unless org-suggestions--reconciliation
    (user-error "Not a suggestion reconciliation buffer"))
  (let* ((state org-suggestions--reconciliation)
	 (source (plist-get state :source))
	 (file (org-suggestions--source-file source))
	 (original (plist-get state :original))
	 (threads (org-suggestions-load-sidecar file))
	 (match (org-suggestions-find-candidate threads (plist-get original :id)))
	 (revised (plist-get state :revised)))
    (unless (equal (cdr match) original)
      (user-error "Stored candidate changed; reopen reconciliation before saving"))
    (dolist (region (plist-get state :regions))
      (plist-put (car region) :replacement
		 (buffer-substring-no-properties (nth 1 region) (nth 2 region))))
    (plist-put revised :status 'active)
    (setcar (memq (cdr match) (plist-get (car match) :candidates)) revised)
    (org-suggestions-write-sidecar file threads)
    (set-buffer-modified-p nil)
    (message "Reconciled proposal saved; acceptance will revalidate source")
    (quit-window t)))

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

(defun org-suggestions-comment-suggestion-p (comment)
  "Return non-nil when COMMENT links to an executable suggestion."
  (or (plist-get comment :suggestion-thread-id)
      (plist-get comment :suggestion-ids)))

(defun org-suggestions--comment-candidate-ids (comment)
  "Return suggestion candidate ids explicitly linked from COMMENT."
  (let ((ids (plist-get comment :suggestion-ids)))
    (cond
     ((listp ids) ids)
     ((stringp ids) (split-string ids "[[:space:]]+" t)))))

(defun org-suggestions--source-file (source-buffer)
  "Return visited source file for SOURCE-BUFFER."
  (or (buffer-file-name source-buffer)
      (user-error "Source buffer is not visiting a file")))

(defun org-suggestions--find-thread-for-comment (threads comment)
  "Return suggestion thread from THREADS linked to COMMENT."
  (let ((thread-id (plist-get comment :suggestion-thread-id))
	(comment-id (plist-get comment :id)))
    (cl-find-if
     (lambda (thread)
       (or (and thread-id (equal thread-id (plist-get thread :id)))
	   (and comment-id (equal comment-id (plist-get thread :comment-id)))))
     threads)))

(defun org-suggestions--choose-candidate (thread comment)
  "Return default candidate in THREAD for COMMENT, or signal ambiguity."
  (let* ((ids (org-suggestions--comment-candidate-ids comment))
	 (candidates (plist-get thread :candidates))
	 (linked (if ids
		     (cl-remove-if-not
		      (lambda (candidate)
			(member (plist-get candidate :id) ids))
		      candidates)
		   candidates))
	 (active (cl-remove-if-not
		  (lambda (candidate)
		    (eq (plist-get candidate :status) 'active))
		  linked)))
    (cond
     ((= (length active) 1) (car active))
     ((= (length linked) 1) (car linked))
     ((null linked) (user-error "No linked suggestion candidate"))
     (t (user-error "Multiple suggestion candidates; choose one explicitly")))))

(defun org-suggestions-resolve-comment-suggestion (source-buffer comment)
  "Return (THREAD . CANDIDATE) linked from COMMENT for SOURCE-BUFFER."
  (unless (org-suggestions-comment-suggestion-p comment)
    (user-error "Comment has no linked suggestion"))
  (let* ((source-file (org-suggestions--source-file source-buffer))
	 (threads (org-suggestions-load-sidecar source-file))
	 (thread (org-suggestions--find-thread-for-comment threads comment)))
    (unless thread
      (user-error "No suggestion thread linked to comment"))
    (cons thread (org-suggestions--choose-candidate thread comment))))

(defun org-suggestions-accept-comment-suggestion (source-buffer comment)
  "Accept the default suggestion candidate linked from COMMENT."
  (let* ((source-file (org-suggestions--source-file source-buffer))
	 (threads (org-suggestions-load-sidecar source-file))
	 (thread (org-suggestions--find-thread-for-comment threads comment)))
    (unless thread
      (user-error "No suggestion thread linked to comment"))
    (let ((candidate (org-suggestions--choose-candidate thread comment)))
      (org-suggestions-accept-candidate source-buffer thread candidate)
      (org-suggestions-write-sidecar source-file threads)
      candidate)))

(defun org-suggestions-preview-comment-suggestion (source-buffer comment)
  "Return diff preview for the default suggestion linked from COMMENT."
  (pcase-let ((`(,thread . ,candidate)
	       (org-suggestions-resolve-comment-suggestion source-buffer comment)))
    (org-suggestions-candidate-diff source-buffer thread candidate)))

(defun org-suggestions-preview-comment-suggestion-at-point (source-buffer comment)
  "Show diff preview for the default suggestion linked from COMMENT."
  (let ((diff (org-suggestions-preview-comment-suggestion source-buffer comment)))
    (with-current-buffer (get-buffer-create "*Org Suggestion Diff*")
      (let ((inhibit-read-only t))
	(erase-buffer)
	(insert diff)
	(diff-mode)
	(pop-to-buffer (current-buffer))))
    diff))

(defun org-suggestions-dismiss-comment-suggestion (source-buffer comment)
  "Dismiss the default suggestion candidate linked from COMMENT."
  (let* ((source-file (org-suggestions--source-file source-buffer))
	 (threads (org-suggestions-load-sidecar source-file))
	 (thread (org-suggestions--find-thread-for-comment threads comment)))
    (unless thread
      (user-error "No suggestion thread linked to comment"))
    (let ((candidate (org-suggestions--choose-candidate thread comment)))
      (plist-put candidate :status 'dismissed)
      (org-suggestions-write-sidecar source-file threads)
      candidate)))

(defun org-suggestions-undo-comment-suggestion (source-buffer comment)
  "Undo accepted suggestion candidate linked from COMMENT."
  (let* ((source-file (org-suggestions--source-file source-buffer))
	 (threads (org-suggestions-load-sidecar source-file))
	 (thread (org-suggestions--find-thread-for-comment threads comment)))
    (unless thread
      (user-error "No suggestion thread linked to comment"))
    (let ((candidate (org-suggestions--choose-candidate thread comment)))
      (org-suggestions-undo-accepted-candidate source-buffer candidate)
      (org-suggestions-write-sidecar source-file threads)
      candidate)))

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
   (unless (string-empty-p old)
     (concat
      (mapconcat (lambda (line) (concat "-" line))
		 (split-string (string-trim-right old) "\n")
		 "\n")
      "\n"))
   (unless (string-empty-p new)
     (concat
      (mapconcat (lambda (line) (concat "+" line))
		 (split-string (string-trim-right new) "\n")
		 "\n")
      "\n"))))

(defun org-suggestions-candidate-diff (source-buffer _thread candidate)
  "Return a textual diff preview for CANDIDATE against live SOURCE-BUFFER."
  (let ((operations (org-suggestions--resolve-candidate source-buffer candidate)))
    (unless (and operations (cl-every #'identity operations))
      (user-error "Suggestion candidate target is stale"))
    (mapconcat
     (lambda (operation)
       (concat
	(when-let* ((conflict (plist-get operation :conflict)))
	  (format "CONFLICT %s: %s\nRequest-time baseline:\n%s\nCurrent source → proposed:\n"
		  (plist-get (plist-get operation :hunk) :id) conflict
		  (or (plist-get (plist-get operation :hunk) :original) "<unavailable>")))
	(org-suggestions--line-diff (plist-get operation :original)
				    (plist-get operation :replacement))))
     operations
     "\n")))

(provide 'org-suggestions)
;;; org-suggestions.el ends here
