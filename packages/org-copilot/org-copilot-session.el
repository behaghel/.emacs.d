;;; org-copilot-session.el --- Ephemeral sessions for Org Copilot -*- lexical-binding: t; -*-

;; Author: Hubert Behaghel
;; Maintainer: Hubert Behaghel
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (org "9.6"))
;; Keywords: outlines, tools, convenience
;; URL: https://github.com/behaghel/org-copilot

;;; Commentary:
;; Buffer-local session state for Org Copilot AI comments.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'org-copilot-model)
(require 'org-copilot-sidecar nil 'noerror)
(require 'org-comments-model nil 'noerror)
(require 'org-comments-sidecar nil 'noerror)
(require 'org-comments-store nil 'noerror)
(require 'org-suggestions nil 'noerror)

(defvar-local org-copilot--comments nil
  "Ephemeral AI comments for the current Org source buffer.")

(defvar-local org-copilot--chat-messages nil
  "Ephemeral chat messages for the current Org source buffer.")

(defvar-local org-copilot--chat-messages-restored-p nil
  "Non-nil when durable chat messages have been restored for this buffer.")

(defvar org-copilot--suppress-chat-persistence nil
  "Non-nil while restoring chat messages from durable storage.")

(defvar-local org-copilot-chat-focus-comment-id nil
  "AI comment id currently focused by the Org Copilot chat view.")

(defvar-local org-copilot-chat-context '(:type full-document)
  "Current Org Copilot chat context plist.
The context has a `:type' key.  Supported values are `full-document',
`comment', and `section'.  `org-copilot-chat-focus-comment-id' mirrors
comment contexts for focused chat rendering.")

(cl-defstruct (org-copilot--request (:constructor org-copilot--make-request))
  "Ephemeral request identity owned by a source session, never persisted."
  source-buffer source-file session (state 'pending) cancel cleanup)

(defvar-local org-copilot--request-session nil
  "Identity of the current asynchronous source session.")

(defvar-local org-copilot--requests nil
  "Requests still owned by this source session, including deferred completion.")

(defun org-copilot-start-request (request)
  "Return REQUEST with a source-session identity, unless already registered.
Adapters retain this request for callbacks; obsolete identities are not renewed.
Navigation neither invalidates requests nor changes their captured context."
  (if (plist-get request :request-token)
      request
    (with-current-buffer (plist-get request :source-buffer)
      (org-copilot-restore-chat-messages)
      (unless org-copilot--request-session
	(setq org-copilot--request-session (list 'session)))
      (dolist (hook '(kill-buffer-hook change-major-mode-hook))
	(add-hook hook #'org-copilot--invalidate-requests nil t))
      (add-hook 'after-set-visited-file-name-hook #'org-copilot--retarget-session nil t)
      (let ((token (org-copilot--make-request
		    :source-buffer (current-buffer) :source-file buffer-file-name
		    :session org-copilot--request-session)))
	(push token org-copilot--requests)
	(plist-put (copy-tree request) :request-token token)))))

(defun org-copilot-request-current-p (request &optional state)
  "Return non-nil if REQUEST still belongs to its source session and STATE.
With no STATE, accept both pending and deferred installation phases."
  (when-let* ((token (plist-get request :request-token))
	      (source (org-copilot--request-source-buffer token)))
    (and (buffer-live-p source)
	 (with-current-buffer source
	   (and (eq source (plist-get request :source-buffer))
		(eq org-copilot--request-session (org-copilot--request-session token))
		(equal buffer-file-name (org-copilot--request-source-file token))
		(memq token org-copilot--requests)
		(if state (eq state (org-copilot--request-state token))
		  (memq (org-copilot--request-state token) '(pending installing))))))))

(defun org-copilot--release-request-resources (token)
  "Release TOKEN's adapter resources once."
  (when-let* ((cleanup (org-copilot--request-cleanup token)))
    (setf (org-copilot--request-cleanup token) nil)
    (funcall cleanup)))

(defun org-copilot-finish-request (request)
  "Release REQUEST after completion, even if its source has been closed."
  (when-let* ((token (plist-get request :request-token)))
    (setf (org-copilot--request-state token) 'finished
	  (org-copilot--request-cancel token) nil)
    (when (buffer-live-p (org-copilot--request-source-buffer token))
      (with-current-buffer (org-copilot--request-source-buffer token)
	(org-copilot-remove-pending-chat-message request)
	(setq org-copilot--requests (delq token org-copilot--requests))))
    (org-copilot--release-request-resources token)))

(defun org-copilot-begin-completion (request)
  "Claim REQUEST's pending completion once, retaining its deferred fence."
  (when (org-copilot-request-current-p request 'pending)
    (let ((token (plist-get request :request-token)))
      (setf (org-copilot--request-state token) 'installing
	    (org-copilot--request-cancel token) nil)
      (with-current-buffer (org-copilot--request-source-buffer token)
	(org-copilot-remove-pending-chat-message request))
      (org-copilot--release-request-resources token)
      t)))

(defun org-copilot-request-set-cancellation (request cancel cleanup)
  "Register adapter CANCEL and CLEANUP functions for REQUEST.
CANCEL is best effort; source-session validity never depends on its success."
  (let ((token (plist-get request :request-token)))
    (if (org-copilot-request-current-p request 'pending)
	(setf (org-copilot--request-cancel token) cancel
	      (org-copilot--request-cleanup token) cleanup)
      (funcall cleanup))))

(defun org-copilot-cancel-request (request)
  "Invalidate REQUEST before attempting provider cancellation."
  (when-let* ((token (plist-get request :request-token)))
    (let ((cancel (org-copilot--request-cancel token))
	  (cleanup (org-copilot--request-cleanup token)))
      ;; Provider abort can synchronously invoke the callback again.
      (setf (org-copilot--request-cleanup token) nil)
      (org-copilot-finish-request request)
      (unwind-protect
	  (when cancel
	    (condition-case err (funcall cancel)
	      (error (message "Org Copilot: cancellation failed: %S" err))))
	(when cleanup (funcall cleanup))))))

(defun org-copilot--invalidate-requests ()
  "Invalidate this source's entire session before cancelling its requests."
  (setq org-copilot--request-session nil)
  (dolist (token (copy-sequence org-copilot--requests))
    (org-copilot-cancel-request (list :request-token token))))

(defun org-copilot--retarget-session ()
  "Detach the old session when this source buffer visits another file."
  (org-copilot--invalidate-requests)
  (org-copilot--clear-ephemeral-session)
  (setq org-copilot--chat-messages-restored-p nil))

(defun org-copilot-comments ()
  "Return visible Copilot comments for the current buffer."
  (if (fboundp 'org-copilot-visible-comments)
      (org-copilot-visible-comments)
    (copy-sequence org-copilot--comments)))

(defun org-copilot--candidate-primary-hunk (candidate)
  "Return primary hunk from suggestion CANDIDATE, or its first hunk."
  (let ((hunks (plist-get candidate :hunks)))
    (or (cl-find-if (lambda (hunk) (plist-get hunk :primary)) hunks)
	(car hunks))))

(defun org-copilot--candidate-match-for-comment (source-file comment)
  "Return preferred suggestion candidate match for COMMENT in SOURCE-FILE."
  (when (and source-file
	     (fboundp 'org-suggestions-load-sidecar)
	     (fboundp 'org-suggestions-find-candidate))
    (let* ((ids (split-string (or (plist-get comment :suggestion-ids) "")
			      "[[:space:]]+" t))
	   (threads (org-suggestions-load-sidecar source-file)))
      (cl-loop for id in (reverse ids)
	       for match = (org-suggestions-find-candidate threads id)
	       when (and match (org-copilot--active-status-p
				(plist-get (cdr match) :status)))
	       return match
	       finally return (when-let* ((id (car (last ids))))
				(org-suggestions-find-candidate threads id))))))

(defun org-copilot--active-status-p (status)
  "Return non-nil when STATUS denotes an active suggestion candidate."
  (or (eq status 'active)
      (equal status "active")))

(defun org-copilot--active-candidate-for-comment (source-file comment)
  "Return active suggestion candidate linked to COMMENT in SOURCE-FILE."
  (when-let* ((match (org-copilot--candidate-match-for-comment source-file comment))
	      (candidate (cdr match)))
    (when (org-copilot--active-status-p (plist-get candidate :status))
      candidate)))

(defun org-copilot-linked-suggestion-id (comment source-file)
  "Return active linked suggestion id for COMMENT in SOURCE-FILE, or nil."
  (when-let* ((candidate (org-copilot--active-candidate-for-comment
			  source-file comment)))
    (plist-get candidate :id)))

(defun org-copilot-comment-suggestion-text (comment &optional source-file)
  "Return durable suggestion replacement linked to COMMENT.
When SOURCE-FILE is nil, use the current buffer file name.  Retired
comment-local `:suggestion' values are intentionally ignored."
  (when-let* ((source-file (or source-file
			       (plist-get comment :source-file)
			       buffer-file-name))
	      (candidate (org-copilot--active-candidate-for-comment
			  source-file comment))
	      (hunk (org-copilot--candidate-primary-hunk candidate)))
    (plist-get hunk :replacement)))

(defun org-copilot-comment-target-text (comment &optional source-file)
  "Return durable original text linked to COMMENT.
When no durable suggestion hunk is linked, fall back to COMMENT's target text."
  (or (when-let* ((source-file (or source-file
				   (plist-get comment :source-file)
				   buffer-file-name))
		  (candidate (org-copilot--active-candidate-for-comment
			      source-file comment))
		  (hunk (org-copilot--candidate-primary-hunk candidate)))
	(plist-get hunk :original))
      (plist-get comment :target-text)))

(defun org-copilot--comment-from-sidecar-comment (comment source-file)
  "Return a Copilot-compatible comment from sidecar COMMENT in SOURCE-FILE."
  (let* ((candidate (org-copilot--active-candidate-for-comment source-file comment))
	 (hunk (and candidate (org-copilot--candidate-primary-hunk candidate)))
	 (copy (copy-sequence comment))
	 (start (or (plist-get comment :target-start)
		    (plist-get comment :anchor-pos)))
	 (end (or (plist-get comment :target-end) start))
	 (candidate-status (plist-get candidate :status)))
    (setq copy (plist-put copy :source-file source-file))
    (setq copy (plist-put copy :status
			  (pcase candidate-status
			    ('accepted 'accepted)
			    ('dismissed 'dismissed)
			    ('stale 'stale)
			    (_ (if (equal (plist-get comment :status) "RESOLVED")
				   'dismissed
				 'active)))))
    (setq copy (plist-put copy :type 'inline))
    (setq copy (plist-put copy :source-start start))
    (setq copy (plist-put copy :source-end end))
    (setq copy (plist-put copy :summary (plist-get comment :body)))
    (when-let* ((original (plist-get hunk :original)))
      (setq copy (plist-put copy :target-text (string-trim original))))
    copy))

(defun org-copilot-durable-comments ()
  "Return durable Copilot-linked comments for the current source buffer."
  (when (and buffer-file-name (fboundp 'org-comments-collect))
    (let ((source-file buffer-file-name))
      (mapcar (lambda (comment)
		(org-copilot--comment-from-sidecar-comment comment source-file))
	      (cl-remove-if-not
	       (lambda (comment)
		 (or (equal (plist-get comment :provider) "org-copilot")
		     (plist-get comment :suggestion-thread-id)
		     (plist-get comment :suggestion-ids)))
	       (org-comments-collect (current-buffer) t))))))

(defun org-copilot-visible-comments ()
  "Return durable and ephemeral Copilot comments for the current buffer."
  (let ((comments (org-copilot-durable-comments)))
    (dolist (comment org-copilot--comments)
      (unless (cl-find (org-copilot-comment-id comment) comments
		       :key #'org-copilot-comment-id
		       :test #'equal)
	(push comment comments)))
    (nreverse comments)))

(defun org-copilot-find-visible-comment (id)
  "Return visible Copilot comment with ID."
  (cl-find id (org-copilot-visible-comments)
	   :key #'org-copilot-comment-id
	   :test #'equal))

(defun org-copilot-set-durable-comment-status (comment status)
  "Set durable sidecar COMMENT TODO state to STATUS."
  (when-let* ((sidecar-file (plist-get comment :sidecar-file))
	      (id (plist-get comment :id)))
    (with-current-buffer (find-file-noselect sidecar-file)
      (org-mode)
      (save-excursion
	(unless (org-comments-goto-id id)
	  (user-error "Comment %s not found in sidecar" id))
	(org-comments-set-entry-status status))
      (save-buffer))
    t))

(defun org-copilot-set-linked-suggestions-status (source-file comment status)
  "Set linked suggestion candidates for COMMENT in SOURCE-FILE to STATUS."
  (when (and source-file
	     (fboundp 'org-suggestions-load-sidecar)
	     (fboundp 'org-suggestions-find-candidate)
	     (fboundp 'org-suggestions-write-sidecar))
    (let* ((ids (split-string (or (plist-get comment :suggestion-ids) "")
			      "[[:space:]]+" t))
	   (threads (org-suggestions-load-sidecar source-file))
	   changed)
      (dolist (id ids)
	(when-let* ((match (org-suggestions-find-candidate threads id)))
	  (plist-put (cdr match) :status status)
	  (setq changed t)))
      (when changed
	(org-suggestions-write-sidecar source-file threads))
      changed)))

(defun org-copilot-request-contributor (request)
  "Return REQUEST's host-recorded contributor, or the generic Copilot label."
  (let ((contributor (plist-get request :contributor)))
    (if (and (stringp contributor) (not (string-empty-p contributor)))
	contributor
      "Copilot")))

(defun org-copilot-install-durable-comment (source-buffer comment &optional request)
  "Install COMMENT in SOURCE-BUFFER with REQUEST's host-recorded contributor."
  (unless (and (buffer-live-p source-buffer)
	       (fboundp 'org-comments-create-record)
	       (fboundp 'org-comments-append-to-sidecar))
    (user-error "Durable Org comments are unavailable"))
  (with-current-buffer source-buffer
    (unless buffer-file-name
      (setq buffer-file-name (make-temp-file "org-copilot-source" nil ".org"))
      (write-region (point-min) (point-max) buffer-file-name nil 'silent))
    (let* ((source-file buffer-file-name)
	   (start (plist-get comment :source-start))
	   (end (plist-get comment :source-end))
	   (body (or (plist-get comment :summary)
		     (plist-get comment :body)
		     "AI comment"))
	   record sidecar-file)
      (unless (and (integerp start) (integerp end) (<= start end))
	(user-error "AI comment has no durable source anchor"))
      (setq record (org-comments-create-record
		    source-file start end body
		    (plist-get comment :id) (org-copilot-request-contributor request)
		    (format-time-string "%FT%T%z")))
      (setq record (plist-put record :provider "org-copilot"))
      (setq record (plist-put record :org-copilot-session-id "default"))
      (setq sidecar-file (org-comments-append-to-sidecar record))
      (setq record (plist-put record :sidecar-file sidecar-file))
      (org-copilot--comment-from-sidecar-comment record source-file))))

(defun org-copilot-install-retained-comment (source-buffer comment request)
  "Install COMMENT's retained identity, preserving human resolution on redelivery.
Refuse incompatible content; use the existing durable comment store rather than
maintaining another receipt registry. REQUEST supplies the actual contributor."
  (with-current-buffer source-buffer
    (let* ((id (plist-get comment :retained-id))
	   (existing (org-copilot-find-visible-comment id)))
      (unless (and (stringp id) (not (string-empty-p id))
		   (equal id (plist-get comment :id)))
	(user-error "Retained comment requires its durable identity"))
      (if existing
	  (progn
	    (unless (and (equal (plist-get existing :body)
				(or (plist-get comment :summary) (plist-get comment :body)))
			 (equal (plist-get existing :target-text) (plist-get comment :target-text))
			 (equal (plist-get existing :author) (org-copilot-request-contributor request)))
	      (user-error "Retained comment identity has incompatible content"))
	    (plist-put (copy-sequence existing) :already-installed t))
	(org-copilot-install-durable-comment source-buffer comment request)))))

(defun org-copilot-add-comment (comment)
  "Add AI COMMENT to the current buffer session and return it.
COMMENT is normalized before insertion.  Adding another comment with the same
`:id' replaces the previous comment while preserving list order for other
comments."
  (let* ((normalized (org-copilot-normalize-comment comment))
	 (id (org-copilot-comment-id normalized)))
    (setq org-copilot--comments
	  (append (cl-remove id org-copilot--comments
			     :key #'org-copilot-comment-id
			     :test #'equal)
		  (list normalized)))
    normalized))

(defun org-copilot-set-comments (comments)
  "Replace current buffer session with normalized AI COMMENTS.
Return the normalized comments."
  (setq org-copilot--comments nil)
  (dolist (comment comments)
    (org-copilot-add-comment comment))
  (org-copilot-comments))

(defun org-copilot-find-comment (id)
  "Return Copilot comment with ID, or nil."
  (or (cl-find id (org-copilot-durable-comments)
	       :key #'org-copilot-comment-id
	       :test #'equal)
      (cl-find id org-copilot--comments
	       :key #'org-copilot-comment-id
	       :test #'equal)))

(defun org-copilot-update-comment (comment)
  "Replace AI COMMENT in the current buffer session and return it."
  (let* ((normalized (org-copilot-normalize-comment comment))
	 (id (org-copilot-comment-id normalized)))
    (unless (org-copilot-find-comment id)
      (error "No Org Copilot comment with id: %S" id))
    (setq org-copilot--comments
	  (mapcar (lambda (entry)
		    (if (equal (org-copilot-comment-id entry) id)
			normalized
		      entry))
		  org-copilot--comments))
    normalized))

(defun org-copilot-remove-comment (comment-or-id)
  "Remove COMMENT-OR-ID from the current buffer session."
  (let ((id (if (listp comment-or-id)
		(org-copilot-comment-id comment-or-id)
	      comment-or-id)))
    (setq org-copilot--comments
	  (cl-remove id org-copilot--comments
		     :key #'org-copilot-comment-id
		     :test #'equal))))

(defun org-copilot-chat-messages ()
  "Return ephemeral chat messages for the current buffer."
  (copy-sequence org-copilot--chat-messages))

(defun org-copilot-set-chat-messages (messages)
  "Replace current buffer chat transcript with MESSAGES."
  (org-copilot--invalidate-requests)
  (setq org-copilot--chat-messages messages))

(defun org-copilot-restore-chat-messages ()
  "Restore durable chat messages for the current buffer once."
  (when (and (not org-copilot--chat-messages-restored-p)
	     buffer-file-name
	     (fboundp 'org-copilot-sidecar-load-messages))
    (setq org-copilot--chat-messages-restored-p t)
    (when-let* ((messages (org-copilot-sidecar-load-messages buffer-file-name)))
      (let ((org-copilot--suppress-chat-persistence t))
	(org-copilot-set-chat-messages messages)))))

(defun org-copilot--restore-section-path-bounds (path)
  "Return cons bounds for section outline PATH in the current buffer."
  (when (listp path)
    (save-excursion
      (goto-char (point-min))
      (catch 'found
	(while (re-search-forward org-heading-regexp nil t)
	  (beginning-of-line)
	  (when (equal path (org-get-outline-path t t))
	    (throw 'found (cons (line-beginning-position) (line-end-position))))
	  (org-end-of-subtree t t))))))

(defun org-copilot--restore-section-title-bounds (title)
  "Return cons bounds for section TITLE in the current buffer."
  (when (and (stringp title) (not (string-empty-p title)))
    (save-excursion
      (goto-char (point-min))
      (catch 'found
	(while (re-search-forward org-heading-regexp nil t)
	  (beginning-of-line)
	  (when (string= title (org-get-heading t t t t))
	    (throw 'found (cons (line-beginning-position) (line-end-position))))
	  (org-end-of-subtree t t))))))

(defun org-copilot--restore-text-bounds (text)
  "Return unique cons bounds for TEXT in the current buffer, or nil."
  (let ((needle (and (stringp text) (string-trim text))))
    (when (and needle (not (string-empty-p needle)))
      (save-excursion
	(goto-char (point-min))
	(let (matches)
	  (while (search-forward needle nil t)
	    (push (cons (match-beginning 0) (match-end 0)) matches))
	  (and (= (length matches) 1) (car matches)))))))

(defun org-copilot--restore-hunk-bounds (hunk)
  "Return best visible source bounds for suggestion HUNK."
  (or (org-copilot--restore-section-title-bounds (plist-get hunk :section-title))
      (org-copilot--restore-section-path-bounds (plist-get hunk :section-path))
      (org-copilot--restore-text-bounds (plist-get hunk :original))
      (org-copilot--restore-text-bounds (plist-get hunk :anchor-text))))

(defun org-copilot--restore-primary-hunk (thread)
  "Return primary hunk from THREAD, or the first hunk."
  (let ((hunks (cl-mapcan (lambda (candidate)
			    (copy-sequence (plist-get candidate :hunks)))
			  (plist-get thread :candidates))))
    (or (cl-find-if (lambda (hunk) (plist-get hunk :primary)) hunks)
	(car hunks))))

(defun org-copilot--restore-existing-comment-id (comments thread-id)
  "Return existing linked comment id in COMMENTS for THREAD-ID."
  (when thread-id
    (when-let* ((comment (cl-find thread-id comments
				  :key (lambda (entry)
					 (plist-get entry :suggestion-thread-id))
				  :test #'equal)))
      (plist-get comment :id))))

(defun org-copilot--restore-create-thread-comment (source-file thread suggestion-ids)
  "Create a linked visible comment for THREAD in SOURCE-FILE."
  (when-let* ((hunk (org-copilot--restore-primary-hunk thread))
	      (bounds (org-copilot--restore-hunk-bounds hunk)))
    (let ((record (org-comments-create-record
		   source-file (car bounds) (cdr bounds)
		   (or (plist-get thread :summary) "AI suggestion")
		   nil "org-copilot" (format-time-string "%FT%T%z"))))
      (setq record (plist-put record :provider "org-copilot"))
      (setq record (plist-put record :org-copilot-session-id
			      (or (plist-get thread :session-id) "default")))
      (setq record (plist-put record :suggestion-thread-id
			      (plist-get thread :id)))
      (setq record (plist-put record :suggestion-ids
			      (string-join suggestion-ids " ")))
      (org-comments-append-to-sidecar record)
      (plist-get record :id))))

(defun org-copilot-restore-suggestion-comments ()
  "Backfill visible linked comments for durable Copilot suggestions."
  (when (and buffer-file-name
	     (fboundp 'org-suggestions-load-sidecar)
	     (fboundp 'org-suggestions-write-sidecar)
	     (fboundp 'org-comments-create-record)
	     (fboundp 'org-comments-append-to-sidecar)
	     (fboundp 'org-comments-collect))
    (let* ((source-file buffer-file-name)
	   (threads (org-suggestions-load-sidecar source-file))
	   (comments (org-comments-collect (current-buffer) t))
	   changed)
      (dolist (thread threads)
	(when (and (equal (plist-get thread :provider) "org-copilot")
		   (not (plist-get thread :comment-id)))
	  (let* ((thread-id (plist-get thread :id))
		 (suggestion-ids (mapcar (lambda (candidate)
					   (plist-get candidate :id))
					 (plist-get thread :candidates)))
		 (comment-id (or (org-copilot--restore-existing-comment-id
				  comments thread-id)
				 (org-copilot--restore-create-thread-comment
				  source-file thread suggestion-ids))))
	    (if comment-id
		(progn
		  (setq comments (org-comments-collect (current-buffer) t))
		  (plist-put thread :comment-id comment-id)
		  (setq changed t))
	      (when (fboundp 'org-copilot-debug-record)
		(org-copilot-debug-record
		 "Suggestion restore comment not anchored"
		 :source-file source-file
		 :thread-id thread-id
		 :primary-hunk (org-copilot--restore-primary-hunk thread)))))))
      (when changed
	(org-suggestions-write-sidecar source-file threads)))))

(defun org-copilot-restore-durable-artifacts ()
  "Restore durable Copilot chat and visible suggestion artifacts."
  (org-copilot-restore-chat-messages)
  (org-copilot-restore-suggestion-comments))

(defun org-copilot-record-chat-findings (request finding-ids)
  "Associate newly installed FINDING-IDS with REQUEST's producing response."
  (with-current-buffer (plist-get request :source-buffer)
    (when-let* ((message (cl-find-if
			  (lambda (item)
			    (and (eq (plist-get item :role) 'assistant)
				 (eq (plist-get item :request-id)
				     (plist-get request :request-token))))
			  org-copilot--chat-messages)))
      (when (and buffer-file-name finding-ids (plist-get message :persisted))
	(org-copilot-sidecar-record-findings
	 buffer-file-name (plist-get message :message-id) finding-ids))
      (plist-put message :finding-ids finding-ids))))

(defun org-copilot-add-chat-message (role content &optional comment-id context-id request finding-ids)
  "Add a chat message with ROLE and CONTENT to the current buffer session.
When COMMENT-ID is non-nil, associate the message with that AI comment.
When CONTEXT-ID is non-nil, associate the message with another chat context.
REQUEST supplies an ephemeral identity for pending and completed messages.
FINDING-IDS are durable comments produced by this response, not focus metadata."
  (let ((message (list :role role
		       :content content
		       :comment-id comment-id
		       :context-id context-id
		       :message-id (format "message-%s-%08x" (format-time-string "%Y%m%dT%H%M%S%N") (random #x100000000))
		       :finding-ids finding-ids
		       :request-id (plist-get request :request-token)
		       :created-at (format-time-string "%FT%T%z"))))
    (setq org-copilot--chat-messages
	  (append org-copilot--chat-messages (list message)))
    (when (and buffer-file-name
	       (not org-copilot--suppress-chat-persistence)
	       (memq role '(user assistant))
	       (fboundp 'org-copilot-sidecar-append-message))
      (org-copilot-sidecar-append-message
       buffer-file-name role content
       (list "ORG_COPILOT_MESSAGE_ID" (plist-get message :message-id)
	     "ORG_COPILOT_FINDING_IDS" (and finding-ids (json-serialize (vconcat finding-ids)))
	     "ORG_COPILOT_CREATED_AT" (plist-get message :created-at)
	     "ORG_COPILOT_CONTEXT_ID" context-id
	     "ORG_COPILOT_COMMENT_ID" comment-id))
      (plist-put message :persisted t))
    message))

(defun org-copilot-remove-pending-chat-message (request)
  "Remove only the pending message owned by REQUEST, never a context neighbour."
  (when-let* ((token (plist-get request :request-token)))
    (setq org-copilot--chat-messages
	  (cl-remove-if
	   (lambda (message)
	     (and (eq (plist-get message :role) 'pending)
		  (eq (plist-get message :request-id) token)))
	   org-copilot--chat-messages))))

(defun org-copilot--current-session-id ()
  "Return current Copilot session id."
  "default")

(defun org-copilot--archive-session-artifacts (source-file session-id)
  "Archive durable Copilot artifacts for SOURCE-FILE SESSION-ID."
  (when (and source-file (fboundp 'org-copilot-sidecar-archive-session))
    (org-copilot-sidecar-archive-session source-file session-id))
  (when (and source-file (fboundp 'org-comments-archive-provider-session-comments))
    (let ((comments-sidecar (org-comments-sidecar-path source-file)))
      (when (file-exists-p comments-sidecar)
	(org-comments-archive-provider-session-comments
	 comments-sidecar "org-copilot" session-id))))
  (when (and source-file
	     (fboundp 'org-suggestions-archive-provider-session-threads))
    (let ((suggestions-sidecar (org-suggestions-sidecar-path source-file)))
      (when (file-exists-p suggestions-sidecar)
	(org-suggestions-archive-provider-session-threads
	 suggestions-sidecar "org-copilot" session-id)))))

(defun org-copilot--delete-session-artifacts (source-file session-id)
  "Delete durable Copilot artifacts for SOURCE-FILE SESSION-ID."
  (when (and source-file (fboundp 'org-copilot-sidecar-delete-session))
    (org-copilot-sidecar-delete-session source-file session-id))
  (when (and source-file (fboundp 'org-comments-delete-provider-session-comments))
    (let ((comments-sidecar (org-comments-sidecar-path source-file)))
      (when (file-exists-p comments-sidecar)
	(org-comments-delete-provider-session-comments
	 comments-sidecar "org-copilot" session-id))))
  (when (and source-file
	     (fboundp 'org-suggestions-delete-provider-session-threads))
    (let ((suggestions-sidecar (org-suggestions-sidecar-path source-file)))
      (when (file-exists-p suggestions-sidecar)
	(org-suggestions-delete-provider-session-threads
	 suggestions-sidecar "org-copilot" session-id)))))

(defun org-copilot--clear-ephemeral-session ()
  "Clear ephemeral Org Copilot state for the current buffer."
  (setq org-copilot--comments nil)
  (setq org-copilot--chat-messages nil)
  (setq org-copilot--chat-messages-restored-p t)
  (setq org-copilot-chat-focus-comment-id nil)
  (setq org-copilot-chat-context '(:type full-document))
  (when (fboundp 'org-copilot-delete-overlays)
    (org-copilot-delete-overlays)))

(defun org-copilot-clear-session (&optional preserve-artifacts)
  "Clear current Copilot session.
By default, archive durable Copilot chat, comments, and suggestions for the
current session.  With PRESERVE-ARTIFACTS, only clear ephemeral UI state."
  (interactive "P")
  (let ((source-file buffer-file-name)
	(session-id (org-copilot--current-session-id)))
    (org-copilot--invalidate-requests)
    (unless preserve-artifacts
      (org-copilot--archive-session-artifacts source-file session-id))
    (org-copilot--clear-ephemeral-session)))

(defun org-copilot-erase-session ()
  "Hard-delete durable artifacts for the current Copilot session."
  (interactive)
  (when (yes-or-no-p "Hard-delete current Org Copilot session artifacts? ")
    (org-copilot--invalidate-requests)
    (org-copilot--delete-session-artifacts
     buffer-file-name (org-copilot--current-session-id))
    (org-copilot--clear-ephemeral-session)))

(provide 'org-copilot-session)
;;; org-copilot-session.el ends here
