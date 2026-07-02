;;; org-confluence-publish.el --- Publish Org buffers to Confluence -*- lexical-binding: t; -*-

;;; Commentary:
;; Publish, create, upload assets, and inline-comment preflight orchestration for
;; Org buffers backed by Confluence pages.

;;; Code:

(require 'browse-url)
(require 'cl-lib)
(require 'org)
(require 'org-comments)
(require 'org-comments-sidecar)
(require 'seq)
(require 'subr-x)

(require 'org-confluence-api)
(require 'org-confluence-comments-import)
(require 'org-confluence-comments-push)
(require 'org-confluence-comments-remote)
(require 'org-confluence-export)
(require 'org-confluence-inline-comments)
(require 'org-confluence-inline-repair)
(require 'org-confluence-page)
(require 'org-confluence-process)
(require 'org-confluence-response)
(require 'org-confluence-sync)

(defcustom org-confluence-publish-preserve-inline-comments t
  "Whether publishing should reinsert Confluence inline comment markers.

When non-nil, existing page storage and inline comment metadata are fetched
before page update.  Active inline comments with marker references are wrapped
back around matching selected text in the newly exported storage XHTML so
Confluence keeps their anchors."
  :type 'boolean
  :group 'org-confluence-api)

(defun org-confluence-publish-asset-filename-map (assets)
  "Return export filename mapping for image ASSETS."
  (mapcar (lambda (asset)
	    (cons (plist-get asset :source-link)
		  (plist-get asset :filename)))
	  assets))

(defun org-confluence-publish-duplicate-attachment-error-p (error)
  "Return non-nil when ERROR is cfl's duplicate attachment failure."
  (string-match-p "Cannot add a new attachment with same file name as an existing attachment"
		  (error-message-string error)))

(defun org-confluence-publish-upload-asset (page-id asset upload-directory)
  "Upload one image ASSET to Confluence PAGE-ID from UPLOAD-DIRECTORY."
  (if (plist-get asset :missing-source)
      (message "Confluence attachment source missing, reusing existing %s"
	       (plist-get asset :filename))
    (let ((upload-path (expand-file-name (plist-get asset :filename) upload-directory)))
      (copy-file (plist-get asset :source-path) upload-path t)
      (condition-case err
	  (org-confluence-process-run
	   (org-confluence-api--attachment-upload-command page-id upload-path))
	(user-error
	 (if (org-confluence-publish-duplicate-attachment-error-p err)
	     (message "Confluence attachment already exists, reusing %s"
		      (plist-get asset :filename))
	   (signal (car err) (cdr err))))))))

(defun org-confluence-publish-unique-upload-assets (assets)
  "Return ASSETS deduplicated by generated attachment filename."
  (let ((seen nil)
	(unique nil))
    (dolist (asset assets (nreverse unique))
      (let ((filename (plist-get asset :filename)))
	(unless (member filename seen)
	  (push filename seen)
	  (push asset unique))))))

(defun org-confluence-publish-upload-assets (page-id assets)
  "Upload image ASSETS to Confluence PAGE-ID."
  (when assets
    (let ((upload-directory (make-temp-file "org-confluence-assets-" t)))
      (unwind-protect
	  (dolist (asset (org-confluence-publish-unique-upload-assets assets))
	    (org-confluence-publish-upload-asset page-id asset upload-directory))
	(delete-directory upload-directory t)))))

(defun org-confluence-publish-subpage-root-position ()
  "Return current subtree root position when point is at a subpage export root."
  (when (and (not (org-before-first-heading-p))
	     (org-entry-get nil "CONFLUENCE_PAGE_ID"))
    (point)))

(defun org-confluence-publish-inside-another-subpage-p (root-position)
  "Return non-nil when current heading is inside another subpage.
ROOT-POSITION is the current subtree root position."
  (save-excursion
    (let ((inside nil))
      (while (and (not inside) (org-up-heading-safe))
	(when (and (> (point) root-position)
		   (org-entry-get nil "CONFLUENCE_PAGE_ID"))
	  (setq inside t)))
      inside)))

(defun org-confluence-publish-direct-subpage-markers (&optional subtreep)
  "Return markers for direct child subpages in current export scope.
When SUBTREEP is non-nil, the current subtree root itself is not included."
  (save-excursion
    (save-restriction
      (let ((root-position (if subtreep
			       (progn (org-back-to-heading t) (point))
			     (point-min)))
	    markers)
	(when subtreep
	  (org-narrow-to-subtree))
	(org-map-entries
	 (lambda ()
	   (when (and (org-entry-get nil "CONFLUENCE_PAGE_ID")
		      (not (= (point) root-position))
		      (not (org-confluence-publish-inside-another-subpage-p root-position)))
	     (push (point-marker) markers)))
	 nil (when subtreep 'tree))
	(nreverse markers)))))

(defvar org-confluence-publish--skip-inline-comment-preflight nil
  "Non-nil means recursive publish calls skip inline comment preflight.")

(defvar org-confluence-publish--pages-needing-inline-marker-preservation nil
  "Page IDs whose active inline comment markers should be preserved on update.")

(defvar org-confluence-publish--pages-needing-inline-marker-repair nil
  "Page IDs with safely repairable missing inline comment markers.")

(defvar org-confluence-publish--force-remote-version-check nil
  "Non-nil means publish skips stale remote version checks.")

(defun org-confluence-publish--interactive-repair-p ()
  "Return non-nil when publish may prompt for inline marker repair."
  (not noninteractive))

(defun org-confluence-publish-subpages (subtreep visible-only body-only ext-plist)
  "Publish direct child subpages for current export scope before their parent."
  (dolist (marker (org-confluence-publish-direct-subpage-markers subtreep))
    (save-excursion
      (goto-char marker)
      (org-confluence-publish nil t visible-only body-only ext-plist))))

(defun org-confluence-publish--target-page-ids (subtreep)
  "Return Confluence page IDs that would be updated for SUBTREEP publish."
  (save-excursion
    (save-restriction
      (let ((root-id (org-confluence-api--page-id-from-buffer subtreep))
	    ids)
	(when subtreep
	  (org-back-to-heading t)
	  (org-narrow-to-subtree))
	(when root-id
	  (push (format "%s" root-id) ids))
	(org-map-entries
	 (lambda ()
	   (when-let* ((id (org-entry-get nil "CONFLUENCE_PAGE_ID")))
	     (push (format "%s" id) ids)))
	 nil (when subtreep 'tree))
	(delete-dups (nreverse ids))))))

(defun org-confluence-publish--stored-page-version (&optional subtreep)
  "Return stored Confluence page version for current buffer or subtree."
  (or (when subtreep
	(org-confluence-api--property-from-subtree "CONFLUENCE_PAGE_VERSION"))
      (org-confluence-api--keyword-from-buffer "CONFLUENCE_PAGE_VERSION")))

(defun org-confluence-publish--remote-page-version (page-id)
  "Return remote Confluence version string for PAGE-ID."
  (let* ((response (org-confluence-api--get-page page-id "storage"))
	 (page (org-confluence-response-json-alist
		(org-confluence-response-body response)))
	 (version (alist-get 'number (alist-get 'version page))))
    (when version
      (format "%s" version))))

(defun org-confluence-publish--ensure-current-page-version (page-id subtreep force)
  "Refuse publishing PAGE-ID when remote version differs from stored metadata.
SUBTREEP selects subtree metadata.  FORCE skips the check."
  (unless (or force org-confluence-publish--force-remote-version-check)
    (when-let* ((stored-version (org-confluence-publish--stored-page-version subtreep))
		(remote-version (org-confluence-publish--remote-page-version page-id)))
      (unless (equal remote-version stored-version)
	(user-error
	 "Refusing to publish Confluence page %s: remote version %s differs from last synced version %s; run org-confluence-sync-page-current or org-confluence-publish-force"
	 page-id remote-version stored-version)))))

(defun org-confluence-publish--remote-page-after-publish (page-id)
  "Return PAGE-ID storage-format page metadata after publish, if available."
  (condition-case nil
      (let ((page (org-confluence-response-json-alist
		   (org-confluence-response-body
		    (org-confluence-api--get-page page-id "storage")))))
	(cons (org-confluence-sync--page-version-optional page)
	      (org-confluence-sync--page-storage page)))
    (error nil)))

(defun org-confluence-publish--stamp-sync-metadata (page-id storage subtreep)
  "Stamp successful publish metadata for PAGE-ID and STORAGE.

Whole-buffer publishes record enough sync metadata for a subsequent safe pull.
SUBTREEP publishes do not stamp global file-level sync metadata."
  (unless subtreep
    (let* ((remote (org-confluence-publish--remote-page-after-publish page-id))
	   (remote-version (car remote))
	   (remote-storage (or (cdr remote) storage)))
      (org-confluence-sync--stamp-metadata
       remote-version
       (org-confluence-sync--sha256 remote-storage)
       (org-confluence-sync--local-org-hash))
      (when buffer-file-name
	(save-buffer)))))

(defun org-confluence-publish--inline-comment-blocking-p (comment)
  "Return non-nil when inline COMMENT should block page publish.

Anchored comments with marker refs and selected text are preserved during page
update, so they do not block.  Active comments without enough anchor metadata
still block because they cannot be safely reinserted."
  (let ((status (org-confluence-comments-remote-resolution-status comment)))
    (and (not (member status '("resolved" "dangling")))
	 (or (not org-confluence-publish-preserve-inline-comments)
	     (not (org-confluence-api--present-string-p
		   (org-confluence-comments-remote-inline-marker-ref comment)))
	     (not (org-confluence-api--present-string-p
		   (org-confluence-comments-remote-inline-target-text comment)))))))

(defun org-confluence-publish--sidecar-comment-property (sidecar-file comment-id property)
  "Return PROPERTY for COMMENT-ID in SIDECAR-FILE, or nil."
  (when (and sidecar-file comment-id (file-exists-p sidecar-file))
    (with-temp-buffer
      (insert-file-contents sidecar-file)
      (org-mode)
      (when (org-comments-goto-id comment-id)
	(org-entry-get nil property)))))

(defun org-confluence-publish--sidecar-remote-property (sidecar-file remote-id property)
  "Return PROPERTY for REMOTE-ID in SIDECAR-FILE, or nil."
  (when (and sidecar-file remote-id (file-exists-p sidecar-file))
    (with-temp-buffer
      (insert-file-contents sidecar-file)
      (org-mode)
      (goto-char (point-min))
      (cl-loop while (re-search-forward org-heading-regexp nil t)
	       do (goto-char (match-beginning 0))
	       when (equal remote-id (org-entry-get nil "ORG_COMMENTS_REMOTE_ID"))
	       return (org-entry-get nil property)
	       do (forward-line 1)))))

(defun org-confluence-publish--sidecar-remote-missing-p (sidecar-file record)
  "Return non-nil when sidecar says RECORD is remote-missing."
  (let ((local-id (plist-get record :local-comment-id))
	(remote-id (plist-get record :comment-id)))
    (or (equal "missing" (plist-get record :remote-state))
	(equal "missing"
	       (org-confluence-publish--sidecar-comment-property
		sidecar-file local-id "ORG_COMMENTS_REMOTE_STATE"))
	(equal "missing"
	       (org-confluence-publish--sidecar-remote-property
		sidecar-file remote-id "ORG_COMMENTS_REMOTE_STATE")))))

(defun org-confluence-publish--inline-comment-record (page-id comment &optional sidecar-file source-file)
  "Return report record for PAGE-ID inline COMMENT."
  (let* ((remote-id (org-confluence-comments-remote-id comment))
	 (local-id (or (and sidecar-file
			    (org-comments-local-id-for-remote-id sidecar-file remote-id))
		       (and remote-id (format "remote-confluence-%s" remote-id))))
	 (remote-state (org-confluence-publish--sidecar-comment-property
			sidecar-file local-id "ORG_COMMENTS_REMOTE_STATE")))
    (list :page-id page-id
	  :comment-id remote-id
	  :local-comment-id local-id
	  :source-file source-file
	  :remote-state remote-state
	  :author (org-confluence-comments-remote-author-name comment)
	  :status (or (org-confluence-comments-remote-resolution-status comment) "unknown")
	  :target-text (org-confluence-comments-remote-inline-target-text comment)
	  :anchored (org-confluence-comments-push-remote-inline-anchor-confirmed-p comment))))

(defun org-confluence-publish--preflight-record-link (record)
  "Return actionable Org link text for preflight RECORD."
  (let ((source-file (plist-get record :source-file))
	(local-id (plist-get record :local-comment-id))
	(remote-id (plist-get record :comment-id)))
    (if (and source-file local-id)
	(org-comments-make-link source-file local-id (format "comment %s" remote-id))
      (format "comment %s" (or remote-id "<unknown>")))))

(defun org-confluence-publish--preflight-next-link ()
  "Move point to the next link in the Confluence publish preflight report."
  (interactive)
  (if (fboundp 'org-next-link)
      (org-next-link)
    (unless (re-search-forward org-link-any-re nil t)
      (goto-char (point-min))
      (re-search-forward org-link-any-re nil t))))

(defun org-confluence-publish--record-sidecar-file (record)
  "Return sidecar file for preflight RECORD, or nil."
  (when-let* ((source-file (plist-get record :source-file)))
    (org-comments-sidecar-path source-file)))

(defun org-confluence-publish--record-remote-missing-p (record)
  "Return non-nil when RECORD is marked remote-missing in its sidecar."
  (let ((sidecar-file (org-confluence-publish--record-sidecar-file record)))
    (org-confluence-publish--sidecar-remote-missing-p sidecar-file record)))

(defun org-confluence-publish--nonblocking-record-p (record)
  "Return non-nil when RECORD should not block publish."
  (or (equal "dangling" (plist-get record :status))
      (org-confluence-publish--record-remote-missing-p record)))

(defun org-confluence-publish--reclassify-nonblocking-records (report)
  "Move remote-missing blockers in REPORT to the non-blocking list."
  (let (blockers nonblocking)
    (dolist (record (plist-get report :blockers))
      (if (org-confluence-publish--nonblocking-record-p record)
	  (progn
	    (plist-put record :remote-state "missing")
	    (push record nonblocking))
	(push record blockers)))
    (plist-put report :blockers (nreverse blockers))
    (plist-put report :dangling
	       (append (nreverse nonblocking) (plist-get report :dangling)))
    report))

(defun org-confluence-publish--preflight-report-buffer (report &optional force)
  "Show inline comment preflight REPORT.  FORCE notes non-aborting publish."
  (let ((buffer (get-buffer-create "*Org Confluence Publish Preflight*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
	(erase-buffer)
	(insert "Org Confluence publish preflight\n")
	(when force
	  (insert "FORCE: publish will continue despite blocking inline comments.\n"))
	(insert "\n")
	(insert "* Blocking active inline comments\n")
	(if-let* ((blockers (plist-get report :blockers)))
	    (dolist (record blockers)
	      (insert (format "- page %s %s [%s]%s%s%s\n"
			      (plist-get record :page-id)
			      (org-confluence-publish--preflight-record-link record)
			      (plist-get record :status)
			      (if (plist-get record :anchored) " anchored" " anchor-unconfirmed")
			      (if-let* ((reason (plist-get record :repair-reason)))
				  (format " reason=%s" reason)
				"")
			      (if-let* ((text (plist-get record :target-text)))
				  (format " — %s" text)
				""))))
	  (insert "None.\n"))
	(insert "\n* Safely repairable missing inline markers\n")
	(if-let* ((repairable (plist-get report :repairable)))
	    (dolist (record repairable)
	      (insert (format "- page %s %s [%s]%s%s\n"
			      (plist-get record :page-id)
			      (org-confluence-publish--preflight-record-link record)
			      (or (plist-get record :status) "open")
			      (if-let* ((count (plist-get record :candidate-count)))
				  (format " candidates=%s" count)
				"")
			      (if-let* ((text (plist-get record :target-text)))
				  (format " — %s" text)
				""))))
	  (insert "None.\n"))
	(insert "\n* Already dangling or remote-missing / not blocking\n")
	(if-let* ((dangling (plist-get report :dangling)))
	    (dolist (record dangling)
	      (insert (format "- page %s %s [%s%s]%s\n"
			      (plist-get record :page-id)
			      (org-confluence-publish--preflight-record-link record)
			      (plist-get record :status)
			      (if-let* ((state (plist-get record :remote-state)))
				  (format ", %s" state)
				"")
			      (if-let* ((text (plist-get record :target-text)))
				  (format " — %s" text)
				""))))
	  (insert "None.\n"))
	(insert "\n* Updated sidecars\n")
	(if-let* ((sidecars (plist-get report :sidecars)))
	    (dolist (sidecar (delete-dups sidecars))
	      (insert (format "- %s\n" sidecar)))
	  (insert "None.\n"))
	(goto-char (point-min))
	(org-mode)
	(use-local-map (copy-keymap org-mode-map))
	(dolist (key (list (kbd "TAB") (kbd "<tab>") (kbd "C-i")))
	  (local-set-key key #'org-confluence-publish--preflight-next-link))
	(setq buffer-read-only t)))
    (display-buffer buffer)
    buffer))

(defun org-confluence-publish--inline-comment-preflight (page-ids &optional force)
  "Import and classify inline comments for PAGE-IDS before publish.
When FORCE is nil, signal `user-error' if active inline comments would be
orphaned by a page update."
  (let ((report (list :blockers nil :dangling nil :repairable nil :sidecars nil))
	(body-format "storage"))
    (dolist (page-id page-ids)
      (let* ((response (org-confluence-api--list-page-comments
			page-id "inline-comments" body-format))
	     (comments (org-confluence-response-comment-results response))
	     (sidecar (and buffer-file-name
			   (org-comments-sidecar-path buffer-file-name)))
	     page-storage)
	(cl-labels ((storage ()
		      (or page-storage
			  (setq page-storage
				(org-confluence-response-page-body-storage-value
				 (org-confluence-api--get-page page-id "storage"))))))
	  (org-confluence-comments-import-remote-comments
	   page-id "inline-comments" body-format
	   #'org-confluence-comments-import-append-inline t)
	  (when sidecar
	    (plist-put report :sidecars (cons sidecar (plist-get report :sidecars))))
	  (dolist (comment comments)
	    (let ((record (org-confluence-publish--inline-comment-record
			   page-id comment sidecar buffer-file-name)))
	      (cond
	       ((or (equal "dangling" (plist-get record :status))
		    (org-confluence-publish--sidecar-remote-missing-p sidecar record))
		(plist-put record :remote-state "missing")
		(plist-put report :dangling (cons record (plist-get report :dangling))))
	       ((org-confluence-publish--inline-comment-blocking-p comment)
		(plist-put report :blockers (cons record (plist-get report :blockers))))
	       ((org-confluence-inline-repair-comment-marker-present-p (storage) comment)
		(unless (member (format "%s" page-id)
				org-confluence-publish--pages-needing-inline-marker-preservation)
		  (push (format "%s" page-id)
			org-confluence-publish--pages-needing-inline-marker-preservation)))
	       (t
		(let ((candidate (org-confluence-inline-repair-candidate-for-comment
				  (storage) comment buffer-file-name)))
		  (setq record (append record
				       (list :candidate-count (plist-get candidate :candidate-count)
					     :repair-reason (plist-get candidate :reason))))
		  (if (and (plist-member candidate :start)
			   (not (plist-get candidate :reason)))
		      (progn
			(plist-put report :repairable (cons record (plist-get report :repairable)))
			(unless (member (format "%s" page-id)
					org-confluence-publish--pages-needing-inline-marker-repair)
			  (push (format "%s" page-id)
				org-confluence-publish--pages-needing-inline-marker-repair)))
		    (plist-put report :blockers (cons record (plist-get report :blockers))))))))))))
    (plist-put report :blockers (nreverse (plist-get report :blockers)))
    (plist-put report :repairable (nreverse (plist-get report :repairable)))
    (plist-put report :dangling (nreverse (plist-get report :dangling)))
    (org-confluence-publish--reclassify-nonblocking-records report)
    (when (or (plist-get report :blockers)
	      (plist-get report :repairable)
	      (plist-get report :dangling))
      (org-confluence-publish--preflight-report-buffer report force))
    (when (and (not force) (plist-get report :blockers))
      (user-error "Refusing to publish: %s active inline Confluence comment(s) would lose anchors"
		  (length (plist-get report :blockers))))
    (when (plist-get report :repairable)
      (cond
       ((not (org-confluence-publish--interactive-repair-p))
	(user-error "Refusing to publish: %s inline comment anchor(s) need repair; run org-confluence-inline-repair-comment-anchors"
		    (length (plist-get report :repairable))))
       ((yes-or-no-p (format "Repair %s missing inline comment anchor(s) before publishing? "
			     (length (plist-get report :repairable))))
	(dolist (repair-page-id org-confluence-publish--pages-needing-inline-marker-repair)
	  (org-confluence-inline-repair-comment-anchors repair-page-id t)))
       (t
	(user-error "Refusing to publish with unrepaired inline comment anchors"))))
    report))

;;;###autoload
(defun org-confluence-publish (&optional async subtreep visible-only body-only ext-plist)
  "Publish the current Org buffer or subtree to an existing Confluence page.

The document must contain #+CONFLUENCE_PAGE_ID.  When SUBTREEP is non-nil, a
CONFLUENCE_PAGE_ID Org property on the current subtree takes precedence.  The
Org document is exported to Confluence Storage Format XHTML and passed to `cfl
page edit --file --storage'.  When the buffer records a last-synced Confluence
version, publishing refuses to edit if the remote page has advanced since that
version.  Interactively, a prefix argument forces publishing despite stale
remote-version metadata and active inline comment anchors.  ASYNC, SUBTREEP,
VISIBLE-ONLY, BODY-ONLY, and EXT-PLIST follow Org export conventions."
  (interactive (list current-prefix-arg))
  (let ((force (and (called-interactively-p 'interactive) async))
	(top-level-p (not org-confluence-publish--skip-inline-comment-preflight)))
    (when top-level-p
      (setq org-confluence-publish--pages-needing-inline-marker-preservation nil
	    org-confluence-publish--pages-needing-inline-marker-repair nil)
      (org-confluence-publish--inline-comment-preflight
       (org-confluence-publish--target-page-ids subtreep) force))
    (let ((org-confluence-publish--skip-inline-comment-preflight t))
      (let* ((page-id (org-confluence-api--page-id-from-buffer subtreep))
	     (assets (progn
		       (org-confluence-publish--ensure-current-page-version
			page-id subtreep force)
		       (org-confluence-publish-subpages
			subtreep visible-only body-only ext-plist)
		       (org-confluence-image-assets subtreep)))
	     (export-plist (append (when subtreep '(:confluence-omit-root-heading t))
				   ext-plist))
	     (published-storage nil)
	     (xhtml-file nil))
	(unwind-protect
	    (progn
	      (let ((xhtml (org-confluence-export nil subtreep visible-only body-only
						  (append (list :confluence-image-filenames
								(org-confluence-publish-asset-filename-map assets))
							  export-plist))))
		(setq published-storage
		      (org-confluence-inline-comments-preserve-inline-comments page-id xhtml))
		(setq xhtml-file
		      (org-confluence-process-write-temp-xhtml published-storage)))
	      (org-confluence-publish-upload-assets page-id assets)
	      (org-confluence-process-run
	       (org-confluence-api--page-update-command page-id xhtml-file))
	      (org-confluence-publish--stamp-sync-metadata
	       page-id published-storage subtreep)
	      (message "Published Org buffer to Confluence page %s" page-id)
	      page-id)
	  (when (and xhtml-file (file-exists-p xhtml-file))
	    (delete-file xhtml-file))
	  (when top-level-p
	    (setq org-confluence-publish--pages-needing-inline-marker-preservation nil
		  org-confluence-publish--pages-needing-inline-marker-repair nil)))))))

;;;###autoload
(defun org-confluence-publish-force (&optional subtreep visible-only body-only ext-plist)
  "Publish to Confluence even when remote version or inline-comment checks block."
  (interactive)
  (setq org-confluence-publish--pages-needing-inline-marker-preservation nil
	org-confluence-publish--pages-needing-inline-marker-repair nil)
  (org-confluence-publish--inline-comment-preflight
   (org-confluence-publish--target-page-ids subtreep) t)
  (setq org-confluence-publish--pages-needing-inline-marker-preservation nil
	org-confluence-publish--pages-needing-inline-marker-repair nil)
  (let ((org-confluence-publish--skip-inline-comment-preflight t)
	(org-confluence-publish--force-remote-version-check t))
    (org-confluence-publish nil subtreep visible-only body-only ext-plist)))

;;;###autoload
(defun org-confluence-publish-from-export-dispatch
    (&optional async subtreep visible-only body-only ext-plist)
  "Publish or create through `org-export-dispatch' using Org export options."
  (interactive)
  (org-confluence-publish-dwim nil nil async subtreep visible-only body-only ext-plist))

;;;###autoload
(defun org-confluence-open-page (&optional page-id space)
  "Open Confluence PAGE-ID in browser using optional SPACE."
  (interactive)
  (let* ((id (or page-id (org-confluence-api--page-id-from-buffer)))
	 (page-space (or space (org-confluence-api--space-from-buffer)))
	 (url (org-confluence-api--page-url id page-space)))
    (browse-url url)
    url))

;;;###autoload
(defun org-confluence-publish-and-open-from-export-dispatch
    (&optional async subtreep visible-only body-only ext-plist)
  "Publish or create through `org-export-dispatch', then open the page."
  (interactive)
  (let* ((page-id (org-confluence-publish-dwim nil nil async subtreep visible-only body-only ext-plist))
	 (space (org-confluence-api--space-from-buffer)))
    (org-confluence-open-page page-id space)))

(defun org-confluence-publish--record-created-page (page-id space)
  "Record created PAGE-ID and SPACE metadata in the current Org buffer."
  (org-confluence-page-insert-metadata-after-keywords
   `(("CONFLUENCE_PAGE_ID" . ,page-id)
     ("CONFLUENCE_SPACE" . ,space))))

;;;###autoload
(defun org-confluence-publish-dwim
    (&optional title parent-id async subtreep visible-only body-only ext-plist)
  "Publish current Org buffer to Confluence, updating or creating as needed.

When #+CONFLUENCE_PAGE_ID is present, update that page.  Otherwise use
#+CONFLUENCE_SPACE or `org-confluence-api-default-space' and create a new page
using TITLE, prompting interactively when needed.  Optional PARENT-ID is passed
to cfl as the parent page.  When the new page ID can be parsed from cfl output,
record Confluence metadata, upload image assets, and re-save the page content.
ASYNC, SUBTREEP, VISIBLE-ONLY, BODY-ONLY, and EXT-PLIST follow Org export
conventions."
  (interactive)
  (let ((page-id (org-confluence-api--page-id-from-buffer subtreep)))
    (if page-id
	(org-confluence-publish async subtreep visible-only body-only ext-plist)
      (let ((assets (org-confluence-image-assets subtreep)))
	(let* ((space (org-confluence-api--space-from-buffer))
	       (page-title (or title
			       (org-confluence-page-title-from-buffer)
			       (read-string "Confluence page title: ")))
	       (xhtml-file nil))
	  (unwind-protect
	      (let ((command nil)
		    (output nil)
		    (created-page-id nil))
		(setq xhtml-file
		      (org-confluence-process-write-temp-xhtml
		       (org-confluence-export async subtreep visible-only body-only
					      (append (list :confluence-image-filenames
							    (org-confluence-publish-asset-filename-map assets))
						      ext-plist))))
		(setq command (org-confluence-api--page-create-command space page-title xhtml-file parent-id))
		(setq output (org-confluence-process-run-output command))
		(setq created-page-id (org-confluence-page-created-page-id output))
		(when created-page-id
		  (org-confluence-publish--record-created-page created-page-id space)
		  (org-confluence-publish-upload-assets created-page-id assets)
		  (org-confluence-process-run
		   (org-confluence-api--page-update-command created-page-id xhtml-file)))
		(message "Created Confluence page %s in space %s%s"
			 page-title space
			 (if created-page-id (format " (ID %s)" created-page-id) ""))
		created-page-id)
	    (when (and xhtml-file (file-exists-p xhtml-file))
	      (delete-file xhtml-file))))))))

(provide 'org-confluence-publish)
;;; org-confluence-publish.el ends here
