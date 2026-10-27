;;; ox-sync-status.el --- Org sync status panel -*- lexical-binding: t; -*-

;;; Commentary:
;; Generic bottom-panel status UI for provider-neutral Org sync state.

;;; Code:

(require 'cl-lib)
(require 'context-panels)
(require 'magit-section)
(require 'org)
(require 'ox-sync-model)
(require 'ox-sync-provider)
(require 'subr-x)
(require 'ox-sync-store)

(defgroup ox-sync nil
  "Provider-neutral Org synchronization status."
  :group 'org)

(defcustom ox-sync-status-buffer-name "*Org Sync*"
  "Buffer name used for Org sync status bottom panels."
  :type 'string
  :group 'ox-sync)

(defcustom ox-sync-status-height 14
  "Window height for Org sync status bottom panels."
  :type 'natnum
  :group 'ox-sync)

(defvar-local ox-sync-current-document nil
  "Detected ox-sync document descriptor for the current source buffer.")

(defvar-local ox-sync-status-source-buffer nil
  "Source buffer associated with the current Org sync status panel.")

(defvar-local ox-sync-source-marker-overlay nil
  "Source overlay showing compact Org sync status.")

(defvar ox-sync-status-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") #'ox-sync-refresh)
    (define-key map (kbd "f") #'ox-sync-fetch)
    (define-key map (kbd "F") #'ox-sync-pull)
    (define-key map (kbd "p") #'ox-sync-push)
    (define-key map (kbd "C") #'ox-sync-pull-content)
    (define-key map (kbd "M") #'ox-sync-pull-comments)
    (define-key map (kbd "c") #'ox-sync-push-content)
    (define-key map (kbd "m") #'ox-sync-push-comments)
    (define-key map (kbd "B") #'ox-sync-baseline)
    (define-key map (kbd "R") #'ox-sync-reset-tracking)
    (define-key map (kbd "q") #'ox-sync-close)
    map)
  "Keymap for `ox-sync-status-mode'.")

(define-derived-mode ox-sync-status-mode magit-section-mode "Org Sync"
  "Major mode for Org sync status bottom panels."
  (setq-local truncate-lines nil)
  (setq-local word-wrap t))

(defun ox-sync--source-buffer ()
  "Return source buffer for the current ox-sync command."
  (cond
   ((and (boundp 'context-panels-source-buffer)
	 (buffer-live-p context-panels-source-buffer))
    context-panels-source-buffer)
   ((derived-mode-p 'org-mode) (current-buffer))
   (t (user-error "No Org sync source buffer"))))

(defun ox-sync--document (source-buffer)
  "Detect SOURCE-BUFFER's current identity without trusting a cached descriptor."
  (with-current-buffer source-buffer
    (setq ox-sync-current-document (ox-sync-detect-document source-buffer))))

(defun ox-sync--provider-identity (document)
  "Return the durable identity fields of DOCUMENT."
  (append (list :kind (plist-get document :kind)
		:remote-id (or (plist-get document :remote-id) (plist-get document :id)))
	  (when-let* ((account (plist-get document :account)))
	    (list :account account))))

(defun ox-sync--tracking (source-buffer)
  "Return tracking data for SOURCE-BUFFER, or an empty tracking plist."
  (or (when-let* ((source-file (buffer-file-name source-buffer)))
	(ox-sync-store-read source-file))
      (ox-sync--identity-tracking source-buffer)))

(defun ox-sync--identity-matches-p (source-buffer tracking)
  "Return non-nil if SOURCE-BUFFER and TRACKING identify the same document."
  (let ((expected (ox-sync--provider-identity (ox-sync--document source-buffer)))
	(actual (plist-get tracking :provider)))
    (cl-every (lambda (key) (equal (plist-get expected key) (plist-get actual key)))
	      '(:kind :remote-id :account))))

(defun ox-sync--assert-tracking-identity (source-buffer tracking)
  "Refuse operations when SOURCE-BUFFER and TRACKING identities disagree."
  (unless (ox-sync--identity-matches-p source-buffer tracking)
    (user-error "Tracking identity differs from source; run ox-sync-reset-tracking")))

(defun ox-sync--domain-statuses (source-buffer)
  "Return current domain statuses for SOURCE-BUFFER."
  (let ((tracking (ox-sync--tracking source-buffer)))
    (if (ox-sync--identity-matches-p source-buffer tracking)
	(ox-sync-statuses tracking source-buffer)
      (mapcar (lambda (domain) (cons domain 'conflicted)) ox-sync-domains))))

(defun ox-sync--document-label (document)
  "Return display label for DOCUMENT."
  (format "%s %s"
	  (plist-get document :kind)
	  (or (plist-get document :remote-id)
	      (plist-get document :id)
	      "<unknown>")))

(defun ox-sync-marker-string (&optional source-buffer)
  "Return compact source marker text for SOURCE-BUFFER."
  (let* ((source (or source-buffer (current-buffer)))
	 (statuses (ox-sync--domain-statuses source))
	 (ahead (cl-count 'ahead statuses :key #'cdr))
	 (behind (cl-count 'behind statuses :key #'cdr))
	 (diverged (cl-count 'diverged statuses :key #'cdr))
	 (problem (cl-count-if (lambda (status)
				 (memq status '(conflicted fetch-error)))
			       statuses :key #'cdr))
	 (unknown (cl-count 'unknown statuses :key #'cdr)))
    (concat
     " ⇅ "
     (cond
      ((> problem 0) "!")
      ((> diverged 0) "↑↓")
      ((or (> ahead 0) (> behind 0))
       (string-join
	(delq nil
	      (list (when (> ahead 0) (format "↑%d" ahead))
		    (when (> behind 0) (format "↓%d" behind))))
	" "))
      ((> unknown 0) "?")
      (t "clean")))))

(defun ox-sync-refresh-source-marker (&optional source-buffer)
  "Refresh compact Org sync source marker for SOURCE-BUFFER."
  (let ((source (or source-buffer (current-buffer))))
    (when (buffer-live-p source)
      (with-current-buffer source
	(when (overlayp ox-sync-source-marker-overlay)
	  (delete-overlay ox-sync-source-marker-overlay))
	(setq ox-sync-source-marker-overlay
	      (make-overlay (point-min) (point-min) nil t nil))
	(overlay-put ox-sync-source-marker-overlay 'after-string
		     (propertize (ox-sync-marker-string source)
				 'help-echo "Open Org sync status"))))))

(defun ox-sync--fetch-tracking (tracking fetch document domains)
  "Fetch DOCUMENT's DOMAINS through FETCH and return updated TRACKING.
Record local completion time only when usable requested-domain refs arrive."
  (message "Fetching Org sync refs...")
  (let* ((before (copy-tree tracking))
	 (result (funcall fetch document domains))
	 (received (cl-remove-if-not
		    (lambda (entry)
		      (and (memq (car entry) domains)
			   (ox-sync--fetch-domain-ref (cdr entry))))
		    (plist-get result :domains)))
	 (received-domains (mapcar #'car received)))
    (unless received
      (user-error "Fetch returned no usable remote references"))
    (let* ((updated (ox-sync-merge-fetch-result tracking (list :domains received)))
	   (changed (cl-some
		     (lambda (domain)
		       (not (equal (plist-get (ox-sync-domain-entry before domain) :fetched-remote-ref)
				   (plist-get (ox-sync-domain-entry updated domain) :fetched-remote-ref))))
		     received-domains)))
      (setq updated (plist-put updated :fetched-at
			       (format-time-string "%FT%T%z" (current-time))))
      (setq updated (plist-put updated :fetched-domains received-domains))
      (plist-put updated :fetch-outcome (if changed 'updated 'unchanged)))))

(defun ox-sync--fetch-result-label (tracking)
  "Return a readable last-fetch outcome and domain scope for TRACKING."
  (concat (pcase (plist-get tracking :fetch-outcome)
	    ('updated "remote refs updated")
	    ('unchanged "no remote changes")
	    (_ "outcome unavailable"))
	  (when-let* ((domains (plist-get tracking :fetched-domains)))
	    (format " (%s)" (mapconcat #'symbol-name domains ", ")))))

(defun ox-sync--report-fetch-completion (tracking &optional prefix)
  "Report successful TRACKING receipt, with optional message PREFIX."
  (message "%s: %s" (or prefix "Fetch complete") (ox-sync--fetch-result-label tracking)))

(defun ox-sync--reference-label (reference)
  "Return a concise version, opaque ID, or fingerprint for REFERENCE."
  (cond
   ((null reference) "not fetched")
   ((stringp reference) reference)
   ((numberp reference) (number-to-string reference))
   ((not (listp reference))
    (concat "ref " (substring (ox-sync--hash-object reference) 0 8)))
   ((plist-get reference :version) (format "v%s" (plist-get reference :version)))
   ((or (plist-get reference :revision-id) (plist-get reference :revision))
    (format "%s" (or (plist-get reference :revision-id) (plist-get reference :revision))))
   ((plist-get reference :hash)
    (let ((hash (format "%s" (plist-get reference :hash))))
      (substring hash 0 (min 8 (length hash)))))
   (t (concat "ref " (substring (ox-sync--hash-object reference) 0 8)))))

(defun ox-sync--insert-status-section (title statuses predicate source-buffer tracking)
  "Insert TITLE for STATUSES matching PREDICATE.
Show SOURCE-BUFFER's local refs and TRACKING's fetched and baseline refs."
  (when-let* ((matches (cl-remove-if-not
			(lambda (entry) (funcall predicate (cdr entry))) statuses)))
    (magit-insert-section (ox-sync-status title)
			  (insert title "\n")
			  (magit-insert-section-body
			   (dolist (entry matches)
			     (let* ((domain (car entry))
				    (tracked (ox-sync-domain-entry tracking domain))
				    (base-local (plist-get tracked :base-local-ref))
				    (base-remote (plist-get tracked :base-remote-ref)))
			       (insert (format "  %-8s %s | local %s | fetched %s\n"
					       (capitalize (symbol-name domain)) (cdr entry)
					       (ox-sync--reference-label (ox-sync-local-ref domain source-buffer))
					       (ox-sync--reference-label (plist-get tracked :fetched-remote-ref))))
			       (insert (if (and base-local base-remote)
					   (format "    Baseline: local %s ↔ remote %s\n"
						   (ox-sync--reference-label base-local)
						   (ox-sync--reference-label base-remote))
					 "    Baseline: not set (B accepts local and fetched refs)\n"))))))))

(defun ox-sync-status-render (source-buffer _view)
  "Render Org sync status for SOURCE-BUFFER."
  (setq ox-sync-status-source-buffer source-buffer)
  (let* ((document (ox-sync--document source-buffer))
	 (tracking (ox-sync--tracking source-buffer))
	 (fetched-at (plist-get tracking :fetched-at))
	 (statuses (ox-sync--domain-statuses source-buffer)))
    (insert (format "Org Sync: %s\n" (ox-sync--document-label document)))
    (insert "Head: " (string-trim (ox-sync-marker-string source-buffer)) "\n")
    (insert (if fetched-at
		(format "Last successful fetch: %s — %s\n\n"
			fetched-at (ox-sync--fetch-result-label tracking))
	      "Last successful fetch: not recorded\n\n"))
    (unless (ox-sync--identity-matches-p source-buffer tracking)
      (insert "Identity mismatch: reset tracking (R) before fetch, baseline, pull, or push.\n\n"))
    (dolist (section '(("Unpushed changes" ahead)
		       ("Unpulled changes" behind)
		       ("Conflicts" diverged conflicted)
		       ("Unknown" unknown fetch-error)
		       ("Clean" clean)))
      (ox-sync--insert-status-section
       (car section) statuses (lambda (status) (memq status (cdr section)))
       source-buffer tracking))
    (insert "\ng refresh | f fetch | B baseline | c/C push/pull content | m/M comments | p/F all | R reset | q close\n")))

(defun ox-sync--bottom-views (_source-buffer)
  "Return Org sync bottom view descriptors."
  (list (list :id 'ox-sync-status
	      :label "Org Sync"
	      :buffer-name ox-sync-status-buffer-name
	      :height ox-sync-status-height
	      :mode #'ox-sync-status-mode
	      :render #'ox-sync-status-render)))

(defun ox-sync-context-panel-provider ()
  "Return context-panels provider descriptor for Org sync."
  (list :name 'org-sync
	:icon "⇅"
	:collect-bottom-views #'ox-sync--bottom-views
	:priority 20))

;;;###autoload
(define-minor-mode ox-sync-mode
  "Enable provider-neutral Org sync status integration."
  :lighter " Sync"
  (if ox-sync-mode
      (progn
	(setq ox-sync-current-document (ox-sync-detect-document (current-buffer)))
	(context-panels-mode 1)
	(context-panels-register-provider (ox-sync-context-panel-provider))
	(ox-sync-refresh-source-marker (current-buffer)))
    (context-panels-unregister-provider 'org-sync)
    (when (overlayp ox-sync-source-marker-overlay)
      (delete-overlay ox-sync-source-marker-overlay))
    (setq ox-sync-source-marker-overlay nil)
    (setq ox-sync-current-document nil)))

;;;###autoload
(defun ox-sync-refresh ()
  "Refresh the current Org sync status panel without network I/O."
  (interactive)
  (let ((source-buffer (ox-sync--source-buffer)))
    (ox-sync-refresh-source-marker source-buffer)
    (context-panels-open-bottom-view 'ox-sync-status source-buffer)))

;;;###autoload
(defun ox-sync-fetch ()
  "Fetch remote refs into tracking state without mutating local Org files."
  (interactive)
  (let* ((source-buffer (ox-sync--source-buffer))
	 (source-file (or (buffer-file-name source-buffer)
			  (user-error "Source buffer is not visiting a file")))
	 (document (ox-sync--document source-buffer))
	 (provider (or (plist-get document :provider)
		       (ox-sync-provider (plist-get document :kind))))
	 (fetch (or (plist-get provider :fetch)
		    (user-error "Org sync provider %s does not support fetch"
				(plist-get document :kind))))
	 (tracking (ox-sync--tracking source-buffer))
	 (_identity (ox-sync--assert-tracking-identity source-buffer tracking))
	 (updated (ox-sync--fetch-tracking tracking fetch document ox-sync-domains)))
    (ox-sync-store-write source-file updated)
    (ox-sync-refresh-source-marker source-buffer)
    (context-panels-open-bottom-view 'ox-sync-status source-buffer)
    (ox-sync--report-fetch-completion updated)
    updated))

(defun ox-sync--action-callback (provider action domain)
  "Return PROVIDER callback for ACTION and DOMAIN."
  (plist-get provider (intern (format ":%s-%s" action domain))))

(defun ox-sync--eligible-domains (source-buffer wanted-status &optional domain)
  "Return domains in SOURCE-BUFFER with WANTED-STATUS.
When DOMAIN is non-nil, consider only that domain.  Signal if a considered
domain is diverged, conflicted, or unknown."
  (let* ((statuses (ox-sync--domain-statuses source-buffer))
	 (domains (if domain (list domain) ox-sync-domains))
	 eligible)
    (dolist (current domains)
      (let ((status (alist-get current statuses)))
	(when (memq status '(diverged conflicted unknown fetch-error))
	  (user-error "Refusing to mutate %s while status is %s" current status))
	(when (eq status wanted-status)
	  (push current eligible))))
    (nreverse eligible)))

(defun ox-sync--run-domain-actions (action wanted-status &optional domain)
  "Run ACTION for domains matching WANTED-STATUS, optionally limited to DOMAIN."
  (let* ((source-buffer (ox-sync--source-buffer))
	 (source-file (or (buffer-file-name source-buffer)
			  (user-error "Source buffer is not visiting a file")))
	 (document (ox-sync--document source-buffer))
	 (provider (or (plist-get document :provider)
		       (ox-sync-provider (plist-get document :kind))))
	 (domains (ox-sync--eligible-domains source-buffer wanted-status domain))
	 (tracking (ox-sync--tracking source-buffer))
	 (_identity (ox-sync--assert-tracking-identity source-buffer tracking))
	 (fetch (when (eq action 'push)
		  (or (plist-get provider :fetch)
		      (user-error "Provider needs fetch to confirm published refs before push")))))
    (unless domains
      (user-error "No %s domains to %s" wanted-status action))
    (dolist (current domains)
      (let ((callback (or (ox-sync--action-callback provider action current)
			  (user-error "Org sync provider %s has no %s callback for %s"
				      (plist-get document :kind) action current))))
	(funcall callback document current tracking)))
    (let* ((tracking (if fetch
			 (ox-sync--fetch-tracking tracking fetch document domains)
		       tracking))
	   (updated (ox-sync-advance-base-for-domains tracking source-buffer domains)))
      (ox-sync-store-write source-file updated)
      (ox-sync-refresh-source-marker source-buffer)
      (context-panels-open-bottom-view 'ox-sync-status source-buffer)
      (when fetch
	(ox-sync--report-fetch-completion updated "Push complete; fetch confirmed"))
      updated)))

;;;###autoload
(defun ox-sync-pull ()
  "Pull all eligible behind domains through the active provider."
  (interactive)
  (ox-sync--run-domain-actions 'pull 'behind))

;;;###autoload
(defun ox-sync-push ()
  "Push all eligible ahead domains through the active provider."
  (interactive)
  (ox-sync--run-domain-actions 'push 'ahead))

;;;###autoload
(defun ox-sync-pull-content ()
  "Pull remote content through the active provider."
  (interactive)
  (ox-sync--run-domain-actions 'pull 'behind 'content))

;;;###autoload
(defun ox-sync-pull-comments ()
  "Pull remote comments through the active provider."
  (interactive)
  (ox-sync--run-domain-actions 'pull 'behind 'comments))

;;;###autoload
(defun ox-sync-push-content ()
  "Push local content through the active provider."
  (interactive)
  (ox-sync--run-domain-actions 'push 'ahead 'content))

;;;###autoload
(defun ox-sync-push-comments ()
  "Push local comments through the active provider."
  (interactive)
  (ox-sync--run-domain-actions 'push 'ahead 'comments))

(defun ox-sync--identity-tracking (source-buffer)
  "Return empty tracking data for SOURCE-BUFFER's current provider identity."
  (list :provider (ox-sync--provider-identity (ox-sync--document source-buffer))
	:domains nil))

;;;###autoload
(defun ox-sync-reset-tracking (&optional confirm)
  "Reset sync tracking for the current source identity.
When CONFIRM is non-nil, ask before replacing the sidecar."
  (interactive (list t))
  (let* ((source-buffer (ox-sync--source-buffer))
	 (source-file (or (buffer-file-name source-buffer)
			  (user-error "Source buffer is not visiting a file"))))
    (when (and confirm
	       (not (yes-or-no-p "Reset Org sync tracking for this source? ")))
      (user-error "Reset cancelled"))
    (let ((backup (ox-sync-store-archive source-file))
	  (tracking (ox-sync--identity-tracking source-buffer)))
      (ox-sync-store-write source-file tracking)
      (ox-sync-refresh-source-marker source-buffer)
      (context-panels-open-bottom-view 'ox-sync-status source-buffer)
      (when backup
	(message "Archived previous Org sync tracking to %s" backup))
      tracking)))

;;;###autoload
(defun ox-sync-baseline ()
  "Accept current local refs and fetched remote refs as the sync baseline."
  (interactive)
  (let* ((source-buffer (ox-sync--source-buffer))
	 (source-file (or (buffer-file-name source-buffer)
			  (user-error "Source buffer is not visiting a file")))
	 (tracking (ox-sync--tracking source-buffer))
	 (updated (progn
		    (ox-sync--assert-tracking-identity source-buffer tracking)
		    (ox-sync-baseline-tracking tracking source-buffer))))
    (ox-sync-store-write source-file updated)
    (with-current-buffer source-buffer
      (setq ox-sync-current-document
	    (or ox-sync-current-document
		(ox-sync-detect-document source-buffer))))
    (ox-sync-refresh-source-marker source-buffer)
    (context-panels-open-bottom-view 'ox-sync-status source-buffer)
    updated))

;;;###autoload
(defun ox-sync-status ()
  "Open the Org sync status bottom panel for the current source buffer."
  (interactive)
  (unless ox-sync-mode
    (ox-sync-mode 1))
  (ox-sync-refresh-source-marker (current-buffer))
  (context-panels-open-bottom-view 'ox-sync-status (current-buffer) t))

;;;###autoload
(defun ox-sync-close ()
  "Close the current Org sync status bottom panel."
  (interactive)
  (context-panels-close-bottom-view))

(provide 'ox-sync-status)
;;; ox-sync-status.el ends here
