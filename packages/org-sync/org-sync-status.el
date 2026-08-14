;;; org-sync-status.el --- Org sync status panel -*- lexical-binding: t; -*-

;;; Commentary:
;; Generic bottom-panel status UI for provider-neutral Org sync state.

;;; Code:

(require 'cl-lib)
(require 'context-panels)
(require 'magit-section)
(require 'org)
(require 'org-sync-model)
(require 'org-sync-provider)
(require 'subr-x)
(require 'org-sync-store)

(defgroup org-sync nil
  "Provider-neutral Org synchronization status."
  :group 'org)

(defcustom org-sync-status-buffer-name "*Org Sync*"
  "Buffer name used for Org sync status bottom panels."
  :type 'string
  :group 'org-sync)

(defcustom org-sync-status-height 14
  "Window height for Org sync status bottom panels."
  :type 'natnum
  :group 'org-sync)

(defvar-local org-sync-current-document nil
  "Detected org-sync document descriptor for the current source buffer.")

(defvar-local org-sync-status-source-buffer nil
  "Source buffer associated with the current Org sync status panel.")

(defvar-local org-sync-source-marker-overlay nil
  "Source overlay showing compact Org sync status.")

(defvar org-sync-status-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") #'org-sync-refresh)
    (define-key map (kbd "f") #'org-sync-fetch)
    (define-key map (kbd "F") #'org-sync-pull)
    (define-key map (kbd "p") #'org-sync-push)
    (define-key map (kbd "C") #'org-sync-pull-content)
    (define-key map (kbd "M") #'org-sync-pull-comments)
    (define-key map (kbd "c") #'org-sync-push-content)
    (define-key map (kbd "m") #'org-sync-push-comments)
    (define-key map (kbd "B") #'org-sync-baseline)
    (define-key map (kbd "q") #'org-sync-close)
    map)
  "Keymap for `org-sync-status-mode'.")

(define-derived-mode org-sync-status-mode magit-section-mode "Org Sync"
  "Major mode for Org sync status bottom panels."
  (setq-local truncate-lines nil)
  (setq-local word-wrap t))

(defun org-sync--source-buffer ()
  "Return source buffer for the current org-sync command."
  (cond
   ((and (boundp 'context-panels-source-buffer)
	 (buffer-live-p context-panels-source-buffer))
    context-panels-source-buffer)
   ((derived-mode-p 'org-mode) (current-buffer))
   (t (user-error "No Org sync source buffer"))))

(defun org-sync--tracking (source-buffer)
  "Return tracking data for SOURCE-BUFFER, or an empty tracking plist."
  (or (when-let* ((source-file (buffer-file-name source-buffer)))
	(org-sync-store-read source-file))
      (list :provider (with-current-buffer source-buffer
			(let ((document (or org-sync-current-document
					    (org-sync-detect-document source-buffer))))
			  (list :kind (plist-get document :kind)
				:remote-id (or (plist-get document :remote-id)
					       (plist-get document :id)))))
	    :domains nil)))

(defun org-sync--domain-statuses (source-buffer)
  "Return current domain statuses for SOURCE-BUFFER."
  (org-sync-statuses (org-sync--tracking source-buffer) source-buffer))

(defun org-sync--document-label (document)
  "Return display label for DOCUMENT."
  (format "%s %s"
	  (plist-get document :kind)
	  (or (plist-get document :remote-id)
	      (plist-get document :id)
	      "<unknown>")))

(defun org-sync-marker-string (&optional source-buffer)
  "Return compact source marker text for SOURCE-BUFFER."
  (let* ((source (or source-buffer (current-buffer)))
	 (statuses (org-sync--domain-statuses source))
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

(defun org-sync-refresh-source-marker (&optional source-buffer)
  "Refresh compact Org sync source marker for SOURCE-BUFFER."
  (let ((source (or source-buffer (current-buffer))))
    (when (buffer-live-p source)
      (with-current-buffer source
	(when (overlayp org-sync-source-marker-overlay)
	  (delete-overlay org-sync-source-marker-overlay))
	(setq org-sync-source-marker-overlay
	      (make-overlay (point-min) (point-min) nil t nil))
	(overlay-put org-sync-source-marker-overlay 'after-string
		     (propertize (org-sync-marker-string source)
				 'help-echo "Open Org sync status"))))))

(defun org-sync--insert-status-section (title statuses predicate)
  "Insert status section TITLE for STATUSES matching PREDICATE."
  (let ((matches (cl-remove-if-not (lambda (entry) (funcall predicate (cdr entry)))
				   statuses)))
    (magit-insert-section (org-sync-status title)
			  (insert title "\n")
			  (magit-insert-section-body
			   (if matches
			       (dolist (entry matches)
				 (insert (format "  %-8s %s\n"
						 (capitalize (symbol-name (car entry)))
						 (cdr entry))))
			     (insert "  none\n"))))))

(defun org-sync-status-render (source-buffer _view)
  "Render Org sync status for SOURCE-BUFFER."
  (setq org-sync-status-source-buffer source-buffer)
  (let ((document (with-current-buffer source-buffer
		    (or org-sync-current-document
			(setq org-sync-current-document
			      (org-sync-detect-document source-buffer)))))
	(statuses (org-sync--domain-statuses source-buffer)))
    (insert (format "Org Sync: %s\n" (org-sync--document-label document)))
    (insert "Head: " (string-trim (org-sync-marker-string source-buffer)) "\n\n")
    (org-sync--insert-status-section "Unpushed changes" statuses
				     (lambda (status) (eq status 'ahead)))
    (org-sync--insert-status-section "Unpulled changes" statuses
				     (lambda (status) (eq status 'behind)))
    (org-sync--insert-status-section "Conflicts" statuses
				     (lambda (status) (memq status '(diverged conflicted))))
    (org-sync--insert-status-section "Unknown" statuses
				     (lambda (status) (memq status '(unknown fetch-error))))
    (insert "\nActions: g refresh, f fetch, p push, F pull, c/m push domain, C/M pull domain, B baseline, R reset, ? help, q close\n")))

(defun org-sync--bottom-views (_source-buffer)
  "Return Org sync bottom view descriptors."
  (list (list :id 'org-sync-status
	      :label "Org Sync"
	      :buffer-name org-sync-status-buffer-name
	      :height org-sync-status-height
	      :mode #'org-sync-status-mode
	      :render #'org-sync-status-render)))

(defun org-sync-context-panel-provider ()
  "Return context-panels provider descriptor for Org sync."
  (list :name 'org-sync
	:icon "⇅"
	:collect-bottom-views #'org-sync--bottom-views
	:priority 20))

;;;###autoload
(define-minor-mode org-sync-mode
  "Enable provider-neutral Org sync status integration."
  :lighter " Sync"
  (if org-sync-mode
      (progn
	(setq org-sync-current-document (org-sync-detect-document (current-buffer)))
	(context-panels-mode 1)
	(context-panels-register-provider (org-sync-context-panel-provider))
	(org-sync-refresh-source-marker (current-buffer)))
    (context-panels-unregister-provider 'org-sync)
    (when (overlayp org-sync-source-marker-overlay)
      (delete-overlay org-sync-source-marker-overlay))
    (setq org-sync-source-marker-overlay nil)
    (setq org-sync-current-document nil)))

;;;###autoload
(defun org-sync-refresh ()
  "Refresh the current Org sync status panel without network I/O."
  (interactive)
  (let ((source-buffer (org-sync--source-buffer)))
    (org-sync-refresh-source-marker source-buffer)
    (context-panels-open-bottom-view 'org-sync-status source-buffer)))

;;;###autoload
(defun org-sync-fetch ()
  "Fetch remote refs into tracking state without mutating local Org files."
  (interactive)
  (let* ((source-buffer (org-sync--source-buffer))
	 (source-file (or (buffer-file-name source-buffer)
			  (user-error "Source buffer is not visiting a file")))
	 (document (with-current-buffer source-buffer
		     (or org-sync-current-document
			 (setq org-sync-current-document
			       (org-sync-detect-document source-buffer)))))
	 (provider (or (plist-get document :provider)
		       (org-sync-provider (plist-get document :kind))))
	 (fetch (or (plist-get provider :fetch)
		    (user-error "Org sync provider %s does not support fetch"
				(plist-get document :kind))))
	 (tracking (org-sync--tracking source-buffer))
	 (fetch-result (funcall fetch document org-sync-domains))
	 (updated (org-sync-merge-fetch-result tracking fetch-result)))
    (org-sync-store-write source-file updated)
    (org-sync-refresh-source-marker source-buffer)
    (context-panels-open-bottom-view 'org-sync-status source-buffer)
    updated))

(defun org-sync--action-callback (provider action domain)
  "Return PROVIDER callback for ACTION and DOMAIN."
  (plist-get provider (intern (format ":%s-%s" action domain))))

(defun org-sync--eligible-domains (source-buffer wanted-status &optional domain)
  "Return domains in SOURCE-BUFFER with WANTED-STATUS.
When DOMAIN is non-nil, consider only that domain.  Signal if a considered
domain is diverged, conflicted, or unknown."
  (let* ((statuses (org-sync--domain-statuses source-buffer))
	 (domains (if domain (list domain) org-sync-domains))
	 eligible)
    (dolist (current domains)
      (let ((status (alist-get current statuses)))
	(when (memq status '(diverged conflicted unknown fetch-error))
	  (user-error "Refusing to mutate %s while status is %s" current status))
	(when (eq status wanted-status)
	  (push current eligible))))
    (nreverse eligible)))

(defun org-sync--run-domain-actions (action wanted-status &optional domain)
  "Run ACTION for domains matching WANTED-STATUS, optionally limited to DOMAIN."
  (let* ((source-buffer (org-sync--source-buffer))
	 (source-file (or (buffer-file-name source-buffer)
			  (user-error "Source buffer is not visiting a file")))
	 (document (with-current-buffer source-buffer
		     (or org-sync-current-document
			 (setq org-sync-current-document
			       (org-sync-detect-document source-buffer)))))
	 (provider (or (plist-get document :provider)
		       (org-sync-provider (plist-get document :kind))))
	 (domains (org-sync--eligible-domains source-buffer wanted-status domain))
	 (tracking (org-sync--tracking source-buffer)))
    (unless domains
      (user-error "No %s domains to %s" wanted-status action))
    (dolist (current domains)
      (let ((callback (or (org-sync--action-callback provider action current)
			  (user-error "Org sync provider %s has no %s callback for %s"
				      (plist-get document :kind) action current))))
	(funcall callback document current tracking)))
    (let ((updated (org-sync-advance-base-for-domains tracking source-buffer domains)))
      (org-sync-store-write source-file updated)
      (org-sync-refresh-source-marker source-buffer)
      (context-panels-open-bottom-view 'org-sync-status source-buffer)
      updated)))

;;;###autoload
(defun org-sync-pull ()
  "Pull all eligible behind domains through the active provider."
  (interactive)
  (org-sync--run-domain-actions 'pull 'behind))

;;;###autoload
(defun org-sync-push ()
  "Push all eligible ahead domains through the active provider."
  (interactive)
  (org-sync--run-domain-actions 'push 'ahead))

;;;###autoload
(defun org-sync-pull-content ()
  "Pull remote content through the active provider."
  (interactive)
  (org-sync--run-domain-actions 'pull 'behind 'content))

;;;###autoload
(defun org-sync-pull-comments ()
  "Pull remote comments through the active provider."
  (interactive)
  (org-sync--run-domain-actions 'pull 'behind 'comments))

;;;###autoload
(defun org-sync-push-content ()
  "Push local content through the active provider."
  (interactive)
  (org-sync--run-domain-actions 'push 'ahead 'content))

;;;###autoload
(defun org-sync-push-comments ()
  "Push local comments through the active provider."
  (interactive)
  (org-sync--run-domain-actions 'push 'ahead 'comments))

;;;###autoload
(defun org-sync-baseline ()
  "Accept current local refs and fetched remote refs as the sync baseline."
  (interactive)
  (let* ((source-buffer (org-sync--source-buffer))
	 (source-file (or (buffer-file-name source-buffer)
			  (user-error "Source buffer is not visiting a file")))
	 (tracking (org-sync--tracking source-buffer))
	 (updated (org-sync-baseline-tracking tracking source-buffer)))
    (org-sync-store-write source-file updated)
    (with-current-buffer source-buffer
      (setq org-sync-current-document
	    (or org-sync-current-document
		(org-sync-detect-document source-buffer))))
    (org-sync-refresh-source-marker source-buffer)
    (context-panels-open-bottom-view 'org-sync-status source-buffer)
    updated))

;;;###autoload
(defun org-sync-status ()
  "Open the Org sync status bottom panel for the current source buffer."
  (interactive)
  (unless org-sync-mode
    (org-sync-mode 1))
  (org-sync-refresh-source-marker (current-buffer))
  (context-panels-open-bottom-view 'org-sync-status (current-buffer) t))

;;;###autoload
(defun org-sync-close ()
  "Close the current Org sync status bottom panel."
  (interactive)
  (context-panels-close-bottom-view))

(provide 'org-sync-status)
;;; org-sync-status.el ends here
