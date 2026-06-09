;;; dashboard.el --- UI dashboard activation -*- lexical-binding: t; -*-

;;; Commentary:
;; Dashboard startup configuration and deferred expensive section rendering.

;;; Code:

(require 'ui/performance)

(defgroup hub/dashboard nil
  "Customizations for dashboard startup data."
  :group 'convenience)

(defcustom hub/dashboard-workspace-roots '("~/ws/")
  "Directories containing project folders to seed `project.el'."
  :type '(repeat directory)
  :group 'hub/dashboard)

(defcustom hub/dashboard-deferred-items '(denote agenda)
  "Dashboard item identifiers to render after the first dashboard paint.
These sections may load heavier dependencies such as Org agenda support.  Keep
those dependencies inside their item generators so disabling dashboard, or
removing the item from `dashboard-items', also avoids loading them."
  :type '(repeat symbol)
  :group 'hub/dashboard)

(defcustom hub/dashboard-deferred-item-delays '((denote . 1.0) (agenda . 5.0))
  "Idle delay per deferred dashboard item before refreshing it."
  :type '(alist :key-type symbol :value-type number)
  :group 'hub/dashboard)

(defvar hub/dashboard--full-item-generators nil
  "Dashboard item generators restored after first dashboard paint.")

(defvar hub/dashboard--deferred-refresh-timers nil
  "Idle timers used to refresh dashboard deferred sections.")

(defvar hub/dashboard--resize-refresh-enabled nil
  "Non-nil once dashboard resize refresh has been enabled after startup.")

(defun hub/dashboard-seed-project-list ()
  "Ensure `project.el' knows workspace projects before dashboard renders."
  (let ((start-time (current-time))
	(project-count 0))
    (require 'project)
    (condition-case err
	(dolist (root hub/dashboard-workspace-roots)
	  (let ((expanded-root (expand-file-name root)))
	    (when (file-directory-p expanded-root)
	      (setq project-count
		    (+ project-count
		       (let ((inhibit-message t)
			     (message-log-max nil))
			 (project-remember-projects-under expanded-root nil)))))))
      (error
       (message "[dashboard] failed to seed projects: %s"
		(error-message-string err))))
    (hub/performance-log-startup-event
     (format "dashboard seed projects (%d)" project-count)
     start-time)))

(defun hub/dashboard--item-name (item)
  "Return dashboard item identifier from ITEM."
  (if (consp item) (car item) item))

(defun hub/dashboard-insert-deferred-placeholder (name list-size)
  "Insert a placeholder for deferred dashboard section NAME of LIST-SIZE."
  (ignore list-size)
  (dashboard-insert-heading (format "Loading %s..." (capitalize (symbol-name name))) nil nil)
  (insert (propertize "\n    Preparing data…" 'face 'dashboard-no-items-face)))

(defun hub/dashboard-install-deferred-placeholders ()
  "Replace deferred dashboard item generators with cheap placeholders."
  (setq hub/dashboard--full-item-generators (copy-tree dashboard-item-generators))
  (dolist (item dashboard-items)
    (let ((name (hub/dashboard--item-name item)))
      (when (memq name hub/dashboard-deferred-items)
	(setf (alist-get name dashboard-item-generators)
	      (lambda (list-size)
		(hub/dashboard-insert-deferred-placeholder name list-size)))))))

(defun hub/dashboard--restore-deferred-items (items)
  "Restore real dashboard generators for deferred ITEMS."
  (dolist (item items)
    (when-let ((generator (alist-get item hub/dashboard--full-item-generators)))
      (setf (alist-get item dashboard-item-generators) generator))))

(defun hub/dashboard-refresh-deferred-items (items)
  "Refresh dashboard after restoring deferred section ITEMS."
  (when (and hub/dashboard--full-item-generators (get-buffer dashboard-buffer-name))
    (let ((start-time (current-time))
	  (window (get-buffer-window dashboard-buffer-name))
	  (label (format "dashboard deferred refresh (%s)"
			 (mapconcat #'symbol-name items ", "))))
      (hub/dashboard--restore-deferred-items items)
      (condition-case err
	  (progn
	    (when window
	      (with-selected-window window
		(dashboard-insert-startupify-lists t)
		(goto-char (point-min))))
	    (unless window
	      (with-current-buffer dashboard-buffer-name
		(dashboard-insert-startupify-lists t)))
	    (hub/performance-log-startup-event label start-time))
	(error
	 (message "[dashboard] deferred refresh failed: %s" (error-message-string err)))))))

(defun hub/dashboard-schedule-deferred-refresh ()
  "Schedule dashboard refreshes for deferred sections."
  (when hub/dashboard--full-item-generators
    (mapc #'cancel-timer hub/dashboard--deferred-refresh-timers)
    (setq hub/dashboard--deferred-refresh-timers nil)
    (dolist (entry hub/dashboard-deferred-item-delays)
      (let ((item (car entry))
	    (delay (cdr entry)))
	(when (memq item hub/dashboard-deferred-items)
	  (push (run-with-idle-timer delay nil
				     #'hub/dashboard-refresh-deferred-items
				     (list item))
		hub/dashboard--deferred-refresh-timers))))))

(defun hub/dashboard-enable-resize-refresh ()
  "Enable dashboard resize refresh after startup has settled."
  (unless hub/dashboard--resize-refresh-enabled
    (setq hub/dashboard--resize-refresh-enabled t)
    (add-hook 'window-size-change-functions #'dashboard-resize-on-hook 100)))

(use-package dashboard
  :config
  (let ((start-time (current-time)))
    (hub/performance-log-startup-event "dashboard config start")
    ;; Ensure recentf is enabled so Recents has data.
    (require 'bookmark)
    (require 'recentf)
    (require 'seq)
    (require 'project)
    (recentf-mode 1)
    (ignore-errors (recentf-load-list))
    (hub/performance-log-startup-event "dashboard recentf ready" start-time))
  (hub/dashboard-seed-project-list)
  (let ((start-time (current-time)))
    (ignore-errors (project-known-project-roots))
    (hub/performance-log-startup-event "dashboard known projects ready" start-time))
  (setq dashboard-set-heading-icons t
	dashboard-set-file-icons t
	dashboard-set-navigator t
	dashboard-projects-backend 'project-el
	dashboard-projects-switch-function 'project-switch-project
	dashboard-filter-agenda-entry 'dashboard-filter-agenda-by-todo
	dashboard-items '((recents . 8) (projects . 8) (bookmarks . 5) (denote . 5) (agenda . 5)))
  (let ((start-time (current-time)))
    (dashboard-setup-startup-hook)
    ;; We render the dashboard explicitly from `hub/dashboard-first-paint' before
    ;; loading lower-priority interactive modules.  Keep dashboard's resize hooks,
    ;; but remove its default post-init render hooks so first paint is not delayed
    ;; until the end of init.el.
    (remove-hook 'after-init-hook #'dashboard-insert-startupify-lists)
    (remove-hook 'emacs-startup-hook #'dashboard-initialize)
    ;; Dashboard's resize hooks can re-render during late startup.  With explicit
    ;; first paint this is redundant, noisy, and can race package/custom setup.
    ;; Re-enable real resize refresh only after startup has settled.
    (remove-hook 'window-setup-hook #'dashboard-resize-on-hook)
    (remove-hook 'window-size-change-functions #'dashboard-resize-on-hook)
    (add-hook 'emacs-startup-hook #'hub/dashboard-schedule-deferred-refresh 90)
    (add-hook 'emacs-startup-hook
	      (lambda ()
		(run-with-idle-timer 2.0 nil #'hub/dashboard-enable-resize-refresh))
	      95)
    (hub/performance-log-startup-event "dashboard startup hook installed" start-time))

  (defun hub/dashboard-insert-agenda-safe (list-size)
    "Insert Agenda section without breaking dashboard startup."
    (let ((start-time (current-time)))
      (unwind-protect
	  (condition-case err
	      (progn
		(when (fboundp 'hub/org-prune-missing-agenda-files)
		  (hub/org-prune-missing-agenda-files t))
		(with-timeout (2.0 (error "Agenda generation timed out"))
		  (dashboard-insert-agenda list-size)))
	    (error (message "[dashboard] skipping agenda section: %s" (error-message-string err))))
	(hub/performance-log-startup-event "dashboard agenda section" start-time))))

  (setf (alist-get 'agenda dashboard-item-generators) #'hub/dashboard-insert-agenda-safe)

  (defun hub/dashboard--denote-dirs ()
    "Return existing denote directories, or nil when unavailable."
    (when (boundp 'denote-directory)
      (let* ((dirs (if (listp denote-directory) denote-directory (list denote-directory))))
	(seq-filter #'file-directory-p dirs))))

  (defun dashboard-insert-denote (list-size)
    "Insert Denote dashboard section for LIST-SIZE items."
    (let ((start-time (current-time)))
      (unwind-protect
	  (condition-case err
	      (with-timeout (1.5 (error "Denote scan timed out"))
		(let* ((dirs (hub/dashboard--denote-dirs))
		       (denote-ok (and (require 'denote nil 'noerror) dirs)))
		  (when denote-ok
		    (let* ((denote-directory dirs)
			   (recent-notes (seq-sort-by #'file-name-nondirectory
						      (lambda (x y) (string-lessp y x))
						      (denote-directory-files))))
		      (when recent-notes
			(insert (all-the-icons-octicon "repo" :height 1.2 :v-adjust 0.0 :face 'dashboard-heading))
			(dashboard-insert-section "Recent Notes:" recent-notes list-size 'notes "n"
						  `(lambda (&rest ignore) (find-file-existing ,el))
						  (or (denote-retrieve-title-value el (denote-filetype-heuristics el))
						      (file-name-base el))))))))
	    (error (message "[dashboard] skipping denote section: %s" (error-message-string err))))
	(hub/performance-log-startup-event "dashboard denote section" start-time))))
  (add-to-list 'dashboard-item-generators '(denote . dashboard-insert-denote))
  (hub/dashboard-install-deferred-placeholders)

  ;; Hide Maildir entries from the Recent Files section while keeping them in recentf.
  (defun hub/dashboard--mail-path-p (path)
    (when (stringp path)
      (let ((mail-root (file-name-as-directory (expand-file-name "Mail" (or (getenv "HOME") "~")))))
	(string-prefix-p mail-root (expand-file-name path)))))

  (defun hub/dashboard--filter-recents (paths)
    (seq-remove #'hub/dashboard--mail-path-p paths))

  (defun hub/dashboard-filter-mail-recents (orig-fn &rest args)
    "Call ORIG-FN with ARGS after filtering Maildir recents."
    (let* ((recentf-list (if (and (boundp 'recentf-list) (recentf-enabled-p))
			     (hub/dashboard--filter-recents recentf-list)
			   recentf-list)))
      (apply orig-fn args)))

  (defun hub/dashboard-log-startupify-lists (orig-fn &rest args)
    "Call ORIG-FN with ARGS and log dashboard rendering duration."
    (let ((start-time (current-time)))
      (unwind-protect
	  (apply orig-fn args)
	(hub/performance-log-startup-event "dashboard startupify lists" start-time))))

  (advice-add 'dashboard-insert-startupify-lists :around #'hub/dashboard-filter-mail-recents)
  (advice-add 'dashboard-insert-startupify-lists :around #'hub/dashboard-log-startupify-lists)
  (hub/performance-log-startup-event "dashboard config complete"))

(defun hub/dashboard-first-paint ()
  "Render and display dashboard before lower-priority startup modules load."
  (when (and (featurep 'dashboard)
	     (< (length command-line-args) 2)
	     (display-graphic-p))
    (let ((start-time (current-time)))
      (condition-case err
	  (progn
	    (dashboard-insert-startupify-lists t)
	    (dashboard-initialize)
	    (hub/performance-log-startup-event "dashboard first paint" start-time))
	(error
	 (message "[dashboard] first paint failed: %s" (error-message-string err)))))))

(provide 'ui/dashboard)
;;; dashboard.el ends here
