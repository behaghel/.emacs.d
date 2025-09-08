;;; perspective-auto.el --- Navigation: auto project perspectives -*- lexical-binding: t; -*-

;;; Commentary:
;; Automatically create/switch to a perspective per project when visiting files.
;; Uses project.el for detection and perspective.el for isolation. Optionally
;; opens Treemacs when creating a new project perspective.

;;; Code:

;; Compatibility: Emacs versions lacking `define-completion-category'
;; (older than 29/30) may encounter configs that attempt to register
;; custom completion categories (e.g., from project.el). Provide a no-op
;; early, before requiring `project', to keep batch loads and CI green.
(unless (fboundp 'define-completion-category)
  (defun define-completion-category (&rest _args)
    nil))

(require 'project)
(require 'hub-utils)
;; Allow batch parse/tests to load this file even if perspective is not installed
(unless (require 'perspective nil 'noerror)
  ;; Minimal fallbacks so this module can load without the package.
  ;; Intentionally do NOT (provide 'perspective): tests may install their own stubs.
  (defvar persp-switch-hook nil)
  (defvar persp--current "main")
  (defvar persp--names '("main"))
  (defun persp-switch (name)
    (setq persp--current name
	  persp--names (delete-dups (cons name persp--names)))
    t)
  (defun persp-current-name () persp--current)
  (defun persp-names () persp--names)
  (defun persp-add-buffer (&rest _args) t))

(defgroup hub/persp-auto nil
  "Automatically manage perspectives per project."
  :group 'perspective)

(defcustom hub/persp-auto-switch t
  "When non-nil, switch to the project's perspective upon visiting a file."
  :type 'boolean
  :group 'hub/persp-auto)

(defcustom hub/persp-auto-open-treemacs t
  "When non-nil, open Treemacs when creating a new project perspective."
  :type 'boolean
  :group 'hub/persp-auto)

(defcustom hub/persp-treemacs-follow t
  "When non-nil, enable `treemacs-follow-mode' to track visited files."
  :type 'boolean
  :group 'hub/persp-auto)

(defcustom hub/persp-ignore-roots nil
  "List of directory prefixes to ignore for auto perspectives."
  :type '(repeat directory)
  :group 'hub/persp-auto)

(defcustom hub/persp-ignore-remote t
  "When non-nil, ignore remote files for auto perspectives."
  :type 'boolean
  :group 'hub/persp-auto)

(defcustom hub/persp-name-function
  (lambda (proj)
    (let* ((root (car (project-roots proj)))
	   (name (if (fboundp 'project-name)
		     (project-name proj)
		   (file-name-nondirectory (directory-file-name root)))))
      name))
  "Function of one arg PROJ returning a perspective name."
  :type 'function
  :group 'hub/persp-auto)

(defcustom hub/persp-add-existing-buffers nil
  "When creating a new perspective, add existing project buffers to it."
  :type 'boolean
  :group 'hub/persp-auto)

(defun hub/persp--ignored-root-p (root)
  "Return non-nil if ROOT should be ignored based on `hub/persp-ignore-roots'."
  (seq-some (lambda (pfx) (string-prefix-p (file-truename pfx) (file-truename root)))
	    hub/persp-ignore-roots))

(defun hub/persp--project-for-current-buffer ()
  "Return current project, or nil. Applies remote/ignore filters."
  (let ((fname (buffer-file-name (current-buffer))))
    (when (and fname
	       (or (not hub/persp-ignore-remote) (not (file-remote-p fname))))
      (let ((proj (project-current nil)))
	(when proj
	  (let ((root (car (project-roots proj))))
	    (unless (hub/persp--ignored-root-p root)
	      proj)))))))

(defun hub/persp--treemacs-align-to-project (proj)
  "Ensure Treemacs shows PROJ's root. Fall back to find-file alignment."
  (when (fboundp 'treemacs)
    (let ((default-directory (car (project-roots proj))))
      (cond
       ((fboundp 'treemacs-display-current-project-exclusively)
	(ignore-errors (treemacs-display-current-project-exclusively)))
       ((fboundp 'treemacs-add-and-display-current-project)
	(ignore-errors (treemacs-add-and-display-current-project)))
       ((fboundp 'treemacs-find-file)
	(ignore-errors (treemacs-find-file))))
      (when (and hub/persp-treemacs-follow (fboundp 'treemacs-follow-mode))
	(treemacs-follow-mode 1)))))

(defun hub/persp--treemacs-visible-p ()
  "Return non-nil when Treemacs has a visible window in this frame."
  (or (and (fboundp 'treemacs-get-local-window)
	   (treemacs-get-local-window))
      (get-buffer-window "*Treemacs*" t)))

(defun hub/persp--visit-file-handler ()
  "Hook to create/switch to project perspective when visiting files."
  (when (and (not (minibufferp))
	     (buffer-file-name (current-buffer)))
    (let* ((buf (current-buffer))
	   (proj (hub/persp--project-for-current-buffer)))
      (when proj
	(let* ((name (funcall hub/persp-name-function proj))
	       (need-switch (and hub/persp-auto-switch
				 (not (string= (persp-current-name) name))))
	       (creating (and need-switch (not (member name (persp-names))))))
	  ;; Perform actions synchronously to avoid race conditions with timers
	  (when need-switch (persp-switch name))
	  (when (and (buffer-live-p buf) (fboundp 'persp-add-buffer))
	    (ignore-errors (persp-add-buffer buf)))
	  ;; keep requested file visible
	  (when (buffer-live-p buf)
	    (ignore-errors (switch-to-buffer buf)))
	  ;; Optionally add other buffers on create
	  (when (and hub/persp-add-existing-buffers creating)
	    (dolist (b (project-buffers proj))
	      (when (buffer-live-p b)
		(ignore-errors (persp-add-buffer b)))))
	  ;; Open Treemacs only on first create if configured, then align
	  (when (and hub/persp-auto-open-treemacs creating (fboundp 'treemacs))
	    (ignore-errors (treemacs)))
	  ;; Align Treemacs to project if it is visible (don’t pop it on switches)
	  (when (hub/persp--treemacs-visible-p)
	    (hub/persp--treemacs-align-to-project proj))
	  ;; After potentially popping Treemacs, return focus to requested file
	  (when (buffer-live-p buf)
	    (let ((win (get-buffer-window buf t)))
	      (when (window-live-p win)
		(select-window win)))))))))
(defun hub/persp--align-treemacs-on-persp-switch (&rest _args)
  "Align Treemacs to a project's root when the perspective changes."
  (when (and (fboundp 'treemacs)
	     (hub/persp--treemacs-visible-p))
    (let ((proj (project-current nil)))
      (when proj
	(save-selected-window
	  (hub/persp--treemacs-align-to-project proj))))))

(define-minor-mode hub/persp-auto-project-mode
  "Automatically create/switch to a perspective per project on file visit."
  :global t
  :group 'hub/persp-auto
  (if hub/persp-auto-project-mode
      (progn
	(add-hook 'find-file-hook #'hub/persp--visit-file-handler)
	(add-hook 'persp-switch-hook #'hub/persp--align-treemacs-on-persp-switch)
	;; Process current buffer immediately on enable to cover already-open files
	(when (buffer-file-name (current-buffer))
	  (hub/persp--visit-file-handler)))
    (remove-hook 'find-file-hook #'hub/persp--visit-file-handler)
    (remove-hook 'persp-switch-hook #'hub/persp--align-treemacs-on-persp-switch)))

;; Enable by default only when interactive
(when (and (boundp 'noninteractive) (not noninteractive))
  (hub/persp-auto-project-mode 1))
(provide 'navigation/perspective-auto)
;;; perspective-auto.el ends here
