;;; perspective-auto.el --- Navigation: auto project perspectives -*- lexical-binding: t; -*-

;;; Commentary:
;; Automatically create/switch to a perspective per project when visiting files.
;; Uses project.el for detection and perspective.el for isolation. Optionally
;; opens Treemacs when creating a new project perspective.

;;; Code:

(require 'project)
(require 'perspective)

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

(defun hub/persp--visit-file-handler ()
  "Hook to create/switch to project perspective when visiting files."
  (when (and (not (minibufferp))
	     (buffer-file-name (current-buffer)))
    (let ((proj (hub/persp--project-for-current-buffer)))
      (when proj
	(let* ((name (funcall hub/persp-name-function proj))
	       (existing (member name (persp-names))))
	  ;; Switch (creates if missing)
	  (when hub/persp-auto-switch
	    (persp-switch name))
	  ;; Add buffer explicitly if API exists
	  (when (fboundp 'persp-add-buffer)
	    (ignore-errors (persp-add-buffer (current-buffer))))
	  ;; Optionally add other buffers in project when creating
	  (when (and hub/persp-add-existing-buffers (not existing))
	    (dolist (buf (project-buffers proj))
	      (when (buffer-live-p buf)
		(ignore-errors (persp-add-buffer buf)))))
	  ;; Open sidebar on first create
	  (unless existing
	    (hub/persp--maybe-open-treemacs)))))))

(define-minor-mode hub/persp-auto-project-mode
  "Automatically create/switch to a perspective per project on file visit."
  :global t
  :group 'hub/persp-auto
  (if hub/persp-auto-project-mode
      (progn
	(add-hook 'find-file-hook #'hub/persp--visit-file-handler)
	(add-hook 'persp-switch-hook #'hub/persp--align-treemacs-on-persp-switch))
    (remove-hook 'find-file-hook #'hub/persp--visit-file-handler)
    (remove-hook 'persp-switch-hook #'hub/persp--align-treemacs-on-persp-switch)))

;; Enable by default in interactive sessions
(hub/persp-auto-project-mode 1)

(provide 'navigation/perspective-auto)
;;; perspective-auto.el ends here


(defun hub/persp--align-treemacs-on-persp-switch (_new _frame)
  "Align Treemacs to a project's root when the perspective changes."
  (when (and (fboundp 'treemacs) (buffer-live-p (current-buffer)))
    (let* ((buf (current-buffer))
	   (fname (buffer-file-name buf)))
      (when fname
	(let ((proj (project-current nil)))
	  (when proj
	    (hub/persp--treemacs-align-to-project proj))))))
