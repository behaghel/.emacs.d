;;; perspective.el --- Navigation: perspectives setup -*- lexical-binding: t; -*-

;;; Commentary:
;; Perspective and project bridge configuration, speed-dial helpers, and
;; consult bindings. Migrated from legacy setup-perspective.el.

;;; Code:

(require 'hub-utils)
(defvar org-directory (expand-file-name "Documents/org/" (or (getenv "HOME") "~"))
  "Base directory for Org files.
This is overridden by `org/core' during init. Consumers should not
capture its value at load time; compute paths at call time instead.")

(defvar hub/speed-dial--registry nil
  "Hash table mapping speed-dial keys to their open functions.")

(defun hub/speed-dial--const-fn (value)
  "Return a no-arg function that yields VALUE without needing lexical scope."
  (eval `(lambda () ,value)))

(defun hub/speed-dial--find-file-fn (path-fn)
  "Return a function that visits the PATH-FN result without lexical scope."
  (eval `(lambda () (find-file (funcall ,path-fn)))))

(defun hub/speed-dial--file-with-tree-fn (persp path)
  "Return a function that switches to PERSP and opens PATH, showing Treemacs."
  (eval
   `(lambda ()
      (interactive)
      (let ((exists (member ,persp (persp-names))))
	(persp-switch ,persp)
	(find-file ,path)
	(unless exists (hub/open-treemacs-sidebar))))))

(use-package perspective
  :defer t
  :general (:keymaps 'persp-mode-map
		     :states 'normal
		     "gP" #'persp-switch
		     "g," #'persp-switch-last
		     ",p," #'persp-switch-to-buffer*
		     ",pk" #'persp-kill-buffer*
		     ",p-" #'persp-remove-buffer
		     ",p+" #'persp-add-buffer
		     ",pM" #'persp-set-buffer
		     ",pR" #'persp-rename
		     ",pX" #'persp-kill)
  :custom
  (persp-sort 'access)
  (persp-show-modestring t)
  (persp-modestring-short t)
  (persp-suppress-no-prefix-key-warning t)
  :init
  (persp-mode)
  :config
  (defun hub/speed-dial--open-fn (persp &optional path-fn command)
    "Return an interactive function to switch to PERSP and run helpers.
PATH-FN, when non-nil, is called after the switch to open a file.
COMMAND, when non-nil, is funcalled after the switch/path."
    ;; Build with constants so it works even when lexical-binding is disabled.
    (eval
     `(lambda ()
	(interactive)
	(persp-switch ,persp)
	,@(when path-fn
	    `((when ,path-fn (funcall ,path-fn))))
	,@(when command
	    `((when ,command (funcall ,command)))))))

  (defun hub/speed-dial--lookup (key)
    "Return OPEN-FN for KEY from `hub/speed-dial--registry', or nil."
    (when (hash-table-p hub/speed-dial--registry)
      (gethash key hub/speed-dial--registry)))

  (defun hub/speed-dial--lookup-or-error (key)
    "Return OPEN-FN for KEY, raising if not registered."
    (or (hub/speed-dial--lookup key)
	(user-error "No speed-dial binding registered for %s" key)))

  (defun hub/speed-dial--register (key open-fn)
    "Store OPEN-FN under KEY for reuse."
    (unless (hash-table-p hub/speed-dial--registry)
      (setq hub/speed-dial--registry (make-hash-table :test #'equal)))
    (puthash key open-fn hub/speed-dial--registry))

  (defun hub/speed-dial--command (key)
    "Return an interactive command that runs the speed-dial OPEN-FN for KEY."
    (eval
     `(lambda ()
	(interactive)
	(funcall (hub/speed-dial--lookup-or-error ,key)))))

  (defun hub/speed-dial--resume (key persp)
    "Return an interactive command that resumes PERSP or opens via KEY."
    (eval
     `(lambda ()
	(interactive)
	(if (member ,persp (persp-names))
	    (persp-switch ,persp)
	  (funcall (hub/speed-dial--lookup-or-error ,key))))))

  (defun hub/speed-dial--binding-persp (binding)
    "Return the perspective name for BINDING entry, or nil."
    (pcase binding
      (`(,_ perspective ,persp) persp)
      (`(,_ file ,_ ,persp) persp)
      (`(,_ file-with-tree ,_ ,persp) persp)
      (`(,_ command ,_ ,persp) persp)))

  (defun hub/speed-dial--define (key persp key-persps open-fn)
    "Define ,oKEY to OPEN-FN and ,o(KEY uppercased) to resume PERSP.
KEY-PERSPS is an alist of keys to their perspectives, used to avoid
overwriting intentional uppercase bindings that target other spaces."
    (hub/speed-dial--register key open-fn)
    (define-key evil-normal-state-map (kbd (concat ",o" key))
		(hub/speed-dial--command key))
    (let* ((resume-key (upcase key))
	   (existing (assoc-string resume-key key-persps))
	   (same-persp (and existing (string= (cdr existing) persp))))
      (when (and (not (equal resume-key key))
		 (or (not existing) same-persp))
	;; Resume-key shares OPEN-FN from KEY; stash for clarity.
	(hub/speed-dial--register resume-key open-fn)
	(define-key evil-normal-state-map (kbd (concat ",o" resume-key))
		    (hub/speed-dial--resume key persp)))))

  (defun hub/open-treemacs-sidebar ()
    "Ensure Treemacs is open as a sidebar without stealing focus."
    (let ((cur (current-buffer)))
      (when (fboundp 'treemacs)
	(treemacs))
      (when (buffer-live-p cur)
	(let ((win (get-buffer-window cur t)))
	  (when (window-live-p win)
	    (select-window win))))))

  (setq hub/speed-dial-items
	`(("e" file-with-tree ,(expand-file-name "init.el" user-emacs-directory) ".emacs.d")
	  ("n" file "~/nixos-config/configurations/home/hubertbehaghel.nix" "nixos-config")
	  ("O" perspective "org")
	  ("i" file ,(lambda () (concat org-directory "inbox.org")) "org")
	  ("h" file ,(lambda () (concat org-directory "hubert.org")) "org")
	  ("m" command mu4e-sidebar "mails")
	  ("d" perspective "main")
	  ("f" command elfeed "feeds")
	  ("t" command treemacs "Treemacs")
	  ("T" command treemacs-find-file "Treemacs Find File")))

  (defun hub/setup-speed-dial ()
    "Install ',o' speed-dial bindings from `hub/speed-dial-items'."
    (setq hub/speed-dial--registry (make-hash-table :test #'equal))
    (let ((key-persps (delq nil
			    (mapcar (lambda (binding)
				      (let ((persp (hub/speed-dial--binding-persp binding)))
					(when persp (cons (car binding) persp))))
				    hub/speed-dial-items))))
      (dolist (binding hub/speed-dial-items)
	(pcase binding
	  (`(,key perspective ,persp)
	   (hub/speed-dial--define key persp key-persps
				   (hub/speed-dial--open-fn persp)))
	  (`(,key file ,path ,persp)
	   (let* ((path-fn (cond
			    ((and path (functionp path)) path)
			    ((and path (listp path) (eq (car-safe path) 'lambda)) path)
			    (path (hub/speed-dial--const-fn path))))
		  (open-fn (hub/speed-dial--open-fn
			    persp
			    (when path-fn (hub/speed-dial--find-file-fn path-fn)))))
	     (hub/speed-dial--define key persp key-persps open-fn)))
	  (`(,key file-with-tree ,path ,persp)
	   (hub/speed-dial--define key persp key-persps
				   (hub/speed-dial--file-with-tree-fn persp path)))
	  (`(,key command ,cmd ,persp)
	   (hub/speed-dial--define key persp key-persps
				   (hub/speed-dial--open-fn persp nil cmd)))))))
  (hub/setup-speed-dial)

  (defun mu4e-sidebar ()
    (interactive)
    ;; Open mu4e as the main view
    (mu4e)
    ;; Build dashboard buffer via mu4e-dashboard, but display it as a side window
    (let* ((buf (save-window-excursion
		  (let ((hub/persp--suppress t))
		    (mu4e-dashboard))
		  (current-buffer)))
	   (params '((side . left)
		     (slot . -1)
		     (window-parameters . ((no-delete-other-windows . t)
					   (no-other-window . t))))))
      (when (buffer-live-p buf)
	(let ((win (display-buffer-in-side-window buf params)))
	  ;; Resize the sidebar explicitly in columns; default to 8 if unset
	  (let* ((desired (or (and (boundp 'hub/mu4e-dashboard-sidebar-width)
				   hub/mu4e-dashboard-sidebar-width)
			      36))
		 (cur (window-total-width win)))
	    (when (and (integerp desired) (> desired 1))
	      (window-resize win (- desired cur) t)))))))

  (add-hook 'kill-emacs-hook #'persp-state-save)
  (setq persp-sort 'access
	persp-state-default-file (expand-file-name "persp.state"
						   (expand-file-name "var/" user-emacs-directory)))
  (make-directory (file-name-directory persp-state-default-file) t))

(use-package perspective-project-bridge
  :after (perspective)
  :general (:keymaps '(normal)
		     "gp" #'project-switch-project
		     ",pb" #'project-switch-to-buffer
		     ",P" #'project-other-window-command
		     ",pf" #'project-find-file
		     ",p/" #'project-dired
		     ",ps" #'project-search
		     ",pS" #'project-query-replace-regexp
		     ",p$" #'project-shell
		     ",pe" #'project-eshell
		     ",pb" #'project-compile)
  :hook
  (perspective-project-bridge-mode . (lambda ()
				       (if perspective-project-bridge-mode
					   (perspective-project-bridge-find-perspectives-for-all-buffers)
					 (perspective-project-bridge-kill-perspectives))))
  (persp-mode . perspective-project-bridge-mode))

(use-package consult
  :general
  (:keymaps '(normal)
	    ",hh" #'consult-history
	    ",hm" #'consult-man
	    ",hi" #'consult-info
	    "gB"  #'consult-buffer-other-window
	    "gm"  #'consult-bookmark
	    "gb"  #'consult-project-buffer
	    "g}"  #'consult-compile-error
	    "ge"  #'consult-flymake
	    "g."  #'consult-goto-line
	    "gh"  #'consult-outline
	    ",e/" #'consult-find
	    ",e." #'consult-locate
	    ",eg" #'consult-grep
	    ",eG" #'consult-git-grep
	    ",er" #'consult-ripgrep
	    ",e;" #'consult-line
	    ",eL" #'consult-line-multi
	    ",ek" #'consult-keep-lines
	    ",eu" #'consult-focus-lines)
  (:keymaps '(normal insert)
	    "C-c M-x" #'consult-mode-command
	    "M-y"     #'consult-yank-pop
	    "C-x M-:" #'consult-complex-command
	    "C-x b"   #'consult-buffer)
  (:keymaps 'minibuffer-local-map
	    :states '(normal insert)
	    "M-s" #'consult-history
	    "M-r" #'consult-history)
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :init
  (setq register-preview-delay 0.5
	register-preview-function #'consult-register-format)
  (advice-add #'register-preview :override #'consult-register-window)
  (setq xref-show-xrefs-function #'consult-xref
	xref-show-definitions-function #'consult-xref)
  :config
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   :preview-key '(:debounce 0.4 any))
  (setq consult-narrow-key "<")
  (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help))

(winner-mode 1)
(define-key evil-window-map (kbd "u") 'winner-undo)
(define-key evil-window-map (kbd "U") 'winner-redo)

(provide 'navigation/perspective)
;;; perspective.el ends here
