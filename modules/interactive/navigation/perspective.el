;;; perspective.el --- Navigation: perspectives setup -*- lexical-binding: t; -*-

;;; Commentary:
;; Perspective and project bridge configuration, speed-dial helpers, and
;; consult bindings. Migrated from legacy setup-perspective.el.

;;; Code:

(defvar org-directory (expand-file-name "org/" (or (getenv "HOME") "~"))
  "Base directory for Org files.
This is overridden by `org/core' during init. Consumers should not
capture its value at load time; compute paths at call time instead.")

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
  (defun hub/speed-dial (key persp &optional fpath command)
    (let* ((path-form (cond
		       ((and fpath (functionp fpath)) `(find-file (funcall ,fpath)))
		       ((and fpath (listp fpath) (eq (car-safe fpath) 'lambda)) `(find-file (funcall ,fpath)))
		       (fpath `(find-file ,fpath))
		       (t nil)))
	   (f `(lambda ()
		 (interactive)
		 (persp-switch ,persp)
		 ,@ (when path-form (list path-form))
		 ,@ (when command `((,command))))))
      (define-key evil-normal-state-map (kbd (concat ",o" key)) (eval f))))

  (setq hub/speed-dial-items
	`(("E" perspective ".emacs.d")
	  ("e" file ,(expand-file-name "init.el" user-emacs-directory) ".emacs.d")
	  ("n" file "~/nixos-config/configurations/home/hubertbehaghel.nix" "nixos-config")
	  ("O" perspective "org")
	  ("i" file ,(lambda () (concat org-directory "inbox.org")) "org")
	  ("h" file ,(lambda () (concat org-directory "hubert.org")) "org")
	  ("m" command mu4e-sidebar "mails")
	  ("M" perspective "mails")
	  ("d" perspective "main")
	  ("f" command elfeed "feeds")
	  ("t" command treemacs "Treemacs")
	  ("T" command treemacs-find-file "Treemacs Find File")))

  (defun hub/setup-speed-dial ()
    "Install ',o' speed-dial bindings from `hub/speed-dial-items'."
    (dolist (binding hub/speed-dial-items)
      (pcase binding
	(`(,key perspective ,persp) (hub/speed-dial key persp))
	(`(,key file ,path ,persp) (hub/speed-dial key persp path))
	(`(,key command ,cmd ,persp) (hub/speed-dial key persp nil cmd))))
    )
  (hub/setup-speed-dial)

  (defun mu4e-sidebar ()
    (interactive)
    ;; Show mu4e main in the current window
    (mu4e)
    ;; Display the dashboard sidebar as a left side-window, keep focus on main
    (let* ((path (expand-file-name "modules/interactive/email/mail-sidebar.org" user-emacs-directory))
	   (buf  (find-file-noselect path)))
      (display-buffer-in-side-window
       buf '((side . left)
	     (slot . -1)
	     (window-width . 10)
	     (window-parameters . ((no-delete-other-windows . t)
				   (no-other-window . t)))))
      (balance-windows)))

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
