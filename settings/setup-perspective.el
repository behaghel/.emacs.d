;;; setup-perspective.el --- setup perspectives -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:
(use-package perspective
  :defer t
  :general (:keymaps 'persp-mode-map
                     :states 'normal
                     "gP" #'persp-switch
                     "g," #'persp-switch-last
                     ",p," #'persp-switch-to-buffer*
                     ",pk" #'persp-kill-buffer*
                     ",p-" #'persp-remove-buffer ; disassociate buffer from persp
                     ",p+" #'persp-add-buffer    ; associate buffer to current persp
                     ",pM" #'persp-set-buffer    ; like add but remove from all other
                     ",pR" #'persp-rename
                     ",pX" #'persp-kill          ; terminate perspective
                     )
  :custom
  (persp-sort 'access)
  (persp-show-modestring t)
  (persp-modestring-short t)
  (persp-suppress-no-prefix-key-warning t)
  :init
  (persp-mode)
  :config
  (defun hub/speed-dial (key persp &optional fpath command)
    (let ((f `(lambda ()
                (interactive)
                (persp-switch ,persp)
                ,@(when fpath `((find-file ,fpath)))
                ,@(when command `((,command)))
                )
             ))
      (define-key evil-normal-state-map (kbd (concat ",o" key)) (eval f)))
    )
  (setq hub/speed-dial-items
        `(
          ("E" perspective ".emacs.d")
          ("e" file ,(expand-file-name "init.el" user-emacs-directory) ".emacs.d")
          ("n" file "~/nixos-config/configurations/home/hubertbehaghel.nix" "nixos-config")
          ("O" perspective "org")
          ;; ("s" file ,(concat org-directory "sas.org") "org")
          ("i" file ,(concat org-directory "inbox.org") "org")
          ("h" file ,(concat org-directory "hubert.org") "org")
          ("m" command mu4e-sidebar "mails")
          ("M" perspective "mails")
          ("d" perspective "main")
          ("f" command elfeed "feeds")
          ("t" command treemacs "Treemacs")
          ("T" command treemacs-find-file "Treemacs Find File")
          ))
  (defun hub/setup-speed-dial ()
    "Install global keybindings on normal mode with prefix ',o'
    for every item in var hub/speed-dial-items."
    (dolist (binding hub/speed-dial-items)
      (pcase binding
        (`(,key perspective ,persp) (hub/speed-dial key persp))
        (`(,key file ,path ,persp) (hub/speed-dial key persp path))
        (`(,key command ,cmd ,persp) (hub/speed-dial key persp nil cmd))
        )
      )
    )
  (hub/setup-speed-dial)

  (defun mu4e-sidebar ()
    (interactive)
    (mu4e)
    (find-file (expand-file-name "settings/mail-sidebar.org" user-emacs-directory)))

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
                     ",pb" #'project-compile
                     )
  :hook
  (perspective-project-bridge-mode . (lambda ()
                                       (if perspective-project-bridge-mode
                                           (perspective-project-bridge-find-perspectives-for-all-buffers)
                                         (perspective-project-bridge-kill-perspectives))))
  (persp-mode . perspective-project-bridge-mode)
  )

(use-package consult
  :general
  (:keymaps '(normal)
            ",hh" #'consult-history
            ",hm" #'consult-man
            ",hi" #'consult-info
            "gB" #'consult-buffer-other-window ;; orig. switch-to-buffer-other-window
            "gm" #'consult-bookmark            ;; orig. bookmark-jump
            "gb" #'consult-project-buffer      ;; orig. project-switch-to-buffer
            ;; M-g bindings in `goto-map'
            "g}" #'consult-compile-error
            "ge" #'consult-flymake             ;; Alternative: consult-flycheck
            "g." #'consult-goto-line           ;; orig. goto-line
            "gh" #'consult-outline             ;; Alternative: consult-org-heading
            ;; M-s bindings in `search-map'
            ",e/" #'consult-find               ;; Alternative: consult-fd
            ",e." #'consult-locate
            ",eg" #'consult-grep
            ",eG" #'consult-git-grep
            ",er" #'consult-ripgrep
            ",e;" #'consult-line
            ",eL" #'consult-line-multi
            ",ek" #'consult-keep-lines
            ",eu" #'consult-focus-lines)       ;; orig. previous-matching-history-element
  (;; C-c bindings in `mode-specific-map'
   :keymaps '(normal insert)
   "C-c M-x" #'consult-mode-command
   "M-y" #'consult-yank-pop                ;; orig. yank-pop
   ;; C-x bindings in `ctl-x-map'
   "C-x M-:" #'consult-complex-command     ;; orig. repeat-complex-command
   "C-x b" #'consult-buffer                ;; orig. switch-to-buffer
   )
  ;; Minibuffer history
  (:keymaps 'minibuffer-local-map
            :states '(normal insert)
            "M-s" #'consult-history                 ;; orig. next-matching-history-element
            "M-r" #'consult-history
            )

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)
  )

(winner-mode 1)
(define-key evil-window-map (kbd "u") 'winner-undo)
(define-key evil-window-map (kbd "U") 'winner-redo)

(provide 'setup-perspective)
;;; setup-perspective.el ends here
