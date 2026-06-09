;;; magit.el --- VCS: Magit autoloads and configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Lazy Magit command surface and post-load configuration.

;;; Code:

;; Trigger command keybindings should not live inside a `use-package' form here:
;; startup binds them before Magit is loaded.
(define-key evil-normal-state-map (kbd ",vs") #'magit-status)

;; Avoid a startup-time `use-package' declaration for Magit.  Magit's custom
;; initializers can emit a late startup "void-variable symbol" message even when
;; declared with `:commands'.  Keep commands autoloaded and configure Magit only
;; after the user actually invokes it.
;; Do not call `straight-use-package' here: even package setup is enough to
;; trigger Magit's late custom initializer during startup in this config.
;; Magit is pinned/installed by the lockfile; rely on generated autoloads and
;; load it only when a command is invoked.
(autoload 'magit-status "magit" nil t)
(autoload 'magit-file-dispatch "magit" nil t)
(autoload 'magit-file-popup "magit" nil t)
(define-key evil-normal-state-map (kbd ",vh") #'magit-file-popup)
(define-key evil-normal-state-map (kbd ",vf") #'magit-file-dispatch)

(with-eval-after-load 'magit
  (setq magit-popup-use-prefix-argument 'default)
  (global-git-commit-mode)
  (define-advice Info-follow-nearest-node (:around (orig-fn &rest args) hub/gitman)
    "Open gitman references via `man' instead of Info."
    (let ((node (Info-get-token (point) "\\*note[ \n\t]+"
				"\\*note[ \n\t]+\\([^:]*\\):\\(:\\|[ \n\t]*(\\)?")))
      (if (and node (string-match "^(gitman)\\(.+\\)" node))
	  (progn
	    (require 'man)
	    (man (match-string 1 node)))
	(apply orig-fn args)))))

(provide 'vcs/magit)
;;; magit.el ends here
