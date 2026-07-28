;;; magit.el --- VCS: Magit autoloads and configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Lazy Magit command surface and post-load configuration.

;;; Code:

(require 'hub-utils)

(use-package magit
  :commands (magit-status magit-dispatch magit-file-dispatch)
  :init
  (define-key evil-normal-state-map (kbd ",vs") #'magit-status)
  (define-key evil-normal-state-map (kbd ",vh") #'magit-dispatch)
  (define-key evil-normal-state-map (kbd ",vf") #'magit-file-dispatch)
  :config
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
