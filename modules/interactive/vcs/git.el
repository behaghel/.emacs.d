;;; git.el --- VCS: Magit, diff-hl, gist -*- lexical-binding: t; -*-

;;; Commentary:
;; Git tools and keybindings.

;;; Code:

(use-package gist
  :commands (gist-buffer gist-buffer-private gist-region gist-region-private gist-list))

(setq vc-follow-symlinks t)

;; trigger command keybinding shouldn't live inside (use-package):
;; chicken/egg problem
(define-key evil-normal-state-map (kbd ",vs") 'magit-status)
(use-package magit
  :commands (magit-status)
  :config
  (setq magit-popup-use-prefix-argument 'default)
  (global-git-commit-mode)
  (defadvice Info-follow-nearest-node (around gitman activate)
    "Open gitman references via `man' instead of Info."
    (let ((node (Info-get-token (point) "\\*note[ \n\t]+"
				"\\*note[ \n\t]+\\([^:]*\\):\\(:\\|[ \n\t]*(\\)?")))
      (if (and node (string-match "^(gitman)\\(.+\\)" node))
	  (progn (require 'man) (man (match-string 1 node)))
	ad-do-it)))
  (define-key evil-normal-state-map (kbd ",vh") 'magit-file-popup)
  (define-key evil-normal-state-map (kbd ",vf") 'magit-file-dispatch)
  (define-key evil-normal-state-map (kbd ",vB") 'vc-annotate)
  (define-key evil-normal-state-map (kbd ",vg") 'vc-git-grep)
  (define-key evil-normal-state-map (kbd ",v/") 'vc-git-grep)
  (define-key evil-normal-state-map (kbd ",vD") 'ediff-revision))

(use-package ssh-agency
  :if window-system
  :config (setenv "SSH_ASKPASS" "git-gui--askpass"))

(use-package diff-hl
  :demand t
  :config
  (define-key evil-normal-state-map (kbd ",vr") 'diff-hl-revert-hunk)
  (define-key evil-normal-state-map (kbd ",vn") 'diff-hl-next-hunk)
  (define-key evil-normal-state-map (kbd ",vp") 'diff-hl-previous-hunk)
  (define-key evil-normal-state-map (kbd ",vd") 'diff-hl-diff-goto-hunk)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

(provide 'vcs/git)
;;; git.el ends here
