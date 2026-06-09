;;; git.el --- VCS: Git entrypoint -*- lexical-binding: t; -*-

;;; Commentary:
;; Git-related tools and keybindings.  Package-specific activation lives in
;; smaller sibling modules so startup behavior stays easy to audit.

;;; Code:

(use-package gist
  :commands (gist-buffer gist-buffer-private gist-region gist-region-private gist-list))

(setq vc-follow-symlinks t)

(define-key evil-normal-state-map (kbd ",vB") #'vc-annotate)
(define-key evil-normal-state-map (kbd ",vg") #'vc-git-grep)
(define-key evil-normal-state-map (kbd ",v/") #'vc-git-grep)
(define-key evil-normal-state-map (kbd ",vD") #'ediff-revision)

(when (or (not (fboundp 'hub/startup-debug-stage-at-least-p))
	  (hub/startup-debug-stage-at-least-p 'vcs-magit))
  (require 'vcs/magit))

(when (or (not (fboundp 'hub/startup-debug-stage-at-least-p))
	  (hub/startup-debug-stage-at-least-p 'vcs-ssh))
  (require 'vcs/ssh))

(when (or (not (fboundp 'hub/startup-debug-stage-at-least-p))
	  (hub/startup-debug-stage-at-least-p 'vcs-diff-hl))
  (require 'vcs/diff-hl))

(provide 'vcs/git)
;;; git.el ends here
