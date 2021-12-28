;;; Git
(use-package gist
  :commands (gist-buffer gist-buffer-private gist-region gist-region-private gist-list))

(define-key evil-normal-state-map (kbd ",vs") 'magit-status) ;; git control panel
(setq vc-follow-symlinks t)
(use-package magit
  :straight (:depth full)
  :commands (magit-status projectile-vc)
  ;; :pin melpa
  :config
  (setq magit-popup-use-prefix-argument 'default
        magit-completing-read-function 'ivy-completing-read
        )

  (global-git-commit-mode)
  (defadvice Info-follow-nearest-node (around gitman activate)
    "When encountering a cross reference to the `gitman' info
manual, then instead of following that cross reference show
the actual manpage using the function `man'."
    (let ((node (Info-get-token
                 (point) "\\*note[ \n\t]+"
                 "\\*note[ \n\t]+\\([^:]*\\):\\(:\\|[ \n\t]*(\\)?")))
      (if (and node (string-match "^(gitman)\\(.+\\)" node))
          (progn (require 'man)
                 (man (match-string 1 node)))
        ad-do-it)))

  (define-key evil-normal-state-map (kbd ",vh") 'magit-file-popup) ; Commit history for current file
  (define-key evil-normal-state-map (kbd ",vf") 'magit-file-dispatch) ; Commit history for current file
  (define-key evil-normal-state-map (kbd ",vB") 'vc-annotate) ; Git blame with vc
  (define-key evil-normal-state-map (kbd ",vg") 'vc-git-grep) ; Git grep
  (define-key evil-normal-state-map (kbd ",v/") 'vc-git-grep) ; Git grep
  (define-key evil-normal-state-map (kbd ",vD") 'ediff-revision) ; Git diff file on 2 branches
  )

(use-package ssh-agency
  :if window-system
  :config
  (setenv "SSH_ASKPASS" "git-gui--askpass"))
;; (use-package magithub
;;   :disabled
;;   :after magit
;;   :ensure t
;;   :config
;;   (magithub-feature-autoinject t))

(use-package diff-hl
  :demand
  :config
  (define-key evil-normal-state-map (kbd ",vr") 'diff-hl-revert-hunk)
  (define-key evil-normal-state-map (kbd ",vn") 'diff-hl-next-hunk)
  (define-key evil-normal-state-map (kbd ",vp") 'diff-hl-previous-hunk)
  (define-key evil-normal-state-map (kbd ",vd") 'diff-hl-diff-goto-hunk)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

(provide 'setup-git)
