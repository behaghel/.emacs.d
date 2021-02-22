;;; Git
(use-package gist
  :commands (gist-buffer gist-buffer-private gist-region gist-region-private gist-list))

(define-key evil-normal-state-map (kbd ",vs") 'magit-status) ;; git control panel
(setq vc-follow-symlinks t)
(use-package magit
  :straight (:depth 'full)
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
  (define-key evil-normal-state-map (kbd ",vf") 'magit-file-popup) ; Commit history for current file
  (define-key evil-normal-state-map (kbd ",vb") 'magit-blame-popup) ; Blame for current file
  (define-key evil-normal-state-map (kbd ",vB") 'vc-annotate) ; Git blame with vc
  (define-key evil-normal-state-map (kbd ",vg") 'vc-git-grep) ; Git grep
  (define-key evil-normal-state-map (kbd ",v/") 'vc-git-grep) ; Git grep
  (define-key evil-normal-state-map (kbd ",vD") 'ediff-revision) ; Git diff file on 2 branches

  ;; (use-package git-gutter+
  ;;   :diminish git-gutter+-mode
  ;;   :config
  ;;    ;;; Jump between hunks
  ;;   (define-key git-gutter+-mode-map (kbd "C-c f n") 'git-gutter+-next-hunk)
  ;;   (define-key git-gutter+-mode-map (kbd "C-c f p") 'git-gutter+-previous-hunk)

  ;;    ;;; Act on hunks
  ;;   (define-key git-gutter+-mode-map (kbd "C-c f =") 'git-gutter+-show-hunk)
  ;;   (define-key git-gutter+-mode-map (kbd "C-c f r") 'git-gutter+-revert-hunks)
  ;;   ;; Stage hunk at point.
  ;;   ;; If region is active, stage all hunk lines within the region.
  ;;   (define-key git-gutter+-mode-map (kbd "C-c f s") 'git-gutter+-stage-hunks)
  ;;   (define-key git-gutter+-mode-map (kbd "C-c f c") 'git-gutter+-commit)
  ;;   (define-key git-gutter+-mode-map (kbd "C-c f C") 'git-gutter+-stage-and-commit)
  ;;   ;; (define-key evil-normal-state-map (kbd ",vr") 'git-gutter+-revert-hunk)
  ;;   ;; (define-key evil-normal-state-map (kbd ",v+") 'git-gutter+-stage-hunks)
  ;;   ;; (define-key evil-normal-state-map (kbd ",vn") 'git-gutter+-next-hunk)
  ;;   ;; (define-key evil-normal-state-map (kbd ",vp") 'git-gutter+-previous-hunk)
  ;;   ;; (define-key evil-normal-state-map (kbd ",vd") 'git-gutter+-show-hunk)
  ;;   )
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
  (global-diff-hl-mode)
  (define-key evil-normal-state-map (kbd ",vr") 'diff-hl-revert-hunk)
  (define-key evil-normal-state-map (kbd ",vn") 'diff-hl-next-hunk)
  (define-key evil-normal-state-map (kbd ",vp") 'diff-hl-previous-hunk)
  (define-key evil-normal-state-map (kbd ",vd") 'diff-hl-diff-goto-hunk)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

(provide 'setup-git)
