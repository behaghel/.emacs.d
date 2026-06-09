;;; diff-hl.el --- VCS: diff-hl gutter integration -*- lexical-binding: t; -*-

;;; Commentary:
;; Diff-hl configuration and keybindings.

;;; Code:

(use-package diff-hl
  :demand t
  :config
  (define-key evil-normal-state-map (kbd ",vr") #'diff-hl-revert-hunk)
  (define-key evil-normal-state-map (kbd ",vn") #'diff-hl-next-hunk)
  (define-key evil-normal-state-map (kbd ",vp") #'diff-hl-previous-hunk)
  (define-key evil-normal-state-map (kbd ",vd") #'diff-hl-diff-goto-hunk)
  (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh))

(provide 'vcs/diff-hl)
;;; diff-hl.el ends here
