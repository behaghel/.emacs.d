;;; diff-hl.el --- VCS: diff-hl gutter integration -*- lexical-binding: t; -*-

;;; Commentary:
;; Diff-hl configuration and keybindings.

;;; Code:

(defcustom hub/diff-hl-startup-idle-delay 2.0
  "Idle delay before enabling Diff-HL after startup."
  :type 'number
  :group 'vc)

(defun hub/diff-hl-enable-after-startup ()
  "Enable global Diff-HL after startup has been idle."
  (run-with-idle-timer hub/diff-hl-startup-idle-delay nil #'global-diff-hl-mode 1))

(use-package diff-hl
  :commands (diff-hl-revert-hunk
	     diff-hl-next-hunk
	     diff-hl-previous-hunk
	     diff-hl-diff-goto-hunk
	     global-diff-hl-mode)
  :init
  (define-key evil-normal-state-map (kbd ",vr") #'diff-hl-revert-hunk)
  (define-key evil-normal-state-map (kbd ",vn") #'diff-hl-next-hunk)
  (define-key evil-normal-state-map (kbd ",vp") #'diff-hl-previous-hunk)
  (define-key evil-normal-state-map (kbd ",vd") #'diff-hl-diff-goto-hunk)
  (add-hook 'emacs-startup-hook #'hub/diff-hl-enable-after-startup 90)
  :config
  (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh))

(provide 'vcs/diff-hl)
;;; diff-hl.el ends here
