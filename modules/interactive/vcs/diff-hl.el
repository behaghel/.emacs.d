;;; diff-hl.el --- VCS: diff-hl gutter integration -*- lexical-binding: t; -*-

;;; Commentary:
;; Diff-hl configuration and keybindings.

;;; Code:

(defcustom hub/diff-hl-file-idle-delay 0.5
  "Idle delay before enabling Diff-HL after visiting a file."
  :type 'number
  :group 'vc)

(defun hub/diff-hl-enable-for-file-buffer ()
  "Enable Diff-HL for the current file buffer after a short idle delay."
  (when (and buffer-file-name (not (file-remote-p buffer-file-name)))
    (let ((buffer (current-buffer)))
      (run-with-idle-timer
       hub/diff-hl-file-idle-delay nil
       (lambda ()
	 (when (buffer-live-p buffer)
	   (with-current-buffer buffer
	     (when buffer-file-name
	       (diff-hl-mode 1)))))))))

(use-package diff-hl
  :commands (diff-hl-revert-hunk
	     diff-hl-next-hunk
	     diff-hl-previous-hunk
	     diff-hl-diff-goto-hunk
	     global-diff-hl-mode
	     diff-hl-mode)
  :init
  (define-key evil-normal-state-map (kbd ",vr") #'diff-hl-revert-hunk)
  (define-key evil-normal-state-map (kbd ",vn") #'diff-hl-next-hunk)
  (define-key evil-normal-state-map (kbd ",vp") #'diff-hl-previous-hunk)
  (define-key evil-normal-state-map (kbd ",vd") #'diff-hl-diff-goto-hunk)
  (add-hook 'find-file-hook #'hub/diff-hl-enable-for-file-buffer)
  :config
  (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh))

(provide 'vcs/diff-hl)
;;; diff-hl.el ends here
