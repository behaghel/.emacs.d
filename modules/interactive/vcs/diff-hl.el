;;; diff-hl.el --- VCS: diff-hl gutter integration -*- lexical-binding: t; -*-

;;; Commentary:
;; Diff-hl configuration and keybindings.

;;; Code:

(require 'hub-utils)

(defcustom hub/diff-hl-visible-buffer-idle-delay 0.5
  "Idle delay before enabling Diff-HL in a visible file buffer."
  :type 'number
  :group 'vc)

(defvar-local hub/diff-hl--enable-scheduled nil
  "Non-nil when Diff-HL enablement is already scheduled for this buffer.")

(defun hub/diff-hl--eligible-buffer-p ()
  "Return non-nil when the current buffer should get Diff-HL gutters."
  (and buffer-file-name
       (not (file-remote-p buffer-file-name))
       (vc-backend buffer-file-name)
       (not (bound-and-true-p diff-hl-mode))))

(defun hub/diff-hl-enable-visible-buffer (window)
  "Schedule Diff-HL for WINDOW's visible file buffer.
This intentionally reacts to displayed buffers rather than `find-file-hook', so
background file visits such as Org agenda scans do not load Diff-HL during
startup."
  (when (window-live-p window)
    (with-current-buffer (window-buffer window)
      (when (and (hub/diff-hl--eligible-buffer-p)
		 (not hub/diff-hl--enable-scheduled))
	(setq hub/diff-hl--enable-scheduled t)
	(let ((buffer (current-buffer)))
	  (run-with-idle-timer
	   hub/diff-hl-visible-buffer-idle-delay nil
	   (lambda ()
	     (when (buffer-live-p buffer)
	       (with-current-buffer buffer
		 (setq hub/diff-hl--enable-scheduled nil)
		 (when (hub/diff-hl--eligible-buffer-p)
		   (diff-hl-mode 1)))))))))))

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
  (add-hook 'window-buffer-change-functions #'hub/diff-hl-enable-visible-buffer)
  :config
  (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh))

(provide 'vcs/diff-hl)
;;; diff-hl.el ends here
