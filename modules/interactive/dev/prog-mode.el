;;; prog-mode.el --- Programming buffer defaults -*- lexical-binding: t; -*-

;;; Commentary:
;; Cross-language programming buffer behavior that is independent of specific
;; language modules.

;;; Code:

(require 'hub-utils)

(defcustom hub/flyspell-prog-idle-delay 1.0
  "Idle delay before enabling Flyspell in visible programming buffers."
  :type 'number
  :group 'convenience)

(defvar-local hub/flyspell-prog--enable-scheduled nil
  "Non-nil when Flyspell enablement is already scheduled for this buffer.")

(defun hub/flyspell-prog-enable-visible-buffer (window)
  "Schedule Flyspell for WINDOW's visible programming buffer."
  (when (window-live-p window)
    (with-current-buffer (window-buffer window)
      (when (and (derived-mode-p 'prog-mode)
		 (not (bound-and-true-p flyspell-mode))
		 (not hub/flyspell-prog--enable-scheduled))
	(setq hub/flyspell-prog--enable-scheduled t)
	(let ((buffer (current-buffer)))
	  (run-with-idle-timer
	   hub/flyspell-prog-idle-delay nil
	   (lambda ()
	     (when (buffer-live-p buffer)
	       (with-current-buffer buffer
		 (setq hub/flyspell-prog--enable-scheduled nil)
		 (when (and (derived-mode-p 'prog-mode)
			    (not (bound-and-true-p flyspell-mode)))
		   (flyspell-prog-mode)))))))))))

(add-hook 'window-buffer-change-functions #'hub/flyspell-prog-enable-visible-buffer)

(add-hook 'prog-mode-hook
	  (lambda ()
	    (subword-mode)
	    (turn-on-auto-fill)
	    (electric-indent-local-mode)
	    (define-key evil-normal-state-map (kbd ",br") #'recompile)
	    (define-key evil-normal-state-map (kbd ",bx") #'kill-compilation)
	    (define-key evil-insert-state-map (kbd "M-RET") #'indent-new-comment-line)
	    (define-key evil-normal-state-map (kbd "g)") #'flymake-goto-next-error)
	    (define-key evil-normal-state-map (kbd "g(") #'flymake-goto-previous-error)))

(provide 'dev/prog-mode)
;;; prog-mode.el ends here
