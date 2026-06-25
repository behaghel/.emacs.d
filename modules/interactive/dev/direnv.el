;;; direnv.el --- Deferred direnv integration -*- lexical-binding: t; -*-

;;; Commentary:
;; Apply direnv environments after buffers are visible so navigation stays
;; responsive while environment-sensitive tooling catches up.

;;; Code:

(require 'hub-utils)

(use-package direnv
  :commands (direnv-allow direnv-update-directory-environment direnv-update-environment)
  :init
  (defcustom hub/direnv-idle-delay 0.5
    "Idle seconds before applying direnv for the current buffer."
    :type 'number
    :group 'convenience)

  (defvar hub/direnv--pending-timer nil
    "Pending idle timer for deferred direnv updates.")

  (defvar hub/direnv--pending-directory nil
    "Directory currently waiting for a deferred direnv update.")

  (defvar hub/direnv--last-applied-directory nil
    "Directory most recently applied by the deferred direnv integration.")

  (defun hub/direnv--buffer-directory (&optional buffer)
    "Return local directory relevant for direnv in BUFFER."
    (with-current-buffer (or buffer (current-buffer))
      (let ((dir (or (and buffer-file-name (file-name-directory buffer-file-name))
		     (and (derived-mode-p 'comint-mode 'compilation-mode 'dired-mode 'eshell-mode)
			  default-directory))))
	(when (and dir (not (file-remote-p dir)))
	  (file-name-as-directory (expand-file-name dir))))))

  (defun hub/direnv--project-root-with-envrc (dir)
    "Return nearest ancestor of DIR containing .envrc, or nil."
    (when dir
      (locate-dominating-file dir ".envrc")))

  (defun hub/direnv--active-directory ()
    "Return the directory currently active in `direnv', if known."
    (when (boundp 'direnv--active-directory)
      (symbol-value 'direnv--active-directory)))

  (defun hub/direnv--environment-current-p (dir)
    "Return non-nil when DIR is already the active direnv directory."
    (equal dir (or (hub/direnv--active-directory)
		   hub/direnv--last-applied-directory)))

  (defun hub/direnv--apply-directory (dir)
    "Apply direnv environment for DIR."
    (unwind-protect
	(when (and dir (file-directory-p dir) (not (file-remote-p dir)))
	  (condition-case err
	      (progn
		(direnv-update-directory-environment dir)
		(setq hub/direnv--last-applied-directory dir))
	    (error
	     (message "[direnv] failed for %s: %s" dir (error-message-string err)))))
      (when (equal dir hub/direnv--pending-directory)
	(setq hub/direnv--pending-directory nil))
      (setq hub/direnv--pending-timer nil)))

  (defun hub/direnv-schedule-update (&optional buffer)
    "Schedule a deferred direnv update for BUFFER.
The update runs on an idle timer so file visits and project switches can display
first; environment-sensitive tooling catches up shortly after."
    (let* ((dir (hub/direnv--buffer-directory buffer))
	   (env-root (hub/direnv--project-root-with-envrc dir)))
      (when (and env-root
		 (not (hub/direnv--environment-current-p env-root))
		 (not (equal env-root hub/direnv--pending-directory)))
	(when (timerp hub/direnv--pending-timer)
	  (cancel-timer hub/direnv--pending-timer))
	(setq hub/direnv--pending-directory env-root)
	(setq hub/direnv--pending-timer
	      (run-with-idle-timer hub/direnv-idle-delay nil
				   #'hub/direnv--apply-directory env-root)))))

  (add-hook 'find-file-hook #'hub/direnv-schedule-update)
  (add-hook 'dired-mode-hook #'hub/direnv-schedule-update)
  (add-hook 'eshell-mode-hook #'hub/direnv-schedule-update)
  (add-hook 'compilation-mode-hook #'hub/direnv-schedule-update)
  (add-hook 'comint-mode-hook #'hub/direnv-schedule-update)
  (add-hook 'post-command-hook #'hub/direnv-schedule-update))

(provide 'dev/direnv)
;;; direnv.el ends here
