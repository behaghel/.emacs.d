;;; predicates.el --- Environment predicates -*- lexical-binding: t; -*-

;;; Commentary:
;; Small helpers to express environment layers (batch/interactive, GUI/TTY, CI).

;;; Code:

(defun hub/batch-p ()
  "Return non-nil when running in batch (noninteractive)."
  noninteractive)

(defvar hub/ci-forced-interactive (getenv "HUB_FORCE_FULL_LOAD")
  "When non-nil, treat batch sessions as interactive for CI full-load checks.")

(defun hub/interactive-p ()
  "Return non-nil when running interactively (not batch).
In CI full-load mode, this can be forced via HUB_FORCE_FULL_LOAD."
  (or (not (hub/batch-p)) hub/ci-forced-interactive))

(defun hub/gui-p ()
  "Return non-nil when running with a graphical display."
  (and (hub/interactive-p) (display-graphic-p)))

(defun hub/tty-p ()
  "Return non-nil when running in an interactive TTY session."
  (and (hub/interactive-p) (not (hub/gui-p))))

(defun hub/ci-p ()
  "Return non-nil when running under CI."
  (getenv "GITHUB_ACTIONS"))

(defun hub/preferred-straight-protocol ()
  "Return the preferred straight.el Git protocol for this environment.

Prefers HTTPS unless we are confident SSH is available. This makes
GUI startup on macOS (where launchd may not propagate SSH agent envs)
more reliable, while still using SSH when explicitly requested or
when an agent socket is present.

Rules:
- On CI: always HTTPS.
- If env var HUB_USE_SSH_FOR_GIT is set/non-empty: SSH.
- If SSH_AUTH_SOCK is set and the socket exists: SSH.
- Otherwise: HTTPS."
  (cond
   ((hub/ci-p) 'https)
   ;; Explicit user override
   ((getenv "HUB_USE_SSH_FOR_GIT") 'ssh)
   ;; Heuristic: a usable agent socket generally implies SSH works
   ((let ((sock (getenv "SSH_AUTH_SOCK")))
      (and sock (file-exists-p sock))) 'ssh)
   (t 'https)))

(provide 'core-predicates)
;;; predicates.el ends here
