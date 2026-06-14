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

Prefers HTTPS by default.  A present SSH agent socket is not sufficient on
macOS/YubiKey setups, where batch or GUI Emacs may see an agent but still be
unable to authorize signing prompts during package clones.

Rules:
- If env var HUB_USE_SSH_FOR_GIT is set/non-empty: SSH.
- Otherwise: HTTPS."
  (if (getenv "HUB_USE_SSH_FOR_GIT")
      'ssh
    'https))

(provide 'core-predicates)
;;; predicates.el ends here
