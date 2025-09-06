;;; predicates.el --- Environment predicates -*- lexical-binding: t; -*-

;;; Commentary:
;; Small helpers to express environment layers (batch/interactive, GUI/TTY, CI).

;;; Code:

(defun hub/batch-p ()
  "Return non-nil when running in batch (noninteractive)."
  noninteractive)

(defun hub/interactive-p ()
  "Return non-nil when running interactively (not batch)."
  (not (hub/batch-p)))

(defun hub/gui-p ()
  "Return non-nil when running with a graphical display."
  (and (hub/interactive-p) (display-graphic-p)))

(defun hub/tty-p ()
  "Return non-nil when running in an interactive TTY session."
  (and (hub/interactive-p) (not (hub/gui-p))))

(defun hub/ci-p ()
  "Return non-nil when running under CI."
  (getenv "GITHUB_ACTIONS"))

(provide 'core-predicates)
;;; predicates.el ends here
