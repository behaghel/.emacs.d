;;; core-server.el --- Emacs server startup helpers -*- lexical-binding: t; -*-

;;; Commentary:
;; Explicit helpers for starting the Emacs server from the runtime
;; orchestration layer.

;;; Code:

(defvar hub/skip-server (getenv "HUB_SKIP_SERVER")
  "When non-nil, skip starting the Emacs server.")

(defun hub/ensure-server-started ()
  "Start the Emacs server unless it is already running or skipped."
  (require 'server)
  (unless (or hub/skip-server (server-running-p))
    (server-start)))

(provide 'core-server)
;;; core-server.el ends here
