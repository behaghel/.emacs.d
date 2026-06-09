;;; performance.el --- UI startup performance instrumentation -*- lexical-binding: t; -*-

;;; Commentary:
;; Lightweight helpers for measuring interactive startup milestones.

;;; Code:

(defgroup hub/performance nil
  "Startup and responsiveness measurement helpers."
  :group 'convenience)

(defcustom hub/performance-log-startup t
  "When non-nil, log GUI startup milestones to `*Messages*'."
  :type 'boolean
  :group 'hub/performance)

(defun hub/performance--startup-elapsed ()
  "Return elapsed seconds since Emacs startup began."
  (float-time (time-subtract (current-time) before-init-time)))

(defun hub/performance-log-startup-event (label &optional start-time)
  "Log startup milestone LABEL.
When START-TIME is non-nil, include duration since START-TIME."
  (when hub/performance-log-startup
    (message "[perf] +%.3fs %s%s"
	     (hub/performance--startup-elapsed)
	     label
	     (if start-time
		 (format " (%.3fs)"
			 (float-time (time-subtract (current-time) start-time)))
	       ""))))

(add-hook 'emacs-startup-hook
	  (lambda ()
	    (hub/performance-log-startup-event "emacs-startup-hook")))

(provide 'ui/performance)
;;; performance.el ends here
