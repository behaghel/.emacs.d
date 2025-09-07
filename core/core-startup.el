;;; startup.el --- Core startup helpers -*- lexical-binding: t; -*-

;;; Commentary:
;; Helpers to manage GC and file-name-handlers during intensive operations.

;;; Code:

(defvar core/startup--saved-gc-cons-threshold gc-cons-threshold)
(defvar core/startup--saved-gc-cons-percentage gc-cons-percentage)
(defvar core/startup--saved-file-name-handler-alist file-name-handler-alist)

(defun core/startup-maximize-gc-and-io ()
  "Loosen GC and disable file-name handlers for speed."
  (setq gc-cons-threshold most-positive-fixnum
	gc-cons-percentage 0.6
	file-name-handler-alist nil))

(defun core/startup-restore-gc-and-io ()
  "Restore GC and file-name handlers after init or heavy work."
  (setq gc-cons-threshold core/startup--saved-gc-cons-threshold
	gc-cons-percentage core/startup--saved-gc-cons-percentage
	file-name-handler-alist core/startup--saved-file-name-handler-alist))

(provide 'core-startup)
;;; startup.el ends here
