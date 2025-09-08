;;; test-helpers.el --- ERT helpers for project/perspective tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Small helpers to create temp projects and process timers during tests.

;;; Code:

(require 'ert)
(require 'project)

(defmacro hub/with-temp-project (body-file &rest body)
  "Create a temporary project with a git root and visit BODY-FILE, then run BODY.
BODY-FILE is a path relative to the project root."
  (declare (indent 1))
  `(let* ((root (make-temp-file "hub-proj-" t))
	  (default-directory root)
	  (file (expand-file-name ,body-file root)))
     (make-directory (file-name-directory file) t)
     (make-directory (expand-file-name ".git" root) t)
     (with-current-buffer (find-file-noselect file)
       (switch-to-buffer (current-buffer))
       ;; Touch file content so it exists on disk
       (insert "test")
       (save-buffer)
       ,@body)))

(defun hub/test-run-pending-timers (&optional seconds)
  "Run pending timers by waiting for SECONDS (default 0.05)."
  (let ((sec (or seconds 0.05)))
    (sleep-for sec)
    (while (input-pending-p) (discard-input))))

(provide 'test-helpers)
;;; test-helpers.el ends here
