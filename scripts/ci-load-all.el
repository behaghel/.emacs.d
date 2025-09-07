;;; ci-load-all.el --- Force full config load in CI  -*- lexical-binding: t; -*-

;; CI-only script. Forces interactive layer loading and stubs mu4e if missing
;; so that email modules can be byte-compiled/loaded without external deps.

(setq hub/ci-forced-interactive t)
(setenv "HUB_FORCE_FULL_LOAD" "1")

(defun hub/ci--stub-mu4e ()
  (unless (require 'mu4e nil 'noerror)
    (defvar mu4e-main-mode-map (make-sparse-keymap))
    (defvar mu4e-headers-mode-map (make-sparse-keymap))
    (defun mu4e-message (&rest _args) (ignore))
    (defun make-mu4e-context (&rest _args) nil)
    (defun mu4e-context-determine (&rest _args) nil)
    (defun mu4e-context-current () nil)
    (defun mu4e-message-field (&rest _args) nil)
    (defun mu4e-message-contact-field-matches (&rest _args) nil)
    (provide 'mu4e)))

;; Provide mu4e stubs before init.el brings in email modules
(hub/ci--stub-mu4e)

;; Load full init, which will now consider session interactive
(let* ((this-file (or load-file-name buffer-file-name))
       (scripts-dir (file-name-directory this-file))
       (repo-root (file-name-directory (directory-file-name scripts-dir))))
  (load (expand-file-name "init.el" repo-root)))

(message "CI full-load completed")
