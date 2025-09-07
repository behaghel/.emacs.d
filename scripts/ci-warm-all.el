;;; ci-warm-all.el --- Populate straight caches via full load  -*- lexical-binding: t; -*-

;; Runs a real full-load (forced interactive) to populate straight caches.
;; Stubs mu4e only, so email modules can load even if mu4e is not installed.

(setq use-package-verbose t)
(setq use-package-minimum-reported-time 0.01)
(setenv "HUB_FORCE_FULL_LOAD" "1")
(defvar hub/ci-start-time (current-time))

(defun hub/ci--stub-mu4e ()
  (unless (require 'mu4e nil 'noerror)
    (defvar mu4e-mu-version "ci-stub")
    (defvar mu4e-main-mode-map (make-sparse-keymap))
    (defvar mu4e-headers-mode-map (make-sparse-keymap))
    (defun mu4e-message (&rest _args) (ignore))
    (defun make-mu4e-context (&rest _args) nil)
    (defun mu4e-context-determine (&rest _args) nil)
    (defun mu4e-context-current () nil)
    (defun mu4e-message-field (&rest _args) nil)
    (defun mu4e-message-contact-field-matches (&rest _args) nil)
    (provide 'mu4e)))

(let* ((this-file (or load-file-name buffer-file-name))
       (scripts-dir (file-name-directory this-file))
       (repo-root (file-name-directory (directory-file-name scripts-dir))))
  (add-to-list 'load-path (expand-file-name "core" repo-root))
  (ignore-errors (require 'core-predicates))
  (hub/ci--stub-mu4e)
  (load (expand-file-name "init.el" repo-root)))

(let* ((expected '(ui/core ui/gui ui/tty
			   editing/evil completion/core navigation/treemacs
			   vcs/git navigation/dired shell/eshell
			   org/core notes/brain tools/blog tools/ai
			   apps/elfeed email/core email/contexts email/bookmarks email/view))
       (present (seq-filter (lambda (f) (featurep f)) expected))
       (missing (seq-remove (lambda (f) (featurep f)) expected))
       (elapsed (float-time (time-subtract (current-time) hub/ci-start-time))))
  (message "[ci-warm-all] Present: %s" present)
  (when missing (message "[ci-warm-all] Missing: %s" missing))
  (message "[ci-warm-all] Total load time: %.3fs" elapsed))
