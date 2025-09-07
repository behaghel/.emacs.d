;;; ci-load-all.el --- Force full config load in CI  -*- lexical-binding: t; -*-

;; CI-only script. Forces interactive layer loading and stubs mu4e if missing
;; so that email modules can be byte-compiled/loaded without external deps.

(setq hub/ci-forced-interactive t)
(setenv "HUB_FORCE_FULL_LOAD" "1")
(setq use-package-verbose t)
(setq use-package-minimum-reported-time 0.01)
(defvar hub/ci-start-time (current-time))
;; In PR CI runs, skip optional/network-heavy modules (AI, notes) to avoid clones
(setenv "HUB_CI_SKIP_OPTIONALS" "1")

;; Disable straight network ops and stub use-package in CI.
(setq straight-use-package-by-default nil)
(defun straight-use-package (&rest _args) t)
(unless (fboundp 'use-package)
  (defmacro use-package (name &rest _args)
    `(progn (provide ',name))))

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

;; Compute repo root and ensure core predicates are available early
(let* ((this-file (or load-file-name buffer-file-name))
       (scripts-dir (file-name-directory this-file))
       (repo-root (file-name-directory (directory-file-name scripts-dir))))
  (add-to-list 'load-path (expand-file-name "core" repo-root))
  (ignore-errors (require 'core-predicates))
  ;; Provide mu4e stubs before init.el brings in email modules
  (hub/ci--stub-mu4e)
  ;; Load full init from repo root (treating session as interactive)
  (load (expand-file-name "init.el" repo-root)))

;; Report present/missing layered features and total time
(let* ((expected '(ui/core ui/gui ui/tty
			   editing/evil completion/core navigation/treemacs
			   vcs/git navigation/dired shell/eshell
			   org/core notes/brain tools/blog tools/ai
			   apps/elfeed email/core email/contexts email/bookmarks email/view))
       (present (seq-filter (lambda (f) (featurep f)) expected))
       (missing (seq-remove (lambda (f) (featurep f)) expected))
       (elapsed (float-time (time-subtract (current-time) hub/ci-start-time))))
  (message "[ci-load-all] core-predicates loaded: %s" (featurep 'core-predicates))
  (when (featurep 'core-predicates)
    (message "[ci-load-all] hub/ci-forced-interactive=%s hub/interactive-p=%s"
	     hub/ci-forced-interactive (hub/interactive-p)))
  (message "[ci-load-all] Present: %s" present)
  (when missing (message "[ci-load-all] Missing: %s" missing))
  (message "[ci-load-all] Total load time: %.3fs" elapsed)
  (message "CI full-load completed"))
