;;; lint-architecture.el --- CI checks for layered architecture  -*- lexical-binding: t; -*-

(defun hub--assert (pred msg &rest args)
  (unless pred (apply #'error msg args)))

(let* ((interactive-dir (expand-file-name "modules/interactive" user-emacs-directory))
       (lp load-path)
       (on-lp (seq-some (lambda (p) (file-equal-p p interactive-dir)) lp)))
  ;; Batch sessions must not expose interactive modules on load-path
  (when noninteractive
    (hub--assert (not on-lp)
		 "modules/interactive is on load-path in batch: %S" load-path)
    ;; Interactive-only features must not be preloaded in batch
    (dolist (feat '(editing/evil navigation/treemacs completion/core))
      (hub--assert (not (featurep feat))
		   "Feature %S should not be loaded in batch" feat))))

;; Exit cleanly
(message "Architecture lint passed")

;; (Removed) Legacy setup-* require lint: no longer enforced in CI

;; Ensure settings/ contains no tracked files (folder is deprecated)
(let* ((default-directory user-emacs-directory)
       (tracked (split-string (shell-command-to-string "git ls-files 'settings/**' 2>/dev/null") "\n" t)))
  (when tracked
    (error "settings/ should be empty; tracked files found: %S" tracked)))
