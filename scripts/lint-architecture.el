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

;; Core must not require namespaced module features (contain "/").
(let* ((default-directory user-emacs-directory)
       (core-files (split-string (shell-command-to-string "git ls-files 'core/*.el'") "\n" t))
       (bad '()))
  (dolist (f core-files)
    (with-temp-buffer
      (insert-file-contents f)
      (goto-char (point-min))
      (while (re-search-forward "(require '([^)']*/[^)']+))" nil t)
	(push (format "%s:%d" f (line-number-at-pos)) bad))))
  (when bad
    (error "Core requires namespaced features: %S" (nreverse bad))))

;; Lint: any reference to hub/* functions should declare (require 'hub-utils)
(let* ((default-directory user-emacs-directory)
       (files (split-string (shell-command-to-string "git ls-files 'modules/**/*.el' 'lisp/*.el'" ) "\n" t))
       (exceptions '("lisp/hub-utils.el"))
       (violations '()))
  (dolist (f files)
    (with-temp-buffer
      (insert-file-contents f)
      (goto-char (point-min))
      (let ((uses-hub (re-search-forward "\\bhub/[A-Za-z0-9_-]+" nil t))
	    (has-require (save-excursion
			   (goto-char (point-min))
			   (re-search-forward "(require 'hub-utils)" nil t))))
	(when (and uses-hub (not has-require) (not (member f exceptions)))
	  (push f violations)))))
  (when violations
    (error "Modules use hub/* without (require 'hub-utils): %S" (nreverse violations))))

;; Ensure settings/ contains no tracked files (folder is deprecated)
(let* ((default-directory user-emacs-directory)
       (tracked (split-string (shell-command-to-string "git ls-files 'settings/**' 2>/dev/null") "\n" t)))
  (when tracked
    (error "settings/ should be empty; tracked files found: %S" tracked)))
