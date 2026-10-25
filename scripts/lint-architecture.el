;;; lint-architecture.el --- CI checks for layered architecture  -*- lexical-binding: t; -*-

;;; Commentary:
;; Check runtime boundaries and explicit shared-helper dependencies without
;; evaluating the configuration sources inspected by the static checks.

;;; Code:

(require 'seq)

(defun hub/lint--read-forms (file)
  "Read all forms in FILE, signalling malformed source without evaluating it."
  (with-temp-buffer
    (insert-file-contents file)
    (set-syntax-table emacs-lisp-mode-syntax-table)
    (let (forms)
      (while (progn (forward-comment (point-max)) (not (eobp)))
	(push (read (current-buffer)) forms))
      (nreverse forms))))

(defun hub/lint--tree-any-p (predicate tree &optional skip-quoted)
  "Return non-nil when PREDICATE matches any node in TREE.
When SKIP-QUOTED is non-nil, do not descend into quoted data."
  (or (funcall predicate tree)
      (cond
       ((consp tree)
	(unless (and skip-quoted (eq (car tree) 'quote))
	  (or (hub/lint--tree-any-p predicate (car tree) skip-quoted)
	      (hub/lint--tree-any-p predicate (cdr tree) skip-quoted))))
       ((vectorp tree)
	(seq-some (lambda (item)
		    (hub/lint--tree-any-p predicate item skip-quoted))
		  tree)))))

(defun hub/lint--shared-helpers ()
  "Return the namespaced helpers defined by the authoritative utility library."
  (let (helpers)
    (dolist (form (hub/lint--read-forms
		   (expand-file-name "lisp/hub-utils.el" user-emacs-directory)))
      (when (memq (car-safe form)
		  '(defun defmacro defsubst cl-defun cl-defmacro
			  defvar defconst defcustom defalias))
	(let ((name (nth 1 form)))
	  (when (eq (car-safe name) 'quote)
	    (setq name (cadr name)))
	  (when (and (symbolp name)
		     (string-prefix-p "hub/" (symbol-name name)))
	    (push name helpers)))))
    helpers))

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

;; Only helpers actually supplied by hub-utils imply that dependency.
(let* ((default-directory user-emacs-directory)
       (files (process-lines "git" "ls-files" "modules/**/*.el" "lisp/*.el"))
       (helpers (hub/lint--shared-helpers))
       violations)
  (dolist (file files)
    (unless (equal file "lisp/hub-utils.el")
      (let* ((forms (hub/lint--read-forms file))
	     (uses-helper
	      (hub/lint--tree-any-p (lambda (node) (memq node helpers)) forms))
	     (has-require
	      (hub/lint--tree-any-p
	       (lambda (node) (equal node '(require 'hub-utils))) forms t)))
	(when (and uses-helper (not has-require))
	  (push file violations)))))
  (when violations
    (error "Shared helpers used without (require 'hub-utils): %S"
	   (nreverse violations))))

;; Ensure settings/ contains no tracked files (folder is deprecated)
(let* ((default-directory user-emacs-directory)
       (tracked (split-string (shell-command-to-string "git ls-files 'settings/**' 2>/dev/null") "\n" t)))
  (when tracked
    (error "settings/ should be empty; tracked files found: %S" tracked)))

;; Domain-owned tests should live under test/<domain>/..., not flat test/.
(let* ((default-directory user-emacs-directory)
       (flat-tests (split-string (shell-command-to-string "git ls-files ':(glob)test/*-test.el' 2>/dev/null") "\n" t)))
  (when flat-tests
    (error "Domain tests should live under test/<domain>/; flat tests found: %S" flat-tests)))

(message "Architecture lint passed")
;;; lint-architecture.el ends here
