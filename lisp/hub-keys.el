;;; hub-keys.el --- Leaders, DWIM, shared key helpers -*- lexical-binding: t; -*-

;;; Commentary:
;; Centralized keybinding helpers:
;; - Leader and localleader definers (general.el)
;; - Shared DWIM helpers (project-aware defaults)
;; - Entry points for hydras/which-key labels

;;; Code:

(require 'hub-utils)
(require 'project)

;; Definers (require general at call sites)
(defvar hub/leader-prefix ","
  "Global leader prefix for Evil normal/visual states.")
(defvar hub/localleader-prefix ";"
  "Local leader prefix for Evil normal/visual states.")
(defvar hub/global-override-states '(normal visual motion replace operator)
  "States covered by `hub/global-override'.")

(defun hub/define-leaders ()
  "Define `hub/leader', `hub/localleader', and `hub/global-override'."
  (when (require 'general nil 'noerror)
    (general-create-definer hub/leader :states '(normal visual) :prefix hub/leader-prefix)
    (general-create-definer hub/localleader :states '(normal visual) :prefix hub/localleader-prefix)
    (general-create-definer hub/global-override :states hub/global-override-states :keymaps 'override)))

(defmacro hub/leader-bind (&rest args)
  "Bind keys under the leader prefix, deferred until general loads.
Keyword options before key-command pairs:
  :keymap MAP  (default: global)  :states S  (default: \\='(normal))
  :label STR   which-key label for the first binding
  :file FILE   autoload for the first command"
  (declare (indent defun))
  (let* ((keymap ''global) (states ''(normal)) (file nil) (label nil)
	 (bindings '()) (rest args))
    (while (keywordp (car-safe rest))
      (pcase (pop rest)
	(:keymap (setq keymap (pop rest)))
	(:states (setq states (pop rest)))
	(:label (setq label (pop rest)))
	(:file (setq file (pop rest)))
	(_ (pop rest))))
    (while rest
      (let ((key (pop rest)) (cmd (pop rest)))
	(push (list key (if (eq (car-safe cmd) 'function) (cadr cmd) cmd))
	      bindings)))
    (setq bindings (nreverse bindings))
    (let ((fc (cadr (car bindings))) (fk (car (car bindings))))
      `(eval-after-load 'general
	 '(progn
	    (hub/define-leaders)
	    ,@(when (and file fc) `((autoload ',fc ,file nil t)))
	    (hub/leader :keymaps ,keymap :states ,states
			,@(mapcan
			   (lambda (b) (list (car b) `',(cadr b)))
			   bindings))
	    ,@(when (and label fk)
		`((with-eval-after-load 'which-key
		    (which-key-add-key-based-replacements
		     ,(concat hub/leader-prefix fk) ,label)))))))))

;; DWIM helpers
(defun hub/in-project-p ()
  "Return non-nil if current buffer is part of a project."
  (ignore-errors (project-current nil)))

(defun hub/dwim-find-file ()
  "Find file with project awareness.
If in a project, use `project-find-file', otherwise fallback to `find-file'."
  (interactive)
  (if (hub/in-project-p)
      (call-interactively #'project-find-file)
    (call-interactively #'find-file)))

(defun hub/dwim-switch-buffer ()
  "Switch buffer with project awareness. Prefer project buffers."
  (interactive)
  (cond
   ((and (hub/in-project-p) (fboundp 'consult-project-buffer))
    (call-interactively #'consult-project-buffer))
   ((fboundp 'consult-buffer)
    (call-interactively #'consult-buffer))
   (t (call-interactively #'switch-to-buffer))))

(defun hub/dwim-search ()
  "Search DWIM: project ripgrep if in project, else in-buffer line search."
  (interactive)
  (cond
   ((and (hub/in-project-p) (fboundp 'consult-ripgrep))
    (call-interactively #'consult-ripgrep))
   ((fboundp 'consult-line)
    (call-interactively #'consult-line))
   (t (call-interactively #'isearch-forward))))

(defun hub/dwim-format-buffer ()
  "Format buffer DWIM: Eglot if managed, else Apheleia, else indent."
  (interactive)
  (cond
   ((and (fboundp 'eglot-format) (fboundp 'eglot-managed-p) (eglot-managed-p))
    (call-interactively #'eglot-format))
   ((fboundp 'apheleia-format-buffer)
    (call-interactively #'apheleia-format-buffer))
   (t (indent-region (point-min) (point-max)))))

(defun hub/dwim-test-current ()
  "Run current test DWIM based on major mode/packages."
  (interactive)
  (cond
   ((fboundp 'rspec-verify) (call-interactively #'rspec-verify))
   ((fboundp 'minitest-verify) (call-interactively #'minitest-verify))
   ((fboundp 'project-compile) (call-interactively #'project-compile))
   ((fboundp 'recompile) (call-interactively #'recompile))
   (t (message "No DWIM test runner for this mode"))))

(defun hub/dwim-repl ()
  "Start or attach to a REPL suitable for the current mode."
  (interactive)
  (cond
   ((and (derived-mode-p 'ruby-mode) (fboundp 'inf-ruby)) (call-interactively #'inf-ruby))
   ((and (derived-mode-p 'python-mode) (fboundp 'run-python)) (call-interactively #'run-python))
   ((and (derived-mode-p 'scala-mode 'scala-ts-mode) (fboundp 'sbt-start)) (call-interactively #'sbt-start))
   ((derived-mode-p 'emacs-lisp-mode) (call-interactively #'ielm))
   (t (message "No DWIM REPL for this mode"))))

(defun hub/dwim-vcs-grep ()
  "Search in VCS if available (git-grep UI), otherwise ripgrep."
  (interactive)
  (let* ((git-root (locate-dominating-file default-directory ".git"))
	 (proj (ignore-errors (project-current nil)))
	 (ripgrep-root (and proj (car (project-roots proj))))
	 (initial (thing-at-point 'symbol t)))
    (cond
     ((and git-root (fboundp 'consult-git-grep))
      (consult-git-grep git-root initial))
     ((fboundp 'consult-ripgrep)
      (consult-ripgrep (or ripgrep-root default-directory) initial))
     ((fboundp 'rgrep) (call-interactively #'rgrep))
     (t (message "No grep available")))))

(provide 'hub-keys)
;;; hub-keys.el ends here
