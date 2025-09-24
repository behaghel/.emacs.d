;;; core.el --- Email: mu4e core -*- lexical-binding: t; -*-

;;; Commentary:
;; Minimal mu4e setup; contexts and extras are required from here.

;;; Code:

(require 'hub-utils)

(setq mail-user-agent 'mu4e-user-agent)

(require 'cl-lib)

(use-package adaptive-wrap :commands adaptive-wrap-prefix-mode)

(defun hub/email-enable-visual-wrap ()
  "Enable non-destructive visual wrapping suitable for email buffers."
  (setq-local truncate-lines nil
	      word-wrap t)
  (visual-line-mode 1)
  (when (fboundp 'adaptive-wrap-prefix-mode)
    (setq-local adaptive-wrap-extra-indent 0)
    (adaptive-wrap-prefix-mode 1)))

;; Respect mu4e/mu provided by external environment (e.g., Nix home-manager extraConfig)
;; Prefer an existing mu4e on `load-path`; fall back to probing standard locations.

(defun hub/add-mu4e-load-path ()
  "Attempt to add mu4e elisp to `load-path` based on detected mu binary.
Returns the resolved mu binary path (or nil)."
  (let* ((mu-bin (executable-find "mu"))
	 (prefix (when mu-bin (expand-file-name ".." (file-name-directory mu-bin))))
	 (candidates (list (when prefix (expand-file-name "share/emacs/site-lisp/mu/mu4e" prefix))
			   (when prefix (expand-file-name "share/emacs/site-lisp/mu4e" prefix))
			   "/usr/local/share/emacs/site-lisp/mu/mu4e"
			   "/usr/local/share/emacs/site-lisp/mu4e"
			   "/usr/share/emacs/site-lisp/mu/mu4e"
			   "/usr/share/emacs/site-lisp/mu4e")))
    (cl-loop for dir in candidates do
	     (when (and dir (file-directory-p dir))
	       (add-to-list 'load-path dir)
	       (cl-return)))
    mu-bin))

(defun hub/ensure-mu4e-loaded ()
  "Load mu4e from environment if available; fallback to probing common paths."
  (unless (featurep 'mu4e)
    (let ((lib (locate-library "mu4e")))
      (unless lib
	(hub/add-mu4e-load-path)
	(setq lib (locate-library "mu4e")))
      (when lib (require 'mu4e)))))

;; Configure mu4e only after it is present (provided by environment or fallback)
(hub/ensure-mu4e-loaded)

(with-eval-after-load 'mu4e
  ;; Do not override externally provided mu4e-mu-binary if already set
  (when (and (boundp 'mu4e-mu-binary) (string-empty-p (or mu4e-mu-binary "")))
    (setq mu4e-mu-binary (or (executable-find "mu") "mu")))
  (evil-collection-define-key 'normal 'mu4e-main-mode-map
			      "ê" 'mu4e-headers-search)
  (setq mu4e-context-policy 'pick-first
	mu4e-compose-context-policy 'ask-if-none
	mu4e-update-interval 200
	mu4e-index-update-in-background t
	mu4e-index-lazy-check t))

;; Tell straight.el not to try to install/compile mu4e — it is provided by the environment.
(when (boundp 'straight-built-in-pseudo-packages)
  (add-to-list 'straight-built-in-pseudo-packages 'mu4e))

;; Pull in modularized pieces
(require 'email/contexts)
;; Optional: uncomment as modules are migrated
(ignore-errors (require 'email/noise))
(ignore-errors (require 'email/bookmarks))
(ignore-errors (require 'email/view))
(ignore-errors (require 'email/compose))
(ignore-errors (require 'email/dashboard))
(ignore-errors (require 'email/actions))

(provide 'email/core)
;;; core.el ends here
