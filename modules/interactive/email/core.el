;;; core.el --- Email: mu4e core -*- lexical-binding: t; -*-

;;; Commentary:
;; Minimal mu4e setup; contexts and extras are required from here.

;;; Code:

(require 'hub-utils)

(setq mail-user-agent 'mu4e-user-agent)

(require 'cl-lib)

(defun hub/add-mu4e-load-path ()
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

(defvar hub/mu-binary (hub/add-mu4e-load-path))

(use-package mu4e
  :straight (:type built-in)
  :init (unless hub/mu-binary (message "mu binary not found; mu4e may not be available"))
  :custom (mu4e-mu-binary (or hub/mu-binary "mu"))
  :config
  (evil-collection-define-key 'normal 'mu4e-main-mode-map
			      "ê" 'mu4e-headers-search)
  (setq mu4e-context-policy 'pick-first mu4e-compose-context-policy 'ask-if-none))

;; Pull in modularized pieces
(require 'email/contexts)
;; Optional: uncomment as modules are migrated
(ignore-errors (require 'email/bookmarks))
(ignore-errors (require 'email/view))
(ignore-errors (require 'email/compose))
(ignore-errors (require 'email/dashboard))

(provide 'email/core)
;;; core.el ends here
