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

(defun hub/mu--version-from-bin (mu-bin)
  "Return MU binary version string for MU-BIN, or nil when unavailable."
  (when mu-bin
    (let* ((out (shell-command-to-string (format "\"%s\" --version" mu-bin)))
	   (parts (split-string out "[ \n\r\t]+" t)))
      (cl-find-if (lambda (part)
		    (string-match-p "\\`[0-9]+\\.[0-9]+\\.[0-9]+\\'" part))
		  parts))))

(defun hub/mu4e--nix-candidates (mu-version)
  "Return candidate mu4e load-paths for Nix installs matching MU-VERSION."
  (let* ((version (and (stringp mu-version)
		       (string-match "\\`[0-9]+\\.[0-9]+\\.[0-9]+\\'" mu-version)
		       mu-version))
	 (profiles (delete-dups
		    (append (split-string (or (getenv "NIX_PROFILES") "") " " t)
			    (list (expand-file-name "~/.nix-profile"))))))
    (append
     (when version
       (file-expand-wildcards
	(format "/nix/store/*mu4e-%s*/share/emacs/site-lisp/elpa/mu4e-%s" version version)))
     (cl-loop for profile in profiles append
	      (file-expand-wildcards
	       (expand-file-name
		(if version
		    (format "share/emacs/site-lisp/elpa/mu4e-%s" version)
		  "share/emacs/site-lisp/elpa/mu4e-*")
		profile))))))

(defun hub/add-mu4e-load-path (&optional mu-bin mu-version)
  "Attempt to add mu4e elisp to `load-path` based on MU-BIN and MU-VERSION.
Returns the resolved mu binary path (or nil)."
  (let* ((mu-bin (or mu-bin (executable-find "mu")))
	 (mu-version (or mu-version (hub/mu--version-from-bin mu-bin)))
	 (prefix (when mu-bin (expand-file-name ".." (file-name-directory mu-bin))))
	 (candidates (append (hub/mu4e--nix-candidates mu-version)
			     (list (when prefix (expand-file-name "share/emacs/site-lisp/mu/mu4e" prefix))
				   (when prefix (expand-file-name "share/emacs/site-lisp/mu4e" prefix))
				   "/usr/local/share/emacs/site-lisp/mu/mu4e"
				   "/usr/local/share/emacs/site-lisp/mu4e"
				   "/usr/share/emacs/site-lisp/mu/mu4e"
				   "/usr/share/emacs/site-lisp/mu4e"))))
    (cl-loop for dir in candidates do
	     (when (and dir (file-directory-p dir))
	       (add-to-list 'load-path dir)
	       (cl-return)))
    mu-bin))

(defun hub/mu4e--lib-matches-version (lib mu-version)
  "Return non-nil when mu4e LIB path matches MU-VERSION."
  (and lib mu-version
       (string-match-p (format "mu4e-%s" (regexp-quote mu-version)) lib)))

(defun hub/ensure-mu4e-loaded ()
  "Load mu4e from environment if available; fallback to probing common paths."
  (unless (featurep 'mu4e)
    (let* ((mu-bin (executable-find "mu"))
	   (mu-version (hub/mu--version-from-bin mu-bin))
	   (lib (locate-library "mu4e")))
      (unless (and lib (hub/mu4e--lib-matches-version lib mu-version))
	(hub/add-mu4e-load-path mu-bin mu-version)
	(setq lib (locate-library "mu4e")))
      (when lib (require 'mu4e)))))

;; Configure mu4e only after it is present (provided by environment or fallback)
(hub/ensure-mu4e-loaded)

(with-eval-after-load 'mu4e
  ;; Do not override externally provided mu4e-mu-binary if already set
  (when (and (boundp 'mu4e-mu-binary) (string-empty-p (or mu4e-mu-binary "")))
    (setq mu4e-mu-binary (or (executable-find "mu") "mu")))
  ;; evil-collection-mu4e may not have initialized if mu4e was absent from
  ;; `load-path' at `evil-collection-init' time (common when the Nix-provided
  ;; mu4e path differs between devenv shell and GUI Emacs).  Initialize it now
  ;; so mu4e modes enter Evil normal state with bépo rotation.
  (when (and (featurep 'evil-collection)
	     (not (featurep 'evil-collection-mu4e)))
    (evil-collection-init '(mu4e)))
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
(require 'email/actions)
(require 'email/view)
(require 'email/noise)
(require 'email/bookmarks)
(require 'email/compose)
(require 'email/dashboard)

(provide 'email/core)
;;; core.el ends here
