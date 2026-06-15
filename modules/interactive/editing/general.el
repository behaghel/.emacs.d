;;; general.el --- General editing defaults -*- lexical-binding: t; -*-

;;; Commentary:
;; Better defaults, UX helpers, and general-purpose tweaks for editing.

;;; Code:

(require 'hub-utils)

;; Stop cluttering FS with #file.ext# and centralize backups
(setq backup-directory-alist `((".*" . ,temporary-file-directory))
      auto-save-file-name-transforms `((".*" ,(concat user-emacs-directory "backups") t)))

(use-package emacs :delight (auto-fill-function))

;; Performance
(setq gc-cons-threshold 100000000
      read-process-output-max (* 1024 1024)
      native-comp-async-report-warnings-errors 'silent
      native-compile-prune-cache t)

;; macOS integration
(setq is-mac (equal system-type 'darwin)
      mac-command-key-is-meta t
      mac-command-modifier 'meta
      mac-option-key-is-meta nil
      mac-option-modifier nil
      ns-use-srgb-colorspace t)

(defcustom hub/explicit-exec-path-extra-directories
  (list (expand-file-name "~/.nix-profile/bin")
	(format "/etc/profiles/per-user/%s/bin" (user-login-name))
	"/nix/var/nix/profiles/default/bin"
	"/run/current-system/sw/bin"
	"/opt/homebrew/bin"
	"/usr/local/bin"
	"/usr/bin"
	"/bin"
	"/usr/sbin"
	"/sbin")
  "Explicit executable search path entries for Emacs.
These directories are merged with inherited PATH without invoking a shell.  GUI
launchd PATH should be managed declaratively from nixos-config."
  :type '(repeat directory)
  :group 'convenience)

(defun hub/configure-explicit-exec-path ()
  "Configure `exec-path' from explicit directories and inherited PATH.
This intentionally avoids `exec-path-from-shell' so GUI startup does not block on
interactive shell initialization."
  (let ((seen nil)
	(dirs nil))
    (dolist (dir (append hub/explicit-exec-path-extra-directories
			 (parse-colon-path (or (getenv "PATH") ""))))
      (when (and dir (file-directory-p dir) (not (member dir seen)))
	(push dir seen)
	(push dir dirs)))
    (setq dirs (nreverse dirs))
    (setenv "PATH" (mapconcat #'identity dirs path-separator))
    (setq exec-path (append dirs (list exec-directory)))))

(hub/configure-explicit-exec-path)

;; Clipboard integration
(use-package clipmon :defer t)

;; Auto-reload files changed on disk
(global-auto-revert-mode t)

(use-package undo-tree
  :diminish undo-tree-mode
  :bind ("C-é" . undo)
  :config (global-undo-tree-mode))

(use-package unfill :bind (("M-Q" . unfill-toggle)) :commands (unfill-region unfill-paragraph unfill-toggle))
(use-package expand-region :commands (er/expand-region))

(setq comment-auto-fill-only-comments t)
(add-to-list 'fill-nobreak-predicate 'fill-french-nobreak-p)
(setq tab-always-indent 'complete
      mode-require-final-newline nil
      shift-select-mode nil)

;; Save all when Emacs loses focus
(add-hook 'focus-out-hook 'hub/save-all)

;; TRAMP
(setq tramp-default-method "ssh")

;; General keybindings
(global-set-key (kbd "C-=") 'align-current)
(global-set-key (kbd "M-<DEL>") 'delete-indentation)
(define-key key-translation-map (kbd "<f8> <right>") (kbd "→"))
(define-key key-translation-map (kbd "<f8> i") (kbd "∞"))
(global-set-key "\M-;" 'hub/comment-dwim-line)

(provide 'editing/general)
;;; general.el ends here
