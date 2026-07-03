;;; emacs --- Hubert's .emacs file         -*- lexical-binding: t -*-
;; Copyright (C) 2013 Hubert Behaghel
;;
;;; Commentary:
;; Main orchestration entrypoint.  Feature and package configuration lives in
;; core modules and domain modules.
;;
;;; Code:

;; Ensure `user-emacs-directory' is the directory of this file when running in
;; environments such as GitHub actions where HOME might not point to the
;; repository root.
(setq user-emacs-directory
      (file-name-directory (or load-file-name buffer-file-name)))

(add-to-list 'load-path (expand-file-name "core" user-emacs-directory))
(require 'core-predicates)

;; CI optimizations for straight.el: avoid modification checks and use shallow clones.
(when (getenv "GITHUB_ACTIONS")
  ;; Older straight.el expects a list here; use nil to disable checks in CI.
  (setq straight-check-for-modifications nil)
  (setq straight-vc-git-default-clone-depth 1))

;; Ensure a writable temp directory exists within `user-emacs-directory'.
;; Some async/native compilation paths try to place temp files under
;; `~/.emacs.d/tmp/` — create it proactively to avoid CI failures.
(let ((tmpdir (expand-file-name "tmp" user-emacs-directory)))
  (unless (file-directory-p tmpdir)
    (ignore-errors (make-directory tmpdir t))))

(defvar native-comp-deferred-compilation-deny-list nil)
;; Keep Customize output out of init.el.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

(setq hub-lisp-dir (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path hub-lisp-dir)
(add-to-list 'load-path (expand-file-name "packages/org-comments" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "packages/org-confluence" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "packages/org-marginalia" user-emacs-directory))

;; Legacy settings/ folder is being retired; do not add it to load-path.
;; Private machine-specific setup now lives under private/setup.el (gitignored).
;; Keep legacy modules/ on load-path for now (writing, etc.). Also add
;; interactive layer path only for interactive/full-load sessions so ordinary
;; batch checks do not see interactive-only modules.
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "modules/lang" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "modules/org" user-emacs-directory))
(when (hub/interactive-p)
  (add-to-list 'load-path (expand-file-name "modules/interactive" user-emacs-directory)))

(defvar hub/private-setup-loaded nil
  "Non-nil once private setup has been loaded.")

(defun hub/load-private-setup ()
  "Load machine-specific setup once, preferring `private/setup.el'."
  (unless hub/private-setup-loaded
    (let ((private-new (expand-file-name "private/setup.el" user-emacs-directory))
	  (private-legacy (expand-file-name "settings/setup-private.el" user-emacs-directory)))
      (cond
       ((file-exists-p private-new) (load private-new t) (setq hub/private-setup-loaded t))
       ((file-exists-p private-legacy) (load private-legacy t) (setq hub/private-setup-loaded t))))))

(setq user-mail-address "behaghel@gmail.com")
(setq user-full-name "Hubert Behaghel")

;; Use centralized package management from core/ with safe fallback.
(defvar hub/core-packages-load-ok nil)
(condition-case err
    (progn
      (require 'core-packages)
      (setq hub/core-packages-load-ok t))
  (error
   (message "[init] core-packages failed, falling back: %S" err)
   ;; Keep fallback bootstrap aligned with core/core-packages.el. Respect any
   ;; earlier CI override that deliberately disabled modification checks.
   (unless (boundp 'straight-check-for-modifications)
     (setq straight-check-for-modifications '(find-when-checking only-once)))
   (defvar bootstrap-version)
   (let* ((repo "radian-software/straight.el")
	  (branch "develop")
	  (install-url (format "https://raw.githubusercontent.com/%s/%s/install.el" repo branch))
	  (bootstrap-file (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory)))
     (unless (file-exists-p bootstrap-file)
       (with-current-buffer
	   (url-retrieve-synchronously install-url 'silent 'inhibit-cookies)
	 (goto-char (point-max))
	 (eval-print-last-sexp)))
     (load bootstrap-file nil 'nomessage))
   (setq straight-use-package-by-default t)
   (setq straight-vc-git-default-protocol (hub/preferred-straight-protocol))
   (straight-use-package 'use-package)
   (straight-use-package 'diminish)
   (require 'use-package)
   (require 'use-package-ensure)
   (require 'use-package-delight)
   (require 'use-package-diminish)
   (setq hub/core-packages-load-ok nil)))

;; In CI, if a pinned versions file exists, thaw to it early to avoid rebuilds.
(when (getenv "GITHUB_ACTIONS")
  (let ((versions-file (expand-file-name "straight/versions/default.el" user-emacs-directory)))
    (when (file-exists-p versions-file)
      (ignore-errors (straight-thaw-versions)))))

(setq use-package-verbose t
      use-package-always-defer nil
      use-package-always-ensure nil)

;; Load private overrides after package bootstrap so private integrations cannot
;; accidentally load built-in packages before straight-managed versions are on
;; `load-path', but before domain modules consume private values.
(hub/load-private-setup)

(require 'core-paths)
(require 'core-auth)
(require 'hub-utils)

;; Keep variable state under var/: prefer explicit files for common state.
(setq
 recentf-save-file (expand-file-name "var/recentf-save.el" user-emacs-directory)
 savehist-file     (expand-file-name "var/savehist.el" user-emacs-directory)
 save-place-file   (expand-file-name "var/save-place.el" user-emacs-directory)
 tramp-persistency-file-name (expand-file-name "var/tramp" user-emacs-directory))

;; Provide streamlined access to writing helpers without changing UX.
(autoload 'writing/enable-basics "modules/writing/writing" nil t)

;; Load language configuration (autoloads, treesit sources/remaps, language servers).
(ignore-errors (require 'lang/treesit-config))
(ignore-errors (require 'lang/scala))
(ignore-errors (require 'lang/nix))
(ignore-errors (require 'lang/web))
(ignore-errors (require 'lang/json-config))
(ignore-errors (require 'lang/yaml))
(ignore-errors (require 'lang/misc))

(when (hub/interactive-p)
  (require 'editing/core)
  (require 'core-server)
  (hub/ensure-server-started)
  (when (eq system-type 'darwin)
    (require 'os/darwin))
  (require 'completion/core)
  (require 'navigation/core)
  (require 'writing/core)
  (require 'dev/core)
  (require 'ui/core)
  (if (display-graphic-p)
      (progn
	(require 'ui/gui)
	(when (fboundp 'hub/dashboard-first-paint)
	  (hub/dashboard-first-paint)))
    (require 'ui/tty))
  (require 'vcs/core)
  (require 'shell/core)
  ;; Knowledge & writing.
  (require 'org/core)
  (require 'org/confluence)
  (require 'org/google-docs)
  (require 'org/export-latex)
  (unless (getenv "HUB_CI_SKIP_OPTIONALS")
    (require 'notes/brain))
  (require 'tools/blog)
  (require 'tools/pdf)
  (require 'tools/nomina)
  (unless (getenv "HUB_CI_SKIP_OPTIONALS")
    (require 'tools/ai))
  (require 'apps/core)
  (require 'video/eve)
  (require 'email/core))

(put 'narrow-to-region 'disabled nil)
(put 'erase-buffer 'disabled nil)

(provide 'init)
;;; init.el ends here
