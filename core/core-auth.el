;;; core-auth.el --- Secret and authentication integration -*- lexical-binding: t; -*-

;;; Commentary:
;; Configure shared authentication primitives such as pass/auth-source and
;; pinentry.  Machine-specific secrets remain in private/setup.el.

;;; Code:

(require 'auth-source-pass)
(require 'epg)

(auth-source-pass-enable)

(use-package pinentry)
;; On macOS, let native pinentry handle card insertion and PIN requests rather
;; than holding the shared agent behind an Emacs minibuffer request.  Preserve
;; loopback on Linux and other platforms, including terminal-only workflows.
(setq epg-pinentry-mode (if (eq system-type 'darwin) 'ask 'loopback))

(provide 'core-auth)
;;; core-auth.el ends here
