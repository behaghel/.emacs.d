;;; core-auth.el --- Secret and authentication integration -*- lexical-binding: t; -*-

;;; Commentary:
;; Configure shared authentication primitives such as pass/auth-source and
;; pinentry.  Machine-specific secrets remain in private/setup.el.

;;; Code:

(require 'auth-source-pass)

(auth-source-pass-enable)

(use-package pinentry)
(setq epa-pinentry-mode 'loopback)

(provide 'core-auth)
;;; core-auth.el ends here
