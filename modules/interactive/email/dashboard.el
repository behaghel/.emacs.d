;;; dashboard.el --- Email: mu4e-dashboard hook -*- lexical-binding: t; -*-

;;; Commentary:
;; Hook up mu4e-dashboard for the mail sidebar.

;;; Code:

(use-package mu4e-dashboard
  :if (locate-library "mu4e-headers")
  :after mu4e
  :straight (mu4e-dashboard :type git :host github :repo "rougier/mu4e-dashboard")
  :custom (mu4e-dashboard-file (expand-file-name "modules/interactive/email/mail-sidebar.org" user-emacs-directory))
  :config
  (defun mu4e-dashboard-hook ()
    (when (and buffer-file-name
	       (string= (file-name-nondirectory buffer-file-name) "mail-sidebar.org"))
      (mu4e-dashboard-mode)
      (when (fboundp 'org-modern-mode)
	(org-modern-mode 0))))
  (add-hook 'find-file-hook 'mu4e-dashboard-hook))

(provide 'email/dashboard)
;;; dashboard.el ends here
