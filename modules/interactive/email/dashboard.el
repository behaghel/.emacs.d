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
  ;; Ensure the dashboard file opens with mu4e-dashboard-mode when visited.
  (defun mu4e-dashboard-hook ()
    (when (and buffer-file-name
	       (string= (file-name-nondirectory buffer-file-name) "mail-sidebar.org"))
      (mu4e-dashboard-mode)
      (when (fboundp 'org-modern-mode)
	(org-modern-mode 0))))
  (add-hook 'find-file-hook 'mu4e-dashboard-hook))

;; Keep the mu4e dashboard sidebar visible across mail workflows
(defun hub/mu4e--in-mails-perspective-p ()
  "Return non-nil when the current perspective is 'mails'."
  (and (fboundp 'persp-current-name)
       (string= (persp-current-name) "mails")))

(defun hub/mu4e-ensure-dashboard-sidebar ()
  "Ensure the mu4e dashboard sidebar is visible as a left side window.
Only acts when in mu4e buffers or in the 'mails' perspective to avoid
starting mu4e unexpectedly. Keeps focus on the current window."
  (interactive)
  ;; Gate: act only if we're in a mu4e buffer, or composing (org-msg) within the mails perspective,
  ;; or we are already in the mails perspective (user explicitly switched there).
  (let* ((in-mu4e (or (derived-mode-p 'mu4e-main-mode 'mu4e-headers-mode 'mu4e-view-mode)
		      ;; accept being in the mu4e dashboard buffer
		      (and (derived-mode-p 'org-mode)
			   (equal (file-name-nondirectory (or buffer-file-name "")) "mail-sidebar.org"))))
	 (in-compose (bound-and-true-p org-msg-mode))
	 (in-mails-persp (hub/mu4e--in-mails-perspective-p)))
    (when (or in-mu4e (and in-compose in-mails-persp) in-mails-persp)
      (let ((cur (selected-window))
	    (sidebar-file (expand-file-name "modules/interactive/email/mail-sidebar.org" user-emacs-directory)))
	(cond
	 ;; Preferred: use the dedicated helper from navigation/perspective if available
	 ((and (fboundp 'mu4e-sidebar) in-mails-persp)
	  (mu4e-sidebar))
	 (t
	  (let* ((buf (or (get-file-buffer sidebar-file)
			  (and (file-exists-p sidebar-file)
			       (find-file-noselect sidebar-file))))
		 (params '((side . left)
			   (slot . -1)
			   (window-parameters . ((no-delete-other-windows . t)
						 (no-other-window . t))))))
	    (when (buffer-live-p buf)
	      (with-current-buffer buf
		(when (fboundp 'mu4e-dashboard-mode) (mu4e-dashboard-mode 1))
		(when (fboundp 'org-modern-mode) (org-modern-mode 0)))
	      (display-buffer-in-side-window buf params)))))
	(when (and (window-live-p cur) (not (eq cur (selected-window))))
	  (select-window cur))))))

;; Sidebar persistence hooks
;; (add-hook 'mu4e-main-mode-hook    #'hub/mu4e-ensure-dashboard-sidebar)
;; (add-hook 'mu4e-headers-mode-hook #'hub/mu4e-ensure-dashboard-sidebar)
;; (add-hook 'mu4e-view-mode-hook    #'hub/mu4e-ensure-dashboard-sidebar)
;; (with-eval-after-load 'org-msg
;;   (add-hook 'org-msg-mode-hook #'hub/mu4e-ensure-dashboard-sidebar))

(provide 'email/dashboard)
;;; dashboard.el ends here
