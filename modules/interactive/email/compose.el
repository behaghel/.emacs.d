;;; compose.el --- Email: compose + org-msg + sending -*- lexical-binding: t; -*-

;;; Commentary:
;; Compose workflow using org-msg and msmtp sendmail setup.

;;; Code:

(require 'hub-utils)

(defgroup hub/tmp nil
  "Temporary directory configuration."
  :group 'environment)

(defcustom hub/tmp-directory (expand-file-name "var/tmp/" user-emacs-directory)
  "Base directory for temporary files created by helper modes."
  :type 'directory
  :group 'hub/tmp)

(defun make-tmp-file-browsable ()
  "Allow temporary files to be accessed by the browser."
  (interactive)
  (let ((dir hub/tmp-directory))
    (unless (file-directory-p dir) (make-directory dir t))
    (setq-local temporary-file-directory dir)))

(use-package org-msg
  :after org
  :config
  (setq org-msg-options "html-postamble:nil H:5 num:nil ^:{} toc:nil author:nil email:nil \\
:t tex:dvipng"
	org-msg-startup "hidestars indent inlineimages"
	org-msg-greeting-fmt "\nHi%s,\n\n"
	org-msg-greeting-name-limit 3
	org-msg-text-plain-alternative t
	org-msg-posting-style 'top-posting
	org-msg-default-alternatives '((new           . (text html))
				       (reply-to-html . (text html))
				       (reply-to-text . (text html)))
	org-msg-convert-citation t
	org-msg-signature "\n#+begin_signature\n--\n\nHubert\n#+end_signature")
  (add-hook 'org-msg-mode-hook 'make-tmp-file-browsable)
  (add-hook 'org-msg-mode-hook 'turn-off-auto-fill)

  (defun my-org-msg-composition-parameters (orig-fun &rest args)
    "Adjust greeting and signature for plain/text replies."
    (let ((res (apply orig-fun args)))
      (when (equal (cadr args) '(text))
	(setf (alist-get 'signature res)
	      (replace-regexp-in-string "\\.html.*" "" org-msg-signature)))
      res))
  (advice-add 'org-msg-composition-parameters :around #'my-org-msg-composition-parameters)
  (org-msg-mode))

;; Sending via msmtp
(setq send-mail-function 'sendmail-send-it
      message-send-mail-function 'message-send-mail-with-sendmail
      sendmail-program "msmtp"
      mail-specify-envelope-from t
      message-sendmail-envelope-from 'header
      mail-envelope-from 'header)

(provide 'email/compose)
;;; compose.el ends here
