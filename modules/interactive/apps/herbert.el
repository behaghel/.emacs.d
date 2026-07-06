;;; herbert.el --- Herbert cockpit integration -*- lexical-binding: t; -*-

;;; Commentary:
;; Dedicated Herbert perspective wrapper.  Keeps the generated dashboard and
;; impacted Org item windows isolated from project/mail/etc. layouts.

;;; Code:

(defvar hub/persp--suppress nil
  "When non-nil, suppress automatic perspective handling.")

(autoload 'herbert "herbert-dashboard" "Open the Herbert dashboard." t)

(defgroup hub/herbert nil
  "Personal Herbert cockpit integration."
  :group 'applications)

(defcustom hub/herbert-perspective-name "Herbert"
  "Perspective name used for Herbert."
  :type 'string
  :group 'hub/herbert)

(defun hub/herbert--switch-perspective ()
  "Switch to the dedicated Herbert perspective when available."
  (when (fboundp 'persp-switch)
    (persp-switch hub/herbert-perspective-name)))

;;;###autoload
(defun hub/herbert ()
  "Open Herbert in its dedicated perspective."
  (interactive)
  (let ((hub/persp--suppress t))
    (hub/herbert--switch-perspective)
    (call-interactively #'herbert)))

(defun hub/herbert--around-herbert (orig &rest args)
  "Run ORIG `herbert' inside the Herbert perspective."
  (let ((hub/persp--suppress t))
    (hub/herbert--switch-perspective)
    (apply orig args)))

(when (fboundp 'advice-add)
  (advice-add 'herbert :around #'hub/herbert--around-herbert))

(provide 'apps/herbert)
;;; herbert.el ends here
