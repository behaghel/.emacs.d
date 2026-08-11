;;; org-sync-provider.el --- Org sync provider registry -*- lexical-binding: t; -*-

;;; Commentary:
;; Provider-neutral registry for Org synchronization status adapters.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defvar org-sync-providers nil
  "Registered Org sync providers.
Each provider is a plist containing at least `:kind' and `:detect'.")

(defun org-sync-register-provider (&rest provider)
  "Register org-sync PROVIDER plist and return it."
  (let ((kind (plist-get provider :kind)))
    (unless kind
      (user-error "Org sync provider requires :kind"))
    (unless (functionp (plist-get provider :detect))
      (user-error "Org sync provider %s requires :detect" kind))
    (setq org-sync-providers
	  (cons provider
		(cl-remove kind org-sync-providers
			   :key (lambda (entry) (plist-get entry :kind))
			   :test #'equal)))
    provider))

(defun org-sync-provider (kind)
  "Return org-sync provider KIND, or nil."
  (cl-find kind org-sync-providers
	   :key (lambda (provider) (plist-get provider :kind))
	   :test #'equal))

(defun org-sync-detect-documents (source-buffer)
  "Return all provider descriptors matching SOURCE-BUFFER."
  (delq nil
	(mapcar (lambda (provider)
		  (when-let* ((detect (plist-get provider :detect))
			      (descriptor (funcall detect source-buffer)))
		    (let ((copy (copy-sequence descriptor)))
		      (plist-put copy :kind
				 (or (plist-get copy :kind)
				     (plist-get provider :kind)))
		      (plist-put copy :provider provider))))
		org-sync-providers)))

(defun org-sync-detect-document (&optional source-buffer)
  "Return the single org-sync document descriptor for SOURCE-BUFFER."
  (let* ((source (or source-buffer (current-buffer)))
	 (matches (org-sync-detect-documents source)))
    (pcase (length matches)
      (0 (user-error "No Org sync provider detected for this buffer"))
      (1 (car matches))
      (_ (user-error "Multiple Org sync providers detected for this buffer")))))

(provide 'org-sync-provider)
;;; org-sync-provider.el ends here
