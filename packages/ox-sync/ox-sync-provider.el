;;; ox-sync-provider.el --- Org sync provider registry -*- lexical-binding: t; -*-

;;; Commentary:
;; Provider-neutral registry for Org synchronization status adapters.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defvar ox-sync-providers nil
  "Registered Org sync providers.
Each provider is a plist containing at least `:kind' and `:detect'.")

(defun ox-sync-register-provider (&rest provider)
  "Register ox-sync PROVIDER plist and return it."
  (let ((kind (plist-get provider :kind)))
    (unless kind
      (user-error "Org sync provider requires :kind"))
    (unless (functionp (plist-get provider :detect))
      (user-error "Org sync provider %s requires :detect" kind))
    (setq ox-sync-providers
	  (cons provider
		(cl-remove kind ox-sync-providers
			   :key (lambda (entry) (plist-get entry :kind))
			   :test #'equal)))
    provider))

(defun ox-sync-provider (kind)
  "Return ox-sync provider KIND, or nil."
  (cl-find kind ox-sync-providers
	   :key (lambda (provider) (plist-get provider :kind))
	   :test #'equal))

(defun ox-sync-detect-documents (source-buffer)
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
		ox-sync-providers)))

(defun ox-sync-detect-document (&optional source-buffer)
  "Return the single ox-sync document descriptor for SOURCE-BUFFER."
  (let* ((source (or source-buffer (current-buffer)))
	 (matches (ox-sync-detect-documents source)))
    (pcase (length matches)
      (0 (user-error "No Org sync provider detected for this buffer"))
      (1 (car matches))
      (_ (user-error "Multiple Org sync providers detected for this buffer")))))

(provide 'ox-sync-provider)
;;; ox-sync-provider.el ends here
