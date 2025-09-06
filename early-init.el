;;; early-init.el --- Early init tweaks -*- lexical-binding: t; -*-

;;; Commentary:
;; Minimal, under-the-hood startup improvements that do not alter UX.
;; - Disable package.el auto-activation (we use straight.el)
;; - Loosen GC during init, then restore after startup
;; - Temporarily disable file-name handlers for faster startup

;;; Code:

(defvar hub--original-gc-cons-threshold gc-cons-threshold)
(defvar hub--original-gc-cons-percentage gc-cons-percentage)
(defvar hub--original-file-name-handler-alist file-name-handler-alist)

(setq package-enable-at-startup nil
      gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6
      file-name-handler-alist nil)

(add-hook 'after-init-hook
	  (lambda ()
	    (setq gc-cons-threshold hub--original-gc-cons-threshold
		  gc-cons-percentage hub--original-gc-cons-percentage
		  file-name-handler-alist hub--original-file-name-handler-alist)))

(provide 'early-init)
;;; early-init.el ends here
