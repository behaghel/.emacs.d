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

;; === darwin-performance begin ===
;; macOS-specific redisplay/scrolling and native-comp tweaks
(when (eq system-type 'darwin)
  (setq fast-but-imprecise-scrolling t
	redisplay-skip-fontification-on-input t
	inhibit-compacting-font-caches t
	frame-resize-pixelwise t
	bidi-display-reordering nil
	bidi-inhibit-bpa t)
  (with-eval-after-load 'comp
    (setq native-comp-async-report-warnings-errors 'silent)
    (when (boundp 'native-comp-async-jobs-number)
      (setq native-comp-async-jobs-number
	    (max 2 (min 8 (or (ignore-errors (num-processors)) 4))))))
  (add-hook 'after-init-hook
	    (lambda ()
	      (when (fboundp 'pixel-scroll-precision-mode)
		(pixel-scroll-precision-mode 1))))
  (with-eval-after-load 'jit-lock
    (setq jit-lock-stealth-time 0.2
	  jit-lock-chunk-size 2048
	  jit-lock-defer-time 0)))
;; === darwin-performance end ===
