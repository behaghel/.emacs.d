;;; js.el --- Wrapper to avoid clobbering built-in js -*- lexical-binding: t; -*-

;;; Commentary:
;; This file delegates to the built-in `js.el` so callers such as Org export can
;; rely on stock functionality.  Our richer JavaScript/TypeScript configuration
;; lives in `js-config.el` and loads only once the package layer is ready.

;;; Code:

(require 'cl-lib)

(defconst lang/js--module-dir
  (and load-file-name (file-name-directory (expand-file-name load-file-name)))
  "Directory containing the lang/js module components.")

(defun lang/js--load-builtin ()
  "Load the built-in js.el without recursing into this module."
  (let* ((module-dir (and lang/js--module-dir
			  (directory-file-name lang/js--module-dir)))
	 (load-path (if module-dir
			(cl-remove-if (lambda (path)
					(when path
					  (string= (directory-file-name (expand-file-name path))
						   module-dir)))
				      load-path)
		      load-path))
	 (builtin (locate-library "js" nil load-path)))
    (when builtin
      (load builtin nil t t))))

(lang/js--load-builtin)

(defvar lang/js--config-loaded nil
  "Internal flag recording whether `js-config.el' has been loaded.")

(defun lang/js--maybe-load-config (&rest _)
  "Load the full JS configuration when straight/use-package are available."
  (unless lang/js--config-loaded
    ;; Keep batch tasks (e.g., Org export) independent from optional JS packages.
    (when (and (not noninteractive)
	       lang/js--module-dir
	       (fboundp 'use-package)
	       (fboundp 'straight-use-package))
      (let* ((candidate (expand-file-name "js-config.el" lang/js--module-dir)))
	(if (file-readable-p candidate)
	    (progn
	      (load candidate nil t t)
	      (setq lang/js--config-loaded t))
	  (message "[lang/js] Optional js-config.el not found at %s" candidate))))))

(lang/js--maybe-load-config)
(add-hook 'after-init-hook #'lang/js--maybe-load-config)
(with-eval-after-load 'straight (lang/js--maybe-load-config))

(provide 'js)
;;; js.el ends here
