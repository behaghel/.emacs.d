;;; packages.el --- straight.el bootstrap and use-package -*- lexical-binding: t; -*-

;;; Commentary:
;; Centralizes package management with straight.el and use-package.

;;; Code:

(defvar bootstrap-version)
(let* ((repo "radian-software/straight.el")
       (branch "develop")
       (install-url (format "https://raw.githubusercontent.com/%s/%s/install.el" repo branch))
       (bootstrap-file (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory)))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
	(url-retrieve-synchronously install-url 'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq straight-use-package-by-default t)

(straight-use-package 'use-package)
(require 'use-package)
(require 'use-package-ensure)
(require 'use-package-delight)
(require 'use-package-diminish)

;;;###autoload
(defun core/packages-freeze ()
  (interactive)
  (straight-freeze-versions))

;;;###autoload
(defun core/packages-thaw ()
  (interactive)
  (straight-thaw-versions))

(provide 'core-packages)
;;; packages.el ends here
