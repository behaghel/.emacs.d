;;; export-demo-adaptive-cards.el --- Export demo fixture -*- lexical-binding: t; -*-

;;; Commentary:
;; Helper script to export the adaptive cards demo fixture to PDF.
;; Run via: devenv shell -- env HOME=$PWD emacs --batch -Q -L . -l scripts/export-demo-adaptive-cards.el

;;; Code:

(require 'org)
(require 'ox-latex)

(let ((user-emacs-directory (expand-file-name ".." (file-name-directory load-file-name))))
  (add-to-list 'load-path (expand-file-name "core" user-emacs-directory))
  (add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
  (add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
  (add-to-list 'load-path (expand-file-name "modules/lang" user-emacs-directory))
  (add-to-list 'load-path (expand-file-name "modules/org" user-emacs-directory))
  (add-to-list 'load-path (expand-file-name "modules/interactive" user-emacs-directory))
  (require 'core-packages)
  (require 'org/core)
  (require 'org/export-latex))

(defun hub/export-demo-adaptive-cards ()
  "Export the adaptive cards demo fixture to PDF."
  (let* ((repo-root (expand-file-name ".." (file-name-directory load-file-name)))
	 (fixture (expand-file-name "test/fixtures/org-export/demo-adaptive-cards.org" repo-root))
	 (out-dir (expand-file-name "var/demo-adaptive-cards" repo-root)))
    (make-directory out-dir t)
    (with-current-buffer (find-file-noselect fixture)
      (let ((hub/org-export-output-root out-dir))
	(hub/org-export-buffer-to-pdf out-dir)))
    (message "Exported to %s" out-dir)))

(hub/export-demo-adaptive-cards)

(provide 'export-demo-adaptive-cards)
;;; export-demo-adaptive-cards.el ends here
