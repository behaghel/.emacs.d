;;; diff.el --- Development diff configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Ediff defaults and theme-aware face integration for development workflows.

;;; Code:

(require 'hub-utils)

(setq ediff-diff-options "-w")
(setq ediff-split-window-function 'split-window-horizontally)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

(defun hub/ediff-use-diff-faces (&rest _)
  "Reuse diff-mode faces for Ediff to keep contrast in TTY sessions."
  (dolist (mapping '((ediff-current-diff-A . diff-removed)
		     (ediff-current-diff-B . diff-added)
		     (ediff-current-diff-C . diff-changed)
		     (ediff-current-diff-Ancestor . diff-changed)
		     (ediff-fine-diff-A . diff-refine-removed)
		     (ediff-fine-diff-B . diff-refine-added)
		     (ediff-fine-diff-C . diff-refine-changed)
		     (ediff-fine-diff-Ancestor . diff-refine-changed)
		     (ediff-merge-current-diff-A . diff-removed)
		     (ediff-merge-current-diff-B . diff-added)
		     (ediff-merge-current-diff-C . diff-changed)
		     (ediff-merge-current-diff-Ancestor . diff-changed)
		     (ediff-merge-diff-A . diff-removed)
		     (ediff-merge-diff-B . diff-added)
		     (ediff-merge-diff-C . diff-changed)
		     (ediff-merge-diff-Ancestor . diff-changed)
		     (ediff-merge-fine-diff-A . diff-refine-removed)
		     (ediff-merge-fine-diff-B . diff-added)
		     (ediff-merge-fine-diff-C . diff-refine-changed)
		     (ediff-merge-fine-diff-Ancestor . diff-refine-changed)))
    (when (and (facep (car mapping)) (facep (cdr mapping)))
      (set-face-attribute (car mapping) nil :inherit (cdr mapping)
			  :foreground nil :background nil)))
  (dolist (face '(ediff-even-diff-A ediff-even-diff-B ediff-even-diff-C
				    ediff-even-diff-Ancestor ediff-odd-diff-A ediff-odd-diff-B
				    ediff-odd-diff-C ediff-odd-diff-Ancestor))
    (when (facep face)
      (set-face-attribute face nil :inherit 'diff-context
			  :foreground nil :background nil))))

(add-hook 'ediff-load-hook #'hub/ediff-use-diff-faces)
(add-hook 'enable-theme-functions #'hub/ediff-use-diff-faces)

(provide 'dev/diff)
;;; diff.el ends here
