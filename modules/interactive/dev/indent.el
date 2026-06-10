;;; indent.el --- Development indentation defaults -*- lexical-binding: t; -*-

;;; Commentary:
;; Indentation defaults and yank indentation behavior for programming buffers.

;;; Code:

(require 'hub-utils)

(setq-default indent-tabs-mode nil)
(setq backward-delete-char-untabify-method 'hungry)

(use-package editorconfig :defer t :diminish editorconfig-mode)
(use-package dtrt-indent :disabled t :defer 3
  :config (dtrt-indent-mode)
  (setq dtrt-indent-min-quality 60 dtrt-indent-verbosity 3))

(defun hub/indent-yanked-region-in-prog-mode (&rest _args)
  "Indent the just-yanked region in programming buffers."
  (and (not current-prefix-arg)
       (derived-mode-p 'prog-mode)
       (let ((mark-even-if-inactive transient-mark-mode))
	 (indent-region (region-beginning) (region-end) nil))))

(advice-add 'yank :after #'hub/indent-yanked-region-in-prog-mode)
(advice-add 'yank-pop :after #'hub/indent-yanked-region-in-prog-mode)

(provide 'dev/indent)
;;; indent.el ends here
