;;; hub-prose.el --- Prose editing display helpers -*- lexical-binding: t; -*-

;;; Commentary:
;; Shared helpers for prose buffers that should display as visually filled
;; without inserting hard line breaks into the file.

;;; Code:

(require 'hub-utils)

(defgroup hub/prose nil
  "Customizations for prose editing display."
  :group 'editing)

(defcustom hub/prose-visual-fill-column 90
  "Visual fill width for prose buffers."
  :type 'integer
  :group 'hub/prose)

(defun hub/prose-visual-fill-mode ()
  "Enable virtual autofill for the current prose buffer.

This disables hard autofill, enables word wrapping, and uses
`visual-fill-column-mode' when available so wrapping happens at
`hub/prose-visual-fill-column' without changing the file contents."
  (auto-fill-mode -1)
  (visual-line-mode 1)
  (setq-local comment-auto-fill-only-comments nil
	      truncate-lines nil
	      word-wrap t)
  (when (require 'visual-fill-column nil t)
    (setq-local visual-fill-column-width hub/prose-visual-fill-column
		visual-fill-column-center-text t)
    (visual-fill-column-mode 1)))

(provide 'hub-prose)
;;; hub-prose.el ends here
