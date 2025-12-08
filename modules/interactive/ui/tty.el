;;; tty.el --- UI: TTY-specific tweaks -*- lexical-binding: t; -*-

;;; Commentary:
;; Terminal-friendly UI adjustments layered on top of ui/core.

;;; Code:

(use-package modus-themes
  :demand t
  :config
  (setq modus-themes-slanted-constructs t
	modus-themes-bold-constructs nil
	modus-themes-variable-pitch-ui nil
	modus-themes-headings '((1 . (variable-pitch extrabold 1.15))
				(2 . (variable-pitch 1.1))
				(3 . (variable-pitch semibold 1.05))
				(4 . (variable-pitch 1))
				(5 . (variable-pitch 0.9))
				(agenda-date . (0.8))
				(agenda-structure . (variable-pitch light 1))
				(t . (1)))
	modus-themes-mixed-fonts t
	modus-themes-prompts '(extrabold italic)
	modus-themes-org-blocks 'tinted-background
	modus-themes-mode-line '(borderless))
  ;; Stick to a high-contrast dark palette in terminals so diffs remain legible.
  (load-theme 'modus-vivendi :no-confirm))

(provide 'ui/tty)
;;; tty.el ends here
