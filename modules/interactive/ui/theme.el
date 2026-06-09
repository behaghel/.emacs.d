;;; theme.el --- UI theme configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Graphical theme configuration layered on top of ui/core.

;;; Code:

(use-package modus-themes
  :demand t
  :bind (:map evil-normal-state-map (",zk" . modus-themes-toggle))
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
  (load-theme 'modus-vivendi-tinted :no-confirm))

(provide 'ui/theme)
;;; theme.el ends here
