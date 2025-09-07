;;; dired.el --- Navigation: Dired config -*- lexical-binding: t; -*-

;;; Commentary:
;; Dired defaults, helpers and keybindings.

;;; Code:

(use-package dired
  :straight nil
  :after evil
  :config
  (setq dired-dwim-target t
	dired-listing-switches "-XGFahlv --group-directories-first --time-style=long-iso")
  (define-key evil-normal-state-map (kbd "g/") 'dired))

(use-package dired-aux
  :straight nil
  :after evil
  :config
  (evil-define-key 'normal dired-mode-map ",vv" 'dired-vc-next-action)
  (setq dired-isearch-filenames 'dwim
	dired-create-destination-dirs 'ask
	dired-vc-rename-file t))

(use-package dired-subtree
  :config
  (bind-keys :map dired-mode-map
	     ("<tab>" . dired-subtree-toggle)
	     ("<C-tab>" . dired-subtree-cycle)))

(use-package dired-narrow
  :bind (:map dired-mode-map (">" . dired-narrow)))

(use-package wdired
  :config
  (setq wdired-allow-to-change-permissions t
	wdired-create-parent-directories t))

(use-package dired-ranger
  :bind (:map dired-mode-map
	      ("Y" . dired-ranger-copy)
	      ("X" . dired-ranger-move)
	      ("V" . dired-ranger-paste)))

(provide 'navigation/dired)
;;; dired.el ends here
