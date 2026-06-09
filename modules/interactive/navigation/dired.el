;;; dired.el --- Navigation: Dired config -*- lexical-binding: t; -*-

;;; Commentary:
;; Dired defaults, helpers and keybindings.

;;; Code:

(use-package dired
  :straight nil
  :commands (dired)
  :init
  (setq dired-dwim-target t
	dired-listing-switches "-XGFahlv --group-directories-first --time-style=long-iso")
  (define-key evil-normal-state-map (kbd "g/") #'dired))

(use-package dired-aux
  :straight nil
  :after dired
  :init
  (setq dired-isearch-filenames 'dwim
	dired-create-destination-dirs 'ask
	dired-vc-rename-file t)
  :config
  (evil-define-key 'normal dired-mode-map ",vv" #'dired-vc-next-action))

(use-package dired-subtree
  :after dired
  :bind (:map dired-mode-map
	      ("<tab>" . dired-subtree-toggle)
	      ("<C-tab>" . dired-subtree-cycle)))

(use-package dired-narrow
  :after dired
  :bind (:map dired-mode-map (">" . dired-narrow)))

(use-package wdired
  :straight nil
  :commands (wdired-change-to-wdired-mode)
  :init
  (setq wdired-allow-to-change-permissions t
	wdired-create-parent-directories t))

(use-package dired-ranger
  :after dired
  :bind (:map dired-mode-map
	      ("Y" . dired-ranger-copy)
	      ("X" . dired-ranger-move)
	      ("V" . dired-ranger-paste)))

(provide 'navigation/dired)
;;; dired.el ends here
