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
  :commands (dired-vc-next-action)
  :init
  (setq dired-isearch-filenames 'dwim
	dired-create-destination-dirs 'ask
	dired-vc-rename-file t)
  (with-eval-after-load 'dired
    (evil-define-key 'normal dired-mode-map ",vv" #'dired-vc-next-action)))

(use-package dired-subtree
  :commands (dired-subtree-toggle dired-subtree-cycle)
  :init
  (with-eval-after-load 'dired
    (bind-keys :map dired-mode-map
	       ("<tab>" . dired-subtree-toggle)
	       ("<C-tab>" . dired-subtree-cycle))))

(use-package dired-narrow
  :commands (dired-narrow)
  :init
  (with-eval-after-load 'dired
    (bind-key ">" #'dired-narrow dired-mode-map)))

(use-package wdired
  :straight nil
  :commands (wdired-change-to-wdired-mode)
  :init
  (setq wdired-allow-to-change-permissions t
	wdired-create-parent-directories t))

(use-package dired-ranger
  :commands (dired-ranger-copy dired-ranger-move dired-ranger-paste)
  :init
  (with-eval-after-load 'dired
    (bind-keys :map dired-mode-map
	       ("Y" . dired-ranger-copy)
	       ("X" . dired-ranger-move)
	       ("V" . dired-ranger-paste))))

(provide 'navigation/dired)
;;; dired.el ends here
