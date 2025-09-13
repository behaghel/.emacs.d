;;; treemacs.el --- Navigation: Treemacs config -*- lexical-binding: t; -*-

;;; Commentary:
;; Extracted from legacy setup-treemacs.el. Treemacs core + evil + extras.

;;; Code:

(use-package treemacs
  :after (doom-themes evil)
  :bind (("M-0"       . treemacs-select-window)
	 ("C-x t 1"   . treemacs-delete-other-windows)
	 ("C-x t t"   . treemacs)
	 ("C-x t B"   . treemacs-bookmark)
	 ("C-x t C-t" . treemacs-find-file)
	 ("C-x t M-t" . treemacs-find-tag))
  :config
  (require 'treemacs-icons)
  (require 'treemacs-follow-mode)
  (treemacs-follow-mode t)
  (when (require 'treemacs-project-follow-mode nil 'noerror)
    (treemacs-project-follow-mode t))
  (require 'treemacs-filewatch-mode)
  (treemacs-filewatch-mode t)
  (require 'treemacs-fringe-indicator)
  (treemacs-fringe-indicator-mode t)
  ;; Prefer simple git mode to avoid deferred annotation timer issues
  (when (executable-find "git")
    (treemacs-git-mode 'simple))
  ;; Disable annotations if available to reduce timer noise
  (with-eval-after-load 'treemacs-annotations
    (when (fboundp 'treemacs-annotations-mode)
      (ignore-errors (treemacs-annotations-mode -1))))

  (require 'doom-themes-ext-treemacs)
  (setq doom-themes-treemacs-theme "doom-colors")
  (doom-themes-treemacs-config)

  ;; Treemacs buffers don’t need which-function-mode; avoid mode-line hook errors
  (add-hook 'treemacs-mode-hook (lambda () (setq-local which-function-mode nil)))

  (with-eval-after-load 'evil-collection
    (evil-collection-define-key 'normal 'global
				",gt" 'treemacs-select-window
				",mt" 'treemacs-bookmark)))

(use-package treemacs-evil
  :after (treemacs evil)
  :config
  (define-key evil-treemacs-state-map (kbd "z.")  #'treemacs-toggle-show-dotfiles)
  (define-key evil-treemacs-state-map (kbd "zw")  #'treemacs-toggle-fixed-width)
  (define-key evil-treemacs-state-map (kbd "zv")  #'treemacs-fringe-indicator-mode)
  (define-key evil-treemacs-state-map (kbd "zf")  #'treemacs-follow-mode)
  (define-key evil-treemacs-state-map (kbd "za")  #'treemacs-filewatch-mode)
  (define-key evil-treemacs-state-map (kbd "zg")  #'treemacs-git-mode))

(use-package treemacs-icons-dired
  :after (treemacs dired)
  :config (treemacs-icons-dired-mode))

(use-package treemacs-magit
  :after (treemacs magit))

(provide 'navigation/treemacs)
;;; treemacs.el ends here
