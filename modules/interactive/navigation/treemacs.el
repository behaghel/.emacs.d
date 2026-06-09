;;; treemacs.el --- Navigation: Treemacs config -*- lexical-binding: t; -*-

;;; Commentary:
;; Extracted from legacy setup-treemacs.el. Treemacs core + evil + extras.

;;; Code:

(require 'hub-utils)

;; Some older Custom snippets rely on a dynamically scoped `thisfile` during
;; `after-load-functions`; define it to avoid void-variable errors when loading
;; this module in isolated/batch setups.
(defvar thisfile nil)

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
  (add-hook 'treemacs-mode-hook
	    (lambda ()
	      ;; Disable which-function and its hooks to avoid nil buffer errors.
	      (setq-local which-function-mode nil)
	      (remove-hook 'find-file-hook #'which-function-ff-hook t)
	      (remove-hook 'after-save-hook #'which-function-imenu-setup t)))

  (with-eval-after-load 'evil-collection
    (evil-collection-define-key 'normal 'global
				",gt" 'treemacs-select-window
				",mt" 'treemacs-bookmark)))

(use-package treemacs-evil
  :after (treemacs evil)
  :config
  (define-key evil-treemacs-state-map (kbd "M-c") #'hub/window-focus-far-left)
  (define-key evil-treemacs-state-map (kbd "M-t") #'hub/window-focus-far-down)
  (define-key evil-treemacs-state-map (kbd "M-s") #'hub/window-focus-far-up)
  (define-key evil-treemacs-state-map (kbd "M-n") #'hub/window-focus-far-right)
  (define-key evil-treemacs-state-map (kbd "z.")  #'treemacs-toggle-show-dotfiles)
  (define-key evil-treemacs-state-map (kbd "zw")  #'treemacs-toggle-fixed-width)
  (define-key evil-treemacs-state-map (kbd "zv")  #'treemacs-fringe-indicator-mode)
  (define-key evil-treemacs-state-map (kbd "zf")  #'treemacs-follow-mode)
  (define-key evil-treemacs-state-map (kbd "za")  #'treemacs-filewatch-mode)
  (define-key evil-treemacs-state-map (kbd "zg")  #'treemacs-git-mode))

(use-package treemacs-icons-dired
  :after (treemacs dired)
  :config (treemacs-icons-dired-mode))

;; Avoid a startup-time `use-package' declaration for this optional bridge.
;; Load it only when both packages it connects are already in use.
(with-eval-after-load 'magit
  (with-eval-after-load 'treemacs
    (require 'treemacs-magit nil 'noerror)))

(provide 'navigation/treemacs)
;;; treemacs.el ends here
