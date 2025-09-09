;;; brain.el --- Notes: Denote configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Personal knowledge system with denote.

;;; Code:

(require 'hub-utils)

(use-package denote
  :config
  (evil-global-set-key 'normal ",no" #'denote-open-or-create)
  (evil-global-set-key 'normal ",nn" #'denote)
  (evil-global-set-key 'normal ",nt" #'denote-type)
  (evil-global-set-key 'normal ",nd" #'denote-date)
  (evil-global-set-key 'normal ",ns" #'denote-subdirectory)
  (evil-global-set-key 'normal ",nt" #'denote-template)
  (evil-global-set-key 'normal ",nr" #'denote-rename-file)

  (evil-collection-define-key 'normal 'org-mode-map
			      ",nl" #'denote-link-or-create
			      ",nL" #'denote-link-add-links
			      ",nb" #'denote-backlinks
			      ",nf" #'denote-link-find-file
			      ",nB" #'denote-link-find-backlink
			      ",nR" #'denote-rename-file-using-front-matter)

  (defgroup hub/notes nil
    "Notes configuration for Denote."
    :group 'denote)
  (defcustom hub/denote-directory (expand-file-name "~/ws/blog.behaghel.org/content-org/journal/")
    "Default directory for Denote notes."
    :type 'directory
    :group 'hub/notes)
  (setq denote-directory hub/denote-directory
	denote-known-keywords '("emacs" "faith" "family" "hubert" "pro" "engineering" "leadership")
	denote-infer-keywords t
	denote-sort-keywords t
	denote-prompts '(title keywords)
	denote-excluded-directories-regexp nil
	denote-rename-confirmations '(rewrite-front-matter modify-file-name)
	denote-date-prompt-use-org-read-date t
	denote-allow-multi-word-keywords nil
	denote-date-format nil
	denote-backlinks-show-context t)

  (add-hook 'text-mode-hook #'denote-fontify-links-mode-maybe)
  (setq denote-dired-directories (list denote-directory))
  (add-hook 'dired-mode-hook #'denote-dired-mode-in-directories)

  (with-eval-after-load 'org-capture
    (setq denote-org-capture-specifiers "%l\n%i\n%?")
    (add-to-list 'org-capture-templates
		 '("n" "New note (with denote.el)" plain
		   (file denote-last-path)
		   #'denote-org-capture
		   :no-save t :immediate-finish nil :kill-buffer t :jump-to-captured t))))

(provide 'notes/brain)
;;; brain.el ends here
