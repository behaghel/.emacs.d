;;; brain.el --- Notes: Denote configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Personal knowledge system with denote.

;;; Code:

(require 'hub-utils)

(defgroup hub/notes nil
  "Notes configuration for Denote."
  :group 'denote)

(defcustom hub/denote-directory (expand-file-name "~/ws/blog.behaghel.org/content-org/journal/")
  "Default directory for Denote notes."
  :type 'directory
  :group 'hub/notes)

(defconst hub/denote--org-front-matter
  "#+title:      %s
#+date:       %s
#+filetags:   %s
#+identifier: %s
#+signature:  %s
#+latex_class: hub-article

"
  "Org front matter used for new Denote notes.")

(defun hub/denote--configure-org-capture ()
  "Configure Org capture templates that create Denote notes."
  (setq denote-org-capture-specifiers "%l\n%i\n%?")
  (add-to-list 'org-capture-templates
	       '("n" "New note (with denote.el)" plain
		 (file denote-last-path)
		 #'denote-org-capture
		 :no-save t :immediate-finish nil :kill-buffer t :jump-to-captured t)))

(use-package denote
  :defer t
  :commands (denote
	     denote-backlinks
	     denote-date
	     denote-dired-mode-in-directories
	     denote-fontify-links-mode-maybe
	     denote-link-find-backlink
	     denote-link-find-file
	     denote-link-add-links
	     denote-link-or-create
	     denote-open-or-create
	     denote-org-capture
	     denote-rename-file
	     denote-rename-file-using-front-matter
	     denote-subdirectory
	     denote-template
	     denote-type)
  :init
  (setq denote-directory hub/denote-directory
	denote-org-front-matter hub/denote--org-front-matter
	denote-known-keywords '("emacs" "faith" "family" "hubert" "pro" "engineering" "leadership")
	denote-infer-keywords t
	denote-sort-keywords t
	denote-prompts '(title keywords)
	denote-excluded-directories-regexp nil
	denote-rename-confirmations '(rewrite-front-matter modify-file-name)
	denote-date-prompt-use-org-read-date t
	denote-allow-multi-word-keywords nil
	denote-date-format nil
	denote-backlinks-show-context t
	denote-dired-directories (list denote-directory))

  (evil-global-set-key 'normal ",no" #'denote-open-or-create)
  (evil-global-set-key 'normal ",nn" #'denote)
  (evil-global-set-key 'normal ",nt" #'denote-type)
  (evil-global-set-key 'normal ",nd" #'denote-date)
  (evil-global-set-key 'normal ",ns" #'denote-subdirectory)
  (evil-global-set-key 'normal ",nt" #'denote-template)
  (evil-global-set-key 'normal ",nr" #'denote-rename-file)

  (with-eval-after-load 'evil-collection
    (evil-collection-define-key 'normal 'org-mode-map
				",nl" #'denote-link-or-create
				",nL" #'denote-link-add-links
				",nb" #'denote-backlinks
				",nf" #'denote-link-find-file
				",nB" #'denote-link-find-backlink
				",nR" #'denote-rename-file-using-front-matter))

  (add-hook 'text-mode-hook #'denote-fontify-links-mode-maybe)
  (add-hook 'dired-mode-hook #'denote-dired-mode-in-directories)
  (with-eval-after-load 'org-capture
    (hub/denote--configure-org-capture)))

(provide 'notes/brain)
;;; brain.el ends here
