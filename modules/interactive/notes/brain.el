;;; brain.el --- Notes: Denote configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Personal knowledge system with denote.

;;; Code:

(require 'hub-denote)
(require 'hub-noise)
(require 'hub-utils)
(require 'cl-lib)

(defcustom hub/denote-known-keywords
  '("emacs" "faith" "family" "hubert" "pro" "engineering" "leadership")
  "Known keywords offered when creating personal and blog Denote notes."
  :type '(repeat string)
  :group 'hub/notes)

(defcustom hub/denote-work-known-keywords
  '("product" "engineering" "business" "culture" "organisation")
  "Known keywords offered when creating work Denote notes."
  :type '(repeat string)
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

(defconst hub/denote--work-org-front-matter
  "#+title:      %s
#+date:       %s
#+filetags:   %s
#+identifier: %s
#+signature:  %s

"
  "Org front matter used for new work Denote notes.")

(put 'denote-org-front-matter 'safe-local-variable #'stringp)

(defun hub/denote--with-settings (command directory keywords front-matter)
  "Call Denote COMMAND using DIRECTORY, KEYWORDS, and FRONT-MATTER."
  (make-directory directory t)
  (with-temp-buffer
    (cl-progv '(denote-directory
		denote-known-keywords
		denote-org-front-matter
		denote-prompts)
	(list directory keywords front-matter '(title keywords))
      (call-interactively command))))

(defun hub/denote-personal ()
  "Open or create a Denote note in `hub/denote-directory'."
  (interactive)
  (hub/denote--with-settings
   #'denote-open-or-create
   hub/denote-directory
   hub/denote-known-keywords
   hub/denote--org-front-matter))

(defun hub/denote-personal-create ()
  "Create a Denote note in `hub/denote-directory'."
  (interactive)
  (hub/denote--with-settings
   #'denote
   hub/denote-directory
   hub/denote-known-keywords
   hub/denote--org-front-matter))

(defun hub/denote-work ()
  "Open or create a Denote note in `hub/denote-work-directory'."
  (interactive)
  (hub/denote--with-settings
   #'denote-open-or-create
   hub/denote-work-directory
   hub/denote-work-known-keywords
   hub/denote--work-org-front-matter))

(defun hub/denote-work-create ()
  "Create a Denote note in `hub/denote-work-directory'."
  (interactive)
  (hub/denote--with-settings
   #'denote
   hub/denote-work-directory
   hub/denote-work-known-keywords
   hub/denote--work-org-front-matter))

(defun hub/denote-blog-create ()
  "Create a Denote note in `hub/denote-blog-directory'."
  (interactive)
  (hub/denote--with-settings
   #'denote
   hub/denote-blog-directory
   hub/denote-known-keywords
   hub/denote--org-front-matter))

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
	denote-known-keywords hub/denote-known-keywords
	denote-infer-keywords t
	denote-sort-keywords t
	denote-prompts '(title keywords)
	denote-excluded-directories-regexp nil
	denote-excluded-files-regexp
	(mapconcat (lambda (suffix)
		     (concat (regexp-quote suffix) "\\'"))
		   hub/noise-sidecar-suffixes
		   "\\|")
	denote-rename-confirmations '(rewrite-front-matter modify-file-name)
	denote-date-prompt-use-org-read-date t
	denote-allow-multi-word-keywords nil
	denote-date-format nil
	denote-backlinks-show-context t
	denote-dired-directories (list denote-directory))

  (evil-global-set-key 'normal ",no" #'hub/denote-personal)
  (evil-global-set-key 'normal ",nn" #'hub/denote-personal-create)
  (evil-global-set-key 'normal ",nw" #'hub/denote-work)
  (evil-global-set-key 'normal ",nW" #'hub/denote-work-create)
  (evil-global-set-key 'normal ",nj" #'hub/denote-blog-create)
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
