;;; core.el --- Org mode: agenda, capture, editing -*- lexical-binding: t; -*-

;;; Commentary:
;; Core Org configuration: agenda, capture, editing UX, integrations.

;;; Code:

(setq org-directory "~/Dropbox/Documents/org/")

(use-package org
  :straight (:depth full)
  :commands (org-capture org-agenda)
  :after (evil evil-collection)
  :mode ("\\.\\(org\\|orj\\|org_archive\\|txt\\)$" . org-mode)
  :config
  (define-key evil-normal-state-map (kbd ",oc") 'org-capture)
  (define-key evil-normal-state-map (kbd ",ol") 'org-store-link)
  (define-key evil-normal-state-map (kbd ",oa") 'org-agenda)
  (evil-collection-define-key 'normal 'org-mode-map
			      ",or" 'org-babel-open-src-block-result
			      ",à"  'org-archive-subtree-default
			      ",s"  'outline-up-heading
			      ",t"  'outline-down-heading
			      "à"   'org-refile
			      (kbd ", SPC") 'hub/outline-focus-next-section
			      (kbd "<next>")  'org-move-subtree-down
			      (kbd "<prior>") 'org-move-subtree-up
			      ",fn" 'org-footnote-new)
  (evil-define-key 'motion org-mode-map (kbd "RET") 'org-return)
  (evil-define-key 'motion calendar-mode-map (kbd "RET") 'org-calendar-select)
  (evil-define-key 'insert org-mode-map (kbd "M-RET") 'org-meta-return)

  (setq org-return-follows-link t
	org-hide-leading-stars t
	org-startup-indented t
	org-footnote-auto-adjust t
	org-cycle-separator-lines 0
	org-archive-location "archive/%s_archive::datetree/")
  (add-hook 'org-mode-hook (lambda () (setq-local comment-auto-fill-only-comments nil)))

  ;; Agenda + capture
  (setq org-agenda-window-setup 'other-window
	org-default-notes-file (concat org-directory "inbox.org")
	org-agenda-files (list (concat org-directory "hubert.org")
			       (concat org-directory "inbox.org")
			       (concat org-directory "gcal-gmail.org")))
  (require 'org-protocol)
  (setq org-capture-templates
	'(("i" "inbox" entry (file org-default-notes-file) "* TODO %?" :prepend t)
	  ("f" "follow-up" entry (file org-default-notes-file) "* TODO %? %a\n  %i" :prepend t)
	  ("r" "respond to email (mu4e)" entry (file org-default-notes-file)
	   "* TODO REPLY to [[mailto:%:fromaddress][%:fromname]] on %a\nDEADLINE: %(org-insert-time-stamp (org-read-date nil t \"+1d\"))\n%U\n\n"
	   :immediate-finish t :prepend t)
	  ("l" "link" entry (file+headline org-default-notes-file "Browsing")
	   "* TODO %(org-cliplink-capture)" :immediate-finish t :prepend t)
	  ("c" "org-protocol-capture" entry (file+headline org-default-notes-file "Browsing")
	   "* TODO [[%:link][%:description]]\n\n %i" :immediate-finish t :prepend t)
	  ("p" "Philosophy" entry (file "faith.org") "* %?\nEntered on %U\n  %i\n  %a")
	  ("m" "Meeting Minutes" entry (file org-default-notes-file)
	   "* Meeting Minutes\n** Present at meeting\n- [X] Peter\n- [ ] Sarah - [X] Lucy\n ** Agenda\n- item 1\n- item 2\n- item 3\n** Notes\n*** Last meeting minutes are approved                              :decision:\n*** Discussion\n**** TODO Topic 1                                      :@Fred:\n**** TODO Topic 2                                    :@Sara:\n**** DONE Topic 2.1                                      :@Lucy:@Ted:\nDEADLINE: <2020-03-01 So>\n**** Another sub-topic                                    :decision:\n* Actions\n#+BEGIN: columnview :id global :match \"/TODO|DONE\" :format \"%ITEM(What) %TAGS(Who) %DEADLINE(When) %TODO(State)\"\n#+END:\n\n* Decisions\n#+BEGIN: columnview :id global :match \"decision\" :format \"%ITEM(decisions)\"\n#+END:")
	  ))

  (require 'tools/blog)
  (setq org-outline-path-complete-in-steps nil
	org-refile-use-outline-path 'file
	org-refile-allow-creating-parent-nodes 'confirm
	org-reverse-note-order t
	org-refile-targets '(("veriff.org" :maxlevel . 4)
			     ("faith.org" :maxlevel . 2)
			     ("hubert.org" :maxlevel . 2)
			     ("family.org" :maxlevel . 2)))

  ;; Babel and exporters
  (setq org-confirm-babel-evaluate nil
	org-export-allow-bind-keywords t
	org-export-backends '(ascii html latex md odt)
	org-cite-global-bibliography (list (expand-file-name "Dropbox/Documents/library.bib" (getenv "HOME")))
	org-cite-csl-styles-dir (expand-file-name "straight/repos/org/etc/csl" user-emacs-directory)
	org-cite-csl-locales-dir (expand-file-name "straight/repos/org/etc/csl" user-emacs-directory)
	org-src-preserve-indentation t
	org-src-window-setup 'other-window
	org-src-tab-acts-natively t)
  (setq org-re-reveal-root (concat (getenv "HOME") "/Apps/reveal.js"))
  (setq org-plantuml-jar-path "~/install/plantuml.jar")
  (add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
  (setq org-html-htmlize-output-type 'css
	org-html-head-include-default-style nil)

  (defun hub/outline-focus-next-section ()
    (interactive)
    (outline-next-heading)
    (outline-show-entry)
    (outline-hide-other)))

(use-package org-re-reveal :defer t)

(use-package evil-org
  :after (evil org)
  :hook (org-mode . evil-org-mode)
  :config
  (setq evil-org-movement-bindings '((up . "s") (down . "t") (left . "c") (right . "r")))
  (evil-org-set-key-theme '(textobjects return insert navigation additional shift calendar))
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys)
  (defun hub/setup-agenda-keybindings ()
    (evil-collection-translate-key 'motion 'org-agenda-mode-map
				   "c" "h" "C" "H" "t" "j" "T" "J" "s" "k" "S" "K" "r" "l" "R" "L"
				   ":" "t" "H" "T" "e" "c" "a" "c" "L" "C" "é" "r" "É" "R" "|" "s" "K" "S"))
  (add-hook 'org-agenda-mode-hook #'hub/setup-agenda-keybindings))

(use-package ox-clip
  :config (evil-define-key 'visual org-mode-map (kbd ",y") 'ox-clip-formatted-copy))

(use-package org-cliplink
  :after org
  :bind (:map evil-normal-state-map (",eP" . org-cliplink)))

(use-package anki-editor
  :after org
  :hook (org-capture-after-finalize . anki-editor-reset-cloze-number)
  :config
  (evil-collection-define-key 'visual 'org-mode-map ",_" 'anki-editor-cloze-region-dont-incr ",-" 'anki-editor-cloze-region-auto-incr)
  (evil-collection-define-key 'normal 'org-mode-map ",0" 'anki-editor-reset-cloze-number ",^" 'anki-editor-push-tree)
  (defun anki-editor-cloze-region-auto-incr (&optional _arg)
    (interactive)
    (anki-editor-cloze-region my-anki-editor-cloze-number "")
    (setq my-anki-editor-cloze-number (1+ my-anki-editor-cloze-number))
    (forward-sexp))
  (defun anki-editor-cloze-region-dont-incr (&optional _arg)
    (interactive)
    (anki-editor-cloze-region (1- my-anki-editor-cloze-number) "")
    (forward-sexp))
  (defun anki-editor-reset-cloze-number (&optional arg)
    (interactive)
    (setq my-anki-editor-cloze-number (or arg 1)))
  (defun anki-editor-push-tree ()
    (interactive)
    (anki-editor-push-notes '(4))
    (anki-editor-reset-cloze-number))
  (anki-editor-reset-cloze-number)
  (add-to-list 'org-capture-templates
	       '("a" "Anki basic" entry (file+headline org-default-notes-file "Anki")
		 "* %U   %^g\n:PROPERTIES:\n:ANKI_NOTE_TYPE: Basic\n:ANKI_DECK: from-org\n:END:\n** Front\n%?\n** Back\n%x\n"))
  (add-to-list 'org-capture-templates
	       '("A" "Anki cloze" entry (file+headline org-default-notes-file "Anki")
		 "* %U   %^g\n:PROPERTIES:\n:ANKI_NOTE_TYPE: Cloze\n:ANKI_DECK: from-org\n:END:\n** Text\n%x\n** Extra\n")))

(use-package org-download
  :after org
  :hook (dired-mode-hook . org-download-enable)
  :config
  (evil-collection-define-key 'normal 'org-mode-map ",Y" 'org-download-clipboard)
  (setq-default org-download-method 'attach
		org-download-heading-lvl nil
		org-download-delete-image-after-download t))

(use-package org-drill :after org :defer 10)
(use-package citeproc)

(provide 'org/core)
;;; core.el ends here
