;;; core.el --- Org mode: agenda, capture, editing -*- lexical-binding: t; -*-

;;; Commentary:
;; Core Org configuration: agenda, capture, editing UX, integrations.

;;; Code:

(require 'hub-utils)
(require 'seq)

(defgroup hub/org nil
  "Customizations for Org paths."
  :group 'org)

(defcustom hub/org-directory "~/Documents/org/"
  "Default Org directory. Set to nil to avoid overriding."
  :type '(choice (const :tag "Do not override" nil) directory)
  :group 'hub/org)

(defcustom hub/org-bibliography-file (expand-file-name "Dropbox/Documents/library.bib" (getenv "HOME"))
  "Default bibliography file for org-cite."
  :type 'file
  :group 'hub/org)

(defcustom hub/org-plantuml-jar "~/install/plantuml.jar"
  "Default PlantUML jar path."
  :type 'file
  :group 'hub/org)

(defcustom hub/org-agenda-file-names '("hubert.org" "inbox.org" "gcal-gmail.org")
  "Agenda file names relative to `org-directory'."
  :type '(repeat string)
  :group 'hub/org)

(defcustom hub/org-veriff-template-file
  (expand-file-name "insert/template.veriff.org" user-emacs-directory)
  "Yasnippet-compatible template used for new Veriff Org articles."
  :type 'file
  :group 'hub/org)

(defcustom hub/org-latex-class-directory
  (expand-file-name "etc/latex" user-emacs-directory)
  "Directory containing repo-local LaTeX class assets for Org templates."
  :type 'directory
  :group 'hub/org)

(defun hub/org--normalize-agenda-files (value)
  "Return VALUE as a list of file paths, or nil.
Accepts a single string or a list of strings."
  (cond
   ((null value) nil)
   ((stringp value) (list value))
   ((listp value) value)
   (t nil)))

(defun hub/org-existing-agenda-files ()
  "Return agenda files that currently exist on disk.
Uses `org-agenda-files' when set, otherwise `hub/org-agenda-file-names'."
  (let* ((candidates (or (hub/org--normalize-agenda-files (bound-and-true-p org-agenda-files))
			 (mapcar (lambda (file)
				   (expand-file-name file org-directory))
				 hub/org-agenda-file-names)))
	 (expanded (mapcar #'expand-file-name candidates)))
    (seq-filter #'file-exists-p expanded)))

(defun hub/org-prune-missing-agenda-files (&optional quiet)
  "Remove missing files from `org-agenda-files'.
When QUIET is non-nil, do not emit informational messages."
  (interactive)
  (let* ((candidates (or (hub/org--normalize-agenda-files org-agenda-files)
			 (mapcar (lambda (file)
				   (expand-file-name file org-directory))
				 hub/org-agenda-file-names)))
	 (expanded (mapcar #'expand-file-name candidates))
	 (existing (seq-filter #'file-exists-p expanded))
	 (missing (seq-remove #'file-exists-p expanded)))
    (setq org-agenda-files existing)
    (unless (or quiet (null missing))
      (message "[org] removed missing agenda files: %s"
	       (mapconcat #'abbreviate-file-name missing ", ")))
    existing))

(when hub/org-directory (setq org-directory hub/org-directory))

(defun hub/org-setup-wrapping ()
  "Use soft wrapping in Org buffers and avoid hard line breaks."
  (auto-fill-mode -1)
  (visual-line-mode 1)
  (setq-local comment-auto-fill-only-comments nil))

(defun hub/org-set-structure-template (key value)
  "Set Org structure template KEY to VALUE without duplicate entries."
  (setq org-structure-template-alist
	(cons (cons key value)
	      (assoc-delete-all key org-structure-template-alist))))

(defun hub/org-tab-dwim (arg)
  "Move through active Yasnippet fields, otherwise run `org-cycle'.
Forward prefix ARG to `org-cycle' so prefixed Org cycling keeps its
native behavior."
  (interactive "P")
  (if (and (null arg)
	   (fboundp 'yas-active-snippets)
	   (yas-active-snippets)
	   (fboundp 'yas-next-field-or-maybe-expand))
      (yas-next-field-or-maybe-expand)
    (org-cycle arg)))

(defun hub/org-discover-local-latex-classes (&optional directory)
  "Return class names discovered from .cls files under DIRECTORY.
When DIRECTORY is nil, use `hub/org-latex-class-directory'."
  (let ((class-directory (or directory hub/org-latex-class-directory)))
    (when (file-directory-p class-directory)
      (sort
       (mapcar #'file-name-base
	       (directory-files class-directory t "\\.cls\\'" 'nosort))
       #'string<))))

(defun hub/org-registered-latex-classes ()
  "Return class names currently registered in `org-latex-classes'."
  (when (boundp 'org-latex-classes)
    (sort (mapcar #'car org-latex-classes) #'string<)))

(defun hub/org-latex-class-candidates ()
  "Return available Org LaTeX class names for template selection.
Repo-local .cls files are listed before classes already registered with Org."
  (delete-dups
   (append (hub/org-discover-local-latex-classes)
	   (hub/org-registered-latex-classes))))

(defun hub/org-read-latex-class ()
  "Prompt for an Org LaTeX class discovered from local and registered classes."
  (let* ((candidates (hub/org-latex-class-candidates))
	 (default (car candidates)))
    (unless candidates
      (user-error "No Org LaTeX classes are available"))
    (completing-read
     (if default
	 (format "LaTeX class (default %s): " default)
       "LaTeX class: ")
     candidates nil t nil nil default)))

(defun hub/org-insert-veriff-template ()
  "Insert the Veriff article template at point.
When Yasnippet is available, expand fields in the inserted template."
  (interactive)
  (let ((template (with-temp-buffer
		    (insert-file-contents hub/org-veriff-template-file)
		    (buffer-string))))
    (if (fboundp 'yas-expand-snippet)
	(yas-expand-snippet template)
      (insert template))))

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
			      ",fn" 'org-footnote-new
			      ",ov" 'hub/org-insert-veriff-template)
  (evil-define-key 'motion org-mode-map (kbd "RET") 'org-return)
  (evil-define-key 'motion calendar-mode-map (kbd "RET") 'org-calendar-select)
  (define-key org-mode-map (kbd "<tab>") #'hub/org-tab-dwim)
  (define-key org-mode-map (kbd "TAB") #'hub/org-tab-dwim)
  (evil-define-key 'insert org-mode-map (kbd "<tab>") #'hub/org-tab-dwim)
  (evil-define-key 'insert org-mode-map (kbd "TAB") #'hub/org-tab-dwim)
  (evil-define-key 'insert org-mode-map (kbd "M-RET") 'org-meta-return)
  (evil-define-key 'insert org-mode-map (kbd "<escape>") 'evil-normal-state)
  (evil-define-key 'insert org-mode-map (kbd "C-[") 'evil-normal-state)

  (hub/org-set-structure-template "c" "comment")
  (hub/org-set-structure-template "C" "center")
  (hub/org-set-structure-template "sf" "standfirst")
  (hub/org-set-structure-template "ep" "epigraph")
  (hub/org-set-structure-template "pq" "pullquote")
  (hub/org-set-structure-template "co" "callout")
  (hub/org-set-structure-template "me" "metrics")
  (hub/org-set-structure-template "mi" "metric")
  (hub/org-set-structure-template "pi" "pillars")
  (hub/org-set-structure-template "pa" "pillar")
  (hub/org-set-structure-template "gr" "graph")
  (require 'org-tempo)

  (setq org-return-follows-link t
	org-hide-leading-stars t
	org-startup-indented t
	org-footnote-auto-adjust t
	org-cycle-separator-lines 0
	org-archive-location "archive/%s_archive::datetree/")
  (add-hook 'org-mode-hook #'hub/org-setup-wrapping)

  ;; Agenda + capture
  (setq org-agenda-window-setup 'other-window
	org-default-notes-file (expand-file-name "inbox.org" org-directory)
	org-agenda-files (mapcar (lambda (file)
				   (expand-file-name file org-directory))
				 hub/org-agenda-file-names))
  (hub/org-prune-missing-agenda-files t)
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
	org-cite-global-bibliography (list hub/org-bibliography-file)
	org-cite-csl-styles-dir (expand-file-name "straight/repos/org/etc/csl" user-emacs-directory)
	org-cite-csl-locales-dir (expand-file-name "straight/repos/org/etc/csl" user-emacs-directory)
	org-src-preserve-indentation t
	org-src-window-setup 'other-window
	org-src-tab-acts-natively t)
  (defcustom hub/org-re-reveal-root (expand-file-name "Apps/reveal.js" (getenv "HOME"))
    "Root directory for reveal.js used by org-re-reveal."
    :type 'directory
    :group 'hub/org)
  (setq org-re-reveal-root hub/org-re-reveal-root)
  (setq org-plantuml-jar-path hub/org-plantuml-jar)
  (add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
  ;; Keep TS/TSX source blocks independent from external language packages.
  ;; `js-mode' gives stable highlighting for export without requiring
  ;; `typescript-mode' or Tree-sitter grammars.
  (add-to-list 'org-src-lang-modes '("typescript" . js))
  (add-to-list 'org-src-lang-modes '("ts" . js))
  (add-to-list 'org-src-lang-modes '("tsx" . js))
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
