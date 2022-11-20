(use-package denote
  :config

  (evil-collection-define-key 'normal 'org-mode-map
    ",nn"   #'denote
    ",nN"   #'denote-type
    ",nd"   #'denote-date
    ",ns"   #'denote-subdirectory
    ",nt"   #'denote-template
    ",nl"   #'denote-link
    ",nL"   #'denote-link-add-links
    ",nb"   #'denote-link-backlinks
    ",nf"   #'denote-link-find-file
    ",nB"   #'denote-link-find-backlink
    ",nr"   #'denote-rename-file
    ",nR"   #'denote-rename-file-using-front-matter
    )
  ;; Key bindings specifically for Dired.
  ;; (let ((map dired-mode-map))
  ;;   (define-key map (kbd "C-c C-d C-i") #'denote-link-dired-marked-notes)
  ;;   (define-key map (kbd "C-c C-d C-r") #'denote-dired-rename-marked-files)
  ;;   (define-key map (kbd "C-c C-d C-R") #'denote-dired-rename-marked-files-using-front-matter))

  (setq denote-directory (expand-file-name "~/Dropbox/Documents/org/notes/"))
  (setq denote-known-keywords '("emacs" "faith" "family" "hubert" "pro"))
  (setq denote-infer-keywords t)
  (setq denote-sort-keywords t)
  ;; (setq denote-file-type nil) ; Org is the default, set others here
  (setq denote-prompts '(title keywords))
  (setq denote-excluded-directories-regexp nil)

  ;; Pick dates, where relevant, with Org's advanced interface:
  (setq denote-date-prompt-use-org-read-date t)


  ;; Read this manual for how to specify `denote-templates'.  We do not
  ;; include an example here to avoid potential confusion.


  ;; We allow multi-word keywords by default.  The author's personal
  ;; preference is for single-word keywords for a more rigid workflow.
  (setq denote-allow-multi-word-keywords nil)

  (setq denote-date-format nil) ; read doc string

  ;; By default, we do not show the context of links.  We just display
  ;; file names.  This provides a more informative view.
  (setq denote-backlinks-show-context t)

  ;; Also see `denote-link-backlinks-display-buffer-action' which is a bit
  ;; advanced.

  ;; If you use Markdown or plain text files (Org renders links as buttons
  ;; right away)
  (add-hook 'find-file-hook #'denote-link-buttonize-buffer)

  ; fontify file name fragments in Dired
  ;; We use different ways to specify a path for demo purposes.
  (setq denote-dired-directories
        (list denote-directory
              ;; (thread-last denote-directory (expand-file-name "attachments"))
              ;; (expand-file-name "~/Documents/books")
              ))

  ;; Generic (great if you rename files Denote-style in lots of places):
  ;; (add-hook 'dired-mode-hook #'denote-dired-mode)
  ;;
  ;; OR if only want it in `denote-dired-directories':
  (add-hook 'dired-mode-hook #'denote-dired-mode-in-directories)

  ;; Here is a custom, user-level command from one of the examples we
  ;; showed in this manual.  We define it here and add it to a key binding
  ;; below.
  ;; (defun my-denote-journal ()
  ;;   "Create an entry tagged 'journal', while prompting for a title."
  ;;   (interactive)
  ;;   (denote
  ;;    (denote--title-prompt)
  ;;    '("journal")))

  ;; Denote DOES NOT define any key bindings.  This is for the user to
  ;; decide.  For example:
  (with-eval-after-load 'org-capture
    (setq denote-org-capture-specifiers "%l\n%i\n%?")
    (add-to-list 'org-capture-templates
                 '("n" "New note (with denote.el)" plain
                   (file denote-last-path)
                   #'denote-org-capture
                   :no-save t
                   :immediate-finish nil
                   :kill-buffer t
                   :jump-to-captured t)))

  ;; Also check the commands `denote-link-after-creating',
  ;; `denote-link-or-create'.  You may want to bind them to keys as well.)

  )
