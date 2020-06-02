(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(define-key evil-normal-state-map (kbd ",cc") 'org-capture)
(define-key evil-normal-state-map (kbd ",cl") 'org-store-link)
(define-key evil-normal-state-map (kbd ",ca") 'org-agenda)
(setq org-directory "~/Dropbox/Documents/org/")
(use-package org
  :ensure org-plus-contrib
  :defer t
  :commands (org-capture org-agenda)
  :after (evil evil-collection)
  :mode ("\\.\\(org\\|orj\\|org_archive\\|txt\\)$" . org-mode)
  :init
  ;; great idea to be explored but isn't what I need in many occasion.
  ;; http://proselint.com/
  ;; TODO: bring it into the writeroom experience instead
  ;; (with-eval-after-load 'flycheck
  ;;   (flycheck-add-mode 'proselint 'org-mode)
    )
  :config
  (evil-collection-define-key 'normal 'org-mode-map
    ",or"   'org-babel-open-src-block-result
    ",ea"   'org-archive-subtree-default
    ",s"    'outline-up-heading
    "à"     'org-refile
    (kbd ", SPC")   'hub/outline-focus-next-section
    ;; chromebook remap alt + <up/down> to <prior/next>
    (kbd "<next>")  'org-move-subtree-down
    (kbd "<prior>") 'org-move-subtree-up
    )
  (evil-define-key 'motion org-mode-map (kbd "RET") 'org-return)
  (evil-define-key 'motion calendar-mode-map (kbd "RET") 'org-calendar-select)
  ;; TODO: delete teh below if above replacement is proven to work well
  ;; (evil-define-key 'normal org-mode-map (kbd ",or") 'org-babel-open-src-block-result)
  ;; (evil-define-key 'normal org-mode-map (kbd ",ea") 'org-archive-subtree-default)
  ;; (evil-define-key 'normal org-mode-map (kbd "<next>") 'org-move-subtree-down)
  ;; (evil-define-key 'normal org-mode-map (kbd "<prior>") 'org-move-subtree-up)
  ;; (evil-define-key 'normal org-mode-map (kbd ", SPC") 'hub/outline-focus-next-section)
  ;; (evil-define-key 'normal org-mode-map (kbd ",s") 'outline-up-heading)
  ;; (evil-define-key 'normal org-mode-map (kbd "à") 'org-refile)
  ;; FIXME: it's not just hiding leading stars, most of the time it's
  ;; all the stars unless I put the cursor on the star that should be
  ;; visible, then it appears at least for a while...
  ;; (setq org-hide-leading-stars t)
  (setq org-startup-indented t)
  (setq org-src-fontify-natively t)
  (setq org-archive-location "archive/%s_archive::datetree/")
  ;; having (setq comment-auto-fill-only-comments t) means org-mode
  ;; doesn't get word-wrapping. Deactivating just for org-mode.
  (add-hook 'org-mode-hook
            (lambda () (setq-local comment-auto-fill-only-comments nil)))

 ;; (require 'org-mac-link)
 ;; (add-hook 'org-mode-hook (lambda ()
 ;;                            (define-key org-mode-map (kbd "C-c m") 'org-mac-grab-link)))

  ;; org-capture && org-agenda
  (setq org-default-notes-file (concat org-directory "inbox.org"))
  ;; org-agenda-files should be a list of files and not a dir
  ;; prefer C-c [ to add and C-c ] to remove file from this list
  ;; (setq org-agenda-files org-directory)
  ;; org-protocol: capture outside of Emacs (mostly from browser)
  ;; (start-server) is managed by edit-server-mode in init.el
  (require 'org-protocol)
  (use-package org-cliplink
    :defer t
    :bind (:map evil-normal-state-map
           (",eP" . org-cliplink)))
  (setq org-capture-templates
        '(
          ("i" "inbox" entry (file org-default-notes-file)
           "* TODO %?")
          ("f" "follow-up" entry (file org-default-notes-file)
           "* TODO %?\n  %i\n  %a")
          ("l" "link" entry (file org-default-notes-file)
           "* TODO %(org-cliplink-capture)" :immediate-finish t)
          ("c" "org-protocol-capture" entry (file org-default-notes-file)
           "* TODO [[%:link][%:description]]\n\n %i" :immediate-finish t)
          ("r" "respond to email (mu4e)"
           entry (file org-default-notes-file)
           "* TODO REPLY to [[mailto:%:fromaddress][%:fromname]] on %a\nDEADLINE: %(org-insert-time-stamp (org-read-date nil t \"+1d\"))\n%U\n\n"
           :immediate-finish t
           :prepend t)
          ("j" "Journal" entry (file+datetree "journal.org")
           "* %?\nEntered on %U\n  %i\n  %a")))
  ;; Note: setup-blog.el also injects a blog template on "b"
  (setq org-outline-path-complete-in-steps nil)         ; Refile in a single go
  (setq org-refile-use-outline-path 'file)                  ; Show full paths for refiling
  (setq org-refile-allow-creating-parent-nodes 'confirm)
  (setq org-refile-targets '((org-agenda-files :maxlevel . 3)))
  ;; consider me idle on my currently clocked-in task after 15 minutes
  ;; and ask me to resolve idle time when I am back
  (setq org-clock-idle-time 15)
  ;; (require 'org-install)
  ;; (require 'org-habit)
  ;; for easy templates to work (e.g. <s[Tab] to create src block)
  (require 'org-tempo)

  ;;; Babel
  ;; org-babel and source code in org
  ;; (setq org-modules
  ;;   (quote
  ;;    (org-bbdb org-bibtex org-docview org-gnus org-info org-irc org-mhe org-rmail org-w3m mac-link)))
  ;; (use-package ob-elixir)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((ditaa . t)(plantuml . t)(ruby . t)(awk . t)(gnuplot . t)(R . t)(latex . t)(java . t)))
  (use-package gnuplot-mode
    :defer t)
  ;; (add-to-list 'org-babel-default-header-args:gnuplot
  ;;              '((:prologue . "reset")))
  (setq org-confirm-babel-evaluate nil)   ; stop asking. May be dangerous...
  ;; (setq org-latex-listings nil)
  (setq org-reveal-root (getenv "REVEAL_JS_ROOT_URL"))
  ;; (load-library "/Users/hbe07/tmp/org-reveal/ox-reveal.el")
  (setq org-plantuml-jar-path "~/install/plantuml.jar")
  (add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
;; (org-babel-do-load-languages 'org-babel-load-languages '((plantuml . t)))

  (defun hub/outline-focus-next-section ()
    (interactive)
    (progn
      (outline-next-heading)
      (outline-show-entry)
      (outline-hide-other)))
  ;; stolen from http://orgmode.org/worg/org-hacks.html
  ;; TODO: compare with elpa package org-wc
  (defun org-word-count (beg end
                             &optional count-latex-macro-args?
                             count-footnotes?)
    "Report the number of words in the Org mode buffer or selected region.
Ignores:
- comments
- tables
- source code blocks (#+BEGIN_SRC ... #+END_SRC, and inline blocks)
- hyperlinks (but does count words in hyperlink descriptions)
- tags, priorities, and TODO keywords in headers
- sections tagged as 'not for export'.

The text of footnote definitions is ignored, unless the optional argument
COUNT-FOOTNOTES? is non-nil.

If the optional argument COUNT-LATEX-MACRO-ARGS? is non-nil, the word count
includes LaTeX macro arguments (the material between {curly braces}).
Otherwise, and by default, every LaTeX macro counts as 1 word regardless
of its arguments."
    (interactive "r")
    (unless mark-active
      (setf beg (point-min)
            end (point-max)))
    (let ((wc 0)
          (latex-macro-regexp "\\\\[A-Za-z]+\\(\\[[^]]*\\]\\|\\){\\([^}]*\\)}"))
      (save-excursion
        (goto-char beg)
        (while (< (point) end)
          (cond
           ;; Ignore comments.
           ((or (org-in-commented-line) (org-at-table-p))
            nil)
           ;; Ignore hyperlinks. But if link has a description, count
           ;; the words within the description.
           ((looking-at org-bracket-link-analytic-regexp)
            (when (match-string-no-properties 5)
              (let ((desc (match-string-no-properties 5)))
                (save-match-data
                  (incf wc (length (remove "" (org-split-string
                                               desc "\\W")))))))
            (goto-char (match-end 0)))
           ((looking-at org-any-link-re)
            (goto-char (match-end 0)))
           ;; Ignore source code blocks.
           ((org-between-regexps-p "^#\\+BEGIN_SRC\\W" "^#\\+END_SRC\\W")
            nil)
           ;; Ignore inline source blocks, counting them as 1 word.
           ((save-excursion
              (backward-char)
              (looking-at org-babel-inline-src-block-regexp))
            (goto-char (match-end 0))
            (setf wc (+ 2 wc)))
           ;; Count latex macros as 1 word, ignoring their arguments.
           ((save-excursion
              (backward-char)
              (looking-at latex-macro-regexp))
            (goto-char (if count-latex-macro-args?
                           (match-beginning 2)
                         (match-end 0)))
            (setf wc (+ 2 wc)))
           ;; Ignore footnotes.
           ((and (not count-footnotes?)
                 (or (org-footnote-at-definition-p)
                     (org-footnote-at-reference-p)))
            nil)
           (t
            (let ((contexts (org-context)))
              (cond
               ;; Ignore tags and TODO keywords, etc.
               ((or (assoc :todo-keyword contexts)
                    (assoc :priority contexts)
                    (assoc :keyword contexts)
                    (assoc :checkbox contexts))
                nil)
               ;; Ignore sections marked with tags that are
               ;; excluded from export.
               ((assoc :tags contexts)
                (if (intersection (org-get-tags-at) org-export-exclude-tags
                                  :test 'equal)
                    (org-forward-same-level 1)
                  nil))
               (t
                (incf wc))))))
          (re-search-forward "\\w+\\W*")))
      (message (format "%d words in %s." wc
                       (if mark-active "region" "buffer")))))

  ;; Presentations
  (use-package org-re-reveal
    :defer t)
  ;;FIXME: this keeps adding the same value vastly accelerating
  ;;entropy in the universe
  ;; (add-hook 'org-mode-hook
  ;;           '(lambda ()
  ;;              (setq org-file-apps
  ;;                    (append '(
  ;;                              ("\\.pptx\\'" . default)
  ;;                              ) org-file-apps ))))
  ;;)

;; Evil and org-mode
;; evil-org see its key bindings here:
;; https://github.com/Somelauw/evil-org-mode/blob/master/doc/keythemes.org#basic
(use-package evil-org
  :pin melpa
  :after (evil org)
  :hook (
         (org-mode . evil-org-mode)
         )
  :config
  (setq evil-org-movement-bindings '((up . "s")
                                     (down . "t")
                                     (left . "c")
                                     (right . "r")))
  ;; 'todo' theme takes over 't' which is 'down' for bepo
  ;; 'heading' theme does something weird with O in normal mode
  (evil-org-set-key-theme '(textobjects return insert navigation additional shift calendar))
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys)

  (defun hub/setup-agenda-keybindings ()
    (evil-collection-translate-key 'motion 'org-agenda-mode-map
      ;; hjkl rotation
      "c" "h"
      "C" "H"
      "t" "j"
      "T" "J"
      "s" "k"
      "S" "K"
      "r" "l"
      "R" "L"
      ;;
      ":" "t"               ; todo, tags
      "H" "T"               ; timer
      "e" "c"               ; config, capture, category, clock, cancel, calendar
      "a" "c"               ; category, clock, cancel, calendar
      "L" "C"               ; convert, capture
      "é" "r"               ; redo, regexp, report
      "É" "R"               ; redo all
      "|" "s"               ; filter
      "K" "S"
      ))
  (add-hook 'org-agenda-mode-hook #'hub/setup-agenda-keybindings)
    )
;; to copy from org-mode while removing line-wrapping but also
;; retaining formatting
(use-package ox-clip
  :config
  (evil-define-key 'visual org-mode-map (kbd ",y") 'ox-clip-formatted-copy))
(provide 'setup-org)
