(setq org-directory "~/Dropbox/Documents/org/")
;;(hub/setup-speed-dial)


(use-package org
  :straight (:depth full)              ; otherwise org-version breaks
  ;; :defer t
  :commands (org-capture org-agenda)
  :after (evil evil-collection)
  :mode ("\\.\\(org\\|orj\\|org_archive\\|txt\\)$" . org-mode)
  ;; :init
  ;; great idea to be explored but isn't what I need in many occasion.
  ;; http://proselint.com/
  ;; TODO: bring it into the writeroom experience instead
  ;; (with-eval-after-load 'flycheck
  ;;   (flycheck-add-mode 'proselint 'org-mode)
  :config
  (define-key evil-normal-state-map (kbd ",oc") 'org-capture)
  (define-key evil-normal-state-map (kbd ",ol") 'org-store-link)
  (define-key evil-normal-state-map (kbd ",oa") 'org-agenda)
  (evil-collection-define-key 'normal 'org-mode-map
    ",or"   'org-babel-open-src-block-result
    ",à"   'org-archive-subtree-default
    ",s"    'outline-up-heading
    "à"     'org-refile
    (kbd ", SPC")   'hub/outline-focus-next-section
    ;; chromebook remap alt + <up/down> to <prior/next>
    (kbd "<next>")  'org-move-subtree-down
    (kbd "<prior>") 'org-move-subtree-up
    )
  (evil-define-key 'motion org-mode-map (kbd "RET") 'org-return)
  (evil-define-key 'motion calendar-mode-map (kbd "RET") 'org-calendar-select)
  ;; FIXME: it's not just hiding leading stars, most of the time it's
  ;; all the stars unless I put the cursor on the star that should be
  ;; visible, then it appears at least for a while...
  ;; (setq org-hide-leading-stars t)
  (setq org-startup-indented t)
  (setq org-return-follows-link t)
  (setq org-footnote-auto-adjust t)
  ;;The following setting hides blank lines between headings which keeps folded view nice and compact.
  (setq org-cycle-separator-lines 0)
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
  ; otherwise org-agenda destroys your layout
  (setq org-agenda-window-setup 'other-window)
  (setq org-default-notes-file (concat org-directory "inbox.org"))
  ;; org-agenda-files should be a list of files and not a dir
  (setq org-agenda-files
        (list
         (concat org-directory "typeform.org")
         (concat org-directory "gcal-typeform.org")
         (concat org-directory "gcal-gmail.org")
         ;; (concat org-directory "")
         ))
  ;; org-protocol: capture outside of Emacs (mostly from browser)
  ;; (start-server) is managed by edit-server-mode in init.el
  (require 'org-protocol)
  (setq org-capture-templates
        '(
          ("i" "inbox" entry (file org-default-notes-file)
           "* TODO %?" :prepend t)
          ("f" "follow-up" entry (file org-default-notes-file)
           "* TODO %? %a\n  %i" :prepend t)
          ("r" "respond to email (mu4e)"
           entry (file org-default-notes-file)
           "* TODO REPLY to [[mailto:%:fromaddress][%:fromname]] on %a\nDEADLINE: %(org-insert-time-stamp (org-read-date nil t \"+1d\"))\n%U\n\n"
           :immediate-finish t
           :prepend t)
          ("l" "link" entry (file+headline org-default-notes-file "Browsing")
           "* TODO %(org-cliplink-capture)" :immediate-finish t :prepend t)
          ("c" "org-protocol-capture" entry (file+headline org-default-notes-file "Browsing")
           "* TODO [[%:link][%:description]]\n\n %i" :immediate-finish t :prepend t)
          ("p" "Philosophy" entry (file+datetree (concat org-directory "faith.org"))
           "* %?\nEntered on %U\n  %i\n  %a")
          ("m" "Meeting Minutes" entry (file org-default-notes-file)
           "* Meeting Minutes\n** Present at meeting\n- [X] Peter\n- [ ] Sarah - [X] Lucy\n ** Agenda\n- item 1\n- item 2\n- item 3\n** Notes\n*** Last meeting minutes are approved                              :decision:\n*** Discussion\n**** TODO Topic 1                                      :@Fred:\n**** TODO Topic 2                                    :@Sara:\n**** DONE Topic 2.1                                      :@Lucy:@Ted:\nDEADLINE: <2020-03-01 So>\n**** Another sub-topic                                    :decision:\n* Actions\n#+BEGIN: columnview :id global :match "/TODO|DONE" :format "%ITEM(What) %TAGS(Who) %DEADLINE(When) %TODO(State)"\n#+END:\n\n* Decisions\n#+BEGIN: columnview :id global :match "decision" :format "%ITEM(decisions)"\n#+END:"
           :prepend t))
        )
  ;; Note: setup-blog.el also injects a blog template on "b"
  (setq org-outline-path-complete-in-steps nil)      ; Refile in a single go
  (setq org-refile-use-outline-path 'file)           ; Show full paths for refiling
  (setq org-refile-allow-creating-parent-nodes 'confirm)
  (setq org-reverse-note-order t)       ; refile at the top / prepend
  (setq org-refile-targets '(("typeform.org" :maxlevel . 4)
                             ("faith.org" :maxlevel . 2)
                             ("hubert.org" :maxlevel . 2)
                             ("family.org" :maxlevel . 2)
                             ))
  ;; consider me idle on my currently clocked-in task after 15 minutes
  ;; and ask me to resolve idle time when I am back
  (setq org-clock-idle-time 15)
  ;; for easy templates to work (e.g. <s[Tab] to create src block)
  (require 'org-tempo)

  ;;; Babel
  ;; org-babel and source code in org
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((ditaa . t)(plantuml . t)(ruby . t)(awk . t)(gnuplot . t)(R . t)(latex . t)(java . t)))
  (use-package gnuplot-mode
    :defer t)
  (setq org-confirm-babel-evaluate nil)   ; stop asking. May be dangerous...
  ;; (setq org-latex-listings nil)
  (setq org-latex-compiler "lualatex")
  (setq org-preview-latex-default-process 'dvisvgm)

  (setq org-reveal-root (getenv "REVEAL_JS_ROOT_URL"))
  (setq org-plantuml-jar-path "~/install/plantuml.jar")
  (add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
  ;; (org-babel-do-load-languages 'org-babel-load-languages '((plantuml . t)))
  ;; from https://github.com/gongzhitaao/orgcss
  (setq org-html-htmlize-output-type 'css)
  (setq org-html-head-include-default-style nil)

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
                (if (intersection (org-get-tags) org-export-exclude-tags
                                  :test 'equal)
                    (org-forward-same-level 1)
                  nil))
               (t
                (incf wc))))))
          (re-search-forward "\\w+\\W*")))
      (message (format "%d words in %s." wc
                       (if mark-active "region" "buffer")))))
  )

;; Presentations
(use-package org-re-reveal
  :defer t)

;; Evil and org-mode
;; evil-org see its key bindings here:
;; https://github.com/Somelauw/evil-org-mode/blob/master/doc/keythemes.org#basic
(use-package evil-org
  ;; :pin melpa
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

(use-package org-cliplink
  :defer t
  :after org
  :bind (:map evil-normal-state-map
              (",eP" . org-cliplink)))

;; https://yiufung.net/post/anki-org/
(use-package anki-editor
  :defer 10
  :after org
  :hook (org-capture-after-finalize . anki-editor-reset-cloze-number) ; Reset cloze-number after each capture.
  :config
  (evil-collection-define-key 'visual 'org-mode-map
    ",_"   'anki-editor-cloze-region-dont-incr
    ",-"   'anki-editor-cloze-region-auto-incr
    )
  (evil-collection-define-key 'normal 'org-mode-map
    ",0"   'anki-editor-reset-cloze-number
    ",^"   'anki-editor-push-tree
    )
  (defun anki-editor-cloze-region-auto-incr (&optional arg)
    "Cloze region without hint and increase card number."
    (interactive)
    (anki-editor-cloze-region my-anki-editor-cloze-number "")
    (setq my-anki-editor-cloze-number (1+ my-anki-editor-cloze-number))
    (forward-sexp))
  (defun anki-editor-cloze-region-dont-incr (&optional arg)
    "Cloze region without hint using the previous card number."
    (interactive)
    (anki-editor-cloze-region (1- my-anki-editor-cloze-number) "")
    (forward-sexp))
  (defun anki-editor-reset-cloze-number (&optional arg)
    "Reset cloze number to ARG or 1"
    (interactive)
    (setq my-anki-editor-cloze-number (or arg 1)))
  (defun anki-editor-push-tree ()
    "Push all notes under a tree."
    (interactive)
    (anki-editor-push-notes '(4))
    (anki-editor-reset-cloze-number))
  ;; Initialize
  (anki-editor-reset-cloze-number)
  (add-to-list 'org-capture-templates
               '("a" "Anki basic"
                 entry
                 (file+headline org-default-notes-file "Anki")
                 "* %U   %^g\n:PROPERTIES:\n:ANKI_NOTE_TYPE: Basic\n:ANKI_DECK: from-org\n:END:\n** Front\n%?\n** Back\n%x\n"))
  (add-to-list 'org-capture-templates
               '("A" "Anki cloze"
                 entry
                 (file+headline org-default-notes-file "Anki")
                 "* %U   %^g\n:PROPERTIES:\n:ANKI_NOTE_TYPE: Cloze\n:ANKI_DECK: from-org\n:END:\n** Text\n%x\n** Extra\n"))
  )

(use-package org-superstar
  :hook (org-mode . org-superstar-mode)
  :config
  (setq org-superstar-headline-bullets-list
        '("❋" "◉" "○" "▷" "◇" "➵" "❧"))
  )

(use-package org-download
  ;; :pin melpa
  :defer 10
  :after org
  ;; Drag-and-drop to `dired`
  :hook (dired-mode-hook . org-download-enable)
  :config
  (require 'org-download)
  (evil-collection-define-key 'normal 'org-mode-map
    ",Y"   'org-download-clipboard
    )
  ;; org-download use buffer-local variables. Set it individually in files. Otherwise, put things flatly in misc
  ;; folder.
  (setq-default org-download-method 'attach ;; Screenshots are stored in data/ directory by ID. Easier to manage
                org-download-heading-lvl nil
                org-download-delete-image-after-download t
                )
)

(use-package org-drill
  :after org
  :defer 10
  :config
  (add-to-list 'org-capture-templates
               '("y" "sysadmin drill question"
                  entry
                  (file+headline "~/Dropbox/Documents/org/learning/sysadmin.org" "Drills")
                  "\n\n** %^{Question title}                           :sysadmin:drill:\n\n   %^{Question body} \n\n*** Answer \n\n    #+BEGIN_SRC %^{awk_bash} :results output code :in-file ./text-files/%^{text file}\n      %^{awk_bash program}\n    #+END_SRC")))

(provide 'setup-org)
