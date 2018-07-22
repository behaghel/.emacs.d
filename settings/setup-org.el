(use-package org
  :ensure org-plus-contrib
  :defer t
  :mode ("\\.\\(org\\|orj\\|org_archive\\|txt\\)$" . org-mode)
  :config
  ;; Evil and org-mode
  ; Makes (setq org-return-follows-link t) work with Evil
  (evil-define-key 'motion org-mode-map (kbd "RET") 'org-return)
  (evil-define-key 'normal org-mode-map (kbd ",or") 'org-babel-open-src-block-result)
  (evil-define-key 'normal org-mode-map (kbd "gp") 'outline-backward-same-level)
  (evil-define-key 'normal org-mode-map (kbd "gn") 'outline-forward-same-level)
  (evil-define-key 'normal org-mode-map (kbd ",r") 'outline-next-heading)
  (evil-define-key 'normal org-mode-map (kbd ",a") 'org-archive-subtree-default)
  (defun hub/outline-focus-next-section ()
    (interactive)
    (progn
      (outline-next-heading)
      (outline-show-entry)
      (outline-hide-other)))
  (evil-define-key 'normal org-mode-map (kbd ", SPC") 'hub/outline-focus-next-section)
  (evil-define-key 'normal org-mode-map (kbd ",s") 'outline-up-heading)
  (setq org-directory "~/Dropbox/Documents/org")
  (setq org-default-notes-file (concat org-directory "/sas.org"))
  (setq org-capture-templates
        '(("t" "Todo" entry (file+headline (concat org-directory "/sas.org") "Tasks")
           "* TODO %?\n  %i\n  %a")
          ("j" "Journal" entry (file+datetree "~/org/journal.org")
           "* %?\nEntered on %U\n  %i\n  %a")))
 (setq org-modules
   (quote
    (org-bbdb org-bibtex org-docview org-gnus org-info org-irc org-mhe org-rmail org-w3m mac-link)))
 ;; (require 'org-mac-link)
 (add-hook 'org-mode-hook (lambda ()
                            (define-key org-mode-map (kbd "C-c m") 'org-mac-grab-link)))
  ;; org-agenda-files should be a list of files and not a dir
  ;; prefer C-c [ to add and C-c ] to remove file from this list
  ;; (setq org-agenda-files org-directory)
  (setq org-hide-leading-stars t)
  (setq org-startup-indented t)
  (setq org-src-fontify-natively t)
  ;; (setq org-latex-listings nil)
  (setq org-reveal-root (getenv "REVEAL_JS_ROOT_URL"))
  ;; (load-library "/Users/hbe07/tmp/org-reveal/ox-reveal.el")
  (setq org-plantuml-jar-path "~/install/plantuml.jar")
  ;; (require 'org-install)
  ;; (require 'org-habit)

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

  ;; (require 'org-ac)
  ;; (org-ac/config-default)
  (autoload 'google-contacts "google-contacts" "Google Contacts." t)
  ;; having (setq comment-auto-fill-only-comments t) means org-mode
  ;; doesn't get word-wrapping. Deactivating just for org-mode.
  (add-hook 'org-mode-hook (lambda () (setq-local comment-auto-fill-only-comments nil)))

  (use-package ox-reveal)
  ;; (use-package ox-ioslide)
  (use-package sublime-themes
    :defer t)
  (use-package htmlize
    :defer t)

  ;;
  (add-hook 'org-mode-hook
            '(lambda ()
               (setq org-file-apps
                     (append '(
                               ("\\.pptx\\'" . default)
                               ) org-file-apps ))))

  ;;; Babel
  (use-package ob-elixir)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((ditaa . t)(plantuml . t)(ruby . t)(awk . t)(gnuplot . t)(R . t)(latex . t)(java . t)))
  (use-package gnuplot-mode)
  ;; (add-to-list 'org-babel-default-header-args:gnuplot
  ;;              '((:prologue . "reset")))
  (setq org-confirm-babel-evaluate nil)   ; stop asking. May be dangerous...

  (global-set-key "\C-cl" 'org-store-link)
  (global-set-key "\C-cc" 'org-capture)
  (global-set-key "\C-ca" 'org-agenda)
  (global-set-key "\C-cb" 'org-iswitchb))

(provide 'setup-org)
