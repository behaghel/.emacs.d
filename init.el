;;; emacs --- Hubert's .emacs file         -*- lexical-binding: t -*-
;; Copyright (C) 2013 Hubert Behaghel
;;
;;; Commentary:
;; *** TODO: move it to .emacs.d and split it into modules
;; *** TODO: look into https://github.com/jwiegley/use-package
;; *** TODO: clarify key-bindings (move all of them in their own .el)
;;           - abandon any use of C-c
;;           - have a clear strategy when to use evil keymaps
;;             - , == <leader>
;;             - ,v == anything versioning
;;             - ,o == open
;;             - ,c == compile/check or build task
;;             - ,g == anything find or go to
;;             - ,e == anything execute
;;             - ,t == anything test (few exceptions)
;;             - ,h == anything help
;;             - ,n == new / create
;;             - coding:
;;               - ,.  -> find definition for symbol at point
;;               - ,hh -> go to help for symbol at point
;;               - ,l  -> load buffer (or region or paragraph...) to
;;                        inferior process
;;               - ,b  -> build / compile task
;;               - ,ii  -> inspect type at point
;;               - ,gr -> go to REPL
;;
;;; Code:

(toggle-debug-on-error)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-ghc-show-info t)
 '(company-selection-wrap-around t)
 '(custom-safe-themes
   (quote
    ("613a7c50dbea57860eae686d580f83867582ffdadd63f0f3ebe6a85455ab7706" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "c5a044ba03d43a725bd79700087dea813abcb6beb6be08c7eb3303ed90782482" "3a727bdc09a7a141e58925258b6e873c65ccf393b2240c51553098ca93957723" "756597b162f1be60a12dbd52bab71d40d6a2845a3e3c2584c6573ee9c332a66e" "6a37be365d1d95fad2f4d185e51928c789ef7a4ccf17e7ca13ad63a8bf5b922f" "e26780280b5248eb9b2d02a237d9941956fc94972443b0f7aeec12b5c15db9f3" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "25f330cb050c7e7ec402af1b60243e8185a7837b455af0fa026593d4f48a78b2" default)))
 '(haskell-process-args-cabal-repl (quote ("--ghc-option=-ferror-spans")))
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log t)
 '(haskell-process-suggest-remove-import-lines t)
 '(haskell-process-type (quote cabal-repl))
 '(haskell-tags-on-save t)
 '(package-selected-packages
   (quote
    (rubocop company-tern js-doc ob-elixir elixir-mode alchemist base16-theme monokai-theme moe-theme rvm ox-ioslide speed-type writeroom-mode editorconfig feature-mode twittering-mode dtrt-indent evil-surround clj-refactor diff-hl magit company-ghc ox-reveal ag zencoding-mode zenburn-theme web-beautify sublime-themes solarized-theme smex smartparens smart-mode-line scss-mode robe restclient react-snippets rainbow-mode rainbow-delimiters popwin persp-projectile org-ac org nlinum molokai-theme minitest markdown-mode langtool key-chord json-mode js2-refactor ido-vertical-mode ido-ubiquitous ido-at-point htmlize gnuplot git gist ghc ggtags flycheck-haskell flx-ido expand-region exec-path-from-shell evil-nerd-commenter evil-matchit evil-leader ess ensime dockerfile-mode diminish dash-at-point better-defaults artbollocks-mode anti-zenburn-theme adoc-mode ac-js2 ac-inf-ruby ac-cider 4clojure)))
 '(rainbow-delimiters-max-face-count 1)
 '(safe-local-variable-values
   (quote
    ((js2-concat-multiline-strings . eol)
     (eval flycheck-add-next-checker
           (quote javascript-jshint)
           (quote
            (warning . javascript-eslint)))))))

;; putting it earlier in attempt to make it work.
(setq org-return-follows-link t)
(setq org-footnote-auto-adjust t)

(setq user-mail-address "behaghel@gmail.com")
(setq user-full-name "Hubert Behaghel")
;; (toggle-debug-on-error)

;; (set-face-attribute 'default nil :font "Droid Sans Mono-12")
(when (member "Source Code Pro-12" (font-family-list))
  (set-face-attribute 'default nil :font "Source Code Pro-12"))

;; install
(setq hub-lisp-dir (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path hub-lisp-dir)  ; to include my .el
(require 'package)
(require 'cl)
;; (add-to-list 'package-archives
;;           '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/"))
; required to find melpa-installed package after restart at init time
(package-initialize)

(setq url-http-attempt-keepalives nil)

(defvar ensure-packages '(auto-complete better-defaults cider
  cl-lib dash dash-at-point diminish epl ess evil evil-leader
  evil-matchit evil-nerd-commenter exec-path-from-shell
  expand-region f flx-ido flycheck flycheck-haskell ggtags gh
  gist git gnuplot goto-chg haskell-mode ido-at-point
  ido-ubiquitous ido-vertical-mode inf-ruby js2-mode js2-refactor
  logito markdown-mode multiple-cursors nlinum org org-ac
  ox-reveal pcache persp-projectile pkg-info popwin
  rainbow-delimiters rainbow-mode restclient robe s sbt-mode
  smartparens smex solarized-theme sublime-themes tern
  tern-auto-complete undo-tree yasnippet zenburn-theme
  zencoding-mode)
  "A list of packages to check/install at launch.")

(defun ensure-packages-package-installed-p (p)
  (cond ((package-installed-p p) t)
        (t nil)))

(defun ensure-packages-installed-p ()
  (mapcar 'ensure-packages-package-installed-p ensure-packages))

(defun ensure-packages-install-missing ()
  (interactive)
  (unless (every 'identity (ensure-packages-installed-p))
  ;; check for new packages (package versions)
  (message "%s" "Emacs is now refreshing its package database...")
  (package-refresh-contents)
  (message "%s" " done.")
  ;; install the missing packages
  (dolist (p ensure-packages)
    (when (not (package-installed-p p))
      (package-install p)))))

(ensure-packages-install-missing)

(require 'better-defaults)

; try to stabilize windows and buffers positions
(setq switch-to-buffer-preserve-window-point 'already-displayed)
; stop cluttering my fs with #file.ext#
(setq auto-save-file-name-transforms
      `((".*" ,(concat user-emacs-directory "backups") t)))

;; Look / Theme
;; http://pawelbx.github.io/emacs-theme-gallery/
;;
;; (load-theme 'zenburn t)
(require 'moe-theme)
(moe-dark)
;; http://chriskempson.github.io/base16/#eighties
;; (load-theme 'base16-eighties-dark t)

;; ;; zenburn region face is invisible...
;; (set-face-attribute 'region nil :background "#666")

;; (require 'rainbow-delimiters)
;; (set-face-attribute 'rainbow-delimiters-unmatched-face nil
;;                     :foreground 'unspecified
;;                     :inherit 'error)

;;; Smart Mode Line
(setq sml/theme 'respectful)
(sml/setup)

; Mac

;; Are we on a mac? Thanks @magnars
(setq is-mac (equal system-type 'darwin))

;; fix for Mac OS X PATH in Emacs GUI
(when window-system
  (exec-path-from-shell-initialize))

(setq mac-command-key-is-meta t)
(setq mac-command-modifier 'meta)
(setq mac-option-key-is-meta nil)
(setq mac-option-modifier nil)

(setq ns-use-srgb-colorspace t)

;;;;;;;;;;;;
; General Behaviour
;;;;;;;;;;;;

;; No annoying buffer for completion, compilation, help...
(require 'popwin)
(popwin-mode 1)

(column-number-mode 1)               ; show column number in mode line

;; particularly useful in git repositories to avoid the hassle of
;; manually reloading each buffer when you change branch.
(global-auto-revert-mode t)

;; does to M-x what ido does to C-x C-f
(require 'smex)
(smex-initialize)

(setq tab-always-indent 'complete)      ; tab try to indent, if indented complete
(setq mode-require-final-newline nil)   ; don't add new line at EOF on save
;; General Keybindings
(global-set-key (kbd "C-=") 'align-current)
; let's make something useful with those french keys
(global-set-key (kbd "C-é") 'undo)
(autoload 'er/expand-region "expand-region" "expand-region.el" t)
(global-set-key (kbd "M-r") 'er/expand-region)

(defun save-all ()
  "To be used to automatically save when I leave Emacs."
  (interactive)
  (save-some-buffers t))
(add-hook 'focus-out-hook 'save-all)

(define-key key-translation-map (kbd "<f8> <right>") (kbd "→"))
(define-key key-translation-map (kbd "<f8> i") (kbd "∞"))

;; left cmd + right cmd + csrn in order to jump from window to window
(global-set-key (kbd "M-©") 'evil-window-left)
(global-set-key (kbd "M-®") 'evil-window-right)
(global-set-key (kbd "M-þ") 'evil-window-down)
(global-set-key (kbd "M-ß") 'evil-window-up)
;; stolen from https://github.com/magnars/.emacs.d/blob/master/key-bindings.el
;; Transpose stuff with M-t
(global-unset-key (kbd "M-t")) ;; which used to be transpose-words
(global-set-key (kbd "M-t l") 'transpose-lines)
(global-set-key (kbd "M-t w") 'transpose-words)
(global-set-key (kbd "M-t s") 'transpose-sexps)
(global-set-key (kbd "M-t p") 'transpose-params)

(require 'smartparens-config)
(smartparens-global-mode t)

;; this works great for lisp languages
;; (global-set-key (kbd "C-<right>") 'sp-forward-slurp-sexp)
;; this works better for other languages
(global-set-key (kbd "C-<right>") 'sp-slurp-hybrid-sexp)
(global-set-key (kbd "C-<left>") 'sp-forward-barf-sexp)
(global-set-key (kbd "C-S-<left>") 'sp-backward-slurp-sexp)
(global-set-key (kbd "C-S-<right>") 'sp-backward-barf-sexp)
(global-set-key (kbd "C-<down>") 'sp-down-sexp)
(global-set-key (kbd "C-<up>") 'sp-up-sexp)
(global-set-key (kbd "M-<down>") 'sp-backward-down-sexp)
(global-set-key (kbd "M-<up>") 'sp-backward-up-sexp)
(global-set-key (kbd "M-S-f") 'sp-forward-sexp)
(global-set-key (kbd "M-S-b") 'sp-backward-sexp)


(defun transpose-params ()
  "Presumes that params are in the form (p, p, p) or {p, p, p} or [p, p, p]."
  (interactive)
  (let* ((end-of-first (cond
                        ((looking-at ", ") (point))
                        ((and (looking-back ",") (looking-at " ")) (- (point) 1))
                        ((looking-back ", ") (- (point) 2))
                        (t (error "Place point between params to transpose."))))
         (start-of-first (save-excursion
                           (goto-char end-of-first)
                           (move-backward-out-of-param)
                           (point)))
         (start-of-last (+ end-of-first 2))
         (end-of-last (save-excursion
                        (goto-char start-of-last)
                        (move-forward-out-of-param)
                        (point))))
    (transpose-regions start-of-first end-of-first start-of-last end-of-last)))

(defun move-forward-out-of-param ()
  (while (not (looking-at ")\\|, \\| ?}\\| ?\\]"))
    (cond
     ((point-is-in-string-p) (move-point-forward-out-of-string))
     ((looking-at "(\\|{\\|\\[") (forward-list))
     (t (forward-char)))))

(defun move-backward-out-of-param ()
  (while (not (looking-back "(\\|, \\|{ ?\\|\\[ ?"))
    (cond
     ((point-is-in-string-p) (move-point-backward-out-of-string))
     ((looking-back ")\\|}\\|\\]") (backward-list))
     (t (backward-char)))))

(defun current-quotes-char ()
  (nth 3 (syntax-ppss)))

(defalias 'point-is-in-string-p 'current-quotes-char)

(defun move-point-forward-out-of-string ()
  (while (point-is-in-string-p) (forward-char)))

(defun move-point-backward-out-of-string ()
  (while (point-is-in-string-p) (backward-char)))

;;; ido
(setq ido-create-new-buffer 'always)
(require 'flx-ido)
(flx-ido-mode t)
;; disable ido highlights to see ido-flx ones
(setq ido-use-faces nil)
(setq ido-everywhere t)
(require 'ido-vertical-mode)
(ido-vertical-mode t)
(ido-everywhere t)
;; I mean really everywhere, don't be shy
(require 'ido-ubiquitous)
(ido-ubiquitous-mode 1)
(require 'ido-at-point)
(ido-at-point-mode)

;; Everything I do is within the context of a specific project
(persp-mode t)
(projectile-global-mode)

;; taken from Magnar Sveen
;; Jump to last perspective
(defun custom-persp-last ()
  (interactive)
  (persp-switch (persp-name persp-last)))
;; Easily switch to your last perspective
(defmacro custom-persp (name &rest body)
  `(let ((initialize (not (gethash ,name perspectives-hash)))
         (current-perspective persp-curr))
     (persp-switch ,name)
     (when initialize ,@body)
     (setq persp-last current-perspective)))
(defun custom-persp/emacs ()
  (interactive)
  (custom-persp ".emacs.d"
                (find-file "~/.emacs.d/init.el")))
(defun custom-persp/sas ()
  (interactive)
  (custom-persp "sas"
                (find-file "~/Documents/org/sas.org")))

;; Ediff
(setq ediff-diff-options "-w")
(setq ediff-split-window-function 'split-window-horizontally)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;;; auto-insert-mode (template filling at file creation time)
(add-hook 'find-file-hook 'auto-insert)
(setq auto-insert-directory "~/.emacs.d/insert/")
;; TODO: create template for .org
;; you can use yasnippet to expand it
;; see: http://www.emacswiki.org/emacs/AutoInsertMode
;; the standard emacs way use skeleton
;; see: https://github.com/cinsk/emacs-scripts/blob/8212d714d5c6f6b95e873e8688b30ba130d07775/xskel.el
(defun hub/autoinsert-yas-expand (&optional expand-env)
    "Replace text in yasnippet template optionally passing EXPAND-ENV (let-style)."
    (yas-expand-snippet (buffer-string) (point-min) (point-max) expand-env))
(define-auto-insert "\.org" ["template.org" hub/autoinsert-yas-expand])
;; orj is an extension I invented: org-revealJS
(define-auto-insert "\.orj" ["template.orj" hub/autoinsert-yas-expand])

;;; org-mode
(add-to-list 'auto-mode-alist '("\\.\\(org\\|orj\\|org_archive\\|txt\\)$" . org-mode))
(setq org-directory "~/Documents/org")
(setq org-default-notes-file (concat org-directory "/notes.org"))
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
(add-hook 'org-mode 'auto-fill-mode)
;; stolen from http://www.emacswiki.org/emacs/ArtistMode
;;; integrate ido with artist-mode
(defun artist-ido-select-operation (type)
  "Use ido to select a drawing operation in artist-mode"
  (interactive (list (ido-completing-read "Drawing operation: "
                                          (list "Pen" "Pen Line" "line" "straight line" "rectangle"
                                                "square" "poly-line" "straight poly-line" "ellipse"
                                                "circle" "text see-thru" "text-overwrite" "spray-can"
                                                "erase char" "erase rectangle" "vaporize line" "vaporize lines"
                                                "cut rectangle" "cut square" "copy rectangle" "copy square"
                                                "paste" "flood-fill"))))
  (artist-select-operation type))
(defun artist-ido-select-settings (type)
  "Use ido to select a setting to change in artist-mode"
  (interactive (list (ido-completing-read "Setting: "
                                          (list "Set Fill" "Set Line" "Set Erase" "Spray-size" "Spray-chars"
                                                "Rubber-banding" "Trimming" "Borders"))))
  (if (equal type "Spray-size")
      (artist-select-operation "spray set size")
    (call-interactively (artist-fc-get-fn-from-symbol
                         (cdr (assoc type '(("Set Fill" . set-fill)
                                            ("Set Line" . set-line)
                                            ("Set Erase" . set-erase)
                                            ("Rubber-banding" . rubber-band)
                                            ("Trimming" . trimming)
                                            ("Borders" . borders)
                                            ("Spray-chars" . spray-chars))))))))
(add-hook 'artist-mode-init-hook
          (lambda ()
            (define-key artist-mode-map (kbd "C-c C-a C-o") 'artist-ido-select-operation)
            (define-key artist-mode-map (kbd "C-c C-a C-c") 'artist-ido-select-settings)))
;;; Babel
(org-babel-do-load-languages
 'org-babel-load-languages
 '((ditaa . t)(plantuml . t)(ruby . t)(awk . t)(gnuplot . t)(R . t)(latex . t)))
;; (add-to-list 'org-babel-default-header-args:gnuplot
;;              '((:prologue . "reset")))
(setq org-confirm-babel-evaluate nil)   ; stop asking. May be dangerous...

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ci" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

;; stolen: http://stackoverflow.com/a/6541072/249234
(defun func-region (start end func)
  "Run a function FUNC over the region between START and END in current buffer."
  (save-excursion
    (let ((text (delete-and-extract-region start end)))
      (insert (funcall func text)))))

(defun url-encode-region (start end)
  "Urlencode the region between START and END in current buffer."
  (interactive "r")
  (func-region start end #'url-hexify-string))

(defun url-decode-region (start end)
  "De-urlencode the region between START and END in current buffer."
  (interactive "r")
  (func-region start end #'url-unhex-string))

(defvar blog-posts-dir (expand-file-name (concat (getenv "BLOG_ROOT") "/posts")))

(autoload 'artbollocks-mode "artbollocks-mode" "improve your english style" t)

(defun hub/create-post (title)
  "Start editing a new file whose title is following this pattern
%Y-%m-%d-[title].orp in the posts directory."
  (interactive "sTitle: ")
  (let ((slug (hub/sluggify title))
        (today (format-time-string "%Y-%m-%d"))
        (template (expand-file-name "template.post" auto-insert-directory)))
    (remove-hook 'find-file-hooks 'auto-insert) ; to prevent normal .org insertion
    (find-file (format "%s/%s-%s.org" blog-posts-dir today slug))
    (insert-file-contents template)
    (hub/autoinsert-yas-expand `((title ,title)))
    (add-hook 'find-file-hooks 'auto-insert)
    (ispell-change-dictionary "uk")
    (setq-local ispell-check-comments nil)
    (artbollocks-mode)))

(defun hub/sluggify (title)
  "Transform the TITLE of an article into a slug suitable for an URL."
  (let* ((lc-title (downcase title))
         (no-space (replace-regexp-in-string " +" "-" lc-title))
         (striped (replace-regexp-in-string "[',!?.:/()_;\"<>«»@#]" "" no-space))
         (asciified (xah-asciify-string striped)))
    asciified))

(defun xah-asciify-region (&optional φfrom φto)
  "Change European language characters into equivalent ASCII ones, ⁖ “café” ⇒ “cafe”.

This command does not transcode all Unicode chars such as Greek, math symbols. They remains.

When called interactively, work on text selection or current line.
URL `http://ergoemacs.org/emacs/emacs_zap_gremlins.html'
Version 2014-10-20"
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list (line-beginning-position) (line-end-position))))
  (let ((ξcharMap [
                   ["á\\|à\\|â\\|ä\\|ã\\|å" "a"]
                   ["é\\|è\\|ê\\|ë" "e"]
                   ["í\\|ì\\|î\\|ï" "i"]
                   ["ó\\|ò\\|ô\\|ö\\|õ\\|ø" "o"]
                   ["ú\\|ù\\|û\\|ü"     "u"]
                   ["Ý\\|ý\\|ÿ"     "y"]
                   ["ñ" "n"]
                   ["ç" "c"]
                   ["ð" "d"]
                   ["þ" "th"]
                   ["ß" "ss"]
                   ["æ" "ae"]
                   ]))
    (let ((case-fold-search t))
      (save-restriction
        (narrow-to-region φfrom φto)
        (mapc
         (lambda (ξpair)
           (goto-char (point-min))
           (while (search-forward-regexp (elt ξpair 0) (point-max) t)
             (replace-match (elt ξpair 1))))
         ξcharMap)))))

(defun xah-asciify-string (φstring)
  "Returns a new string. European language chars are changed ot ASCII ones ⁖ “café” ⇒ “cafe”.
See `xah-asciify-region'
Version 2014-10-20"
  (with-temp-buffer
    (insert φstring)
    (xah-asciify-region (point-min) (point-max))
    (buffer-string)))

(defun hub/dwim-other-window (f)
  "Run F in a new window if only one window is visible.
Otherwise switch to other window before."
  (if (one-window-p t 'visible)
      (split-window-right))
  (other-window 1)
  (funcall f))

(defun hub/eshell-other-window ()
  "Open eshell in other window."
  (interactive)
  (hub/dwim-other-window 'eshell))

(defun hub/switch-to-other-buffer ()
  "Switch to topmost non-visible buffer. On default bindings, same as
C-x b RET. The buffer selected is the one returned by (other-buffer)."
        (interactive)
        (switch-to-buffer (other-buffer)))
(defun hub/switch-dwim ()
  "Switch to the previously visited windows if multiple windows
  are visible else switch to other buffer."
  (interactive)
  (if (one-window-p t 'visible) (evil-buffer)
    (evil-window-mru)))
(defun hub/copy-buffer-file-name ()
  "Put the current file name on the clipboard"
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (with-temp-buffer
        (insert filename)
        (clipboard-kill-region (point-min) (point-max)))
      (message filename))))

;;; EVIL
(require 'evil)
(evil-mode 1)
;; surround
;; before my config for my config to win
;; otherwise in visual s call surround where I want to go on previous line
(require 'surround)
(global-surround-mode 1)
(evil-define-key 'visual surround-mode-map "s" nil)
(evil-define-key 'visual surround-mode-map "S" 'surround-region)
(evil-define-key 'visual surround-mode-map (kbd "C-S") 'surround-region)
;;;; Bépo rebinding
(define-key evil-motion-state-map (kbd "é") 'evil-forward-word-begin)
(define-key evil-motion-state-map (kbd "É") 'evil-forward-WORD-begin)
(define-key evil-motion-state-map "c" 'evil-backward-char)
(define-key evil-motion-state-map "S" 'evil-window-top)
(define-key evil-motion-state-map "t" 'evil-next-line)
(define-key evil-motion-state-map "s" 'evil-previous-line)
(define-key evil-motion-state-map "r" 'evil-forward-char)
(define-key evil-motion-state-map "L" 'evil-lookup)
(define-key evil-motion-state-map "l" nil)
(define-key evil-motion-state-map "k" nil)
(define-key evil-motion-state-map "j" nil)
(define-key evil-motion-state-map "T" 'evil-window-bottom)
(define-key evil-motion-state-map "h" 'evil-find-char-to)
(define-key evil-motion-state-map "H" 'evil-find-char-to-backward)
(define-key evil-inner-text-objects-map (kbd "é") 'evil-inner-word)
(define-key evil-inner-text-objects-map (kbd "É") 'evil-inner-WORD)
(define-key evil-outer-text-objects-map (kbd "é") 'evil-a-word)
(define-key evil-outer-text-objects-map (kbd "É") 'evil-a-WORD)
(define-key evil-normal-state-map (kbd "W") 'evil-window-next)
(define-key evil-window-map "h" 'evil-window-set-height)
(define-key evil-window-map "H" 'evil-window-set-width)
(define-key evil-window-map "_" 'split-window-vertically)
(define-key evil-window-map "|" 'split-window-horizontally)
(define-key evil-window-map (kbd "c") 'evil-window-left)
(define-key evil-window-map (kbd "C") 'evil-window-move-far-left)
(define-key evil-window-map (kbd "t") 'evil-window-down)
(define-key evil-window-map (kbd "T") 'evil-window-move-far-bottom)
(define-key evil-window-map (kbd "s") 'evil-window-up)
(define-key evil-window-map (kbd "S") 'evil-window-move-very-top)
(define-key evil-window-map (kbd "r") 'evil-window-right)
(define-key evil-window-map (kbd "R") 'evil-window-move-far-right)
(define-key evil-window-map (kbd "x") 'delete-window)
(define-key evil-window-map (kbd "T") 'evil-window-bottom-right)
(define-key evil-window-map (kbd "S") 'evil-window-top-left)
(define-key evil-normal-state-map (kbd "C-à") 'evil-prev-buffer)
(define-key evil-normal-state-map (kbd "c") 'evil-backward-char)
(define-key evil-normal-state-map (kbd "r") 'evil-forward-char)
(define-key evil-normal-state-map (kbd "t") 'evil-next-line)
(define-key evil-normal-state-map (kbd "s") 'evil-previous-line)
(define-key evil-normal-state-map (kbd "C") 'evil-window-top)
(define-key evil-normal-state-map (kbd "R") 'evil-window-low)
(define-key evil-normal-state-map (kbd "T") 'evil-join)
;; following translation only occur when z is a prefix (not already bound)
(define-key key-translation-map (kbd "zs") (kbd "zj"))
(define-key key-translation-map (kbd "zc") (kbd "zh"))
(define-key key-translation-map (kbd "zr") (kbd "zl"))
(define-key key-translation-map (kbd "zC") (kbd "zH"))
(define-key key-translation-map (kbd "zR") (kbd "zL"))
(define-key key-translation-map (kbd "zt") (kbd "zk"))
(define-key evil-normal-state-map (kbd "zS") 'evil-scroll-line-to-top)
(define-key evil-normal-state-map (kbd "zT") 'evil-scroll-line-to-bottom)
(define-key evil-normal-state-map (kbd "zf") 'evil-close-fold)
(define-key evil-normal-state-map (kbd "zx") 'evil-close-fold)
(define-key evil-normal-state-map (kbd "zX") 'evil-close-folds)
(define-key evil-normal-state-map (kbd "zF") 'evil-close-folds)
(define-key evil-normal-state-map (kbd "zO") 'evil-open-folds)
(define-key evil-normal-state-map (kbd "j") 'evil-replace)
(define-key evil-normal-state-map (kbd "l") 'evil-change)
(define-key evil-normal-state-map (kbd "L") 'evil-change-line)
(define-key evil-normal-state-map (kbd "h") 'evil-find-char-to)
(define-key evil-normal-state-map (kbd "H") 'evil-find-char-to-backward)
(define-key evil-normal-state-map (kbd "k") 'evil-substitute)
(define-key evil-normal-state-map (kbd "K") 'evil-change-whole-line)
(define-key evil-normal-state-map (kbd "©") 'backward-sexp)
(define-key evil-normal-state-map (kbd "®") 'forward-sexp)

;;;; Other mapping
(define-key evil-normal-state-map (kbd "C-e") 'evil-end-of-line)
(define-key evil-insert-state-map (kbd "C-e") 'end-of-line)
(define-key evil-insert-state-map (kbd "C-y") 'yank)
(define-key evil-insert-state-map (kbd "M-y") 'yank-pop)
(define-key evil-normal-state-map (kbd ",y") 'hub/copy-buffer-file-name)
(define-key evil-normal-state-map (kbd ",x") 'smex)
(define-key evil-normal-state-map (kbd ",gs") 'eshell)
(define-key evil-normal-state-map (kbd ",gS") 'hub/eshell-other-window)
(define-key evil-normal-state-map (kbd ",g/") 'dired)
(define-key evil-normal-state-map (kbd ",,") 'hub/switch-dwim)
(define-key evil-normal-state-map (kbd ",|") 'shell-command-on-region)
;; stolen from http://www.emacswiki.org/emacs/Evil#toc12
;; Note: lexical-binding must be t in order for this to work correctly.
(defun make-conditional-key-translation (key-from key-to translate-keys-p)
  "Make a Key Translation such that if the translate-keys-p function returns true,
   key-from translates to key-to, else key-from translates to itself.  translate-keys-p
   takes key-from as an argument."
  (define-key key-translation-map key-from
    (lambda (prompt)
      (if (funcall translate-keys-p key-from) key-to key-from))))
(defun not-insert-state-p (key-from)
  "Returns whether conditional key translations should be active.  See make-conditional-key-translation function. "
  (and
   ;; Only allow a non identity translation if we're beginning a Key Sequence.
   (equal key-from (this-command-keys))
   (or (evil-motion-state-p) (evil-normal-state-p) (evil-visual-state-p))))

(make-conditional-key-translation (kbd "è") (kbd "C-x") 'not-insert-state-p)
(make-conditional-key-translation (kbd "È") (kbd "C-u") 'not-insert-state-p)

;; you want to *g*o somewhere
(define-key evil-normal-state-map (kbd ",gg") 'hub/switch-to-other-buffer)
;;; Switch to another open buffer
(define-key evil-normal-state-map (kbd ",gb") 'switch-to-buffer)
(define-key evil-normal-state-map (kbd ",gB") 'switch-to-buffer-other-window)
;; you want to *o*pen something
;;; Open file
(define-key evil-normal-state-map (kbd ",of") 'ido-find-file)
;; Browse URL
(define-key evil-normal-state-map (kbd ",ou") 'browse-url)
; open init.el
(define-key evil-normal-state-map (kbd ",oe") 'custom-persp/emacs)
; open hubert.org
(define-key evil-normal-state-map (kbd ",oh") (lambda()(interactive)(find-file "~/Dropbox/Documents/org/hubert.org")))
(define-key evil-normal-state-map (kbd ",os") 'custom-persp/sas)
;; open file in project
(define-key evil-normal-state-map (kbd ",pf") 'projectile-find-file)
(define-key evil-normal-state-map (kbd ",pF") 'projectile-find-file-other-window)
(define-key evil-normal-state-map (kbd ",pT") 'projectile-regenerate-tags)
(define-key evil-normal-state-map (kbd ",p.") 'projectile-find-tag)
;; switch from code file to test file and vice-versa
(define-key evil-normal-state-map (kbd  ",gt") 'projectile-toggle-between-implementation-and-test)
;; FIXME: when active, break projectile-find-file
(require 'persp-projectile)
(define-key evil-normal-state-map (kbd ",ps") 'projectile-persp-switch-project)
(define-key evil-normal-state-map (kbd ",op") 'projectile-persp-switch-project)
(define-key evil-normal-state-map (kbd ",pp") 'custom-persp-last)
(define-key evil-normal-state-map (kbd ",gp") 'persp-switch)
(define-key evil-normal-state-map (kbd ",pk") 'persp-remove-buffer) ; disassociate buffer from persp
(define-key evil-normal-state-map (kbd ",pr") 'persp-rename)
(define-key evil-normal-state-map (kbd ",px") 'persp-kill) ; terminate perspective
(define-key evil-normal-state-map (kbd ",pa") 'persp-add-buffer) ; associate buffer to current persp
(define-key evil-normal-state-map (kbd ",pA") 'persp-set-buffer) ; like add but remove from all other
;; Evil and org-mode
; Makes (setq org-return-follows-link t) work with Evil
(evil-define-key 'motion org-mode-map (kbd "RET") 'org-return)
(evil-define-key 'normal org-mode-map (kbd ",or") 'org-babel-open-src-block-result)
(evil-define-key 'normal org-mode-map (kbd ",s") 'outline-backward-same-level)
(evil-define-key 'normal org-mode-map (kbd ",t") 'outline-forward-same-level)
(evil-define-key 'normal org-mode-map (kbd ",T") 'outline-up-heading)
(define-key evil-normal-state-map (kbd ",nb") 'hub/create-post)
(require 'evil-matchit)
(global-evil-matchit-mode 1)

;;; Dired
(require 'dired-x)
(evil-define-key 'normal dired-mode-map "t" 'dired-next-line)
(evil-define-key 'normal dired-mode-map ",t" 'dired-next-dirline)
(evil-define-key 'normal dired-mode-map "s" 'dired-previous-line)
(evil-define-key 'normal dired-mode-map ",s" 'dired-previous-dirline)
(evil-define-key 'normal dired-mode-map ",S" 'dired-sort-toggle)
(evil-define-key 'normal dired-mode-map ",m" 'dired-toggle-marks)
;; (evil-define-key 'normal dired-mode-map ",c" 'dired-copy-file)
;; (evil-define-key 'normal dired-mode-map ",r" 'dired-rename-file)



;; errors and *c*ompilation
(define-key evil-normal-state-map (kbd "]e") 'next-error)
(define-key evil-normal-state-map (kbd "[e") 'previous-error)
(define-key evil-normal-state-map (kbd ",)") 'flycheck-next-error)
(define-key evil-normal-state-map (kbd ",(") 'flycheck-previous-error)
(define-key evil-normal-state-map (kbd ",cc") 'compile)
(define-key evil-normal-state-map (kbd ",cr") 'recompile)
(define-key evil-normal-state-map (kbd ",ck") 'kill-compilation)
(define-key evil-normal-state-map (kbd ",cw") 'artbollocks-count-words)
(define-key evil-normal-state-map (kbd ",cw") 'ispell-buffer)
(define-key evil-normal-state-map (kbd ",cg") 'langtool-check)
;; evil is crazy
(define-key evil-insert-state-map (kbd "C-d") nil)
(define-key evil-normal-state-map (kbd "M-.") nil)
;;;; Default state
(evil-set-initial-state 'help-mode 'emacs)
;; (evil-set-initial-state 'dired-mode 'emacs)
;; (evil-set-initial-state 'fundamental-mode 'emacs)
(evil-set-initial-state 'ess-help-mode 'emacs)
(evil-set-initial-state 'ensime-scalex-mode 'emacs)
(evil-set-initial-state 'erc-mode 'emacs)
(evil-set-initial-state 'image-mode 'emacs)
(evil-set-initial-state 'cider-stacktrace-mode 'emacs)
;;; Info & Evil
(evil-set-initial-state 'Info 'emacs)
(evil-define-key 'motion Info-mode-map "l" nil) ; use l to say last

;; (require 'powerline-evil)
;; (powerline-evil-center-color-theme)

;;; Comint
(setq
 comint-scroll-to-bottom-on-input t
 comint-scroll-to-bottom-on-output t
 comint-show-maximum-output t
 comint-input-ignoredups t
 comint-completion-addsuffix t
 comint-buffer-maximum-size 10000
 )
;; truncate buffers continuously
(add-hook 'comint-output-filter-functions 'comint-truncate-buffer)
(evil-define-key 'normal comint-mode-map ",ee" 'comint-clear-buffer)
(evil-define-key 'insert comint-mode-map (kbd "C-c C-e") 'comint-clear-buffer)
;; ESS config
(eval-after-load "comint"
  '(progn
     (define-key comint-mode-map [up]
       'comint-previous-matching-input-from-input)
     (define-key comint-mode-map [down]
       'comint-next-matching-input-from-input)

     ;; also recommended for ESS use --
     (setq comint-scroll-to-bottom-on-output 'others)
     (setq comint-scroll-show-maximum-output t)
     ;; somewhat extreme, almost disabling writing in *R*, *shell* buffers above prompt:
     (setq comint-scroll-to-bottom-on-input 'this)
     ))

(defun hub/load-term-theme-locally ()
  "Load the color theme I want to use for term into the current buffer."
  (load-theme-buffer-local 'tango-dark (current-buffer)))

;; Emacs and the shell
;; currently my zsh setup fails when used from Emacs with
;; complete:13: command not found: compdef
(setq shell-file-name "bash")

;; eshell
;; (add-hook 'eshell-mode-hook 'hub/load-term-theme-locally)
(eval-after-load 'esh-opt
  '(progn
     (require 'em-term)
     (add-to-list 'eshell-visual-commands "sbt")
     (add-to-list 'eshell-visual-commands "vimdiff")
     (require 'em-cmpl)
     (add-to-list 'eshell-command-completions-alist
                  '("gunzip" "gz\\'"))
     (add-to-list 'eshell-command-completions-alist
                  '("tar" "\\(\\.tar|\\.tgz\\|\\.tar\\.gz\\)\\'"))
     ;; cycling completion doesn't work for me
     ;; it complete with one and you
     (setq eshell-cmpl-cycle-completions t)
     ;; (require 'em-smart)
     ;; (setq eshell-where-to-jump 'begin)
     ;; (setq eshell-review-quick-commands nil)
     ;; (setq eshell-smart-space-goes-to-end t)

     ;; stolen from http://blog.liangzan.net/blog/2012/12/12/customizing-your-emacs-eshell-prompt/
     (defun eshell/ef (fname-regexp &rest dir) (ef fname-regexp default-directory))
;;; ---- path manipulation
     (defun pwd-repl-home (pwd)
       "detects when pwd includes HOME and substitutes this part with '~'"
       (interactive)
       (let* ((home (expand-file-name (getenv "HOME")))
              (home-len (length home)))
         (if (and
              (>= (length pwd) home-len)
              (equal home (substring pwd 0 home-len)))
             (concat "~" (substring pwd home-len))
           pwd)))
     (defun curr-dir-git-branch-string (pwd)
       "Returns current git branch as a string, or the empty string if
PWD is not in a git repo (or the git command is not found)."
       (interactive)
       (when (and (eshell-search-path "git")
                  (locate-dominating-file pwd ".git"))
         (let ((git-output
                (shell-command-to-string
                 (concat "cd " pwd " && git branch | grep '\\*' | sed -e 's/^\\* //'"))))
           (propertize (concat "["
                               (if (> (length git-output) 0)
                                   (substring git-output 0 -1)
                                 "(no branch)")
                               "]") 'face `(:foreground "tomato1"))
           )))

     (require 'em-hist)           ; So the history vars are defined
     (setq eshell-history-size 1024)
     (if (boundp 'eshell-save-history-on-exit)
         (setq eshell-save-history-on-exit t)) ; Don't ask, just save
     (if (boundp 'eshell-ask-to-save-history)
         (setq eshell-ask-to-save-history 'always)) ; For older(?) version
     (setq eshell-prompt-regexp "^[^%#$]*[%#$] ")
     (setq eshell-prompt-function
           (lambda ()
             (concat
              (propertize ((lambda (p-lst)
                             (if (> (length p-lst) 3)
                                 (concat
                                  (mapconcat
                                   (lambda (elm) (if (zerop (length elm)) ""
                                                   (substring elm 0 1)))
                                   (butlast p-lst 3)
                                   "/")
                                  "/"
                                  (mapconcat (lambda (elm) elm)
                                             (last p-lst 3)
                                             "/"))
                               (mapconcat (lambda (elm) elm)
                                          p-lst
                                          "/")))
                           (split-string
                            (pwd-repl-home (eshell/pwd)) "/")) 'face '(:foreground "DarkOrange1"))
              (curr-dir-git-branch-string (eshell/pwd))
              (propertize " % " 'face 'default))))
     ;; don't enforce theme colors since it will make the prompt monochrome
     ;; there is only one face for the whole prompt.
     (setq eshell-highlight-prompt nil)
     ;; end of stealing

     (require 'eshell-autojump)))

(defun eshell-run-last ()
  "Relaunch without moving point 'cause this will work now."
  (interactive)
  (with-current-buffer (get-buffer "*eshell*")
    (hub/eshell-other-window)
    (insert-and-inherit (eshell-get-history 0))
    (eshell-send-input)))

;;; TRAMP
(setq tramp-default-method "ssh")

;;; ERC
;;; start with M-x erc-select
;;; switch channel by switching emacs buffer
;;; switch to last active channel C-c C-SPC
(autoload 'erc-select "erc" "IRC client." t)
(load "~/.emacs.d/.ercpass")
(setq erc-echo-notices-in-minibuffer-flag t)
(eval-after-load 'erc
  '(progn
    (require 'tls)
    (setq erc-modules '(autojoin button completion fill
                                 irccontrols list match menu
                                 move-to-prompt netsplit networks
                                 noncommands
                                 readonly ring scrolltobottom
                                 services stamp track))
    (setq erc-prompt-for-password 1)  ; use authinfo instead
    (setq erc-prompt-for-nickserv-password nil)
    (setq erc-fill-function 'erc-fill-static)
    (setq erc-fill-static-center 18)    ; margin for ts + nicks
    (setq erc-timestamp-format "[%H:%M] "
          erc-insert-timestamp-function 'erc-insert-timestamp-left)
    (erc-scroll-to-bottom)
    (setq erc-input-line-position -1)
    (setq erc-nickserv-passwords
          `((freenode (("behaghel" . ,freenode-passwd-behaghel)
                       ("larch" . ,freenode-passwd-behaghel)))
            (skyglobal (("hubert" . ,slack-global-passwd-hubert)))))
    (require 'erc-match)
    (setq erc-keywords '("\\bhub\\b" "behaghel" "hubert" "hubertb"))
    (setq erc-pals '("aloiscochard"))
    (setq erc-autojoin-mode t)
    (setq erc-autojoin-channels-alist
          '((".*\\.freenode.net" "#scala" "#scalaz" "#shapeless")
            ("irc.amazon.com" "#ingestion" "#reconciliation")))
    (setq erc-auto-query 'bury)
    (setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
                                    "324" "329" "332" "333" "353" "477"))
    (setq erc-lurker-hide-list '("JOIN" "NICK" "PART" "QUIT" "MODE"))
    ;; (setq erc-echo-notices-in-minibuffer-flag t)
    ;; (erc-minibuffer-notice t)
    (setq erc-lurker-threshold-time 3600)
))

;; Twitter
(setq twittering-use-master-password t)
(setq twittering-icon-mode t)
(setq twittering-use-icon-storage t)
(setq twittering-timer-interval 90)
(setq twittering-relative-retrieval-interval-alist
      '(("\\`:direct.*\\'" 4)    ; 360 seconds
        (":home" ":mentions" 1)  ; 90 seconds
        (t 30)))                 ; anything else in 15 minutes
(setq twittering-status-format
      "%i %s,  %@:  %FACE[shadow]{%R}
%FILL[  ]{%T %FACE[shadow]{// from %f%L%r}}
 ")
(setq twittering-initial-timeline-spec-string
      '(":home"
        ":mentions"
        ":direct_messages"
        ":favorites"
        "behaghel/scala"
        "behaghel/catholic"
        "behaghel/cycling"
        ))
(evil-define-key 'normal twittering-mode-map ",," 'twittering-enter)
(evil-define-key 'normal twittering-mode-map (kbd "C-m") 'twittering-enter)
(evil-define-key 'normal twittering-mode-map (kbd  "C-i") 'twittering-goto-next-thing)
(evil-define-key 'normal twittering-mode-map (kbd "M-C-i") 'twittering-goto-previous-thing)
(evil-define-key 'normal twittering-mode-map ",gd" 'twittering-direct-messages-timeline)
(evil-define-key 'normal twittering-mode-map ",gr" 'twittering-replies-timeline)
(evil-define-key 'normal twittering-mode-map ",R" 'twittering-retweet)
(evil-define-key 'normal twittering-mode-map ",r" 'twittering-native-retweet)
(evil-define-key 'normal twittering-mode-map ",d" 'twittering-direct-message)
(evil-define-key 'normal twittering-mode-map ",n" 'twittering-update-status-interactive)
(evil-define-key 'normal twittering-mode-map ")" 'twittering-switch-to-next-timeline)
(evil-define-key 'normal twittering-mode-map ",N" 'twittering-switch-to-next-timeline)
(evil-define-key 'normal twittering-mode-map "(" 'twittering-switch-to-next-timeline)
(evil-define-key 'normal twittering-mode-map "t" 'twittering-goto-next-status)
(evil-define-key 'normal twittering-mode-map "s" 'twittering-goto-previous-status)
(defun custom-persp/social ()
  (interactive)
  (custom-persp "social"
                (twit)
                (switch-to-buffer ":home")))

(define-key evil-normal-state-map (kbd ",ot") 'custom-persp/social)


;;; Git
;; magit
;; (autoload 'magit-status "magit" nil t)
;; (global-set-key (kbd "C-c g s") 'magit-status)
;; (global-set-key (kbd "C-c g ?") 'magit-blame-mode)
;; (global-set-key (kbd "C-c g /") 'vc-git-grep)
(defadvice Info-follow-nearest-node (around gitman activate)
  "When encountering a cross reference to the `gitman' info
manual, then instead of following that cross reference show
the actual manpage using the function `man'."
  (let ((node (Info-get-token
               (point) "\\*note[ \n\t]+"
               "\\*note[ \n\t]+\\([^:]*\\):\\(:\\|[ \n\t]*(\\)?")))
    (if (and node (string-match "^(gitman)\\(.+\\)" node))
        (progn (require 'man)
               (man (match-string 1 node)))
      ad-do-it)))
;; git-gutter
;; (if (display-graphic-p)
;;     (progn
;;       (require 'git-gutter-fringe+))
;;   (require 'git-gutter+))
;; (global-git-gutter+-mode t)
;; (global-set-key (kbd "C-c g f") 'git-gutter+-mode) ; turn on/off git-gutter+ in the current buffer
;; diff-hl
(global-diff-hl-mode)
(eval-after-load 'git-gutter+
  '(progn
     ;;; Jump between hunks
     (define-key git-gutter+-mode-map (kbd "C-c f n") 'git-gutter+-next-hunk)
     (define-key git-gutter+-mode-map (kbd "C-c f p") 'git-gutter+-previous-hunk)

     ;;; Act on hunks
     (define-key git-gutter+-mode-map (kbd "C-c f =") 'git-gutter+-show-hunk)
     (define-key git-gutter+-mode-map (kbd "C-c f r") 'git-gutter+-revert-hunks)
     ;; Stage hunk at point.
     ;; If region is active, stage all hunk lines within the region.
     (define-key git-gutter+-mode-map (kbd "C-c f s") 'git-gutter+-stage-hunks)
     (define-key git-gutter+-mode-map (kbd "C-c f c") 'git-gutter+-commit)
     (define-key git-gutter+-mode-map (kbd "C-c f C") 'git-gutter+-stage-and-commit)))
;; Git tools
;; REQUIRES Magit
(define-key evil-normal-state-map (kbd ",vs") 'magit-status) ;; git control panel
(define-key evil-normal-state-map (kbd ",vh") 'magit-file-popup) ; Commit history for current file
(define-key evil-normal-state-map (kbd ",vf") 'magit-file-popup) ; Commit history for current file
(define-key evil-normal-state-map (kbd ",vb") 'magit-blame-popup) ; Blame for current file
(define-key evil-normal-state-map (kbd ",vB") 'vc-annotate) ; Git blame with vc
(define-key evil-normal-state-map (kbd ",vg") 'vc-git-grep) ; Git grep
(define-key evil-normal-state-map (kbd ",v/") 'vc-git-grep) ; Git grep
(define-key evil-normal-state-map (kbd ",vD") 'ediff-revision) ; Git diff file on 2 branches
;; (define-key evil-normal-state-map (kbd ",vr") 'git-gutter+-revert-hunk)
;; (define-key evil-normal-state-map (kbd ",v+") 'git-gutter+-stage-hunks)
;; (define-key evil-normal-state-map (kbd ",vn") 'git-gutter+-next-hunk)
;; (define-key evil-normal-state-map (kbd ",vp") 'git-gutter+-previous-hunk)
;; (define-key evil-normal-state-map (kbd ",vd") 'git-gutter+-show-hunk)
(define-key evil-normal-state-map (kbd ",vr") 'diff-hl-revert-hunk)
(define-key evil-normal-state-map (kbd ",vn") 'diff-hl-next-hunk)
(define-key evil-normal-state-map (kbd ",vp") 'diff-hl-previous-hunk)
(define-key evil-normal-state-map (kbd ",vd") 'diff-hl-goto-hunk)

;; multiple-cursors
(require 'multiple-cursors)
(global-set-key (kbd "<M-C-down>") 'mc/mark-next-like-this)
(global-set-key (kbd "<M-C-up>") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c m @") 'mc/edit-lines)
(global-set-key (kbd "C-c m a") 'mc/mark-all-like-this-dwim)
;; compat with evil. Stolen https://github.com/jcpetkovich/.emacs.d/blob/master/global-key-bindings.el#L257
;; alternative: https://github.com/magnars/multiple-cursors.el/issues/19
;; waiting for resolution of:
;; https://github.com/magnars/multiple-cursors.el/issues/17
(defvar mc-evil-compat/evil-prev-state nil)
(defvar mc-evil-compat/mark-was-active nil)
(defun mc-evil-compat/switch-to-emacs-state ()
  (when (and (bound-and-true-p evil-mode)
              (not (memq evil-state '(insert emacs))))
    (setq mc-evil-compat/evil-prev-state evil-state)
    (when (region-active-p)
      (setq mc-evil-compat/mark-was-active t))
    (let ((mark-before (mark))
          (point-before (point)))
      (evil-emacs-state 1)
      (when (or mc-evil-compat/mark-was-active (region-active-p))
        (goto-char point-before)
        (set-mark mark-before)))))
(defun mc-evil-compat/back-to-previous-state ()
  (when mc-evil-compat/evil-prev-state
    (unwind-protect
        (case mc-evil-compat/evil-prev-state
          ((normal visual) (evil-force-normal-state))
          (t (message "Don't know how to handle previous state: %S"
                      mc-evil-compat/evil-prev-state)))
      (setq mc-evil-compat/evil-prev-state nil)
      (setq mc-evil-compat/mark-was-active nil))))
(add-hook 'multiple-cursors-mode-enabled-hook
          'mc-evil-compat/switch-to-emacs-state)
(add-hook 'multiple-cursors-mode-disabled-hook
          'mc-evil-compat/back-to-previous-state)
(defun mc-evil-compat/rectangular-switch-state ()
  (if rectangular-region-mode
      (mc-evil-compat/switch-to-emacs-state)
    (setq mc-evil-compat/evil-prev-state nil)))
;; When running edit-lines, point will return (position + 1) as a
;; result of how evil deals with regions
(defadvice mc/edit-lines (before change-point-by-1 activate)
  (when (and (bound-and-true-p evil-mode)
              (not (memq evil-state '(insert emacs))))
    (if (> (point) (mark))
        (goto-char (1- (point)))
      (push-mark (1- (mark))))))
(add-hook 'rectangular-region-mode-hook 'mc-evil-compat/rectangular-switch-state)
(defvar mc--default-cmds-to-run-once nil)

;; Editing text
(add-hook 'text-mode-hook 'turn-on-auto-fill) ; auto-wrap
(setq sentence-end-double-space nil)    ; one space is enough after a period to end a sentence

;; Writing with style
;; http://rs.io/software-writers-tools-improve-writing/
;; chase weasel words, count words and more
;; https://github.com/sachac/artbollocks-mode
;; http://sachachua.com/blog/2011/12/emacs-artbollocks-mode-el-and-writing-more-clearly/
(autoload 'artbollocks-mode "artbollocks-mode")
(setq langtool-language-tool-jar "/usr/local/Cellar/languagetool/2.6/libexec/languagetool.jar")


;; asciidoc
(add-hook 'adoc-mode-hook (lambda() (buffer-face-mode t)))
(add-to-list 'auto-mode-alist (cons "\\.asciidoc\\'" 'adoc-mode))

;; Markdown
(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(evil-define-key 'normal markdown-mode-map (kbd ",il") 'markdown-insert-link)
(evil-define-key 'normal markdown-mode-map (kbd ",iH") 'markdown-insert-header-dwim)
(evil-define-key 'normal markdown-mode-map (kbd ",ih") 'markdown-insert-header-setext-dwim)
(evil-define-key 'normal markdown-mode-map (kbd ",i2") 'markdown-insert-header-setext-2)
(evil-define-key 'normal markdown-mode-map (kbd ",i1") 'markdown-insert-header-setext-1)
(evil-define-key 'normal markdown-mode-map (kbd ",ev") 'markdown-preview)
(evil-define-key 'normal markdown-mode-map (kbd ",eV") 'markdown-export-and-preview)

; coding
;; keys
;; stolen here: http://www.emacswiki.org/emacs/CommentingCode
;; Original idea from
;; http://www.opensubscriber.com/message/emacs-devel@gnu.org/10971693.html
(defun comment-dwim-line (&optional arg)
  "Replacement for the 'comment-dwim' command.
If no region is selected and current line is not blank
and we are not at the end of the line, then comment
current line. Replaces default behaviour of 'comment-dwim',
when it inserts comment at the end of the line."
  (interactive "*P")
  (comment-normalize-vars)
  (if (and (not (region-active-p)) (not (looking-at "[ \t]*$")))
      (comment-or-uncomment-region (line-beginning-position) (line-end-position))
    (comment-dwim arg)))
(global-set-key "\M-;" 'comment-dwim-line)

;; RET insert newline and indent + comment if required
;; Not suitable for multi-line comments à la javadoc
(defun hub/set-newline-and-indent-comment ()
  "Bind RET locally to 'comment-indent-new-line'."
  (interactive)
  (local-set-key (kbd "RET") 'comment-indent-new-line))


;; Behaviours
; automatically indent yanked text in prog-modes
(dolist (command '(yank yank-pop))
  (eval `(defadvice ,command (after indent-region activate)
           (and (not current-prefix-arg)
                (derived-mode-p 'prog-mode)
                (let ((mark-even-if-inactive transient-mark-mode))
                  (indent-region (region-beginning) (region-end) nil))))))

;; Visual
; stop cluttering my modeline with so many minor modes
(eval-after-load 'yasnippet '(diminish 'yas-minor-mode))
(eval-after-load 'undo-tree '(diminish 'undo-tree-mode))
(eval-after-load 'projectile '(diminish 'projectile-mode))
(eval-after-load 'whitespace '(diminish 'whitespace-mode))
;; this one seems hard to diminish: insisting
(eval-after-load 'auto-fill-mode '(diminish 'auto-fill-function))
(eval-after-load 'auto-fill '(diminish 'auto-fill-function))
(diminish 'auto-fill-function)

(eval-after-load 'ggtags '(diminish 'ggtags-mode))
(eval-after-load 'eldoc '(diminish 'eldoc-mode))
(eval-after-load 'git-gutter+ '(diminish 'git-gutter+-mode))
(eval-after-load 'js-mode '(diminish 'js-mode "js"))
(eval-after-load 'auto-complete '(diminish 'auto-complete-mode))
;; (eval-after-load 'dtrt-indent '(diminish 'dtrt-indent-mode))

;; Syntax
;;; stolen here: http://emacsredux.com/blog/2013/07/24/highlight-comment-annotations/
(defun font-lock-comment-annotations ()
  "Highlight a bunch of well known comment annotations.

This functions should be added to the hooks of major modes for programming."
  (font-lock-add-keywords
   nil '(("\\<XXX\\>" 0 'font-lock-warning-face t)
         ("\\<\\(FIX\\(ME\\)?\\|TODO\\|OPTIMIZE\\|HACK\\|REFACTOR\\):"
          1 'font-lock-warning-face t))))
;; XXX
(add-hook 'prog-mode-hook 'font-lock-comment-annotations)

;; Modes
(which-function-mode 1)                 ; which function the point is in
;; (show-paren-mode 1)                     ; highlight matching brackets
(setq-default indent-tabs-mode nil)     ; no tabs, only spaces
(setq comment-auto-fill-only-comments t) ; auto-fill comments and only them

;;; Yasnippet
(yas-global-mode 1)
(setq yas-prompt-functions '(yas/ido-prompt yas/completing-prompt))
;; expand with company
(define-key yas-minor-mode-map (kbd "<tab>") nil)
(define-key yas-minor-mode-map (kbd "TAB") nil)
;; (define-key yas-minor-mode-map (kbd "<the new key>") 'yas-expand)

;; company-mode
(add-hook 'after-init-hook 'global-company-mode)
(global-set-key (kbd "<C-tab>") 'company-yasnippet)
;; give way in minibuffer to company keymap
(define-key minibuffer-local-map "\M-n" nil)
;; company dabbrev backend downcase everything by default
(setq company-dabbrev-downcase nil)

(autoload 'projectile-on "projectile" "Project awareness in Emacs." t)
(add-hook 'prog-mode-hook
          (lambda () (progn
                       ; all code buffers with nlinum
                       ; (so much faster than linum!)
                       ;; (nlinum-mode 1) ; line # are overrated
                       ;; (ggtags-mode 1) ; trial
                       (flycheck-mode)
                       (subword-mode) ; camelcase moves
                       ;; (load-theme-buffer-local 'solarized-dark (current-buffer) t)
                       ;; (projectile-on) ; project awareness
                       (turn-on-auto-fill)
                       ;; really cool dtrt-indent but haven't seen a
                       ;; need for it recently
                       ;; (dtrt-indent-mode) ; auto-adjust tab-width
                       (hub/anti-useless-whitespace)
                       (hub/set-newline-and-indent-comment)
                       (rainbow-delimiters-mode t)
                       (eldoc-mode)
                       (electric-indent-local-mode)
                       )))
;; (setq linum-format " %3d ")    ; remove graphical glitches with fringe

;; Help
(define-key evil-normal-state-map (kbd ",hi") 'info)
;; dash: API Documentation Browser for MacOSX
(autoload 'dash-at-point "dash-at-point"
  "Search the word at point with Dash." t nil)
(define-key evil-normal-state-map (kbd ",hd") 'dash-at-point)
(define-key evil-normal-state-map (kbd ",hD") 'dash-at-point-with-docset)

;; languages
; anti useless whitespace
(defun hub/anti-useless-whitespace ()
  "Show and clean on save any trailing/useless whitespace."
  (require 'whitespace)
  ;; clean-up whitespace at save
  (make-local-variable 'before-save-hook)
  (add-hook 'before-save-hook 'whitespace-cleanup)
  ;; turn on highlight. To configure what is highlighted, customize
  ;; the *whitespace-style* variable. A sane set of things to
  ;; highlight is: face, tabs, trailing
  (make-local-variable 'whitespace-style)
  (setq whitespace-style '(face tabs lines-tail empty trailing))
  (whitespace-mode)
)

;; sbt
(add-hook 'sbt-mode-hook '(lambda ()
                            ;; compilation-skip-threshold tells the compilation minor-mode
                            ;; which type of compiler output can be skipped. 1 = skip info
                            ;; 2 = skip info and warnings.
                            (setq compilation-skip-threshold 1)

                            ;; Bind C-a to 'comint-bol when in sbt-mode. This will move the
                            ;; cursor to just after prompt.
                            (local-set-key (kbd "C-a") 'comint-bol)

                            ;; Bind M-RET to 'comint-accumulate. This will allow you to add
                            ;; more than one line to scala console prompt before sending it
                            ;; for interpretation. It will keep your command history cleaner.
                            (local-set-key (kbd "S-RET") 'comint-accumulate)
                            ))
(setq sbt:ansi-support t)
;; default (sbt) is not enough to get ANSI colors as sbt infers that
;; it's not supported. Forcing colors in sbt output.
(setq sbt:program-name "sbt -Dspecs2.color=true -Dsbt.log.format=true")

; scala
;; (add-to-list 'load-path (getenv "SCALA_MODE2_ROOT"))
(autoload 'scala-mode "scala-mode2")
(add-to-list 'auto-mode-alist '("\\.scala$" . scala-mode))
;; (add-to-list 'load-path (getenv "ENSIME_ROOT"))

(defun hub/sbt-start ()
  "Go to (or start) sbt buffer without affecting the current buffer."
  (interactive)
  (hub/dwim-other-window 'sbt-start))
(defun hub/ensime-inf-reload ()
  "Restart the REPL with the last definition."
  (interactive)
  (if (ensime-inf-running-p-1) (ensime-inf-quit-interpreter))
  (ensime-inf-switch))
(defun hub/ensime-goto-test--my-template ()
  "My default test template in Scala."
  "package %TESTPACKAGE%

import org.scalatest._

class %TESTCLASS% extends FlatSpec with Matchers
  // with prop.GeneratorDrivenPropertyChecks
  {
  \"%IMPLPACKAGE%.%IMPLCLASS%\" should \"\" in {
      assert(1 === 0)
  }
}
")
(defun hub/ensime-setup ()
  "ENSIME tweaking."
  (local-set-key (kbd "C-c C-l") 'hub/ensime-inf-reload)
  (setq ensime-goto-test-config-defaults
        '(:test-class-names-fn ensime-goto-test--test-class-names
         :test-class-suffixes ("Spec" "Test" "Specification" "Check")
         :impl-class-name-fn ensime-goto-test--impl-class-name
         :impl-to-test-dir-fn ensime-goto-test--impl-to-test-dir
         :is-test-dir-fn ensime-goto-test--is-test-dir
         :test-template-fn hub/ensime-goto-test--my-template)))
(defun hub/scala-ret ()
  "Dwim with RET even inside multiline comments."
  (interactive)
  (newline-and-indent)
  (scala-indent:insert-asterisk-on-multiline-comment))
(defun hub/projectile-test-suffix (project-type)
  "Include sbt suffixes."
  (or (projectile-test-suffix project-type)
      (if (eq project-type 'sbt) "Spec" ())))
(setq projectile-test-suffix-function 'hub/projectile-test-suffix)
(defun hub/scala-config ()
  "Config scala-mode to my liking and start ensime."
  (setq
   scala-indent:use-javadoc-style nil
   scala-indent:align-forms t
   scala-indent:align-parameters t
   scala-indent:default-run-on-strategy 1
   ; otherwise flycheck hides sbt errors with M-n/p
   flycheck-standard-error-navigation nil)
  ;; is buggy with scala-mode2
  ;; FIXME: doesn't look like I'm useful...
  ;; (make-local-variable 'comment-style)
  ;; (setq comment-style 'multi-line)
  ;; scala-mode doesn't work well (yet) with auto-fill
  ;; use M-q to wrap and indent long comments
  (turn-off-auto-fill)
  (require 'ensime)
  (ensime-scala-mode-hook)
  (local-set-key (kbd "RET") 'hub/scala-ret))

(evil-define-key 'normal scala-mode-map ",s." 'sbt-find-definitions)
(evil-define-key 'normal scala-mode-map ",cT" 'sbt-run-previous-command)
(evil-define-key 'normal scala-mode-map ",ct" 'sbt-command)
(evil-define-key 'normal scala-mode-map ",s/" 'sbt-grep)
(evil-define-key 'normal scala-mode-map ",sr" 'sbt-find-usages)
(evil-define-key 'normal scala-mode-map ",ss" 'hub/sbt-start)
(evil-define-key 'visual scala-mode-map ",l" 'sbt-send-region)

(evil-define-key 'normal scala-mode-map ",b" 'ensime-sbt-do-compile)
(evil-define-key 'normal scala-mode-map ",cn" 'ensime-sbt-do-clean)
(evil-define-key 'normal scala-mode-map ",E" 'ensime-sbt-do-run)
(evil-define-key 'normal scala-mode-map (kbd "M-.") 'ensime-edit-definition)
(evil-define-key 'normal scala-mode-map ",." 'ensime-edit-definition)
(evil-define-key 'normal scala-mode-map ",et" 'ensime-goto-test)
(evil-define-key 'normal scala-mode-map ",eT" 'ensime-goto-impl)
(evil-define-key 'normal scala-mode-map ",tt" 'ensime-sbt-do-test-only)
(evil-define-key 'normal scala-mode-map ",tT" 'ensime-sbt-do-test)
(evil-define-key 'normal scala-mode-map ",ii" 'ensime-inspect-type-at-point)
(evil-define-key 'normal scala-mode-map ",it" 'ensime-print-type-at-point)
(evil-define-key 'normal scala-mode-map ",ie" 'ensime-print-error-at-point)
(evil-define-key 'normal scala-mode-map ",im" 'ensime-import-type-at-point)
(evil-define-key 'normal scala-mode-map ",eq" 'ensime-show-all-errors-and-warnings) ; q -> quickfix
(evil-define-key 'normal scala-mode-map ",ef" 'ensime-format-source)
(evil-define-key 'normal scala-mode-map ",e/" 'ensime-search)
(evil-define-key 'normal scala-mode-map ",eq" 'ensime-scalex)
(evil-define-key 'normal scala-mode-map ",er" 'ensime-expand-selection)
(evil-define-key 'normal scala-mode-map ",hh" 'ensime-show-doc-for-symbol-at-point)
(evil-define-key 'normal scala-mode-map ",rr" 'ensime-refactor-rename)
(evil-define-key 'visual scala-mode-map ",rev" 'ensime-refactor-extract-local)
(evil-define-key 'visual scala-mode-map ",rem" 'ensime-refactor-extract-method)
(evil-define-key 'normal scala-mode-map ",roi" 'ensime-refactor-organize-imports)
(evil-define-key 'normal scala-mode-map ",gr" 'ensime-inf-switch)
(evil-define-key 'normal scala-mode-map ",es" 'ensime-sbt-switch)
(evil-define-key 'normal scala-mode-map ",eS" 'hub/ensime-sbt-dwim)
(evil-define-key 'insert scala-mode-map (kbd "C-S-<right>") 'sp-slurp-hybrid-sexp)
(evil-define-key 'insert scala-mode-map (kbd "C-S-<left>") 'sp-barf-hybrid-sexp)
(evil-define-key 'insert scala-mode-map (kbd "C-d") 'sp-kill-hybrid-sexp)

(defun hub/ensime-sbt-dwim ()
  "switch to SBT"
  (interactive)
  (hub/dwim-other-window 'ensime-sbt-switch))
;; { + Return => create a block and put the cursor on its own line
(sp-local-pair 'scala-mode "{" nil :post-handlers '(("||\n[i]" "RET")))

;; TODO: make a smart "go to definition" where you can configure the
;; list of functions to try in order.
;;
;; For Scala the logic would be:
;; If connected to ensime, use ensime-search,
;;
;;if not or if
;; unsuccessful, if a tag table is created use it,
;; (defun scala-smart-gd () (
;;   (interactive)
;;   (let ((smart-gd-try-functions-list '(ensime-search
;; tag-find-symbol-at-point-if-tag-table sbt-grep evil-goto-definition))))
;;     (smart-gd)
;;   ))
;; (evil-define-key 'normal scala-mode-map ",gd" 'scala-smart-gd)

(add-hook 'scala-mode-hook 'hub/scala-config)
(add-hook 'ensime-source-buffer-loaded-hook 'hub/ensime-setup)

; Emacs Lisp
(defun hub/emacs-lisp-config ()
  "Set up my emacs-lisp hacking environment."
  ;; (hub/set-newline-and-indent-comment)
  ;; (rainbow-delimiters-mode t)
  ;; (eldoc-mode)
  ;; (electric-indent-local-mode)
)
(add-hook 'emacs-lisp-mode-hook 'hub/emacs-lisp-config)
(require 'jka-compr) ; find-tag to be able to find .el.gz
(evil-define-key 'normal lisp-mode-shared-map ",." 'find-function)
(evil-define-key 'normal lisp-mode-shared-map ",hf" 'describe-function)
(evil-define-key 'normal lisp-mode-shared-map ",hv" 'describe-variable)

;; Smalltalk
(add-to-list 'auto-mode-alist '("\\.st$" . shampoo-code-mode))
(add-to-list 'load-path "~/Temp/shampoo-emacs/")
(autoload 'shampoo-code-mode "shampoo-modes")

;; Haskell
;; unicode input doesn't work with eg >= (Not in scope: ≥): disabled
;; (add-hook 'haskell-mode-hook 'turn-on-haskell-unicode-input-method)
(let ((my-cabal-path (expand-file-name "~/.cabal/bin")))
  (setenv "PATH" (concat my-cabal-path path-separator (getenv "PATH")))
  (add-to-list 'exec-path my-cabal-path))
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)
(add-hook 'haskell-mode-hook 'haskell-indent-mode)
(add-hook 'haskell-mode-hook 'haskell-doc-mode)
(add-hook 'flycheck-mode-hook 'flycheck-haskell-setup)
(autoload 'ghc-init "ghc" nil t)
(autoload 'ghc-debug "ghc" nil t)
(add-hook 'haskell-mode-hook (lambda () (ghc-init)))
(defun hub/haskell-config ()
  "Set up my emacs-lisp hacking environment."
  ;; (hub/set-newline-and-indent-comment)
  ;; (rainbow-delimiters-mode t)
  ;; (electric-indent-local-mode)          ; deactivate: weird indent toggle
  (ghc-init))
(add-hook 'haskell-mode-hook 'hub/haskell-config)

(with-eval-after-load 'company
  '(add-to-list 'company-backends 'company-ghc))
;;; key bindings
(eval-after-load "haskell-mode"
  '(progn
     (define-key haskell-mode-map (kbd "M-<left>") 'haskell-move-nested-left)
     (define-key haskell-mode-map (kbd "M-<right>") 'haskell-move-nested-right)))
(eval-after-load "haskell-mode"
  '(progn (define-key haskell-mode-map (kbd "C-c C-c") 'haskell-compile)
          (define-key haskell-mode-map (kbd  "C-c |") 'haskell-indent-insert-guard))
)
(eval-after-load "haskell-cabal"
  '(define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-compile))
(evil-define-key 'normal haskell-cabal-mode-map ",b" 'haskell-compile)
(evil-define-key 'normal haskell-mode-map ",b" 'haskell-compile)
(evil-define-key 'normal haskell-mode-map ",." 'find-tag)
(evil-define-key 'normal haskell-mode-map ",l" 'inferior-haskell-load-file)
(evil-define-key 'normal haskell-mode-map ",gr" 'switch-to-haskell)
(evil-define-key 'normal haskell-mode-map ",ii" 'haskell-process-do-info)
(evil-define-key 'normal haskell-mode-map ",et" 'haskell-process-do-type)
(evil-define-key 'normal haskell-mode-map ",hh" 'inferior-haskell-find-haddock)
;; align
;; TODO: fixme. Goal when I call align-current in a scala file it
;; magically align on => and <-
(add-hook 'align-load-hook (lambda ()
                             (add-to-list 'align-rules-list
                                          '(scala-cases
                                            (regexp  . "\\(\\s-+\\)\\(=>\\)")
                                            (group   . 1)
                                            (modes   . '(scala-mode))
                                            (repeat  . nil)))
                             (add-to-list 'align-rules-list
                                          '(scala-align
                                            (regexp  . "\\(\\s-+\\)\\(<-\\)")
                                            (group   . 1)
                                            (modes   . '(scala-mode))
                                            (repeat  . nil)))
                             (add-to-list 'align-rules-list
                                          '(haskell-types
                                            (regexp . "\\(\\s-+\\)\\(::\\|∷\\)\\s-+")
                                            (modes quote (haskell-mode literate-haskell-mode))))
                             (add-to-list 'align-rules-list
                                          '(haskell-assignment
                                            (regexp . "\\(\\s-+\\)=\\s-+")
                                            (modes quote (haskell-mode literate-haskell-mode))))
                             (add-to-list 'align-rules-list
                                          '(haskell-arrows
                                            (regexp . "\\(\\s-+\\)\\(->\\|→\\)\\s-+")
                                            (modes quote (haskell-mode literate-haskell-mode))))
                             (add-to-list 'align-rules-list
                                          '(haskell-left-arrows
                                            (regexp . "\\(\\s-+\\)\\(<-\\|←\\)\\s-+")
                                            (modes quote (haskell-mode literate-haskell-mode))))))

;; Ruby
(add-hook 'inf-ruby-mode-hook '(lambda ()
                           ;; turn off the annoying input echo in irb
                           (setq comint-process-echoes t)
                           ))
;; stolen from https://github.com/rejeep/ruby-tools.el/blob/master/ruby-tools.el
(defun ruby-tools-looking-around (back at)
  "Check if looking backwards at BACK and forward at AT."
  (and (looking-at-p at) (looking-back back)))
(defun ruby-tools-interpolate ()
  "Interpolate with #{} in some places."
  (interactive)
  (if (and mark-active (equal (point) (region-end)))
      (exchange-point-and-mark))
  (insert "#")
  (when (or
         (ruby-tools-looking-around "\"[^\"\n]*" "[^\"\n]*\"")
         (ruby-tools-looking-around "`[^`\n]*"   "[^`\n]*`")
         (ruby-tools-looking-around "%([^(\n]*"  "[^)\n]*)"))
    (cond (mark-active
           (goto-char (region-beginning))
           (insert "{")
           (goto-char (region-end))
           (insert "}"))
          (t
           (insert "{}")
           (forward-char -1)))))
(defun hub-ruby-config ()
  "My Ruby config."
  (local-set-key (kbd "#") 'ruby-tools-interpolate)
  ;; (hub/set-newline-and-indent-comment)
  ;; fix indenting with Evil
  (setq evil-shift-width ruby-indent-level)
  ;; stop adding coding: utf8 comment in every header
  (setq ruby-insert-encoding-magic-comment nil))
(add-hook 'ruby-mode-hook 'hub-ruby-config)
(eval-after-load 'company
  '(push 'company-robe company-backends))
(defadvice inf-ruby-console-auto (before activate-rvm-for-robe activate)
  (rvm-activate-corresponding-ruby))
(add-hook 'ruby-mode-hook 'robe-mode)
(add-hook 'minitest-compilation-mode-hook 'inf-ruby-mode) ;or you can't debug
;; so that I can debug things with `binding.pry'
(add-hook 'compilation-filter-hook 'inf-ruby-auto-enter)
(add-hook 'after-init-hook 'inf-ruby-switch-setup)
;; (add-hook 'ruby-mode-hook 'minitest-mode)
(eval-after-load 'minitest
  '(minitest-install-snippets))
(evil-define-key 'visual ruby-mode-map ",l" 'ruby-send-region)
(evil-define-key 'normal ruby-mode-map ",." 'robe-jump)
(evil-define-key 'normal ruby-mode-map ",gr" 'ruby-switch-to-inf)
(evil-define-key 'normal ruby-mode-map ",tt" 'minitest-verify)
(evil-define-key 'normal ruby-mode-map ",ta" 'minitest-verify-all)
(evil-define-key 'normal ruby-mode-map ",tc" 'minitest-verify-single)
(evil-define-key 'normal ruby-mode-map ",tr" 'minitest-rerun)

;; Web: HTML/CSS
(add-hook 'sgml-mode-hook 'zencoding-mode) ;; Auto-start on any markup modes
;; (add-hook 'sgml-mode-hook (lambda () (progn (nlinum-mode 1))))
;; after deleting a tag, indent properly
(defadvice sgml-delete-tag (after reindent activate)
  (indent-region (point-min) (point-max)))
(sp-local-pair 'css-mode "{" nil :post-handlers '(("||\n[i]" "RET")))
(add-to-list 'auto-mode-alist '("\\.scss$" . css-mode))
(add-hook 'sgml-mode-hook
          (lambda ()
            ;; Default indentation to 2, but let SGML mode guess, too.
            (set (make-local-variable 'sgml-basic-offset) 4)
            (sgml-guess-indent)))

;; JS
(add-to-list 'auto-mode-alist '("\\.json$" . js-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
;; when you only want linting from js2
;; (add-hook 'js-mode-hook 'js2-minor-mode)
;; flycheck
;; ideally I'd want to check for the presence of a eslint config file
;; in the parent directories. That would start like this but instead I
;; would prefer to:
;; - reduce over the hierarchy starting from the current most bottom path
;; - eslint supports *many* config file so regexp rather than file-exists-p
;; (defun parent-directory (dir)
;;   "Parent directory of DIR or nil if root."
;;   (unless (equal "/" dir)
;;     (file-name-directory (directory-file-name dir))))

;; (defun find-file-in-hierarchy (current-dir fname)
;;   "Search from CURRENT-DIR for a file FNAME upwards."
;;   (let ((file (concat current-dir fname))
;;         (parent (parent-directory (expand-file-name current-dir))))
;;     (if (file-exists-p file)
;;         file
;;       (when parent
;;         (find-file-in-hierarchy parent fname)))))
;; in the meantime that should do...
;; (add-hook 'js2-mode-hook
;;           (defun my-js2-mode-setup ()
;;             (when (executable-find "eslint")
;;               (flycheck-select-checker 'javascript-eslint))))

;; somehow I have to explicitly require js2-refactor
(eval-after-load 'js2-mode '(require 'js2-refactor))
(eval-after-load 'js2-mode
  '(sp-local-pair 'js2-mode "<" ">"))
(sp-local-pair 'js2-mode "{" nil :post-handlers '(("||\n[i]" "RET")))
(sp-local-pair 'js2-mode "(" nil :post-handlers '(:add sp-js2-electric-semicolon))
;; adapted to smartparens from:
;; https://gist.github.com/lewang/908bc9cb7677d5936936
(defun sp-js2-electric-semicolon (id action context)
  (message "node before is %s" (js2-node-type (js2-node-at-point (- (point) 1))))
  (message "action is %s" action)
  (message "pair is %s %s %d" id (plist-get (sp-get-pair id) :close) (length id))
  (when (and (eq action 'insert)
             (save-excursion
               (goto-char (- (point) (length id)))
               (skip-chars-backward " \t")
               (memq (js2-node-type (js2-node-at-point (point)))
                     (list js2-NAME js2-LP js2-SCRIPT js2-CALL js2-BLOCK))))
    (save-excursion
      (goto-char (+ (point) (length (plist-get (sp-get-pair id) :close))))
      (insert ";"))))

(setq js2-basic-offset 2
      js2-bounce-indent-p t
      js2-highlight-level 2)
;; I don't like web-mode. jsx-mode should be a minor mode letting me
;; use all the goodness of my js2 config... No JSX support for now,
;; bare js2.
;; (add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))
;; (defadvice web-mode-highlight-part (around tweak-jsx activate)
;;   (if (equal web-mode-content-type "jsx")
;;       (let ((web-mode-enable-part-face nil))
;;         ad-do-it)
;;     ad-do-it))

(defun hub/js-keymap()
  (evil-define-key 'insert js2-mode-map (kbd "RET") 'js2-line-break)
  (evil-define-key 'normal js2-mode-map "zc" 'js2-mode-hide-element)
  (evil-define-key 'normal js2-mode-map "zo" 'js2-mode-show-element)
  (evil-define-key 'normal js2-mode-map "za" 'js2-mode-toggle-element)
  (evil-define-key 'normal js2-mode-map "zC" 'js2-mode-hide-functions)
  (evil-define-key 'normal js2-mode-map "zM" 'js2-mode-hide-functions)
  (evil-define-key 'normal js2-mode-map "zO" 'js2-mode-show-functions)
  (evil-define-key 'normal js2-mode-map "zR" 'js2-mode-show-functions)
  (evil-define-key 'normal js2-mode-map "zA" 'js2-mode-toggle-hide-functions)
  (evil-define-key 'normal js2-mode-map "zj" 'js2-mode-toggle-hide-comments)
  (evil-define-key 'normal js2-mode-map ",hi" 'js-doc-insert-function-doc)
  (evil-define-key 'normal js2-mode-map ",@" 'js-doc-insert-tag)
  (evil-define-key 'normal js2-mode-map ",hI" 'js-doc-insert-file-doc)
  (evil-define-key 'normal js2-mode-map ",=" 'web-beautify-js)
  (evil-define-key 'visual js2-mode-map ",=" 'web-beautify-js)
  (evil-define-key 'normal js2-mode-map ",." 'tern-find-definition)
  (evil-define-key 'normal js2-mode-map ",t" 'tern-pop-find-definition)
  (evil-define-key 'normal js2-mode-map (kbd "M-.") 'tern-find-definition)
  (evil-define-key 'normal js2-mode-map ",:" 'tern-find-definition-by-name)
  (evil-define-key 'normal js2-mode-map ",ii" 'tern-get-type)
  (evil-define-key 'normal js2-mode-map ",hh" 'tern-get-docs)
  (evil-define-key 'normal js2-mode-map (kbd  "M-n") 'flycheck-next-error)
  (evil-define-key 'insert js2-mode-map (kbd  "M-n") 'flycheck-next-error)
  (evil-define-key 'normal js2-mode-map ",reo" 'js2r-expand-object)
  (evil-define-key 'normal js2-mode-map ",rco" 'js2r-contract-object)
  (evil-define-key 'normal js2-mode-map ",reu" 'js2r-expand-function)
  (evil-define-key 'normal js2-mode-map ",rcu" 'js2r-contract-function)
  (evil-define-key 'normal js2-mode-map ",rea" 'js2r-expand-array)
  (evil-define-key 'normal js2-mode-map ",rca" 'js2r-contract-array)
  (evil-define-key 'normal js2-mode-map ",rwi" 'js2r-wrap-buffer-in-iife)
  (evil-define-key 'normal js2-mode-map ",rig" 'js2r-inject-global-in-iife)
  (evil-define-key 'normal js2-mode-map ",riv" 'js2r-inline-var)
  (evil-define-key 'normal js2-mode-map ",rrv" 'js2r-rename-var)
  (evil-define-key 'normal js2-mode-map ",rvt" 'js2r-var-to-this)
  (evil-define-key 'normal js2-mode-map ",rag" 'js2r-add-to-globals-annotation)
  (evil-define-key 'normal js2-mode-map ",rsv" 'js2r-split-var-declaration)
  (evil-define-key 'normal js2-mode-map ",rss" 'js2r-split-string)
  (evil-define-key 'normal js2-mode-map ",rip" 'js2r-introduce-parameter)
  (evil-define-key 'normal js2-mode-map ",rlp" 'js2r-localize-parameter)
  (evil-define-key 'normal js2-mode-map ",rtf" 'js2r-toggle-function-expression-and-declaration)
  (evil-define-key 'normal js2-mode-map ",rao" 'js2r-arguments-to-object)
  (evil-define-key 'normal js2-mode-map ",ruw" 'js2r-unwrap)
  (evil-define-key 'normal js2-mode-map ",rwl" 'js2r-wrap-in-for-loop)
  (evil-define-key 'normal js2-mode-map ",r3i" 'js2r-ternary-to-if)
  (evil-define-key 'normal js2-mode-map ",rlt" 'js2r-log-this)
  ;; (evil-define-key 'normal js2-mode-map ",)" 'js2r-forward-slurp)
  ;; (evil-define-key 'normal js2-mode-map ",(" 'js2r-forward-barf)
  (local-set-key (kbd "C-<right>") 'js2r-forward-slurp)
  (local-set-key (kbd "C-<left>") 'js2r-forward-barf)
  (evil-define-key 'visual js2-mode-map ",rev" 'js2r-extract-var)
  (evil-define-key 'visual js2-mode-map ",ref" 'js2r-extract-function)
  (evil-define-key 'visual js2-mode-map ",rem" 'js2r-extract-method)
  )
(defun hub/js-config()
  "How I like my JS XP."
  (tern-mode t)
  (hub/js-keymap)
  ;; (ggtags-mode nil)
  (hub/anti-useless-whitespace))
(add-hook 'js2-mode-hook 'hub/js-config)

;; R
(require 'ess-site)
(evil-define-key 'normal ess-mode-map ",ho" 'ess-display-help-on-object)
(evil-define-key 'visual ess-mode-map ",l" 'ess-eval-region)
(evil-define-key 'visual ess-mode-map ",L" 'ess-eval-region-and-go)

;; also see comint section

;; Clojure
(add-hook 'cider-mode-hook 'set-auto-complete-as-completion-at-point-function)
(defun hub/clojure-config ()
  "Clojure the way I like it."
  (clj-refactor-mode 1)
  (yas-minor-mode 1) ; for adding require/use/import
  ;; (eldoc-mode)
  (cljr-add-keybindings-with-prefix "C-c C-r"))
(evil-define-key 'normal cider-mode-map ",l" 'cider-load-buffer)
(evil-define-key 'visual cider-mode-map ",l" 'cider-eval-region)
(evil-define-key 'normal cider-mode-map ",." 'cider-jump-to-var)
(evil-define-key 'normal cider-mode-map ",;" 'cider-jump-back)
(evil-define-key 'normal cider-mode-map ",ii" 'cider-inspect)
(evil-define-key 'normal cider-mode-map ",gr" 'cider-switch-to-repl-buffer)
(evil-define-key 'normal cider-mode-map ",hh" 'cider-doc)
(evil-define-key 'normal cider-mode-map ",hg" 'cider-docview-grimoire)
(evil-define-key 'normal cider-mode-map ",hG" 'cider-docview-grimoire-web)
;; stolen from http://jakemccrary.com/blog/2015/06/30/my-favorite-clj-refactor-features/
(require 'clj-refactor)
;; Add custom magic requires.
(dolist (mapping '(("maps" . "outpace.util.maps")
                   ("seqs" . "outpace.util.seqs")
                   ("times" . "outpace.util.times")
                   ("repl" . "outpace.util.repl")
                   ("time" . "clj-time.core")
                   ("string" . "clojure.string")))
  (add-to-list 'cljr-magic-require-namespaces mapping t))

(setq cljr-favor-prefix-notation nil)


;;; 4clojure
(defadvice 4clojure-open-question (around 4clojure-open-question-around)
  "Start a cider/nREPL connection if one hasn't already been started when
opening 4clojure questions"
  ad-do-it
  (unless cider-current-clojure-buffer
    (cider-jack-in)))
(defun endless/4clojure-check-and-proceed ()
  "Check the answer and show the next question if it worked."
  (interactive)
  (unless
      (save-excursion
        ;; Find last sexp (the answer).
        (goto-char (point-max))
        (forward-sexp -1)
        ;; go to the beginning of the line for answer like :a :b :c
        (beginning-of-line)
        ;; Check the answer.
        (cl-letf ((answer
                   (buffer-substring (point) (point-max)))
                  ;; Preserve buffer contents, in case you failed.
                  ((buffer-string)))
          (goto-char (point-min))
          (while (search-forward "__" nil t)
            (replace-match answer))
          (string-match "failed." (4clojure-check-answers))))
    (4clojure-next-question)))
(defadvice 4clojure/start-new-problem
    (after endless/4clojure/start-new-problem-advice () activate)
  ;; Prettify the 4clojure buffer.
  (goto-char (point-min))
  (forward-line 2)
  (forward-char 3)
  (fill-paragraph)
  ;; Position point for the answer
  (goto-char (point-max))
  (insert "\n\n\n")
  (forward-char -1)
  (whitespace-mode -1)                  ; deactivate, it's annoying
  ;; Define our key.
  (local-set-key (kbd "M-j") #'endless/4clojure-check-and-proceed))
(defun 4clojure-login (user pwd)
  "Login to 4clojure"
  (interactive "sWhat's your name? \nsAnd your password ")
  (request
   "http://www.4clojure.com/login"
   :type "POST"
   :sync t
   :headers '(
              ("User-Agent" . "Mozilla/5.0 (X11; Linux x86_64; rv:28.0) Gecko/20100101  Firefox/28.0")
              ("Referer" . "http://www.4clojure.com/login")
              )
                                        ;   :parser 'buffer-string
   :data `(("user" . ,user) ("pwd" . ,pwd))
   :success (function*
             (lambda (&key data &allow-other-keys)
               data
               )
             )
                                        ; when server send 302 header, `request` redirect request with original method POST,
                                        ; So 4clojure will not handle this redirect and given 404
   :status-code '((404 . (lambda (&rest _) (message "login successful!"))))
   )
  )

;; Shell scripting
(add-to-list 'auto-mode-alist '("\\.zsh$" . shell-script-mode))

;; Elixir
(sp-with-modes '(elixir-mode)
  (sp-local-pair "fn" "end"
                 :when '(("SPC" "RET"))
                 :actions '(insert navigate))
  (sp-local-pair "do" "end"
                 :when '(("SPC" "RET"))
                 :post-handlers '(sp-ruby-def-post-handler)
                 :actions '(insert navigate)))
(setq alchemist-hooks-test-on-save t)

;; OSX launchd plist
(define-auto-insert "\.plist" ["template.plist" hub/autoinsert-yas-expand])
(add-to-list 'auto-mode-alist '("\\.plist$" . xml-mode))


; tweaking to get my proper setup
; OSX
; * iTerm2
; Preferences > Keys (tab) > Remap Left Command to Left Option
; Preferences > Profile > Left Option: Meta + Esc
; * Numpad (for calc): remap keypad-dot to Option+Shift+Keypad-dot

(put 'narrow-to-region 'disabled nil)
(put 'erase-buffer 'disabled nil)
(provide 'init)
;;; init.el ends here
