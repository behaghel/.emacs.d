;;; emacs --- Hubert's .emacs file         -*- lexical-binding: t -*-
;; Copyright (C) 2013 Hubert Behaghel
;;
;;; Commentary:
;; *** TODO: move it to .emacs.d and split it into modules
;; *** TODO: clarify key-bindings (move all of them in their own .el)
;;           - abandon any use of C-c
;;           - have a clear strategy when to use evil keymaps
;;             - z == toggle
;;             - g == goto + use [] () {} for prior/next
;;             - ,g == go to special
;;               - ,gr -> go to REPL TODO: should be gz
;;               - ,gs -> go to EShell
;;             - , == <leader>
;;             - ,v == anything versioning
;;             - ,o == open
;;             - ,e == anything execute
;;               > ,el -> execute file or region by loading it in REPL
;;             - ,h == anything help
;;             - ,n == new / create
;;             - coding:
;;               - ,.  -> find definition for symbol at point
;;               - ,hh -> go to help for symbol at point
;;               - ,b  -> build / compile task
;;               - ,d  -> debug
;;               - ,ii -> inspect type at point
;;                 ,il -> inspect last expression
;;               - ,f  -> anything formatting / refactoring
;;               - ,= -> align nicely using M-x align
;;
;;; Code:

;; On Windows: set HOME environment variable and put .emacs.d in there!

;; to stop M-x customize to pollute my init.el: http://emacsblog.org/2008/12/06/quick-tip-detaching-the-custom-file/
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)
(setq hub-lisp-dir (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path hub-lisp-dir)  ; to include my .el
(setq settings-dir
      (expand-file-name "settings" user-emacs-directory))
;; Set up load path
(add-to-list 'load-path settings-dir)

;; putting it earlier in attempt to make it work.
(setq org-return-follows-link t)
(setq org-footnote-auto-adjust t)

(setq user-mail-address "behaghel@gmail.com")
(setq user-full-name "Hubert Behaghel")
;; (toggle-debug-on-error)

;; install
(require 'package)
(require 'cl)
;; (add-to-list 'package-archives
;;           '("marmalade" . "http://marmalade-repo.org/packages/"))
;; (add-to-list 'package-archives
;;              '("melpa" . "http://melpa.milkbox.net/packages/"))
;; (add-to-list 'package-archives
;;              '("melpa-stable" . "http://stable.melpa.org/packages/"))
(setq
 package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                    ("org" . "http://orgmode.org/elpa/")
                    ("melpa" . "http://melpa.org/packages/")
                    ("melpa-stable" . "http://stable.melpa.org/packages/"))
 package-archive-priorities '(("melpa-stable" . 1)))

; required to find melpa-installed package after restart at init time
(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(unless (package-installed-p 'diminish)
  (package-install 'diminish))

(eval-when-compile
  (require 'use-package))
(require 'diminish)
(setq use-package-verbose t
      use-package-always-defer nil
      use-package-always-ensure t)

(require 'auth-source-pass)
(auth-source-pass-enable)
(use-package pinentry)
(setq epa-pinentry-mode 'loopback)

(use-package better-defaults)
;; https://github.com/emacs-lsp/lsp-mode#performance
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024)) ;; 1mb

(use-package hydra)

(require 'setup-evil)

; stop cluttering my fs with #file.ext#
(setq auto-save-file-name-transforms
      `((".*" ,(concat user-emacs-directory "backups") t)))

; Mac

;; Are we on a mac? Thanks @magnars
(setq is-mac (equal system-type 'darwin))

;; fix for Mac OS X PATH in Emacs GUI
(use-package exec-path-from-shell
  :if (and window-system is-mac)
  :config
  (exec-path-from-shell-initialize))

(setq mac-command-key-is-meta t)
(setq mac-command-modifier 'meta)
(setq mac-option-key-is-meta nil)
(setq mac-option-modifier nil)

(setq ns-use-srgb-colorspace t)

;;;;;;;;;;;;
; General Behaviour
;;;;;;;;;;;;

(defun save-all ()
  "To be used to automatically save when I leave Emacs."
  (interactive)
  (save-some-buffers t))
(add-hook 'focus-out-hook 'save-all)

;; synchronise emacs clipboard with system clipboard
(use-package clipmon
  :defer t
  :ensure t)

;; particularly useful in git repositories to avoid the hassle of
;; manually reloading each buffer when you change branch.
(global-auto-revert-mode t)

(use-package undo-tree
  :diminish undo-tree-mode
  :bind ("C-é" . undo)
  :config
  (global-undo-tree-mode))

(use-package unfill
  :commands (unfill-region unfill-paragraph toggle-fill-unfill))

(setq comment-auto-fill-only-comments t) ; auto-fill comments and only them
;; this one seems hard to diminish: insisting
;; (eval-after-load 'auto-fill-mode '(diminish 'auto-fill-function))
;; (eval-after-load 'auto-fill '(diminish 'auto-fill-function))
;; (diminish 'auto-fill-function)
;; don't wrap for space before French punctuation
(add-to-list 'fill-nobreak-predicate 'fill-french-nobreak-p)

(setq tab-always-indent 'complete)      ; tab try to indent, if indented complete
(setq mode-require-final-newline nil)   ; don't add new line at EOF on save

;; General Keybindings
;;; We don't want shift selection
(setq shift-select-mode nil)
(global-set-key (kbd "C-=") 'align-current)
;; fix windows inability to pick up font change at load time...
(global-set-key (kbd "<f10>")
                (lambda () (interactive)
                  (set-face-attribute 'default nil :font "Iosevka-12")))

(use-package expand-region
  :bind ("M-r" . er/expand-region)
  :commands (er/expand-region))

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

(use-package smartparens
  :diminish smartparens-mode
  :defer 2
  ;; this works great for lisp languages
  ;; ("C-<right>" . sp-forward-slurp-sexp)
  ;; this works better for other languages
  :bind (("C-<right>" . sp-slurp-hybrid-sexp)
         ("M-<left>" . sp-backward-slurp-sexp)
         ("C-<left>" . sp-forward-barf-sexp)
         ("M-<right>" . sp-backward-barf-sexp)
         ("C-<down>" . sp-down-sexp)
         ("C-<up>" . sp-backward-up-sexp)
         ("M-<down>" . sp-backward-down-sexp)
         ("M-<up>" . sp-up-sexp)
         ("S-M-f" . sp-forward-sexp)
         ("S-M-b" . sp-backward-sexp))
  :init
  (use-package evil-smartparens
    :diminish evil-smartparens-mode
    :config
    (defadvice evil-sp--add-bindings
        (after evil-sp--add-bindings-after activate)
      (evil-define-key 'normal evil-smartparens-mode-map
        (kbd ",l") #'evil-sp-change
        (kbd ",L") #'evil-sp-change-line
        (kbd ",K") #'evil-sp-change-whole-line
        (kbd ",D") #'evil-sp-delete-line
        (kbd "D") nil
        (kbd "c") nil
        (kbd "s") nil
        (kbd "S") nil
        (kbd ",k") #'evil-sp-substitute
        (kbd ",K") #'sp-kill-sexp
        ;; Finds opening '(' of the current list.
        (kbd ",(") #'sp-backward-up-sexp
        ;; Finds closing ')' of the current list.
        (kbd ",)") #'sp-up-sexp
        (kbd ",k") #'sp-down-sexp
        ;; Go to the start of current/previous sexp
        (kbd "[[") #'sp-backward-sexp
        ;; Go to the start of next sexp.
        (kbd "]]") #'sp-forward-sexp
        (kbd ",{") #'sp-backward-barf-sexp
        (kbd ",}") #'sp-forward-barf-sexp
        (kbd "gn") #'sp-next-sexp
        (kbd "gp") #'sp-previous-sexp
        ;; (define-key evil-motion-state-map "S" 'evil-window-top)
        ;; (define-key evil-motion-state-map "s" 'evil-previous-line)
        ))
    (add-hook 'smartparens-enabled-hook #'evil-smartparens-mode))
  :config
  (require 'smartparens-config)
  (add-to-list 'sp-ignore-modes-list 'org-mode)
  (smartparens-global-mode t)
  (show-smartparens-global-mode)
  (smartparens-global-strict-mode)
  )

;; Dired
(use-package dired-details
  :disabled t
  :config
  (setq-default dired-details-hidden-string "--- ")
  (dired-details-install))
(setq dired-dwim-target t)
(use-package dired-subtree
  :config
  (bind-keys :map dired-mode-map
             ("(" . dired-subtree-insert)
             (")" . dired-subtree-remove)))
;;narrow dired to match filter
(use-package dired-narrow
  :ensure t
  :bind (:map dired-mode-map
              ("/" . dired-narrow)))
;; clipboard-type feature for copying/moving files.
;; Mark files then Y to copy them, add more with C-u Y
;; then go to destination and use X (move) or V (paste)
(use-package dired-ranger
  :ensure t
  :bind (:map dired-mode-map
              ("Y" . dired-ranger-copy)
              ("X" . dired-ranger-move)
              ("V" . dired-ranger-paste)))

;; ivy
(use-package ivy
  :defer nil
  :diminish
  :bind (("M-D"   . send-m-del)
         ("M-c"   . ivy-copy-selection)
         ("C-c o" . ivy-tv-filtered-candidates)
         ("C-s"   . swiper)
         ("C-r"   . swiper)
         :map evil-normal-state-map
         (",of"   . counsel-find-file)
         (",ga"   . counsel-ag)
         (",x"    . counsel-M-x)
         (",gB"   . ivy-switch-buffer-other-window)
         )
  :custom
  (ivy-count-format "(%d/%d) ")
  (ivy-display-style 'fancy)
  ;; I suspect with ivy virtual buffers
  ;; there is a bug where
  ;; bookmark module is required at
  ;; each ivy-switch-buffer,
  ;; bookmark post load hooks are
  ;; run each time as well and somehow
  ;; this makes my system slower each time
  (ivy-use-virtual-buffers nil)
  :config
  (ivy-mode)
  (counsel-mode))

;; (use-package ivy-rich)

;; keybindings for projectile (replace s-p with ,p)
;; https://docs.projectile.mx/en/latest/usage/#interactive-commands
(use-package projectile
  :diminish projectile-mode
  :defer 1
  :bind (:map projectile-command-map    ; under ,p
              ("P" . projectile-switch-project)
              ("p" . persp-switch-last)
              ("-" . persp-remove-buffer) ; disassociate buffer from persp
              ("R" . persp-rename)
              ("X" . persp-kill) ; terminate perspective
              ("+" . persp-add-buffer) ; associate buffer to current persp
              ("M" . persp-set-buffer) ; like add but remove from all other
              ("b" . persp-counsel-switch-buffer) ; persp-aware
              ("T" . projectile-find-implementation-or-test-other-window)
              ("C-s" . persp-state-save) ; save perspective layout to file
              ("C-l" . persp-state-load) ; load perspective layout from file
)
  :config
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (evil-define-key 'normal 'projectile-mode-map ",p" 'projectile-command-map)
  (setq projectile-completion-system 'ivy)
  (setq projectile-project-search-path '("~/ws/"))
  (use-package counsel-projectile
    :after counsel
    :config
    (counsel-projectile-mode))
  )

(require 'setup-perspective)

;; Ediff
(setq ediff-diff-options "-w")
(setq ediff-split-window-function 'split-window-horizontally)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;;; Yasnippet
(use-package yasnippet
  :defer t
  ;; :disabled t
  :ensure t
  :diminish yas-minor-mode
  :bind (("<C-tab>" . company-yasnippet)
         :map yas-minor-mode-map
         ;; expand with company
         ("<tab>" . nil)
         ("TAB" . nil))
  :config
  (yas-global-mode 1)
  (setq yas-prompt-functions '(yas/completing-prompt))
)

(use-package yasnippet-snippets)

;;; auto-insert-mode (template filling at file creation time)
(auto-insert-mode 1)
;; (add-hook 'find-file-hook 'auto-insert)
(setq auto-insert-directory "~/.emacs.d/insert/")
;; you can use yasnippet to expand it
;; see: http://www.emacswiki.org/emacs/AutoInsertMode
;; the standard emacs way use skeleton
;; see: https://github.com/cinsk/emacs-scripts/blob/8212d714d5c6f6b95e873e8688b30ba130d07775/xskel.el
;; also: http://www.howardism.org/Technical/Emacs/templates-tutorial.html
(defun hub/autoinsert-yas-expand (&optional expand-env)
    "Replace text in yasnippet template optionally passing EXPAND-ENV (let-style)."
    (yas-expand-snippet (buffer-string) (point-min) (point-max) expand-env))
(define-auto-insert "\.org" ["template.org" hub/autoinsert-yas-expand])
;; orj is an extension I invented: org-revealJS
(define-auto-insert "\.orj" ["template.orj" hub/autoinsert-yas-expand])

(require 'setup-org)

;; Use emacs to edit textarea in Chrome
(use-package edit-server
  :if window-system
  :ensure t
  :defer t
  :init
  (add-hook 'after-init-hook 'server-start t)
  (add-hook 'after-init-hook 'edit-server-start t)
  :config
  (setq edit-server-default-major-mode 'markdown-mode))

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

(require 'setup-blog)

(defun hub/dwim-other-window (f)
  "Run F in a new window if only one window is visible.
Otherwise switch to other window before."
  (if (one-window-p t 'visible)
      (split-window-right))
  (other-window 1)
  (funcall f))

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

;; Emacs and the shell
;; currently my zsh setup fails when used from Emacs with
;; complete:13: command not found: compdef
;; (setq shell-file-name "bash")

(require 'setup-eshell)

(use-package restclient
  :commands (restclient-mode))
(setq url-http-attempt-keepalives nil)

;;; TRAMP
(setq tramp-default-method "ssh")

(require 'setup-erc)

(require 'setup-twitter)

(require 'setup-git)

(require 'setup-multiple-cursors)

;; Editing text
(add-hook 'text-mode-hook 'turn-on-auto-fill) ; auto-wrap
(setq sentence-end-double-space nil)    ; one space is enough after a period to end a sentence

;; Writing with style
;; http://rs.io/software-writers-tools-improve-writing/
;; chase weasel words, count words and more
;; https://github.com/sachac/artbollocks-mode
;; http://sachachua.com/blog/2011/12/emacs-artbollocks-mode-el-and-writing-more-clearly/
(use-package artbollocks-mode
  :commands artbollocks-mode)

(use-package writeroom-mode
  :commands (writeroom-mode))

(setq langtool-language-tool-jar "/usr/local/Cellar/languagetool/2.6/libexec/languagetool.jar")

;; asciidoc
(add-hook 'adoc-mode-hook (lambda() (buffer-face-mode t)))
(add-to-list 'auto-mode-alist (cons "\\.asciidoc\\'" 'adoc-mode))

;; Markdown
(use-package markdown-mode
  :mode (("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)
         ("README\\.md\\'" . gfm-mode))
  :init
  ;; in github flavour markdown, \n are enforced strictly
  (add-hook 'gfm-mode-hook (lambda () (auto-fill-mode -1)))
  :commands (markdown-mode)
  :config
  (setq markdown-command "pandoc -c file://${HOME}/.emacs.d/github-pandoc.css --from markdown_github -t html5 --mathjax --highlight-style pygments --standalone")

  (evil-define-key 'normal markdown-mode-map (kbd ",il") 'markdown-insert-link)
  (evil-define-key 'normal markdown-mode-map (kbd ",iH") 'markdown-insert-header-dwim)
  (evil-define-key 'normal markdown-mode-map (kbd ",ih") 'markdown-insert-header-setext-dwim)
  (evil-define-key 'normal markdown-mode-map (kbd ",i2") 'markdown-insert-header-setext-2)
  (evil-define-key 'normal markdown-mode-map (kbd ",i1") 'markdown-insert-header-setext-1)
  (evil-define-key 'normal markdown-mode-map (kbd ",ev") 'markdown-preview)
  (evil-define-key 'normal markdown-mode-map (kbd ",eV") 'markdown-export-and-preview))

;; Cucumber
(use-package feature-mode
  :defer t
  :mode ("\\.feature$" . feature-mode)
  :config
  (setq feature-step-search-path "features/**/*steps.rb")
  ;; (setq feature-step-search-gems-path "gems/ruby/*/gems/*/**/*steps.rb")
  (add-hook 'feature-mode-hook
            (lambda ()
              (electric-indent-mode -1))))
; CODING
;; keys
;; stolen here: http://www.emacswiki.org/emacs/CommentingCode
;; Original idea from
;; http://www.opensubscriber.com/message/emacs-devel@gnu.org/10971693.html

;; Debugging
(use-package realgud
  :disabled t                           ; install error: can't install org-mac-link??
  :commands (realgud:gdb realgud:byebug realgud:pry))

;; Navigating
(use-package ggtags
  :commands ggtags-mode
  :diminish ggtags-mode)

(use-package ag
  :commands (ag ag-files ag-regexp ag-project ag-dired)
  :config (setq ag-highlight-search t
                ag-reuse-buffers t))

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

(eval-after-load 'whitespace '(diminish 'whitespace-mode))

(eval-after-load 'eldoc '(diminish 'eldoc-mode))
;; (eval-after-load 'auto-complete '(diminish 'auto-complete-mode))
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

(use-package editorconfig
  :defer t)
(use-package dtrt-indent
  :defer 3
  :config
  (dtrt-indent-mode)
  (setq dtrt-indent-min-quality 60
        dtrt-indent-verbosity 3))

(use-package origami
  :commands origami-mode
  :after hydra
  :init
  (defhydra hydra-folding (:color red :hint nil)
    "
_o_pen node    _n_ext fold       toggle forw_a_rd    _u_ndo            _F_ill column: %`fill-column
_c_lose node   _p_revious fold   toggle _A_ll        _r_edo            e_x_it
_z_oom on node
"
    ("o" origami-open-node)
    ("c" origami-close-node)
    ("z" origami-show-only-node)
    ("u" origami-undo)
    ("r" origami-redo)
    ("n" origami-next-fold)
    ("p" origami-previous-fold)
    ("a" origami-forward-toggle-node)
    ("A" origami-toggle-all-nodes)
    ("F" fill-column)
    ("x" nil :color blue))
  :bind (:map evil-normal-state-map
              (",z" . hydra-folding/body)))

(use-package flycheck
  :commands flycheck-mode
  :init (global-flycheck-mode)
  :defer t
  :bind (:map evil-normal-state-map
              (",ge" . flycheck-next-error)
              (",gE" . flycheck-previous-error))
)

;; set prefix for lsp-command-keymap
;; all lsp commands: https://github.com/emacs-lsp/lsp-mode#commands
(setq lsp-keymap-prefix "C-l")
(use-package lsp-mode
  :defer t
  :init
  (setq lsp-prefer-flymake nil
        ;; doesn't work with pyls :(
        lsp-enable-snippet nil)
  :hook (
         ;; python needs first to be in the right virtualenv
         (python-mode . lsp)
         (scala-mode . lsp)
         (lsp-mode . lsp-lens-mode)
         )
  :commands lsp
  )

(use-package lsp-ui
  :defer t
  :commands lsp-ui-mode
  :bind (:map evil-normal-state-map
              (",B?" . netrom/lsp-hydra/body))
  :config
  ;; https://www.mortens.dev/blog/emacs-and-the-language-server-protocol/
  (setq netrom--general-lsp-hydra-heads
        '(;; Xref
          ("d" xref-find-definitions "Definitions" :column "Xref")
          ("D" xref-find-definitions-other-window "-> other win")
          ("r" xref-find-references "References")
          ("s" counsel-ag "Search")

          ;; Peek
          ("C-d" lsp-ui-peek-find-definitions "Definitions" :column "Peek")
          ("C-r" lsp-ui-peek-find-references "References")
          ("C-i" lsp-ui-peek-find-implementation "Implementation")

          ;; LSP
          ("p" lsp-describe-thing-at-point "Describe at point" :column "LSP")
          ("C-a" lsp-execute-code-action "Execute code action")
          ("R" lsp-rename "Rename")
          ("t" lsp-goto-type-definition "Type definition")
          ("i" lsp-goto-implementation "Implementation")
          ("f" lsp-ui-imenu "Filter funcs/classes (Helm)")
          ("C-c" lsp-describe-session "Describe session")

          ;; Flycheck
          ("l" lsp-ui-flycheck-list "List errs/warns/notes" :column "Flycheck"))

        netrom--misc-lsp-hydra-heads
        '(;; Misc
          ("q" nil "Cancel" :column "Misc")
          ("b" pop-tag-mark "Back")))

  ;; Create general hydra.
  (eval `(defhydra netrom/lsp-hydra (:color blue :hint nil)
           ,@(append
              netrom--general-lsp-hydra-heads
              netrom--misc-lsp-hydra-heads)))

  )

(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)

;; company-mode
(use-package company
  :diminish company-mode
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  :bind (("C-," . company-complete)
         :map minibuffer-local-map
         ;; give way in minibuffer to company keymap
         ("\M-n" . nil))
  :config
  ;; company dabbrev backend downcase everything by default
  (setq company-dabbrev-downcase nil)
  (setq company-selection-wrap-around t)
  ;; (push 'company-elisp company-backends)
  ;; (push 'company-yasnippet company-backends)
  )

(use-package company-quickhelp
  :defer 4
  :config
  (company-quickhelp-mode))

(use-package company-lsp
  :defer t
  :pin melpa
  :config
  (setq company-lsp-cache-candidates 'auto)
  (setq company-minimum-prefix-length 1
        company-idle-delay 0.0)         ;default is 0.2
  ;; https://www.mortens.dev/blog/emacs-and-the-language-server-protocol/
  ;; Disable client-side cache because the LSP server does a better job.
  ;; (setq company-transformers nil
  ;;       company-lsp-async t
  ;;       company-lsp-cache-candidates nil)
)

;; Use the Debug Adapter Protocol for running tests and debugging
(use-package posframe
  ;; Posframe is a pop-up tool that must be manually installed for dap-mode
  )
(use-package dap-mode
  :pin melpa
  :defer t
  :commands (dap-ui-mode dap-mode dap-hydra)
  :hook (
         (lsp-mode . dap-mode)
         (lsp-mode . dap-ui-mode)
         ;; enables mouse hover support
         (lsp-mode . dap-tooltip-mode)
         ;; use tooltips for mouse hover
         ;; if it is not enabled `dap-mode' will use the minibuffer.
         ;; (lsp-mode . tooltip-mode)
         ;; displays floating panel with debug buttons
         ;; requies emacs 26+
         ;; (lsp-mode . dap-ui-controls-mode)
         (dap-stopped . (lambda (arg) (call-interactively #'dap-hydra)))
         (python-mode . (lambda ()(require 'dap-python)))
         ;; (dap-server-log-mode . XXX repaint last entry with
         ;; ansi-colorizing, see function colorize-compilation-buffer)
         )

  :bind (:map evil-normal-state-map
              (",dd" . dap-debug)
              (",dl" . dap-debug-last)
              (",de" . dap-eval-thing-at-point)
              (",dD" . dap-debug-recent)
              ("<f5>" . dap-continue)
              ("<f9>" . dap-breakpoint-toggle)
              ("<f10>" . dap-next)
              ("<f11>" . dap-step-in)
              ("S-<f11>" . dap-step-out)
              :map evil-visual-state-map
              (",d:" . dap-eval-region )
              :map dap-server-log-mode-map
              ( "n" . dap-next )
              ( "i" . dap-step-in )
              ( "o" . dap-step-out )
              ( "c" . dap-continue )
              ( "L" . dap-ui-locals )
              ( "S" . dap-ui-sessions )
              ( "E" . dap-ui-expressions )
              ( "B" . dap-ui-breakpoints )
              ( "R" . dap-ui-repl )
              ( "l" . dap-go-to-output-buffer )
              ( "q" . dap-disconnect )
              ;; H : Continue until Point
              ( ":" . dap-eval )
              ( "b" . dap-breakpoint-add )
              ( "u" . dap-breakpoint-delete )
              ( ">" . dap-switch-stack-frame )
              ( "<" . dap-switch-stack-frame )
              ;; g? : Help
              ;; J : Jump to debugger location
              ( "R" . dap-restart-frame )
              :map +dap-running-session-mode-map
              ( ",dn" . dap-next )
              ( ",di" . dap-step-in )
              ( ",do" . dap-step-out )
              ( ",dc" . dap-continue )
              ( ",dL" . dap-ui-locals )
              ( ",dS" . dap-ui-sessions )
              ( ",dE" . dap-ui-expressions )
              ( ",dB" . dap-ui-breakpoints )
              ( ",dR" . dap-ui-repl )
              ( ",dt" . dap-go-to-output-buffer )
              ( ",dq" . dap-disconnect )
              ;; H : Continue until Point
              ( ",d:" . dap-eval )
              ( ",dba" . dap-breakpoint-add )
              ( ",dbu" . dap-breakpoint-delete )
              ( ",dbb" . dap-breakpoint-toggle )
              ( ",dbc" . dap-breakpoint-condition )
              ( ",dbC" . dap-breakpoint-hit-condition )
              ( ",dbl" . dap-breakpoint-log-message )
              ( ",d>" . dap-switch-stack-frame )
              ( ",d<" . dap-switch-stack-frame )
              ;; g? : Help
              ;; J : Jump to debugger location
              ( ",dR" . dap-restart-frame )
              )
  :config
  ;; https://github.com/emacs-lsp/dap-mode/wiki/How-to-activate-minor-modes-when-stepping-through-code
  (define-minor-mode +dap-running-session-mode
    "A mode for adding keybindings to running sessions"
    nil
    nil
    (make-sparse-keymap)
    (evil-normalize-keymaps) ;; if you use evil, this is necessary to update the keymaps
    ;; The following code adds to the dap-terminated-hook
    ;; so that this minor mode will be deactivated when the debugger finishes
    (when +dap-running-session-mode
      (let ((session-at-creation (dap--cur-active-session-or-die)))
        (add-hook 'dap-terminated-hook
                  (lambda (session)
                    (when (eq session session-at-creation)
                      (+dap-running-session-mode -1)))))))

  ;; Activate this minor mode when dap is initialized
  (add-hook 'dap-session-created-hook '+dap-running-session-mode)

  ;; Activate this minor mode when hitting a breakpoint in another file
  (add-hook 'dap-stopped-hook '+dap-running-session-mode)

  ;; Activate this minor mode when stepping into code in another file
  (add-hook 'dap-stack-frame-changed-hook (lambda (session)
                                            (when (dap--session-running session)
                                              (+dap-running-session-mode 1))))
)

;; Use the Tree View Protocol for viewing the project structure and triggering compilation
(use-package lsp-treemacs
  :disabled t
  :after treemacs
  :defer t
  :pin melpa
  :config
  (lsp-metals-treeview-enable t)
  (setq lsp-metals-treeview-show-when-views-received t)
  (lsp-treemacs-sync-mode 1)
  )

(use-package dumb-jump
  :bind (:map evil-normal-state-map
              (",gd" . dumb-jump-go)
              (",gD" . dumb-jump-go-other-window))
  :config (dumb-jump-mode))

;;; Compilation
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  "Accept coloured output from testing."
  (ansi-color-apply-on-region compilation-filter-start (point)))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)
(setq compilation-scroll-output 'first-error
      compilation-always-kill t
      compilation-auto-jump-to-first-error t
      compilation-context-lines 5)      ; auto scroll in compilation buffer

(add-hook 'prog-mode-hook
          (lambda () (progn
                       ; all code buffers with nlinum
                       ; (so much faster than linum!)
                       ;; (nlinum-mode 1) ; line # are overrated
                       ;; (ggtags-mode 1) ; trial
                       (subword-mode) ; camelcase moves
                       (turn-on-auto-fill)
                       ;; auto-fill comments and only them
                       ;; (setq-local comment-auto-fill-only-comments t)
                       (rainbow-delimiters-mode t)
                       (eldoc-mode)
                       (origami-mode)
                       (editorconfig-mode 1)
                       (electric-indent-local-mode)
                       )))
;; (setq linum-format " %3d ")    ; remove graphical glitches with fringe

;; https://github.com/wbolster/emacs-direnv
(use-package direnv
  :after exec-path-from-shell
  :config
  (direnv-mode))

;; Help
(use-package dash-at-point
  :if (memq window-system '(mac))
  :commands (dash-at-point dash-at-point-docset)
  :bind (:map evil-normal-state-map (",hd" . dash-at-point)))
(define-key evil-normal-state-map (kbd ",hI") 'info)

(use-package helm-dash
  :commands (helm-dash-at-point helm-dash)
  :bind (:map evil-normal-state-map (",hd" . helm-dash-at-point)))

(use-package whitespace
  :commands (whitespace-mode)
  :config
  (setq whitespace-style '(face tabs lines-tail empty trailing)))
(use-package whitespace-cleanup-mode
  :diminish whitespace-cleanup-mode
  :defer t
  :init (global-whitespace-cleanup-mode))

;; LANGUAGES

(require 'setup-scala)

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

(require 'setup-haskell)

(require 'setup-ruby)

;; Web: HTML/CSS
(add-to-list 'auto-mode-alist '("\\.html$" . html-mode))

;; (use-package zencoding-mode
;;   :mode "\\.html\\'")
;; (add-hook 'sgml-mode-hook 'zencoding-mode) ;; Auto-start on any markup modes
;; (add-hook 'sgml-mode-hook (lambda () (progn (nlinum-mode 1))))
;; after deleting a tag, indent properly
(defadvice sgml-delete-tag (after reindent activate)
  (indent-region (point-min) (point-max)))
(use-package css-mode
  :commands css-mode
  :config
  (sp-local-pair 'css-mode "{" nil :post-handlers '(("||\n[i]" "RET")))
  (setq css-indent-offset 2)
  (use-package css-eldoc)
  )
(use-package scss-mode
  :ensure t
  :mode "\\.scss\\'")

(add-hook 'sgml-mode-hook
          (lambda ()
            ;; Default indentation to 2, but let SGML mode guess, too.
            (set (make-local-variable 'sgml-basic-offset) 4)
            (sgml-guess-indent)))

(require 'setup-js)

(use-package json-mode
  :mode "\\.json$"
  :defer t
  :config
  (setq js-indent-level 2)
  (add-to-list 'auto-mode-alist '("\\.eslintrc\\'" . json-mode)))

(use-package yaml-mode
  :mode (("\\.yml$" . yaml-mode)
         ("\\.yaml$" . yaml-mode))
  :defer t)

;; gnuplot
(use-package gnuplot
  :commands (gnuplot-mode gnuplot-make-buffer))
;; R
;;(require 'ess-site)
;; ESS config
;; (use-package ess-site
;;   :mode ("\\.R\\'" . R-mode)
;;   :commands R
;;   :config
;;   (eval-after-load "comint"
;;     '(progn
;;        (define-key comint-mode-map [up]
;;          'comint-previous-matching-input-from-input)
;;        (define-key comint-mode-map [down]
;;          'comint-next-matching-input-from-input)

;;        ;; also recommended for ESS use --
;;        (setq comint-scroll-to-bottom-on-output 'others)
;;        (setq comint-scroll-show-maximum-output t)
;;        ;; somewhat extreme, almost disabling writing in *R*, *shell* buffers above prompt:
;;        (setq comint-scroll-to-bottom-on-input 'this)
;;        )))
;; (evil-define-key 'normal ess-mode-map ",ho" 'ess-display-help-on-object)
;; (evil-define-key 'visual ess-mode-map ",l" 'ess-eval-region)
;; (evil-define-key 'visual ess-mode-map ",L" 'ess-eval-region-and-go)

;; also see comint section

(require 'setup-clojure)

;; Shell scripting
(add-to-list 'auto-mode-alist '("\\.zsh$" . shell-script-mode))

;; Elixir
(use-package elixir-mode
  :commands elixir-mode
  :config

  (add-hook 'elixir-mode-hook 'alchemist-mode)

  (sp-with-modes '(elixir-mode)
    (sp-local-pair "fn" "end"
                   :when '(("SPC" "RET"))
                   :actions '(insert navigate))
    (sp-local-pair "do" "end"
                   :when '(("SPC" "RET"))
                   :post-handlers '(sp-ruby-def-post-handler)
                   :actions '(insert navigate))))

(use-package alchemist
  :commands alchemist-mode
  :config
  (setq alchemist-hooks-test-on-save t))

;; OSX launchd plist
(define-auto-insert "\.plist" ["template.plist" hub/autoinsert-yas-expand])
(add-to-list 'auto-mode-alist '("\\.plist$" . xml-mode))

;; Python
(defun disable-flycheck ()
  "Flycheck doesn't understand virtualenv. And lsp provides the equivalent."
  (flycheck-mode -1))

;; switch to the right
(defvar *python-current-env* nil)
(defun hub/workon ()
  "To load the virtual env with the same name as the root dir."
  (interactive)
  ;; (message "running hub/workon before: %s"
  ;;          *python-current-env*)
  (let* ((rootdir (directory-file-name (projectile-project-root)))
         (env (file-name-nondirectory rootdir)))
    (when (and env
               (not (equal env *python-current-env*)))
      (progn
        (setf *python-current-env* env)
        (pyvenv-workon env)
        (message "Current python env: %s"
                 *python-current-env*))))
  (save-some-buffers t))

(use-package pyvenv
  :ensure nil
  :commands (pyvenv-activate pyvenv-workon)
  :init
  (add-to-list 'exec-path "~/.pyenv/shims")
  (setenv "WORKON_HOME" "~/.virtualenvs")
  (add-hook 'pyvenv-post-activate-hooks 'lsp)
  (add-hook 'pyvenv-post-activate-hooks 'pyvenv-restart-python)
  :hook (
         (python-mode . disable-flycheck)
         (python-mode . pyvenv-mode)
         (python-mode . hub/workon)
         )
  :config
  (evil-define-key 'normal python-mode-map ",gr" 'run-python)
  )

(use-package pydoc
  :commands (pydoc pydoc-at-point pydoc-browse))

; tweaking to get my proper setup
; OSX
; * iTerm2
; Preferences > Keys (tab) > Remap Left Command to Left Option
; Preferences > Profile > Left Option: Meta + Esc
; * Numpad (for calc): remap keypad-dot to Option+Shift+Keypad-dot

(put 'narrow-to-region 'disabled nil)
(put 'erase-buffer 'disabled nil)

;; at the end, for windows to pick up the font change
(require 'setup-ui)

(provide 'init)
;;; init.el ends here
