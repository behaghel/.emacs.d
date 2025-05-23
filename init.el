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

;; Ensure `user-emacs-directory' is the directory of this file when running in
;; environments such as GitHub actions where HOME might not point to the
;; repository root.
(setq user-emacs-directory
      (file-name-directory (or load-file-name buffer-file-name)))

;; On Windows: set HOME environment variable and put .emacs.d in there!

(defvar native-comp-deferred-compilation-deny-list nil)
;; to stop M-x customize to pollute my init.el: http://emacsblog.org/2008/12/06/quick-tip-detaching-the-custom-file/
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

(setq hub-lisp-dir (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path hub-lisp-dir)  ; to include my .el

(setq settings-dir
      (expand-file-name "settings" user-emacs-directory))
;; Set up load path
(add-to-list 'load-path settings-dir)
(add-to-list 'load-path (expand-file-name "dev" settings-dir))

(setq user-mail-address "behaghel@gmail.com")
(setq user-full-name "Hubert Behaghel")

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq straight-use-package-by-default t
      ;; breaks org even when selectively depth set to full
      ;; straight-vc-git-default-clone-depth 1
      )
(use-package diminish)
(straight-use-package 'use-package)

(setq use-package-verbose t
      use-package-always-defer nil
      use-package-always-ensure nil)

(require 'auth-source-pass)
(auth-source-pass-enable)
(use-package pinentry)
(setq epa-pinentry-mode 'loopback)

(require 'hub-utils)

(require 'setup-general)
(server-start)

(require 'setup-evil)

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
         ("M-(" . sp-backward-unwrap-sexp)
         ("M-)" . sp-unwrap-sexp)
         ("C-<down>" . sp-down-sexp)
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
        ;; (kbd ",{") #'sp-backward-up-sexp
        ;; Finds closing ')' of the current list.
        ;; (kbd ",}") #'sp-up-sexp
        (kbd ",s") #'sp-backward-up-sexp
        (kbd ",t") #'sp-down-sexp
        (kbd ",(") #'sp-backward-up-sexp
        (kbd ",)") #'sp-up-sexp
        ;; Go to the start of current/previous sexp
        (kbd "[[") #'sp-backward-sexp
        ;; Go to the start of next sexp.
        (kbd "]]") #'sp-forward-sexp
        (kbd ",r") #'sp-next-sexp
        (kbd ",c") #'sp-previous-sexp
        ;; (define-key evil-motion-state-map "S" 'evil-window-top)
        ;; (define-key evil-motion-state-map "s" 'evil-previous-line)
        ))
    (add-hook 'smartparens-enabled-hook #'evil-smartparens-mode))
  :config
  (require 'smartparens-config)
  (add-to-list 'sp-ignore-modes-list 'org-mode)
  (smartparens-global-mode t)
  (show-smartparens-global-mode)
  (smartparens-global-strict-mode))

(use-package hydra)

(use-package general)

(require 'setup-completion)

;;; Yasnippet
(use-package yasnippet
  :defer t
  ;; :ensure t
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

;;; auto-insert-mode is Emacs file templating
(auto-insert-mode 0)        ; no more the default, use auto-insert manually
(setq auto-insert-directory "~/.emacs.d/insert/")
;; you can use yasnippet to expand it
;; see: http://www.emacswiki.org/emacs/AutoInsertMode
;; the standard emacs way use skeleton
;; see: https://github.com/cinsk/emacs-scripts/blob/8212d714d5c6f6b95e873e8688b30ba130d07775/xskel.el
;; also: http://www.howardism.org/Technical/Emacs/templates-tutorial.html
(defun hub/autoinsert-yas-expand (&optional expand-env)
  "Replace text in yasnippet template optionally passing EXPAND-ENV (let-style)."
  (yas-expand-snippet (buffer-string) (point-min) (point-max) expand-env))
(define-auto-insert "\.org\'" ["template.org" hub/autoinsert-yas-expand])
;; orj is an extension I invented: org-revealJS
(define-auto-insert "\.orj\'" ["template.orj" hub/autoinsert-yas-expand])

(require 'setup-git)

;; Editing text
(add-hook 'text-mode-hook 'turn-on-auto-fill) ; auto-wrap
(setq sentence-end-double-space nil)    ; one space is enough after a period to end a sentence
(define-key evil-normal-state-map (kbd ",bs") 'flyspell-mode)

;; Writing with style
;; http://rs.io/software-writers-tools-improve-writing/
;; chase weasel words, count words and more
;; https://github.com/sachac/artbollocks-mode
;; http://sachachua.com/blog/2011/12/emacs-artbollocks-mode-el-and-writing-more-clearly/
(use-package artbollocks-mode
  :commands (artbollocks-mode)
  :bind (:map evil-normal-state-map     ;TODO: don't pollute global normal map
              (",bw" . artbollocks-count-words)
              (",bg" . artbollocks-grade-level)
              (",be" . artbollocks-reading-ease)
              (",br" . artbollocks-readability-index)))

(use-package writeroom-mode
  :commands (writeroom-mode)
  :config
  (add-to-list 'writeroom-local-effects #'variable-pitch-mode))
;; inspiration: https://protesilaos.com/codelog/2020-07-16-emacs-focused-editing/

(use-package languagetool
  :commands (languagetool-check)
  :bind (:map evil-normal-state-map
              (",bc" . langtool-check))
  :config
  ;; style and grammar checker
  (setq langtool-language-tool-jar "/usr/local/Cellar/languagetool/2.6/libexec/languagetool.jar")
  (define-key evil-normal-state-map (kbd ",bg") 'langtool-check))

(require 'setup-org)

;; asciidoc
(add-hook 'adoc-mode-hook (lambda() (buffer-face-mode t)))
(add-to-list 'auto-mode-alist (cons "\\.asciidoc\\'" 'adoc-mode))

;; Markdown
(use-package markdown-mode
  :defer t
  :mode (("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)
         ("README\\.md\\'" . gfm-mode))
  :init
  ;; in github flavour markdown, \n are enforced strictly
  (add-hook 'gfm-mode-hook (lambda () (auto-fill-mode -1)))
  :commands (markdown-mode)
  :config
  (setq markdown-command "pandoc -c file://${HOME}/.emacs.d/github-pandoc.css --from gfm -t html5 --mathjax --highlight-style pygments --standalone --quiet")

  (evil-define-key 'normal markdown-mode-map (kbd ",il") 'markdown-insert-link)
  (evil-define-key 'normal markdown-mode-map (kbd ",iH") 'markdown-insert-header-dwim)
  (evil-define-key 'normal markdown-mode-map (kbd ",ih") 'markdown-insert-header-setext-dwim)
  (evil-define-key 'normal markdown-mode-map (kbd ",i2") 'markdown-insert-header-setext-2)
  (evil-define-key 'normal markdown-mode-map (kbd ",i1") 'markdown-insert-header-setext-1)
  (evil-define-key 'normal markdown-mode-map (kbd ",ev") 'markdown-preview)
  (evil-define-key 'normal markdown-mode-map (kbd "M->") 'markdown-demote)
  (evil-define-key 'normal markdown-mode-map (kbd "M-<") 'markdown-promote)
  (evil-define-key 'normal markdown-mode-map (kbd ",eV") 'markdown-export-and-preview))

                                        ; CODING
(require 'dev-common)

(require 'setup-perspective)
;; at the end, for windows to pick up the font change
(require 'setup-ui)
;; (require 'setup-multiple-cursors)
(put 'narrow-to-region 'disabled nil)
(put 'erase-buffer 'disabled nil)

;; (require 'setup-treemacs)
(require 'setup-brain)
(require 'setup-private nil t)

;; (use-package use-package-ensure-system-package
;;   :ensure t)

;; APPS
;; (require 'setup-erc)
;; (require 'setup-twitter)
(require 'setup-ai)
(require 'setup-elfeed)
(require 'setup-email nil t)
(require 'setup-dired)
(require 'setup-eshell)
(use-package restclient
  :commands (restclient-mode))

;; Use emacs to edit textarea in Chrome
(use-package edit-server
  :if window-system
  ;; :ensure t
  :defer 5
  :init
  (add-hook 'after-init-hook 'server-start t)
  (add-hook 'after-init-hook 'edit-server-start t)
  :config
  (setq edit-server-default-major-mode 'markdown-mode))

(provide 'init)
;;; init.el ends here
