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

(straight-use-package 'use-package)

;(eval-when-compile
  ;(require 'use-package)
  ;(require 'use-package-ensure)
  ;(setq use-package-verbose nil)
  ;(setq use-package-always-ensure t))

(use-package diminish)
(setq use-package-verbose t
      use-package-always-defer nil
      use-package-always-ensure nil)

(require 'auth-source-pass)
(auth-source-pass-enable)
(use-package pinentry)
(setq epa-pinentry-mode 'loopback)

(require 'hub-utils)

(require 'setup-general)

(require 'setup-evil)

(use-package general)

(require 'setup-completion)

(require 'setup-dired)

(require 'setup-eshell)

(use-package restclient
  :commands (restclient-mode))
;; never used it but could prove useful
;; (use-package company-restclient
;;   :after (restclient company)
;;   :config
;;   (add-to-list 'company-backends 'company-restclient))
;; (use-package ob-restclient
;;   :after (org)
;;   :config
;;   (org-babel-do-load-languages
;;    'org-babel-load-languages
;;    '((restclient . t))))

;; (use-package pinboard)

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

(require 'setup-blog)

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
(require 'setup-git)
;; Ediff
(setq ediff-diff-options "-w")
(setq ediff-split-window-function 'split-window-horizontally)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

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
        (kbd ",{") #'sp-backward-up-sexp
        ;; Finds closing ')' of the current list.
        (kbd ",}") #'sp-up-sexp
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

;; Navigating
(use-package ggtags
  :commands ggtags-mode
  :diminish ggtags-mode)

(use-package ag
  :commands (ag ag-files ag-regexp ag-project ag-dired)
  :config (setq ag-highlight-search t
                ag-reuse-buffers t))

;; Use the Tree View Protocol for viewing the project structure and triggering compilation
(use-package lsp-treemacs
  :disabled t
  :after (treemacs)
  :defer t
  ;; :pin melpa
  :config
  (lsp-metals-treeview-enable t)
  (setq lsp-metals-treeview-show-when-views-received t)
  (lsp-treemacs-sync-mode 1)
  )

;; Indenting
(setq-default indent-tabs-mode nil)     ; no tabs, only spaces
;; don't delete tabs one space at a time
(setq backward-delete-char-untabify-method 'hungry)
(use-package editorconfig
  :defer t
  :diminish editorconfig-mode)
(use-package dtrt-indent
  :disabled t
  :defer 3
  :config
  (dtrt-indent-mode)
  (setq dtrt-indent-min-quality 60
        dtrt-indent-verbosity 3))
; automatically indent yanked text in prog-modes
(dolist (command '(yank yank-pop))
  (eval `(defadvice ,command (after indent-region activate)
           (and (not current-prefix-arg)
                (derived-mode-p 'prog-mode)
                (let ((mark-even-if-inactive transient-mark-mode))
                  (indent-region (region-beginning) (region-end) nil))))))


;; highlight TODO, FIXME, etc.
(add-hook 'prog-mode-hook 'hub/font-lock-comment-annotations)
;; (show-paren-mode 1)                     ; highlight matching brackets
(setq treesit-font-lock-level 4)

(use-package origami
  :commands origami-mode
  :after (hydra)
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
              (",Z" . hydra-folding/body)))

;; Use the Debug Adapter Protocol for running tests and debugging
(use-package posframe
  ;; Posframe is a pop-up tool that must be manually installed for dap-mode
  )

;; Debugging
(use-package dap-mode
  ;; :pin melpa
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
         ;; (dap-server-log-mode . XXX repaint last entry with
         ;; ansi-colorizing, see function colorize-compilation-buffer)
         )
  :bind (:map evil-normal-state-map
              (",dd" . dap-debug)
              (",dl" . dap-debug-last)
              (",de" . dap-eval-thing-at-point)
              (",dD" . dap-debug-recent)
              (",dc" . dap-continue)
              (",dB" . dap-breakpoint-toggle)
              (",dn" . dap-next)
              (",dt" . dap-step-in)
              (",ds" . dap-step-out)
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
              ( ",dt" . dap-step-in )
              ( ",ds" . dap-step-out )
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

(use-package realgud
  :disabled t                           ; install error: can't install org-mac-link??
  :commands (realgud:gdb realgud:byebug realgud:pry))

;;; Building
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

(use-package flycheck
  :commands flycheck-mode
  :init (global-flycheck-mode)
  :defer t
  :disabled t
  :config
  (define-key evil-normal-state-map (kbd "g)") 'flycheck-next-error)
  (define-key evil-normal-state-map (kbd "g(") 'flycheck-previous-error)
  (define-key evil-normal-state-map (kbd ",)") 'flycheck-next-error)
  (define-key evil-normal-state-map (kbd ",(") 'flycheck-previous-error)
)

;; Compilation
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
                       (flyspell-prog-mode)
                       (turn-on-auto-fill)
                       ;; auto-fill comments and only them
                       ;; (setq-local comment-auto-fill-only-comments t)
                       (eldoc-mode)
                       (origami-mode)
                       (editorconfig-mode 1)
                       (electric-indent-local-mode)
                       (define-key evil-normal-state-map (kbd ",bb") 'compile)
                       (define-key evil-normal-state-map (kbd ",br") 'recompile)
                       (define-key evil-normal-state-map (kbd ",bx") 'kill-compilation)
                       (define-key evil-insert-state-map (kbd "M-RET") 'indent-new-comment-line)
                       )))
;; (setq linum-format " %3d ")    ; remove graphical glitches with fringe

;; https://github.com/wbolster/emacs-direnv
(use-package direnv
  :after (exec-path-from-shell)
  :config
  (direnv-mode))

;; Help
(which-function-mode 1)                 ; which function the point is in
(use-package dash-at-point
  :disabled t
  :if (memq window-system '(mac))
  :commands (dash-at-point dash-at-point-docset)
  :bind (:map evil-normal-state-map (",hd" . dash-at-point)))
(define-key evil-normal-state-map (kbd ",hI") 'info)

(use-package whitespace
  :diminish global-whitespace-mode
  :config
  (setq whitespace-style '(face tabs lines-tail empty trailing))
  (setq whitespace-global-modes '(not org-mode))
  (global-whitespace-mode))
(use-package whitespace-cleanup-mode
  :diminish whitespace-cleanup-mode
  :defer t
  :init (global-whitespace-cleanup-mode))

;; LANGUAGES

(require 'setup-scala)

; Emacs Lisp
(require 'jka-compr) ; find-tag to be able to find .el.gz
(evil-define-key 'normal lisp-mode-shared-map
  ",." 'find-function
  ",hf" 'describe-function
  ",hv" 'describe-variable
  ",hc" 'describe-char
  ",el" 'eval-last-sexp
  ",il" 'eval-print-last-sexp)
(eval-after-load 'eldoc '(diminish 'eldoc-mode))

;; Scheme
(use-package geiser
  :defer t
  :commands geiser-connect
  :config
  (evil-define-key 'normal geiser-mode-map ",gr" 'switch-to-geiser)
  (evil-define-key 'normal geiser-mode-map ",gR" 'geiser-mode-switch-to-repl-and-enter)
  (evil-define-key 'normal geiser-mode-map ",el" 'geiser-load-current-buffer)
  (evil-define-key 'normal geiser-mode-map ",ii" 'geiser-doc-symbol-at-point)
  (evil-define-key 'normal geiser-mode-map ",." 'geiser-edit-symbol-at-point)
  )

(require 'setup-haskell)

(require 'setup-ruby)

;; Web: HTML/CSS
(add-to-list 'auto-mode-alist '("\\.html$" . html-mode))

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
  ;; :ensure t
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

;; OSX launchd plist
(define-auto-insert "\.plist\'" ["template.plist" hub/autoinsert-yas-expand])
(add-to-list 'auto-mode-alist '("\\.plist$" . xml-mode))

;; Python
(require 'setup-python)

(put 'narrow-to-region 'disabled nil)
(put 'erase-buffer 'disabled nil)

(require 'setup-perspective)
;; at the end, for windows to pick up the font change
(require 'setup-ui)
;; (require 'setup-treemacs)
(require 'setup-elfeed)
(require 'setup-email nil t)
(require 'setup-brain)
(require 'setup-private nil t)

;; NixOS
(use-package nix-mode
  :mode "\\.nix\\'"
  :config
  (sp-local-pair 'nix-mode "{" nil :post-handlers '(("||\n[i]" "RET")))
  )

(use-package use-package-ensure-system-package
  :ensure t)
(use-package eglot
  :ensure nil
  ;; ensure `nil' is on the PATH (lsp server for nix)
  :ensure-system-package
  (nil . "nix profile install github:oxalica/nil")
  :config
  (add-to-list 'eglot-server-programs '(nix-mode . ("nil")))
  :hook
  (nix-mode . eglot-ensure)
  (scala-ts-mode . eglot-ensure))

;; AWK
(add-hook 'awk-mode-hook (lambda ()
                           (setq c-basic-offset 2)))

;; (require 'setup-erc)
;; (require 'setup-twitter)
;; (require 'setup-multiple-cursors)

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
