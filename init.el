;;; emacs --- Hubert's .emacs file
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
;;             - ,g == anything to find or go to
;;             - ,e == anything to execute
;; 
;;; Code:

(setq user-mail-address "behaghel@gmail.com")
(setq user-full-name "Hubert Behaghel")
(toggle-debug-on-error)

;; (set-face-attribute 'default nil :font "Droid Sans Mono-12")
(set-face-attribute 'default nil :font "Source Code Pro-12")

;; install
(add-to-list 'load-path "~/.emacs.d/")  ; to include my .el
(require 'package)
(require 'cl)
(add-to-list 'package-archives 
	     '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/"))
; required to find melpa-installed package after restart at init time
(package-initialize)

(setq url-http-attempt-keepalives nil)

(defvar ensure-packages '(better-defaults cl-lib dash diminish
  epl evil evil-leader evil-nerd-commenter exec-path-from-shell
  expand-region f flx-ido flycheck flycheck-haskell gh gist git
  git-commit-mode git-gutter-fringe+ git-rebase-mode goto-chg
  haskell-mode ido-at-point ido-ubiquitous ido-vertical-mode
  logito magit multiple-cursors org ox-reveal pcache pkg-info
  popwin projectile rainbow-delimiters s sbt-mode smartparens
  smex solarized-theme surround undo-tree yagist yasnippet
  zencoding-mode)
  "A list of packages to ensure are installed at launch.")

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

(load-theme 'solarized-light t)

; Mac

;; Are we on a mac? Thanks @magnars
(setq is-mac (equal system-type 'darwin))

;; fix for Mac OS X PATH in Emacs GUI
(when is-mac
  (exec-path-from-shell-initialize))

(setq mac-command-key-is-meta t)
(setq mac-command-modifier 'meta)
(setq mac-option-key-is-meta nil)
(setq mac-option-modifier nil)

;;;;;;;;;;;;
; General Behaviour
;;;;;;;;;;;;

;; No annoying buffer for completion, compilation, help...
(require 'popwin)
(popwin-mode 1)

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

;; stolen from https://github.com/magnars/.emacs.d/blob/master/key-bindings.el
;; Transpose stuff with M-t
(global-unset-key (kbd "M-t")) ;; which used to be transpose-words
(global-set-key (kbd "M-t l") 'transpose-lines)
(global-set-key (kbd "M-t w") 'transpose-words)
(global-set-key (kbd "M-t s") 'transpose-sexps)
(global-set-key (kbd "M-t p") 'transpose-params)

(require 'smartparens-config)
(smartparens-global-mode t)

;; Transpose
;; stolen from @magnars
(global-set-key (kbd "C-<right>") 'sp-forward-slurp-sexp)
(global-set-key (kbd "C-<left>") 'sp-forward-barf-sexp)
(global-set-key (kbd "C-S-<left>") 'sp-backward-slurp-sexp)
(global-set-key (kbd "C-S-<right>") 'sp-backward-barf-sexp)

(defun transpose-params ()
  "Presumes that params are in the form (p, p, p) or {p, p, p} or [p, p, p]"
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

;;; Dired
(require 'dired-x)

;;; Yasnippet
;(yas-global-mode 1)

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


;;; org-mode
(add-to-list 'auto-mode-alist '("\\.\\(org|org_archive\\|txt\\)$" . org-mode))
(setq org-directory "~/Documents/org")
(setq org-default-notes-file (concat org-directory "/notes.org")) 
;; org-agenda-files should be a list of files and not a dir
;; prefer C-c [ to add and C-c ] to remove file from this list
;; (setq org-agenda-files org-directory)
(setq org-hide-leading-stars t)
(setq org-startup-indented t)
(setq org-return-follows-link t)
(setq org-src-fontify-natively t)
;; (setq org-reveal-root (getenv "REVEAL_JS_ROOT_URL"))
;; (require 'org-install)
;; (require 'org-habit)
(autoload 'google-contacts "google-contacts" "Google Contacts." t)
(add-hook 'org-mode 'auto-fill-mode)

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)


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
(define-key evil-window-map "_" 'split-window-horizontally)
(define-key evil-window-map "|" 'split-window-vertically)
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
(define-key evil-normal-state-map "z" nil) ; to make z a prefix key
;; following translation only occur when z is a prefix (not already bound)
(define-key key-translation-map (kbd "zs") (kbd "zj"))
(define-key key-translation-map (kbd "zt") (kbd "zk"))
(define-key evil-normal-state-map (kbd "j") 'evil-replace)
(define-key evil-normal-state-map (kbd "l") 'evil-change)
(define-key evil-normal-state-map (kbd "L") 'evil-change-line)
(define-key evil-normal-state-map (kbd "h") 'evil-find-char-to)
(define-key evil-normal-state-map (kbd "H") 'evil-find-char-to-backward)
(define-key evil-normal-state-map (kbd "k") 'evil-substitute)
(define-key evil-normal-state-map (kbd "K") 'evil-change-whole-line)

;;;; Other mapping
(define-key evil-normal-state-map (kbd "C-e") 'evil-end-of-line)
(define-key evil-insert-state-map (kbd "C-e") 'end-of-line)
(define-key evil-insert-state-map (kbd "C-y") 'yank)
(define-key evil-insert-state-map (kbd "M-y") 'yank-pop)
(define-key evil-normal-state-map (kbd ",y") 'hub/copy-buffer-file-name)
(define-key evil-normal-state-map (kbd ",x") 'smex)
(define-key evil-normal-state-map (kbd ",,") 'hub/switch-dwim)
(define-key evil-normal-state-map (kbd ",go") 'hub/switch-to-other-buffer)
;; Switch to another open buffer
(define-key evil-normal-state-map (kbd ",gb") 'switch-to-buffer)
;; Open file
(define-key evil-normal-state-map (kbd ",gf") 'ido-find-file)
;; Browse URL
(define-key evil-normal-state-map (kbd ",gu") 'browse-url)
; open init.el
(define-key evil-normal-state-map (kbd ",ge") (lambda()(interactive)(find-file "~/.emacs.d/init.el")))
; open hubert.org
(define-key evil-normal-state-map (kbd ",go") (lambda()(interactive)(find-file "~/Documents/org/hubert.org")))
;; open file in project
(define-key evil-normal-state-map (kbd ",pf") 'projectile-find-file)

;; Git tools
;; REQUIRES Magit
(define-key evil-normal-state-map (kbd ",vs") 'magit-status) ;; git control panel
(define-key evil-normal-state-map (kbd ",vh") 'magit-file-log) ; Commit history for current file
(define-key evil-normal-state-map (kbd ",vb") 'magit-blame-mode) ; Blame for current file
(define-key evil-normal-state-map (kbd ",vg") 'vc-git-grep) ; Git grep
;; errors and compilation
(define-key evil-normal-state-map (kbd "]c") 'next-error)
(define-key evil-normal-state-map (kbd "[c") 'previous-error)
(define-key evil-normal-state-map (kbd ",cc") 'compile)
(define-key evil-normal-state-map (kbd ",cr") 'recompile)
(define-key evil-normal-state-map (kbd ",ck") 'kill-compilation)
;; evil is crazy
(define-key evil-insert-state-map (kbd "C-d") nil)
;;;; Default state
(evil-set-initial-state 'help-mode 'emacs)
(evil-set-initial-state 'dired-mode 'emacs)
(evil-set-initial-state 'Info 'emacs)
(evil-set-initial-state 'ensime-scalex-mode 'emacs)
(evil-set-initial-state 'erc-mode 'emacs)


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

(defun hub/load-term-theme-locally ()
  "Load the color theme I want to use for term into the current buffer."
  (load-theme-buffer-local 'tango-dark (current-buffer)))

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
                               "]") 'face `(:foreground "blue"))
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
                            (pwd-repl-home (eshell/pwd)) "/")) 'face '(:foreground "Red"))
              (curr-dir-git-branch-string (eshell/pwd))
              (propertize " % " 'face 'default))))
     ;; don't enforce theme colors since it will make the prompt monochrome
     ;; there is only one face for the whole prompt.
     (setq eshell-highlight-prompt nil)
     ;; end of stealing
     
     (require 'eshell-autojump)))

;;; TRAMP
(setq tramp-default-method "ssh")

;;; ERC
;;; start with M-x erc-select
;;; switch channel by switching emacs buffer
;;; switch to last active channel C-c C-SPC
(autoload 'erc-select "erc" "IRC client." t)
(setq erc-echo-notices-in-minibuffer-flag t)
(eval-after-load 'erc
  '(progn
    (setq erc-modules '(autojoin button completion fill
                                 irccontrols list match
                                 menu move-to-prompt netsplit
                                 networks noncommands readonly ring
                                 scrolltobottom services stamp track))
    (setq erc-prompt-for-password nil)  ; use authinfo instead
    (setq erc-fill-function 'erc-fill-static)
    (setq erc-fill-static-center 18)    ; margin for ts + nicks
    (setq erc-timestamp-format "[%H:%M] "
          erc-insert-timestamp-function 'erc-insert-timestamp-left)
    (setq erc-input-line-position -2)
    (require 'erc-match)
    (setq erc-keywords '("hub" "behaghel" "hubert" "hubertb"))
    (setq erc-pals '("aloiscochard"))
    (setq erc-autojoin-mode t)
    (setq erc-autojoin-channels-alist
          '((".*\\.freenode.net" "#emacs" "#scala")
            ("irc.amazon.com" "#ingestion" "#reconciliation")))
    (setq erc-auto-query 'bury)
    (setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
                                    "324" "329" "332" "333" "353" "477"))
    (setq erc-lurker-hide-list '("JOIN" "NICK" "PART" "QUIT"))
    ;; (setq erc-echo-notices-in-minibuffer-flag t)
    ;; (erc-minibuffer-notice t)
    (setq erc-lurker-threshold-time 3600)
))

;;; Git
;; magit
(autoload 'magit-status "magit" nil t)
(global-set-key (kbd "C-c g s") 'magit-status)
(global-set-key (kbd "C-c g ?") 'magit-blame-mode)
(global-set-key (kbd "C-c g /") 'vc-git-grep)
;; require yagist.el
(global-set-key (kbd "C-c g p") 'yagist-region-or-buffer)
;; mo-git-blame
;; (autoload 'mo-git-blame-file "mo-git-blame" nil t)
;; (autoload 'mo-git-blame-current "mo-git-blame" nil t)
;; (global-set-key (kbd "C-c g c") 'mo-git-blame-current)
;; (global-set-key (kbd "C-c g f") 'mo-git-blame-file)
;; git-gutter
(if (display-graphic-p)
    (progn
      (require 'git-gutter-fringe+))
  (require-package 'git-gutter+))
(global-git-gutter+-mode t)
(global-set-key (kbd "C-c g f") 'git-gutter+-mode) ; turn on/off git-gutter+ in the current buffer
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

;; multiple-cursors (deactivated as it's incompatible with Evil)
(require 'multiple-cursors)
(global-set-key (kbd "<M-C-down>") 'mc/mark-next-like-this)
(global-set-key (kbd "<M-C-up>") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c m @") 'mc/edit-lines)
(global-set-key (kbd "C-c m a") 'mc/mark-all-like-this-dwim)

;; Editing text
(add-hook 'text-mode-hook 'turn-on-auto-fill) ; auto-wrap
(setq sentence-end-double-space nil)    ; one space is enough after a period to end a sentence

;; Markdown
(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

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
(defun hub/set-newline-and-indent-comment ()
  "Bind RET locally to 'comment-indent-new-line'."
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
(eval-after-load 'auto-fill-mode '(diminish 'auto-fill-function))
(eval-after-load 'git-gutter+ '(diminish 'git-gutter+-mode))
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
;; (show-paren-mode 1)                     ; highlight matching brackets
(setq-default indent-tabs-mode nil)     ; no tabs, only spaces
(setq comment-auto-fill-only-comments t) ; auto-fill comments and only them
(column-number-mode 1)               ; show column number in mode line
(projectile-global-mode)
;; (global-linum-mode t) ; always show line numbers
(autoload 'projectile-on "projectile" "Project awareness in Emacs." t)
(add-hook 'prog-mode-hook
          (lambda () (progn (linum-mode 1) ; all code buffers with linum
                            (flycheck-mode)
                            (subword-mode) ; camelcase moves
                            ;; (load-theme-buffer-local 'solarized-dark (current-buffer) t)
                            ;; (projectile-on) ; project awareness
                            (turn-on-auto-fill)
                            )))
(setq linum-format " %3d ")    ; remove graphical glitches with fringe

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
; scala
(add-to-list 'load-path (getenv "SCALA_MODE2_ROOT"))
(autoload 'scala-mode "scala-mode2")
(add-to-list 'auto-mode-alist '("\\.scala$" . scala-mode))
(add-to-list 'load-path (getenv "ENSIME_ROOT"))

;; TODO: fixme. Goal when I call align-current in a scala file it
;; magically align on => and <-
(add-hook 'align-load-hook (lambda ()
                             (add-to-list 'align-rules-list
                                          '(scala-align
                                            (regexp  . "\\(\\s-+\\)\\(<-\\|=>\\)")
                                            (group   . 1)
                                            (modes   . '(scala-mode))
                                            (repeat  . nil)))))
(defun hub/ensime-inf-reload ()
  "Restart the REPL with the last definition."
  (interactive)
  (if (ensime-inf-running-p-1) (ensime-inf-quit-interpreter))
  (ensime-inf-switch))
(defun hub/ensime-setup ()
  "ENSIME tweaking."
  (local-set-key (kbd "C-c C-l") 'hub/ensime-inf-reload))
(defun hub/scala-ret ()
  "dwim with RET even inside multiline comments."
  (interactive)
  (newline-and-indent)
  (scala-indent:insert-asterisk-on-multiline-comment))
(defun hub/scala-config ()
  "Config scala-mode to my liking and start ensime."
  (setq
   scala-indent:use-javadoc-style nil
   scala-indent:align-forms t
   scala-indent:align-parameters t
   scala-indent:default-run-on-strategy 1)
  (hub/anti-useless-whitespace)
  ;; is buggy with scala-mode2
  ;; FIXME: doesn't look like I'm useful...
  ;; (make-local-variable 'comment-style)
  ;; (setq comment-style 'multi-line)
  ;; scala-mode doesn't work well (yet) with auto-fill
  ;; use M-q to wrap and indent long comments
  (turn-off-auto-fill)
  (require 'ensime)
  (add-hook 'ensime-source-buffer-loaded-hook 'hub/ensime-setup)
  (ensime-scala-mode-hook)
  (local-set-key (kbd "RET") 'hub/scala-ret))
(evil-define-key 'normal scala-mode-map ",s." 'sbt-find-definitions)
(evil-define-key 'normal scala-mode-map ",sa" 'sbt-run-previous-command)
(evil-define-key 'normal scala-mode-map ",sc" 'sbt-command)
(evil-define-key 'normal scala-mode-map ",s/" 'sbt-grep)
(evil-define-key 'normal scala-mode-map ",sr" 'sbt-find-usages)
(evil-define-key 'visual scala-mode-map ",sl" 'sbt-send-region)
(evil-define-key 'normal scala-mode-map (kbd "M-.") 'ensime-edit-definition)
(evil-define-key 'normal scala-mode-map ",e." 'ensime-edit-definition)
(evil-define-key 'normal scala-mode-map ",ei" 'ensime-inspect-type-at-point)
(evil-define-key 'normal scala-mode-map ",ee" 'ensime-show-all-errors-and-warnings)
(evil-define-key 'normal scala-mode-map ",ef" 'ensime-format-source)
(evil-define-key 'normal scala-mode-map ",e/" 'ensime-search)
(evil-define-key 'normal scala-mode-map ",ex" 'ensime-scalex)
(evil-define-key 'normal scala-mode-map ",er" 'ensime-expand-selection)
(evil-define-key 'normal scala-mode-map ",eh" 'ensime-show-doc-for-symbol-at-point)
(evil-define-key 'normal scala-mode-map ",eb" 'ensime-builder-rebuild)
(evil-define-key 'normal scala-mode-map ",eB" 'ensime-builder-build)
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

; Emacs Lisp
(defun hub/emacs-lisp-config ()
  "Set up my emacs-lisp hacking environment."
  (hub/set-newline-and-indent-comment)
  (rainbow-delimiters-mode t)
  (turn-on-eldoc-mode))
(add-hook 'emacs-lisp-mode-hook 'hub/emacs-lisp-config)
(require 'jka-compr) ; find-tag to be able to find .el.gz 
(evil-define-key 'normal lisp-mode-shared-map ",." 'find-function)

;; Smalltalk
(add-to-list 'auto-mode-alist '("\\.st$" . shampoo-code-mode))
(add-to-list 'load-path "~/Temp/shampoo-emacs/")
(autoload 'shampoo-code-mode "shampoo-modes")

;; Haskell
;; unicode input doesn't work with eg >= (Not in scope: ≥): disabled
;; (add-hook 'haskell-mode-hook 'turn-on-haskell-unicode-input-method)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(eval-after-load "haskell-mode"
  '(progn
     (define-key haskell-mode-map (kbd "M-<left>") 'haskell-move-nested-left)
     (define-key haskell-mode-map (kbd "M-<right>") 'haskell-move-nested-right)))
(eval-after-load "haskell-mode"
  '(define-key haskell-mode-map (kbd "C-c C-c") 'haskell-compile))
(eval-after-load "haskell-cabal"
  '(define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-compile))

;; Web: HTML/CSS/JS
(add-hook 'sgml-mode-hook 'zencoding-mode) ;; Auto-start on any markup modes
(add-hook 'sgml-mode-hook (lambda () (progn (linum-mode 1)))) 
;; after deleting a tag, indent properly
(defadvice sgml-delete-tag (after reindent activate)
  (indent-region (point-min) (point-max)))

; tweaking to get my proper setup
; OSX
; * iTerm2
; Preferences > Keys (tab) > Remap Left Command to Left Option
; Preferences > Profile > Left Option: Meta + Esc
; * Numpad (for calc): remap keypad-dot to Option+Shift+Keypad-dot

;;; emacs ends here
