;;; setup-evil.el --- Hubert's evil settings — otherwise I can't use my fingers  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Hubert Behaghel

;; Author: Hubert Behaghel <behaghel@gmail.com>
(use-package evil
  :init
  :config
  (use-package evil-leader)
  (use-package evil-matchit)
  (use-package evil-surround)
  (use-package evil-nerd-commenter)
  (evil-mode 1)
  ;; surround
  ;; before my config for my config to win
  ;; otherwise in visual s call surround where I want to go on previous line
  (global-evil-surround-mode 1)
  ;; (evil-define-key 'visual evil-surround-mode-map "s" nil)
  ;; (evil-define-key 'visual evil-surround-mode-map "S" 'surround-region)
  ;; (evil-define-key 'visual evil-surround-mode-map (kbd "C-S") 'surround-region)
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
  ;; (define-key evil-normal-state-map (kbd "C") 'evil-window-top)
  ;; (define-key evil-normal-state-map (kbd "R") 'evil-window-low)
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
  (define-key evil-normal-state-map (kbd ",g/") 'dired)
  (define-key evil-normal-state-map (kbd ",,") 'hub/switch-dwim)
  (define-key evil-normal-state-map (kbd ",|") 'shell-command-on-region)
  (define-key evil-normal-state-map (kbd ",=") 'align-current))
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

;; (define-key evil-normal-state-map (kbd ",x") 'smex)
(define-key evil-normal-state-map (kbd ",gs") 'eshell)
(define-key evil-normal-state-map (kbd ",el") 'eshell-run-last)
(define-key evil-normal-state-map (kbd ",gS") 'hub/eshell-other-window)
;; you want to *g*o somewhere
(define-key evil-normal-state-map (kbd ",gg") 'hub/switch-to-other-buffer)
;;; Switch to another open buffer
;; (define-key evil-normal-state-map (kbd ",gb") 'switch-to-buffer)
;; (define-key evil-normal-state-map (kbd ",gB") 'switch-to-buffer-other-window)
;; you want to *o*pen something
;;; Open file
;; (define-key evil-normal-state-map (kbd ",of") 'ido-find-file)
;; Browse URL
(define-key evil-normal-state-map (kbd ",ou") 'browse-url)
(define-key evil-normal-state-map (kbd ",oc") '(lambda () (interactive)(find-file "~/Dropbox/Projects/scala/exercises/cs-fundamental/CLRS.org")))

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
;; until this is merged: https://github.com/emacs-evil/evil/pull/873
;; otherwise, gd always asks for a TAGS file :facepalm:
(define-key evil-normal-state-map (kbd "gd") '(lambda () (interactive)  (let* ((string (evil-find-symbol t))
                                                                          (search (format "\\_<%s\\_>" (regexp-quote string))))
                                                                     (evil-search search t t (point-min)))))


;; Evil has those but I don't need evil to handle completion
;; (define-key evil-insert-state-map "\C-n" 'evil-complete-next)
;; (define-key evil-insert-state-map "\C-p" 'evil-complete-previous)
(define-key evil-insert-state-map "\C-n" nil)
(define-key evil-insert-state-map "\C-p" nil)
;;;; Default state
(evil-set-initial-state 'help-mode 'emacs)
;; (evil-set-initial-state 'dired-mode 'emacs)
;; (evil-set-initial-state 'fundamental-mode 'emacs)
(evil-set-initial-state 'ess-help-mode 'emacs)
(evil-set-initial-state 'erc-mode 'emacs)
(evil-set-initial-state 'image-mode 'emacs)
(evil-set-initial-state 'cider-stacktrace-mode 'emacs)
(evil-set-initial-state 'epa-key-list-mode 'emacs)
(evil-set-initial-state 'magit-popup-mode 'emacs)
(evil-set-initial-state 'twittering-mode 'normal)
(evil-set-initial-state 'ensime-inspector-mode 'emacs)
(evil-set-initial-state 'haskell-error-mode 'emacs)
(evil-set-initial-state 'haskell-interactive-mode 'insert)
(evil-set-initial-state 'intero-repl-mode 'insert)
;;; Info & Evil
(evil-set-initial-state 'Info 'emacs)
(evil-define-key 'motion Info-mode-map "l" nil) ; use l to say last

;; (require 'powerline-evil)
;; (powerline-evil-center-color-theme)

(provide 'setup-evil)
;;; setup-evil.el ends here
