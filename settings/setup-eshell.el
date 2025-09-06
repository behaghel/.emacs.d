;;; setup-eshell.el --- Eshell configuration  -*- lexical-binding: t; -*-

(use-package eshell
  :straight nil
  :init
  (defun eshell-run-last ()
    "Relaunch last command without moving point."
    (interactive)
    (with-current-buffer (get-buffer "*eshell*")
      (hub/eshell-other-window)
      (insert-and-inherit (eshell-get-history 0))
      (eshell-send-input)))

  (defun hub/eshell-other-window ()
    "Open eshell in other window."
    (interactive)
    (hub/dwim-other-window 'eshell))

  (defun hub/load-term-theme-locally ()
    "Load the color theme I want to use for term into the current buffer."
    (load-theme-buffer-local 'tango-dark (current-buffer)))
  :general
  (:states 'normal
	   "gs" 'eshell
	   ",el" 'eshell-run-last
	   "gS" 'hub/eshell-other-window)
  :hook
  (eshell-mode . hub/load-term-theme-locally)
  :config
  (require 'em-term)
  (add-to-list 'eshell-visual-commands "sbt")
  (add-to-list 'eshell-visual-commands "vimdiff")
  (require 'em-cmpl)
  (add-to-list 'eshell-command-completions-alist '("gunzip" "gz\'") )
  (add-to-list 'eshell-command-completions-alist '("tar" "\(\.tar|\.tgz\|\.tar\.gz\)\'") )
  (setq eshell-cmpl-cycle-completions t)

  ;; stolen from http://blog.liangzan.net/blog/2012/12/12/customizing-your-emacs-eshell-prompt/
  (defun eshell/ef (fname-regexp &rest dir) (ef fname-regexp default-directory))

  ;; ---- path manipulation
  (defun pwd-repl-home (pwd)
    "Detect when PWD includes HOME and substitute this part with '~'."
    (let* ((home (expand-file-name (getenv "HOME")))
	   (home-len (length home)))
      (if (and (>= (length pwd) home-len)
	       (equal home (substring pwd 0 home-len)))
	  (concat "~" (substring pwd home-len))
	pwd)))

  (defun curr-dir-git-branch-string (pwd)
    "Return current git branch as a string or empty string if none."
    (when (and (eshell-search-path "git")
	       (locate-dominating-file pwd ".git"))
      (let ((git-output (shell-command-to-string
			 (concat "cd " pwd " && git branch | grep '\\*' | sed -e 's/^\\* //'"))))
	(propertize (concat "[" (if (> (length git-output) 0)
				    (substring git-output 0 -1)
				  "(no branch)") "]")
		    'face `(:foreground "tomato1")))))

  (require 'em-hist)
  (setq eshell-history-size 1024)
  (when (boundp 'eshell-save-history-on-exit)
    (setq eshell-save-history-on-exit t))
  (when (boundp 'eshell-ask-to-save-history)
    (setq eshell-ask-to-save-history 'always))

  (setq eshell-prompt-regexp "^[^%#$]*[%#$] ")
  (setq eshell-prompt-function
	(lambda ()
	  (concat
	   (propertize
	    ((lambda (p-lst)
	       (if (> (length p-lst) 3)
		   (concat
		    (mapconcat (lambda (elm) (if (zerop (length elm)) "" (substring elm 0 1)))
			       (butlast p-lst 3) "/")
		    "/"
		    (mapconcat #'identity (last p-lst 3) "/"))
		 (mapconcat #'identity p-lst "/")))
	     (split-string (pwd-repl-home (eshell/pwd)) "/"))
	    'face '(:foreground "DarkOrange1"))
	   (curr-dir-git-branch-string (eshell/pwd))
	   (propertize " % " 'face 'default))))

  (setq eshell-highlight-prompt nil)
  (require 'eshell-autojump))

(provide 'setup-eshell)
;;; setup-eshell.el ends here
