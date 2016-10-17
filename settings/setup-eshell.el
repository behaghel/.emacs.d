;; eshell
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

(defun hub/eshell-other-window ()
  "Open eshell in other window."
  (interactive)
  (hub/dwim-other-window 'eshell))

(defun hub/load-term-theme-locally ()
  "Load the color theme I want to use for term into the current buffer."
  (load-theme-buffer-local 'tango-dark (current-buffer)))
;; (add-hook 'eshell-mode-hook 'hub/load-term-theme-locally)

(provide 'setup-eshell)