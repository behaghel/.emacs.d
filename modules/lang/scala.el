;;; scala.el --- Scala setup -*- lexical-binding: t; -*-

;;; Code:

(require 'hub-utils)

(use-package scala-ts-mode
  :straight '(scala-ts-mode :type git :host github :repo "KaranAhlawat/scala-ts-mode")
  :bind (("C-c C-c" . save-buffer)))

(use-package sbt-mode
  :commands sbt-start sbt-command
  :config
  (substitute-key-definition 'minibuffer-complete-word 'self-insert-command minibuffer-local-completion-map)
  (setq compilation-skip-threshold 1 sbt:ansi-support t sbt:program-options '("-Dsbt.supershell=false"))
  (add-hook 'sbt-mode-hook
	    (lambda ()
	      (setq prettify-symbols-alist
		    `((,(expand-file-name (directory-file-name (sbt:find-root))) . ?⌂)
		      (,(expand-file-name "~") . ?~)))
	      (prettify-symbols-mode t)))
  (defun hub/sbt-start () (interactive) (hub/dwim-other-window 'sbt-start))
  (evil-define-key 'normal scala-mode-map ",g." 'sbt-find-definitions)
  (evil-define-key 'normal scala-mode-map ",bB" 'sbt-run-previous-command)
  (evil-define-key 'normal scala-mode-map ",bb" 'sbt-command)
  (evil-define-key 'normal scala-mode-map ",bh" 'sbt-hydra)
  (evil-define-key 'normal scala-mode-map ",e/" 'sbt-grep)
  (evil-define-key 'normal scala-mode-map ",gu" 'sbt-find-usages)
  (evil-define-key 'normal scala-mode-map ",es" 'hub/sbt-start)
  (evil-define-key 'visual scala-mode-map ",el" 'sbt-send-region))

(provide 'lang/scala)
;;; scala.el ends here
