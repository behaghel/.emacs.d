;;; scala.el --- Scala setup -*- lexical-binding: t; -*-

;;; Code:

(require 'hub-utils)

(use-package scala-ts-mode
  :straight '(scala-ts-mode :type git :host github :repo "KaranAhlawat/scala-ts-mode")
  :mode ("\\.scala\\'" "\\.sbt\\'")
  :bind (("C-c C-c" . save-buffer))
  :hook
  ;; which-function-mode can be brittle in Scala TS buffers; disable for now.
  (scala-ts-mode . (lambda () (which-function-mode -1))))

;; Register Scala treesit grammar source and lazy-install on first use
(with-eval-after-load 'treesit
  (add-to-list 'treesit-language-source-alist '(scala "https://github.com/tree-sitter/tree-sitter-scala")))

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs '(scala-ts-mode . ("metals"))))

(add-hook 'scala-ts-mode-hook #'eglot-ensure)

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
