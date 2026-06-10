;;; markup.el --- Lightweight markup writing configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; AsciiDoc and Markdown authoring behavior for prose-oriented markup files.

;;; Code:

(require 'hub-prose)
(require 'hub-utils)

(add-hook 'adoc-mode-hook (lambda () (buffer-face-mode t)))
(add-to-list 'auto-mode-alist (cons "\\.asciidoc\\'" 'adoc-mode))

(use-package markdown-mode
  :defer t
  :mode (("\\.md\\'" . markdown-mode)
	 ("\\.markdown\\'" . markdown-mode)
	 ("README\\.md\\'" . gfm-mode))
  :init
  (add-hook 'markdown-mode-hook #'hub/prose-visual-fill-mode)
  (add-hook 'gfm-mode-hook #'hub/prose-visual-fill-mode)
  :commands (markdown-mode)
  :config
  (setq markdown-command
	(format "pandoc -c file://%s --from gfm -t html5 --mathjax --highlight-style pygments --standalone --quiet"
		(expand-file-name "etc/github-pandoc.css" user-emacs-directory)))
  (evil-define-key 'normal markdown-mode-map (kbd ",il") #'markdown-insert-link)
  (evil-define-key 'normal markdown-mode-map (kbd ",iH") #'markdown-insert-header-dwim)
  (evil-define-key 'normal markdown-mode-map (kbd ",ih") #'markdown-insert-header-setext-dwim)
  (evil-define-key 'normal markdown-mode-map (kbd ",i2") #'markdown-insert-header-setext-2)
  (evil-define-key 'normal markdown-mode-map (kbd ",i1") #'markdown-insert-header-setext-1)
  (evil-define-key 'normal markdown-mode-map (kbd ",ev") #'markdown-preview)
  (evil-define-key 'normal markdown-mode-map (kbd "M->") #'markdown-demote)
  (evil-define-key 'normal markdown-mode-map (kbd "M-<") #'markdown-promote)
  (evil-define-key 'normal markdown-mode-map (kbd ",eV") #'markdown-export-and-preview))

(provide 'writing/markup)
;;; markup.el ends here
