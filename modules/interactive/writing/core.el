;;; core.el --- Interactive writing entrypoint -*- lexical-binding: t; -*-

;;; Commentary:
;; Coarse entrypoint for prose, snippets, templates, and lightweight markup.

;;; Code:

(require 'autoinsert)
(require 'hub-utils)
(require 'hub-prose)

;;; Snippets
(use-package yasnippet
  :defer t
  :diminish yas-minor-mode
  :bind (("<C-tab>" . company-yasnippet)
	 :map yas-minor-mode-map
	 ("<tab>" . nil)
	 ("TAB" . nil))
  :config
  (yas-global-mode 1)
  (add-to-list 'yas-snippet-dirs
	       (expand-file-name "modules/interactive/editing/snippets" user-emacs-directory))
  (define-key yas-keymap (kbd "<tab>") #'yas-next-field-or-maybe-expand)
  (define-key yas-keymap (kbd "TAB") #'yas-next-field-or-maybe-expand)
  (setq yas-prompt-functions '(yas/completing-prompt)))

(use-package yasnippet-snippets
  :after yasnippet)

;;; Templates
(auto-insert-mode 0)
(setq auto-insert-directory (expand-file-name "insert/" user-emacs-directory))

(defun hub/autoinsert-yas-expand (&optional expand-env)
  "Replace text in yasnippet template optionally passing EXPAND-ENV."
  (yas-expand-snippet (buffer-string) (point-min) (point-max) expand-env))

(define-auto-insert "\\.org\\'" ["template.org" hub/autoinsert-yas-expand])
(define-auto-insert "\\.veriff\\.org\\'" ["template.veriff.org" hub/autoinsert-yas-expand])
(define-auto-insert "\\(?:nomina\\|.*-nomina\\).*\\.tex\\'" ["template.nomina.tex" hub/autoinsert-yas-expand])
(define-auto-insert "\\.orj\\'" ["template.orj" hub/autoinsert-yas-expand])

;;; Prose
(add-hook 'text-mode-hook #'turn-on-auto-fill)
(setq sentence-end-double-space nil)
(define-key evil-normal-state-map (kbd ",bs") #'flyspell-mode)

(use-package visual-fill-column :defer t)

(use-package artbollocks-mode
  :commands (artbollocks-mode)
  :bind (:map evil-normal-state-map
	      (",bw" . artbollocks-count-words)
	      (",bg" . artbollocks-grade-level)
	      (",be" . artbollocks-reading-ease)
	      (",br" . artbollocks-readability-index)))

(use-package writeroom-mode
  :commands (writeroom-mode)
  :config
  (add-to-list 'writeroom-local-effects #'variable-pitch-mode))

(use-package languagetool
  :commands (languagetool-check)
  :bind (:map evil-normal-state-map
	      (",bc" . langtool-check))
  :config
  (setq langtool-language-tool-jar "/usr/local/Cellar/languagetool/2.6/libexec/languagetool.jar")
  (define-key evil-normal-state-map (kbd ",bg") #'langtool-check))

;;; Lightweight markup
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

(provide 'writing/core)
;;; core.el ends here
