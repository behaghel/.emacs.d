;;; snippets.el --- Writing snippets configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; YASnippet setup for writing-oriented templates and expansions.

;;; Code:

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

(provide 'writing/snippets)
;;; snippets.el ends here
