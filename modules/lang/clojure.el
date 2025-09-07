;;; clojure.el --- Clojure setup -*- lexical-binding: t; -*-

;;; Code:

(use-package cider
  :commands (cider cider-connect cider-jack-in)
  :init (add-hook 'cider-mode-hook 'set-auto-complete-as-completion-at-point-function)
  :config
  (evil-define-key 'normal cider-mode-map ",el" 'cider-load-buffer)
  (evil-define-key 'visual cider-mode-map ",l" 'cider-eval-region)
  (evil-define-key 'normal cider-mode-map ",." 'cider-jump-to-var)
  (evil-define-key 'normal cider-mode-map ",;" 'cider-jump-back)
  (evil-define-key 'normal cider-mode-map ",ii" 'cider-inspect)
  (evil-define-key 'normal cider-mode-map ",gr" 'cider-switch-to-repl-buffer)
  (evil-define-key 'normal cider-mode-map ",hh" 'cider-doc)
  (evil-define-key 'normal cider-mode-map ",hg" 'cider-docview-grimoire)
  (evil-define-key 'normal cider-mode-map ",hG" 'cider-docview-grimoire-web)
  (yas-minor-mode 1)
  (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
  (add-hook 'cider-repl-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'cider-repl-mode-hook 'smartparens-strict-mode)
  (add-hook 'cider-repl-mode-hook 'subword-mode)
  (add-hook 'cider-test-report-mode 'jcf-soft-wrap)
  (use-package clj-refactor
    :init
    (dolist (mapping '(("maps" . "outpace.util.maps")
		       ("seqs" . "outpace.util.seqs")
		       ("times" . "outpace.util.times")
		       ("repl" . "outpace.util.repl")
		       ("time" . "clj-time.core")
		       ("string" . "clojure.string")))
      (add-to-list 'cljr-magic-require-namespaces mapping t))
    (setq cljr-favor-prefix-notation nil)
    :config (clj-refactor-mode 1)
    (cljr-add-keybindings-with-prefix "C-c C-r")))

(provide 'lang/clojure)
;;; clojure.el ends here
