;;; prose.el --- Interactive prose writing configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Natural-language writing behavior: wrapping, spell-check toggles,
;; readability helpers, and focused writing modes.

;;; Code:

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

(provide 'writing/prose)
;;; prose.el ends here
