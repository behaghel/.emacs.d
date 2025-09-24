;;; json-config.el --- JSON setup -*- lexical-binding: t; -*-

;;; Code:

(use-package json-mode :mode "\\.json$"
  :config
  (setq js-indent-level 2)
  (add-to-list 'auto-mode-alist '("\\.eslintrc\\'" . json-mode)))

(provide 'lang/json-config)
;;; json-config.el ends here
