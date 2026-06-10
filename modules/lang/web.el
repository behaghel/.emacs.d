;;; web.el --- Web languages setup (HTML/CSS/SCSS) -*- lexical-binding: t; -*-

;;; Code:

(require 'hub-utils)

;; HTML
(add-to-list 'auto-mode-alist '("\\.html$" . html-mode))
(define-advice sgml-delete-tag (:after (&rest _args) hub/reindent-buffer)
  (indent-region (point-min) (point-max)))

;; CSS and SCSS
(use-package css-mode :commands css-mode
  :config
  (with-eval-after-load 'smartparens
    (sp-local-pair 'css-mode "{" nil :post-handlers '(("||\n[i]" "RET"))))
  (setq css-indent-offset 2)
  (use-package css-eldoc))

(use-package scss-mode :mode "\\.scss\\'")

(provide 'lang/web)
;;; web.el ends here
