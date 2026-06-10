;;; smartparens.el --- Structural editing for programming buffers -*- lexical-binding: t; -*-

;;; Commentary:
;; Smartparens and evil-smartparens setup for programming buffers.

;;; Code:

(require 'hub-utils)

(defun hub/smartparens-enable-prog-mode ()
  "Enable Smartparens structural editing in programming buffers."
  (smartparens-strict-mode 1)
  (show-smartparens-mode 1))

(use-package smartparens
  :diminish smartparens-mode
  :commands (smartparens-mode smartparens-strict-mode show-smartparens-mode)
  :init
  (add-hook 'prog-mode-hook #'hub/smartparens-enable-prog-mode)
  (use-package evil-smartparens
    :diminish evil-smartparens-mode
    :commands (evil-smartparens-mode)
    :init
    (add-hook 'smartparens-enabled-hook #'evil-smartparens-mode)
    :config
    (define-advice evil-sp--add-bindings (:after (&rest _args) hub/clear-conflicting-keys)
      (evil-define-key 'normal evil-smartparens-mode-map
		       (kbd "D") nil
		       (kbd "c") nil
		       (kbd "s") nil
		       (kbd "S") nil)))
  :config
  (require 'smartparens-config)
  ;; Keep sexp structural editing bindings scoped to buffers where
  ;; smartparens is active so they don't override Org's meta arrows.
  (let ((map smartparens-mode-map))
    (define-key map (kbd "C-<right>") #'sp-slurp-hybrid-sexp)
    (define-key map (kbd "M-<left>")  #'sp-backward-slurp-sexp)
    (define-key map (kbd "C-<left>")  #'sp-forward-barf-sexp)
    (define-key map (kbd "M-<right>") #'sp-backward-barf-sexp)
    (define-key map (kbd "M-(")       #'sp-backward-unwrap-sexp)
    (define-key map (kbd "M-)")       #'sp-unwrap-sexp)
    (define-key map (kbd "C-<down>")  #'sp-down-sexp)
    (define-key map (kbd "C-<up>")    #'sp-backward-up-sexp)
    (define-key map (kbd "M-<down>")  #'sp-backward-down-sexp)
    (define-key map (kbd "M-<up>")    #'sp-up-sexp)
    (define-key map (kbd "S-M-f")     #'sp-forward-sexp)
    (define-key map (kbd "S-M-b")     #'sp-backward-sexp))
  (add-to-list 'sp-ignore-modes-list 'org-mode))

(provide 'dev/smartparens)
;;; smartparens.el ends here
