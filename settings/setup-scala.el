(use-package scala-mode
  :interpreter
  ("scala" . scala-mode)
  :hook (scala-mode . lsp)
  :config
  (setq
   scala-indent:use-javadoc-style nil
   scala-indent:align-forms t
   scala-indent:align-parameters t
   scala-indent:default-run-on-strategy 1
   ; otherwise flycheck hides sbt errors with M-n/p
   flycheck-standard-error-navigation nil)
  ;; is buggy with scala-mode2
  ;; FIXME: doesn't look like I'm useful...
  ;; (make-local-variable 'comment-style)
  ;; (setq comment-style 'multi-line)
  ;; scala-mode doesn't work well (yet) with auto-fill
  ;; use M-q to wrap and indent long comments
  (turn-off-auto-fill)
  ;; (require 'ensime)
  ;; (ensime-scala-mode-hook)
  ;; (ensime-mode)
  ;; (make-local-variable 'company-backends)
  ;; (add-to-list 'company-backends 'ensime-company)
  (defun hub/scala-ret ()
    "Dwim with RET even inside multiline comments."
    (interactive)
    (newline-and-indent)
    ;; (comment-indent-new-line)
    (scala-indent:insert-asterisk-on-multiline-comment))
  (local-set-key (kbd "RET") 'hub/scala-ret)

  (evil-define-key 'insert scala-mode-map (kbd "C-S-<right>") 'sp-slurp-hybrid-sexp)
  (evil-define-key 'insert scala-mode-map (kbd "C-S-<left>") 'sp-barf-hybrid-sexp)
  (evil-define-key 'insert scala-mode-map (kbd "C-d") 'sp-kill-hybrid-sexp)
  ;; { + Return => create a block and put the cursor on its own line
  (sp-local-pair 'scala-mode "{" nil :post-handlers '(("||\n[i]" "RET")))

  ;; TODO: fixme. Goal when I call align-current in a scala file it
  ;; magically align on => and <-
  (add-hook 'align-load-hook (lambda ()
                             (add-to-list 'align-rules-list
                                          '(scala-cases
                                            (regexp  . "\\(\\s-+\\)\\(=>\\)")
                                            (group   . 1)
                                            (modes   . '(scala-mode))
                                            (repeat  . nil)))
                             (add-to-list 'align-rules-list
                                          '(scala-align
                                            (regexp  . "\\(\\s-+\\)\\(<-\\)")
                                            (group   . 1)
                                            (modes   . '(scala-mode))
                                            (repeat  . nil))))))

(use-package lsp-metals
  :after scala-mode
  ;; :pin melpa
  :config (setq lsp-metals-treeview-show-when-views-received t))

(use-package sbt-mode
  :commands sbt-start sbt-command
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map)
  ;; compilation-skip-threshold tells the compilation minor-mode
  ;; which type of compiler output can be skipped. 1 = skip info
  ;; 2 = skip info and warnings.
  (setq compilation-skip-threshold 1)

  ;; Bind C-a to 'comint-bol when in sbt-mode. This will move the
  ;; cursor to just after prompt.
  (local-set-key (kbd "C-a") 'comint-bol)

  ;; Bind M-RET to 'comint-accumulate. This will allow you to add
  ;; more than one line to scala console prompt before sending it
  ;; for interpretation. It will keep your command history cleaner.
  (local-set-key (kbd "S-RET") 'comint-accumulate)
  (setq sbt:ansi-support t)
  ;; sbt-supershell kills sbt-mode:
  ;; https://github.com/hvesalai/emacs-sbt-mode/issues/152
  (setq sbt:program-options '("-Dsbt.supershell=false"))
  (add-hook 'sbt-mode-hook
            (lambda ()
              (setq prettify-symbols-alist
                    `((,(expand-file-name (directory-file-name (sbt:find-root))) . ?⌂)
                      (,(expand-file-name "~") . ?~)))
              (prettify-symbols-mode t)))
  ;; default (sbt) is not enough to get ANSI colors as sbt infers that
  ;; it's not supported. Forcing colors in sbt output.
  ;;(setq sbt:program-name "sbt -Dspecs2.color=true -Dsbt.log.format=true")
  (defun hub/sbt-start ()
    "Go to (or start) sbt buffer without affecting the current buffer."
    (interactive)
    (hub/dwim-other-window 'sbt-start))
  (evil-define-key 'normal scala-mode-map ",g." 'sbt-find-definitions)
  (evil-define-key 'normal scala-mode-map ",bB" 'sbt-run-previous-command)
  (evil-define-key 'normal scala-mode-map ",bb" 'sbt-command)
  (evil-define-key 'normal scala-mode-map ",bh" 'sbt-hydra)
  (evil-define-key 'normal scala-mode-map ",e/" 'sbt-grep)
  (evil-define-key 'normal scala-mode-map ",gu" 'sbt-find-usages)
  (evil-define-key 'normal scala-mode-map ",es" 'hub/sbt-start)
  (evil-define-key 'visual scala-mode-map ",el" 'sbt-send-region))

(provide 'setup-scala)
