;; JS
(use-package js2-mode
  :ensure t
  :commands (js2-mode js2-jsx-mode)
  :mode ("\\.jsx?\\'" . js2-jsx-mode)
  :interpreter ("node" . js2-mode)
  :diminish js-mode
  :config
  (setq js-indent-level 2
        js2-basic-offset 2
        js2-bounce-indent-p t
        js2-highlight-level 2
        ;; js2-mode-show-parse-errors nil
        ;; js2-mode-show-strict-warnings nil
        js2-strict-trailing-comma-warning nil
        js2-concat-multiline-strings t)

  (evil-define-key 'insert js2-mode-map (kbd "RET") 'js2-line-break)
  (evil-define-key 'normal js2-mode-map "zc" 'js2-mode-hide-element)
  (evil-define-key 'normal js2-mode-map "zo" 'js2-mode-show-element)
  (evil-define-key 'normal js2-mode-map "za" 'js2-mode-toggle-element)
  (evil-define-key 'normal js2-mode-map "zC" 'js2-mode-hide-functions)
  (evil-define-key 'normal js2-mode-map "zM" 'js2-mode-hide-functions)
  (evil-define-key 'normal js2-mode-map "zO" 'js2-mode-show-functions)
  (evil-define-key 'normal js2-mode-map "zR" 'js2-mode-show-functions)
  (evil-define-key 'normal js2-mode-map "zA" 'js2-mode-toggle-hide-functions)
  (evil-define-key 'normal js2-mode-map "zj" 'js2-mode-toggle-hide-comments)

  (evil-define-key 'normal js2-mode-map ",=" 'web-beautify-js)
  (evil-define-key 'visual js2-mode-map ",=" 'web-beautify-js)
  (evil-define-key 'normal js2-mode-map (kbd  "M-n") 'flycheck-next-error)
  (evil-define-key 'insert js2-mode-map (kbd  "M-n") 'flycheck-next-error)

  (sp-local-pair 'js2-mode "<" ">")
  (sp-local-pair 'js2-mode "{" nil :post-handlers '(("||\n[i]" "RET")))
  ;; https://gist.github.com/lewang/908bc9cb7677d5936936
  (defun sp-js2-electric-semicolon (id action context)
    (message "node before is %s" (js2-node-type (js2-node-at-point (- (point) 1))))
    (message "action is %s" action)
    (message "pair is %s %s %d" id (plist-get (sp-get-pair id) :close) (length id))
    (when (and (eq action 'insert)
               (save-excursion
                 (goto-char (- (point) (length id)))
                 (skip-chars-backward " \t")
                 (memq (js2-node-type (js2-node-at-point (point)))
                       (list js2-NAME js2-LP js2-SCRIPT js2-CALL js2-BLOCK))))
      (save-excursion
        (goto-char (+ (point) (length (plist-get (sp-get-pair id) :close))))
        (insert ";"))))
  (sp-local-pair 'js2-mode "(" nil :post-handlers '(:add sp-js2-electric-semicolon))

  (use-package js2-refactor :ensure t
    :config
    (evil-define-key 'normal js2-mode-map ",reo" 'js2r-expand-object)
    (evil-define-key 'normal js2-mode-map ",rco" 'js2r-contract-object)
    (evil-define-key 'normal js2-mode-map ",reu" 'js2r-expand-function)
    (evil-define-key 'normal js2-mode-map ",rcu" 'js2r-contract-function)
    (evil-define-key 'normal js2-mode-map ",rea" 'js2r-expand-array)
    (evil-define-key 'normal js2-mode-map ",rca" 'js2r-contract-array)
    (evil-define-key 'normal js2-mode-map ",rwi" 'js2r-wrap-buffer-in-iife)
    (evil-define-key 'normal js2-mode-map ",rig" 'js2r-inject-global-in-iife)
    (evil-define-key 'normal js2-mode-map ",riv" 'js2r-inline-var)
    (evil-define-key 'normal js2-mode-map ",rrv" 'js2r-rename-var)
    (evil-define-key 'normal js2-mode-map ",rvt" 'js2r-var-to-this)
    (evil-define-key 'normal js2-mode-map ",rag" 'js2r-add-to-globals-annotation)
    (evil-define-key 'normal js2-mode-map ",rsv" 'js2r-split-var-declaration)
    (evil-define-key 'normal js2-mode-map ",rss" 'js2r-split-string)
    (evil-define-key 'normal js2-mode-map ",rip" 'js2r-introduce-parameter)
    (evil-define-key 'normal js2-mode-map ",rlp" 'js2r-localize-parameter)
    (evil-define-key 'normal js2-mode-map ",rtf" 'js2r-toggle-function-expression-and-declaration)
    (evil-define-key 'normal js2-mode-map ",rao" 'js2r-arguments-to-object)
    (evil-define-key 'normal js2-mode-map ",ruw" 'js2r-unwrap)
    (evil-define-key 'normal js2-mode-map ",rwl" 'js2r-wrap-in-for-loop)
    (evil-define-key 'normal js2-mode-map ",r3i" 'js2r-ternary-to-if)
    (evil-define-key 'normal js2-mode-map ",rlt" 'js2r-log-this)
    ;; (evil-define-key 'normal js2-mode-map ",)" 'js2r-forward-slurp)
    ;; (evil-define-key 'normal js2-mode-map ",(" 'js2r-forward-barf)
    (local-set-key (kbd "C-<right>") 'js2r-forward-slurp)
    (local-set-key (kbd "C-<left>") 'js2r-forward-barf)
    (evil-define-key 'visual js2-mode-map ",rev" 'js2r-extract-var)
    (evil-define-key 'visual js2-mode-map ",ref" 'js2r-extract-function)
    (evil-define-key 'visual js2-mode-map ",rem" 'js2r-extract-method))

  ;; (ggtags-mode nil)
  (hub/anti-useless-whitespace)

  ;; https://github.com/verdammelt/dotfiles/blob/master/.emacs.d/init-javascript.el
  (defvar mjs/project-node-module-special-cases (list)
    "Some projects may not have their node_modules directory at
  their top level.")
  (defvar mjs/previous-node-modules-added-to-path nil)
  (defun mjs/add-node-modules-in-path ()
    "I don't install project dependencies globally so I need to add
the .node_modules/.bin directory to the exec path. Sometimes the
node_modules directory is not in the project root, add special
case subdirectory names to
mjs/project-node-module-special-cases."
    (interactive)
    ;; (projectile-current-project-files)
    ;; (message (projectile-project-root))
    (let* ((project-root (if (projectile-project-p) (projectile-project-root) (locate-dominating-file default-directory "package.json")))
           (all-possibilities
            (mapcar #'(lambda (dir) (expand-file-name "./node_modules/.bin" dir))
                    (cons project-root mjs/project-node-module-special-cases)))
           (node-modules-bind-dir
            (cl-find-if #'file-exists-p all-possibilities)))
      (when mjs/previous-node-modules-added-to-path
        (setq exec-path
              (cl-remove mjs/previous-node-modules-added-to-path exec-path
                         :test #'string=)
              mjs/previous-node-modules-added-to-path nil))
      (when node-modules-bind-dir
        (setq mjs/previous-node-modules-added-to-path node-modules-bind-dir
              exec-path (cl-pushnew node-modules-bind-dir exec-path
                                    :test #'string=)))))

  (with-eval-after-load 'projectile
    (add-hook 'projectile-after-switch-project-hook 'mjs/add-node-modules-in-path))

  (add-hook 'js2-mode-hook
            (lambda ()
              (when (projectile-project-p)
                (setq
                 flycheck-javascript-eslint-executable (concat (projectile-project-root) "node_modules/.bin/eslint"))
                ;; (setq flycheck-eslint-rules-directories (cons (projectile-project-root) flycheck-eslint-rules-directories))))
                )))
  ;; (add-hook 'js2-mode-hook (lambda ()
  ;;                            (setq flycheck-enabled-checkers (list 'javascript-eslint))))

  (use-package nvm
    :ensure t
    :config
    (autoload 'nvm-use "nvm")
    (autoload 'nvm-use-for "nvm")
    (autoload 'nvm--installed-versions "nvm")
    (defvar mjs/previous-node-version nil)
    (defun mjs/choose-node-version ()
      "Choose and use the correct version of node for this current
directory. If there is a .nvmrc file use that - otherwise pick
one of the installed versions (arbitrarily: the last)."
      (when mjs/previous-node-version
        (setq exec-path
              (cl-remove mjs/previous-node-version exec-path
                         :test #'string=)
              mjs/previous-node-version nil))
      (if (file-exists-p ".nvmrc")
          (nvm-use-for ".")
        (nvm-use (caar (last (nvm--installed-versions)))))
      (setq mjs/previous-node-version (getenv "NVM_BIN")
            exec-path (cl-pushnew mjs/previous-node-version exec-path
                                  :test #'string=)))
    )

  (use-package mocha)
  (with-eval-after-load 'projectile
    (add-hook 'projectile-after-switch-project-hook 'mjs/choose-node-version))

  ;; completion and code navigation
  (use-package tern
    :config
    (add-hook 'js2-mode-hook 'tern-mode)

    (evil-define-key 'normal js2-mode-map ",." 'tern-find-definition)
    (evil-define-key 'normal js2-mode-map ",t" 'tern-pop-find-definition)
    (evil-define-key 'normal js2-mode-map (kbd "M-.") 'tern-find-definition)
    (evil-define-key 'normal js2-mode-map ",:" 'tern-find-definition-by-name)
    (evil-define-key 'normal js2-mode-map ",ii" 'tern-get-type)
    (evil-define-key 'normal js2-mode-map ",hh" 'tern-get-docs)

    (use-package company-tern
      :config
      (add-to-list 'company-backends 'company-tern)))

  ;; writing doc
  (use-package js-doc
    :config
    (evil-define-key 'normal js2-mode-map ",hi" 'js-doc-insert-function-doc)
    (evil-define-key 'normal js2-mode-map ",@" 'js-doc-insert-tag)
    (evil-define-key 'normal js2-mode-map ",hI" 'js-doc-insert-file-doc))

  )
;; when you only want linting from js2
;; (add-hook 'js-mode-hook 'js2-minor-mode)

(provide 'setup-js)