;; JS
(use-package typescript-mode
  :after tree-sitter
  :config
  ;; we choose this instead of tsx-mode so that eglot can automatically figure out language for server
  ;; see https://github.com/joaotavora/eglot/issues/624 and https://github.com/joaotavora/eglot#handling-quirky-servers
  (define-derived-mode typescriptreact-mode typescript-mode
    "TypeScript TSX")
  (define-derived-mode typescriptreact-ts-mode typescript-ts-mode
    "TypeScript TSX")

  ;; use our derived mode for tsx files
  (add-to-list 'auto-mode-alist '("\\.tsx?\\'" . typescriptreact-mode))
  ;; by default, typescript-mode is mapped to the treesitter typescript parser
  ;; use our derived mode to map both .tsx AND .ts -> typescriptreact-mode -> treesitter tsx
  (add-to-list 'tree-sitter-major-mode-language-alist '(typescriptreact-mode . tsx)))

;; https://github.com/orzechowskid/tsi.el/
;; great tree-sitter-based indentation for typescript/tsx, css, json
(use-package tsi
  :after tree-sitter
  :straight (:type git :host github :repo "orzechowskid/tsi.el")
  ;; define autoload definitions which when actually invoked will cause package to be loaded
  :commands (tsi-typescript-mode tsi-json-mode tsi-css-mode)
  :init
  (add-hook 'typescript-mode-hook (lambda () (tsi-typescript-mode 1)))
  (add-hook 'json-mode-hook (lambda () (tsi-json-mode 1)))
  (add-hook 'css-mode-hook (lambda () (tsi-css-mode 1)))
  (add-hook 'scss-mode-hook (lambda () (tsi-scss-mode 1))))
;; (use-package js2-mode
;;   :ensure t
;;   :commands (js2-mode js2-jsx-mode)
;;   :mode ("\\.jsx?\\'" . js2-jsx-mode)
;;   :interpreter ("node" . js2-mode)
;;   :diminish js-mode
;;   :bind (:map js2-mode-map
;;               ("M-." . nil))                   ; let xref-js2 do it
;;   :config
;;   (setq-default js-indent-level 2
;;                 js2-basic-offset 2
;;                 js2-bounce-indent-p t
;;                 js2-highlight-level 2
;;                 ;; js2-mode-show-parse-errors nil
;;                 ;; js2-mode-show-strict-warnings nil
;;                 js2-strict-trailing-comma-warning nil
;;                 js2-concat-multiline-strings nil
;;                 js2-include-node-externs t)

;;   (evil-define-key 'insert js2-mode-map (kbd "RET") 'js2-line-break)
;;   (evil-define-key 'normal js2-mode-map "zc" 'js2-mode-hide-element)
;;   (evil-define-key 'normal js2-mode-map "zo" 'js2-mode-show-element)
;;   (evil-define-key 'normal js2-mode-map "za" 'js2-mode-toggle-element)
;;   (evil-define-key 'normal js2-mode-map "zC" 'js2-mode-hide-functions)
;;   (evil-define-key 'normal js2-mode-map "zM" 'js2-mode-hide-functions)
;;   (evil-define-key 'normal js2-mode-map "zO" 'js2-mode-show-functions)
;;   (evil-define-key 'normal js2-mode-map "zR" 'js2-mode-show-functions)
;;   (evil-define-key 'normal js2-mode-map "zA" 'js2-mode-toggle-hide-functions)
;;   (evil-define-key 'normal js2-mode-map "zj" 'js2-mode-toggle-hide-comments)

;;   (evil-define-key 'normal js2-mode-map ",=" 'web-beautify-js)
;;   (evil-define-key 'visual js2-mode-map ",=" 'web-beautify-js)
;;   (evil-define-key 'normal js2-mode-map (kbd  "M-n") 'flycheck-next-error)
;;   (evil-define-key 'insert js2-mode-map (kbd  "M-n") 'flycheck-next-error)

;;   ;; right thing to do but makes it very difficult to delete
;;   ;; comparison operators.
;;   ;; (sp-local-pair 'js2-jsx-mode "<" ">")
;;   (sp-local-pair 'js2-mode "{" nil :post-handlers '(("||\n[i]" "RET")))
;;   (sp-local-pair 'js2-jsx-mode "{" nil :post-handlers '(("||\n[i]" "RET")))
;;   ;; to slurp without leaving punctuation over
;;   (add-to-list 'sp-sexp-suffix (list 'js2-mode 'regexp ""))
;;   (add-to-list 'sp-sexp-suffix (list 'js2-jsx-mode 'regexp ""))

;;   ;; https://gist.github.com/lewang/908bc9cb7677d5936936
;;   (defun sp-js2-electric-semicolon (id action context)
;;     (message "node before is %s" (js2-node-type (js2-node-at-point (- (point) 1))))
;;     (message "action is %s" action)
;;     (message "pair is %s %s %d" id (plist-get (sp-get-pair id) :close) (length id))
;;     (when (and (eq action 'insert)
;;                (save-excursion
;;                  (goto-char (- (point) (length id)))
;;                  (skip-chars-backward " \t")
;;                  (memq (js2-node-type (js2-node-at-point (point)))
;;                        (list js2-NAME js2-LP js2-SCRIPT js2-CALL js2-BLOCK))))
;;       (save-excursion
;;         (goto-char (+ (point) (length (plist-get (sp-get-pair id) :close))))
;;         (insert ";"))))
;;   (sp-local-pair 'js2-mode "(" nil :post-handlers '(:add sp-js2-electric-semicolon))

;;   ;; (ggtags-mode nil)


  (if (executable-find "eslint_d")
      (setq flycheck-javascript-eslint-executable "eslint_d")
    (warn "emacs-js: You might want to install eslint_d: sudo npm install -g eslint_d."))

;;   (add-hook 'js2-mode-hook 'turn-off-auto-fill)

;;   (flycheck-add-mode 'javascript-jshint 'js2-jsx-mode)
;;   )

(use-package color-identifiers-mode
  :commands (js2-mode js2-jsx-mode)
  :disabled t
  :defer 3
  :config
  (add-hook 'js2-mode-hook 'color-identifiers-mode))

;; (use-package js2-refactor
;;   :commands (js2-mode js2-jsx-mode)
;;   :after (js2-mode)
;;   :bind (:map js2-mode-map
;;               ("C-<right>" . js2r-forward-slurp)
;;               ("C-<left>" .  js2r-forward-barf)
;;               ("C-k" . js2r-kill))
;;   :config
;;   (evil-define-key 'normal js2-mode-map ",feo" 'js2r-expand-object)
;;   (evil-define-key 'normal js2-mode-map ",fco" 'js2r-contract-object)
;;   (evil-define-key 'normal js2-mode-map ",feu" 'js2r-expand-function)
;;   (evil-define-key 'normal js2-mode-map ",fcu" 'js2r-contract-function)
;;   (evil-define-key 'normal js2-mode-map ",fea" 'js2r-expand-array)
;;   (evil-define-key 'normal js2-mode-map ",fca" 'js2r-contract-array)
;;   (evil-define-key 'normal js2-mode-map ",fwi" 'js2r-wrap-buffer-in-iife)
;;   (evil-define-key 'normal js2-mode-map ",fig" 'js2r-inject-global-in-iife)
;;   (evil-define-key 'normal js2-mode-map ",fiv" 'js2r-inline-var)
;;   (evil-define-key 'normal js2-mode-map ",frv" 'js2r-rename-var)
;;   (evil-define-key 'normal js2-mode-map ",fvt" 'js2r-var-to-this)
;;   (evil-define-key 'normal js2-mode-map ",fag" 'js2r-add-to-globals-annotation)
;;   (evil-define-key 'normal js2-mode-map ",fip" 'js2r-introduce-parameter)
;;   (evil-define-key 'normal js2-mode-map ",flp" 'js2r-localize-parameter)
;;   (evil-define-key 'normal js2-mode-map ",ftf" 'js2r-toggle-function-expression-and-declaration)
;;   (evil-define-key 'normal js2-mode-map ",fao" 'js2r-arguments-to-object)
;;   (evil-define-key 'normal js2-mode-map ",fuw" 'js2r-unwrap)
;;   (evil-define-key 'normal js2-mode-map ",fwl" 'js2r-wrap-in-for-loop)
;;   (evil-define-key 'normal js2-mode-map ",f3i" 'js2r-ternary-to-if)
;;   (evil-define-key 'normal js2-mode-map ",flt" 'js2r-log-this)
;;   ;; (evil-define-key 'normal js2-mode-map ",)" 'js2r-forward-slurp)
;;   ;; (evil-define-key 'normal js2-mode-map ",(" 'js2r-forward-barf)
;;   (evil-define-key 'visual js2-mode-map ",fev" 'js2r-extract-var)
;;   (evil-define-key 'visual js2-mode-map ",fef" 'js2r-extract-function)
;;   (evil-define-key 'visual js2-mode-map ",fem" 'js2r-extract-method)

;;   (defun js-smart-split ()
;;     "Split the string or var declaration at point."
;;     (interactive)
;;     (let ((node (js2-node-at-point)))
;;       (cond ((js2-string-node-p node) (js2r-split-string))
;;             (t (js2r-split-var-declaration)))))
;;   (evil-define-key 'normal js2-mode-map "S-<return>" 'js-smart-split)
;; )

(use-package nvm
  :ensure t
  :commands (nvm-use nvm-use-for nvm--installed-versions)
  :config
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
  (with-eval-after-load 'projectile
    (add-hook 'projectile-after-switch-project-hook 'mjs/choose-node-version))
  )

;; (use-package mocha
;;   :commands (js2-mode js2-jsx-mode))

;; ;; completion and code navigation
;; (use-package tern
;;   :commands (js2-mode js2-jsx-mode)
;;   :after (js2-mode)
;;   :bind (:map tern-mode-keymap
;;               ("M-." . nil)
;;               ("M-," . nil))
;;   :config
;;   (add-hook 'js2-mode-hook 'tern-mode)

;;   (evil-define-key 'normal js2-mode-map ",." 'tern-find-definition)
;;   (evil-define-key 'normal js2-mode-map ",;" 'tern-pop-find-definition)
;;   ;; (evil-define-key 'normal js2-mode-map (kbd "M-.") 'tern-find-definition)
;;   (evil-define-key 'normal js2-mode-map ",:" 'tern-find-definition-by-name)
;;   (evil-define-key 'normal js2-mode-map ",ii" 'tern-get-type)
;;   (evil-define-key 'normal js2-mode-map ",hh" 'tern-get-docs)
;;   (defun tern-kill-process ()
;;     "Kill the tern process if any.
;; The process will be restarted.  This is useful if tern becomes
;; unreachable."
;;     (interactive)
;;     (delete-process "Tern"))
;;   )

;; (use-package company-tern
;;   :disabled t                           ; defunkt?
;;   :commands (js2-mode js2-jsx-mode)
;;   :after (tern)
;;   :config
;;   (add-to-list 'company-backends 'company-tern))

;; (use-package xref-js2
;;   :commands (js2-mode js2-jsx-mode)
;;   :after (js2-mode)
;;   :config
;;   (add-hook 'js2-mode-hook (lambda ()
;;                              (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t))))
;; ;; writing doc
;; (use-package js-doc
;;   :commands (js2-mode js2-jsx-mode)
;;   :config
;;   (evil-define-key 'normal js2-mode-map ",hi" 'js-doc-insert-function-doc)
;;   (evil-define-key 'normal js2-mode-map ",@" 'js-doc-insert-tag)
;;   (evil-define-key 'normal js2-mode-map ",hI" 'js-doc-insert-file-doc)

;;   (defun mdn-search (searchString)
;;     "Open a browser on the MDN page for SEARCHSTRING."
;;     (interactive (list (read-string "Search: " (thing-at-point 'symbol))))
;;     (browse-url (format "https://developer.mozilla.org/en-US/search?q=%s&topic=js" searchString)))

;;   (evil-define-key 'normal js2-mode-map ",h/" 'mdn-search)
;;   )

;; (use-package indium
;;   :commands (js2-mode js2-jsx-mode)
;;   :config
;;   (add-hook 'js2-mode-hook 'indium-interaction-mode)
;;   (evil-define-key 'normal js2-mode-map ",gr" 'indium-switch-to-repl-buffer)
;;   (evil-define-key 'normal js2-mode-map ",ii" 'indium-inspect-expression)
;;   (evil-define-key 'normal js2-mode-map ",il" 'indium-inspect-last-node)
;;   (evil-define-key 'normal js2-mode-map ",el" 'indium-eval-buffer)
;;   (evil-define-key 'visual js2-mode-map ",l" 'indium-eval-region)
;;   (evil-define-key 'normal js2-mode-map ",:" 'indium-inspect-expression)
;;   )

;; (use-package eslintd-fix
;;   :commands (js2-mode js2-jsx-mode)
;;   :config
;;   (add-hook 'js2-mode-hook 'eslintd-fix-mode))

;; ;; when you only want linting from js2
;; ;; (add-hook 'js-mode-hook 'js2-minor-mode)

;; (use-package tide
;;   :ensure t
;;   :mode (("\\.ts\\'" . typescript-mode)
;;          ("\\.tsx\\'" . typescript-mode))
;;   :config
;;   (add-hook 'typescript-mode-hook
;;             (lambda ()
;;               (tide-setup)
;;               (add-hook 'before-save-hook 'tide-format-before-save nil t)))
;;   (evil-define-key 'normal typescript-mode-map ",hh" 'tide-documentation-at-point)
;;   (evil-define-key 'normal typescript-mode-map (kbd "M-.") 'tide-jump-to-definition)
;;   (evil-define-key 'normal typescript-mode-map ",frv" 'tide-rename-symbol))

(provide 'setup-js)
