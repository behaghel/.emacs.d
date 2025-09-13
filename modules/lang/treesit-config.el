;;; treesit-config.el --- Language-specific Treesit sources and remaps -*- lexical-binding: t; -*-

;;; Code:

(with-eval-after-load 'treesit
  ;; Register known language sources (install on demand via helper)
  (dolist (entry '((bash "https://github.com/tree-sitter/tree-sitter-bash")
		   (cmake "https://github.com/uyha/tree-sitter-cmake")
		   (css "https://github.com/tree-sitter/tree-sitter-css")
		   (elisp "https://github.com/Wilfred/tree-sitter-elisp")
		   (go "https://github.com/tree-sitter/tree-sitter-go")
		   (html "https://github.com/tree-sitter/tree-sitter-html")
		   (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
		   (json "https://github.com/tree-sitter/tree-sitter-json")
		   (kotlin "https://github.com/tree-sitter/kotlin-tree-sitter")
		   (make "https://github.com/alemuller/tree-sitter-make")
		   (markdown "https://github.com/ikatyang/tree-sitter-markdown")
		   (pharo "https://github.com/Evref-BL/Pharo-Tree-Sitter")
		   (python "https://github.com/tree-sitter/tree-sitter-python")
		   (scala "https://github.com/tree-sitter/tree-sitter-scala")
		   (toml "https://github.com/tree-sitter/tree-sitter-toml")
		   (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
		   (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
		   (yaml "https://github.com/ikatyang/tree-sitter-yaml")))
    (add-to-list 'treesit-language-source-alist entry))

  ;; Major-mode remaps to TS modes
  (dolist (remap '((yaml-mode . yaml-ts-mode)
		   (bash-mode . bash-ts-mode)
		   (js2-mode . js-ts-mode)
		   (typescript-mode . typescript-ts-mode)
		   (typescriptreact-mode . typescriptreact-ts-mode)
		   (json-mode . json-ts-mode)
		   (css-mode . css-ts-mode)
		   (python-mode . python-ts-mode)
		   (scala-mode . scala-ts-mode)))
    (add-to-list 'major-mode-remap-alist remap)))

(provide 'lang/treesit-config)
;;; treesit-config.el ends here
