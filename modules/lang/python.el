;;; python.el --- Python setup -*- lexical-binding: t; -*-

;;; Code:

(use-package python :straight (:type built-in) :hook ((python-mode . eglot-ensure)))
(use-package python-docstring :hook (python-mode . python-docstring-mode))
(use-package pydoc :commands (pydoc pydoc-at-point pydoc-browse)
  :bind (:map evil-normal-state-map (",hh" . pydoc-at-point-no-jedi)))
(use-package python-pytest
  :bind (:map evil-normal-state-map
	      (",Th" . python-pytest-dispatch)
	      (",Tt" . python-pytest)
	      (",Tf" . python-pytest-file)
	      (",T," . python-pytest-file-dwim)
	      (",Tr" . python-pytest-repeat)
	      (",Tl" . python-pytest-last-failed)))
(use-package pip-requirements)
;; (use-package ein :after (org) :disabled t
;;   :config (org-babel-do-load-languages 'org-babel-load-languages '((ein . t))))

(provide 'lang/python)
;;; python.el ends here
