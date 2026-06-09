;;; babel.el --- Org mode: Babel source block configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Org Babel evaluation and source editing settings.

;;; Code:

(defcustom hub/org-plantuml-jar "~/install/plantuml.jar"
  "Default PlantUML jar path."
  :type 'file
  :group 'hub/org)

(defun hub/org-setup-babel ()
  "Configure Org Babel and source block behavior."
  (setq org-confirm-babel-evaluate nil
	org-src-preserve-indentation t
	org-src-window-setup 'other-window
	org-src-tab-acts-natively t
	org-plantuml-jar-path hub/org-plantuml-jar)
  (add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
  ;; Keep TS/TSX source blocks independent from external language packages.
  ;; `js-mode' gives stable highlighting for export without requiring
  ;; `typescript-mode' or Tree-sitter grammars.
  (add-to-list 'org-src-lang-modes '("typescript" . js))
  (add-to-list 'org-src-lang-modes '("ts" . js))
  (add-to-list 'org-src-lang-modes '("tsx" . js)))

(provide 'org/babel)
;;; babel.el ends here
