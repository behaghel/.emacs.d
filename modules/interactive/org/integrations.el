;;; integrations.el --- Org mode: package integrations -*- lexical-binding: t; -*-

;;; Commentary:
;; Org-adjacent package declarations and post-load integrations.

;;; Code:

(require 'hub-utils)

(use-package org-re-reveal :defer t)

(defvar hub/org-background-agenda-render-p nil
  "Non-nil while rendering Org agenda data for non-interactive consumers.")

(defun hub/evil-org-enable-for-interactive-org-buffer ()
  "Enable Evil Org unless the Org buffer belongs to background agenda work."
  (unless hub/org-background-agenda-render-p
    (evil-org-mode 1)))

(defun hub/evil-org-setup-agenda-keybindings ()
  "Install Evil Org agenda keys for interactive agenda buffers."
  (unless hub/org-background-agenda-render-p
    (require 'evil-org-agenda)
    (evil-org-agenda-set-keys)
    (evil-collection-translate-key 'motion 'org-agenda-mode-map
				   "c" "h" "C" "H" "t" "j" "T" "J" "s" "k" "S" "K" "r" "l" "R" "L"
				   ":" "t" "H" "T" "e" "c" "a" "c" "L" "C" "é" "r" "É" "R" "|" "s" "K" "S")))

(use-package evil-org
  :commands (evil-org-mode)
  :init
  (add-hook 'org-mode-hook #'hub/evil-org-enable-for-interactive-org-buffer)
  (add-hook 'org-agenda-mode-hook #'hub/evil-org-setup-agenda-keybindings)
  :config
  (setq evil-org-movement-bindings '((up . "s") (down . "t") (left . "c") (right . "r")))
  (evil-org-set-key-theme '(textobjects return insert navigation additional shift calendar)))

(use-package ox-clip
  :commands (ox-clip-formatted-copy)
  :init
  (with-eval-after-load 'org
    (evil-define-key 'visual org-mode-map (kbd ",y") #'ox-clip-formatted-copy)))

(use-package org-cliplink
  :after org
  :bind (:map evil-normal-state-map (",eP" . org-cliplink)))

(use-package anki-editor
  :after org
  :hook (org-capture-after-finalize . anki-editor-reset-cloze-number)
  :config
  (evil-collection-define-key 'visual 'org-mode-map ",_" 'anki-editor-cloze-region-dont-incr ",-" 'anki-editor-cloze-region-auto-incr)
  (evil-collection-define-key 'normal 'org-mode-map ",0" 'anki-editor-reset-cloze-number ",^" 'anki-editor-push-tree)
  (defun anki-editor-cloze-region-auto-incr (&optional _arg)
    (interactive)
    (anki-editor-cloze-region my-anki-editor-cloze-number "")
    (setq my-anki-editor-cloze-number (1+ my-anki-editor-cloze-number))
    (forward-sexp))
  (defun anki-editor-cloze-region-dont-incr (&optional _arg)
    (interactive)
    (anki-editor-cloze-region (1- my-anki-editor-cloze-number) "")
    (forward-sexp))
  (defun anki-editor-reset-cloze-number (&optional arg)
    (interactive)
    (setq my-anki-editor-cloze-number (or arg 1)))
  (defun anki-editor-push-tree ()
    (interactive)
    (anki-editor-push-notes '(4))
    (anki-editor-reset-cloze-number))
  (anki-editor-reset-cloze-number)
  (add-to-list 'org-capture-templates
	       '("a" "Anki basic" entry (file+headline org-default-notes-file "Anki")
		 "* %U   %^g\n:PROPERTIES:\n:ANKI_NOTE_TYPE: Basic\n:ANKI_DECK: from-org\n:END:\n** Front\n%?\n** Back\n%x\n"))
  (add-to-list 'org-capture-templates
	       '("A" "Anki cloze" entry (file+headline org-default-notes-file "Anki")
		 "* %U   %^g\n:PROPERTIES:\n:ANKI_NOTE_TYPE: Cloze\n:ANKI_DECK: from-org\n:END:\n** Text\n%x\n** Extra\n")))

(use-package org-download
  :after org
  :hook (dired-mode-hook . org-download-enable)
  :config
  (evil-collection-define-key 'normal 'org-mode-map ",Y" 'org-download-clipboard)
  (setq-default org-download-method 'attach
		org-download-heading-lvl nil
		org-download-delete-image-after-download t))

(use-package org-drill :after org :defer 10)
(use-package citeproc :defer t)

(provide 'org/integrations)
;;; integrations.el ends here
