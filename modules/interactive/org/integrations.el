;;; integrations.el --- Org mode: package integrations -*- lexical-binding: t; -*-

;;; Commentary:
;; Org-adjacent package declarations and post-load integrations.

;;; Code:

(use-package org-re-reveal :defer t)

(use-package evil-org
  :after (evil org)
  :hook (org-mode . evil-org-mode)
  :config
  (setq evil-org-movement-bindings '((up . "s") (down . "t") (left . "c") (right . "r")))
  (evil-org-set-key-theme '(textobjects return insert navigation additional shift calendar))
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys)
  (defun hub/setup-agenda-keybindings ()
    (evil-collection-translate-key 'motion 'org-agenda-mode-map
				   "c" "h" "C" "H" "t" "j" "T" "J" "s" "k" "S" "K" "r" "l" "R" "L"
				   ":" "t" "H" "T" "e" "c" "a" "c" "L" "C" "é" "r" "É" "R" "|" "s" "K" "S"))
  (add-hook 'org-agenda-mode-hook #'hub/setup-agenda-keybindings))

(use-package ox-clip
  :config (evil-define-key 'visual org-mode-map (kbd ",y") 'ox-clip-formatted-copy))

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
