;;; eve.el --- eve package integration -*- lexical-binding: t; -*-

;;; Commentary:
;;; Code:

(defconst hub/eve-straight-recipe
  '(eve :type git :host github :repo "behaghel/eve.el"
	:local-repo "eve.el"
	:build (:not compile))
  "straight.el recipe for the standalone `eve' package.")

(defun hub/eve--ensure-package ()
  "Load `eve' from its straight-managed GitHub checkout."
  (cond
   ((featurep 'eve) t)
   ((fboundp 'straight-use-package)
    (straight-use-package hub/eve-straight-recipe)
    (require 'eve))
   ((require 'eve nil 'noerror) t)
   (t
    (error "Unable to load eve; straight.el recipe %S is unavailable"
	   hub/eve-straight-recipe))))

(hub/eve--ensure-package)

(with-eval-after-load 'evil
  (evil-define-key 'normal eve-mode-map
		   (kbd "RET") #'eve-play-segment
		   (kbd "SPC") #'eve-play-segment
		   (kbd "C-t") #'eve-next-segment
		   (kbd "C-s") #'eve-previous-segment
		   (kbd "M-t") #'eve-move-segment-down
		   (kbd "M-s") #'eve-move-segment-up
		   (kbd "M-j") #'eve-insert-marker
		   (kbd "t") #'evil-next-visual-line
		   (kbd "s") #'evil-previous-visual-line
		   (kbd "T") #'evil-next-visual-line
		   (kbd "S") #'evil-previous-visual-line
		   (kbd "k") #'eve-delete-segment
		   (kbd "|") #'eve-split-segment
		   (kbd "J") #'eve-merge-with-next
		   (kbd "d") #'eve-delete-word
		   (kbd "i") #'eve-edit-text
		   (kbd "u") #'eve-undo))

(with-eval-after-load 'general
  (require 'hub-keys nil 'noerror)
  (when (fboundp 'hub/define-leaders)
    (hub/define-leaders))
  (when (fboundp 'hub/localleader)
    (hub/localleader
     :states '(normal motion)
     :keymaps 'eve-mode-map
     "n" #'eve-next-segment
     "p" #'eve-previous-segment
     "N" #'eve-move-segment-down
     "P" #'eve-move-segment-up
     "d" #'eve-delete-segment
     "e" #'eve-edit-text
     "s" #'eve-edit-speaker
     "t" #'eve-toggle-tag
     "b" #'eve-edit-broll
     "B" #'eve-toggle-broll-continue
     "w" #'eve-toggle-words
     "W" #'eve-delete-word
     "S" #'eve-split-segment
     "m" #'eve-insert-marker
     "j" #'eve-toggle-separator
     "r" #'eve-edit-start-end
     "C-n" #'eve-edit-notes
     "v" #'eve-validate
     "o" #'eve-open-raw-json
     "k" #'eve-stop-playback
     "u" #'eve-undo
     "R" #'eve-redo
     "l" #'eve-reload
     "?" #'eve-hydra/body)))

(provide 'video/eve)

;;; eve.el ends here
