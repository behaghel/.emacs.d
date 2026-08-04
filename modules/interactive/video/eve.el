;;; eve.el --- eve package integration -*- lexical-binding: t; -*-

;;; Commentary:
;;; Code:

(require 'hub-utils)

(defconst hub/eve-straight-recipe
  '(eve :type git :host github :repo "behaghel/eve.el"
	:local-repo "eve.el"
	:build (:not compile))
  "straight.el recipe for the standalone `eve' package.")

(defun hub/eve--ensure-package ()
  "Load `eve' preferring local checkout, falling back to GitHub.
Try ~/ws/eve.el first; if absent, use the straight.el GitHub recipe."
  (cond
   ((featurep 'eve) t)
   ((file-directory-p "~/ws/eve.el")
    (add-to-list 'load-path (expand-file-name "~/ws/eve.el"))
    (require 'eve))
   ((fboundp 'straight-use-package)
    (straight-use-package hub/eve-straight-recipe)
    (require 'eve))
   ((require 'eve nil 'noerror) t)
   (t
    (error "Unable to load eve; straight.el recipe %S is unavailable"
	   hub/eve-straight-recipe))))

(hub/eve--ensure-package)

(setq eve-filler-phrases
      '("um" "uh" "you know" "I mean" "like"))

(defun hub/eve-goto-buffer-start ()
  "Move point to the first buffer position in `eve-mode'."
  (interactive)
  (goto-char (point-min)))

(defun hub/eve-goto-buffer-end ()
  "Move point to the last buffer position in `eve-mode'."
  (interactive)
  (goto-char (point-max)))

(with-eval-after-load 'evil
  (evil-set-initial-state 'eve-mode 'normal)
  (when (boundp 'eve-segment-panel-mode-map)
    (evil-set-initial-state 'eve-segment-panel-mode 'normal))
  (define-key eve-mode-map (kbd "g") nil) ; let Evil's `g' prefix handle `gg'.
  (evil-make-overriding-map eve-mode-map 'normal)
  (evil-define-key 'normal eve-mode-map
		   ;; Bépo muscle-memory adaptation: down/up becomes next/previous segment.
		   (kbd "t")   #'eve-next-segment
		   (kbd "s")   #'eve-previous-segment
		   (kbd "C-t") #'eve-next-segment
		   (kbd "C-s") #'eve-previous-segment
		   (kbd "M-T") #'eve-move-segment-down
		   (kbd "M-S") #'eve-move-segment-up
		   (kbd "p")   #'eve-paste-segment
		   (kbd "P")   #'eve-paste-segment-before
		   (kbd "y")   #'eve-copy-segment
		   (kbd "{")   #'eve-previous-marker
		   (kbd "}")   #'eve-next-marker
		   (kbd "TAB") #'eve-toggle-marker-fold
		   (kbd "<backtab>") #'eve-toggle-all-marker-folds
		   (kbd "S-TAB") #'eve-toggle-all-marker-folds
		   ;; Preserve core Evil muscle memory by jumping to buffer start,
		   ;; not logical line 1: Eve visual rows may be one physical line.
		   (kbd "gg")  #'hub/eve-goto-buffer-start
		   (kbd "G")   #'hub/eve-goto-buffer-end
		   (kbd "gr")  #'eve-reload)
  (when (boundp 'eve-segment-panel-mode-map)
    ;; Keep the bottom panel as a dedicated Eve cockpit.  Evil normal/motion
    ;; state bindings can shadow `special-mode' maps, so bind every panel action
    ;; explicitly and promote this map to Evil's intercept precedence.
    (evil-define-key* '(normal motion) eve-segment-panel-mode-map
		      (kbd "w") #'eve-segment-panel-next-word
		      (kbd "é") #'eve-segment-panel-next-word
		      (kbd "b") #'eve-segment-panel-previous-word
		      (kbd "$") #'eve-segment-panel-last-word
		      (kbd "v") #'eve-segment-panel-toggle-selection
		      (kbd "j") #'eve-segment-panel-next-segment
		      (kbd "n") #'eve-segment-panel-next-segment
		      (kbd "t") #'eve-segment-panel-next-segment
		      (kbd "C-t") #'eve-segment-panel-next-segment
		      (kbd "k") #'eve-segment-panel-previous-segment
		      (kbd "p") #'eve-segment-panel-paste-after
		      (kbd "s") #'eve-segment-panel-previous-segment
		      (kbd "C-s") #'eve-segment-panel-previous-segment
		      (kbd "i") #'eve-segment-panel-edit-words
		      (kbd "E") #'eve-segment-panel-edit-segment
		      (kbd "r") #'eve-segment-panel-regenerate-transcript
		      (kbd "U") #'eve-segment-panel-regenerate-until
		      (kbd "~") #'eve-segment-panel-capitalize-words
		      (kbd ".") #'eve-segment-panel-add-dot
		      (kbd ",,") #'eve-segment-panel-add-comma
		      (kbd "d") #'eve-segment-panel-delete-words
		      (kbd "D") #'eve-segment-panel-delete-segment
		      (kbd "f") #'eve-segment-panel-mark-filler
		      (kbd "A") #'eve-segment-panel-accept-filler
		      (kbd "F") #'eve-segment-panel-delete-fillers
		      (kbd "|") #'eve-segment-panel-split
		      (kbd "B") #'eve-segment-panel-edit-broll
		      (kbd "C") #'eve-segment-panel-toggle-broll-continue
		      (kbd "y") #'eve-segment-panel-copy
		      (kbd "P") #'eve-segment-panel-paste-before
		      (kbd "N") #'eve-segment-panel-edit-notes
		      (kbd "T") #'eve-segment-panel-toggle-tag
		      (kbd "I") #'eve-segment-panel-edit-speaker
		      (kbd "x") #'eve-segment-panel-edit-range
		      (kbd "gg") #'eve-segment-panel-first-segment
		      (kbd "G") #'eve-segment-panel-last-segment
		      (kbd "{") #'eve-segment-panel-previous-marker
		      (kbd "}") #'eve-segment-panel-next-marker
		      (kbd "TAB") #'eve-segment-panel-toggle-marker-fold
		      (kbd "<backtab>") #'eve-segment-panel-toggle-all-marker-folds
		      (kbd "S-TAB") #'eve-segment-panel-toggle-all-marker-folds
		      (kbd "O") #'eve-segment-panel-insert-marker
		      (kbd "SPC") #'eve-segment-panel-play-source
		      (kbd "R") #'eve-segment-panel-play-rendered
		      (kbd "c") #'eve-segment-panel-cache-current
		      (kbd "m") #'eve-segment-panel-merge-next
		      (kbd "J") #'eve-segment-panel-merge-next
		      (kbd "M") #'eve-segment-panel-merge-previous
		      (kbd "u") #'eve-segment-panel-undo
		      (kbd "C-r") #'eve-segment-panel-redo
		      (kbd "C-x C-s") #'eve-segment-panel-save
		      (kbd "C-c C-c") #'eve-segment-panel-compile
		      (kbd "C-c C-r") #'eve-segment-panel-play-rendered
		      (kbd "C-c C-s") #'eve-segment-panel-play-source-continuous
		      (kbd "C-c C-v") #'eve-segment-panel-validate
		      (kbd "q") #'eve-segment-panel-focus-source
		      (kbd "Q") #'eve-segment-panel-close)
    (evil-make-intercept-map eve-segment-panel-mode-map 'normal)
    (evil-make-intercept-map eve-segment-panel-mode-map 'motion)
    (evil-normalize-keymaps))
  ;; Relocate displaced Eve commands; split remains available on `|'.
  (define-key eve-mode-map "T" #'eve-toggle-tag)     ; was t
  (define-key eve-mode-map "?" #'eve--show-help))

(provide 'video/eve)

;;; eve.el ends here
