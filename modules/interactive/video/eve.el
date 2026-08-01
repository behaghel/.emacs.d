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
		   ;; Preserve core Evil muscle memory by jumping to buffer start,
		   ;; not logical line 1: Eve visual rows may be one physical line.
		   (kbd "gg")  #'hub/eve-goto-buffer-start
		   (kbd "G")   #'hub/eve-goto-buffer-end
		   (kbd "gr")  #'eve-reload)
  ;; Relocate displaced Eve commands; split remains available on `|'.
  (define-key eve-mode-map "T" #'eve-toggle-tag)     ; was t
  (define-key eve-mode-map "?" #'eve--show-help))

(provide 'video/eve)

;;; eve.el ends here
