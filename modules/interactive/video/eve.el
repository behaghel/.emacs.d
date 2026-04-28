;;; eve.el --- eve package integration -*- lexical-binding: t; -*-

;;; Commentary:
;;; Code:

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

(with-eval-after-load 'evil
  (evil-make-overriding-map eve-mode-map 'normal)
  (evil-define-key 'normal eve-mode-map
		   ;; Bépo nav — t/s stay as Evil down/up
		   (kbd "t")   #'evil-next-visual-line
		   (kbd "s")   #'evil-previous-visual-line
		   ;; Segment nav
		   (kbd "C-t") #'eve-next-segment
		   (kbd "C-s") #'eve-previous-segment
		   (kbd "M-T") #'eve-move-segment-down
		   (kbd "M-S") #'eve-move-segment-up)
  ;; Relocate the displaced eve commands
  (define-key eve-mode-map "T" #'eve-toggle-tag)     ; was t
  (define-key eve-mode-map "?" #'eve--show-help))

(provide 'video/eve)

;;; eve.el ends here
