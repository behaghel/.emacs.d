;;; setup-perspective.el --- setup perspectives -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:
(use-package perspective
  :defer 1
  ;; :pin melpa
  :bind (("C-x b" . persp-switch-to-buffer*)
         ("C-x k" . persp-kill-buffer*)
         ;; also for free from projectile integration
         ;; (",pb"   . persp-counsel-switch-buffer)
         ;; the counsel version preview buffers as they get selected,
         ;; useful when unsure what buffer we are looking for
         )
  :config

  (define-key evil-normal-state-map (kbd "gb") 'persp-ivy-switch-buffer)

  (persp-mode t)
  (add-hook 'kill-emacs-hook #'persp-state-save)
  (setq persp-sort 'access
        persp-state-default-file "~/.emacs.d/.persp")
  )

(use-package persp-projectile
  :after evil projectile org perspective
  ;; :straight (:host github :repo "bbatsov/persp-projectile"
  ;;                 :branch "master")
  ;; :defer nil
  ;; :pin melpa
  )
  (defun hub/speed-dial (key persp &optional fpath command)
    (let ((f `(lambda ()
                (interactive)
                (persp-switch ,persp)
                ,@(when fpath `((find-file ,fpath)))
                ,@(when command `((,command)))
                )
             ))
      (define-key evil-normal-state-map (kbd (concat ",o" key)) (eval f)))
    )
  (setq hub/speed-dial-items
    `(
      ("E" perspective ".emacs.d")
      ("e" file "~/.emacs.d/init.el" ".emacs.d")
      ("O" perspective "org")
      ;; ("s" file ,(concat org-directory "sas.org") "org")
      ("i" file ,(concat org-directory "inbox.org") "org")
      ("h" file ,(concat org-directory "hubert.org") "org")
      ("m" command mu4e "mails")
      ("f" command elfeed "feeds")
      ))
  (defun hub/setup-speed-dial ()
    "Install global keybindings on normal mode with prefix ',o'
    for every item in var hub/speed-dial-items."
    (dolist (binding hub/speed-dial-items)
      (pcase binding
        (`(,key perspective ,persp) (hub/speed-dial key persp))
        (`(,key file ,path ,persp) (hub/speed-dial key persp path))
        (`(,key command ,cmd ,persp) (hub/speed-dial key persp nil cmd))
        )
      )
    )
  (hub/setup-speed-dial)

(define-key evil-normal-state-map (kbd ",op") 'projectile-persp-switch-project)
(define-key evil-normal-state-map (kbd "gP") 'persp-switch)

(winner-mode 1)
(define-key evil-window-map (kbd "C-<left>") 'winner-undo)
(define-key evil-window-map (kbd "C-<right>") 'winner-redo)

(provide 'setup-perspective)
;;; setup-perspective.el ends here
