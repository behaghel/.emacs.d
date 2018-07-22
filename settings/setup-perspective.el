(use-package perspective
  :defer 1
  :config
  ;; taken from Magnar Sveen
  ;; Jump to last perspective
  ;; (defun custom-persp-last ()
  ;;   (interactive)
  ;;   (persp-switch (persp-name persp-last)))
  ;; ;; Easily switch to your last perspective
  ;; (defmacro custom-persp (name &rest body)
  ;;   `(let ((current-perspective (persp-curr)))
  ;;      (persp-switch ,name)
  ;;      ,@body
  ;;      (setq persp-last current-perspective)))

  (persp-mode t)
  ;; open file in project
  (use-package persp-projectile
    :pin melpa
    :bind (:map evil-normal-state-map
                (",op" . projectile-persp-switch-project)
                (",pp" . persp-switch-last)
                (",gp" . persp-switch)
                (",pk" . persp-remove-buffer) ; disassociate buffer from persp
                (",pr" . persp-rename)
                (",px" . persp-kill) ; terminate perspective
                (",pa" . persp-add-buffer) ; associate buffer to current persp
                (",pA" . persp-set-buffer) ; like add but remove from all other
                                        ; open init.el
                (",oe" . custom-persp/emacs)
                                        ; open hubert.org
                (",oh" . custom-persp/hubert)
                (",os" . custom-persp/sas)))

  (defun custom-persp/emacs ()
    (interactive)
    (persp-switch ".emacs.d")
    (find-file "~/.emacs.d/init.el"))
  (defun custom-persp/sas ()
    (interactive)
    (persp-switch "org")
    (find-file "~/Dropbox/Documents/org/sas.org"))
  (defun custom-persp/hubert ()
    (interactive)
    (persp-switch "org")
    (find-file "~/Dropbox/Documents/org/hubert.org")))

(provide 'setup-perspective)