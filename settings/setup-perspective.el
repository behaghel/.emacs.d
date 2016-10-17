(use-package perspective
  :defer 1
  :config
  ;; taken from Magnar Sveen
  ;; Jump to last perspective
  (defun custom-persp-last ()
    (interactive)
    (persp-switch (persp-name persp-last)))
  ;; Easily switch to your last perspective
  (defmacro custom-persp (name &rest body)
    `(let ((current-perspective persp-curr))
       (persp-switch ,name)
       ,@body
       (setq persp-last current-perspective)))

  (persp-mode t)

  (defun custom-persp/emacs ()
    (interactive)
    (custom-persp ".emacs.d"
                  (find-file "~/.emacs.d/init.el")))
  (defun custom-persp/sas ()
    (interactive)
    (custom-persp "org"
                  (find-file "~/Documents/org/sas.org")))
  (defun custom-persp/hubert ()
    (interactive)
    (custom-persp "org"
                  (find-file "~/Dropbox/Documents/org/hubert.org"))))

(provide 'setup-perspective)