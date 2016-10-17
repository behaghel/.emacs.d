;; multiple-cursors
(use-package multiple-cursors
  :commands (mc/mark-next-like-this mc/mark-previous-like-this mc/edit-lines mc/mark-all-like-this-dwim)
  :config
  (global-set-key (kbd "<M-C-down>") 'mc/mark-next-like-this)
  (global-set-key (kbd "<M-C-up>") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c m @") 'mc/edit-lines)
  (global-set-key (kbd "C-c m a") 'mc/mark-all-like-this-dwim)
  ;; compat with evil. Stolen https://github.com/jcpetkovich/.emacs.d/blob/master/global-key-bindings.el#L257
  ;; alternative: https://github.com/magnars/multiple-cursors.el/issues/19
  ;; waiting for resolution of:
  ;; https://github.com/magnars/multiple-cursors.el/issues/17
  (defvar mc-evil-compat/evil-prev-state nil)
  (defvar mc-evil-compat/mark-was-active nil)
  (defun mc-evil-compat/switch-to-emacs-state ()
    (when (and (bound-and-true-p evil-mode)
               (not (memq evil-state '(insert emacs))))
      (setq mc-evil-compat/evil-prev-state evil-state)
      (when (region-active-p)
        (setq mc-evil-compat/mark-was-active t))
      (let ((mark-before (mark))
            (point-before (point)))
        (evil-emacs-state 1)
        (when (or mc-evil-compat/mark-was-active (region-active-p))
          (goto-char point-before)
          (set-mark mark-before)))))
  (defun mc-evil-compat/back-to-previous-state ()
    (when mc-evil-compat/evil-prev-state
      (unwind-protect
          (case mc-evil-compat/evil-prev-state
            ((normal visual) (evil-force-normal-state))
            (t (message "Don't know how to handle previous state: %S"
                        mc-evil-compat/evil-prev-state)))
        (setq mc-evil-compat/evil-prev-state nil)
        (setq mc-evil-compat/mark-was-active nil))))
  (add-hook 'multiple-cursors-mode-enabled-hook
            'mc-evil-compat/switch-to-emacs-state)
  (add-hook 'multiple-cursors-mode-disabled-hook
            'mc-evil-compat/back-to-previous-state)
  (defun mc-evil-compat/rectangular-switch-state ()
    (if rectangular-region-mode
        (mc-evil-compat/switch-to-emacs-state)
      (setq mc-evil-compat/evil-prev-state nil)))
  ;; When running edit-lines, point will return (position + 1) as a
  ;; result of how evil deals with regions
  (defadvice mc/edit-lines (before change-point-by-1 activate)
    (when (and (bound-and-true-p evil-mode)
               (not (memq evil-state '(insert emacs))))
      (if (> (point) (mark))
          (goto-char (1- (point)))
        (push-mark (1- (mark))))))
  (add-hook 'rectangular-region-mode-hook 'mc-evil-compat/rectangular-switch-state)
  (defvar mc--default-cmds-to-run-once nil))

(provide 'setup-multiple-cursors)