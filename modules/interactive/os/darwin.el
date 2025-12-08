;;; darwin.el --- macOS-specific interactive tweaks -*- lexical-binding: t; -*-

;;; Commentary:
;; Unbind M-SPC on macOS to avoid conflicts with Bépo underscore input.
;; Loaded only in interactive sessions on Darwin.

;;; Code:

(when (eq system-type 'darwin)
  ;; Free Meta-SPC globally so macOS/Bépo layouts can use it for "_".
  (global-unset-key (kbd "M-SPC"))

  ;; Ensure Evil states don't grab M-SPC either.
  (with-eval-after-load 'evil
    (define-key evil-normal-state-map (kbd "M-SPC") nil)
    (define-key evil-insert-state-map (kbd "M-SPC") nil)
    (define-key evil-motion-state-map (kbd "M-SPC") nil)
    (define-key evil-visual-state-map (kbd "M-SPC") nil)))

(provide 'os/darwin)
;;; darwin.el ends here
