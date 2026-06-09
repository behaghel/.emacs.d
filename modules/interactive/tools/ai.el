;;; ai.el --- AI assistants integration -*- lexical-binding: t; -*-

;;; Commentary:
;; Codeium, org-ai, whisper configuration.

;;; Code:

(use-package codeium
  :straight '(:type git :host github :repo "Exafunction/codeium.el")
  :disabled t
  :hook (prog-mode . (lambda ()
		       (setq-local completion-at-point-functions
				   (list (cape-super-capf #'codeium-completion-at-point
							  #'eglot-completion-at-point)))))
  :config
  (setq use-dialog-box nil))

(use-package org-ai
  :straight '(:type git :host github :repo "rksm/org-ai")
  :commands (org-ai-mode org-ai-global-mode)
  :init
  (setq org-ai-default-chat-model "gpt-4"
	org-ai-openai-api-token nil
	org-ai-auto-fill t)
  :config
  ;; `org-ai' hardcodes <A for `ai', which collides with Org Tempo's
  ;; built-in <A ASCII export keyword and emits a startup warning.
  (when (boundp 'org-structure-template-alist)
    (setq org-structure-template-alist
	  (assoc-delete-all "A" org-structure-template-alist)))
  (org-ai-install-yasnippets))

(use-package whisper
  :straight '(:type git :host github :repo "natrys/whisper.el")
  :commands (whisper-run whisper-file whisper-select-language)
  :init
  (setq whisper-install-directory "/tmp/"
	whisper-model "base"
	whisper-language "en"
	whisper-translate nil
	whisper-use-threads (/ (num-processors) 2)))

(provide 'tools/ai)
;;; ai.el ends here
