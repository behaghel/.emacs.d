;;; ai.el --- AI assistants integration -*- lexical-binding: t; -*-

;;; Commentary:
;; Codeium, org-ai, whisper configuration.

;;; Code:

(defgroup hub/ai nil
  "Personal AI integration settings."
  :group 'tools)

(defcustom hub/gptel-cache-api-key t
  "Whether to memoize the OpenAI API key for the current Emacs session."
  :type 'boolean
  :group 'hub/ai)

(defcustom hub/gptel-use-openai-oauth t
  "Whether to use ChatGPT subscription OAuth for gptel by default."
  :type 'boolean
  :group 'hub/ai)

(defcustom hub/gptel-openai-oauth-model 'gpt-5.5
  "Default model for the ChatGPT subscription OAuth gptel backend."
  :type 'symbol
  :group 'hub/ai)

(defvar hub/gptel--cached-api-key nil
  "Cached OpenAI API key for gptel requests.")

(defun hub/gptel-clear-api-key-cache ()
  "Clear the cached OpenAI API key used by gptel."
  (interactive)
  (setq hub/gptel--cached-api-key nil)
  (message "Cleared cached gptel API key"))

(defun hub/gptel-api-key ()
  "Return the OpenAI API key for gptel, optionally memoized."
  (or (and hub/gptel-cache-api-key hub/gptel--cached-api-key)
      (let ((secret (or (auth-source-pass-get
			 'secret "veriff/api.openai.com/apikey")
			(user-error
			 "No OpenAI key in pass entry veriff/api.openai.com/apikey"))))
	(when hub/gptel-cache-api-key
	  (setq hub/gptel--cached-api-key secret))
	secret)))

(defun hub/gptel-configure-openai-oauth ()
  "Configure gptel's subscription OAuth without activating Org Copilot.
Fail closed instead of falling back to an API-key backend."
  (when hub/gptel-use-openai-oauth
    (setq gptel-api-key nil
	  gptel-backend nil)
    (condition-case err
	(progn
	  (require 'gptel-openai-oauth)
	  (setq gptel-backend (gptel-make-openai-oauth "OpenAI-sub")
		gptel-model hub/gptel-openai-oauth-model))
      (error
       (message "gptel: ChatGPT subscription OAuth unavailable: %s"
		(error-message-string err))))))

(use-package gptel
  :commands (gptel gptel-send gptel-request)
  :init
  (require 'auth-source-pass)
  (auth-source-pass-enable)
  (setq gptel-api-key #'hub/gptel-api-key)
  :config
  (hub/gptel-configure-openai-oauth))

(use-package gptel-agent
  :after gptel
  :demand t
  :commands (gptel-agent gptel-agent-update)
  :config
  (gptel-agent-update))

(defun hub/gptel-agent-enable-tools ()
  "Load gptel-agent tools when available."
  (when (require 'gptel-agent nil 'noerror)
    (gptel-agent-update)))

(defun hub/org-copilot-enable-gptel ()
  "Configure Org Copilot to use the selected gptel authentication route."
  (if (not (require 'org-copilot-gptel nil 'noerror))
      (message (concat "Org Copilot: org-copilot-gptel adapter unavailable; "
		       "restore it and re-enable org-copilot-mode"))
    (if hub/gptel-use-openai-oauth
	(condition-case err
	    (org-copilot-gptel-use-openai-oauth hub/gptel-openai-oauth-model)
	  (error
	   (setq org-copilot-gptel-backend nil
		 org-copilot-gptel-model nil
		 org-copilot-chat-function nil
		 org-copilot-review-function nil)
	   (message "Org Copilot: ChatGPT subscription OAuth unavailable: %s"
		    (error-message-string err))))
      (condition-case err
	  (org-copilot-gptel-enable)
	(error
	 (message "Org Copilot: failed to enable gptel provider: %s"
		  (error-message-string err)))))))

(defun hub/org-copilot-configure-on-activation ()
  "Configure Copilot's adapter only when its source mode is enabled."
  (when (bound-and-true-p org-copilot-mode)
    (hub/org-copilot-enable-gptel)))

;; Also undo the former automatic activation when this config is reloaded.
(remove-hook 'org-mode-hook #'org-copilot-mode)
(add-hook 'org-copilot-mode-hook #'hub/org-copilot-configure-on-activation)

;; Review requests precede mode activation, unlike the chat/panel commands.
(with-eval-after-load 'org-copilot-llm
  (advice-add 'org-copilot-review-dwim :before #'hub/org-copilot-enable-gptel))

(with-eval-after-load 'gptel
  (hub/gptel-agent-enable-tools))

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
