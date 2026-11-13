;;; ai.el --- AI assistants integration -*- lexical-binding: t; -*-

;;; Commentary:
;; Codeium, org-ai, whisper configuration.

;;; Code:

(defvar gptel-api-key)
(defvar gptel--known-backends)
(defvar gptel--openai-oauth-token-file)
(defvar org-copilot-gptel-backend)
(defvar org-copilot-gptel-model)

(declare-function gptel-make-openai-oauth "gptel-openai-oauth")
(declare-function gptel-openai-oauth-p "gptel-openai-oauth")
(declare-function gptel-backend-models "gptel-request")

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

(defcustom hub/gptel-openai-oauth-model 'gpt-6-luna
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

(defun hub/gptel-secure-oauth-cache ()
  "Restrict an existing subscription cache to its owner without reading it."
  (when (and (boundp 'gptel--openai-oauth-token-file)
	     (stringp gptel--openai-oauth-token-file)
	     (file-exists-p gptel--openai-oauth-token-file))
    (set-file-modes gptel--openai-oauth-token-file #o600)))

(defun hub/gptel-write-private-oauth-token (write file token)
  "Call WRITE for FILE and TOKEN with owner-only file permissions.
Create parent directories before restricting file creation modes.  Restore
process-wide creation modes even if writing fails; never log TOKEN."
  (make-directory (file-name-directory file) t)
  (when (file-exists-p file)
    (set-file-modes file #o600))
  (let ((modes (default-file-modes)))
    (unwind-protect
	(progn
	  (set-default-file-modes #o600)
	  (funcall write file token))
      (set-default-file-modes modes))))

(with-eval-after-load 'gptel-oauth
  (advice-add 'gptel-oauth--write-token :around
	      #'hub/gptel-write-private-oauth-token))

(defun hub/gptel-subscription-backend ()
  "Return the registered ChatGPT subscription backend without replacing it.
Preserve its authenticated state.  A conflicting named API backend is an
error, not permission to replace it or select a different billing route."
  (require 'gptel-openai-oauth)
  (unless (fboundp 'gptel--openai-oauth-default-backend)
    (user-error
     "Subscription adapter is outdated; install the gptel revision pinned in straight/versions/default.el and rebuild gptel"))
  (hub/gptel-secure-oauth-cache)
  (let ((backend (alist-get "OpenAI-sub" gptel--known-backends nil nil #'equal)))
    (when (and backend (not (gptel-openai-oauth-p backend)))
      (user-error "OpenAI-sub is not a ChatGPT subscription backend"))
    (or backend (gptel-make-openai-oauth "OpenAI-sub"))))

(defun hub/gptel-configure-openai-oauth ()
  "Configure gptel's subscription OAuth without activating Org Copilot.
Fail closed instead of falling back to an API-key backend."
  (when hub/gptel-use-openai-oauth
    (setq gptel-api-key nil
	  gptel-backend nil)
    (condition-case err
	(progn
	  (require 'gptel-openai-oauth)
	  (setq gptel-backend (hub/gptel-subscription-backend)
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

(defun hub/org-copilot-chat-editing-state ()
  "Enter insert state in personal Copilot chat buffers when Evil is available."
  (when (fboundp 'evil-insert-state)
    (evil-insert-state)))

(add-hook 'org-copilot-chat-mode-hook #'hub/org-copilot-chat-editing-state)

;; Also undo the former automatic activation when this config is reloaded.
(remove-hook 'org-mode-hook #'org-copilot-mode)
(add-hook 'org-copilot-mode-hook #'hub/org-copilot-configure-on-activation)

;; Review requests precede mode activation, unlike the chat/panel commands.
(with-eval-after-load 'org-copilot-llm
  (advice-add 'org-copilot-review-dwim :before #'hub/org-copilot-enable-gptel))

(defun hub/org-copilot-babel-with-subscription (generate &rest args)
  "Call GENERATE with ARGS using the selected subscription authentication.
Configure at request time, independently of mode activation and gptel's
main-library load order.  Never fall back to API billing or another model."
  (if (not hub/gptel-use-openai-oauth)
      (apply generate args)
    (let ((gptel-api-key nil)
	  (org-copilot-gptel-backend nil)
	  (org-copilot-gptel-model hub/gptel-openai-oauth-model))
      (condition-case err
	  (progn
	    (unless (and org-copilot-gptel-model
			 (symbolp org-copilot-gptel-model))
	      (user-error "Select a ChatGPT subscription model"))
	    (require 'gptel-openai-oauth)
	    (setq org-copilot-gptel-backend (hub/gptel-subscription-backend))
	    (unless (gptel-openai-oauth-p org-copilot-gptel-backend)
	      (user-error "Configured backend is not ChatGPT subscription OAuth"))
	    (unless (memq org-copilot-gptel-model
			  (gptel-backend-models org-copilot-gptel-backend))
	      (user-error
	       "Model %s is not listed by the subscription adapter; select a supported model explicitly"
	       org-copilot-gptel-model)))
	(error
	 (user-error
	  "Copilot Babel: ChatGPT subscription setup failed: %s. No API fallback was attempted"
	  (error-message-string err))))
      (apply generate args))))

(with-eval-after-load 'ob-copilot
  (advice-add 'org-copilot-babel-gptel-generate :around
	      #'hub/org-copilot-babel-with-subscription))

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
