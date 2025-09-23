;;; compose.el --- Email: compose + org-msg + sending -*- lexical-binding: t; -*-

;;; Commentary:
;; Compose workflow using org-msg and msmtp sendmail setup.

;;; Code:

(require 'hub-utils)
(require 'mail-utils)

(declare-function message-point-in-cited-region "message")
(declare-function message-point-in-header-p "message")

(defgroup hub/tmp nil
  "Temporary directory configuration."
  :group 'environment)

(defcustom hub/tmp-directory (expand-file-name "var/tmp/" user-emacs-directory)
  "Base directory for temporary files created by helper modes."
  :type 'directory
  :group 'hub/tmp)

(defcustom hub/message-compose-fill-column 72
  "Fill column to use when manually refilling new paragraphs in compose buffers."
  :type 'integer
  :group 'hub/mu4e)

(defun hub/message-fill-paragraph-safely (&optional justify)
  "Fill the current paragraph when it is safe to do so.
Skips cited/quoted regions unless JUSTIFY is non-nil."
  (interactive "P")
  (when (and (fboundp 'message-point-in-header-p)
	     (message-point-in-header-p))
    (user-error "Refusing to reflow message headers"))
  (when (and (not justify)
	     (or (and (fboundp 'message-point-in-cited-region)
		      (message-point-in-cited-region))
		 (save-excursion
		   (beginning-of-line)
		   (looking-at-p "[[:space:]]*>+"))))
    (user-error "Refusing to reflow quoted text; use C-u M-q to override"))
  (let ((fill-column (or (and (numberp hub/message-compose-fill-column)
			      hub/message-compose-fill-column)
			 (and (boundp 'message-fill-column)
			      (numberp message-fill-column)
			      message-fill-column)
			 fill-column)))
    (fill-paragraph justify)
    (message "Paragraph refilled at column %d" fill-column)))

(defun hub/message-compose-softwrap ()
  "Configure compose buffers for flowed text without hard wrapping."
  (turn-off-auto-fill)
  (setq-local fill-column 998
	      message-fill-column nil
	      mml-enable-flowed t
	      sentence-end-double-space nil
	      use-hard-newlines t))

(defun make-tmp-file-browsable ()
  "Allow temporary files to be accessed by the browser."
  (interactive)
  (let ((dir hub/tmp-directory))
    (unless (file-directory-p dir) (make-directory dir t))
    (setq-local temporary-file-directory dir)))

;; Toggle OrgMsg integration (disabled by default while we stabilize basics)
(defcustom hub/use-org-msg nil
  "Enable org-msg integration for composing HTML emails."
  :type 'boolean
  :group 'hub/tmp)

(use-package org-msg
  :if hub/use-org-msg
  :after org
  :config
  (setq org-msg-options "html-postamble:nil H:5 num:nil ^:{} toc:nil author:nil email:nil \\
:t tex:dvipng"
	org-msg-startup "hidestars indent inlineimages"
	org-msg-greeting-fmt "\nHi%s,\n\n"
	org-msg-greeting-name-limit 3
	org-msg-text-plain-alternative t
	org-msg-posting-style 'top-posting
	org-msg-default-alternatives '((new           . (text html))
				       (reply-to-html . (text html))
				       (reply-to-text . (text html)))
	org-msg-convert-citation t
	org-msg-signature "\n#+begin_signature\n--\n\nHubert\n#+end_signature")
  (add-hook 'org-msg-mode-hook 'make-tmp-file-browsable)
  (add-hook 'org-msg-mode-hook 'turn-off-auto-fill)
  (add-hook 'org-msg-edit-mode-hook 'hub/message-compose-softwrap)
  (add-hook 'org-msg-edit-mode-hook 'hub/email-enable-visual-wrap)

  (defun my-org-msg-composition-parameters (orig-fun &rest args)
    "Adjust greeting and signature for plain/text replies."
    (let ((res (apply orig-fun args)))
      (when (equal (cadr args) '(text))
	(setf (alist-get 'signature res)
	      (replace-regexp-in-string "\\.html.*" "" org-msg-signature)))
      res))
  (advice-add 'org-msg-composition-parameters :around #'my-org-msg-composition-parameters)
  (org-msg-mode))

;; Ensure intuitive header navigation/completion in message headers
(with-eval-after-load 'message
  (add-hook 'message-mode-hook 'hub/message-compose-softwrap)
  (add-hook 'message-mode-hook 'hub/email-enable-visual-wrap)
  ;; In the headers area: TAB completes addresses and jumps to the next field;
  ;; on Subject, it jumps into the body. This mirrors message-mode defaults and
  ;; guards against other modes shadowing TAB.
  (define-key message-mode-map (kbd "TAB") #'message-tab)
  (define-key message-mode-map (kbd "M-q") #'hub/message-fill-paragraph-safely)
  ;; Reply citation line: include date/time and sender name, omit email address
  ;; Example: "On Sat, Sep 13, 2025 at 3:34 PM Alice writes:"
  (setq message-citation-line-function 'message-insert-formatted-citation-line
	message-citation-line-format     "On %a, %b %e, %Y at %l:%M %p %f writes:")
  (setq fill-flowed-encode-column 998)
  ;; Make ESC produce [escape] (not Meta prefix) locally in compose buffers so
  ;; Evil can reliably catch it to exit insert state.
  (defun hub/message-compose-esc-setup ()
    (setq-local meta-prefix-char nil))
  (add-hook 'message-mode-hook #'hub/message-compose-esc-setup)
  ;; Convenience: quick jumps (native in message-mode too via C-c C-f t/s/b)
  ;; Keep the standard bindings; no changes needed here.
  ;; Ensure ESC always returns to Evil normal in compose buffers
  (with-eval-after-load 'evil
    (define-key message-mode-map (kbd "<escape>") #'evil-force-normal-state)
    (define-key message-mode-map (kbd "C-[") #'evil-force-normal-state))
  (with-eval-after-load 'general
    (hub/define-leaders)
    (hub/localleader
     :states '(normal visual)
     :keymaps 'message-mode-map
     "q" #'hub/message-fill-paragraph-safely)))

;; Make TAB in org-msg buffers truly DWIM: if point is in headers, run
;; `message-tab` (complete + jump to next field); otherwise, use Org's cycle.
(with-eval-after-load 'org-msg
  (defun hub/org-msg-tab-dwim ()
    "DWIM TAB in OrgMsg: headers → `message-tab`, body → `org-cycle`."
    (interactive)
    (cond
     ;; Prefer message's own header/body detector when available
     ((and (fboundp 'message-in-body-p)
	   (not (message-in-body-p))) (message-tab))
     ;; Fallback heuristic using org-msg-start
     ((and (fboundp 'org-msg-start)
	   (< (point) (save-excursion (org-msg-start)))) (message-tab))
     (t (org-cycle))))
  (define-key org-msg-edit-mode-map (kbd "<tab>") #'hub/org-msg-tab-dwim)
  (define-key org-msg-edit-mode-map (kbd "M-q") #'hub/message-fill-paragraph-safely)
  (with-eval-after-load 'general
    (hub/define-leaders)
    (hub/localleader
     :states '(normal visual)
     :keymaps 'org-msg-edit-mode-map
     "q" #'hub/message-fill-paragraph-safely)))

;; Ensure Evil plays nicely in compose buffers
(with-eval-after-load 'org-msg
  (with-eval-after-load 'evil
    ;; Allow returning to normal state from insert in org-msg editors
    (evil-define-key 'insert org-msg-edit-mode-map (kbd "<escape>") 'evil-normal-state)
    (evil-define-key 'insert org-msg-edit-mode-map (kbd "C-[") 'evil-normal-state)))

;; Make C-c C-c send in org-msg buffers (bypass org-ctrl-c-ctrl-c semantics)
(with-eval-after-load 'org-msg
  (define-key org-msg-edit-mode-map (kbd "C-c C-c") #'org-msg-ctrl-c-ctrl-c))

;; Plain message-mode: ensure C-c C-c sends, and TAB completes + jumps
(with-eval-after-load 'message
  (define-key message-mode-map (kbd "C-c C-c") #'message-send-and-exit)
  (define-key message-mode-map (kbd "TAB") #'message-tab))

;; Ensure ESC works in mu4e-compose-mode as well
(with-eval-after-load 'mu4e-compose
  (setq mu4e-compose-format-flowed t)
  (define-key mu4e-compose-mode-map (kbd "M-q") #'hub/message-fill-paragraph-safely)
  (with-eval-after-load 'evil
    (define-key mu4e-compose-mode-map (kbd "<escape>") #'evil-force-normal-state)
    (define-key mu4e-compose-mode-map (kbd "C-[") #'evil-force-normal-state))

  (defvar-local hub/mu4e-compose-context nil
    "Name of the mu4e context this compose buffer belongs to.")

  (defun hub/mu4e--extract-email (header-value)
    "Return the email address from HEADER-VALUE, or nil if absent."
    (when (and header-value (string-match-p "\\S-" header-value))
      (cadr (mail-extract-address-components header-value))))

  (defun hub/mu4e--expected-context-for-compose ()
    "Determine the context that should back the current compose buffer."
    (or (and (boundp 'mu4e-compose-parent-message)
	     (hub/mu4e-context-for-message mu4e-compose-parent-message))
	(mu4e-context-current)))

  (defun hub/mu4e--apply-compose-context ()
    "Ensure the compose buffer has the correct mu4e context and From header."
    (when (derived-mode-p 'mu4e-compose-mode)
      (when-let* ((context (hub/mu4e--expected-context-for-compose))
		  (name (mu4e-context-name context)))
	(setq-local hub/mu4e-compose-context name)
	(mu4e-compose-context-switch t name))))

  (defun hub/mu4e--validate-compose-from ()
    "Prevent sending messages with a From outside the active context."
    (when (derived-mode-p 'mu4e-compose-mode)
      (hub/mu4e--apply-compose-context)
      (let* ((from (hub/mu4e--extract-email (message-field-value "From")))
	     (context (mu4e-context-current))
	     (expected (and context (hub/mu4e-context-var context 'user-mail-address)))
	     (from-context (and from (hub/mu4e-context-for-address from))))
	(cond
	 ((not from) (user-error "Compose buffer has no From header"))
	 ((not expected)
	  (user-error "Active mu4e context has no user-mail-address configured"))
	 ((not from-context)
	  (user-error "From address %s is not associated with any mu4e context" from))
	 ((not (string= (downcase from) (downcase expected)))
	  (user-error "From address %s does not match active context %s" from hub/mu4e-compose-context))))))

  (add-hook 'mu4e-compose-mode-hook #'hub/mu4e--apply-compose-context)
  (add-hook 'message-send-hook #'hub/mu4e--validate-compose-from))

;; Sending via msmtp
(setq send-mail-function 'sendmail-send-it
      message-send-mail-function 'message-send-mail-with-sendmail
      sendmail-program "msmtp"
      mail-specify-envelope-from t
      message-sendmail-envelope-from 'header
      mail-envelope-from 'header)

(provide 'email/compose)
;;; compose.el ends here
