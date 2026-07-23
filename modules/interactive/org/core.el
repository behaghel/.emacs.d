;;; core.el --- Org mode: editing entrypoint -*- lexical-binding: t; -*-

;;; Commentary:
;; Core Org editing UX and orchestration for agenda, capture, Babel, export, and
;; package integrations.

;;; Code:

(require 'hub-prose)
(require 'hub-utils)
(require 'org/settings)
(require 'org/agenda)
(require 'org/authoring)
(require 'org/babel)
(require 'org/capture)
(require 'org/export)
(require 'org/context-panel)
(require 'org/comments)

(autoload 'org-copilot-accept-at-point "org-copilot-diff" nil t)
(autoload 'org-copilot-chat "org-copilot-chat" nil t)
(autoload 'org-copilot-chat-focus-next-comment "org-copilot-chat" nil t)
(autoload 'org-copilot-chat-focus-previous-comment "org-copilot-chat" nil t)
(autoload 'org-copilot-chat-full-document "org-copilot-chat" nil t)
(autoload 'org-copilot-chat-section "org-copilot-chat" nil t)
(autoload 'org-copilot-chat-undo-focused-comment-at-point "org-copilot-chat" nil t)
(autoload 'org-copilot-clear-session "org-copilot-session" nil t)
(autoload 'org-copilot-debug-open "org-copilot-debug" nil t)
(autoload 'org-copilot-dismiss-at-point "org-copilot-diff" nil t)
(autoload 'org-copilot-erase-session "org-copilot-session" nil t)
(autoload 'org-copilot-open-panels "org-copilot-context-panel" nil t)
(autoload 'org-copilot-review-dwim "org-copilot-llm" nil t)
(autoload 'org-copilot-toggle-suggestion-panel "org-copilot-suggestion" nil t)
(autoload 'org-copilot-view-diff-at-point "org-copilot-diff" nil t)

(defun hub/org-setup-wrapping ()
  "Use virtual autofill in Org buffers and avoid hard line breaks."
  (hub/prose-visual-fill-mode))

(defun hub/org-set-structure-template (key value)
  "Set Org structure template KEY to VALUE without duplicate entries."
  (setq org-structure-template-alist
	(cons (cons key value)
	      (assoc-delete-all key org-structure-template-alist))))

(defun hub/org-tab-dwim (arg)
  "Move through snippets or inline templates, otherwise run `org-cycle'.
Forward prefix ARG to `org-cycle' so prefixed Org cycling keeps its
native behavior."
  (interactive "P")
  (cond
   ((and (null arg)
	 (fboundp 'yas-active-snippets)
	 (yas-active-snippets)
	 (fboundp 'yas-next-field-or-maybe-expand))
    (yas-next-field-or-maybe-expand))
   ((and (null arg)
	 (fboundp 'hub/org-tempo-complete-quote)
	 (hub/org-tempo-complete-quote)))
   ((and (null arg)
	 (fboundp 'hub/org-tempo-complete-status)
	 (hub/org-tempo-complete-status)))
   ((and (null arg)
	 (fboundp 'hub/org-tempo-complete-footnote)
	 (hub/org-tempo-complete-footnote)))
   ((and (null arg)
	 (fboundp 'hub/org-tempo-complete-traditional-footnote)
	 (hub/org-tempo-complete-traditional-footnote)))
   (t (org-cycle arg))))

(defun hub/outline-focus-next-section ()
  "Show only the next outline section."
  (interactive)
  (outline-next-heading)
  (outline-show-entry)
  (outline-hide-other))

(defun hub/org-setup-editing ()
  "Configure Org editing behavior and keybindings."
  (define-key evil-normal-state-map (kbd ",oc") #'org-capture)
  (define-key evil-normal-state-map (kbd ",ol") #'org-store-link)
  (define-key evil-normal-state-map (kbd ",oa") #'org-agenda)
  (evil-collection-define-key 'normal 'org-mode-map
			      ",aa" #'org-copilot-chat
			      ",aA" #'org-copilot-accept-at-point
			      ",ac" #'org-copilot-clear-session
			      ",aC" #'org-copilot-erase-session
			      ",ad" #'org-copilot-view-diff-at-point
			      ",aD" #'org-copilot-debug-open
			      ",ag" #'org-copilot-chat-full-document
			      ",an" #'org-copilot-chat-focus-next-comment
			      ",ao" #'org-copilot-open-panels
			      ",ap" #'org-copilot-chat-focus-previous-comment
			      ",ar" #'org-copilot-review-dwim
			      ",as" #'org-copilot-chat-section
			      ",au" #'org-copilot-chat-undo-focused-comment-at-point
			      ",av" #'org-copilot-toggle-suggestion-panel
			      ",ax" #'org-copilot-dismiss-at-point
			      ",or" #'org-babel-open-src-block-result
			      ",à"  #'org-archive-subtree-default
			      ",s"  #'outline-up-heading
			      ",t"  #'outline-down-heading
			      "à"   #'org-refile
			      (kbd ", SPC") #'hub/outline-focus-next-section
			      (kbd "<next>")  #'org-move-subtree-down
			      (kbd "<prior>") #'org-move-subtree-up
			      ",fn" #'org-footnote-new
			      ",cA" #'hub/org-comment-reanchor
			      ",cC" #'hub/org-comment-open-sidecar
			      ",ce" #'hub/org-comment-edit
			      ",cj" #'hub/org-comment-jump-to-sidecar
			      ",cl" #'org-confluence-comments-open-current
			      ",cc" #'hub/org-context-panel-toggle-open
			      ",cM" #'hub/org-context-panel-mode
			      ",cO" #'org-confluence-open-page
			      ",cf" #'hub/org-page-comment-create
			      ",cP" #'hub/org-page-comments-open
			      ",cr" #'hub/org-comment-reply-create
			      ",cmo" #'hub/org-comment-mark-open
			      ",cmt" #'hub/org-comment-mark-todo
			      ",cmr" #'hub/org-comment-mark-resolved
			      ",cx" #'hub/org-comment-delete
			      ",ov" #'hub/org-insert-veriff-template
			      "]c" #'org-context-panel-next-item
			      "[c" #'org-context-panel-previous-item)
  (evil-define-key 'visual org-mode-map (kbd ",cA") #'hub/org-comment-reanchor-from-region)
  (evil-define-key 'visual org-mode-map (kbd ",cc") #'hub/org-comment-create-from-region)
  (evil-define-key 'normal org-mode-map (kbd "RET") #'hub/org-comments-source-ret-dwim)
  (evil-define-key 'insert org-mode-map (kbd "RET") #'org-return)
  (evil-define-key 'motion calendar-mode-map (kbd "RET") #'org-calendar-select)
  (define-key org-mode-map (kbd "<tab>") #'hub/org-tab-dwim)
  (define-key org-mode-map (kbd "TAB") #'hub/org-tab-dwim)
  (evil-define-key 'insert org-mode-map (kbd "<tab>") #'hub/org-tab-dwim)
  (evil-define-key 'insert org-mode-map (kbd "TAB") #'hub/org-tab-dwim)
  ;; Do not bind M-RET through `evil-define-key' in insert state: in terminals
  ;; it may be represented as ESC RET, while ESC is deliberately a non-prefix
  ;; key for leaving insert state.
  (define-key org-mode-map [M-return] #'org-meta-return)
  (define-key org-mode-map (kbd "M-RET") #'org-meta-return)
  (evil-define-key 'insert org-mode-map (kbd "<escape>") #'evil-normal-state)
  (evil-define-key 'insert org-mode-map (kbd "C-[") #'evil-normal-state)
  (setq org-return-follows-link t
	org-hide-leading-stars t
	org-startup-indented t
	org-footnote-auto-adjust t
	org-cycle-separator-lines 0
	org-archive-location "archive/%s_archive::datetree/")
  (add-hook 'org-mode-hook #'hub/org-setup-wrapping)
  (add-hook 'org-mode-hook #'hub/org-epigraph-align-mode)
  (add-hook 'org-mode-hook #'org-comments-mode))

(use-package org
  :straight (:depth full)
  :commands (org-capture org-agenda)
  :after (evil evil-collection)
  :mode ("\\.\\(org\\|orj\\|org_archive\\|txt\\)$" . org-mode)
  :config
  (hub/org-setup-editing)
  (hub/org-setup-authoring-templates)
  (hub/org-setup-agenda)
  (hub/org-setup-capture)
  (hub/org-setup-babel)
  (hub/org-setup-export))

(require 'org/integrations)

(provide 'org/core)
;;; core.el ends here
