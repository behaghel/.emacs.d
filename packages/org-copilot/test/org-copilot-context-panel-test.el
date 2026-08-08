;;; org-copilot-context-panel-test.el --- Context panel tests for org-copilot -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for Org Copilot's context-panels provider and source overlays.

;;; Code:

(require 'ert)
(require 'org)
(require 'org-copilot)
(require 'org-copilot-context-panel)

(ert-deftest org-copilot-highlight-faces-keep-foreground-unspecified ()
  "Org Copilot highlight faces only force background colors."
  (dolist (face '(org-copilot-target-face org-copilot-target-dim-face))
    (should (eq (face-attribute face :foreground nil t) 'unspecified))
    (should (eq (face-attribute face :inherit nil t) 'unspecified))
    (should (stringp (face-attribute face :background nil t)))))

(ert-deftest org-copilot-context-panel-registers-provider ()
  "Enabling Org Copilot registers a context-panels provider."
  (with-temp-buffer
    (org-mode)
    (org-copilot-mode 1)
    (should (context-panels-registered-provider 'copilot))
    (org-copilot-mode -1)
    (should-not (context-panels-registered-provider 'copilot))))

(ert-deftest org-copilot-context-panel-provider-exposes-chat-bottom-view ()
  "Org Copilot chat is exposed as a context-panels bottom view with height."
  (with-temp-buffer
    (org-mode)
    (let ((org-copilot-chat-window-height 18))
      (org-copilot-mode 1)
      (let ((view (context-panels-bottom-view 'copilot-chat (current-buffer))))
	(should view)
	(should (eq (plist-get view :provider) 'copilot))
	(should (eq (plist-get view :mode) 'org-copilot-chat-mode))
	(should (= (plist-get view :height) 18))))))

(ert-deftest org-copilot-context-panel-provider-does-not-own-side-items ()
  "Copilot provider leaves persistent side comment rows to org-comments."
  (let ((provider (org-copilot-context-panel-provider)))
    (should (equal (plist-get provider :icon) "🤖"))
    (should-not (plist-get provider :collect-side-items))
    (should-not (plist-get provider :render-side-item))
    (should-not (plist-get provider :side-panel-buffer-name))))

(ert-deftest org-copilot-open-delegates-side-panel-to-org-comments ()
  "Opening Copilot opens unified comments instead of a Copilot side panel."
  (with-temp-buffer
    (org-mode)
    (let (opened)
      (cl-letf (((symbol-function 'org-comments-open)
		 (lambda () (setq opened t)))
		((symbol-function 'org-copilot-mode)
		 (lambda (&optional _arg) t)))
	(org-copilot-open)
	(should opened)))))

(ert-deftest org-copilot-refresh-overlays-creates-dim-target-overlay-by-default ()
  "Refreshing overlays dims anchored AI comment target ranges by default."
  (with-temp-buffer
    (org-mode)
    (insert "Alpha sentence.\n")
    (org-copilot-add-comment
     (list :id "ai-1"
	   :source-start (point-min)
	   :source-end (+ (point-min) (length "Alpha"))
	   :target-text "Alpha"
	   :status 'active))
    (org-copilot-refresh-overlays)
    (let ((overlays (overlays-at (point-min))))
      (should (= (length org-copilot--overlays) 1))
      (should (cl-some (lambda (overlay)
			 (eq (overlay-get overlay 'face) 'org-copilot-target-dim-face))
		       overlays)))))

(ert-deftest org-copilot-refresh-overlays-highlights-scope-title-only ()
  "Scope comments highlight only the section title line."
  (with-temp-buffer
    (org-mode)
    (insert "* Section\nBody line.\nMore body.\n")
    (org-copilot-add-comment
     (list :id "ai-1"
	   :type 'scope
	   :source-start (point-min)
	   :source-end (point-max)
	   :target-text (buffer-string)
	   :status 'active))
    (org-copilot-refresh-overlays)
    (let ((overlay (car org-copilot--overlays)))
      (should (= (overlay-start overlay) (point-min)))
      (should (= (overlay-end overlay)
		 (save-excursion
		   (goto-char (point-min))
		   (line-end-position)))))))

(ert-deftest org-copilot-refresh-overlays-highlights-only-focused-target ()
  "Only the focused AI comment target gets the focused face."
  (with-temp-buffer
    (org-mode)
    (insert "Alpha beta gamma\n")
    (org-copilot-add-comment
     (list :id "ai-1" :source-start 1 :source-end 6 :status 'active))
    (org-copilot-add-comment
     (list :id "ai-2" :source-start 7 :source-end 11 :status 'active))
    (setq org-copilot-chat-focus-comment-id "ai-2")
    (org-copilot-refresh-overlays)
    (should (= (length org-copilot--overlays) 2))
    (should (eq (overlay-get (nth 0 org-copilot--overlays) 'face)
		'org-copilot-target-dim-face))
    (should (eq (overlay-get (nth 1 org-copilot--overlays) 'face)
		'org-copilot-target-face))))

(ert-deftest org-copilot-refresh-overlays-skips-dismissed-comments ()
  "Dismissed AI comments do not create source overlays."
  (with-temp-buffer
    (org-mode)
    (insert "Alpha\n")
    (org-copilot-add-comment
     (list :id "ai-1" :source-start 1 :source-end 6 :status 'dismissed))
    (org-copilot-refresh-overlays)
    (should-not org-copilot--overlays)))

(ert-deftest org-copilot-clear-session-removes-overlays ()
  "Clearing a session removes source overlays."
  (with-temp-buffer
    (org-mode)
    (insert "Alpha\n")
    (org-copilot-add-comment
     (list :id "ai-1" :source-start 1 :source-end 6 :status 'active))
    (org-copilot-refresh-overlays)
    (should org-copilot--overlays)
    (org-copilot-clear-session t)
    (should-not org-copilot--overlays)))

(ert-deftest org-copilot-mode-defines-prefixed-source-keys ()
  "Source buffers expose Copilot commands through the prefixed keymap."
  (should (eq (lookup-key org-copilot-mode-map (kbd "C-c C-x / n"))
	      #'context-panels-next-item))
  (should (eq (lookup-key org-copilot-mode-map (kbd "C-c C-x / p"))
	      #'context-panels-previous-item))
  (should (eq (lookup-key org-copilot-mode-map (kbd "C-c C-x / c"))
	      #'org-copilot-chat))
  (should (eq (lookup-key org-copilot-mode-map (kbd "C-c C-x / o"))
	      #'org-copilot-open-panels)))

(ert-deftest org-copilot-close-from-chat-buries-bottom-buffer ()
  "Closing from a chat buffer buries that bottom buffer."
  (let ((source (generate-new-buffer " *org copilot close source*"))
	(chat (generate-new-buffer " *org copilot close chat*")))
    (unwind-protect
	(progn
	  (delete-other-windows)
	  (switch-to-buffer source)
	  (org-mode)
	  (split-window-below)
	  (other-window 1)
	  (switch-to-buffer chat)
	  (org-copilot-chat-mode)
	  (setq context-panels-source-buffer source)
	  (org-copilot-close)
	  (should-not (get-buffer-window chat t)))
      (when (buffer-live-p source) (kill-buffer source))
      (when (buffer-live-p chat) (kill-buffer chat))
      (delete-other-windows))))

(provide 'org-copilot-context-panel-test)
;;; org-copilot-context-panel-test.el ends here
