;;; org-copilot-chat.el --- Bottom chat view for Org Copilot -*- lexical-binding: t; -*-

;; Author: Hubert Behaghel
;; Maintainer: Hubert Behaghel
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (org "9.6"))
;; Keywords: outlines, tools, convenience
;; URL: https://github.com/behaghel/org-copilot

;;; Commentary:
;; Source-buffer-scoped chat transcript and bottom view for Org Copilot.
;; The core chat skeleton stores messages in session state and can call an
;; optional adapter function for assistant responses.

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'org-context-panel)
(require 'subr-x)
(require 'org-copilot-diff)
(require 'org-copilot-model)
(require 'org-copilot-session)

(defcustom org-copilot-chat-buffer-name "*Org Copilot Chat*"
  "Buffer name used for Org Copilot bottom chat views."
  :type 'string
  :group 'org-copilot)

(defcustom org-copilot-chat-function nil
  "Function used to answer Org Copilot chat messages.
The function receives a chat request plist.  It may return nil, a string
assistant message, or a plist with `:message' and optional `:comments'.  Normal
`org-copilot-chat-send' does not install returned comments."
  :type '(choice (const :tag "No adapter" nil)
		 function)
  :group 'org-copilot)

(defcustom org-copilot-chat-advance-after-action nil
  "Whether accept and dismiss actions focus the next AI comment."
  :type 'boolean
  :group 'org-copilot)

(defcustom org-copilot-chat-window-height 18
  "Height of the Org Copilot chat bottom window."
  :type 'natnum
  :group 'org-copilot)

(defvar-local org-copilot-chat-source-buffer nil
  "Source buffer associated with the current Org Copilot chat buffer.")

(defvar-local org-copilot-chat--prompt-start nil
  "Marker for the start of the editable Org Copilot prompt field.")

(defface org-copilot-chat-metadata-face
  '((t :inherit shadow))
  "Face used for Org Copilot chat metadata."
  :group 'org-copilot)

(defface org-copilot-chat-you-face
  '((t :inherit font-lock-keyword-face :weight bold))
  "Face used for user labels in Org Copilot chat."
  :group 'org-copilot)

(defface org-copilot-chat-copilot-face
  '((t :inherit font-lock-function-name-face :weight bold))
  "Face used for Copilot labels in Org Copilot chat."
  :group 'org-copilot)

(defface org-copilot-chat-pending-face
  '((t :inherit shadow :slant italic))
  "Face used for pending Org Copilot chat responses."
  :group 'org-copilot)

(defface org-copilot-chat-prompt-face
  '((t :inherit minibuffer-prompt :weight bold))
  "Face used for the Org Copilot chat prompt label."
  :group 'org-copilot)

(defvar org-copilot-chat-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'org-copilot-chat-return-dwim)
    (define-key map (kbd "C-c C-c") #'org-copilot-chat-send-current-input)
    (define-key map (kbd "/") #'org-copilot-chat-slash-or-complete)
    (define-key map (kbd "C-c C-f") #'org-copilot-chat-goto-prompt)
    (define-key map (kbd "C-c C-a") #'org-copilot-chat-accept-focused-suggestion-at-point)
    (define-key map (kbd "C-c C-d") #'org-copilot-chat-dismiss-focused-comment-at-point)
    (define-key map (kbd "C-c C-n") #'org-copilot-chat-focus-next-comment)
    (define-key map (kbd "C-c C-p") #'org-copilot-chat-focus-previous-comment)
    (define-key map (kbd "C-c C-u") #'org-copilot-chat-undo-focused-comment-at-point)
    (define-key map (kbd "C-c C-g") #'org-copilot-chat-full-document)
    (define-key map (kbd "C-c C-x / g") #'org-copilot-chat-full-document)
    (define-key map (kbd "C-c C-x / s") #'org-copilot-chat-section)
    (define-key map (kbd "C-c C-x / o") #'org-copilot-open-panels)
    (define-key map (kbd "M-a") #'org-copilot-chat-accept-focused-suggestion-at-point)
    (define-key map (kbd "M-d") #'org-copilot-chat-dismiss-focused-comment-at-point)
    (define-key map (kbd "M-n") #'org-copilot-chat-focus-next-comment)
    (define-key map (kbd "M-p") #'org-copilot-chat-focus-previous-comment)
    (define-key map (kbd "M-u") #'org-copilot-chat-undo-focused-comment-at-point)
    (define-key map (kbd "M-g") #'org-copilot-chat-full-document)
    (define-key map (kbd "M-s") #'org-copilot-chat-section)
    (define-key map (kbd "M-<up>") #'org-copilot-chat-recall-last-prompt)
    map)
  "Keymap used in Org Copilot chat buffers.")

(defun org-copilot-chat--status-line ()
  "Return compact bottom status text for the current chat buffer."
  (let* ((source (and (buffer-live-p org-context-panel-source-buffer)
		      org-context-panel-source-buffer))
	 (comments (and source
			(with-current-buffer source
			  (cl-remove-if
			   (lambda (comment)
			     (eq (org-copilot-comment-status comment) 'dismissed))
			   (org-copilot-comments)))))
	 (total (length comments))
	 (scope-count (cl-count-if
		       (lambda (comment)
			 (eq (plist-get comment :type) 'scope))
		       comments))
	 (focus-id (and source
			(with-current-buffer source
			  org-copilot-chat-focus-comment-id))))
    (string-join
     (delq nil
	   (list (format "%d comment%s" total (if (= total 1) "" "s"))
		 (when (> scope-count 0)
		   (format "%d scope" scope-count))
		 focus-id))
     " · ")))

(define-derived-mode org-copilot-chat-mode fundamental-mode "Org-Copilot-Chat"
  "Major mode for Org Copilot bottom chat buffers."
  (setq buffer-read-only nil)
  (setq-local mode-line-format '(" " (:eval (org-copilot-chat--status-line))))
  (when (fboundp 'evil-insert-state)
    (evil-insert-state)))

(defun org-copilot-chat--protect-window (window panel-buffer source-buffer)
  "Protect WINDOW for PANEL-BUFFER and SOURCE-BUFFER when supported."
  (when (and (window-live-p window)
	     (fboundp 'org-context-panel-protect-window))
    (org-context-panel-protect-window window panel-buffer source-buffer)))

(defun org-copilot-chat--source-buffer ()
  "Return the source buffer for Org Copilot chat commands."
  (cond
   ((buffer-live-p org-copilot-chat-source-buffer)
    org-copilot-chat-source-buffer)
   ((and (boundp 'org-copilot-suggestion-source-buffer)
	 (buffer-live-p org-copilot-suggestion-source-buffer))
    org-copilot-suggestion-source-buffer)
   ((buffer-live-p org-context-panel-source-buffer)
    org-context-panel-source-buffer)
   ((derived-mode-p 'org-mode)
    (current-buffer))
   (t
    (org-context-panel-current-source-buffer))))

(defun org-copilot-chat--comment-at-point ()
  "Return an AI comment at point, or nil."
  (ignore-errors
    (org-context-panel-item-at-point)))

(defun org-copilot-chat--context-id (context)
  "Return stable identifier for chat CONTEXT."
  (pcase (plist-get context :type)
    ('comment (format "comment:%s" (plist-get context :comment-id)))
    ('section (format "section:%s"
		      (or (plist-get context :section-path)
			  (plist-get context :heading-line)
			  (plist-get context :section-title))))
    (_ nil)))

(defun org-copilot-chat--set-context (source-buffer context)
  "Set SOURCE-BUFFER chat CONTEXT and compatibility focus fields."
  (with-current-buffer source-buffer
    (setq org-copilot-chat-context context)
    (setq org-copilot-chat-focus-comment-id
	  (and (eq (plist-get context :type) 'comment)
	       (plist-get context :comment-id)))))

(defun org-copilot-chat--section-context-at-point ()
  "Return section context plist for the current Org subtree."
  (unless (derived-mode-p 'org-mode)
    (user-error "Section context needs an Org source buffer"))
  (save-excursion
    (org-back-to-heading t)
    (let* ((heading-start (point-marker))
	   (heading-line (buffer-substring-no-properties
			  (line-beginning-position) (line-end-position)))
	   (section-title (org-get-heading t t t t))
	   (section-path (org-get-outline-path t t))
	   (body-start (save-excursion
			 (forward-line 1)
			 (point-marker)))
	   (end (save-excursion
		  (org-end-of-subtree t t)
		  (point-marker))))
      (list :type 'section
	    :heading-line heading-line
	    :section-title section-title
	    :section-path section-path
	    :heading-marker heading-start
	    :body-start-marker body-start
	    :end-marker end))))

(defun org-copilot-chat--focus-comment (source-buffer comment)
  "Focus SOURCE-BUFFER chat on COMMENT when COMMENT is non-nil."
  (when comment
    (org-copilot-chat--set-context
     source-buffer
     (list :type 'comment :comment-id (org-copilot-comment-id comment)))
    (when (fboundp 'org-copilot-chat--refresh-source-ui)
      (org-copilot-chat--refresh-source-ui source-buffer))))

(defun org-copilot-chat--focused-comment (source-buffer)
  "Return SOURCE-BUFFER's focused AI comment, or nil."
  (with-current-buffer source-buffer
    (and org-copilot-chat-focus-comment-id
	 (org-copilot-find-comment org-copilot-chat-focus-comment-id))))

(defun org-copilot-chat--insert-metadata (text)
  "Insert metadata TEXT using the Org Copilot metadata face."
  (insert (propertize text 'face 'org-copilot-chat-metadata-face)))

(defun org-copilot-chat--comment-marker (comment)
  "Return compact context marker for COMMENT."
  (pcase (org-copilot-comment-status comment)
    ('accepted "✅")
    ('stale "⚠️")
    (_ (if (plist-get comment :suggestion) "✏️" "💬"))))

(defun org-copilot-chat--context-marker (source-buffer)
  "Return chat context marker for SOURCE-BUFFER."
  (with-current-buffer source-buffer
    (pcase (plist-get org-copilot-chat-context :type)
      ('comment (if-let* ((comment (org-copilot-chat--focused-comment source-buffer)))
		    (org-copilot-chat--comment-marker comment)
		  "💬"))
      ('section "§")
      (_ "🌐"))))

(defun org-copilot-chat--context-label (source-buffer)
  "Return chat context label for SOURCE-BUFFER."
  (with-current-buffer source-buffer
    (pcase (plist-get org-copilot-chat-context :type)
      ('comment (if-let* ((comment (org-copilot-chat--focused-comment source-buffer)))
		    (format "%s Comment %s · %s"
			    (org-copilot-chat--comment-marker comment)
			    (org-copilot-comment-id comment)
			    (org-copilot-comment-status comment))
		  "💬 Comment"))
      ('section (format "§ Section: %s"
			(or (plist-get org-copilot-chat-context :section-title)
			    (plist-get org-copilot-chat-context :heading-line)
			    "current")))
      (_ "🌐 Full document"))))

(defun org-copilot-chat--focused-comment-body-message (source-buffer)
  "Return focused comment body as a Copilot message for SOURCE-BUFFER."
  (when-let* ((comment (org-copilot-chat--focused-comment source-buffer))
	      (body (plist-get comment :body)))
    (list :role 'assistant
	  :content body
	  :comment-id (org-copilot-comment-id comment))))

(defun org-copilot-chat--role-label (role)
  "Return display label for chat ROLE."
  (pcase role
    ('assistant "Copilot")
    ('pending "Copilot")
    ('user "You")
    (_ (capitalize (symbol-name role)))))

(defun org-copilot-chat--role-face (role)
  "Return display face for chat ROLE labels."
  (pcase role
    ('user 'org-copilot-chat-you-face)
    ('pending 'org-copilot-chat-copilot-face)
    ('assistant 'org-copilot-chat-copilot-face)
    (_ 'bold)))

(defun org-copilot-chat--message-face (role)
  "Return display face for chat ROLE content."
  (pcase role
    ('pending 'org-copilot-chat-pending-face)
    (_ nil)))

(defun org-copilot-chat--insert-indented-content (content &optional face)
  "Insert CONTENT indented as a chat block, optionally using FACE."
  (dolist (line (split-string content "\n"))
    (insert "  ")
    (if face
	(insert (propertize line 'face face))
      (insert line))
    (insert "\n")))

(defun org-copilot-chat--insert-message (message)
  "Insert one chat MESSAGE plist into the current buffer."
  (let* ((role (plist-get message :role))
	 (label (org-copilot-chat--role-label role))
	 (content (or (plist-get message :content) ""))
	 (start (point)))
    (insert (propertize label 'face (org-copilot-chat--role-face role)))
    (insert "\n")
    (org-copilot-chat--insert-indented-content
     content
     (org-copilot-chat--message-face role))
    (insert "\n")
    (add-text-properties start (point)
			 '(org-copilot-chat-message t))
    (put-text-property start (point) 'org-copilot-chat-message-role role)))

(defun org-copilot-chat--message-content-position (position)
  "Return the first content line position for message at POSITION."
  (save-excursion
    (goto-char position)
    (forward-line 1)
    (point)))

(defun org-copilot-chat--last-message-position (role &optional content)
  "Return the last rendered chat message position for ROLE.
When CONTENT is non-nil, return the first content line instead of the role
label line."
  (let ((position nil)
	(cursor (point-min)))
    (while (< cursor (point-max))
      (when (eq (get-text-property cursor 'org-copilot-chat-message-role) role)
	(setq position cursor))
      (setq cursor (or (next-single-property-change
			cursor 'org-copilot-chat-message-role nil (point-max))
		       (point-max))))
    (if (and content position)
	(org-copilot-chat--message-content-position position)
      position)))

(defun org-copilot-chat-scroll-to-last-message (source-buffer role)
  "Scroll visible SOURCE-BUFFER chat windows to the last message with ROLE.
The first content line is placed at the top of the viewport so completed
assistant responses start where the user begins reading."
  (when-let* ((buffer (get-buffer org-copilot-chat-buffer-name)))
    (with-current-buffer buffer
      (when (eq org-copilot-chat-source-buffer source-buffer)
	(when-let* ((position (org-copilot-chat--last-message-position role t)))
	  (dolist (window (get-buffer-window-list buffer nil t))
	    (set-window-point window position)
	    (set-window-start window position)))))))

(defun org-copilot-chat--visible-messages (source-buffer)
  "Return chat messages visible for SOURCE-BUFFER's current chat context."
  (with-current-buffer source-buffer
    (let* ((context org-copilot-chat-context)
	   (context-id (org-copilot-chat--context-id context))
	   (focus-id org-copilot-chat-focus-comment-id))
      (cl-remove-if-not
       (lambda (message)
	 (pcase (plist-get context :type)
	   ('comment (equal (plist-get message :comment-id) focus-id))
	   ('section (equal (plist-get message :context-id) context-id))
	   (_ (and (null (plist-get message :comment-id))
		   (null (plist-get message :context-id))))))
       (org-copilot-chat-messages)))))

(defun org-copilot-chat-render (source-buffer)
  "Render the Org Copilot chat transcript for SOURCE-BUFFER."
  (let* ((inhibit-read-only t)
	 (messages (org-copilot-chat--visible-messages source-buffer))
	 (body-message (org-copilot-chat--focused-comment-body-message source-buffer)))
    (erase-buffer)
    (let ((transcript-start (point)))
      (org-copilot-chat--insert-metadata
       (format "Org Copilot Chat — %s · %s\n\n"
	       (buffer-name source-buffer)
	       (org-copilot-chat--context-label source-buffer)))
      (cond
       ((or body-message messages)
	(when body-message
	  (org-copilot-chat--insert-message body-message))
	(dolist (message messages)
	  (org-copilot-chat--insert-message message)))
       (t
	(org-copilot-chat--insert-metadata
	 (if (org-copilot-chat--focused-comment source-buffer)
	     "Ask Copilot to revise, explain, or improve this suggestion.\n"
	   "Ask Copilot about the full document.\n"))))
      (add-text-properties transcript-start (point)
			   '(read-only t rear-nonsticky (read-only))))
    (insert (propertize "\n────────────────────────────────\n"
			'face 'org-copilot-chat-metadata-face
			'read-only t
			'rear-nonsticky '(read-only field)))
    (insert (propertize (format "%s You: "
				(org-copilot-chat--context-marker source-buffer))
			'face 'org-copilot-chat-prompt-face
			'field 'org-copilot-chat-prompt
			'read-only t
			'rear-nonsticky '(read-only field)))
    (setq org-copilot-chat--prompt-start (copy-marker (point) nil))
    (add-text-properties org-copilot-chat--prompt-start (point-max)
			 '(field org-copilot-chat-input
				 read-only nil
				 front-sticky nil
				 rear-nonsticky t))
    (setq buffer-read-only nil)
    (goto-char org-copilot-chat--prompt-start)))

(defun org-copilot-chat--focused-suggestion-comment (source-buffer)
  "Return focused comment with a suggestion for SOURCE-BUFFER, or nil."
  (when-let* ((comment (org-copilot-chat--focused-comment source-buffer)))
    (and (plist-get comment :suggestion) comment)))

(defun org-copilot-chat--navigable-comments (source-buffer)
  "Return non-dismissed AI comments for SOURCE-BUFFER."
  (with-current-buffer source-buffer
    (cl-remove-if
     (lambda (comment)
       (eq (org-copilot-comment-status comment) 'dismissed))
     (org-copilot-comments))))

(defun org-copilot-chat--focus-comment-by-step (source-buffer step)
  "Focus SOURCE-BUFFER chat on the next comment by STEP."
  (let* ((comments (org-copilot-chat--navigable-comments source-buffer))
	 (count (length comments)))
    (unless comments
      (user-error "No AI comments to focus"))
    (with-current-buffer source-buffer
      (let* ((current org-copilot-chat-focus-comment-id)
	     (index (cl-position current comments
				 :key #'org-copilot-comment-id
				 :test #'equal))
	     (next-index (if index
			     (mod (+ index step) count)
			   (if (> step 0) 0 (1- count))))
	     (comment (nth next-index comments)))
	(org-copilot-chat--set-context
	 source-buffer
	 (list :type 'comment :comment-id (org-copilot-comment-id comment)))
	comment))))

(defun org-copilot-chat-render-bottom-view (source-buffer _view)
  "Render the Org Copilot bottom chat view for SOURCE-BUFFER."
  (setq org-copilot-chat-source-buffer source-buffer)
  (setq org-context-panel-source-buffer source-buffer)
  (setq org-context-panel-view-id 'copilot-chat)
  (org-copilot-chat-render source-buffer))

(defun org-copilot-chat-bottom-views (_source-buffer)
  "Return Org Copilot bottom view descriptors."
  (list (list :id 'copilot-chat
	      :buffer-name org-copilot-chat-buffer-name
	      :mode 'org-copilot-chat-mode
	      :height org-copilot-chat-window-height
	      :render #'org-copilot-chat-render-bottom-view)))

(defun org-copilot-chat--open-bottom-view (source-buffer)
  "Open the Org Copilot bottom chat view for SOURCE-BUFFER."
  (with-current-buffer source-buffer
    (org-copilot-mode 1))
  (let* ((buffer (org-context-panel-open-bottom-view 'copilot-chat source-buffer))
	 (window (get-buffer-window buffer t)))
    (org-copilot-chat-sync-diff source-buffer)
    (when (window-live-p window)
      (select-window window)
      (with-current-buffer buffer
	(org-copilot-chat-goto-prompt))
      (set-window-point window
			(with-current-buffer buffer
			  org-copilot-chat--prompt-start)))
    buffer))

;;;###autoload
(defun org-copilot-chat-full-document ()
  "Open Org Copilot chat in full-document context.
When invoked from an existing chat, clear the focused AI comment and keep the
prompt active.  Source target overlays remain visible as dim context markers."
  (interactive)
  (let ((source (org-copilot-chat--source-buffer)))
    (org-copilot-chat--set-context source '(:type full-document))
    (org-copilot-chat--refresh-source-ui source)
    (org-copilot-chat--open-bottom-view source)))

;;;###autoload
(defun org-copilot-chat-section ()
  "Open Org Copilot chat in current Org section context."
  (interactive)
  (let* ((source (org-copilot-chat--source-buffer))
	 (context (with-current-buffer source
		    (org-copilot-chat--section-context-at-point))))
    (org-copilot-chat--set-context source context)
    (org-copilot-chat--refresh-source-ui source)
    (org-copilot-chat--open-bottom-view source)))

(defun org-copilot-chat--refresh-source-ui (source-buffer)
  "Refresh source overlays and visible side panel for SOURCE-BUFFER."
  (with-current-buffer source-buffer
    (when (fboundp 'org-copilot-refresh-overlays)
      (org-copilot-refresh-overlays))
    (when (and (boundp 'org-copilot-panel-buffer-name)
	       (get-buffer org-copilot-panel-buffer-name))
      (org-context-panel-refresh))))

(defun org-copilot-chat--focus-and-refresh (source-buffer comment)
  "Refresh chat and diff after focusing COMMENT in SOURCE-BUFFER."
  (org-copilot-chat--set-context
   source-buffer
   (list :type 'comment :comment-id (org-copilot-comment-id comment)))
  (org-copilot-chat--refresh-source-ui source-buffer)
  (let ((buffer (org-copilot-chat--buffer source-buffer)))
    (when-let* ((window (get-buffer-window buffer t)))
      (with-current-buffer buffer
	(org-copilot-chat-goto-prompt))
      (set-window-point window
			(with-current-buffer buffer
			  org-copilot-chat--prompt-start)))
    buffer))

;;;###autoload
(defun org-copilot-chat-focus-next-comment ()
  "Focus chat on the next non-dismissed AI comment."
  (interactive)
  (let* ((source (org-copilot-chat--source-buffer))
	 (comment (org-copilot-chat--focus-comment-by-step source 1)))
    (org-copilot-chat--focus-and-refresh source comment)))

;;;###autoload
(defun org-copilot-chat-focus-previous-comment ()
  "Focus chat on the previous non-dismissed AI comment."
  (interactive)
  (let* ((source (org-copilot-chat--source-buffer))
	 (comment (org-copilot-chat--focus-comment-by-step source -1)))
    (org-copilot-chat--focus-and-refresh source comment)))

(defun org-copilot-chat--close-stale-diff ()
  "Close the Org Copilot diff buffer when it exists."
  (when-let* ((buffer (get-buffer org-copilot-diff-buffer-name)))
    (when-let* ((window (get-buffer-window buffer t)))
      (unless (one-window-p t)
	(delete-window window)))
    (when (buffer-live-p buffer)
      (kill-buffer buffer))))

(defun org-copilot-chat-sync-diff (source-buffer)
  "Open or close suggestion diff to match SOURCE-BUFFER's chat focus."
  (if-let* ((comment (org-copilot-chat--focused-suggestion-comment source-buffer)))
      (let* ((buffer (org-copilot-diff-open source-buffer comment))
	     (window (display-buffer-in-side-window
		      buffer
		      '((side . bottom)
			(slot . 0)
			(window-height . 12)
			(window-parameters
			 . ((no-other-window . t)
			    (no-delete-other-windows . t)))))))
	(org-copilot-chat--protect-window window buffer source-buffer))
    (org-copilot-chat--close-stale-diff)))

(defun org-copilot-chat-goto-prompt ()
  "Move point to the Org Copilot chat prompt."
  (interactive)
  (unless (markerp org-copilot-chat--prompt-start)
    (user-error "No Org Copilot prompt field"))
  (goto-char org-copilot-chat--prompt-start))

(defun org-copilot-chat--buffer (source-buffer)
  "Return the chat buffer for SOURCE-BUFFER, creating it when needed."
  (let ((buffer (get-buffer-create org-copilot-chat-buffer-name)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'org-copilot-chat-mode)
	(org-copilot-chat-mode))
      (setq org-copilot-chat-source-buffer source-buffer)
      (setq org-context-panel-source-buffer source-buffer)
      (setq org-context-panel-view-id 'copilot-chat)
      (org-copilot-chat-render source-buffer))
    (org-copilot-chat-sync-diff source-buffer)
    (when-let* ((window (get-buffer-window buffer t)))
      (set-window-point window
			(with-current-buffer buffer
			  org-copilot-chat--prompt-start)))
    buffer))

;;;###autoload
(defun org-copilot-chat ()
  "Open the Org Copilot bottom chat for the current source buffer."
  (interactive)
  (let* ((source (org-copilot-chat--source-buffer))
	 (comment (org-copilot-chat--comment-at-point)))
    (org-copilot-chat--focus-comment source comment)
    (org-copilot-chat--open-bottom-view source)))

(defun org-copilot-chat--adapter-message (response)
  "Return assistant message content from adapter RESPONSE, or nil."
  (cond
   ((stringp response) response)
   ((listp response) (plist-get response :message))))

(defun org-copilot-chat--section-source-content ()
  "Return source content for the current section context."
  (let ((start (plist-get org-copilot-chat-context :heading-marker))
	(end (plist-get org-copilot-chat-context :end-marker)))
    (when (and (markerp start) (markerp end))
      (buffer-substring-no-properties start end))))

(defun org-copilot-chat--context-source-content ()
  "Return source content relevant to the current chat context."
  (pcase (plist-get org-copilot-chat-context :type)
    ('comment nil)
    (_ (buffer-substring-no-properties (point-min) (point-max)))))

(defun org-copilot-chat--request (source-buffer message)
  "Return chat request for SOURCE-BUFFER and user MESSAGE."
  (with-current-buffer source-buffer
    (list :source-buffer source-buffer
	  :buffer-name (buffer-name source-buffer)
	  :message message
	  :messages (org-copilot-chat--visible-messages source-buffer)
	  :chat-context org-copilot-chat-context
	  :context-id (org-copilot-chat--context-id org-copilot-chat-context)
	  :focus-comment-id org-copilot-chat-focus-comment-id
	  :source-content (org-copilot-chat--context-source-content)
	  :section-content (when (eq (plist-get org-copilot-chat-context
						:type)
				     'section)
			     (org-copilot-chat--section-source-content))
	  :allow-comment-updates nil)))

(defun org-copilot-chat--current-focused-comment-or-error (source-buffer action)
  "Return focused comment for SOURCE-BUFFER or signal ACTION user error."
  (with-current-buffer source-buffer
    (unless org-copilot-chat-focus-comment-id
      (user-error "No focused AI comment to %s" action))
    (or (org-copilot-find-comment org-copilot-chat-focus-comment-id)
	(user-error "No focused AI comment to %s" action))))

(defun org-copilot-chat--after-focused-action (source-buffer)
  "Refresh chat after an action in SOURCE-BUFFER."
  (org-copilot-chat--refresh-source-ui source-buffer)
  (org-copilot-chat--close-stale-diff)
  (if org-copilot-chat-advance-after-action
      (org-copilot-chat-focus-next-comment)
    (org-copilot-chat--buffer source-buffer)))

(defun org-copilot-chat-accept-focused-suggestion (source-buffer)
  "Accept SOURCE-BUFFER's focused comment suggestion."
  (let ((comment (org-copilot-chat--current-focused-comment-or-error
		  source-buffer "accept")))
    (org-copilot-accept-comment comment source-buffer))
  (org-copilot-chat--after-focused-action source-buffer))

(defun org-copilot-chat-undo-focused-comment (source-buffer)
  "Undo SOURCE-BUFFER's focused accepted AI comment."
  (let ((comment (org-copilot-chat--current-focused-comment-or-error
		  source-buffer "undo")))
    (org-copilot-undo-accepted-comment comment source-buffer))
  (org-copilot-chat--refresh-source-ui source-buffer)
  (org-copilot-chat--buffer source-buffer))

;;;###autoload
(defun org-copilot-chat-accept-focused-suggestion-at-point ()
  "Accept the suggestion for the current Org Copilot chat focus."
  (interactive)
  (let ((source (org-copilot-chat--source-buffer)))
    (org-copilot-chat-accept-focused-suggestion source)))

;;;###autoload
(defun org-copilot-chat-undo-focused-comment-at-point ()
  "Undo the accepted suggestion for the current Org Copilot chat focus."
  (interactive)
  (let ((source (org-copilot-chat--source-buffer)))
    (org-copilot-chat-undo-focused-comment source)))

(defun org-copilot-chat-dismiss-focused-comment (source-buffer)
  "Dismiss SOURCE-BUFFER's focused AI comment."
  (let ((comment (org-copilot-chat--current-focused-comment-or-error
		  source-buffer "dismiss")))
    (org-copilot-dismiss-comment comment source-buffer))
  (unless org-copilot-chat-advance-after-action
    (org-copilot-chat--set-context source-buffer '(:type full-document)))
  (org-copilot-chat--after-focused-action source-buffer))

(defun org-copilot-chat--doctor-value (value)
  "Return VALUE formatted for the Org Copilot doctor report."
  (cond
   ((null value) "no")
   ((eq value t) "yes")
   ((symbolp value) (symbol-name value))
   ((functionp value) (format "%S" value))
   ((bufferp value) (buffer-name value))
   (t (format "%S" value))))

(defun org-copilot-chat--doctor-check (label ok &optional detail)
  "Return a formatted doctor check line for LABEL, OK, and DETAIL."
  (format "- %s: %s%s"
	  label
	  (if ok "OK" "WARN")
	  (if detail (format " (%s)" detail) "")))

(defun org-copilot-chat--doctor-report (source-buffer)
  "Return a deep Org Copilot health report for SOURCE-BUFFER."
  (let* ((chat-buffer (get-buffer org-copilot-chat-buffer-name))
	 (chat-window (and chat-buffer (get-buffer-window chat-buffer t)))
	 (source-live (buffer-live-p source-buffer))
	 (source-mode (and source-live
			   (with-current-buffer source-buffer major-mode)))
	 (comments (and source-live
			(with-current-buffer source-buffer
			  (org-copilot-comments))))
	 (messages (and source-live
			(with-current-buffer source-buffer
			  (org-copilot-chat-messages))))
	 (pending-count (cl-count-if
			 (lambda (message)
			   (eq (plist-get message :role) 'pending))
			 messages))
	 (chat-function org-copilot-chat-function)
	 (review-function (and (boundp 'org-copilot-review-function)
			       org-copilot-review-function))
	 (gptel-loaded (featurep 'gptel))
	 (gptel-request-present (fboundp 'gptel-request))
	 (gptel-adapter-loaded (featurep 'org-copilot-gptel))
	 (gptel-chat-enabled (eq chat-function 'org-copilot-gptel-chat))
	 (gptel-review-enabled (eq review-function 'org-copilot-gptel-review)))
    (string-join
     (list
      "Org Copilot Doctor"
      ""
      "Source"
      (org-copilot-chat--doctor-check
       "source buffer live" source-live (org-copilot-chat--doctor-value source-buffer))
      (org-copilot-chat--doctor-check
       "source buffer is org-mode" (and source-live (eq source-mode 'org-mode))
       (org-copilot-chat--doctor-value source-mode))
      (format "- source file: %s"
	      (or (and source-live
		       (with-current-buffer source-buffer buffer-file-name))
		  "<none>"))
      ""
      "Chat UI"
      (org-copilot-chat--doctor-check
       "chat buffer exists" chat-buffer (org-copilot-chat--doctor-value chat-buffer))
      (org-copilot-chat--doctor-check
       "chat window visible" chat-window (org-copilot-chat--doctor-value chat-window))
      (org-copilot-chat--doctor-check
       "prompt marker valid"
       (and chat-buffer
	    (with-current-buffer chat-buffer
	      (markerp org-copilot-chat--prompt-start))))
      ""
      "Session"
      (format "- comments: %d" (length comments))
      (format "- chat messages: %d" (length messages))
      (format "- pending assistant messages: %d" pending-count)
      (format "- focused comment id: %s"
	      (or (and source-live
		       (with-current-buffer source-buffer
			 org-copilot-chat-focus-comment-id))
		  "<none>"))
      ""
      "Adapters"
      (org-copilot-chat--doctor-check
       "chat adapter configured" chat-function
       (org-copilot-chat--doctor-value chat-function))
      (org-copilot-chat--doctor-check
       "review adapter configured" review-function
       (org-copilot-chat--doctor-value review-function))
      (org-copilot-chat--doctor-check "gptel feature loaded" gptel-loaded)
      (org-copilot-chat--doctor-check "gptel-request available" gptel-request-present)
      (org-copilot-chat--doctor-check "org-copilot-gptel loaded" gptel-adapter-loaded)
      (org-copilot-chat--doctor-check "gptel chat adapter enabled" gptel-chat-enabled)
      (org-copilot-chat--doctor-check "gptel review adapter enabled" gptel-review-enabled)
      (format "- gptel model: %s"
	      (if (boundp 'gptel-model)
		  (org-copilot-chat--doctor-value gptel-model)
		"<unbound>"))
      (format "- gptel backend: %s"
	      (if (boundp 'gptel-backend)
		  (org-copilot-chat--doctor-value gptel-backend)
		"<unbound>"))
      (format "- gptel api key configured: %s"
	      (if (and (boundp 'gptel-api-key) gptel-api-key) "yes" "no"))
      ""
      "Likely fixes"
      (cond
       ((not chat-function)
	"- No chat adapter is configured. Load/enable an adapter, e.g. `org-copilot-gptel-enable'.")
       ((and gptel-chat-enabled (not gptel-request-present))
	"- gptel adapter is selected, but `gptel-request' is unavailable; load gptel.")
       ((> pending-count 0)
	"- A request is pending. Check *Messages* for gptel callback errors or network/auth failures.")
       (t
	"- No obvious local configuration problem detected. If requests still hang, inspect *Messages* and gptel backend logs.")))
     "\n")))

(defun org-copilot-chat-doctor (source-buffer)
  "Append an Org Copilot health report for SOURCE-BUFFER to the chat."
  (let ((report (org-copilot-chat--doctor-report source-buffer))
	(context-id (with-current-buffer source-buffer
		      (org-copilot-chat--context-id org-copilot-chat-context)))
	(comment-id (with-current-buffer source-buffer
		      org-copilot-chat-focus-comment-id)))
    (with-current-buffer source-buffer
      (org-copilot-add-chat-message 'assistant report comment-id context-id))
    (org-copilot-chat--buffer source-buffer)
    (org-copilot-chat-scroll-to-last-message source-buffer 'assistant)
    report))

;;;###autoload
(defun org-copilot-chat-dismiss-focused-comment-at-point ()
  "Dismiss the current Org Copilot chat focus."
  (interactive)
  (let ((source (org-copilot-chat--source-buffer)))
    (org-copilot-chat-dismiss-focused-comment source)))

(defconst org-copilot-chat-slash-commands
  '(("/accept" . "Accept focused suggestion")
    ("/dismiss" . "Dismiss focused comment")
    ("/doctor" . "Run Org Copilot health check")
    ("/next" . "Focus next comment")
    ("/prev" . "Focus previous comment")
    ("/undo" . "Undo accepted suggestion"))
  "Implemented Org Copilot chat slash commands and annotations.")

(defun org-copilot-chat--comment-accepted-with-rollback-p (comment)
  "Return non-nil when COMMENT can be undone."
  (and (eq (org-copilot-comment-status comment) 'accepted)
       (plist-get comment :original-target-text)
       (or (plist-get comment :accepted-text)
	   (plist-get comment :target-text))))

(defun org-copilot-chat--command-available-p (command source-buffer)
  "Return non-nil when slash COMMAND is available for SOURCE-BUFFER."
  (let ((comment (org-copilot-chat--focused-comment source-buffer)))
    (pcase command
      ("/doctor" t)
      ("/next" (org-copilot-chat--navigable-comments source-buffer))
      ("/prev" (org-copilot-chat--navigable-comments source-buffer))
      ("/accept" (and comment
		      (not (memq (org-copilot-comment-status comment)
				 '(accepted stale dismissed)))
		      (plist-get comment :suggestion)))
      ("/dismiss" (and comment
		       (not (eq (org-copilot-comment-status comment)
				'dismissed))))
      ("/undo" (and comment
		    (org-copilot-chat--comment-accepted-with-rollback-p
		     comment)))
      (_ nil))))

(defun org-copilot-chat--available-slash-commands (source-buffer)
  "Return slash commands available for SOURCE-BUFFER."
  (cl-remove-if-not
   (lambda (entry)
     (org-copilot-chat--command-available-p (car entry) source-buffer))
   org-copilot-chat-slash-commands))

(defun org-copilot-chat--command-p (message command)
  "Return non-nil when MESSAGE is slash COMMAND."
  (string-equal (string-trim message) command))

(defun org-copilot-chat--slash-command-annotation (command)
  "Return completion annotation for slash COMMAND."
  (when-let* ((description (cdr (assoc command org-copilot-chat-slash-commands))))
    (concat "  " description)))

(defun org-copilot-chat--read-slash-command (source-buffer)
  "Read an Org Copilot slash command available for SOURCE-BUFFER."
  (let ((commands (org-copilot-chat--available-slash-commands source-buffer)))
    (unless commands
      (user-error "No Org Copilot commands available"))
    (let ((completion-extra-properties
	   '(:annotation-function org-copilot-chat--slash-command-annotation)))
      (completing-read "Org Copilot command: "
		       (mapcar #'car commands)
		       nil t "/"))))

(defun org-copilot-chat--dispatch-command (message source-buffer)
  "Dispatch slash MESSAGE for SOURCE-BUFFER, or return nil."
  (pcase (string-trim message)
    ("/accept"
     (org-copilot-chat-accept-focused-suggestion source-buffer)
     t)
    ("/doctor"
     (org-copilot-chat-doctor source-buffer)
     t)
    ("/dismiss"
     (org-copilot-chat-dismiss-focused-comment source-buffer)
     t)
    ("/undo"
     (org-copilot-chat-undo-focused-comment source-buffer)
     t)
    ("/next"
     (with-current-buffer source-buffer
       (org-copilot-chat-focus-next-comment))
     t)
    ("/prev"
     (with-current-buffer source-buffer
       (org-copilot-chat-focus-previous-comment))
     t)
    (_ nil)))

(defun org-copilot-chat--prompt-empty-p ()
  "Return non-nil when the current prompt field is empty."
  (string-empty-p (org-copilot-chat--prompt-text)))

;;;###autoload
(defun org-copilot-chat-slash-or-complete ()
  "Complete and run slash command, or insert a literal slash.
Slash command completion is offered only when the prompt field is empty."
  (interactive)
  (if (org-copilot-chat--prompt-empty-p)
      (org-copilot-chat-send
       (org-copilot-chat--read-slash-command
	(org-copilot-chat--source-buffer)))
    (insert "/")))

(defun org-copilot-chat--prompt-text ()
  "Return current editable prompt text."
  (unless (markerp org-copilot-chat--prompt-start)
    (user-error "No Org Copilot prompt field"))
  (string-trim
   (buffer-substring-no-properties org-copilot-chat--prompt-start (point-max))))

(defun org-copilot-chat--set-prompt-text (text)
  "Replace the current editable prompt with TEXT."
  (unless (markerp org-copilot-chat--prompt-start)
    (user-error "No Org Copilot prompt field"))
  (let ((inhibit-read-only t))
    (delete-region org-copilot-chat--prompt-start (point-max))
    (goto-char org-copilot-chat--prompt-start)
    (insert text)))

(defun org-copilot-chat--last-user-message (source-buffer)
  "Return last visible user message content for SOURCE-BUFFER."
  (cl-loop for message in (reverse (org-copilot-chat--visible-messages source-buffer))
	   when (eq (plist-get message :role) 'user)
	   return (plist-get message :content)))

;;;###autoload
(defun org-copilot-chat-recall-last-prompt ()
  "Recall the last submitted prompt in the current chat context."
  (interactive)
  (let* ((source (org-copilot-chat--source-buffer))
	 (message (org-copilot-chat--last-user-message source)))
    (unless message
      (user-error "No previous Org Copilot prompt in this context"))
    (org-copilot-chat--set-prompt-text message)))

;;;###autoload
(defun org-copilot-chat-send-current-input ()
  "Send the current Org Copilot prompt field."
  (interactive)
  (let ((message (org-copilot-chat--prompt-text)))
    (unless (string-empty-p message)
      (org-copilot-chat-send message))))

;;;###autoload
(defun org-copilot-chat-return-dwim ()
  "Send input from the prompt, or move to the prompt from transcript text."
  (interactive)
  (if (and (markerp org-copilot-chat--prompt-start)
	   (>= (point) org-copilot-chat--prompt-start))
      (org-copilot-chat-send-current-input)
    (org-copilot-chat-goto-prompt)))

;;;###autoload
(defun org-copilot-chat-send (message)
  "Send MESSAGE to the Org Copilot chat session.
Interactively, read MESSAGE from the minibuffer outside chat buffers.  When
`org-copilot-chat-function' returns an assistant response, append it to the
session transcript.  Slash command `/accept' accepts the focused suggestion."
  (interactive (list (if (derived-mode-p 'org-copilot-chat-mode)
			 (org-copilot-chat--prompt-text)
		       (read-string "Org Copilot chat: "))))
  (let* ((source (org-copilot-chat--source-buffer))
	 (comment-id (with-current-buffer source
		       org-copilot-chat-focus-comment-id))
	 (context-id (with-current-buffer source
		       (org-copilot-chat--context-id org-copilot-chat-context)))
	 response
	 scroll-role)
    (unless (org-copilot-chat--dispatch-command message source)
      (with-current-buffer source
	(org-copilot-add-chat-message 'user message comment-id context-id)
	(setq response
	      (when org-copilot-chat-function
		(funcall org-copilot-chat-function
			 (org-copilot-chat--request source message))))
	(if-let* ((assistant-message
		   (org-copilot-chat--adapter-message response)))
	    (progn
	      (org-copilot-add-chat-message
	       'assistant assistant-message comment-id context-id)
	      (setq scroll-role 'assistant))
	  (if org-copilot-chat-function
	      (progn
		(org-copilot-add-chat-message 'pending "…" comment-id context-id)
		(setq scroll-role 'pending))
	    (setq scroll-role 'user))))
      (org-copilot-chat--buffer source)
      (org-copilot-chat-scroll-to-last-message source scroll-role))))

(provide 'org-copilot-chat)
;;; org-copilot-chat.el ends here
