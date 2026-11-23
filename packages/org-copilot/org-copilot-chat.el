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
(require 'color)
(require 'face-remap)
(require 'org)
(require 'org-element)
(require 'context-panels)
(require 'context-panels-org)
(require 'button)
(require 'subr-x)
(require 'org-copilot-diff)
(require 'org-copilot-model)
(require 'org-copilot-session)

(declare-function org-copilot-mode "org-copilot" (&optional arg))
(declare-function org-copilot-open-panels "org-copilot" ())

(defcustom org-copilot-comment-preview-length 240
  "Maximum characters in a Copilot side-card preview, or nil for full text.
The right arrow opens the comment thread with its full stored explanation."
  :type '(choice (const :tag "Full text" nil) (integer :tag "Characters"))
  :group 'org-copilot)

(defun org-copilot-chat-comment-presentation (source comment)
  "Offer a bounded preview and full explanation action for SOURCE's COMMENT."
  (when (equal (or (plist-get comment :comment-provider)
		   (plist-get comment :provider)) "org-copilot")
    (let* ((id (plist-get comment :id))
	   (body (org-link-display-format (or (plist-get comment :body) "")))
	   (text (string-trim (replace-regexp-in-string "[[:space:]]+" " " body)))
	   (limit (buffer-local-value 'org-copilot-comment-preview-length source)))
      (when (and (stringp id) (not (string-empty-p id)))
	(when (and limit (> (length text) (max 1 limit)))
	  (let* ((prefix (substring text 0 (max 0 (- limit 2))))
		 (boundary (string-match "[[:space:]][^[:space:]]*\\'" prefix)))
	    (setq text (substring prefix 0 (or boundary (length prefix))))))
	(list :text text :label "→" :inline t
	      :open (lambda () (org-copilot-chat-show-explanation source id)))))))

(defun org-copilot-chat-show-explanation (source comment-id)
  "Open SOURCE's COMMENT-ID thread and focus its ordinary typing area."
  (unless (buffer-live-p source)
    (user-error "The explanation's source is no longer available"))
  (with-current-buffer source
    (unless (org-copilot-find-visible-comment comment-id)
      (user-error "The explanation is no longer available; no other comment was opened"))
    (org-copilot-chat-discuss-comment source comment-id)))

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

(defcustom org-copilot-chat-input-height 5
  "Height of the fixed Org Copilot typing area, in lines."
  :type 'natnum
  :group 'org-copilot)

(defvar-local org-copilot-chat--input-buffer nil
  "Active source-owned typing buffer; retained when its views are closed.")

(defvar-local org-copilot-chat--input-buffers nil
  "Typing buffers keyed by document or durable comment identity.
Drafts are presentation state, never conversation history.")

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
    (define-key map (kbd "C-c C-r") #'org-copilot-chat-resolve-focused-comment-at-point)
    (define-key map (kbd "C-c C-n") #'org-copilot-chat-focus-next-comment)
    (define-key map (kbd "C-c C-p") #'org-copilot-chat-focus-previous-comment)
    (define-key map (kbd "C-c C-u") #'org-copilot-chat-undo-focused-comment-at-point)
    (define-key map (kbd "C-c C-g") #'org-copilot-chat-full-document)
    (define-key map (kbd "C-c C-l") #'org-copilot-chat-toggle-hierarchy)
    (define-key map (kbd "C-c C-x / g") #'org-copilot-chat-full-document)
    (define-key map (kbd "C-c C-x / s") #'org-copilot-chat-section)
    (define-key map (kbd "C-c C-x / o") #'org-copilot-open-panels)
    (define-key map (kbd "C-c C-x / c") #'org-copilot-chat-clear-session-at-point)
    (define-key map (kbd "C-c C-x / u") #'org-copilot-chat-clear-ui-at-point)
    (define-key map (kbd "C-c C-x / d") #'org-copilot-chat-doctor-at-point)
    (define-key map (kbd "C-c C-x / e") #'org-copilot-chat-erase-session-at-point)
    (define-key map (kbd "M-a") #'org-copilot-chat-accept-focused-suggestion-at-point)
    (define-key map (kbd "M-d") #'org-copilot-chat-dismiss-focused-comment-at-point)
    (define-key map (kbd "M-z") #'org-copilot-chat-resolve-focused-comment-at-point)
    (define-key map (kbd "M-n") #'org-copilot-chat-focus-next-comment)
    (define-key map (kbd "M-p") #'org-copilot-chat-focus-previous-comment)
    (define-key map (kbd "M-u") #'org-copilot-chat-undo-focused-comment-at-point)
    (define-key map (kbd "M-g") #'org-copilot-chat-full-document)
    (define-key map (kbd "M-s") #'org-copilot-chat-section)
    (define-key map (kbd "M-<up>") #'org-copilot-chat-recall-last-prompt)
    map)
  "Keymap used in Org Copilot chat buffers.")

(defvar-local org-copilot-chat--hierarchy-hidden nil
  "Whether the source's discussion index is explicitly collapsed.")

(defvar-local org-copilot-chat--hierarchy-buffer nil
  "Read-only index buffer owned by the source discussion view.")

(declare-function context-panels-footer-display-lateral-pane "context-panels-footer" (body buffer width))
(declare-function context-panels-footer-hide-lateral-pane "context-panels-footer" (body))

(defun org-copilot-chat--footer (source _view)
  "Describe the selected discussion in SOURCE for the native fixed footer."
  (when (buffer-live-p source)
    (list :metadata-leading t
	  :actions (when (fboundp 'context-panels-footer-display-lateral-pane)
		     (list (list :key "C-c C-l"
				 :label (if (buffer-local-value 'org-copilot-chat--hierarchy-hidden source)
					    "Show discussions" "Hide discussions")
				 :command #'org-copilot-chat-toggle-hierarchy)))
	  :metadata (format "%s · %s" (buffer-name source)
			    (org-copilot-chat--context-label source)))))

(define-derived-mode org-copilot-chat-mode fundamental-mode "Org-Copilot-Chat"
  "Major mode for Org Copilot bottom chat buffers."
  (setq buffer-read-only nil)
  (setq-local mode-line-format nil)
  (add-hook 'window-buffer-change-functions #'org-copilot-chat--window-shown nil t))

(define-derived-mode org-copilot-chat-input-mode org-copilot-chat-mode "Copilot-Input"
  "Editable, source-owned typing area, independent of transcript scrolling."
  (remove-hook 'window-buffer-change-functions #'org-copilot-chat--window-shown t)
  (setq-local mode-line-format nil)
  (setq-local header-line-format " ")
  (setq-local truncate-lines nil)
  (setq-local word-wrap t)
  (when-let* ((background (face-background 'default nil t))
	      (rgb (color-name-to-rgb background)))
    (face-remap-add-relative
     'default (list :background
		    (apply #'color-rgb-to-hex
			   (append (mapcar (lambda (channel) (+ (* 0.98 channel) 0.02)) rgb) '(2)))))
    (face-remap-add-relative
     'header-line (list :height 0.1 :box nil :background
			(apply #'color-rgb-to-hex
			       (append (mapcar (lambda (channel) (+ (* 0.92 channel) 0.08)) rgb) '(2)))))))

(defun org-copilot-chat-input-buffer (source-buffer)
  "Return SOURCE-BUFFER's active discussion input, preserving other drafts."
  (with-current-buffer source-buffer
    (let ((key (or (org-copilot-chat--context-id org-copilot-chat-context) 'document)))
      (setq org-copilot-chat--input-buffer (cdr (assoc key org-copilot-chat--input-buffers)))
      (unless (buffer-live-p org-copilot-chat--input-buffer)
	(setq org-copilot-chat--input-buffer
	      (generate-new-buffer (format "*Copilot Input: %s*" (buffer-name source-buffer))))
	(with-current-buffer org-copilot-chat--input-buffer
	  (org-copilot-chat-input-mode)
	  (setq org-copilot-chat-source-buffer source-buffer
		context-panels-source-buffer source-buffer)
	  ;; Input is a child of the chat view, not another framework bottom view.
	  (setq context-panels-view-id nil)
	  (insert (propertize "You: " 'face 'org-copilot-chat-you-face
			      'read-only t 'rear-nonsticky t))
	  (setq org-copilot-chat--prompt-start (copy-marker (point) nil)))
	(setf (alist-get key org-copilot-chat--input-buffers nil nil #'equal) org-copilot-chat--input-buffer))
      org-copilot-chat--input-buffer)))

(defun org-copilot-chat--ensure-input (window)
  "Display the source-owned input below live transcript WINDOW."
  (when (and (window-live-p window)
	     (window-parameter window 'context-panels-protected)
	     (with-current-buffer (window-buffer window)
	       (and (derived-mode-p 'org-copilot-chat-mode)
		    (not (derived-mode-p 'org-copilot-chat-input-mode))
		    (buffer-live-p org-copilot-chat-source-buffer))))
    (let* ((source (buffer-local-value 'org-copilot-chat-source-buffer (window-buffer window)))
	   (input (org-copilot-chat-input-buffer source)))
      ;; The hierarchy can move the existing input into the transcript column.
      ;; Resolve the input after arranging the hierarchy, not before.
      (org-copilot-chat--show-hierarchy source window)
      (context-panels-footer-display-pane window input org-copilot-chat-input-height))))

(defun org-copilot-chat--window-shown (window)
  "Schedule the input for transcript WINDOW after layout hooks finish."
  (when (and (window-live-p window)
	     (not (derived-mode-p 'org-copilot-chat-input-mode)))
    (run-with-idle-timer 0 nil #'org-copilot-chat--ensure-input window)))

(defun org-copilot-chat-focus-input (&optional source-buffer noerror)
  "Focus SOURCE-BUFFER's visible typing area without changing its draft.
With NOERROR, return nil when its chat is not visible."
  (let* ((source (or source-buffer (org-copilot-chat--source-buffer)))
	 (transcript (get-buffer (buffer-local-value 'org-copilot-chat-buffer-name source)))
	 (window (and transcript (get-buffer-window transcript (selected-frame))))
	 (child (and window
		     (eq (buffer-local-value 'org-copilot-chat-source-buffer transcript) source)
		     (org-copilot-chat--ensure-input window))))
    (if (window-live-p child)
	(progn (select-window child) (goto-char (point-max)) (window-buffer child))
      (unless noerror (user-error "No visible Copilot typing area")))))

(defun org-copilot-chat--protect-window (window panel-buffer source-buffer)
  "Protect WINDOW for PANEL-BUFFER and SOURCE-BUFFER when supported."
  (when (and (window-live-p window)
	     (fboundp 'context-panels-protect-window))
    (context-panels-protect-window window panel-buffer source-buffer)))

(defun org-copilot-chat--source-buffer ()
  "Return the source buffer for Org Copilot chat commands."
  (cond
   ((buffer-live-p org-copilot-chat-source-buffer)
    org-copilot-chat-source-buffer)
   ((buffer-live-p context-panels-source-buffer)
    context-panels-source-buffer)
   ((derived-mode-p 'org-mode)
    (current-buffer))
   (t
    (context-panels-current-source-buffer))))

(defun org-copilot-chat--comment-at-point ()
  "Return an AI comment at point, or nil."
  (ignore-errors
    (context-panels-item-at-point)))

(defun org-copilot-chat--context-id (context)
  "Return stable identifier for chat CONTEXT."
  (pcase (plist-get context :type)
    ('comment (format "comment:%s" (plist-get context :comment-id)))
    ('section (or (plist-get context :recorded-id)
		  (format "section:%s"
			  (or (plist-get context :section-path)
			      (plist-get context :heading-line)
			      (plist-get context :section-title)))))
    (_ nil)))

(defun org-copilot-chat--set-context (source-buffer context)
  "Set SOURCE-BUFFER chat CONTEXT and focus fields."
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
    (org-copilot-chat--scroll-source-to-comment source-buffer comment)
    (when (fboundp 'org-copilot-chat--refresh-source-ui)
      (org-copilot-chat--refresh-source-ui source-buffer))))

(defun org-copilot-chat--focused-comment (source-buffer)
  "Return SOURCE-BUFFER's focused AI comment, or nil."
  (with-current-buffer source-buffer
    (and org-copilot-chat-focus-comment-id
	 (org-copilot-find-visible-comment org-copilot-chat-focus-comment-id))))

(defun org-copilot-chat--scroll-source-to-comment (source-buffer comment)
  "Scroll SOURCE-BUFFER windows to COMMENT without moving buffer point."
  (when-let* ((position (plist-get comment :source-start)))
    (dolist (window (get-buffer-window-list source-buffer nil t))
      (when (window-live-p window)
	(let ((old-point (window-point window))
	      start)
	  (with-current-buffer source-buffer
	    (save-excursion
	      (goto-char position)
	      (forward-line -2)
	      (setq start (line-beginning-position))))
	  (set-window-start window start t)
	  (set-window-point window old-point))))))

(defun org-copilot-chat--insert-metadata (text)
  "Insert metadata TEXT using the Org Copilot metadata face."
  (insert (propertize text 'face 'org-copilot-chat-metadata-face)))

(defun org-copilot-chat--comment-marker (comment)
  "Return compact context marker for COMMENT."
  (pcase (org-copilot-comment-status comment)
    ('accepted "✅")
    ('stale "⚠️")
    (_ (if (org-copilot-comment-suggestion-text comment) "✏️" "💬"))))

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
		    (format "Discussion · %s"
			    (truncate-string-to-width
			     (replace-regexp-in-string "[[:space:]]+" " "
						       (or (plist-get comment :target-text) "Finding"))
			     60 nil nil "…"))
		  "💬 Comment"))
      ('section (format "§ Section: %s"
			(or (plist-get org-copilot-chat-context :section-title)
			    (plist-get org-copilot-chat-context :heading-line)
			    "current")))
      (_ "🌐 Full document"))))

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

(defun org-copilot-chat--insert-org-content (content)
  "Insert CONTENT as hook-free Org with explicit source-owned link actions."
  (let ((start (point))
	(source org-copilot-chat-source-buffer))
    (context-panels-org-insert-body content)
    (let ((links (with-temp-buffer
		   (delay-mode-hooks (org-mode))
		   (insert content)
		   (org-element-map (org-element-parse-buffer) 'link
				    (lambda (link)
				      (let* ((begin (org-element-property :begin link))
					     (end (- (org-element-property :end link)
						     (org-element-property :post-blank link))))
					(list (or (org-element-property :contents-begin link) begin)
					      (or (org-element-property :contents-end link) end)
					      (buffer-substring-no-properties begin end))))))))
      (dolist (link links)
	(let ((target (nth 2 link)))
	  (make-text-button
	   (+ start (1- (nth 0 link))) (+ start (1- (nth 1 link)))
	   'follow-link t 'help-echo target
	   'action (lambda (_)
		     (unless (buffer-live-p source)
		       (user-error "The link's source is no longer available"))
		     (with-current-buffer source
		       (org-link-open-from-string target)))))))))

(defun org-copilot-chat--insert-indented-content (content &optional face)
  "Insert CONTENT as an indented Org chat block, optionally using FACE."
  (if face
      (dolist (line (split-string content "\n"))
	(insert "  " (propertize line 'face face) "\n"))
    (let ((start (point)))
      (org-copilot-chat--insert-org-content content)
      (insert "\n")
      (let ((end (copy-marker (point))))
	(save-excursion
	  (goto-char start)
	  (while (< (point) end)
	    (insert "  ")
	    (forward-line 1)))
	(set-marker end nil)))))

(defun org-copilot-chat--timestamp-label (created-at)
  "Format recorded CREATED-AT in local time without inventing missing dates."
  (condition-case nil
      (if (and (stringp created-at) (not (string-empty-p created-at)))
	  (format-time-string "[%H:%M]" (date-to-time created-at))
	"[time unknown]")
    (error "[time unknown]")))

(defun org-copilot-chat--insert-message (message)
  "Insert one chat MESSAGE plist into the current buffer."
  (let* ((role (plist-get message :role))
	 (label (org-copilot-chat--role-label role))
	 (content (or (plist-get message :content) ""))
	 (start (point)))
    (insert (propertize
	     (format "%s %s:"
		     (org-copilot-chat--timestamp-label (plist-get message :created-at)) label)
	     'face (org-copilot-chat--role-face role)
	     'help-echo (or (plist-get message :created-at) "No recorded timestamp")))
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
  "Show the tail after a visible SOURCE-BUFFER update with ROLE."
  (when-let* ((buffer (get-buffer org-copilot-chat-buffer-name)))
    (with-current-buffer buffer
      (when (eq org-copilot-chat-source-buffer source-buffer)
	(when (org-copilot-chat--last-message-position role)
	  (org-copilot-chat--show-tail buffer))))))

(defun org-copilot-chat--message-discussion (message)
  "Return MESSAGE's recorded discussion key, or nil for the document."
  (let ((id (plist-get message :context-id))
	(comment (or (plist-get message :thread-comment-id) (plist-get message :comment-id))))
    (cond (comment (format "comment:%s" comment))
	  ((and id (or (string-prefix-p "section:" id) (string-prefix-p "comment:" id))) id))))

(defun org-copilot-chat--visible-messages (source-buffer)
  "Return only the exchanges in SOURCE-BUFFER's selected discussion."
  (with-current-buffer source-buffer
    (let ((key (org-copilot-chat--context-id org-copilot-chat-context)))
      (cl-remove-if-not (lambda (message) (equal key (org-copilot-chat--message-discussion message)))
			(org-copilot-chat-messages)))))

(defun org-copilot-chat-discuss-comment (source id)
  "Explicitly select SOURCE's comment ID as discussion focus and open its input."
  (unless (with-current-buffer source (org-copilot-find-visible-comment id))
    (user-error "This comment is no longer available for discussion"))
  (org-copilot-chat-focus-comment-id source id)
  (org-copilot-chat--open-bottom-view source))

(defun org-copilot-chat--insert-finding (source id)
  "Insert SOURCE's full finding ID at its producing response, before navigation."
  (when-let* ((comment (with-current-buffer source (org-copilot-find-visible-comment id))))
    (let ((start (point)))
      (org-copilot-chat--insert-indented-content (plist-get comment :body))
      (put-text-property start (point) 'org-copilot-chat-explanation id)
      (insert-text-button "→" 'follow-link t 'help-echo "Open this comment discussion"
			  'action (lambda (_) (org-copilot-chat-discuss-comment source id)))
      (insert "\n\n"))))

(defun org-copilot-chat-render (source-buffer)
  "Render the Org Copilot chat transcript for SOURCE-BUFFER."
  (let* ((inhibit-read-only t)
	 (messages (org-copilot-chat--visible-messages source-buffer))
	 (thread (buffer-local-value 'org-copilot-chat-focus-comment-id source-buffer))
	 (comment (and thread (with-current-buffer source-buffer
				(org-copilot-find-visible-comment thread)))))
    (erase-buffer)
    (let ((transcript-start (point)))
      (when thread
	(when comment
	  (let ((root-start (point)))
	    (insert (propertize
		     (format "%s %s:\n"
			     (org-copilot-chat--timestamp-label (plist-get comment :created-at))
			     (or (plist-get comment :author) "Comment"))
		     'face 'org-copilot-chat-copilot-face))
	    (org-copilot-chat--insert-indented-content (or (plist-get comment :body) ""))
	    (insert "\n")
	    (put-text-property root-start (point) 'org-copilot-chat-explanation thread))))
      (cond
       (messages
	(dolist (message messages)
	  (org-copilot-chat--insert-message message)
	  (dolist (id (plist-get message :finding-ids))
	    (org-copilot-chat--insert-finding source-buffer id))))
       ((not thread)
	(org-copilot-chat--insert-metadata
	 (if (org-copilot-chat--focused-comment source-buffer)
	     "Ask Copilot to revise, explain, or improve this suggestion.\n"
	   "Ask Copilot about the full document.\n"))))
      (add-text-properties transcript-start (point)
			   '(read-only t rear-nonsticky (read-only))))
    (setq buffer-read-only t)
    (org-copilot-chat-input-buffer source-buffer)
    (when (plist-get context-panels-footer-spec :footer)
      (context-panels-footer-update (current-buffer)))
    (let ((selection (cons source-buffer
			   (with-current-buffer source-buffer
			     (org-copilot-chat--context-id org-copilot-chat-context)))))
      (dolist (window (get-buffer-window-list (current-buffer) nil t))
	(org-copilot-chat--ensure-input window)
	(unless (equal selection (window-parameter window 'org-copilot-discussion))
	  (set-window-parameter window 'org-copilot-discussion selection)
	  (org-copilot-chat--show-window-tail window))))))

(defun org-copilot-chat--focused-suggestion-comment (source-buffer)
  "Return focused comment with a suggestion for SOURCE-BUFFER, or nil."
  (when-let* ((comment (org-copilot-chat--focused-comment source-buffer)))
    (and (with-current-buffer source-buffer
	   (org-copilot-comment-suggestion-text comment))
	 comment)))

(defun org-copilot-chat--navigable-comments (source-buffer)
  "Return non-dismissed AI comments for SOURCE-BUFFER."
  (with-current-buffer source-buffer
    (cl-remove-if
     (lambda (comment)
       (eq (org-copilot-comment-status comment) 'dismissed))
     (org-copilot-visible-comments))))

(defun org-copilot-chat--discussion-ring (source)
  "Return SOURCE's established discussion contexts in source order."
  (with-current-buffer source
    (let ((keys (delete-dups (mapcar #'org-copilot-chat--message-discussion
				     (cl-remove-if-not (lambda (turn) (memq (plist-get turn :role) '(user assistant)))
						       (org-copilot-chat-messages)))))
	  sections entries)
      (when (and (derived-mode-p 'org-mode)
		 (seq-some (lambda (key) (and key (string-prefix-p "section:" key))) keys))
	(org-with-wide-buffer
	 (goto-char (point-min))
	 (while (re-search-forward org-heading-regexp nil t)
	   (beginning-of-line)
	   (let* ((context (org-copilot-chat--section-context-at-point))
		  (key (org-copilot-chat--context-id context)))
	     (when (member key keys)
	       (push (cons key context) sections)))
	   (forward-line 1))))
      (when (memq nil keys) (push (cons -1 '(:type full-document)) entries))
      (dolist (key keys)
	(when (and key (string-prefix-p "section:" key))
	  (let* ((matches (cl-remove-if-not (lambda (entry) (equal (car entry) key)) sections))
		 (context (and (= (length matches) 1) (cdar matches))))
	    (push (cons (or (and context (marker-position (plist-get context :heading-marker))) most-positive-fixnum)
			(or context (list :type 'section :recorded-id key :section-title (concat (substring key 8) " · location unavailable"))))
		  entries))))
      (dolist (comment (org-copilot-chat--navigable-comments source))
	(push (cons (or (plist-get comment :source-start) most-positive-fixnum)
		    (list :type 'comment :comment-id (org-copilot-comment-id comment))) entries))
      (mapcar #'cdr (cl-stable-sort (nreverse entries) #'< :key #'car)))))

(defun org-copilot-chat--step-discussion (source step)
  "Select the next or previous established discussion in SOURCE by STEP."
  (let* ((ring (org-copilot-chat--discussion-ring source))
	 (index (cl-position (with-current-buffer source (org-copilot-chat--context-id org-copilot-chat-context))
			     ring :key #'org-copilot-chat--context-id :test #'equal))
	 (count (length ring)))
    (unless ring (user-error "No established discussions"))
    (org-copilot-chat--select-discussion
     source (nth (if index (mod (+ index step) count) (if (> step 0) 0 (1- count))) ring))))

(defun org-copilot-chat--select-discussion (source context)
  "Select CONTEXT in SOURCE, keeping the ordinary input ready."
  (if-let* ((id (plist-get context :comment-id)))
      (progn
	(org-copilot-chat--focus-and-refresh source
					     (with-current-buffer source (org-copilot-find-visible-comment id)))
	(org-copilot-chat-focus-input source t))
    (org-copilot-chat--set-context source context)
    (when-let* ((marker (plist-get context :heading-marker)))
      (with-current-buffer source
	(when (<= (point-min) (marker-position marker) (point-max))
	  (org-copilot-chat--scroll-source-to-comment source (list :source-start (marker-position marker))))))
    (org-copilot-chat--refresh-source-ui source)
    (org-copilot-chat--open-bottom-view source)))

(defun org-copilot-chat--hierarchy-rows (source)
  "Return established discussions with display labels and depths for SOURCE."
  (with-current-buffer source
    (let* ((ring (org-copilot-chat--discussion-ring source))
	   (sections (delq nil (mapcar (lambda (entry) (plist-get entry :section-path)) ring)))
	   (document (seq-some (lambda (entry) (eq (plist-get entry :type) 'full-document)) ring)))
      (mapcar
       (lambda (context)
	 (let* ((kind (plist-get context :type))
		(comment (when-let* ((id (plist-get context :comment-id)))
			   (org-copilot-find-visible-comment id)))
		(path (or (plist-get context :section-path)
			  (when-let* ((position (plist-get comment :source-start)))
			    (unless (eq (org-copilot-comment-status comment) 'stale)
			      (org-with-wide-buffer
			       (when (<= (point-min) position (point-max))
				 (goto-char position)
				 (unless (org-before-first-heading-p)
				   (org-get-outline-path t t))))))))
		(ancestors (cl-count-if (lambda (section)
					  (and (or (eq kind 'comment) (< (length section) (length path)))
					       (equal section (seq-take path (length section))))) sections))
		(label (pcase kind
			 ('full-document "Document")
			 ('section (or (plist-get context :section-title) "Section"))
			 (_ (concat (when (and path (not (member path sections)))
				      (concat (car (last path)) " · "))
				    (or (plist-get comment :target-text) (plist-get comment :body) "Comment"))))))
	   (list context (if (eq kind 'full-document) 0 (+ ancestors (if document 1 0)))
		 (string-trim (replace-regexp-in-string "[[:space:]]+" " " label)))))
       ring))))

(defun org-copilot-chat--kill-hierarchy ()
  "Dispose of the hierarchy buffer after the source's windows close."
  (when (buffer-live-p org-copilot-chat--hierarchy-buffer)
    (kill-buffer org-copilot-chat--hierarchy-buffer)))

(defun org-copilot-chat--show-hierarchy (source window)
  "Render SOURCE's discussion index beside transcript WINDOW if it fits."
  (when (fboundp 'context-panels-footer-display-lateral-pane)
    (if (buffer-local-value 'org-copilot-chat--hierarchy-hidden source)
	(context-panels-footer-hide-lateral-pane window)
      (let* ((buffer (with-current-buffer source
		       (unless (buffer-live-p org-copilot-chat--hierarchy-buffer)
			 (setq org-copilot-chat--hierarchy-buffer
			       (generate-new-buffer (format "*Discussions: %s*" (buffer-name))))
			 (with-current-buffer org-copilot-chat--hierarchy-buffer
			   (special-mode)
			   (setq-local truncate-lines t
				       mode-line-format nil))
			 (add-hook 'kill-buffer-hook #'org-copilot-chat--kill-hierarchy t t))
		       org-copilot-chat--hierarchy-buffer))
	     (width (context-panels--side-panel-width source))
	     (index (context-panels-footer-display-lateral-pane window buffer width))
	     (active (with-current-buffer source (org-copilot-chat--context-id org-copilot-chat-context))))
	(when (window-live-p index)
	  (with-current-buffer buffer
	    (let ((inhibit-read-only t) selected)
	      (erase-buffer)
	      (dolist (row (org-copilot-chat--hierarchy-rows source))
		(pcase-let ((`(,context ,depth ,label) row))
		  (let ((current (equal active (org-copilot-chat--context-id context)))
			(start (point)))
		    (insert (make-string (* 2 depth) ?\s) (if current "▸ " "  "))
		    (insert-text-button label 'follow-link t
					'action (lambda (_) (org-copilot-chat--select-discussion source context)))
		    (insert "\n")
		    (when current
		      (setq selected start)
		      (add-face-text-property start (point) 'highlight t)))))
	      (set-window-point index (or selected (point-min))))))))))

(defun org-copilot-chat-toggle-hierarchy ()
  "Toggle the conversation hierarchy without changing the selected discussion."
  (interactive)
  (unless (fboundp 'context-panels-footer-display-lateral-pane)
    (user-error "Update Context Panels to show the discussion hierarchy"))
  (let ((source (org-copilot-chat--source-buffer)))
    (with-current-buffer source
      (setq org-copilot-chat--hierarchy-hidden (not org-copilot-chat--hierarchy-hidden)))
    (org-copilot-chat--buffer source)))

(defun org-copilot-chat-render-bottom-view (source-buffer _view)
  "Render the Org Copilot bottom chat view for SOURCE-BUFFER."
  (with-current-buffer source-buffer
    (org-copilot-restore-chat-messages))
  (setq org-copilot-chat-source-buffer source-buffer)
  (setq context-panels-source-buffer source-buffer)
  (setq context-panels-view-id 'copilot-chat)
  (org-copilot-chat-render source-buffer))

(defun org-copilot-chat-bottom-views (_source-buffer)
  "Return Org Copilot bottom view descriptors."
  (list (list :id 'copilot-chat
	      :buffer-name org-copilot-chat-buffer-name
	      :mode 'org-copilot-chat-mode
	      :height org-copilot-chat-window-height
	      :render #'org-copilot-chat-render-bottom-view
	      :footer #'org-copilot-chat--footer)))

(defun org-copilot-chat--show-window-tail (window)
  "Put the last transcript content at the bottom of WINDOW."
  (with-selected-window window
    (goto-char (point-max))
    (skip-chars-backward " \t\n")
    (let ((scroll-margin 0)) (recenter -1))))

(defun org-copilot-chat--show-tail (buffer)
  "Put BUFFER's last content at the bottom of each transcript window."
  (dolist (window (get-buffer-window-list buffer nil t))
    (org-copilot-chat--show-window-tail window)))

(defun org-copilot-chat--open-bottom-view (source-buffer)
  "Open the Org Copilot bottom chat view for SOURCE-BUFFER."
  (with-current-buffer source-buffer
    (unless org-copilot-mode (org-copilot-mode 1)))
  (let* ((buffer (context-panels-open-bottom-view 'copilot-chat source-buffer))
	 (window (get-buffer-window buffer t)))
    (org-copilot-chat-sync-diff source-buffer)
    (when (window-live-p window)
      (org-copilot-chat--ensure-input window)
      (org-copilot-chat--show-tail buffer)
      (org-copilot-chat-focus-input source-buffer))
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
  "Refresh source overlays for SOURCE-BUFFER."
  (with-current-buffer source-buffer
    (when (fboundp 'org-copilot-refresh-overlays)
      (org-copilot-refresh-overlays))))

(defun org-copilot-chat--focus-and-refresh (source-buffer comment)
  "Refresh chat and diff after focusing COMMENT in SOURCE-BUFFER."
  (org-copilot-chat--set-context
   source-buffer
   (list :type 'comment :comment-id (org-copilot-comment-id comment)))
  (unless (eq (org-copilot-comment-status comment) 'stale)
    (org-copilot-chat--scroll-source-to-comment source-buffer comment)
    (when (fboundp 'org-comments-context-panel-focus-comment)
      (ignore-errors
	(org-comments-context-panel-focus-comment source-buffer comment t))))
  (org-copilot-chat--refresh-source-ui source-buffer)
  (org-copilot-chat-sync-diff source-buffer)
  (org-copilot-chat--buffer source-buffer))

(defun org-copilot-chat-focus-comment-id (source-buffer comment-id)
  "Focus SOURCE-BUFFER's Copilot chat on COMMENT-ID without stealing focus."
  (when-let* ((comment (with-current-buffer source-buffer
			 (org-copilot-find-visible-comment comment-id))))
    (let ((selected (selected-window)))
      (org-copilot-chat--focus-and-refresh source-buffer comment)
      (when (and (fboundp 'context-panels-open-bottom-view)
		 (or (not (boundp 'context-panels-bottom-panel-buffer))
		     (not (buffer-live-p context-panels-bottom-panel-buffer))
		     (get-buffer-window context-panels-bottom-panel-buffer t)))
	(context-panels-open-bottom-view 'copilot-chat source-buffer nil))
      (when (window-live-p selected)
	(select-window selected)))
    comment))

;;;###autoload
(defun org-copilot-chat-focus-next-comment ()
  "Select the next established discussion in source order."
  (interactive)
  (org-copilot-chat--step-discussion (org-copilot-chat--source-buffer) 1))

;;;###autoload
(defun org-copilot-chat-focus-previous-comment ()
  "Select the previous established discussion in source order."
  (interactive)
  (org-copilot-chat--step-discussion (org-copilot-chat--source-buffer) -1))

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
      (let ((buffer (org-copilot-diff-open source-buffer comment)))
	(org-copilot--display-diff-buffer source-buffer buffer t))
    (org-copilot-chat--close-stale-diff)))

(defun org-copilot-chat-goto-prompt ()
  "Move point to the Org Copilot chat prompt."
  (interactive)
  (org-copilot-chat-focus-input))

(defun org-copilot-chat--buffer (source-buffer)
  "Return the chat buffer for SOURCE-BUFFER, creating it when needed."
  (with-current-buffer source-buffer
    (org-copilot-restore-chat-messages))
  (let ((buffer (get-buffer-create org-copilot-chat-buffer-name)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'org-copilot-chat-mode)
	(org-copilot-chat-mode))
      (setq org-copilot-chat-source-buffer source-buffer)
      (setq context-panels-source-buffer source-buffer)
      (setq context-panels-view-id 'copilot-chat)
      (org-copilot-chat-render source-buffer))
    (org-copilot-chat-sync-diff source-buffer)
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
  (buffer-substring-no-properties (point-min) (point-max)))

(defun org-copilot-chat--request (source-buffer message)
  "Return chat request for SOURCE-BUFFER and user MESSAGE."
  (with-current-buffer source-buffer
    (list :source-buffer source-buffer
	  :buffer-name (buffer-name source-buffer)
	  :message message
	  :messages (cl-remove-if-not
		     (lambda (entry) (memq (plist-get entry :role) '(user assistant)))
		     (org-copilot-chat-messages))
	  :chat-context org-copilot-chat-context
	  :context-id (org-copilot-chat--context-id org-copilot-chat-context)
	  :focus-comment-id org-copilot-chat-focus-comment-id
	  :source-content (org-copilot-chat--context-source-content)
	  :suggestion-source-context (org-suggestions-capture-source-context)
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
    (or (org-copilot-find-visible-comment org-copilot-chat-focus-comment-id)
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

(defun org-copilot-chat--next-comment-after (source-buffer comment)
  "Return navigable comment after COMMENT in SOURCE-BUFFER, or nil."
  (let* ((comments (org-copilot-chat--navigable-comments source-buffer))
	 (count (length comments))
	 (current-id (org-copilot-comment-id comment))
	 (index (cl-position current-id comments
			     :key #'org-copilot-comment-id
			     :test #'equal)))
    (when (and index (> count 1))
      (nth (mod (1+ index) count) comments))))

;;;###autoload
(defun org-copilot-chat-resolve-focused-comment-at-point ()
  "Resolve the current Org Copilot chat focus and advance."
  (interactive)
  (let ((source (org-copilot-chat--source-buffer)))
    (org-copilot-chat-resolve-focused-comment source)))

(defun org-copilot-chat-resolve-focused-comment (source-buffer)
  "Resolve SOURCE-BUFFER's focused AI comment and advance to the next one."
  (let* ((comment (org-copilot-chat--current-focused-comment-or-error
		   source-buffer "resolve"))
	 (next (org-copilot-chat--next-comment-after source-buffer comment)))
    (if (plist-get comment :sidecar-file)
	(org-copilot-set-durable-comment-status comment "RESOLVED")
      (user-error "Legacy in-memory Copilot comments are retired"))
    (org-copilot-chat--refresh-source-ui source-buffer)
    (org-copilot-chat--close-stale-diff)
    (if next
	(org-copilot-chat--focus-and-refresh source-buffer next)
      (org-copilot-chat--set-context source-buffer '(:type full-document))
      (org-copilot-chat--buffer source-buffer))))

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
			  (org-copilot-visible-comments))))
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
       (and chat-buffer source-live
	    (with-current-buffer (org-copilot-chat-input-buffer source-buffer)
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

;;;###autoload
(defun org-copilot-cancel-requests ()
  "Cancel current source requests without removing conversation or artifacts."
  (interactive)
  (let ((source (org-copilot-chat--source-buffer)))
    (with-current-buffer source
      (org-copilot--invalidate-requests))
    (org-copilot-chat--buffer source)))

;;;###autoload
(defun org-copilot-chat-clear-session-at-point ()
  "Archive the current Org Copilot session from the chat buffer."
  (interactive)
  (with-current-buffer (org-copilot-chat--source-buffer)
    (org-copilot-clear-session)))

;;;###autoload
(defun org-copilot-chat-clear-ui-at-point ()
  "Clear the current chat UI while preserving artifacts."
  (interactive)
  (with-current-buffer (org-copilot-chat--source-buffer)
    (org-copilot-clear-session t)))

;;;###autoload
(defun org-copilot-chat-erase-session-at-point ()
  "Hard-delete the current Org Copilot session from the chat buffer."
  (interactive)
  (with-current-buffer (org-copilot-chat--source-buffer)
    (org-copilot-erase-session)))

;;;###autoload
(defun org-copilot-chat-doctor-at-point ()
  "Append a local Org Copilot health report from the chat buffer."
  (interactive)
  (org-copilot-chat-doctor (org-copilot-chat--source-buffer)))

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
    ("/clear" . "Archive current Copilot session")
    ("/clear-ui" . "Clear chat UI while preserving artifacts")
    ("/dismiss" . "Dismiss focused comment")
    ("/doctor" . "Run Org Copilot health check")
    ("/erase" . "Hard-delete current Copilot session")
    ("/next" . "Next discussion")
    ("/prev" . "Previous discussion")
    ("/resolve" . "Resolve focused comment and advance")
    ("/undo" . "Undo accepted suggestion"))
  "Implemented Org Copilot chat slash commands and annotations.")

(defconst org-copilot-chat-slash-command-keys
  '(("/accept" "M-a" "C-c C-a")
    ("/clear" "C-c C-x / c")
    ("/clear-ui" "C-c C-x / u")
    ("/dismiss" "M-d" "C-c C-d")
    ("/doctor" "C-c C-x / d")
    ("/erase" "C-c C-x / e")
    ("/next" "M-n" "C-c C-n")
    ("/prev" "M-p" "C-c C-p")
    ("/resolve" "M-z" "C-c C-r")
    ("/undo" "M-u" "C-c C-u"))
  "Documented key coverage for Org Copilot chat slash commands.")

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
      ("/clear" t)
      ("/clear-ui" t)
      ("/doctor" t)
      ("/erase" t)
      ((or "/next" "/prev") (org-copilot-chat--discussion-ring source-buffer))
      ("/accept" (and comment
		      (not (memq (org-copilot-comment-status comment)
				 '(accepted stale dismissed)))
		      (with-current-buffer source-buffer
			(org-copilot-comment-suggestion-text comment))))
      ("/dismiss" (and comment
		       (not (eq (org-copilot-comment-status comment)
				'dismissed))))
      ("/resolve" (and comment
		       (not (memq (org-copilot-comment-status comment)
				  '(dismissed accepted stale)))))
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
    ("/clear"
     (with-current-buffer source-buffer
       (org-copilot-clear-session))
     t)
    ("/clear-ui"
     (with-current-buffer source-buffer
       (org-copilot-clear-session t))
     t)
    ("/doctor"
     (org-copilot-chat-doctor source-buffer)
     t)
    ("/dismiss"
     (org-copilot-chat-dismiss-focused-comment source-buffer)
     t)
    ("/erase"
     (with-current-buffer source-buffer
       (org-copilot-erase-session))
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
    ("/resolve"
     (org-copilot-chat-resolve-focused-comment source-buffer)
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
  (with-current-buffer (org-copilot-chat-input-buffer (org-copilot-chat--source-buffer))
    (string-trim
     (buffer-substring-no-properties org-copilot-chat--prompt-start (point-max)))))

(defun org-copilot-chat--set-prompt-text (text)
  "Replace the current editable prompt with TEXT."
  (with-current-buffer (org-copilot-chat-input-buffer (org-copilot-chat--source-buffer))
    (let ((inhibit-read-only t))
      (delete-region org-copilot-chat--prompt-start (point-max))
      (goto-char org-copilot-chat--prompt-start)
      (insert text))))

(defun org-copilot-chat--last-user-message (source-buffer)
  "Return last user message content in SOURCE-BUFFER's conversation."
  (cl-loop for message in (reverse (org-copilot-chat--visible-messages source-buffer))
	   when (eq (plist-get message :role) 'user)
	   return (plist-get message :content)))

;;;###autoload
(defun org-copilot-chat-recall-last-prompt ()
  "Recall the last submitted prompt in the current conversation."
  (interactive)
  (let* ((source (org-copilot-chat--source-buffer))
	 (message (org-copilot-chat--last-user-message source)))
    (unless message
      (user-error "No previous Org Copilot prompt in this conversation"))
    (org-copilot-chat--set-prompt-text message)))

;;;###autoload
(defun org-copilot-chat-send-current-input ()
  "Send the current Org Copilot prompt field."
  (interactive)
  (when (and (derived-mode-p 'org-copilot-chat-input-mode)
	     (not (eq (current-buffer)
		      (org-copilot-chat-input-buffer (org-copilot-chat--source-buffer)))))
    (user-error "This draft belongs to another discussion; reopen that discussion before sending"))
  (let ((message (org-copilot-chat--prompt-text)))
    (unless (string-empty-p message)
      (let ((input (org-copilot-chat-input-buffer (org-copilot-chat--source-buffer))))
	(org-copilot-chat-send message)
	(when (buffer-live-p input)
	  (with-current-buffer input
	    (delete-region org-copilot-chat--prompt-start (point-max))))))))

;;;###autoload
(defun org-copilot-chat-return-dwim ()
  "Send input from the prompt, or move to the prompt from transcript text."
  (interactive)
  (if (and (markerp org-copilot-chat--prompt-start)
	   (>= (point) org-copilot-chat--prompt-start))
      (org-copilot-chat-send-current-input)
    (org-copilot-chat-goto-prompt)))

;;;###autoload
(defun org-copilot-chat-send (message &optional captured-request)
  "Send MESSAGE to the Org Copilot chat session.
Interactively, read MESSAGE from the minibuffer outside chat buffers.  When
`org-copilot-chat-function' returns an assistant response, append it to the
session transcript.  Slash command `/accept' accepts the focused suggestion."
  (interactive (list (if (derived-mode-p 'org-copilot-chat-mode)
			 (org-copilot-chat--prompt-text)
		       (read-string "Org Copilot chat: "))))
  (let* ((source (or (plist-get captured-request :source-buffer)
		     (org-copilot-chat--source-buffer)))
	 (comment-id (if captured-request (plist-get captured-request :focus-comment-id)
		       (with-current-buffer source org-copilot-chat-focus-comment-id)))
	 (context-id (if captured-request (plist-get captured-request :context-id)
		       (with-current-buffer source (org-copilot-chat--context-id org-copilot-chat-context))))
	 request)
    (unless (org-copilot-chat--dispatch-command message source)
      (with-current-buffer source
	(org-copilot-restore-chat-messages)
	(org-copilot-add-chat-message 'user message comment-id context-id captured-request)
	(when org-copilot-chat-function
	  (setq request (org-copilot-start-request
			 (or captured-request (org-copilot-chat--request source message))))
	  ;; Register pending state before invoking even a synchronous adapter.
	  (org-copilot-add-chat-message 'pending "…" comment-id context-id request))
	(org-copilot-chat--buffer source)
	(org-copilot-chat-scroll-to-last-message source (if request 'pending 'user))
	(when request
	  (condition-case err
	      (let* ((response (funcall org-copilot-chat-function request))
		     (answer (org-copilot-chat--adapter-message response)))
		(when (and answer (org-copilot-begin-completion request))
		  (unwind-protect
		      (progn
			(org-copilot-add-chat-message
			 'assistant answer comment-id context-id request)
			(org-copilot-chat--buffer source)
			(org-copilot-chat-scroll-to-last-message source 'assistant))
		    (org-copilot-finish-request request))))
	    (error
	     (org-copilot-cancel-request request)
	     (signal (car err) (cdr err)))))))))

(defun org-copilot-submit-chat-request (request)
  "Submit captured REQUEST through ordinary chat without changing editing focus.
Reuse its source-session token, source witness and context rather than creating
another review execution or replacing them with callback-time editor state."
  (when (or (not (plist-get request :request-token))
	    (org-copilot-request-current-p request 'pending))
    (org-copilot-chat-send (plist-get request :message) request)))

(provide 'org-copilot-chat)
;;; org-copilot-chat.el ends here
