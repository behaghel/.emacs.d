;;; context-panel.el --- Org mode context side panel -*- lexical-binding: t; -*-

;;; Commentary:
;; Read-only right-side panel for contextual Org authoring records.  Native Org
;; footnotes are currently the first data source; sidecar comments will follow.

;;; Code:

(require 'cl-lib)
(require 'hub-confluence-people)
(require 'hub-org-comments)
(require 'hub-org-marginalia)
(require 'org)
(require 'subr-x)

(autoload 'hub/confluence-comment-push-current "org-confluence-commands" nil t)
(autoload 'hub/confluence-sync-status-marker-string "org-confluence-commands" nil nil)
(autoload 'hub/confluence-sync-status-open-from-marker "org-confluence-commands" nil t)

(defgroup hub/org-context-panel nil
  "Interactive Org context panel."
  :group 'org)

(defcustom hub/org-context-panel-buffer-name "*Org Context*"
  "Buffer name used for the Org context side panel."
  :type 'string
  :group 'hub/org-context-panel)

(defcustom hub/org-page-context-buffer-name "*Org Page Context*"
  "Buffer name used for page-level Org context cards."
  :type 'string
  :group 'hub/org-context-panel)

(defcustom hub/org-context-panel-width 38
  "Width of the Org context side panel."
  :type 'natnum
  :group 'hub/org-context-panel)

(defcustom hub/org-context-panel-dock-prose t
  "Whether opening the context panel docks visually filled prose toward it."
  :type 'boolean
  :group 'hub/org-context-panel)

(defcustom hub/org-context-panel-target-preview-length 20
  "Maximum length of quoted comment target previews in the context panel."
  :type 'natnum
  :group 'hub/org-context-panel)

(defcustom hub/org-context-panel-overview-comment-lines 2
  "Maximum number of comment body lines shown in context panel overview mode."
  :type 'natnum
  :group 'hub/org-context-panel)

(defcustom hub/org-context-panel-overview-comment-line-length 34
  "Maximum length of each comment body line in context panel overview mode."
  :type 'natnum
  :group 'hub/org-context-panel)

(defface hub/org-context-panel-status-open-face
  '((t :inherit default :foreground "#FFFFFF" :background "#0052CC" :box (:line-width (1 . -1) :color "#0052CC")))
  "Face for open comment status chips."
  :group 'hub/org-context-panel)

(defface hub/org-context-panel-status-todo-face
  '((t :inherit default :foreground "#172B4D" :background "#FFAB00" :box (:line-width (1 . -1) :color "#FFAB00")))
  "Face for todo comment status chips."
  :group 'hub/org-context-panel)

(defface hub/org-context-panel-status-resolved-face
  '((t :inherit default :foreground "#FFFFFF" :background "#00875A" :box (:line-width (1 . -1) :color "#00875A")))
  "Face for resolved comment status chips."
  :group 'hub/org-context-panel)

(defface hub/org-context-panel-status-default-face
  '((t :inherit default :foreground "#172B4D" :background "#DFE1E6" :box (:line-width (1 . -1) :color "#DFE1E6")))
  "Face for default comment status chips."
  :group 'hub/org-context-panel)

(defface hub/org-context-panel-remote-missing-status-face
  '((t :inherit warning :strike-through t))
  "Face for remote-missing comment status chips."
  :group 'hub/org-context-panel)

(defface hub/org-context-panel-icon-face
  '((t :inherit shadow))
  "Face used for compact context panel icons."
  :group 'hub/org-context-panel)

(defface hub/org-context-panel-target-face
  '((t :inherit shadow :slant italic))
  "Face used for comment target previews."
  :group 'hub/org-context-panel)

(defface hub/org-context-panel-comment-region-face
  '((t :inherit highlight :underline t))
  "Face used to mark commented source regions."
  :group 'hub/org-context-panel)

(defface hub/org-context-panel-current-item-face
  '((t :inherit highlight))
  "Face used for the context item at point in the source buffer."
  :group 'hub/org-context-panel)

(defface hub/org-context-panel-metadata-face
  '((t :inherit font-lock-comment-face))
  "Face used for author and timestamp metadata in the context panel."
  :group 'hub/org-context-panel)

(defface hub/org-context-panel-current-author-face
  '((t :inherit (font-lock-keyword-face hub/org-context-panel-metadata-face)
       :weight bold))
  "Face used for the current user's author name in context panel metadata."
  :group 'hub/org-context-panel)

(defface hub/org-context-panel-help-key-face
  '((t :inherit font-lock-keyword-face :weight bold))
  "Face used for key names in the context panel help footer."
  :group 'hub/org-context-panel)

(defface hub/org-context-panel-help-description-face
  '((t :inherit shadow))
  "Face used for command descriptions in the context panel help footer."
  :group 'hub/org-context-panel)

(defvar-local hub/org-context-panel-source-buffer nil
  "Org source buffer rendered by the current context panel.")

(defvar-local hub/org-context-panel--panel-buffer nil
  "Panel buffer associated with the current Org source buffer.")

(defvar-local hub/org-context-panel--page-panel-buffer nil
  "Page-context buffer associated with the current Org source buffer.")

(defvar-local hub/org-context-panel-page-context-p nil
  "Non-nil when the current context panel buffer shows page comments.")

(defvar-local hub/org-context-panel--source-buffer nil
  "Org source buffer associated with the current panel buffer.")

(defvar-local hub/org-context-panel--comment-overlays nil
  "Comment target overlays in the current Org source buffer.")

(defvar-local hub/org-context-panel--page-comment-overlay nil
  "Top-of-buffer page comment marker overlay in the current Org source buffer.")

(defvar-local hub/org-context-panel--sync-status-overlay nil
  "Top-of-buffer Confluence sync status marker overlay in the current Org source buffer.")

(defvar-local hub/org-context-panel--visual-fill-state nil
  "Saved visual-fill-column state while context panel docks prose.")

(defvar-local hub/org-context-panel--help-window nil
  "Help window currently associated with this context panel buffer.")

(defvar-local hub/org-context-panel-help--origin-window nil
  "Context panel window that opened the current help buffer.")

(defvar-local hub/org-context-panel-help--origin-point nil
  "Point to restore in the originating context panel window.")

(defvar-local hub/org-comment-compose--context nil
  "Plist describing the active Org comment compose operation.")

(defvar-local hub/org-comment-compose--window nil
  "Window currently displaying this compose buffer.")

(defvar-local hub/org-comment-compose--closing nil
  "Non-nil while closing a compose buffer intentionally.")

(defvar-local hub/org-context-panel-filter-state nil
  "Plist of active Org context panel filters for the current source buffer.")

(defvar hub/org-context-panel--following nil
  "Non-nil while the context panel follow hook is refreshing.")

(defcustom hub/org-context-panel-help-buffer-name "*Org Context Help*"
  "Buffer name used for transient context panel help."
  :type 'string
  :group 'hub/org-context-panel)

(defvar hub/org-context-panel-filter-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "z") #'hub/org-context-panel-filter-reset)
    (define-key map (kbd "a") #'hub/org-context-panel-filter-toggle-actionable)
    (define-key map (kbd "d") #'hub/org-context-panel-filter-toggle-drafts)
    (define-key map (kbd "m") #'hub/org-context-panel-filter-toggle-mine)
    (define-key map (kbd "r") #'hub/org-context-panel-filter-toggle-resolved)
    (define-key map (kbd "x") #'hub/org-context-panel-filter-toggle-missing)
    (define-key map (kbd "?") #'hub/org-context-panel-filter-status)
    map)
  "Prefix keymap for Org context panel filters.")

(defvar hub/org-context-panel-status-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "o") #'hub/org-context-panel-mark-open)
    (define-key map (kbd "t") #'hub/org-context-panel-mark-todo)
    (define-key map (kbd "r") #'hub/org-context-panel-mark-resolved)
    map)
  "Prefix keymap for Org context panel status changes.")

(defvar hub/org-context-panel-buffer-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "?") #'hub/org-context-panel-toggle-help)
    (define-key map (kbd "C-c C-c") #'hub/org-context-panel-push-item)
    (define-key map (kbd "RET") #'hub/org-context-panel-jump-to-definition)
    (define-key map (kbd "e") #'hub/org-context-panel-edit-item)
    (define-key map (kbd "m") hub/org-context-panel-status-map)
    (define-key map (kbd "o") #'hub/org-context-panel-open-remote-item)
    (define-key map (kbd "p") #'hub/org-context-panel-open-page-comments)
    (define-key map (kbd "q") #'hub/org-context-panel-close)
    (define-key map (kbd "+") #'hub/org-context-panel-reply-to-item)
    (define-key map (kbd "x") #'hub/org-context-panel-delete-item)
    (define-key map (kbd "z") hub/org-context-panel-filter-map)
    map)
  "Keymap used in Org context panel buffers.")

(with-eval-after-load 'evil
  (evil-define-key 'normal hub/org-context-panel-buffer-mode-map
		   (kbd "?") #'hub/org-context-panel-toggle-help
		   (kbd "C-c C-c") #'hub/org-context-panel-push-item
		   (kbd "RET") #'hub/org-context-panel-jump-to-definition
		   (kbd "e") #'hub/org-context-panel-edit-item
		   (kbd "m") hub/org-context-panel-status-map
		   (kbd "o") #'hub/org-context-panel-open-remote-item
		   (kbd "p") #'hub/org-context-panel-open-page-comments
		   (kbd "q") #'hub/org-context-panel-close
		   (kbd "+") #'hub/org-context-panel-reply-to-item
		   (kbd "x") #'hub/org-context-panel-delete-item
		   (kbd "z") hub/org-context-panel-filter-map
		   (kbd "]c") #'hub/org-context-panel-next-item
		   (kbd "[c") #'hub/org-context-panel-previous-item))

(defvar hub/org-context-panel-help-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") #'hub/org-context-panel-help-close)
    (define-key map (kbd "?") #'hub/org-context-panel-help-close)
    map)
  "Keymap used in Org context panel help buffers.")

(with-eval-after-load 'evil
  (evil-define-key 'normal hub/org-context-panel-help-mode-map
		   (kbd "q") #'hub/org-context-panel-help-close
		   (kbd "?") #'hub/org-context-panel-help-close))

(define-derived-mode hub/org-context-panel-buffer-mode special-mode "Org-Context"
  "Major mode for read-only Org context panel buffers."
  (setq-local truncate-lines nil
	      word-wrap t
	      revert-buffer-function #'hub/org-context-panel-revert-buffer)
  (visual-line-mode 1))

(define-minor-mode hub/org-comment-compose-mode
  "Minor mode for composing Org sidecar comments without visiting sidecars."
  :lighter " CommentCompose"
  :keymap (let ((map (make-sparse-keymap)))
	    (define-key map (kbd "C-c C-c") #'hub/org-comment-compose-submit)
	    (define-key map (kbd "C-c C-s") #'hub/org-comment-compose-save-draft)
	    (define-key map (kbd "C-c C-k") #'hub/org-comment-compose-cancel)
	    map))

(define-derived-mode hub/org-context-panel-help-mode special-mode "Org-Context-Help"
  "Major mode for transient Org context panel help buffers."
  (setq-local truncate-lines nil
	      word-wrap t))

(defun hub/org-context-panel--panel-buffer ()
  "Return the panel buffer for the current Org source buffer."
  (or (and (buffer-live-p hub/org-context-panel--panel-buffer)
	   hub/org-context-panel--panel-buffer)
      (setq hub/org-context-panel--panel-buffer
	    (get-buffer-create hub/org-context-panel-buffer-name))))

(defun hub/org-context-panel--page-panel-buffer ()
  "Return the page-context buffer for the current Org source buffer."
  (or (and (buffer-live-p hub/org-context-panel--page-panel-buffer)
	   hub/org-context-panel--page-panel-buffer)
      (setq hub/org-context-panel--page-panel-buffer
	    (get-buffer-create hub/org-page-context-buffer-name))))

(defun hub/org-context-panel--marginalia-kind-icon (kind)
  "Return a compact display icon for marginalia KIND."
  (pcase kind
    ('footnote "†")
    (_ "✣")))

(defun hub/org-context-panel--comment-status-face (status)
  "Return status chip face for comment STATUS."
  (pcase (downcase (or status ""))
    ("open" 'hub/org-context-panel-status-open-face)
    ("todo" 'hub/org-context-panel-status-todo-face)
    ("resolved" 'hub/org-context-panel-status-resolved-face)
    (_ 'hub/org-context-panel-status-default-face)))

(defun hub/org-context-panel--position-row (position window)
  "Return visible panel line for POSITION in WINDOW, or nil.
This uses Emacs' redisplay coordinates instead of counting buffer lines so
wrapped lines, visual filling, and partial scrolling follow the live window."
  (when-let* ((posn (posn-at-point position window))
	      (row (cdr (posn-col-row posn))))
    (max 1 row)))

(defun hub/org-context-panel--marginalia-with-viewport-anchor (note row)
  "Return NOTE copied with its anchor line replaced by viewport ROW."
  (let ((copy (copy-sequence note)))
    (setq copy (plist-put copy :logical-anchor-line (plist-get note :anchor-line)))
    (setq copy (plist-put copy :anchor-line row))
    copy))

(defun hub/org-context-panel--item-anchor-position (item)
  "Return source buffer anchor position for ITEM."
  (or (plist-get item :reference-pos)
      (plist-get item :anchor-pos)))

(defun hub/org-context-panel--item-with-viewport-anchor (item row)
  "Return ITEM copied with its anchor line replaced by viewport ROW."
  (hub/org-context-panel--marginalia-with-viewport-anchor item row))

(defun hub/org-context-panel--items-for-window (source-window items)
  "Return context ITEMS laid out for SOURCE-WINDOW viewport lines."
  (let* ((unanchored-items (cl-remove-if-not
			    (lambda (item)
			      (or (hub/org-context-panel--stale-comment-p item)
				  (hub/org-context-panel--page-comment-p item)))
			    items))
	 (anchored-items (cl-set-difference items unanchored-items :test #'eq)))
    (append
     (hub/org-marginalia-layout
      (delq nil
	    (mapcar
	     (lambda (item)
	       (if source-window
		   (when-let* ((row (hub/org-context-panel--position-row
				     (hub/org-context-panel--item-anchor-position item)
				     source-window)))
		     (hub/org-context-panel--item-with-viewport-anchor item row))
		 item))
	     anchored-items)))
     unanchored-items)))

(defun hub/org-context-panel--delete-comment-overlays ()
  "Delete context-panel comment overlays in the current buffer."
  (mapc #'delete-overlay hub/org-context-panel--comment-overlays)
  (setq hub/org-context-panel--comment-overlays nil)
  (when (overlayp hub/org-context-panel--page-comment-overlay)
    (delete-overlay hub/org-context-panel--page-comment-overlay))
  (setq hub/org-context-panel--page-comment-overlay nil)
  (when (overlayp hub/org-context-panel--sync-status-overlay)
    (delete-overlay hub/org-context-panel--sync-status-overlay))
  (setq hub/org-context-panel--sync-status-overlay nil))

(defun hub/org-context-panel--comment-item-p (item)
  "Return non-nil when ITEM is a sidecar comment."
  (eq (plist-get item :type) 'comment))

(defun hub/org-context-panel--stale-comment-p (item)
  "Return non-nil when ITEM is an unanchored stale sidecar comment."
  (and (hub/org-context-panel--comment-item-p item)
       (eq (plist-get item :anchor-state) 'stale)))

(defun hub/org-context-panel--page-comment-p (item)
  "Return non-nil when ITEM is a page-level sidecar comment."
  (and (hub/org-context-panel--comment-item-p item)
       (plist-get item :page-comment)))

(defun hub/org-context-panel--source-point-in-item-p (item source-point)
  "Return non-nil when SOURCE-POINT is inside ITEM's target region."
  (and source-point
       (hub/org-context-panel--comment-item-p item)
       (not (hub/org-context-panel--stale-comment-p item))
       (let ((start (plist-get item :target-start))
	     (end (plist-get item :target-end)))
	 (and start end (<= start source-point) (<= source-point end)))))

(defun hub/org-context-panel--item-with-current-state (item source-point)
  "Return ITEM copied with current-state metadata for SOURCE-POINT."
  (if (hub/org-context-panel--source-point-in-item-p item source-point)
      (plist-put (copy-sequence item) :current t)
    item))

(defun hub/org-context-panel--overview-comment-lines (body)
  "Return compact overview lines for comment BODY."
  (let ((text (string-trim (replace-regexp-in-string "[[:space:]]+" " " (or body ""))))
	(lines nil)
	(limit hub/org-context-panel-overview-comment-line-length))
    (while (and (not (string-empty-p text))
		(< (length lines) hub/org-context-panel-overview-comment-lines))
      (if (<= (length text) limit)
	  (setq lines (append lines (list text))
		text "")
	(let ((chunk (substring text 0 limit)))
	  (setq lines (append lines (list chunk))
		text (string-trim-left (substring text limit))))))
    (when (and lines (not (string-empty-p text)))
      (let* ((last-index (1- (length lines)))
	     (last-line (nth last-index lines)))
	(setf (nth last-index lines)
	      (if (> (length last-line) 1)
		  (concat (substring last-line 0 (1- (length last-line))) "…")
		"…"))))
    lines))

(defun hub/org-context-panel--item-with-overview-height (item)
  "Return ITEM copied with compact overview height where appropriate."
  (if (and (hub/org-context-panel--comment-item-p item)
	   (not (plist-get item :current)))
      (let* ((overview-lines (length (hub/org-context-panel--overview-comment-lines
				      (plist-get item :body))))
	     (metadata-lines (if (or (plist-get item :author)
				     (plist-get item :created-at)
				     (plist-get item :remote-author-id)
				     (plist-get item :remote-author-display-name))
				 1 0))
	     (reply-lines (if (plist-get item :replies) 1 0))
	     (warning-lines (if (hub/org-context-panel--stale-comment-p item) 1 0)))
	(plist-put (copy-sequence item) :height
		   (+ 1 metadata-lines overview-lines reply-lines warning-lines)))
    item))

(defun hub/org-context-panel--focused-comment (items)
  "Return the current focused comment from ITEMS, or nil."
  (cl-find-if
   (lambda (item)
     (and (hub/org-context-panel--comment-item-p item)
	  (plist-get item :current)))
   items))

(defun hub/org-context-panel--metadata-end-position ()
  "Return position after leading Org metadata keywords."
  (save-excursion
    (goto-char (point-min))
    (while (looking-at-p "^[	]*#\\+[^:
]+:.*$")
      (forward-line 1))
    (point)))

(defun hub/org-context-panel-page-comment-marker-position ()
  "Return page comment marker position in the current Org buffer, or nil."
  (cond
   ((overlayp hub/org-context-panel--page-comment-overlay)
    (overlay-start hub/org-context-panel--page-comment-overlay))
   ((hub/org-comment-collect-page (current-buffer))
    (hub/org-context-panel--metadata-end-position))))

(defun hub/org-context-panel-page-comment-marker-at-point-p ()
  "Return non-nil when point is at the page comment marker position."
  (and (derived-mode-p 'org-mode)
       (equal (point) (hub/org-context-panel-page-comment-marker-position))))

(defun hub/org-context-panel--page-comment-marker-map ()
  "Return keymap for the top page-comment marker."
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'hub/org-page-comments-open)
    (define-key map [mouse-1] #'hub/org-page-comments-open)
    map))

(defun hub/org-context-panel--refresh-page-comment-marker (page-comments)
  "Refresh top marker for PAGE-COMMENTS in the current source buffer."
  (when (overlayp hub/org-context-panel--page-comment-overlay)
    (delete-overlay hub/org-context-panel--page-comment-overlay))
  (setq hub/org-context-panel--page-comment-overlay nil)
  (when page-comments
    (let* ((count (length page-comments))
	   (label (format "[%s PAGE comment%s]" count (if (= count 1) "" "s")))
	   (overlay (make-overlay (hub/org-context-panel--metadata-end-position)
				  (hub/org-context-panel--metadata-end-position)
				  nil t nil)))
      (overlay-put overlay 'after-string
		   (concat (propertize label
				       'face 'link
				       'mouse-face 'highlight
				       'help-echo "Open page comments"
				       'keymap (hub/org-context-panel--page-comment-marker-map))
			   "\n"))
      (setq hub/org-context-panel--page-comment-overlay overlay))))

(defun hub/org-context-panel--sync-status-marker-map ()
  "Return keymap for the Confluence sync status marker."
  (let ((map (make-sparse-keymap)))
    (when (fboundp 'hub/confluence-sync-status-open-from-marker)
      (define-key map (kbd "RET") #'hub/confluence-sync-status-open-from-marker)
      (define-key map [mouse-1] #'hub/confluence-sync-status-open-from-marker))
    map))

(defun hub/org-context-panel--confluence-page-id-present-p ()
  "Return non-nil when the current Org buffer has a Confluence page ID."
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search t))
      (re-search-forward "^[[:blank:]]*#\\+CONFLUENCE_PAGE_ID:[[:blank:]]*\\S-+" nil t))))

(defun hub/org-context-panel--refresh-sync-status-marker ()
  "Refresh top Confluence sync status marker in the current source buffer."
  (when (overlayp hub/org-context-panel--sync-status-overlay)
    (delete-overlay hub/org-context-panel--sync-status-overlay))
  (setq hub/org-context-panel--sync-status-overlay nil)
  (when (and buffer-file-name
	     (fboundp 'hub/confluence-sync-status-marker-string)
	     (hub/org-context-panel--confluence-page-id-present-p))
    (let* ((label (hub/confluence-sync-status-marker-string buffer-file-name))
	   (overlay (make-overlay (hub/org-context-panel--metadata-end-position)
				  (hub/org-context-panel--metadata-end-position)
				  nil t nil)))
      (overlay-put overlay 'priority -1)
      (overlay-put overlay 'after-string
		   (concat (propertize label
				       'face 'link
				       'mouse-face 'highlight
				       'help-echo "Open Confluence sync status"
				       'keymap (hub/org-context-panel--sync-status-marker-map))
			   "\n"))
      (setq hub/org-context-panel--sync-status-overlay overlay))))

(defun hub/org-context-panel--refresh-comment-overlays (source-buffer items)
  "Refresh source comment overlays for comment ITEMS in SOURCE-BUFFER."
  (when (buffer-live-p source-buffer)
    (with-current-buffer source-buffer
      (hub/org-context-panel--delete-comment-overlays)
      (hub/org-context-panel--refresh-page-comment-marker
       (cl-remove-if-not #'hub/org-context-panel--page-comment-p items))
      (hub/org-context-panel--refresh-sync-status-marker)
      (dolist (item items)
	(when (and (hub/org-context-panel--comment-item-p item)
		   (not (hub/org-context-panel--stale-comment-p item))
		   (not (hub/org-context-panel--page-comment-p item)))
	  (let ((start (plist-get item :target-start))
		(end (plist-get item :target-end)))
	    (when (and start end (<= (point-min) start) (<= start end) (<= end (point-max)))
	      (let ((overlay (make-overlay start end nil t nil)))
		(overlay-put overlay 'face 'hub/org-context-panel-comment-region-face)
		(overlay-put overlay 'hub-org-context-panel-item item)
		(push overlay hub/org-context-panel--comment-overlays)))))))))

;;;###autoload
(defun hub/org-comment-overlays-refresh ()
  "Refresh persistent sidecar comment overlays in the current Org buffer."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Org comment overlays only work in Org buffers"))
  (hub/org-context-panel--refresh-comment-overlays
   (current-buffer)
   (append (hub/org-comment-collect (current-buffer))
	   (hub/org-comment-collect-page (current-buffer)))))

;;;###autoload
(define-minor-mode hub/org-comment-overlays-mode
  "Toggle persistent sidecar comment overlays in the current Org buffer."
  :lighter " Cmnt"
  (if hub/org-comment-overlays-mode
      (progn
	(add-hook 'after-save-hook #'hub/org-comment-overlays-refresh nil t)
	(hub/org-comment-overlays-refresh))
    (remove-hook 'after-save-hook #'hub/org-comment-overlays-refresh t)
    (hub/org-context-panel--delete-comment-overlays)))

(defun hub/org-context-panel--visual-fill-total-margin (source-window)
  "Return total visual-fill-column margin for SOURCE-WINDOW."
  (let* ((width (or (and (boundp 'visual-fill-column-width)
			 (numberp visual-fill-column-width)
			 visual-fill-column-width)
		    fill-column))
	 (total-width (if (fboundp 'visual-fill-column--window-max-text-width)
			  (visual-fill-column--window-max-text-width source-window)
			(window-body-width source-window))))
    (max 0 (- total-width width))))

(defun hub/org-context-panel--dock-prose (source-window)
  "Dock visually filled prose in SOURCE-WINDOW toward the context panel."
  (when (and hub/org-context-panel-dock-prose
	     (window-live-p source-window)
	     (require 'visual-fill-column nil t))
    (with-current-buffer (window-buffer source-window)
      (when (bound-and-true-p visual-fill-column-mode)
	(unless hub/org-context-panel--visual-fill-state
	  (setq hub/org-context-panel--visual-fill-state
		(list :center (and (boundp 'visual-fill-column-center-text)
				   visual-fill-column-center-text)
		      :extra (and (boundp 'visual-fill-column-extra-text-width)
				  visual-fill-column-extra-text-width))))
	(let* ((margin (hub/org-context-panel--visual-fill-total-margin source-window))
	       (half (/ margin 2)))
	  ;; `visual-fill-column' can only left-dock or center text directly.  Keep
	  ;; centering enabled, then shift the centered margins right by expanding the
	  ;; left margin and collapsing the right margin.
	  (setq-local visual-fill-column-center-text t
		      visual-fill-column-extra-text-width (cons (- half) half))
	  (when (fboundp 'visual-fill-column--set-margins)
	    (visual-fill-column--set-margins source-window)))))))

(defun hub/org-context-panel--restore-prose-docking (source-buffer)
  "Restore visual-fill-column state saved for SOURCE-BUFFER."
  (when (and (buffer-live-p source-buffer)
	     (require 'visual-fill-column nil t))
    (with-current-buffer source-buffer
      (when hub/org-context-panel--visual-fill-state
	(setq-local visual-fill-column-center-text
		    (plist-get hub/org-context-panel--visual-fill-state :center)
		    visual-fill-column-extra-text-width
		    (plist-get hub/org-context-panel--visual-fill-state :extra)
		    hub/org-context-panel--visual-fill-state nil)
	(when-let* ((window (get-buffer-window source-buffer t)))
	  (when (fboundp 'visual-fill-column--set-margins)
	    (visual-fill-column--set-margins window)))))))

(defun hub/org-context-panel--post-command-refresh ()
  "Refresh a visible context panel after source-buffer commands."
  (when hub/org-context-panel-mode
    (hub/org-context-panel-refresh)))

(defun hub/org-context-panel--visible-window ()
  "Return the visible context panel window, or nil."
  (when-let* ((panel (get-buffer hub/org-context-panel-buffer-name)))
    (get-buffer-window panel t)))

(defun hub/org-context-panel--panel-source-buffer ()
  "Return the source buffer currently rendered by the context panel."
  (when-let* ((panel (get-buffer hub/org-context-panel-buffer-name)))
    (with-current-buffer panel
      hub/org-context-panel-source-buffer)))

(defun hub/org-context-panel--minibuffer-active-p ()
  "Return non-nil while minibuffer interaction is active."
  (or (active-minibuffer-window)
      (minibufferp (window-buffer (selected-window)))))

(defun hub/org-context-panel--enable-follow ()
  "Enable automatic context panel following."
  (add-hook 'buffer-list-update-hook #'hub/org-context-panel--follow-selected-buffer))

(defun hub/org-context-panel--disable-follow ()
  "Disable automatic context panel following."
  (remove-hook 'buffer-list-update-hook #'hub/org-context-panel--follow-selected-buffer))

(defun hub/org-context-panel--panel-buffer-p (buffer)
  "Return non-nil when BUFFER is an Org context panel buffer."
  (with-current-buffer buffer
    (or (derived-mode-p 'hub/org-context-panel-buffer-mode)
	(derived-mode-p 'hub/org-context-panel-help-mode))))

(defun hub/org-context-panel--follow-selected-buffer ()
  "Keep a visible context panel bound to the selected Org buffer."
  (unless (or hub/org-context-panel--following
	      (hub/org-context-panel--minibuffer-active-p)
	      (hub/org-context-panel--panel-buffer-p (window-buffer (selected-window))))
    (let ((hub/org-context-panel--following t))
      (when-let* ((panel-window (hub/org-context-panel--visible-window))
		  (source-window (selected-window)))
	(unless (eq source-window panel-window)
	  (let ((source-buffer (window-buffer source-window)))
	    (if (with-current-buffer source-buffer (derived-mode-p 'org-mode))
		(let ((previous-source (hub/org-context-panel--panel-source-buffer))
		      (panel-buffer (window-buffer panel-window)))
		  (unless (eq previous-source source-buffer)
		    (hub/org-context-panel--restore-prose-docking previous-source))
		  (hub/org-context-panel--dock-prose source-window)
		  (hub/org-context-panel-render-buffer source-buffer panel-buffer source-window)
		  (with-current-buffer source-buffer
		    (setq hub/org-context-panel--panel-buffer panel-buffer)))
	      (hub/org-context-panel-close))))))))


(defun hub/org-context-panel--truncate (text length)
  "Return TEXT truncated to LENGTH characters with an ellipsis."
  (if (> (length text) length)
      (concat (substring text 0 length) "…")
    text))

(defun hub/org-context-panel--insert-icon (icon)
  "Insert context panel ICON with its face."
  (insert (propertize icon 'face 'hub/org-context-panel-icon-face)))

(defun hub/org-context-panel--insert-status-chip (status &optional face)
  "Insert comment STATUS as a chip, using optional FACE."
  (let ((label (upcase (or status "unknown"))))
    (insert (propertize (concat " " label " ")
			'face (or face (hub/org-context-panel--comment-status-face status))))))

(defun hub/org-context-panel--format-created-at (created-at)
  "Return compact display text for CREATED-AT."
  (if-let* ((time (ignore-errors (date-to-time created-at))))
      (format-time-string "%Y-%m-%d %H:%M" time)
    created-at))

(defun hub/org-context-panel--source-directory ()
  "Return directory of the current context panel source buffer, or nil."
  (when-let* ((source hub/org-context-panel-source-buffer)
	      (file (buffer-file-name source)))
    (file-name-directory file)))

(defun hub/org-context-panel--comment-author (comment)
  "Return display author for COMMENT, resolving people directories when possible."
  (let ((author (plist-get comment :author))
	(remote-author-id (plist-get comment :remote-author-id))
	(remote-author-display-name (plist-get comment :remote-author-display-name)))
    (or (hub/confluence-people-resolve-account-id
	 (or remote-author-id author)
	 (hub/org-context-panel--source-directory))
	remote-author-display-name
	author)))

(defun hub/org-context-panel--current-user-comment-p (comment author)
  "Return non-nil when COMMENT/AUTHOR identifies the current user."
  (let ((directory (hub/org-context-panel--source-directory)))
    (or (hub/confluence-people-current-user-p
	 (plist-get comment :remote-author-id) directory)
	(hub/confluence-people-current-user-p author directory))))

(defun hub/org-context-panel--insert-comment-metadata (comment)
  "Insert author/date metadata for COMMENT when present."
  (let ((author (hub/org-context-panel--comment-author comment))
	(created-at (plist-get comment :created-at)))
    (when (or author created-at)
      (when author
	(insert (propertize
		 author 'face
		 (if (hub/org-context-panel--current-user-comment-p comment author)
		     'hub/org-context-panel-current-author-face
		   'hub/org-context-panel-metadata-face))))
      (when (and author created-at)
	(insert (propertize " · " 'face 'hub/org-context-panel-metadata-face)))
      (when created-at
	(insert (propertize (hub/org-context-panel--format-created-at created-at)
			    'face 'hub/org-context-panel-metadata-face)))
      (insert "\n"))))

(defun hub/org-context-panel--insert-marginalia (note)
  "Insert marginalia NOTE into the current panel buffer."
  (let ((kind (plist-get note :kind))
	(body (or (plist-get note :body) "")))
    (hub/org-context-panel--insert-icon (hub/org-context-panel--marginalia-kind-icon kind))
    (insert (format " %s\n" (plist-get note :id)))
    (hub/org-context-panel--insert-comment-body body)))

(defun hub/org-context-panel--readable-comment-body (comment)
  "Return readable projection of COMMENT body for display."
  (let ((body (or (plist-get comment :body) "")))
    (require 'org-confluence-commands nil 'noerror)
    (or (when (fboundp 'hub/confluence-import-storage-to-org)
	  (ignore-errors (hub/confluence-import-storage-to-org body)))
	(hub/org-comment--preview-plain-text body))))

(defun hub/org-context-panel--org-fontified-text (text)
  "Return TEXT fontified as Org without changing the current buffer mode."
  (with-temp-buffer
    (org-mode)
    (insert text)
    (font-lock-ensure (point-min) (point-max))
    (buffer-string)))

(defun hub/org-context-panel--apply-list-wrap-prefix (start end)
  "Indent wrapped list continuation lines between START and END by two spaces."
  (save-excursion
    (goto-char start)
    (while (< (point) end)
      (when (looking-at-p "[[:space:]]*\\([-+*]\\|[0-9]+[.)]\\)[[:space:]]+")
	(add-text-properties (line-beginning-position) (line-end-position)
			     '(wrap-prefix "  ")))
      (forward-line 1))))

(defun hub/org-context-panel--insert-comment-body (body)
  "Insert readable comment BODY with Org fontification and wrapping hints."
  (unless (string-empty-p body)
    (let ((start (point)))
      (insert (hub/org-context-panel--org-fontified-text body) "\n")
      (hub/org-context-panel--apply-list-wrap-prefix start (point)))))

(defun hub/org-context-panel--insert-comment-replies (comment)
  "Insert full reply conversation for COMMENT."
  (dolist (reply (plist-get comment :replies))
    (let ((start (point))
	  (has-metadata (or (hub/org-context-panel--comment-author reply)
			    (plist-get reply :created-at))))
      (insert "\n↳ " (hub/org-context-panel--comment-sync-badge reply))
      (if has-metadata
	  (progn
	    (insert " ")
	    (hub/org-context-panel--insert-comment-metadata reply))
	(insert "\n"))
      (hub/org-context-panel--insert-comment-body
       (hub/org-context-panel--readable-comment-body reply))
      (add-text-properties start (point) `(hub-org-context-panel-reply-item ,reply)))))

(defun hub/org-context-panel--remote-missing-comment-p (comment)
  "Return non-nil when COMMENT is linked to a missing remote comment."
  (equal (plist-get comment :remote-state) "missing"))

(defun hub/org-context-panel--comment-sync-badge (comment)
  "Return an emoji-only sync-state badge for COMMENT."
  (cond
   ((hub/org-context-panel--remote-missing-comment-p comment) "⚠")
   ((equal (plist-get comment :remote-anchor-state) "dangling") "⚠")
   ((equal (plist-get comment :remote-anchor-state) "unconfirmed") "❓")
   ((plist-get comment :local-updated-at) "✍️")
   ((plist-get comment :remote-id) "🔗")
   (t "✍️")))

(defun hub/org-context-panel--insert-comment (comment)
  "Insert COMMENT into the current panel buffer."
  (let ((status (or (plist-get comment :status) "open"))
	(target (or (plist-get comment :target-text) ""))
	(body (hub/org-context-panel--readable-comment-body comment))
	(replies (plist-get comment :replies))
	(stale (hub/org-context-panel--stale-comment-p comment))
	(remote-missing (hub/org-context-panel--remote-missing-comment-p comment))
	(page-comment (hub/org-context-panel--page-comment-p comment)))
    (hub/org-context-panel--insert-icon (cond (stale "⚠")
					      (remote-missing "⚠")
					      (page-comment "👆")
					      (t "💬")))
    (insert " ")
    (hub/org-context-panel--insert-status-chip
     status (when remote-missing 'hub/org-context-panel-remote-missing-status-face))
    (cond
     ((and page-comment (not hub/org-context-panel-page-context-p))
      (insert " " (propertize "PAGE" 'face 'hub/org-context-panel-target-face)))
     ((not (string-empty-p target))
      (insert " " (propertize
		   (concat "“" (hub/org-context-panel--truncate
				target hub/org-context-panel-target-preview-length) "”")
		   'face 'hub/org-context-panel-target-face))))
    (insert " " (hub/org-context-panel--comment-sync-badge comment) "\n")
    (when stale
      (insert (propertize "Anchor no longer matches source text.\n" 'face 'warning)))
    (when (and remote-missing (plist-get comment :current))
      (insert (propertize
	       (format "⚠ remote missing%s\n"
		       (if-let* ((missing-at (plist-get comment :remote-missing-at)))
			   (format " since %s" missing-at)
			 ""))
	       'face 'warning)))
    (hub/org-context-panel--insert-comment-metadata comment)
    (if (or (plist-get comment :current)
	    hub/org-context-panel-page-context-p)
	(progn
	  (hub/org-context-panel--insert-comment-body body)
	  (hub/org-context-panel--insert-comment-replies comment))
      (progn
	(dolist (line (hub/org-context-panel--overview-comment-lines body))
	  (insert line "\n"))
	(when replies
	  (insert (format "↳ %s repl%s\n"
			  (length replies) (if (= (length replies) 1) "y" "ies"))))))))

(defun hub/org-context-panel--insert-help-line (key description)
  "Insert one help footer line for KEY and DESCRIPTION."
  (insert (propertize key 'face 'hub/org-context-panel-help-key-face)
	  (make-string (max 1 (- 5 (length key))) ? )
	  (propertize description 'face 'hub/org-context-panel-help-description-face)
	  "\n"))

(defun hub/org-context-panel--render-help-buffer (buffer)
  "Render context panel help into BUFFER."
  (with-current-buffer buffer
    (let ((inhibit-read-only t))
      (hub/org-context-panel-help-mode)
      (erase-buffer)
      (insert (propertize "Context actions\n\n" 'face 'bold))
      (hub/org-context-panel--insert-help-line "RET" "jump")
      (hub/org-context-panel--insert-help-line "e" "edit")
      (hub/org-context-panel--insert-help-line "+" "reply")
      (hub/org-context-panel--insert-help-line "C-c C-c" "push draft")
      (hub/org-context-panel--insert-help-line "o" "open remote")
      (hub/org-context-panel--insert-help-line "p" "page comments")
      (hub/org-context-panel--insert-help-line "x" "delete")
      (hub/org-context-panel--insert-help-line "zz" "reset filters")
      (hub/org-context-panel--insert-help-line "za/zm/zd" "toggle actionable/mine/drafts")
      (hub/org-context-panel--insert-help-line "zr/zx" "toggle resolved/missing")
      (setq buffer-read-only t)))
  buffer)

(defun hub/org-context-panel--help-window-live-p ()
  "Return non-nil when this panel has a live help window."
  (and (window-live-p hub/org-context-panel--help-window)
       (equal (buffer-name (window-buffer hub/org-context-panel--help-window))
	      hub/org-context-panel-help-buffer-name)))

(defun hub/org-context-panel-help-close ()
  "Close the context help window and restore the originating panel point."
  (interactive)
  (let ((origin-window hub/org-context-panel-help--origin-window)
	(origin-point hub/org-context-panel-help--origin-point)
	(help-window (selected-window)))
    (when (window-live-p help-window)
      (delete-window help-window))
    (when (window-live-p origin-window)
      (with-current-buffer (window-buffer origin-window)
	(setq-local hub/org-context-panel--help-window nil))
      (when (integerp origin-point)
	(set-window-point origin-window origin-point))
      (select-window origin-window))))

(defun hub/org-context-panel-toggle-help ()
  "Toggle transient keybinding help below the current context panel."
  (interactive)
  (unless (derived-mode-p 'hub/org-context-panel-buffer-mode)
    (user-error "Context panel help only works in context panel buffers"))
  (if (hub/org-context-panel--help-window-live-p)
      (progn
	(delete-window hub/org-context-panel--help-window)
	(setq hub/org-context-panel--help-window nil))
    (let* ((panel-window (selected-window))
	   (help-buffer (hub/org-context-panel--render-help-buffer
			 (get-buffer-create hub/org-context-panel-help-buffer-name)))
	   (height (min 9 (max 6 (1- (window-total-height panel-window)))))
	   (help-window (if (window-parameter panel-window 'window-side)
			    (display-buffer-in-side-window
			     help-buffer
			     `((side . bottom)
			       (slot . 1)
			       (window-height . ,height)))
			  (let ((window (split-window panel-window (- height) 'below)))
			    (set-window-buffer window help-buffer)
			    window))))
      (setq-local hub/org-context-panel--help-window help-window)
      (with-current-buffer help-buffer
	(setq-local hub/org-context-panel-help--origin-window panel-window)
	(setq-local hub/org-context-panel-help--origin-point (window-point panel-window)))
      (set-window-point help-window (with-current-buffer help-buffer (point-min)))
      help-window)))

(defun hub/org-context-panel--insert-item (item)
  "Insert context ITEM into the current panel buffer."
  (let ((start (point)))
    (pcase (plist-get item :type)
      ('comment (hub/org-context-panel--insert-comment item))
      (_ (hub/org-context-panel--insert-marginalia item)))
    (add-text-properties start (point) `(hub-org-context-panel-item ,item))
    (when (plist-get item :current)
      (save-excursion
	(goto-char start)
	(add-face-text-property
	 start (line-end-position) 'hub/org-context-panel-current-item-face t)))))

(defun hub/org-context-panel--item-key (item)
  "Return a stable panel identity key for ITEM."
  (or (plist-get item :id)
      (plist-get item :definition-pos)
      (plist-get item :jump-pos)
      (plist-get item :anchor-pos)))

(defun hub/org-context-panel--item-at-point ()
  "Return context item at point or just before point."
  (or (get-text-property (point) 'hub-org-context-panel-reply-item)
      (get-text-property (max (point-min) (1- (point))) 'hub-org-context-panel-reply-item)
      (get-text-property (point) 'hub-org-context-panel-item)
      (get-text-property (max (point-min) (1- (point))) 'hub-org-context-panel-item)))

(defun hub/org-context-panel--exact-item-at-position (position)
  "Return context item exactly at POSITION, without looking backward."
  (or (get-text-property position 'hub-org-context-panel-reply-item)
      (get-text-property position 'hub-org-context-panel-item)))

(defun hub/org-context-panel--item-starts ()
  "Return positions where context panel items start."
  (let ((position (point-min))
	starts)
    (while (< position (point-max))
      (let ((item (hub/org-context-panel--exact-item-at-position position)))
	(when (and item
		   (or (= position (point-min))
		       (not (equal item (hub/org-context-panel--exact-item-at-position
					 (1- position))))))
	  (push position starts))
	(let ((reply-change (next-single-property-change
			     position 'hub-org-context-panel-reply-item nil (point-max)))
	      (item-change (next-single-property-change
			    position 'hub-org-context-panel-item nil (point-max))))
	  (setq position (min reply-change item-change)))))
    (nreverse starts)))

(defun hub/org-context-panel--default-filter-state ()
  "Return the default Org context panel filter state."
  '(:actionable nil :drafts nil :mine nil :show-resolved t :show-missing t))

(defun hub/org-context-panel--filter-state (source-buffer)
  "Return filter state for SOURCE-BUFFER."
  (with-current-buffer source-buffer
    (or hub/org-context-panel-filter-state
	(setq-local hub/org-context-panel-filter-state
		    (copy-sequence (hub/org-context-panel--default-filter-state))))))

(defun hub/org-context-panel--filter-active-p (state)
  "Return non-nil when restrictive filters in STATE are active."
  (or (plist-get state :actionable)
      (plist-get state :drafts)
      (plist-get state :mine)
      (not (plist-get state :show-resolved))
      (not (plist-get state :show-missing))))

(defun hub/org-context-panel--filter-default-p (state)
  "Return non-nil when filter STATE matches defaults."
  (equal state (hub/org-context-panel--default-filter-state)))

(defun hub/org-context-panel--resolved-comment-p (comment)
  "Return non-nil when COMMENT is locally or remotely resolved."
  (or (equal (plist-get comment :status) "RESOLVED")
      (equal (plist-get comment :remote-resolution-status) "resolved")))

(defun hub/org-context-panel--draft-comment-p (comment)
  "Return non-nil when COMMENT has local draft or edit state."
  (or (not (plist-get comment :remote-id))
      (plist-get comment :local-updated-at)))

(defun hub/org-context-panel--comment-or-reply-p (predicate comment)
  "Return non-nil when PREDICATE matches COMMENT or one of its replies."
  (or (funcall predicate comment)
      (seq-some (lambda (reply) (funcall predicate reply))
		(plist-get comment :replies))))

(defun hub/org-context-panel--current-user-comment-or-reply-p (comment)
  "Return non-nil when COMMENT or one of its replies belongs to the current user."
  (hub/org-context-panel--comment-or-reply-p
   (lambda (item)
     (hub/org-context-panel--current-user-comment-p
      item (hub/org-context-panel--comment-author item)))
   comment))

(defun hub/org-context-panel--actionable-comment-p (comment)
  "Return non-nil when COMMENT deserves action-focused display."
  (or (hub/org-context-panel--draft-comment-p comment)
      (hub/org-context-panel--stale-comment-p comment)
      (equal (plist-get comment :remote-anchor-state) "dangling")
      (equal (plist-get comment :remote-anchor-state) "unconfirmed")
      (not (hub/org-context-panel--resolved-comment-p comment))))

(defun hub/org-context-panel--item-visible-p (item state)
  "Return non-nil when ITEM should be shown under filter STATE."
  (if (not (eq (plist-get item :type) 'comment))
      (not (hub/org-context-panel--filter-active-p state))
    (and (or (plist-get state :show-missing)
	     (not (hub/org-context-panel--remote-missing-comment-p item)))
	 (or (plist-get state :show-resolved)
	     (not (hub/org-context-panel--resolved-comment-p item))
	     (plist-get item :replies))
	 (or (not (plist-get state :actionable))
	     (hub/org-context-panel--comment-or-reply-p
	      #'hub/org-context-panel--actionable-comment-p item))
	 (or (not (plist-get state :drafts))
	     (hub/org-context-panel--comment-or-reply-p
	      #'hub/org-context-panel--draft-comment-p item))
	 (or (not (plist-get state :mine))
	     (hub/org-context-panel--current-user-comment-or-reply-p item)))))

(defun hub/org-context-panel--filter-replies (item state)
  "Return ITEM with replies filtered according to STATE."
  (if (not (eq (plist-get item :type) 'comment))
      item
    (let ((copy (copy-sequence item)))
      (plist-put copy :replies
		 (seq-filter (lambda (reply)
			       (hub/org-context-panel--item-visible-p reply state))
			     (plist-get item :replies)))
      copy)))

(defun hub/org-context-panel--filter-items (items source-buffer)
  "Filter ITEMS according to SOURCE-BUFFER panel filter state."
  (let ((state (hub/org-context-panel--filter-state source-buffer)))
    (seq-filter (lambda (item)
		  (hub/org-context-panel--item-visible-p item state))
		(mapcar (lambda (item)
			  (hub/org-context-panel--filter-replies item state))
			items))))

(defun hub/org-context-panel--filter-summary (state)
  "Return a human-readable summary for filter STATE."
  (string-join
   (delq nil
	 (list (when (plist-get state :actionable) "actionable")
	       (when (plist-get state :mine) "mine")
	       (when (plist-get state :drafts) "drafts")
	       (unless (plist-get state :show-resolved) "hide resolved")
	       (if (plist-get state :show-missing) "show missing" "hide missing")))
   ", "))

(defun hub/org-context-panel--filter-header (state shown total)
  "Return header text for filter STATE showing SHOWN out of TOTAL items."
  (unless (hub/org-context-panel--filter-default-p state)
    (format "Filters: %s · showing %s/%s · zz reset"
	    (hub/org-context-panel--filter-summary state) shown total)))

(defun hub/org-context-panel--set-filter (property value)
  "Set filter PROPERTY to VALUE for this panel's source and refresh panels."
  (unless (buffer-live-p hub/org-context-panel-source-buffer)
    (user-error "No source buffer for this context panel"))
  (let ((source hub/org-context-panel-source-buffer))
    (with-current-buffer source
      (let ((state (copy-sequence (hub/org-context-panel--filter-state source))))
	(setq-local hub/org-context-panel-filter-state
		    (plist-put state property value))))
    (hub/org-context-panel-refresh-visible-panels source)
    (message "Context filters: %s"
	     (hub/org-context-panel--filter-summary
	      (hub/org-context-panel--filter-state source)))))

(defun hub/org-context-panel--toggle-filter (property)
  "Toggle filter PROPERTY for this panel's source."
  (let ((state (hub/org-context-panel--filter-state hub/org-context-panel-source-buffer)))
    (hub/org-context-panel--set-filter property (not (plist-get state property)))))

(defun hub/org-context-panel-filter-reset ()
  "Reset Org context panel filters to defaults."
  (interactive)
  (unless (buffer-live-p hub/org-context-panel-source-buffer)
    (user-error "No source buffer for this context panel"))
  (let ((source hub/org-context-panel-source-buffer))
    (with-current-buffer source
      (setq-local hub/org-context-panel-filter-state
		  (copy-sequence (hub/org-context-panel--default-filter-state))))
    (hub/org-context-panel-refresh-visible-panels source)
    (message "Context filters reset: %s"
	     (hub/org-context-panel--filter-summary
	      (hub/org-context-panel--filter-state source)))))

(defun hub/org-context-panel-filter-toggle-actionable ()
  "Toggle actionable-only context filtering."
  (interactive)
  (hub/org-context-panel--toggle-filter :actionable))

(defun hub/org-context-panel-filter-toggle-drafts ()
  "Toggle draft/local-edit-only context filtering."
  (interactive)
  (hub/org-context-panel--toggle-filter :drafts))

(defun hub/org-context-panel-filter-toggle-mine ()
  "Toggle current-user-only context filtering."
  (interactive)
  (hub/org-context-panel--toggle-filter :mine))

(defun hub/org-context-panel-filter-toggle-resolved ()
  "Toggle whether resolved context items are shown."
  (interactive)
  (hub/org-context-panel--toggle-filter :show-resolved))

(defun hub/org-context-panel-filter-toggle-missing ()
  "Toggle whether remote-missing context items are shown."
  (interactive)
  (hub/org-context-panel--toggle-filter :show-missing))

(defun hub/org-context-panel-filter-status ()
  "Show current Org context panel filter status."
  (interactive)
  (unless (buffer-live-p hub/org-context-panel-source-buffer)
    (user-error "No source buffer for this context panel"))
  (message "Context filters: %s"
	   (hub/org-context-panel--filter-summary
	    (hub/org-context-panel--filter-state hub/org-context-panel-source-buffer))))

(defun hub/org-context-panel--goto-item-key (key)
  "Move point to the context panel item identified by KEY."
  (let ((found nil))
    (goto-char (point-min))
    (while (and (not found) (< (point) (point-max)))
      (let ((item (get-text-property (point) 'hub-org-context-panel-item)))
	(when (and item (equal key (hub/org-context-panel--item-key item)))
	  (setq found (point)))
	(when-let* ((reply (get-text-property (point) 'hub-org-context-panel-reply-item)))
	  (when (equal key (hub/org-context-panel--item-key reply))
	    (setq found (point))))
	(goto-char (or (next-single-property-change
			(point) 'hub-org-context-panel-item nil (point-max))
		       (point-max)))))
    (when found
      (goto-char found))))

(defun hub/org-context-panel--pulse-current-line ()
  "Briefly highlight the current context panel line when possible."
  (when (fboundp 'pulse-momentary-highlight-one-line)
    (pulse-momentary-highlight-one-line (point))))

(defun hub/org-context-panel-goto-comment-id (comment-id)
  "Move point to COMMENT-ID in the current context panel and highlight it."
  (interactive "sComment ID: ")
  (unless (hub/org-context-panel--goto-item-key comment-id)
    (user-error "Comment %s is not visible in this context panel" comment-id))
  (when-let* ((window (get-buffer-window (current-buffer) t)))
    (set-window-point window (point))
    (with-selected-window window
      (recenter)))
  (hub/org-context-panel--pulse-current-line)
  comment-id)

;;;###autoload
(defun hub/org-context-panel-render-buffer (source-buffer panel-buffer &optional source-window)
  "Render SOURCE-BUFFER context items into PANEL-BUFFER.
When SOURCE-WINDOW is non-nil, align notes to visible lines in that window."
  (let* ((source-point (with-current-buffer source-buffer (point)))
	 (all-items (with-current-buffer source-buffer
		      (sort
		       (mapcar
			(lambda (item)
			  (hub/org-context-panel--item-with-overview-height
			   (hub/org-context-panel--item-with-current-state item source-point)))
			(append (hub/org-marginalia-collect)
				(hub/org-comment-collect source-buffer t)))
		       (lambda (left right)
			 (< (or (plist-get left :anchor-line) 1)
			    (or (plist-get right :anchor-line) 1))))))
	 (focused-comment (hub/org-context-panel--focused-comment all-items))
	 (viewport-items (cond
			  (focused-comment (list focused-comment))
			  ((and source-window (window-live-p source-window))
			   (hub/org-context-panel--items-for-window source-window all-items))
			  (t (hub/org-context-panel--items-for-window nil all-items))))
	 (items (hub/org-context-panel--filter-items viewport-items source-buffer)))
    (hub/org-context-panel--refresh-comment-overlays source-buffer all-items)
    (with-current-buffer panel-buffer
      (let ((inhibit-read-only t)
	    (preserved-point (point))
	    (preserved-key (hub/org-context-panel--item-key
			    (hub/org-context-panel--item-at-point))))
	(hub/org-context-panel-buffer-mode)
	(setq-local hub/org-context-panel-source-buffer source-buffer)
	(setq-local hub/org-context-panel--source-buffer source-buffer)
	(setq-local header-line-format
		    (hub/org-context-panel--filter-header
		     (hub/org-context-panel--filter-state source-buffer)
		     (length items) (length viewport-items)))
	(erase-buffer)
	(cond
	 (items
	  (let ((current-line 1))
	    (dolist (item items)
	      (cond
	       ((and (hub/org-context-panel--page-comment-p item)
		     (> current-line 1))
		(insert "\n\n\n")
		(setq current-line (+ current-line 3)))
	       ((and (hub/org-context-panel--stale-comment-p item)
		     (> current-line 1))
		(insert "\n\n")
		(setq current-line (+ current-line 2))))
	      (let ((display-line (or (plist-get item :display-line) current-line)))
		(while (< current-line display-line)
		  (insert "\n")
		  (setq current-line (1+ current-line)))
		(hub/org-context-panel--insert-item item)
		(setq current-line (line-number-at-pos (point) t))))))
	 (viewport-items
	  ;; The current viewport has context anchors, but filters hid them.
	  (insert "No context items match active filters.\n"))
	 (all-items
	  ;; The current viewport simply has no context anchors.
	  (insert ""))
	 (t
	  (insert "No context items in this buffer.\n")))
	(if preserved-key
	    (or (hub/org-context-panel--goto-item-key preserved-key)
		(goto-char (min preserved-point (point-max))))
	  (goto-char (min preserved-point (point-max))))
	(setq buffer-read-only t)))
    (when-let* ((window (get-buffer-window panel-buffer t)))
      (set-window-point window (with-current-buffer panel-buffer (point)))
      (set-window-start window (with-current-buffer panel-buffer (point-min))))
    panel-buffer))

(defun hub/org-page-context-render-buffer (source-buffer page-buffer &optional show-empty)
  "Render SOURCE-BUFFER page comments into PAGE-BUFFER.
When SHOW-EMPTY is non-nil, render an empty-state message when no page comments exist."
  (let* ((all-comments (with-current-buffer source-buffer
			 (hub/org-comment-collect-page source-buffer)))
	 (comments (hub/org-context-panel--filter-items all-comments source-buffer)))
    (with-current-buffer page-buffer
      (let ((inhibit-read-only t)
	    (preserved-point (point))
	    (preserved-key (hub/org-context-panel--item-key
			    (hub/org-context-panel--item-at-point))))
	(hub/org-context-panel-buffer-mode)
	(setq-local hub/org-context-panel-source-buffer source-buffer)
	(setq-local hub/org-context-panel--source-buffer source-buffer)
	(setq-local hub/org-context-panel-page-context-p t)
	(setq-local header-line-format
		    (hub/org-context-panel--filter-header
		     (hub/org-context-panel--filter-state source-buffer)
		     (length comments) (length all-comments)))
	(erase-buffer)
	(cond
	 (comments
	  (dolist (comment comments)
	    (hub/org-context-panel--insert-item comment)
	    (insert "\n")))
	 ((and all-comments (not comments))
	  (insert "No page comments match active filters.\n"))
	 (show-empty
	  (insert "No page comments.\n")))
	(if preserved-key
	    (or (hub/org-context-panel--goto-item-key preserved-key)
		(goto-char (min preserved-point (point-max))))
	  (goto-char (min preserved-point (point-max))))
	(setq buffer-read-only t)))
    (when-let* ((window (get-buffer-window page-buffer t)))
      (set-window-point window (with-current-buffer page-buffer (point)))
      (set-window-start window (with-current-buffer page-buffer (point-min))))
    page-buffer))

(defun hub/org-page-context--display-below-source (source-window page-buffer &optional select)
  "Display PAGE-BUFFER below SOURCE-WINDOW and optionally SELECT it."
  (let* ((source-height (window-total-height source-window))
	 (height (max 8 (floor (* source-height 0.33))))
	 (existing (get-buffer-window page-buffer t))
	 (page-window (or existing
			  (split-window source-window (- height) 'below))))
    (set-window-buffer page-window page-buffer)
    (when select
      (select-window page-window))
    page-window))

(defun hub/org-page-context-close (&optional source-buffer)
  "Close page-context window for SOURCE-BUFFER or current source."
  (let* ((source (or source-buffer
		     (if (derived-mode-p 'hub/org-context-panel-buffer-mode)
			 hub/org-context-panel-source-buffer
		       (current-buffer))))
	 (buffer (and (buffer-live-p source)
		      (with-current-buffer source
			hub/org-context-panel--page-panel-buffer)))
	 (window (and (buffer-live-p buffer) (get-buffer-window buffer t))))
    (when (and (window-live-p window)
	       (not (one-window-p t)))
      (delete-window window))))

;;;###autoload
(defun hub/org-page-context-open (&optional select show-empty)
  "Open page-level context below the current Org source buffer.
When SELECT is non-nil, focus the page-context window.  SHOW-EMPTY controls
whether an empty page-context panel is shown when there are no page comments."
  (interactive (list t t))
  (unless (derived-mode-p 'org-mode)
    (user-error "Page context only works in Org buffers"))
  (let* ((source-buffer (current-buffer))
	 (source-window (or (get-buffer-window source-buffer t) (selected-window)))
	 (page-buffer (hub/org-context-panel--page-panel-buffer))
	 (comments (hub/org-comment-collect-page source-buffer)))
    (if (or comments show-empty)
	(progn
	  (hub/org-page-context-render-buffer source-buffer page-buffer show-empty)
	  (hub/org-page-context--display-below-source source-window page-buffer select)
	  page-buffer)
      (hub/org-page-context-close source-buffer)
      nil)))

(defun hub/org-context-panel-refresh-visible-panels (source)
  "Refresh visible context panels for SOURCE without changing selection."
  (when (buffer-live-p source)
    (with-current-buffer source
      (hub/org-comment-overlays-refresh)
      (when-let* ((panel (and (buffer-live-p hub/org-context-panel--panel-buffer)
			      hub/org-context-panel--panel-buffer)))
	(when (get-buffer-window panel t)
	  (hub/org-context-panel-render-buffer source panel (get-buffer-window source t))))
      (when-let* ((page-panel (and (buffer-live-p hub/org-context-panel--page-panel-buffer)
				   hub/org-context-panel--page-panel-buffer)))
	(when (get-buffer-window page-panel t)
	  (hub/org-page-context-render-buffer source page-panel nil))))))

(defun hub/org-context-panel-revert-buffer (&optional _ignore-auto _noconfirm)
  "Refresh the current context panel buffer for `revert-buffer'."
  (unless (buffer-live-p hub/org-context-panel-source-buffer)
    (user-error "No source buffer for this context panel"))
  (let ((source hub/org-context-panel-source-buffer)
	(panel (current-buffer))
	(page-context hub/org-context-panel-page-context-p))
    (if page-context
	(hub/org-page-context-render-buffer source panel t)
      (hub/org-context-panel-render-buffer
       source panel (get-buffer-window source t)))
    (hub/org-context-panel-refresh-visible-panels source)
    (message "Refreshed Org context panel")))

;;;###autoload
(defun hub/org-context-panel-refresh ()
  "Refresh the context panel for the current Org source buffer."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Org context panel only works in Org buffers"))
  (hub/org-context-panel-render-buffer
   (current-buffer)
   (hub/org-context-panel--panel-buffer)
   (get-buffer-window (current-buffer) t)))

;;;###autoload
(defun hub/org-context-panel-open ()
  "Open or refresh the Org context side panel."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Org context panel only works in Org buffers"))
  (let* ((source-buffer (current-buffer))
	 (source-window (selected-window))
	 (panel (hub/org-context-panel--panel-buffer)))
    (display-buffer-in-side-window
     panel
     `((side . right)
       (slot . 1)
       (window-width . ,hub/org-context-panel-width)))
    ;; Render after displaying the side window: the source window width changes,
    ;; so position coordinates must use the final narrowed source window.
    (redisplay t)
    (hub/org-context-panel--dock-prose source-window)
    (hub/org-context-panel-render-buffer source-buffer panel source-window)
    (with-current-buffer source-buffer
      (setq hub/org-context-panel--panel-buffer panel)
      (hub/org-page-context-open nil nil))
    (hub/org-context-panel--enable-follow)
    panel))

;;;###autoload
(defun hub/org-context-panel-open-comment (comment-id &optional jump-position)
  "Open source context panel and select COMMENT-ID.
When JUMP-POSITION is non-nil, move source point there before rendering so the
comment is visible in viewport-scoped panels."
  (interactive "sComment ID: ")
  (unless (derived-mode-p 'org-mode)
    (user-error "Org context panel only works in Org buffers"))
  (when jump-position
    (goto-char jump-position))
  (let ((panel (hub/org-context-panel-open)))
    (with-current-buffer panel
      (hub/org-context-panel-goto-comment-id comment-id))
    panel))

;;;###autoload
(defun hub/org-context-panel-close ()
  "Close the context side panel associated with the current buffer."
  (interactive)
  (hub/org-context-panel--disable-follow)
  (let* ((panel-window (hub/org-context-panel--visible-window))
	 (source-buffer (cond
			 ((derived-mode-p 'hub/org-context-panel-buffer-mode)
			  hub/org-context-panel-source-buffer)
			 ((bound-and-true-p hub/org-context-panel--panel-buffer)
			  (current-buffer))
			 ((hub/org-context-panel--panel-source-buffer))
			 (t (current-buffer)))))
    (when (buffer-live-p source-buffer)
      (hub/org-context-panel--restore-prose-docking source-buffer)
      (with-current-buffer source-buffer
	(unless hub/org-comment-overlays-mode
	  (hub/org-context-panel--delete-comment-overlays))))
    (hub/org-page-context-close source-buffer)
    (when (and (window-live-p panel-window)
	       (with-current-buffer (window-buffer panel-window)
		 (hub/org-context-panel--help-window-live-p)))
      (with-current-buffer (window-buffer panel-window)
	(delete-window hub/org-context-panel--help-window)
	(setq hub/org-context-panel--help-window nil)))
    (when (window-live-p panel-window)
      (quit-window nil panel-window))))

;;;###autoload
(defun hub/org-context-panel--jump-to-sidecar-comment (comment)
  "Jump to COMMENT heading in its sidecar file."
  (let ((sidecar-file (plist-get comment :sidecar-file))
	(id (plist-get comment :id)))
    (unless (and sidecar-file id)
      (user-error "Comment record is missing sidecar metadata"))
    (find-file sidecar-file)
    (org-mode)
    (hub/org-comment-fold-sidecar-property-drawers)
    (goto-char (point-min))
    (unless (cl-loop while (re-search-forward org-heading-regexp nil t)
		     do (goto-char (match-beginning 0))
		     when (equal id (org-entry-get nil "HUB_COMMENT_ID"))
		     return t
		     do (forward-line 1))
      (user-error "Comment %s not found in sidecar" id))))

(defun hub/org-context-panel--goto-sidecar-body-from-heading ()
  "Move point from the current sidecar heading to its body."
  (let ((subtree-end (save-excursion (org-end-of-subtree t t))))
    (forward-line 1)
    (when (looking-at-p "[[:space:]]*:PROPERTIES:[[:space:]]*$")
      (when (re-search-forward "^[[:space:]]*:END:[[:space:]]*$" subtree-end t)
	(forward-line 1)))
    (while (and (< (point) subtree-end)
		(looking-at-p "\n"))
      (forward-char 1))))

(defun hub/org-context-panel--edit-sidecar-comment (comment)
  "Edit COMMENT sidecar body narrowed to its subtree."
  (hub/org-context-panel--jump-to-sidecar-comment comment)
  (org-narrow-to-subtree)
  (goto-char (point-min))
  (hub/org-context-panel--goto-sidecar-body-from-heading))

(defun hub/org-context-panel-next-item ()
  "Move point to the next context panel item."
  (interactive)
  (let* ((starts (hub/org-context-panel--item-starts))
	 (next (or (cl-find-if (lambda (position) (> position (point))) starts)
		   (car starts))))
    (unless next
      (user-error "No context items"))
    (goto-char next)))

(defun hub/org-context-panel-previous-item ()
  "Move point to the previous context panel item."
  (interactive)
  (let* ((starts (reverse (hub/org-context-panel--item-starts)))
	 (previous (or (cl-find-if (lambda (position) (< position (point))) starts)
		       (car starts))))
    (unless previous
      (user-error "No context items"))
    (goto-char previous)))

(defun hub/org-context-panel--comment-at-point ()
  "Return sidecar comment at point in the current source buffer, or nil."
  (let ((point (point)))
    (cl-find-if
     (lambda (comment)
       (let ((start (plist-get comment :target-start))
	     (end (plist-get comment :target-end)))
	 (and start end (<= start point) (<= point end))))
     (hub/org-comment-collect (current-buffer)))))

;;;###autoload
(defun hub/org-context-panel-jump-to-item-at-point ()
  "Jump from source point to the related item in the context panel.
When point is not inside a commented region, fall back to Evil's normal RET
motion."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Org context panel only works in Org buffers"))
  (cond
   ((hub/org-context-panel-page-comment-marker-at-point-p)
    (hub/org-page-comments-open))
   ((hub/org-context-panel--comment-at-point)
    (let ((comment (hub/org-context-panel--comment-at-point)))
      (hub/org-context-panel-open)
      (when-let* ((window (hub/org-context-panel--visible-window)))
	(select-window window)
	(hub/org-context-panel--goto-item-key (hub/org-context-panel--item-key comment)))))
   ((fboundp 'evil-ret)
    (call-interactively #'evil-ret))
   (t
    (call-interactively #'newline))))

(defun hub/org-context-panel-edit-item ()
  "Edit the sidecar entry backing the context item at point."
  (interactive)
  (let ((item (hub/org-context-panel--item-at-point)))
    (unless item
      (user-error "No context item at point"))
    (unless (eq 'comment (plist-get item :type))
      (user-error "Context item has no sidecar entry"))
    (hub/org-comment-compose-open 'edit item)))

(defun hub/org-context-panel-reply-to-item ()
  "Compose a local reply under the context-panel comment at point."
  (interactive)
  (let ((item (hub/org-context-panel--item-at-point)))
    (unless item
      (user-error "No context item at point"))
    (unless (eq 'comment (plist-get item :type))
      (user-error "Context item has no sidecar entry"))
    (hub/org-comment-compose-open 'reply item)))

(defun hub/org-comment-compose--body ()
  "Return editable compose buffer body."
  (string-trim (buffer-substring-no-properties (point-min) (point-max))))

(defun hub/org-comment-compose--display-buffer (buffer origin-window)
  "Display compose BUFFER below ORIGIN-WINDOW without replacing source text."
  (let* ((height (max 8 (floor (* (window-total-height origin-window) 0.33))))
	 (window (if (window-parameter origin-window 'window-side)
		     (display-buffer-in-side-window
		      buffer
		      `((side . bottom)
			(slot . 2)
			(window-height . ,height)))
		   (let ((target (split-window origin-window (- height) 'below)))
		     (set-window-buffer target buffer)
		     target))))
    (select-window window)
    window))

(defun hub/org-comment-compose-open (operation item)
  "Open a compose buffer for OPERATION on comment ITEM."
  (let ((source hub/org-context-panel-source-buffer)
	(origin-window (selected-window))
	(buffer (get-buffer-create (if (eq operation 'reply)
				       "*Org Comment Reply*"
				     "*Org Comment Edit*"))))
    (unless (buffer-live-p source)
      (user-error "No source buffer for this context panel"))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
	(erase-buffer)
	(org-mode)
	(hub/org-comment-compose-mode 1)
	(setq-local hub/org-comment-compose--context
		    (list :operation operation
			  :item item
			  :source source
			  :origin-window origin-window
			  :origin-key (hub/org-context-panel--item-key item)))
	(setq-local header-line-format
		    (if (eq operation 'reply)
			"Reply · C-c C-c submit · C-c C-s save draft · C-c C-k cancel"
		      "Edit · C-c C-c save local edit and close · C-c C-s save and keep open · C-c C-k cancel"))
	(add-hook 'kill-buffer-query-functions
		  #'hub/org-comment-compose--confirm-kill nil t)
	(when (eq operation 'edit)
	  (insert (hub/org-context-panel--readable-comment-body item)))
	(set-buffer-modified-p nil)
	(goto-char (point-max))))
    (with-current-buffer buffer
      (setq-local hub/org-comment-compose--window
		  (hub/org-comment-compose--display-buffer buffer origin-window)))))

(defun hub/org-comment-compose--restore-origin (context)
  "Restore point and focus to the context panel described by CONTEXT."
  (let ((origin-window (plist-get context :origin-window))
	(origin-key (plist-get context :origin-key)))
    (when (window-live-p origin-window)
      (with-current-buffer (window-buffer origin-window)
	(when origin-key
	  (hub/org-context-panel--goto-item-key origin-key)
	  (set-window-point origin-window (point))))
      (select-window origin-window))))

(defun hub/org-comment-compose--confirm-discard ()
  "Return non-nil when compose changes may be discarded."
  (or (not (buffer-modified-p))
      (yes-or-no-p "Discard unsaved comment changes? ")))

(defun hub/org-comment-compose--confirm-kill ()
  "Ask before killing a compose buffer with unsaved changes."
  (or hub/org-comment-compose--closing
      (hub/org-comment-compose--confirm-discard)))

(defun hub/org-comment-compose--close ()
  "Close the current compose buffer and restore the invoking panel."
  (let ((buffer (current-buffer))
	(window hub/org-comment-compose--window)
	(context hub/org-comment-compose--context))
    (setq-local hub/org-comment-compose--closing t)
    (when (and (window-live-p window)
	       (not (one-window-p t)))
      (delete-window window))
    (when (buffer-live-p buffer)
      (kill-buffer buffer))
    (hub/org-comment-compose--restore-origin context)))

(defun hub/org-comment-compose-cancel ()
  "Cancel the current Org comment compose buffer."
  (interactive)
  (unless hub/org-comment-compose--context
    (user-error "No active comment compose operation"))
  (unless (hub/org-comment-compose--confirm-discard)
    (user-error "Canceled"))
  (hub/org-comment-compose--close))

(defun hub/org-comment-compose--body-bounds ()
  "Return body bounds for the sidecar heading at point."
  (let* ((subtree-end (save-excursion (org-end-of-subtree t t)))
	 (body-start (save-excursion
		       (forward-line 1)
		       (when (looking-at-p "[[:space:]]*:PROPERTIES:[[:space:]]*$")
			 (re-search-forward "^[[:space:]]*:END:[[:space:]]*$" subtree-end t)
			 (forward-line 1))
		       (point)))
	 (body-end (save-excursion
		     (goto-char body-start)
		     (if (re-search-forward org-heading-regexp subtree-end t)
			 (match-beginning 0)
		       subtree-end))))
    (cons body-start body-end)))

(defun hub/org-comment-compose--find-heading (item)
  "Move to sidecar heading for ITEM in the current buffer."
  (hub/org-context-panel--goto-sidecar-comment-in-buffer item))

(defun hub/org-comment-compose--save-edit (item body)
  "Save BODY into existing sidecar ITEM."
  (let ((sidecar (plist-get item :sidecar-file)))
    (with-temp-buffer
      (insert-file-contents sidecar)
      (org-mode)
      (hub/org-comment-compose--find-heading item)
      (let* ((bounds (hub/org-comment-compose--body-bounds))
	     (old-body (string-trim
			(buffer-substring-no-properties (car bounds) (cdr bounds)))))
	(delete-region (car bounds) (cdr bounds))
	(goto-char (car bounds))
	(insert body "\n")
	(when (and (plist-get item :remote-id)
		   (not (string= old-body body)))
	  (save-excursion
	    (org-back-to-heading t)
	    (org-entry-put nil "HUB_COMMENT_LOCAL_UPDATED_AT"
			   (hub/org-comment-current-created-at)))))
      (write-region (point-min) (point-max) sidecar nil 'silent))
    (hub/org-comment-refresh-sidecar-headings sidecar)))

(defun hub/org-comment-compose--root-item (item sidecar)
  "Return root item for ITEM in SIDECAR."
  (with-temp-buffer
    (insert-file-contents sidecar)
    (org-mode)
    (hub/org-comment-compose--find-heading item)
    (while (and (> (org-outline-level) 1) (org-up-heading-safe)))
    (list :id (org-entry-get nil "HUB_COMMENT_ID")
	  :remote-id (org-entry-get nil "HUB_COMMENT_REMOTE_ID"))))

(defun hub/org-comment-compose--save-reply (item body)
  "Create a local reply to ITEM with BODY and return its sidecar record."
  (let* ((sidecar (plist-get item :sidecar-file))
	 (root (hub/org-comment-compose--root-item item sidecar)))
    (unless (plist-get root :remote-id)
      (user-error "Cannot create a Confluence reply before the root comment is remote-linked"))
    (with-temp-buffer
      (insert-file-contents sidecar)
      (org-mode)
      (hub/org-context-panel--goto-sidecar-comment-in-buffer root)
      (let* ((record (list :id (hub/org-comment-generate-id)
			   :remote-parent-id (plist-get root :remote-id)
			   :author (hub/org-comment-current-author)
			   :created-at (hub/org-comment-current-created-at)
			   :body body))
	     (reply-id (plist-get record :id))
	     (insert-at (save-excursion (org-end-of-subtree t t))))
	(goto-char insert-at)
	(unless (bolp) (insert "\n"))
	(insert "\n" (hub/org-comment--format-reply-entry record (file-name-directory sidecar)))
	(write-region (point-min) (point-max) sidecar nil 'silent)
	(hub/org-comment-refresh-sidecar-headings sidecar)
	(list :type 'comment :id reply-id :sidecar-file sidecar)))))

(defun hub/org-comment-compose--save-current (context body)
  "Save compose BODY according to CONTEXT and return the sidecar item."
  (let* ((operation (plist-get context :operation))
	 (item (or (plist-get context :saved-item)
		   (plist-get context :item))))
    (cond
     ((or (eq operation 'edit) (plist-get context :saved-item))
      (hub/org-comment-compose--save-edit item body)
      item)
     (t
      (hub/org-comment-compose--save-reply item body)))))

(defun hub/org-comment-compose-save-draft ()
  "Save the current composed comment as a local draft."
  (interactive)
  (let* ((context hub/org-comment-compose--context)
	 (source (plist-get context :source))
	 (body (hub/org-comment-compose--body))
	 saved)
    (unless context
      (user-error "No active comment compose operation"))
    (setq saved (hub/org-comment-compose--save-current context body))
    (setq-local hub/org-comment-compose--context
		(plist-put context :saved-item saved))
    (set-buffer-modified-p nil)
    (hub/org-context-panel-refresh-visible-panels source)
    (message "Saved comment draft")))

(defun hub/org-comment-compose-submit ()
  "Submit the current composed comment."
  (interactive)
  (let* ((context hub/org-comment-compose--context)
	 (operation (plist-get context :operation))
	 (item (plist-get context :item))
	 (source (plist-get context :source))
	 (body (hub/org-comment-compose--body))
	 saved)
    (unless context
      (user-error "No active comment compose operation"))
    (setq saved (hub/org-comment-compose--save-current context body))
    (when (eq operation 'reply)
      (with-current-buffer (find-file-noselect (plist-get saved :sidecar-file))
	(org-mode)
	(hub/org-context-panel--goto-sidecar-comment-in-buffer saved)
	(hub/confluence-comment-push-current)))
    (hub/org-context-panel-refresh-visible-panels source)
    (when (and (eq operation 'edit) (plist-get item :remote-id))
      (message "Saved locally; remote comment update is not implemented yet"))
    (hub/org-comment-compose--close)))

;;;###autoload
(defun hub/org-page-context-open-comment (comment-id)
  "Open page-context panel and select COMMENT-ID."
  (interactive "sComment ID: ")
  (unless (derived-mode-p 'org-mode)
    (user-error "Org page context only works in Org buffers"))
  (let ((panel (hub/org-page-context-open t t)))
    (unless panel
      (user-error "No page context panel available"))
    (with-current-buffer panel
      (hub/org-context-panel-goto-comment-id comment-id))
    panel))

(defun hub/org-context-panel-open-page-comments ()
  "Open the source buffer page-context bottom window."
  (interactive)
  (unless (buffer-live-p hub/org-context-panel-source-buffer)
    (user-error "No source buffer for this context panel"))
  (with-current-buffer hub/org-context-panel-source-buffer
    (hub/org-page-context-open t t)))

(defun hub/org-context-panel--goto-sidecar-comment-in-buffer (item)
  "Visit ITEM sidecar in the current buffer and move to its heading."
  (let ((id (plist-get item :id)))
    (unless id
      (user-error "Comment record is missing sidecar metadata"))
    (goto-char (point-min))
    (unless (cl-loop while (re-search-forward org-heading-regexp nil t)
		     do (goto-char (match-beginning 0))
		     when (equal id (org-entry-get nil "HUB_COMMENT_ID"))
		     return t
		     do (forward-line 1))
      (user-error "Comment %s not found in sidecar" id))))

(defun hub/org-context-panel-push-item ()
  "Push the draft comment or reply at point without visiting its sidecar."
  (interactive)
  (let* ((item (hub/org-context-panel--item-at-point))
	 (source hub/org-context-panel-source-buffer)
	 (sidecar-file (plist-get item :sidecar-file)))
    (unless item
      (user-error "No context item at point"))
    (unless (eq 'comment (plist-get item :type))
      (user-error "Context item has no sidecar entry"))
    (when (plist-get item :remote-id)
      (user-error "Comment is already linked to Confluence"))
    (unless (and sidecar-file (file-readable-p sidecar-file))
      (user-error "Comment record is missing sidecar file"))
    (with-current-buffer (find-file-noselect sidecar-file)
      (org-mode)
      (hub/org-context-panel--goto-sidecar-comment-in-buffer item)
      (hub/confluence-comment-push-current))
    (hub/org-context-panel-refresh-visible-panels source)))

(defun hub/org-context-panel--set-item-status (status)
  "Set context-panel comment at point to STATUS."
  (let* ((item (hub/org-context-panel--item-at-point))
	 (source hub/org-context-panel-source-buffer)
	 (sidecar-file (plist-get item :sidecar-file))
	 (key (and item (hub/org-context-panel--item-key item))))
    (unless item
      (user-error "No context item at point"))
    (unless (eq 'comment (plist-get item :type))
      (user-error "Context item has no sidecar entry"))
    (unless (and sidecar-file (plist-get item :id))
      (user-error "Comment record is missing sidecar metadata"))
    (with-temp-buffer
      (insert-file-contents sidecar-file)
      (org-mode)
      (hub/org-context-panel--goto-sidecar-comment-in-buffer item)
      (org-todo status)
      (write-region (point-min) (point-max) sidecar-file nil 'silent))
    (when (buffer-live-p source)
      (with-current-buffer source
	(hub/org-comment-overlays-refresh))
      (if hub/org-context-panel-page-context-p
	  (hub/org-page-context-render-buffer source (current-buffer) t)
	(hub/org-context-panel-render-buffer
	 source (current-buffer) (get-buffer-window source t)))
      (hub/org-context-panel-refresh-visible-panels source))
    (when key
      (hub/org-context-panel--goto-item-key key))
    (message "Marked comment %s %s" (plist-get item :id) status)))

(defun hub/org-context-panel-mark-open ()
  "Mark the context-panel comment at point OPEN."
  (interactive)
  (hub/org-context-panel--set-item-status "OPEN"))

(defun hub/org-context-panel-mark-todo ()
  "Mark the context-panel comment at point TODO."
  (interactive)
  (hub/org-context-panel--set-item-status "TODO"))

(defun hub/org-context-panel-mark-resolved ()
  "Mark the context-panel comment at point RESOLVED."
  (interactive)
  (hub/org-context-panel--set-item-status "RESOLVED"))

(defun hub/org-context-panel-open-remote-item ()
  "Open the Confluence URL for the context-panel comment at point."
  (interactive)
  (let ((item (hub/org-context-panel--item-at-point)))
    (unless item
      (user-error "No context item at point"))
    (unless (eq 'comment (plist-get item :type))
      (user-error "Context item has no remote comment"))
    (unless (plist-get item :remote-id)
      (user-error "Comment is not linked to Confluence"))
    (unless (buffer-live-p hub/org-context-panel-source-buffer)
      (user-error "No source buffer for this context panel"))
    (unless (fboundp 'hub/confluence-comment-open-current)
      (user-error "Confluence comment opener is not loaded"))
    (with-current-buffer hub/org-context-panel-source-buffer
      (hub/confluence-comment-open-current nil (plist-get item :remote-id)))))

(defun hub/org-context-panel-delete-item ()
  "Delete the sidecar comment backing the context item at point."
  (interactive)
  (let* ((item (hub/org-context-panel--item-at-point))
	 (source hub/org-context-panel-source-buffer)
	 (id (plist-get item :id))
	 (sidecar-file (plist-get item :sidecar-file)))
    (unless item
      (user-error "No context item at point"))
    (unless (eq 'comment (plist-get item :type))
      (user-error "Context item has no sidecar entry"))
    (unless (and id sidecar-file)
      (user-error "Comment record is missing sidecar metadata"))
    (when (yes-or-no-p (format "Delete comment %s? " id))
      (hub/org-comment-delete-entry sidecar-file id)
      (when (buffer-live-p source)
	(with-current-buffer source
	  (hub/org-comment-overlays-refresh))
	(if hub/org-context-panel-page-context-p
	    (hub/org-page-context-render-buffer source (current-buffer) t)
	  (hub/org-context-panel-render-buffer
	   source (current-buffer) (get-buffer-window source t)))
	(hub/org-context-panel-refresh-visible-panels source))
      (message "Deleted comment %s" id))))

(defun hub/org-context-panel-jump-to-definition ()
  "Jump from a rendered context item to its source or sidecar definition."
  (interactive)
  (let* ((note (hub/org-context-panel--item-at-point))
	 (source hub/org-context-panel-source-buffer)
	 (position (or (plist-get note :definition-pos)
		       (plist-get note :jump-pos))))
    (cond
     ((and note (or (hub/org-context-panel--stale-comment-p note)
		    (hub/org-context-panel--page-comment-p note)))
      (hub/org-context-panel--jump-to-sidecar-comment note))
     ((and note (buffer-live-p source) position)
      (pop-to-buffer source)
      (goto-char position))
     (t
      (user-error "No context item at point")))))

;;;###autoload
(define-minor-mode hub/org-context-panel-mode
  "Toggle an Org context side panel for the current buffer."
  :lighter " Ctx"
  (if hub/org-context-panel-mode
      (progn
	(add-hook 'post-command-hook #'hub/org-context-panel--post-command-refresh nil t)
	(hub/org-context-panel-open))
    (remove-hook 'post-command-hook #'hub/org-context-panel--post-command-refresh t)
    (hub/org-context-panel-close)))

(provide 'org/context-panel)
;;; context-panel.el ends here
